#' Gradient Boosted Trees using XGBoost for Survival Analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param dataset Dataset
#' @param time_var The time-to-event variable in the model
#' @param status_var The event occurrence variable in the model
#' @param evar Explanatory variables in the model
#' @param lev Level to use as the first column in prediction output
#' @param max_depth Maximum 'depth' of tree
#' @param learning_rate Learning rate (eta)
#' @param min_split_loss Minimal improvement (gamma)
#' @param nrounds Number of trees to create
#' @param min_child_weight Minimum number of instances allowed in each node
#' @param subsample Subsample ratio of the training instances (0-1)
#' @param early_stopping_rounds Early stopping rule
#' @param nthread Number of parallel threads to use. Defaults to 12 if available
#' @param wts Weights to use in estimation
#' @param seed Random seed to use as the starting point
#' @param data_filter Expression entered in, e.g., Data > View to filter the dataset in Radiant. The expression should be a string (e.g., "price > 10000")
#' @param arr Expression to arrange (sort) the data on (e.g., "color, desc(price)")
#' @param rows Rows to select from the specified dataset
#' @param envir Environment to extract data from
#' @param cox_regression Logical, if TRUE perform Cox regression modeling
#' @param ... Further arguments to pass to xgboost
#'
#' @return A list with all variables defined in gbt as an object of class gbt
#'
#' @examples
#' \dontrun{
#' gbt_survival(lung, "time", "status", c("age", "sex", "ph.ecog")) %>% summary()
#' gbt_survival(lung, "time", "status", c("age", "sex", "ph.ecog")) %>% str()
#' }
#' gbt_survival(
#'   lung, "time", "status", c("age", "sex", "ph.ecog"), lev = "Yes",
#'   early_stopping_rounds = 0, nthread = 1, cox_regression = TRUE
#' ) %>% summary()
#' gbt_survival(
#'   lung, "time", "status", c("age", "sex", "ph.ecog"),
#'   early_stopping_rounds = 0, nthread = 1, cox_regression = TRUE
#' ) %>% str()
#'
#' @seealso \code{\link{summary.gbt}} to summarize results
#' @seealso \code{\link{plot.gbt}} to plot results
#' @seealso \code{\link{predict.gbt}} for prediction
#'
#' @importFrom xgboost xgboost xgb.importance xgb.DMatrix xgb.train
#' @importFrom lubridate is.Date
#' @importFrom survival survfit survdiff Surv coxph
#' @importFrom broom tidy
#' @importFrom survcomp concordance.index
#'
#' @export
gbt_survival <- function(dataset, time_var, status_var, evar, lev = "",
                         max_depth = c(6), learning_rate = c(0.3), min_split_loss = c(0),
                         min_child_weight = c(1), subsample = c(1),
                         nrounds = c(100), early_stopping_rounds = 10,
                         nthread = 12, wts = "None", seed = 1234,
                         data_filter = "", arr = "", rows = NULL,
                         envir = parent.frame(), cox_regression = FALSE,
                         nfold = 10, test_size = 0.2, ...) {

  # Check and install survcomp package if not already installed
  if (!requireNamespace("BiocManager", quietly = TRUE)) {
    install.packages("BiocManager")
  }
  if (!requireNamespace("survcomp", quietly = TRUE)) {
    BiocManager::install("survcomp")
  }
  library(survcomp)

  if (time_var %in% evar || status_var %in% evar) {
    return("Time or status variable contained in the set of explanatory variables.\nPlease update model specification." %>%
             add_class("gbt_survival"))
  }

  vars <- c(time_var, status_var, evar)

  if (is.empty(wts, "None")) {
    wts <- NULL
  } else if (is_string(wts)) {
    wtsname <- wts
    vars <- c(time_var, status_var, evar, wtsname)
  }

  df_name <- if (is_string(dataset)) dataset else deparse(substitute(dataset))
  dataset <- get_data(dataset, vars, filt = data_filter, arr = arr, rows = rows, envir = envir) %>%
    mutate_if(is.Date, as.numeric)
  nr_obs <- nrow(dataset)

  if (!is.empty(wts, "None")) {
    if (exists("wtsname")) {
      wts <- dataset[[wtsname]]
      dataset <- select_at(dataset, .vars = base::setdiff(colnames(dataset), wtsname))
    }
    if (length(wts) != nrow(dataset)) {
      return(
        paste0("Length of the weights variable is not equal to the number of rows in the dataset (", format_nr(length(wts), dec = 0), " vs ", format_nr(nrow(dataset), dec = 0), ")") %>%
          add_class("gbt_survival")
      )
    }
  }

  not_vary <- colnames(dataset)[summarise_all(dataset, does_vary) == FALSE]
  if (length(not_vary) > 0) {
    return(paste0("The following variable(s) show no variation. Please select other variables.\n\n** ", paste0(not_vary, collapse = ", "), " **") %>%
             add_class("gbt_survival"))
  }

  set.seed(seed)

  # Split dataset into training and testing sets
  train_index <- sample(seq_len(nrow(dataset)), size = (1 - test_size) * nrow(dataset))
  train_data <- dataset[train_index, ]
  test_data <- dataset[-train_index, ]

  # Create new column indicating time label with sign for both training and testing sets
  train_data <- train_data %>%
    mutate(new_time = ifelse(train_data[[status_var]] == 0, -train_data[[time_var]], train_data[[time_var]]))

  test_data <- test_data %>%
    mutate(new_time = ifelse(test_data[[status_var]] == 0, -test_data[[time_var]], test_data[[time_var]]))

  # Prepare the data for xgboost
  dtrain <- xgb.DMatrix(data = model.matrix(~ . - 1, data = train_data[, evar, drop = FALSE]), label = train_data$new_time)
  dtest <- xgb.DMatrix(data = model.matrix(~ . - 1, data = test_data[, evar, drop = FALSE]), label = test_data$new_time)

  folds <- sample(rep(seq_len(nfold), length.out = nrow(train_data)))

  best_model <- NULL
  best_metric_value <- Inf
  best_tuning_parameters <- list()
  eval_log <- data.frame(iter = integer(), value = numeric())

  for (d in max_depth) {
    for (lr in learning_rate) {
      for (msl in min_split_loss) {
        for (mcw in min_child_weight) {
          for (ss in subsample) {
            for (nr in nrounds) {

              cv_results <- data.frame(fold = integer(), metric_value = numeric())

              for (fold in 1:nfold) {
                fold_train_data <- train_data[folds != fold, ]
                fold_eval_data <- train_data[folds == fold, ]

                gbt_input <- list(
                  max_depth = d,
                  learning_rate = lr,
                  min_split_loss = msl,
                  min_child_weight = mcw,
                  subsample = ss,
                  nrounds = nr,
                  early_stopping_rounds = early_stopping_rounds,
                  nthread = nthread,
                  objective = "survival:cox",
                  eval_metric = "cox-nloglik",
                  verbose = 0
                )

                ## adding data
                dtx <- model.matrix(~ . - 1, data = fold_train_data[, evar, drop = FALSE])
                y_lower <- fold_train_data$new_time

                dtrain_fold <- xgb.DMatrix(data = dtx, label = y_lower)

                ## Check that dty has the same length as the number of rows in dtx
                if (length(y_lower) != nrow(dtx)) {
                  stop("The length of labels must equal to the number of rows in the input data")
                }

                watchlist <- list(train = dtrain_fold)

                gbt_input$data <- dtrain_fold
                gbt_input$watchlist <- watchlist

                seed <- gsub("[^0-9]", "", seed)
                if (!is.empty(seed)) {
                  if (exists(".Random.seed")) {
                    gseed <- .Random.seed
                    on.exit(.Random.seed <<- gseed)
                  }
                  set.seed(seed)
                }

                model <- do.call(xgboost::xgb.train, gbt_input)

                metric_value <- model$best_score
                if (!is.null(metric_value) && length(metric_value) > 0) {
                  cv_results <- rbind(cv_results, data.frame(fold = fold, metric_value = metric_value))
                }
              }

              avg_metric_value <- mean(cv_results$metric_value)
              if (avg_metric_value < best_metric_value) {
                best_metric_value <- avg_metric_value
                best_model <- model
                best_tuning_parameters <- list(
                  max_depth = d,
                  learning_rate = lr,
                  min_split_loss = msl,
                  min_child_weight = mcw,
                  subsample = ss,
                  nrounds = nr
                )
                eval_log <- model$evaluation_log
                best_model$train_data <- train_data
                best_model$test_data <- test_data
                best_model$dtrain <- dtrain
                best_model$dtest <- dtest
                best_model$best_tuning_parameters <- best_tuning_parameters
                best_model$best_metric_value <- best_metric_value
                best_model$evaluation_log <- eval_log
              }
            }
          }
        }
      }
    }
  }

  if (cox_regression) {
    # Perform Cox regression modeling using cross-validation
    cox_c_indices <- numeric()

    for (fold in 1:nfold) {
      fold_train_data <- train_data[folds != fold, ]
      fold_eval_data <- train_data[folds == fold, ]

      formula <- as.formula(paste("Surv(", time_var, ",", status_var, ") ~ ", paste(evar, collapse = " + ")))
      cox_model <- coxph(formula, data = fold_train_data)

      # Calculate the concordance index (C-index) for the Cox model
      cox_pred <- predict(cox_model, newdata = fold_eval_data, type = "risk")
      c_index <- concordance.index(cox_pred, fold_eval_data[[time_var]], fold_eval_data[[status_var]])$c.index
      cox_c_indices <- c(cox_c_indices, c_index)
    }

    avg_cox_c_index <- mean(cox_c_indices)

    # Add Cox regression results to the output
    best_model$cox_c_indices <- cox_c_indices
    best_model$avg_cox_c_index <- avg_cox_c_index
  }

  as.list(environment()) %>% add_class(c("gbt_survival", "model"))
}
#' Summary method for the gbt_survival function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{gbt_survival}}
#' @param prn Print iteration history
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt_survival(
#'   lung, "time", "status", c("age", "sex", "ph.ecog"),
#'   early_stopping_rounds = 0, nthread = 1, cox_regression = TRUE
#' )
#' summary(result)
#' @seealso \code{\link{gbt_survival}} to generate results
#' @seealso \code{\link{plot.gbt}} to plot results
#' @seealso \code{\link{predict.gbt}} for prediction
#'
#' @export
summary.gbt_survival <- function(object, prn = TRUE, ...) {
  if (is.character(object)) {
    return(object)
  }
  cat("(XGBoost) - Survival Analysis\n")
  cat("Type                 : Survival Analysis")
  cat("\nData                 :", object$df_name)
  if (!is.empty(object$data_filter)) {
    cat("\nFilter               :", gsub("\\n", "", object$data_filter))
  }
  if (!is.empty(object$arr)) {
    cat("\nArrange              :", gsub("\\n", "", object$arr))
  }
  if (!is.empty(object$rows)) {
    cat("\nSlice                :", gsub("\\n", "", object$rows))
  }
  cat("\nResponse variable    :", object$time_var)
  cat("\nStatus variable      :", object$status_var)
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  }

  if (!is.null(object$cox_model)) {
    cat("\nCox Regression Model:\n")
    print(object$cox_model$call)
    cat("\nCoefficients:\n")
    print(coef(summary(object$cox_model)))
    cat("\nLikelihood ratio test: ",
        object$cox_model$logtest["test"],
        " on ",
        object$cox_model$logtest["df"],
        " df, p-value = ",
        object$cox_model$logtest["pvalue"],
        "\n", sep = "")
    cat("n = ", object$cox_model$n, ", number of events = ", sum(object$cox_model$y[, 2]), "\n", sep = "")
    cat("\nCox Model Concordance Index (C-index): ", object$best_model$avg_cox_c_index, "\n", sep = "")
  } else {
    # Extract best tuning parameters if not using Cox regression
    best_tuning_params <- list(
      max_depth = object$best_model$params$max_depth,
      learning_rate = object$best_model$params$learning_rate,
      min_split_loss = object$best_model$params$min_split_loss,
      min_child_weight = object$best_model$params$min_child_weight,
      subsample = object$best_model$params$subsample,
      nrounds = object$best_model$params$nrounds
    )

    cat("\nBest Tuning Parameters:\n")
    for (param in names(best_tuning_params)) {
      cat(paste(param, ":", best_tuning_params[[param]], "\n"))
    }

    cat("\nBest Train Negative Log-Likelihood:", object$best_metric_value, "\n")
    if (isTRUE(prn)) {
      # Output the training log-likelihood iterations
      eval_log <- object$best_model$evaluation_log
      cat("\nTrain Log Iterations:\n")
      print(eval_log)
    }
  }

  if (!is.empty(object$seed)) {
    cat("Seed                 :", object$seed, "\n")
  }

  if (!is.empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", format_nr(object$nr_obs, dec = 0), "\n")
  }
}


#' Predict method for the gbt_survival function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{gbt_survival}}
#' @param pred_data Provide the dataframe to generate predictions. The dataset must contain all columns used in the estimation
#' @param pred_cmd Generate predictions using a command. For example, `age = seq(30, 60, 5)` would produce predictions for different ages. To add another variable, create a vector of prediction strings, e.g., c('age = seq(30, 60, 5)', 'sex = c("male", "female")')
#' @param dec Number of decimals to show
#' @param envir Environment to extract data from
#' @param cox_regression Boolean flag to indicate if Cox regression predictions are needed
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt_survival(lung, "time", "status", c("age", "sex", "ph.ecog"), nthread = 1)
#' predict(result, pred_cmd = "age = seq(50, 70, 5)")
#' predict(result, pred_data = lung) %>% head()
#' @seealso \code{\link{gbt_survival}} to generate the result
#' @seealso \code{\link{summary.gbt_survival}} to summarize results
#'
#' @export
predict.gbt_survival <- function(object, pred_data = NULL, pred_cmd = "",
                                 dec = 3, envir = parent.frame(), cox_regression = FALSE, ...) {
  if (is.character(object)) {
    return(object)
  }

  # Ensure you have a name for the prediction dataset
  if (is.data.frame(pred_data)) {
    df_name <- deparse(substitute(pred_data))
    # Add RowID to the prediction data
    pred_data$RowID <- seq_len(nrow(pred_data))
  } else {
    df_name <- pred_data
  }
  explanatory_vars <- names(object$best_model$test_data)[!(names(object$best_model$test_data) %in% c("RowID", "new_time", "time", "status"))]

  if (cox_regression) {
    if (!is.null(object$cox_model)) {
      # Predict using the Cox regression model
      survival_prob <- predict(object$cox_model, newdata = object$test_data, type = "expected")
      survival_prob <- exp(-survival_prob)

      # Convert predictions to data frame
      survival_prob_df <- data.frame(SurvivalProbability = survival_prob)
    } else {
      stop("Cox regression model not found in the object. Please ensure cox_regression was set to TRUE when calling gbt_survival.")
    }
  } else {
    # Extract time and status variables
    pred.train <- log(predict(object$best_model, object$best_model$dtrain))
    pred.test  <- log(predict(object$best_model, object$best_model$dtest))
    time_interest <- sort(unique(object$train_data$new_time[object$train_data$status == 1]))
    basehaz_cum <- basehaz.gbm(object$train_data$time, object$train_data$status, pred.train, t.eval = time_interest, cumulative = TRUE)
    surf.i <- exp(-exp(pred.test[1]) * basehaz_cum)
    # Extract explanatory variables from xgb.DMatrix

    # Combine predictions with explanatory variables from the test set
    survival_prob_df <- data.frame(SurvivalProbability = surf.i )
  }
  # Create the formatted output
  header <- paste(
    "Survival Analysis",
    paste("Data                 :", df_name),
    "Response variable    : ",
    paste("Explanatory variables:", paste(explanatory_vars, collapse = ", ")),
    paste("Prediction dataset   :", df_name),
    paste("Rows shown           :", min(10, nrow(survival_prob_df)), "of", nrow(survival_prob_df)),
    sep = "\n"
  )

  # Limit the number of rows shown to 10 for the output
  survival_prob_df_shown <- head(survival_prob_df, 20)

  # Print the header and the data frame
  cat(header, "\n\n")

  survival_prob_df %>%
    set_attr("radiant_pred_data", df_name)
}


#' Print method for predict.gbt_survival
#'
#' @param x Return value from prediction method
#' @param ... further arguments passed to or from other methods
#' @param n Number of lines of prediction results to print. Use -1 to print all lines
#'
#' @export
print.gbt_survival.predict <- function(x, ..., n = 10) {
  print_predict_model(x, ..., n = n, header = "Gradient Boosted Trees - Survival Analysis")
}

#' Cross-Validated Gradient Boosted Trees using XGBoost for Survival Analysis
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/gbt.html} for an example in Radiant
#'
#' @param object The model object, either a previously fitted `gbt` object or an `xgb.Booster`
#' @param K Number of folds for cross-validation
#' @param repeats Number of times to repeat the cross-validation
#' @param params List of parameters for the xgboost model
#' @param nrounds Number of trees to create
#' @param early_stopping_rounds Early stopping rule
#' @param nthread Number of parallel threads to use. Defaults to 12 if available
#' @param train Training data in xgboost DMatrix format
#' @param trace Logical flag for tracing progress
#' @param seed Random seed to use as the starting point
#' @param maximize Logical flag indicating whether to maximize the evaluation metric
#' @param fun Custom evaluation function
#' @param ... Further arguments to pass to xgboost
#'
#' @return A data frame with the cross-validation results
#'
#' @examples
#' \dontrun{
#' cv.gbt_survival(gbt_model, K = 5, repeats = 1, params = list(max_depth = 6, eta = 0.3))
#' }
#' @importFrom xgboost xgb.cv xgb.DMatrix
#' @importFrom dplyr bind_rows
#' @importFrom shiny incProgress withProgress
#'
#' @export
cv.gbt_survival <- function(object, K = 5, repeats = 1, params = list(),
                            nrounds = 500, early_stopping_rounds = 10, nthread = 12,
                            train = NULL, trace = TRUE, seed = 1234, maximize = NULL, fun, ...) {
  if (inherits(object, "gbt_survival")) {
    time_var <- object$time_var
    status_var <- object$status_var
    dataset <- object$model$model
    evar <- object$evar

    if (!is.character(evar)) {
      stop("evar should be a character vector")
    }

    if (is.data.frame(dataset)) {
      dtx <- model.matrix(~ . - 1, data = dataset[, evar, drop = FALSE])
    } else {
      stop("dataset should be a data frame")
    }

    dty <- dataset[[time_var]]
    dstatus <- dataset[[status_var]]
    train <- xgboost::xgb.DMatrix(data = dtx, label = dty)
    objective <- "survival:cox"
    params_base <- object$model$params
  } else if (!inherits(object, "xgb.Booster")) {
    stop("The model object does not seem to be a Gradient Boosted Tree")
  } else {
    if (!inherits(train, "xgb.DMatrix")) {
      train <- eval(object$call[["data"]])
    }
    params_base <- object$params
  }

  if (!inherits(train, "xgb.DMatrix")) {
    stop("Could not access data. Please use the 'train' argument to pass along a matrix created using xgboost::xgb.DMatrix")
  }

  params_base[c("nrounds", "nthread", "silent")] <- NULL
  for (n in names(params)) {
    params_base[[n]] <- params[[n]]
  }
  params <- params_base
  if (is.null(maximize)) {
    maximize <- params$maximize
  }

  if (missing(fun)) {
    fun <- "cox-nloglik"
  }

  if (length(shiny::getDefaultReactiveDomain()) > 0) {
    trace <- FALSE
    incProgress <- shiny::incProgress
    withProgress <- shiny::withProgress
  } else {
    incProgress <- function(...) {}
    withProgress <- function(...) list(...)[["expr"]]
  }

  tf <- tempfile()
  tune_grid <- expand.grid(params)
  nitt <- nrow(tune_grid)
  withProgress(message = "Running cross-validation (gbt_survival)", value = 0, {
    out <- list()
    for (i in seq_len(nitt)) {
      cv_params <- tune_grid[i, ]
      if (!is.null(cv_params$nrounds)) {
        nrounds <- cv_params$nrounds
        cv_params$nrounds <- NULL
      }
      if (trace) {
        cat("Working on", paste0(paste(colnames(cv_params), "=", cv_params), collapse = ", "), "\n")
      }
      for (j in seq_len(repeats)) {
        set.seed(seed)
        sink(tf) ## avoiding messages from xgboost::xgb.cv
        cv_params_tmp <- cv_params
        for (nm in c("eval_metric", "maximize", "early_stopping_rounds", "nthread")) {
          cv_params_tmp[[nm]] <- NULL
        }
        model <- try(xgboost::xgb.cv(
          params = as.list(cv_params_tmp),
          data = train,
          nfold = K,
          print_every_n = 500,
          eval_metric = fun,
          maximize = maximize,
          early_stopping_rounds = early_stopping_rounds,
          nrounds = nrounds,
          nthread = nthread
        ))
        sink()
        if (inherits(model, "try-error")) {
          stop(model)
        }
        out[[paste0(i, "-", j)]] <- as.data.frame(c(
          nrounds = nrounds, best_iteration = model$best_iteration,
          model$evaluation_log[model$best_iteration, -1], cv_params
        ))
      }
      incProgress(1 / nitt, detail = paste("\nCompleted run", i, "out of", nitt))
    }
  })

  out <- bind_rows(out)
  out[order(out[[5]], decreasing = FALSE), ]
}

#' Plot method for the gbt_survival function
#'
#' @param x Return value from \code{\link{gbt_survival}}
#' @param plots A vector indicating which plots to generate ("km" for Kaplan-Meier)
#' @param incl Variables to include in the Kaplan-Meier plot
#' @param evar_values A list where each element is a vector of values for the corresponding variable in `incl` to be plotted
#' @param ... Further arguments passed to or from other methods
#'
#' @examples
#' result <- gbt_survival(
#'   lung, "time", "status", c("age", "sex", "ph.ecog"),
#'   early_stopping_rounds = 0, nthread = 1
#' )
#' plot(result, plots = c("km"), incl = c("age", "sex"), evar_values = list(age = c(60, 70), sex = c(1)))
#' @export
plot.gbt_survival <- function(x, plots = "", incl = NULL, evar_values = list(), cox_regression = FALSE, ...) {
  if (is.character(x) || !inherits(x$model, "xgb.Booster")) {
    return(x)
  }
  plot_list <- list()
  ncol <- 1

  # Load necessary libraries
  library(survival)
  library(ggplot2)
  library(patchwork)
  library(xgboost)
  library(survminer)  # for ggsurvplot

  # Extract data and model
  dataset <- x$dataset
  time_var <- x$time_var
  status_var <- x$status_var
  model <- x$model

  if ("km" %in% plots) {
    surv_obj <- Surv(time = dataset[[time_var]], event = dataset[[status_var]])

    if (cox_regression) {
      # Create a single Cox regression model using all included variables
      cox_fit <- x$cox_model

      for (evar in incl) {
        unique_values <- unique(dataset[[evar]])

        # Create new data frames with unique values of the covariate of interest
        new_data_list <- lapply(unique_values, function(val) {
          new_data <- dataset[1, , drop = FALSE]
          new_data[rep(1, length(unique_values)), ]
          new_data[[evar]] <- val
          new_data
        })

        combined_new_data <- do.call(rbind, new_data_list)
        fit <- survfit(cox_fit, newdata = combined_new_data)

        # Prepare labels for the legend
        legend_labs <- paste(evar, "=", unique_values)

        # Plot survival curves
        ggsurv <- ggsurvplot(fit,
                             conf.int = TRUE,
                             legend.labs = legend_labs,
                             ggtheme = theme_minimal(),
                             data = combined_new_data)

        # Customize plot
        cox_plot <- ggsurv$plot +
          labs(title = paste("Cox Regression Model: ", evar), x = "Time", y = "Survival Probability") +
          geom_hline(yintercept = 0.5, linetype = "dotted", color = "blue", linewidth = 1)

        plot_list[[paste("cox_regression", evar, sep = "_")]] <- cox_plot
      }
    } else {
      # Add surf.i plot for non-Cox regression, split by variables
      for (evar in incl) {
        values <- evar_values[[evar]]
        if (is.null(values)) {
          values <- unique(dataset[[evar]])
        }

        surf_df <- data.frame()

        for (val in values) {
          subset_data <- dataset[dataset[[evar]] == val, ]

          pred.train <- log(predict(x$best_model, x$best_model$dtrain))
          pred.test <- log(predict(x$best_model, xgboost::xgb.DMatrix(data = as.matrix(subset_data[, setdiff(names(subset_data), c(time_var, status_var))]))))
          time_interest <- sort(unique(x$train_data$new_time[x$train_data$status == 1]))
          basehaz_cum <- basehaz.gbm(x$train_data$time, x$train_data$status, pred.train, t.eval = time_interest, cumulative = TRUE)
          surf.i <- exp(-exp(pred.test[1]) * basehaz_cum)

          if (length(surf.i) != length(basehaz_cum)) {
            warning("Length of surf.i and basehaz_cum do not match. Adjusting lengths.")
            min_length <- min(length(surf.i), length(basehaz_cum))
            surf.i <- surf.i[1:min_length]
            basehaz_cum <- basehaz_cum[1:min_length]
          }

          surf_df <- rbind(surf_df, data.frame(Time = time_interest[1:length(surf.i)], SurvivalProbability = surf.i, Value = as.factor(val)))
        }

        surf_plot <- ggplot(surf_df, aes(x = Time, y = SurvivalProbability, color = Value)) +
          geom_line() +
          labs(title = paste("Survival Probability over Time (XGB Model):", evar), x = "Time", y = "Survival Probability") +
          theme_minimal() +
          geom_hline(yintercept = 0.5, linetype = "dotted", color = "blue", linewidth = 1) +
          scale_color_discrete(name = evar)

        plot_list[[paste("surf_i", evar, sep = "_")]] <- surf_plot
      }
    }
    ncol <- max(ncol, 2)
  }

  if ("importance" %in% plots) {
    importance <- xgb.importance(model = model)

    importance_plot <- ggplot(importance, aes(x = reorder(Feature, Gain), y = Gain)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(x = "Variable", y = "Importance (Gain)", title = "Feature Importance using XGBoost") +
      theme_minimal() +
      theme(
        axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(size = 14),
        plot.title = element_text(size = 20)
      )

    plot_list[["importance"]] <- importance_plot
    ncol <- max(ncol, 1)
  }

  if (length(plot_list) > 0) {
    if (length(plot_list) == 1 && "importance" %in% names(plot_list)) {
      return(plot_list[["importance"]])
    } else {
      combined_plot <- wrap_plots(plot_list, ncol = ncol)
      return(combined_plot)
    }
  } else {
    message("No plots generated. Please specify the plots to generate using the 'plots' argument.")
  }
}













