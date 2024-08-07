#' Summary method for the rforest function
#'
#' @details See \url{https://radiant-rstats.github.io/docs/model/rforest.html} for an example in Radiant
#'
#' @param object Return value from \code{\link{rforest}}
#' @param ... further arguments passed to or from other methods
#'
#' @examples
#' result <- rforest(titanic, "survived", "pclass", lev = "Yes")
#' summary(result)
#'
#' @seealso \code{\link{rforest}} to generate results
#' @seealso \code{\link{plot.rforest}} to plot results
#' @seealso \code{\link{predict.rforest}} for prediction
#'
#' @export
summary.rforest <- function(object, ...) {
  if (is.character(object)) {
    return(object)
  }
  cat("Random Forest (Ranger)\n")
  if (object$type == "classification") {
    cat("Type                 : Classification")
  } else {
    cat("Type                 : Regression")
  }
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
  cat("\nResponse variable    :", object$rvar)
  if (object$type == "classification") {
    cat("\nLevel                :", object$lev, "in", object$rvar)
  }
  cat("\nExplanatory variables:", paste0(object$evar, collapse = ", "), "\n")
  if (length(object$wtsname) > 0) {
    cat("Weights used         :", object$wtsname, "\n")
  }

  # Correctly access and print the best hyperparameters
  best_params <- object$best_params
  if (!is.null(best_params)) {
    cat("Mtry                 :", best_params$mtry, "\n")
    cat("Number of trees      :", best_params$num.trees, "\n")
    cat("Min node size        :", best_params$min.node.size, "\n")
    cat("Sample fraction      :", best_params$sample.fraction, "\n")
    cat("Number of threads    :", best_params$num.threads, "\n")
  } else {
    cat("Mtry                 :", object$mtry, "\n")
    cat("Number of trees      :", object$num.trees, "\n")
    cat("Min node size        :", object$min.node.size, "\n")
    cat("Sample fraction      :", object$sample.fraction, "\n")
    cat("Number of threads    :", object$num.threads, "\n")
  }

  if (length(object$extra_args)) {
    extra_args <- deparse(object$extra_args) %>%
      sub("list\\(", "", .) %>%
      sub("\\)$", "", .)
    cat("Additional arguments :", extra_args, "\n")
  }
  if (!is.empty(object$wts, "None") && (length(unique(object$wts)) > 2 || min(object$wts) >= 1)) {
    cat("Nr obs               :", format_nr(sum(object$wts), dec = 0), "\n")
  } else {
    cat("Nr obs               :", format_nr(length(object$rv), dec = 0), "\n")
  }
  if (object$type == "regression") {
    cat("R-squared            :", format_nr(object$best_model$r.squared, dec = 3), "\n")
    cat("MSE                  :", format_nr(mean((object$model$model[[object$rvar]] - object$model$predictions)^2), dec = 3), "\n")
  } else {
    accuracy <- object$best_metric
    cat("Accuracy             :", format_nr(accuracy, dec = 3), "\n")
  }
  OOB <- ifelse(object$type == "classification", object$model$prediction.error, sqrt(object$model$prediction.error))
  cat("OOB prediction error :", format_nr(OOB, dec = 3), "\n")

  ## Explanation for interpretation
  cat("\nHow to Interpret the Values:\n")
  cat("OOB prediction error: Out-of-bag (OOB) error is an internal error estimate\n")
  cat("of a random forest as it is being constructed. Lower values indicate better\n")
  cat("model performance.\n")
  if (object$type == "classification") {
    cat("Accuracy: The proportion of correctly classified instances. Higher values\n")
    cat("indicate better model performance.\n")
  } else {
    cat("MSE: Mean Squared Error measures the average of the squares of the errors.\n")
    cat("Lower values indicate better model performance.\n")
    cat("R-squared: Represents the proportion of the variance for a dependent variable\n")
    cat("that's explained by the independent variables. Values closer to 1 indicate\n")
    cat("better model performance.\n")
  }
}

