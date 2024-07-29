# Model selection options
model_options <- c(
  "XGBoost" = "xgboost",
  "Cox Regression" = "cox",
  "Random Forest" = "rf"
)

# Gradient Boosted Trees plots
gbt_survival_plots <- c(
  "None" = "none",
  "Predicted Survival Curve" = "km",
  "Permutation Importance" = "importance",
  "ROC Curve" = "roc",
  "Brier Curve" = "brier"
)

gbt_survival_args <- as.list(formals(gbt_survival))

# List of function inputs selected by user
gbt_survival_inputs <- reactive({
  gbt_survival_args$data_filter <- if (input$show_filter) input$data_filter else ""
  gbt_survival_args$arr <- if (input$show_filter) input$data_arrange else ""
  gbt_survival_args$rows <- if (input$show_filter) input$data_rows else ""
  gbt_survival_args$dataset <- input$dataset
  gbt_survival_args$cox_regression <- (input$model_selection == "cox")
  gbt_survival_args$random_forest <- (input$model_selection == "rf")
  gbt_survival_args$time_var <- input$gbt_survival_time_var
  gbt_survival_args$status_var <- input$gbt_survival_status_var

  for (i in r_drop(names(gbt_survival_args))) {
    if (i %in% c("max_depth", "learning_rate", "min_split_loss", "min_child_weight", "subsample", "nrounds", "ntree", "mtry", "nodesize", "nsplit")) {
      gbt_survival_args[[i]] <- as.numeric(unlist(strsplit(input[[paste0("gbt_survival_", i)]], ",")))
    } else {
      gbt_survival_args[[i]] <- input[[paste0("gbt_survival_", i)]]
    }
  }
  gbt_survival_args
})

gbt_survival_pred_args <- as.list(if (exists("predict.gbt_survival")) {
  formals(predict.gbt_survival)
} else {
  formals(radiant.model:::predict.gbt_survival)
})
gbt_survival_plot_args <- as.list(if (exists("plot.gbt_survival")) {
  formals(plot.gbt_survival)
} else {
  formals(radiant.model:::plot.gbt_survival)
})

# List of function inputs selected by user
gbt_survival_plot_inputs <- reactive({
  for (i in names(gbt_survival_plot_args)) {
    gbt_survival_plot_args[[i]] <- input[[paste0("gbt_survival_", i)]]
  }
  gbt_survival_plot_args
})

# List of function inputs selected by user
gbt_survival_pred_inputs <- reactive({
  for (i in names(gbt_survival_pred_args)) {
    gbt_survival_pred_args[[i]] <- input[[paste0("gbt_survival_", i)]]
  }

  gbt_survival_pred_args$pred_cmd <- gbt_survival_pred_args$pred_data <- ""
  if (input$gbt_survival_predict == "cmd") {
    gbt_survival_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$gbt_survival_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
  } else if (input$gbt_survival_predict == "data") {
    gbt_survival_pred_args$pred_data <- input$gbt_survival_pred_data
  } else if (input$gbt_survival_predict == "datacmd") {
    gbt_survival_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$gbt_survival_pred_cmd) %>%
      gsub(";\\s+", ";", .) %>%
      gsub("\"", "\'", .)
    gbt_survival_pred_args$pred_data <- input$gbt_survival_pred_data
  }
  gbt_survival_pred_args$evar <- input$gbt_survival_evar
  gbt_survival_pred_args$cox_regression <- if (input$model_selection == "cox") TRUE else FALSE
  gbt_survival_pred_args$random_forest <- if (input$model_selection == "rf") TRUE else FALSE
  gbt_survival_pred_args$status_var <- input$gbt_survival_status_var
  gbt_survival_pred_args
})

output$ui_gbt_survival_time_var <- renderUI({
  req(input$dataset)
  vars <- varnames()
  selectInput(
    inputId = "gbt_survival_time_var",
    label = "Time variable:",
    choices = vars,
    selected = state_single("gbt_survival_time_var", vars),
    multiple = FALSE
  )
})

output$ui_gbt_survival_status_var <- renderUI({
  req(input$dataset)
  vars <- varnames()
  selectInput(
    inputId = "gbt_survival_status_var",
    label = "Status variable:",
    choices = vars,
    selected = state_single("gbt_survival_status_var", vars),
    multiple = FALSE
  )
})

output$ui_gbt_survival_evar <- renderUI({
  req(input$dataset)
  vars <- varnames()
  selectInput(
    inputId = "gbt_survival_evar",
    label = "Explanatory variables:",
    choices = vars,
    selected = state_multiple("gbt_survival_evar", vars),
    multiple = TRUE,
    size = min(10, length(vars)),
    selectize = FALSE
  )
})

output$ui_gbt_survival_wts <- renderUI({
  req(input$dataset)
  vars <- varnames()
  vars <- c("None", vars)
  selectInput(
    inputId = "gbt_survival_wts",
    label = "Weights:",
    choices = vars,
    selected = state_single("gbt_survival_wts", vars),
    multiple = FALSE
  )
})

output$ui_gbt_survival_store_pred_name <- renderUI({
  init <- state_init("gbt_survival_store_pred_name", "pred_gbt_survival")
  textInput(
    "gbt_survival_store_pred_name",
    "Store predictions:",
    init
  )
})

output$ui_gbt_survival_plots <- renderUI({
  req(input$dataset)
  selectInput("gbt_survival_plots", "Plot type:", choices = gbt_survival_plots, selected = "None")
})

output$ui_incl <- renderUI({
  req(input$dataset)
  vars <- varnames()
  evar <- input$gbt_survival_evar  # Get evar from the input
  selected_vars <- if (!is.null(evar) && length(evar) > 0) evar else vars[1]
  selectInput("incl", "Variables to include in KM plot:", choices = vars, selected = selected_vars, multiple = TRUE)
})

output$ui_evar_values <- renderUI({
  req(input$dataset)
  vars <- input$incl
  lapply(vars, function(var) {
    textInput(paste0("evar_values_", var), paste("Values for", var, ":"), value = "")
  })
})

# Reset prediction and plot settings when the dataset changes or model selection changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "gbt_survival_predict", selected = "none")
  updateSelectInput(session = session, inputId = "gbt_survival_plots", selected = "none")
})

output$ui_create_plot_button <- renderUI({
  actionButton("create_plot", "Create plot", icon = icon("play"), class = "btn-primary")
})

output$ui_gbt_survival <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_gbt_survival == 'Model Summary'",
      wellPanel(
        selectInput("model_selection", "Model selection:",
                    choices = model_options,
                    selected = "xgboost"),
        actionButton("gbt_survival_run", "Estimate model", width = "100%", icon = icon("play"), class = "btn-success")
      ),
      wellPanel(
        uiOutput("ui_gbt_survival_time_var"),
        uiOutput("ui_gbt_survival_status_var"),
        uiOutput("ui_gbt_survival_evar"),
        uiOutput("ui_gbt_survival_wts"),
        conditionalPanel(
          condition = "input.model_selection == 'xgboost'",
          tagList(
            textInput("gbt_survival_max_depth", "Max depth (comma-separated):", "6"),
            textInput("gbt_survival_learning_rate", "Learning rate (comma-separated):", "0.3"),
            textInput("gbt_survival_min_split_loss", "Min split loss (comma-separated):", "0"),
            textInput("gbt_survival_min_child_weight", "Min child weight (comma-separated):", "1"),
            textInput("gbt_survival_subsample", "Sub-sample (comma-separated):", "1"),
            textInput("gbt_survival_nrounds", "# rounds (comma-separated):", "100"),
            numericInput(
              "gbt_survival_early_stopping_rounds",
              label = "Early stopping:", min = 1, max = 10,
              step = 1, value = state_init("gbt_survival_early_stopping_rounds", 3)
            ),
            numericInput(
              "gbt_survival_seed",
              label = "Seed:",
              value = state_init("gbt_survival_seed", 1234)
            )
          )
        ),
        conditionalPanel(
          condition = "input.model_selection == 'cox'",
          tagList(
            numericInput(
              "gbt_survival_seed",
              label = "Seed:",
              value = state_init("gbt_survival_seed", 1234)
            )
          )
        ),
        conditionalPanel(
          condition = "input.model_selection == 'rf'",
          tagList(
            textInput("gbt_survival_ntree", "Number of trees (comma-separated):", "500"),
            textInput("gbt_survival_mtry", "Number of variables to try at each split (comma-separated):", "3"),
            textInput("gbt_survival_nodesize", "Minimum size of terminal nodes (comma-separated):", "15"),
            textInput("gbt_survival_nsplit", "Number of random splits to consider at each node (comma-separated):", "10"),
            numericInput(
              "gbt_survival_seed",
              label = "Seed:",
              value = state_init("gbt_survival_seed", 1234)
            )
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabs_gbt_survival == 'Predict'",
      wellPanel(
        selectInput(
          "gbt_survival_predict",
          label = "Prediction input type:", reg_predict,
          selected = state_single("gbt_survival_predict", reg_predict, "none")
        ),
        conditionalPanel(
          "input.gbt_survival_predict == 'data' | input.gbt_survival_predict == 'datacmd'",
          selectizeInput(
            inputId = "gbt_survival_pred_data", label = "Prediction data:",
            choices = c("None" = "", r_info[["datasetlist"]]),
            selected = state_single("gbt_survival_pred_data", c("None" = "", r_info[["datasetlist"]])),
            multiple = FALSE
          )
        ),
        conditionalPanel(
          "input.gbt_survival_predict == 'cmd' | input.gbt_survival_predict == 'datacmd'",
          returnTextAreaInput(
            "gbt_survival_pred_cmd", "Prediction command:",
            value = state_init("gbt_survival_pred_cmd", ""),
            rows = 3,
            placeholder = "Type a formula to set values for model variables (e.g., carat = 1; cut = 'Ideal') and press return"
          )
        ),
        conditionalPanel(
          condition = "input.gbt_survival_predict != 'none'",
          checkboxInput("gbt_survival_pred_plot", "Plot predictions", state_init("gbt_survival_pred_plot", FALSE)),
          conditionalPanel(
            "input.gbt_survival_pred_plot == true",
            uiOutput("ui_gbt_survival_predict_plot")
          )
        ),
        # only show if full data is used for prediction
        conditionalPanel(
          "input.gbt_survival_predict == 'data' | input.gbt_survival_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_gbt_survival_store_pred_name")),
            tags$td(actionButton("gbt_survival_store_pred", "Store", icon = icon("plus")), class = "top")
          )
        )
      )
    ),
    conditionalPanel(
      condition = "input.tabs_gbt_survival == 'Model Performance'",
      wellPanel(
        textInput("roc_times", "ROC Times (comma-separated):", "300"),  # Text input for ROC times
      )
    ),
    conditionalPanel(
      condition = "input.tabs_gbt_survival == 'Plot'",
      wellPanel(
        uiOutput("ui_gbt_survival_plots"),
        uiOutput("ui_create_plot_button"),
        conditionalPanel(
          condition = "input.gbt_survival_plots == 'km'",
          uiOutput("ui_incl"),
          uiOutput("ui_evar_values")
        )
      )
    ),
    help_and_report(
      modal_title = "Gradient Boosted Trees",
      fun_name = "gbt_survival",
      help_file = inclMD(file.path(getOption("radiant.path.model"), "app/tools/help/gbt_survival.Rmd"))
    )
  )
})


gbt_survival_available <- reactive({
  req(input$dataset)
  if (not_available(input$gbt_survival_time_var)) {
    "This analysis requires a time variable. If these variables are not available, please select another dataset.\n\n" %>%
      suggest_data("survival_dataset_example")
  } else if (not_available(input$gbt_survival_status_var)) {
    "This analysis requires a status variable (event indicator). If these variables are not available, please select another dataset.\n\n" %>%
      suggest_data("survival_dataset_example")
  } else if (not_available(input$gbt_survival_evar)) {
    "Please select one or more explanatory variables.\n\n" %>%
      suggest_data("survival_dataset_example")
  } else {
    "available"
  }
})

.gbt_survival <- reactiveVal(NULL)
model_estimated <- reactiveVal(FALSE)
observeEvent(input$model_selection, {
  model_estimated(FALSE) # Reset the estimation status when the model type changes
})

# Event to trigger the model estimation
observeEvent(input$gbt_survival_run, {
  gbti <- gbt_survival_inputs()
  gbti$envir <- r_data
  if (input$model_selection == "cox") {
    gbti$cox_regression <- TRUE
  } else {
    gbti$cox_regression <- FALSE
  }
  if (input$model_selection == "rf") {
    gbti$random_forest <- TRUE
  } else {
    gbti$random_forest <- FALSE
  }
  if (is.empty(gbti$max_depth)) gbti$max_depth <- 6
  if (is.empty(gbti$learning_rate)) gbti$learning_rate <- 0.3
  if (is.empty(gbti$min_split_loss)) gbti$min_split_loss <- 0.01
  if (is.empty(gbti$min_child_weight)) gbti$min_child_weight <- 1
  if (is.empty(gbti$subsample)) gbti$subsample <- 1
  if (is.empty(gbti$nrounds)) gbti$nrounds <- 100
  if (is.empty(gbti$early_stopping_rounds)) gbti["early_stopping_rounds"] <- list(NULL)
  if (is.empty(gbti$ntree)) gbti$ntree <- 500
  if (is.empty(gbti$mtry)) gbti$mtry <- 3
  if (is.empty(gbti$nodesize)) gbti$nodesize <- 15
  if (is.empty(gbti$nsplit)) gbti$nsplit <- 0

  withProgress(
    message = "Estimating model", value = 1,
    {
      model_result <- do.call(gbt_survival, gbti)
      .gbt_survival(model_result)
      model_estimated(TRUE)
    }
  )
})

# Trigger model re-estimation when the model selection changes
## reset prediction settings when the model type changes
observeEvent(input$cox_regression, {
  updateSelectInput(session = session, inputId = "gbt_survival_predict", selected = "none")
  updateSelectInput(session = session, inputId = "gbt_survival_plots", selected = "none")
})

observeEvent(input$random_forest, {
  updateSelectInput(session = session, inputId = "gbt_survival_predict", selected = "none")
  updateSelectInput(session = session, inputId = "gbt_survival_plots", selected = "none")
})

.summary_gbt_survival <- reactive({
  if (is.null(.gbt_survival())) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (gbt_survival_available() != "available") {
    return(gbt_survival_available())
  }
  summary(.gbt_survival())
})

.predict_gbt_survival <- reactive({
  if (is.null(.gbt_survival())) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (gbt_survival_available() != "available") {
    return(gbt_survival_available())
  }
  if (is.empty(input$gbt_survival_predict, "none")) {
    return("** Select prediction input **")
  }

  if ((input$gbt_survival_predict == "data" || input$gbt_survival_predict == "datacmd") && is.empty(input$gbt_survival_pred_data)) {
    return("** Select data for prediction **")
  }
  if (input$gbt_survival_predict == "cmd" && is.empty(input$gbt_survival_pred_cmd)) {
    return("** Enter prediction commands **")
  }

  withProgress(message = "Generating predictions", value = 1, {
    gbti <- gbt_survival_pred_inputs()
    gbti$object <- .gbt_survival()
    gbti$envir <- r_data
    do.call(predict, gbti)
  })
})

.predict_print_gbt_survival <- reactive({
  .predict_gbt_survival() %>%
    (function(x) if (is.character(x)) cat(x, "\n") else print(x))
})

gbt_survival_plot <- reactive({
  if (gbt_survival_available() != "available") {
    return()
  }
  if (is.empty(input$gbt_survival_plots, "none")) {
    return()
  }
  nr_vars <- length(input$incl)
  plot_height <- 250
  plot_width <- 700
  if ("km" %in% input$gbt_survival_plots) {
    plot_height <- max(150, ceiling(nr_vars / 2) * 150)
  }
  list(plot_width = plot_width, plot_height = plot_height)
})

gbt_survival_plot_width <- function() {
  gbt_survival_plot() %>%
    (function(x) if (is.list(x)) x$plot_width else 700)
}

gbt_survival_plot_height <- function() {
  gbt_survival_plot() %>%
    (function(x) if (is.list(x)) x$plot_height else 250)
}

output$gbt_survival <- renderUI({
  register_print_output("summary_gbt_survival", ".summary_gbt_survival")
  register_print_output("predict_gbt_survival", ".predict_print_gbt_survival")

  # four separate tabs
  gbt_survival_output_panels <- tabsetPanel(
    id = "tabs_gbt_survival",
    tabPanel(
      "Model Summary",
      verbatimTextOutput("summary_gbt_survival"),
      plotlyOutput("importance_plot_gbt_survival")
    ),
    tabPanel(
      "Model Performance",
      verbatimTextOutput("model_performance_gbt_survival"),
      plotlyOutput("brier_score_plot"),
      plotlyOutput("roc_curve_plot")     # ROC Curve Plot
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.gbt_survival_pred_plot == true",
        download_link("dlp_gbt_survival_pred"),
        plotlyOutput("predict_plot_gbt_survival", width = "80%", height = "90%")
      ),
      download_link("dl_gbt_survival_pred"), br(),
      verbatimTextOutput("predict_gbt_survival")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_gbt_survival"),
      plotlyOutput("plot_gbt_survival", width = "900px", height = "600px")
    )
  )

  stat_tab_panel(
    menu = "Model > Trees",
    tool = "Gradient Boosted Trees",
    tool_ui = "ui_gbt_survival",
    output_panels = gbt_survival_output_panels
  )
})


output$importance_plot_gbt_survival <- renderPlotly({
  req(model_estimated())  # Ensure the model has been estimated before plotting

  isolate({
    model <- .gbt_survival()
    req(!is.null(model), !is.null(model$evar))  # Check model and required variables

    is_cox <- input$model_selection == "cox"
    is_rf <- input$model_selection == "rf"

    tryCatch({
      plot(model, plots = "importance", incl = model$evar, cox_regression = is_cox, random_forest = is_rf)
    }, error = function(e) {
      NULL  # Handle errors silently
    }, warning = function(w) {
      NULL  # Handle warnings silently
    })
  })
})
output$brier_score_plot <- renderPlotly({
  req(model_estimated())  # Ensure the model has been estimated before plotting

  isolate({
    model <- .gbt_survival()
    req(!is.null(model), !is.null(model$evar))  # Check model and required variables

    tryCatch({
      plot(model, plots = "brier", incl = model$evar, cox_regression = input$model_selection == "cox", random_forest = input$model_selection == "rf")
    }, error = function(e) {
      NULL  # Handle errors silently
    }, warning = function(w) {
      NULL  # Handle warnings silently
    })
  })
})

output$roc_curve_plot <- renderPlotly({
  req(model_estimated())  # Ensure the model has been estimated before plotting

  isolate({
    model <- .gbt_survival()
    req(!is.null(model), !is.null(model$evar))  # Check model and required variables

    tryCatch({
      plot(model, plots = "roc", incl = model$evar, cox_regression = input$model_selection == "cox", random_forest = input$model_selection == "rf", roc_times = input$roc_times)
    }, error = function(e) {
      NULL  # Handle errors silently
    }, warning = function(w) {
      NULL  # Handle warnings silently
    })
  })
})


.plot_gbt_survival <- reactive({
  if (is.null(.gbt_survival())) {
    return("** Press the Estimate button to estimate the model **")
  } else if (gbt_survival_available() != "available") {
    return(gbt_survival_available())
  } else if (is.empty(input$gbt_survivxal_plots, "none")) {
    return("Please select a plot from the drop-down menu")
  }

  pinp <- gbt_survival_plot_inputs()
  pinp$shiny <- TRUE
  check_for_pdp_pred_plots("gbt_survival")
  withProgress(message = "Generating plots", value = 1, {
    do.call(plot, c(list(x = .gbt_survival()), pinp))
  })
})

# Observe event to create the plot when the button is pressed
observeEvent(input$create_plot, {
  req(input$gbt_survival_plots)
  req(input$incl)
  req(input$gbt_survival_evar)

  output$plot_gbt_survival <- renderPlotly({
    validate(
      need(input$incl, "Please select variables to include in the KM plot."),
      need(input$gbt_survival_evar, "Please select explanatory variables.")
    )
    dataset <- get(input$dataset, envir = r_data, inherits = TRUE)
    req(dataset)
    gbt_surv <- gbt_survival_inputs()
    km_plots <- input$gbt_survival_plots
    km_incl <- input$incl

    km_evar_values <- lapply(input$incl, function(var) {
      values <- input[[paste0("evar_values_", var)]]
      if (!is.null(values) && values != "") {
        as.numeric(strsplit(values, ",")[[1]])
      } else {
        NULL
      }
    })
    names(km_evar_values) <- input$incl
    km_evar_values <- km_evar_values[!sapply(km_evar_values, is.null)]

    gbt_surv$plots <- km_plots
    gbt_surv$incl <- km_incl
    gbt_surv$evar_values <- km_evar_values
    gbt_surv$dataset <- dataset

    gbt_surv$time_var <- input$gbt_survival_time_var
    gbt_surv$status_var <- input$gbt_survival_status_var
    gbt_surv$evar <- input$gbt_survival_evar
    gbt_surv$max_depth <- as.numeric(gbt_surv$max_depth)
    gbt_surv$learning_rate <- as.numeric(gbt_surv$learning_rate)
    gbt_surv$min_split_loss <- as.numeric(gbt_surv$min_split_loss)
    gbt_surv$min_child_weight <- as.numeric(gbt_surv$min_child_weight)
    gbt_surv$subsample <- as.numeric(gbt_surv$subsample)
    gbt_surv$nrounds <- as.numeric(gbt_surv$nrounds)
    gbt_surv$early_stopping_rounds <- as.numeric(gbt_surv$early_stopping_rounds)
    gbt_surv$wts <- as.numeric(gbt_surv$wts)
    gbt_surv$seed <- as.numeric(gbt_surv$seed)
    gbt_surv$data_filter <- gbt_surv$data_filter
    gbt_surv$arr <- gbt_surv$arr
    gbt_surv$rows <- gbt_surv$rows
    gbt_surv$cox_regression <- (input$model_selection == "cox")
    gbt_surv$random_forest <- (input$model_selection == "rf")
    if (km_plots == "km") {
      result <- do.call(gbt_survival, gbt_surv)
      plot(result, plots = km_plots, incl = km_incl, evar_values = km_evar_values, cox_regression = input$model_selection == "cox",
           random_forest = input$model_selection == "rf")
    } else if (km_plots == "importance") {
      result <- do.call(gbt_survival, gbt_surv)
      plot(result, plots = km_plots, incl = km_incl, evar_values = km_evar_values, cox_regression = input$model_selection == "cox",
           random_forest = input$model_selection == "rf")
    } else if (km_plots == "roc") {
      result <- do.call(gbt_survival, gbt_surv)
      plot(result, plots = km_plots, incl = km_incl, evar_values = km_evar_values, cox_regression = input$model_selection == "cox",
           random_forest = input$model_selection == "rf")
    }
  })
})

download_handler(
  id = "dlp_gbt_survival",
  fun = download_handler_plot,
  fn = function() paste0(input$dataset, "_gbt_survival"),
  type = "png",
  caption = "Save Kaplan Meier plot",
  plot = .plot_gbt_survival,
  width = gbt_survival_plot_width,
  height = gbt_survival_plot_height
)

observeEvent(input$gbt_survival_report, {
  r_info[["latest_screenshot"]] <- NULL
  gbt_survival_report()
})

observeEvent(input$gbt_survival_screenshot, {
  r_info[["latest_screenshot"]] <- NULL
  radiant_screenshot_modal("modal_gbt_survival_screenshot")
})

observeEvent(input$modal_gbt_survival_screenshot, {
  gbt_survival_report()
  removeModal() # remove shiny modal after save
})










