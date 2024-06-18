gbt_survival_args <- as.list(formals(gbt_survival))

#list of function inputs selected by user
gbt_survival_inputs <- reactive({
  #loop needed because reactive values don't allow single bracket indexing
  gbt_survival_args$data_filter <- if (input$show_filter) input$data_filter else ""
  gbt_survival_args$arr <- if (input$show_filter) input$data_arrange else ""
  gbt_survival_args$rows <- if (input$show_filter) input$data_rows else ""
  gbt_survival_args$dataset <- input$dataset
  for (i in r_drop(names(gbt_survival_args))) {
    gbt_survival_args[[i]] <- input[[paste0("gbt_survival_", i)]]
  }
  gbt_survival_args
})

gbt_survival_pred_args <- as.list(if (exists("predict.gbt_survival")) {
  formals(predict.gbt_survival)
} else {
  formals(radiant.model:::predict.gbt_survival)
})

# list of function inputs selected by user
gbt_survival_pred_inputs <- reactive({
  # loop needed because reactive values don't allow single bracket indexing
  for (i in names(gbt_survival_pred_args)) {
    gbt_survival_pred_args[[i]] <- input[[paste0("gbt_survival", i)]]
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

## reset prediction and plot settings when the dataset changes
observeEvent(input$dataset, {
  updateSelectInput(session = session, inputId = "gbt_predict", selected = "none")
})


output$ui_gbt_survival <- renderUI({
  req(input$dataset)
  tagList(
    conditionalPanel(
      condition = "input.tabs_gbt_survival == 'Summary'",
      wellPanel(
        actionButton("gbt_survival_run", "Estimate model", width = "100%", icon = icon("play", verify_fa = FALSE), class = "btn-success")
      )
    ),
    wellPanel(
      conditionalPanel(
        condition = "input.tabs_gbt_survival == 'Summary'",
        uiOutput("ui_gbt_survival_time_var"),
        uiOutput("ui_gbt_survival_status_var"),
        uiOutput("ui_gbt_survival_evar"),
        uiOutput("ui_gbt_survival_wts"),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_survival_max_depth",
              label = "Max depth:", min = 1, max = 20,
              value = state_init("gbt_survival_max_depth", 6)
            ), width = "50%"),
            td(numericInput(
              "gbt_survival_learning_rate",
              label = "Learning rate:", min = 0, max = 1, step = 0.1,
              value = state_init("gbt_survival_learning_rate", 0.3)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_survival_min_split_loss",
              label = "Min split loss:", min = 0.00001, max = 1000,
              step = 0.01, value = state_init("gbt_survival_min_split_loss", 0)
            ), width = "50%"),
            td(numericInput(
              "gbt_survival_min_child_weight",
              label = "Min child weight:", min = 1, max = 100,
              step = 1, value = state_init("gbt_survival_min_child_weight", 1)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_survival_subsample",
              label = "Sub-sample:", min = 0.1, max = 1,
              value = state_init("gbt_survival_subsample", 1)
            ), width = "50%"),
            td(numericInput(
              "gbt_survival_nrounds",
              label = "# rounds:",
              value = state_init("gbt_survival_nrounds", 100)
            ), width = "50%")
          ),
          width = "100%"
        )),
        with(tags, table(
          tr(
            td(numericInput(
              "gbt_survival_early_stopping_rounds",
              label = "Early stopping:", min = 1, max = 10,
              step = 1, value = state_init("gbt_survival_early_stopping_rounds", 3)
            ), width = "50%"),
            td(numericInput(
              "gbt_survival_seed",
              label = "Seed:",
              value = state_init("gbt_survival_seed", 1234)
            ), width = "50%")
          ),
          width = "100%"
        ))
      ),
      conditionalPanel(
        condition = "input.tabs_gbt_survival == 'Predict'",
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
        #conditionalPanel(
        # condition = "input.gbt_survival_predict != 'none'",
        #checkboxInput("gbt_pred_plot", "Plot predictions", state_init("gbt_pred_plot", FALSE)),
        #conditionalPanel(
        # "input.gbt_pred_plot == true",
        # uiOutput("ui_gbt_predict_plot")
        #)
        #),
        ## only show if full data is used for prediction
        conditionalPanel(
          "input.gbt_survival_predict == 'data' | input.gbt_survival_predict == 'datacmd'",
          tags$table(
            tags$td(uiOutput("ui_gbt_survival_store_pred_name")),
            tags$td(actionButton("gbt_survival_store_pred", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
          )
        )
      ),
      #conditionalPanel(
      #condition = "input.tabs_survival_gbt == 'Plot'",
      #uiOutput("ui_gbt_plots"),
      #conditionalPanel(
      #condition = "input.gbt_plots == 'dashboard'",
      #uiOutput("ui_gbt_nrobs")
      #),
      #conditionalPanel(
      # condition = "input.gbt_plots == 'pdp' | input.gbt_plots == 'pred_plot'",
      #uiOutput("ui_gbt_incl"),
      #uiOutput("ui_gbt_incl_int")
      #)
      #),
      # conditionalPanel(
      #   condition = "input.tabs_gbt == 'Summary'",
      #   tags$table(
      #     tags$td(uiOutput("ui_gbt_store_res_name")),
      #     tags$td(actionButton("gbt_store_res", "Store", icon = icon("plus", verify_fa = FALSE)), class = "top")
      #   )
      # )
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
  } else if (not_available(input$gbt_survival_status_var)) { "This analysis requires a status variable (event indicator). If these variables are not available, please select another dataset.\n\n" %>%
      suggest_data("survival_dataset_example")
  } else if (not_available(input$gbt_survival_evar)) {"Please select one or more explanatory variables.\n\n" %>%
      suggest_data("survival_dataset_example")
  } else { "available"}
})

.gbt_survival <- eventReactive(input$gbt_survival_run, {
  gbti <- gbt_survival_inputs()
  gbti$envir <- r_data
  if (is.empty(gbti$max_depth)) gbti$max_depth <- 6
  if (is.empty(gbti$learning_rate)) gbti$learning_rate <- 0.3
  if (is.empty(gbti$min_split_loss)) gbti$min_split_loss <- 0.01
  if (is.empty(gbti$min_child_weight)) gbti$min_child_weight <- 1
  if (is.empty(gbti$subsample)) gbti$subsample <- 1
  if (is.empty(gbti$nrounds)) gbti$nrounds <- 100
  if (is.empty(gbti$early_stopping_rounds)) gbti["early_stopping_rounds"] <- list(NULL)

  withProgress(
    message = "Estimating model", value = 1,
    do.call(gbt_survival, gbti)
  )
})

.summary_gbt_survival <- reactive({
  if (not_pressed(input$gbt_survival_run)) {
    return("** Press the Estimate button to estimate the model **")
  }
  if (gbt_survival_available() != "available") {
    return(gbt_survival_available())
  }
  summary(.gbt_survival())
})

.predict_gbt_survival <- reactive({
  if (not_pressed(input$gbt_survival_run)) {
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

output$gbt_survival <- renderUI({
  register_print_output("summary_gbt_survival", ".summary_gbt_survival")
  register_print_output("predict_gbt_survival", ".predict_print_gbt_survival")

  ## three separate tabs
  gbt_survival_output_panels <- tabsetPanel(
    id = "tabs_gbt_survival",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_gbt_survival")
    ),
    tabPanel(
      "Predict",
      conditionalPanel(
        "input.gbt_survival_pred_plot == true",
        download_link("dlp_gbt_survival_pred"),
        plotOutput("predict_plot_gbt_survival", width = "100%", height = "100%")
      ),
      download_link("dl_gbt_survival_pred"), br(),
      verbatimTextOutput("predict_gbt_survival")
    ),
    tabPanel(
      "Plot",
      download_link("dlp_gbt_survival"),
      plotOutput("plot_gbt", width = "100%", height = "100%")
    )
  )

  stat_tab_panel(
    menu = "Model > Trees",
    tool = "Gradient Boosted Trees",
    tool_ui = "ui_gbt_survival",
    output_panels = gbt_survival_output_panels
  )
})


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
  removeModal() ## remove shiny modal after save
})



