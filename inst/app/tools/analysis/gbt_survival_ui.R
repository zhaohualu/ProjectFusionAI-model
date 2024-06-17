## list of function arguments
gbt_survival_args <- as.list(formals(gbt_survival))

## list of function inputs selected by user
gbt_survival_inputs <- reactive({
  ## loop needed because reactive values don't allow single bracket indexing
  gbt_survival_args$data_filter <- if (input$show_filter) input$data_filter else ""
  gbt_survival_args$arr <- if (input$show_filter) input$data_arrange else ""
  gbt_survival_args$rows <- if (input$show_filter) input$data_rows else ""
  gbt_survival_args$dataset <- input$dataset
  for (i in r_drop(names(gbt_survival_args))) {
    gbt_survival_args[[i]] <- input[[paste0("gbt_survival_", i)]]
  }
  gbt_survival_args
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
      )
    )
  )
})

.gbt_survival <- eventReactive(input$gbt_survival_run, {
  gbti <- gbt_survival_inputs()
  gbti$envir <- r_data
  withProgress(
    message = "Estimating model", value = 1,
    do.call(gbt_survival, gbti)
  )
})

.summary_gbt_survival <- reactive({
  req(input$gbt_survival_run)
  gbt_result <- .gbt_survival()

  # Check if gbt_result is a character string indicating an error
  if (is.character(gbt_result)) {
    return(gbt_result)  # Return the error message
  }

  # Proceed if gbt_result is not a character string
  req(!is.null(gbt_result) && length(gbt_result) > 0)
  summary(gbt_result)
})

## output is called from the main radiant ui.R
output$gbt_survival <- renderUI({
  register_print_output("summary_gbt_survival", ".summary_gbt_survival")

  ## separate tab for summary
  gbt_survival_output_panels <- tabsetPanel(
    id = "tabs_gbt_survival",
    tabPanel(
      "Summary",
      verbatimTextOutput("summary_gbt_survival")
    )
  )

  stat_tab_panel(
    menu = "Model > Survival",
    tool = "Gradient Boosted Trees for Survival Analysis",
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




