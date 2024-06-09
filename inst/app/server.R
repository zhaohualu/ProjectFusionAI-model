if (isTRUE(getOption("radiant.from.package"))) {
  library(radiant.model)
}

shinyServer(function(input, output, session) {

  ## source shared functions
  source(file.path(getOption("radiant.path.data"), "app/init.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source(file.path(getOption("radiant.path.data"), "app/radiant.R"), encoding = getOption("radiant.encoding"), local = TRUE)
  source("help.R", encoding = getOption("radiant.encoding"), local = TRUE)
  source("radiant.R", encoding = getOption("radiant.encoding"), local = TRUE)

  ## help ui
  output$help_model_ui <- renderUI({
    sidebarLayout(
      sidebarPanel(
        help_data_panel,
        help_model_panel,
        uiOutput("help_text"),
        width = 3
      ),
      mainPanel(
        HTML(paste0("<h2>Select help files to show and search</h2><hr>")),
        htmlOutput("help_data"),
        htmlOutput("help_model")
      )
    )
  })

  ## packages to use for example data
  options(radiant.example.data = c("radiant.data", "radiant.model"))

  ## source data & app tools from radiant.data
  for (file in list.files(
    c(
      file.path(getOption("radiant.path.data"), "app/tools/app"),
      file.path(getOption("radiant.path.data"), "app/tools/data")
    ),
    pattern = "\\.(r|R)$", full.names = TRUE
  ))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## 'sourcing' package functions in the server.R environment for development
  if (!isTRUE(getOption("radiant.from.package"))) {
    for (file in list.files("../../R", pattern = "\\.(r|R)$", full.names = TRUE)) {
      source(file, encoding = getOption("radiant.encoding"), local = TRUE)
    }
    cat("\nGetting radiant.model from source ...\n")
  }

  ## source analysis tools for model menu
  for (file in list.files(c("tools/analysis"), pattern = "\\.(r|R)$", full.names = TRUE))
    source(file, encoding = getOption("radiant.encoding"), local = TRUE)

  ## save state on refresh or browser close
  saveStateOnRefresh(session)

  ## list of function arguments
  crtree_args <- as.list(formals(crtree))

  ## list of function inputs selected by user
  crtree_inputs <- reactive({
    ## loop needed because reactive values don't allow single bracket indexing
    crtree_args$data_filter <- if (input$show_filter) input$data_filter else ""
    crtree_args$arr <- if (input$show_filter) input$data_arrange else ""
    crtree_args$rows <- if (input$show_filter) input$data_rows else ""
    crtree_args$dataset <- input$dataset

    for (i in r_drop(names(crtree_args))) {
      crtree_args[[i]] <- input[[paste0("crtree_", i)]]
    }

    crtree_args$cp <- as.numeric(unlist(strsplit(input$crtree_cp, ",")))
    crtree_args$nodes <- as.numeric(unlist(strsplit(input$crtree_nodes, ",")))
    crtree_args$prior <- as.numeric(unlist(strsplit(input$crtree_prior, ",")))
    crtree_args$cost <- as.numeric(unlist(strsplit(input$crtree_cost, ",")))
    crtree_args$margin <- as.numeric(unlist(strsplit(input$crtree_margin, ",")))
    crtree_args
  })

  crtree_pred_args <- as.list(if (exists("predict.crtree")) {
    formals(predict.crtree)
  } else {
    formals(radiant.model:::predict.crtree)
  })

  # list of function inputs selected by user
  crtree_pred_inputs <- reactive({
    # loop needed because reactive values don't allow single bracket indexing
    for (i in names(crtree_pred_args)) {
      crtree_pred_args[[i]] <- input[[paste0("crtree_", i)]]
    }

    crtree_pred_args$pred_cmd <- crtree_pred_args$pred_data <- ""
    if (input$crtree_predict == "cmd") {
      crtree_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$crtree_pred_cmd) %>%
        gsub(";\\s+", ";", .) %>%
        gsub("\"", "\'", .)
    } else if (input$crtree_predict == "data") {
      crtree_pred_args$pred_data <- input$crtree_pred_data
    } else if (input$crtree_predict == "datacmd") {
      crtree_pred_args$pred_cmd <- gsub("\\s{2,}", " ", input$crtree_pred_cmd) %>%
        gsub(";\\s+", ";", .) %>%
        gsub("\"", "\'", .)
      crtree_pred_args$pred_data <- input$crtree_pred_data
    }
    crtree_pred_args
  })

  crtree_plot_args <- as.list(if (exists("plot.crtree")) {
    formals(plot.crtree)
  } else {
    formals(radiant.model:::plot.crtree)
  })

  ## list of function inputs selected by user
  crtree_plot_inputs <- reactive({
    ## loop needed because reactive values don't allow single bracket indexing
    for (i in names(crtree_plot_args)) {
      crtree_plot_args[[i]] <- input[[paste0("crtree_", i)]]
    }
    crtree_plot_args
  })

  crtree_pred_plot_args <- as.list(if (exists("plot.model.predict")) {
    formals(plot.model.predict)
  } else {
    formals(radiant.model:::plot.model.predict)
  })

  # list of function inputs selected by user
  crtree_pred_plot_inputs <- reactive({
    # loop needed because reactive values don't allow single bracket indexing
    for (i in names(crtree_pred_plot_args)) {
      crtree_pred_plot_args[[i]] <- input[[paste0("crtree_", i)]]
    }
    crtree_pred_plot_args
  })

  output$ui_crtree_rvar <- renderUI({
    req(input$crtree_type)

    withProgress(message = "Acquiring variable information", value = 1, {
      if (input$crtree_type == "classification") {
        vars <- two_level_vars()
      } else {
        isNum <- .get_class() %in% c("integer", "numeric", "ts")
        vars <- varnames()[isNum]
      }
    })
    selectInput(
      inputId = "crtree_rvar", label = "Response variable:", choices = vars,
      selected = state_single("crtree_rvar", vars), multiple = FALSE
    )
  })

  output$ui_crtree_lev <- renderUI({
    req(input$crtree_type == "classification")
    req(available(input$crtree_rvar))
    levs <- .get_data()[[input$crtree_rvar]] %>%
      as.factor() %>%
      levels()

    selectInput(
      inputId = "crtree_lev", label = "Choose level:",
      choices = levs, selected = state_init("crtree_lev")
    )
  })

  output$ui_crtree_evar <- renderUI({
    req(available(input$crtree_rvar))
    vars <- varnames()
    if (length(vars) > 0) {
      vars <- vars[-which(vars == input$crtree_rvar)]
    }

    init <- if (input$crtree_type == "classification") input$logit_evar else input$reg_evar
    selectInput(
      inputId = "crtree_evar", label = "Explanatory variables:", choices = vars,
      selected = state_multiple("crtree_evar", vars, init),
      multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
    )
  })

  output_incl <- function(model) {
    output[[glue("ui_{model}_incl")]] <- renderUI({
      req(available(input[[glue("{model}_evar")]]))
      vars <- input[[glue("{model}_evar")]]
      id <- glue("{model}_incl")
      selectInput(
        inputId = id, label = "Explanatory variables to include:", choices = vars,
        selected = state_multiple(id, vars, c()),
        multiple = TRUE, size = min(10, length(vars)), selectize = FALSE
      )
    })
  }

  output_incl_int <- function(model) {
    output[[glue("ui_{model}_incl_int")]] <- renderUI({
      req(available(input[[glue("{model}_evar")]]))
      choices <- character(0)
      vars <- input[[glue("{model}_evar")]]
      id <- glue("{model}_incl_int")
      ## list of explanatory variables
      if (length(vars) > 1) {
        choices <- .combn(vars, 2) %>%
          apply(2, function(x) paste(x, collapse = ":"))
      }
      ## add second degree interactions
      if (length(vars) > 3) {
        choices <- c(choices, .combn(vars, 3) %>%
                       apply(2, function(x) paste(x, collapse = ":")))
      }
      selectInput(
        inputId = id, label = "Interactions to include:", choices = choices,
        selected = state_multiple(id, choices, c()),
        multiple = TRUE, size = min(10, length(choices)), selectize = FALSE
      )
    })
  }

  output_incl("crtree")
  output_incl_int("crtree")
  output_incl("reg")
  output_incl_int("reg")

  crtree_r <- reactive({
    crtree_args <- crtree_inputs()
    crtree_args$dataset <- getdata(crtree_args$dataset)

    if (is.character(crtree_args$dataset)) {
      return("Dataset not available. Please select a different dataset.")
    }

    if (!is.null(crtree_args$prior)) {
      ## set the prior probabilities
      names(crtree_args$prior) <- levels(factor(crtree_args$dataset[[crtree_args$rvar]]))
    }

    if (!is.null(crtree_args$cost)) {
      ## create a cost matrix
      costs <- matrix(
        crtree_args$cost, nrow = length(crtree_args$cost), ncol = length(crtree_args$cost), byrow = TRUE
      )
      diag(costs) <- 0
      dimnames(costs) <- list(levels(factor(crtree_args$dataset[[crtree_args$rvar]])))
      crtree_args$cost <- costs
    }

    withProgress(message = "Estimating model", value = 1, {
      do.call(crtree, crtree_args)
    })
  })

  crtree_pred_r <- reactive({
    crtree_pred_args <- crtree_pred_inputs()
    crtree_pred_args$dataset <- getdata(crtree_pred_args$pred_data)

    if (is.character(crtree_pred_args$dataset)) {
      return("Dataset not available. Please select a different dataset.")
    }

    crtree_pred_args$model <- crtree_r()
    if (is.character(crtree_pred_args$model)) {
      return(crtree_pred_args$model)
    }

    withProgress(message = "Generating predictions", value = 1, {
      do.call(predict.crtree, crtree_pred_args)
    })
  })

  crtree_plot_r <- reactive({
    crtree_plot_args <- crtree_plot_inputs()
    crtree_plot_args$model <- crtree_r()
    if (is.character(crtree_plot_args$model)) {
      return(crtree_plot_args$model)
    }

    withProgress(message = "Creating plot", value = 1, {
      do.call(plot.crtree, crtree_plot_args)
    })
  })

  crtree_pred_plot_r <- reactive({
    crtree_pred_plot_args <- crtree_pred_plot_inputs()
    crtree_pred_plot_args$model <- crtree_r()
    if (is.character(crtree_pred_plot_args$model)) {
      return(crtree_pred_plot_args$model)
    }

    crtree_pred_plot_args$pred <- crtree_pred_r()
    if (is.character(crtree_pred_plot_args$pred)) {
      return(crtree_pred_plot_args$pred)
    }

    withProgress(message = "Creating plot", value = 1, {
      do.call(plot.model.predict, crtree_pred_plot_args)
    })
  })

  output$crtree_plot <- renderPlot({
    validate(need(crtree_plot_r(), message = FALSE))
    crtree_plot_r()
  })

  output$crtree_pred_plot <- renderPlot({
    validate(need(crtree_pred_plot_r(), message = FALSE))
    crtree_pred_plot_r()
  })

  output$crtree_pred <- renderPrint({
    validate(need(crtree_pred_r(), message = FALSE))
    crtree_pred_r()
  })

  ## save crtree model as rds file
  observeEvent(input$crtree_store_res, {
    if (is.character(crtree_r())) {
      return()
    }
    saveRDS(crtree_r(), file.path("models", paste0(input$crtree_store_res_name, ".rds")))
  })

  ## save crtree predictions as csv file
  observeEvent(input$crtree_store_pred, {
    if (is.character(crtree_pred_r())) {
      return()
    }
    write.csv(crtree_pred_r(), file.path("data", paste0(input$crtree_store_pred_name, ".csv")), row.names = FALSE)
  })

  ## load model from rds file
  observeEvent(input$crtree_load_res, {
    if (file.exists(file.path("models", paste0(input$crtree_load_res_name, ".rds")))) {
      crtree_r <- readRDS(file.path("models", paste0(input$crtree_load_res_name, ".rds")))
    }
  })

})
