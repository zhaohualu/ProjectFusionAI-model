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










