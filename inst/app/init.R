import_fs("radiant.model", libs = "nnet", incl = "nnet.formula")

## urls for menu
r_url_list <- getOption("radiant.url.list")
r_url_list[["Linear regression (OLS)"]] <-
  list("tabs_regress" = list(
    "Summary" = "model/regress/",
    "Predict" = "model/regress/predict/",
    "Plot" = "model/regress/plot/"
  ))
r_url_list[["Logistic regression (GLM)"]] <-
  list("tabs_logistic" = list(
    "Summary" = "model/logistic/",
    "Predict" = "model/logistic/predict/",
    "Plot" = "model/logistic/plot/"
  ))
#r_url_list[["Multinomial logistic regression (MNL)"]] <-
#list("tabs_mnl" = list(
#"Summary" = "model/mnl/",
#"Predict" = "model/mnl/predict/",
#"Plot" = "model/mnl/plot/"
#))
#r_url_list[["Naive Bayes"]] <-
# list("tabs_nb" = list(
#"Summary" = "model/nb/",
#"Predict" = "model/nb/predict/",
#"Plot" = "model/nb/plot/"
#))
#r_url_list[["Neural Network"]] <-
# list("tabs_nn" = list(
#"Summary" = "model/nn/",
#"Predict" = "model/nn/predict/",
#"Plot" = "model/nn/plot/"
#))
r_url_list[["Classification and regression trees"]] <-
  list("tabs_crtree" = list(
    "Summary" = "model/crtree/",
    "Predict" = "model/crtree/predict/",
    "Plot" = "model/crtree/plot/"
  ))
r_url_list[["Random Forest"]] <-
  list("tabs_rf" = list(
    "Summary" = "model/rf/",
    "Predict" = "model/rf/predict/",
    "Plot" = "model/rf/plot/"
  ))
r_url_list[["Gradient Boosted Trees"]] <-
  list("tabs_gbt" = list(
    "Summary" = "model/gbtf/",
    "Predict" = "model/gbt/predict/",
    "Plot" = "model/gbt/plot/"
  ))
options(radiant.url.list = r_url_list)
rm(r_url_list)

## model menu
options(
  radiant.model_ui =
    tagList(
      navbarMenu(
        "Model",
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "www_model/style.css"),
          tags$script(src = "www_model/js/store.js")
        ),
        "Estimate",
        tabPanel("Linear regression (OLS)", uiOutput("regress")),
        tabPanel("Logistic regression (GLM)", uiOutput("logistic")),
        #tabPanel("Multinomial logistic regression (MNL)", uiOutput("mnl")),
        #tabPanel("Naive Bayes", uiOutput("nb")),
        #tabPanel("Neural Network", uiOutput("nn")),
        "----", "Trees",
        tabPanel("Classification and regression trees", uiOutput("crtree")),
        tabPanel("Random Forest", uiOutput("rf")),
        tabPanel("Gradient Boosted Trees", uiOutput("gbt"))
      )
    )
)
