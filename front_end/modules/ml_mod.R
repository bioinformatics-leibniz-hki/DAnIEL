#!/usr/bin/env Rscript

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#
# Interactive analysis module
#

ml_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::div(
      id = ns("interactive"),
      shiny::h2("Analyze a single model"),
      shiny::div("Mean AUC over all folds and it's standard deviation are shown in the table."),
      shiny::div("Test samples of all folds are pooled together to calculate AUC as shown in the plots."),
      shiny::br(),
      shiny::selectInput(
        inputId = ns("selected_model"),
        label = "Select model",
        choices = "not available"
      ),
      shiny::htmlOutput(ns("selected_model")),
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel(
          title = "Pooled",
          plot_mod_UI(ns("pooled_plot_mod"))
        ),
        shiny::tabPanel(
          title = "Per fold",
          plot_mod_UI(ns("folds_plot_mod"))
        )
      )
    )
  )
}

ml_mod <- function(input, output, session, project) {
  shinyjs::hide(id = "interactive")

  analysis_params <- project$params$features_params[[project$selected_params$analysis]]
  analysis_dir <- paste0(
    project$project_dir,
    "/analysis/",
    stringr::str_replace_all(project$selected_params$analysis, " ", "_"),
    "/"
  )
  ml_dir <- paste0(analysis_dir, "ml/")

  output$static_report <- function() {
    paste0(ml_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  #
  # load data
  #

  ml_env <- shiny::reactiveFileReader(CHECK_NEW_RESULTS_TIME, session, paste0(ml_dir, "ml.RData"), load_to_env)

  models_tbl <- shiny::reactive({
    shiny::req("models_tbl" %in% names(ml_env()))

    ml_env() %>% pluck("models_tbl")
  })

  ml_res_tbl <- shiny::reactive({
    shiny::req("ml_res_tbl" %in% names(ml_env()))

    ml_env() %>% pluck("ml_res_tbl")
  })

  #
  # Inspect a single model
  #

  shiny::observeEvent(
    eventExpr = models_tbl,
    handlerExpr = {
      models <- models_tbl()$model_id

      if (!is.null(models)) {
        names(models) <-
          models_tbl() %>%
          dplyr::mutate(label = sprintf("Model %d to classify %s", model_id, target)) %>%
          dplyr::pull(label)

        shiny::updateSelectInput(session, "selected_model", choices = models)
        shinyjs::show(id = "interactive")
      }
    }
  )

  selected_model <- shiny::reactive({
    ml_res_tbl() %>%
      dplyr::filter(model_id == input$selected_model) %>%
      as.list() %>%
      purrr::flatten()
  })

  output$selected_model <- shiny::renderText({
    selected_model() %>%
      magrittr::extract(
        c("model_id", "type", "target", "fs_method", "clf_method", "AUC", "Sens", "Spec", "AUCSD", "SensSD", "SpecSD")
      ) %>%
      dplyr::as_tibble() %>%
      dplyr::rename(Classifier = clf_method, `Feature Selection` = fs_method) %>%
      dplyr::mutate_at(c("AUC", "Sens", "Spec", "AUCSD", "SensSD", "SpecSD"), ~ round(.x, digits = 2)) %>%
      kable() %>%
      kableExtra::kable_styling(full_width = TRUE)
  })

  roc_pooled_plot <- shiny::reactive({
    selected_model()$fs_obj %>%
      danielLib::plot_roc(folds = FALSE)
  })

  roc_folds_plot <- shiny::reactive({
    selected_model()$fs_obj %>%
      danielLib::plot_roc(folds = TRUE)
  })
  
  roc_error <- shiny::reactive(
    if(selected_model()$type == "binary classification") {
      NULL
    } else {
      "ROC plots only apply to binary comparisons"
    }
  )

  pooled_plot_mod <- shiny::callModule(
    module = plot_mod,
    id = "pooled_plot_mod",
    plt = roc_pooled_plot,
    error = roc_error()
  )

  folds_plot_mod <- shiny::callModule(
    module = plot_mod,
    id = "folds_plot_mod",
    plt = roc_folds_plot,
    error = roc_error()
  )
}
