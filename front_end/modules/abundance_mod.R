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
# abundance module
#

abundance_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::flowLayout(
        shiny::selectInput(
          inputId = ns("samples_grouping"),
          label = "Sample grouping",
          choices = NULL
        ),
        shiny::numericInput(
          inputId = ns("min_taxa_per_sample"),
          label = "Min taxa per sample",
          min = 0,
          step = 1,
          value = 5
        ),
        shiny::checkboxGroupInput(
          inputId = ns("show_labels"),
          label = "Show labels",
          choices = c("taxa", "samples"),
          selected = c(TRUE, TRUE)
        ),
        shiny::radioButtons(
          inputId = ns("normalization_method"),
          label = "Normalization method",
          choices = "raw",
          selected = "raw"
        )
      )
    ),
    shiny::plotOutput(ns("plot"))
  )
}

abundance_mod <- function(input, output, session, norm_features_tbl, raw_features_tbl, samples_tbl, features_params) {
  shiny::observeEvent(
    eventExpr = samples_tbl,
    handlerExpr = {
      res <-
        samples_tbl() %>%
        danielLib::guess_coltypes() %>%
        purrr::keep(~ is.factor(.x)) %>%
        colnames() %>%
        union("None")
      
      shiny::updateSelectInput(
        session = session,
        inputId = "samples_grouping",
        choices = res,
        selected = res[[1]]
      )
    }
  )
  
  shiny::updateRadioButtons(
    session = session,
    inputId = "normalization_method",
    choices = c("raw", features_params$normalization_method)
  )
  
  n_max_displayed_items <- 20
  shiny::updateCheckboxGroupInput(
    session = session,
    inputId = "show_labels",
    selected = c(
      shiny::isolate(raw_features_tbl) %>% shiny::isolate() %>% ncol() < n_max_displayed_items + 1,
      shiny::isolate(raw_features_tbl) %>% shiny::isolate() %>% nrow() < n_max_displayed_items
    )
  )

  filtered_samples <- shiny::reactive({
    raw_features_tbl() %>%
      pivot_longer(-sample_id, names_to = "taxon", values_to = "abundance") %>%
      filter(abundance > 0) %>%
      count(sample_id) %>%
      filter(n >= input$min_taxa_per_sample) %>%
      pull(sample_id)
  })
  
  features_tbl <- shiny::reactive({
    if(input$normalization_method == "raw") {
      raw_features_tbl()
    } else if (input$normalization_method == features_params$normalization_method) {
      norm_features_tbl()
    }
  })

  output$plot <- shiny::renderPlot({
    features_tbl() %>%
      dplyr::filter(sample_id %in% filtered_samples()) %>%
      tibble::column_to_rownames("sample_id") %>%
      as.matrix() %>%
      t() %>%
      {
        # hide axis tick labels if there are too many
        .x <- .
        if (! "taxa" %in% input$show_labels) {
          rownames(.x) <- NULL
        }
        if (! "samples" %in% input$show_labels) {
          colnames(.x) <- NULL
        }
        .x
      } %>%
      ComplexHeatmap::Heatmap(
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        column_split = samples_tbl() %>% filter(sample_id %in% filtered_samples()) %>% purrr::pluck(input$samples_grouping)
      )
  })
  
  # plot_mod <- shiny::callModule(
  #   module = plot_mod,
  #   id = "plot_mod",
  #   plt = heatmap,
  #   ggplot = FALSE
  # )
}
