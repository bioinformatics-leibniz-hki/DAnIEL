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
# Statistics module
#

statistics_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::div(
      id = ns("interactive"),
      shiny::h2("Analyze a single feature"),
      shiny::tags$p(
        paste(
          "Abundance distribution is plotted here using any combination of feature and sample grouping.",
          "Significance is indicated by stars (p < 0.001: \u2605\u2605\u2605, p < 0.01: \u2605\u2605, p < 0.05: \u2605)",
          "P values are taken from post hoc tests if multiple test statistics are available.",
          collapse = " "
        )
      ),
      boxplot_mod_UI(ns("boxplot_mod")),
      shiny::h2("Download"),
      shiny::downloadButton(outputId = ns("download_stat_table"), label = "Statistics summary table (XLSX)")
    )
  )
}

statistics_mod <- function(input, output, session, project) {
  shinyjs::hide(id = "interactive")

  analysis_params <- project$params$features_params[[project$selected_params$analysis]]
  analysis_dir <- paste0(
    project$project_dir,
    "/analysis/",
    stringr::str_replace_all(project$selected_params$analysis, " ", "_"),
    "/"
  )
  statistics_dir <- paste0(analysis_dir, "statistics/")

  output$static_report <- function() {
    paste0(statistics_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  #
  # load data
  #

  stat_tbl <- shiny::reactive({
    file_path <-
      project$selected_params$analysis %>%
      stringr::str_replace_all(" ", "_") %>%
      paste0(project$project_dir, "/analysis/", ., "/statistics/stat.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    stat_tbl <- readr::read_csv(file_path)

    stat_tbl
  })

  features_tbl <- shiny::reactive({
    message("Statistics module: Read features_tbl")
    
    file_path <-
      project$selected_params$features %>%
      stringr::str_replace_all(" ", "_") %>%
      paste0(project$project_dir, "/features/", ., "/features.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    danielLib::read_csv_guess_coltypes(file_path)
  })

  samples_tbl <- shiny::reactive({
    file_path <-
      paste0(project$project_dir, "/input/samples.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    danielLib::read_csv_guess_coltypes(file_path)
  })

  shiny::observeEvent(
    eventExpr = stat_tbl,
    handlerExpr = shinyjs::show(id = "interactive")
  )

  boxplot_statistics_mod <- shiny::callModule(
    module = boxplot_mod,
    id = "boxplot_mod",
    features_tbl = features_tbl,
    samples_tbl = samples_tbl
  )

  output$download_stat_table <- shiny::downloadHandler(
    filename = "stat_table.xlsx",
    content = function(file) {
      stat_tbl() %>%
        writexl::write_xlsx(file)
    }
  )
}
