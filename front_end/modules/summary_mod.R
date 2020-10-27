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
# Module to summarize biological findings
#

summary_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::h2("Download"),
    shiny::downloadButton(outputId = ns("download_findings_table"), label = "Findings summary table (XLSX)"),
    shiny::downloadButton(outputId = ns("download_findungs_html"), label = "Findings summary report (HTML)")
  )
}

summary_mod <- function(input, output, session, project) {
  analysis_params <- project$params$features_params[[project$selected_params$analysis]]
  analysis_dir <- paste0(
    project$project_dir,
    "/analysis/",
    stringr::str_replace_all(project$selected_params$analysis, " ", "_"),
    "/"
  )
  summary_dir <- paste0(analysis_dir, "summary/")

  findings_tbl <- shiny::reactive({
    file_path <- paste0(analysis_dir, "summary/findings.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    read_csv_guess_coltypes(file_path)
  })

  output$static_report <- function() {
    paste0(summary_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  output$download_findings_table <- shiny::downloadHandler(
    filename = "findings.xlsx",
    content = function(file) {
      findings_tbl() %>%
        writexl::write_xlsx(file)
    }
  )

  output$download_findungs_html <- shiny::downloadHandler(
    filename = "DAnIEL_report.html",
    content = function(file) {
      paste0(summary_dir, "all.html") %>%
        read_file() %>%
        write_file(file)
    }
  )
}
