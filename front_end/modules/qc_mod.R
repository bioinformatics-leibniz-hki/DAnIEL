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
# QC overview module
#


qc_mod_UI <- function(id) {
  ns <- NS(id)

  shiny::fluidPage(
    shiny::uiOutput(ns("static_report")),
    shiny::h2("Download"),
    shiny::downloadButton(
      outputId = ns("download_multiqc_before"),
      label = "QC report of raw data (HTML)"
    ),
    shiny::downloadButton(
      outputId = ns("download_multiqc_after"),
      label = "QC report of clean data (HTML)"
    )
  )
}

qc_mod <- function(input, output, session, project) {
  qc_params <- project$params$qc_params[[project$selected_params$qc]]
  qc_dir <- paste0(project$project_dir, "/qc/", stringr::str_replace_all(project$selected_params$qc, " ", "_"), "/")

  output$static_report <- function() {
    paste0(qc_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  output$download_multiqc_before <- shiny::downloadHandler(
    filename = "qc_report_raw.html",
    content = function(file) {
      base::paste0(project$project_dir, "/qc/", stringr::str_replace_all(project$selected_params$qc, " ", "_"), "/before/multiqc_report.html") %>%
        readr::read_file() %>%
        readr::write_file(file)
    }
  )

  output$download_multiqc_after <- shiny::downloadHandler(
    filename = "qc_report_clean.html",
    content = function(file) {
      base::paste0(project$project_dir, "/qc/", stringr::str_replace_all(project$selected_params$qc, " ", "_"), "/after/multiqc_report.html") %>%
        readr::read_file() %>%
        readr::write_file(file)
    }
  )
}
