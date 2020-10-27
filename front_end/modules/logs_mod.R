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
# Displaying logs
#

logs_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::h1("Logs"),
    shiny::tags$p("This page shows pipeline logs for debugging purposes."),
    shiny::selectInput(
      inputId = ns("select_log_files"),
      label = "Log file",
      choices = "not available"
    ),
    shiny::verbatimTextOutput(ns("log_content")) %>% withSpinner()
  )
}

logs_mod <- function(input, output, session, project) {
  logs_dir <- paste0(
    project$project_dir,
    "/.snakemake/log/"
  )

  log_files <- list.files(logs_dir)

  shiny::updateSelectInput(session, "select_log_files", choices = log_files)

  output$log_content <- shiny::renderText({
    paste0(logs_dir, "/", input$select_log_files) %>%
      readr::read_file()
  })
}
