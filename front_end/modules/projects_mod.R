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
# Interactive project discovery module
#

projects_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::h1("Database of fungal projects"),
    shiny::selectizeInput(
      inputId = ns("current_project"),
      label = "Project",
      choices = c("no project available"),
      multiple = FALSE,
      selected = "no project available"
    ),
    shiny::htmlOutput(ns("project_description")) %>% withSpinner(),
    ggiraph::girafeOutput(ns("samples_distribution")) %>% withSpinner()
  )
}

projects_mod <- function(input, output, session) {
  projects_l <- projects_tbl$bioproject_id
  names(projects_l) <- projects_tbl$name

  shiny::updateSelectizeInput(
    session = session,
    inputId = "current_project",
    choices = projects_l,
    selected = projects_l[1],
    server = TRUE
  )

  samples_tbl <- shiny::reactive({
    external_samples_tbl %>%
      danielLib::filter_samples(input$current_project) %>%
      dplyr::rename(bioproject_id = project)
  })

  current_project <- shiny::reactive({
    current_project_tbl <-
      projects_tbl %>%
      dplyr::filter(bioproject_id == input$current_project) %>%
      tidyr::gather(key, value)

    res <- current_project_tbl$value
    names(res) <- current_project_tbl$key

    res
  })

  output$project_description <- shiny::renderUI({
    samples_tbl() %>%
      nrow() %>%
      magrittr::is_greater_than(0) %>%
      shiny::req()

    shiny::tagList(
      shiny::tags$h3(current_project()["name"]),
      shiny::tags$p(current_project()["description"])
    )
  })

  output$samples_distribution <- ggiraph::renderGirafe({
    shiny::need(
      expr = samples_tbl() %>% nrow() > 0,
      message = "Please select a project"
    ) %>% shiny::validate()

    plot_tbl <-
      samples_tbl() %>%
      danielLib::guess_coltypes() %>%
      dplyr::select(-bioproject_id)

    shiny::need(
      expr = plot_tbl %>% select_if(~ is.logical(.x) || is.factor(.x)) %>% ncol() > 0,
      message = "No sample attributes available"
    ) %>% shiny::validate()

    samples_distribution_plt <- danielLib::plot_samples_distribution(plot_tbl, bg_color = bg_color)
    ggiraph::girafe(ggobj = samples_distribution_plt)
  })
}
