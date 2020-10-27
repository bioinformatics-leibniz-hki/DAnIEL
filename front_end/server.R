#!/usr/bin/env R

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
# Main Server script of Shiny front end app
#

#' @param l list of params to transform to url query
updateQueryString <- function(l) {
  # mapreduce list to url query string
  l %>%
    purrr::map2(names(.), ~ paste0(.y, "=", .x, "&")) %>%
    purrr::reduce(paste0) %>%
    paste0("?", .) %>%
    stringr::str_remove("\\&$") %>%
    shiny::updateQueryString()
}

server <- function(input, output, session) {
  base::set.seed(42)
  session$allowReconnect(TRUE)

  project_id <-
    shiny::isolate(shiny::getQueryString()$project_id) %>%
    base::ifelse(test = is.null(.), yes = "example", no = .)

  project_dir <- base::paste(USERDAT_DIR, project_id, sep = "/")

  # update tab info of url query
  shiny::observeEvent(
    eventExpr = input$sidebar_menue,
    handlerExpr = {
      shiny::isolate(shiny::getQueryString()) %>%
        magrittr::inset("project_id", project_id) %>%
        magrittr::inset("tab", input$sidebar_menue) %>%
        updateQueryString()
    }
  )

  # update tabset once to look for manual url query
  updateTabsetPanel(session, "sidebar_menue", selected = shiny::isolate(shiny::getQueryString())$tab)

  # load saved data if exists
  project_json_path <- base::paste(project_dir, "input", "project.json", sep = "/")
  if (base::file.exists(project_json_path)) {
    project <- readr::read_file(project_json_path) %>%
      jsonlite::fromJSON()

    project$project_dir <- project_dir

    if (is.null(project$selected_params)) {
      project$selected_params <- danielLib::get_selected_params(project)
    }
  } else {
    project <- base::list(
      version = FUN_EXPLORER_VERSION,
      project_id = project_id,
      project_dir = project_dir
    )
  }

  output$project_bar <- shiny::renderText(project_id)

  # save session for debugging
  shiny::observeEvent(
    eventExpr = input$save_session,
    handlerExpr = {
      base::save.image(base::paste0(project_dir, "/session.RData"))
    }
  )

  shiny::observeEvent(
    eventExpr = input$go_tutorial,
    handlerExpr = {
      shiny::getQueryString() %>%
        magrittr::inset("project_id", "example") %>%
        magrittr::inset("tab", "tutorial") %>%
        updateQueryString()

      session$reload()
    }
  )

  shiny::observeEvent(
    eventExpr = input$new_project,
    handlerExpr = {
      shiny::getQueryString() %>%
        magrittr::inset("project_id", uuid::UUIDgenerate()) %>%
        magrittr::inset("tab", "input") %>%
        updateQueryString()

      session$reload()
    }
  )

  shiny::observeEvent(
    eventExpr = input$resume_project,
    handlerExpr = shiny::showModal(resume_project_modal_UI)
  )

  shiny::observeEvent(
    eventExpr = input$resume_project_ok,
    handlerExpr = {
      shiny::getQueryString() %>%
        magrittr::inset("project_id", input$resume_project_id) %>%
        magrittr::inset("tab", "input") %>%
        updateQueryString()

      session$reload()
    }
  )

  # update menue
  shiny::observe({
    can_displayed_default <- function(project, step) {
      paste(
        project$project_dir,
        step,
        project$selected_params[[step]] %>% str_replace_all(" ", "_"),
        "report.html",
        sep = "/"
      ) %>%
        file.exists()
    }

    can_displayed_analysis <- function(project, step) {
      paste(
        project$project_dir,
        "analysis",
        project$selected_params[["analysis"]] %>% str_replace_all(" ", "_"),
        step,
        "report.html",
        sep = "/"
      ) %>%
        file.exists()
    }

    can_displayed_logs <- function(project) {
      paste0(project$project_dir, "/.snakemake/log/") %>%
        list.files() %>%
        length() > 0
    }

    pipeline_menue <- list(
      "logs" = list(
        can_displayed = can_displayed_logs(project),
        menueItem = shinydashboard::menuItem(text = "Logs", tabName = "logs", icon = icon("receipt"))
      ),
      "qc" = list(
        can_displayed = can_displayed_default(project, "qc"),
        menueItem = shinydashboard::menuItem(text = "Quality Control", tabName = "qc", icon = icon("shield-alt"))
      ),
      "denoising" = list(
        can_displayed = can_displayed_default(project, "denoising"),
        menueItem = shinydashboard::menuItem(text = "Denoising", tabName = "denoising", icon = icon("align-center"))
      ),
      "phylotyping" = list(
        can_displayed = can_displayed_default(project, "phylotyping"),
        menueItem = shinydashboard::menuItem(text = "Phylotyping", tabName = "phylotyping", icon = icon("pagelines"))
      ),
      "features" = list(
        can_displayed = can_displayed_default(project, "features"),
        menueItem = shinydashboard::menuItem(text = "Features", tabName = "features", icon = icon("chart-pie"))
      ),
      "correlation" = list(
        can_displayed = can_displayed_analysis(project, "correlation"),
        menueItem = shinydashboard::menuItem(text = "Correlations", tabName = "correlation", icon = icon("project-diagram"))
      ),
      "statistics" = list(
        can_displayed = can_displayed_analysis(project, "statistics"),
        menueItem = shinydashboard::menuItem(text = "Statistics", tabName = "statistics", icon = icon("chart-bar"))
      ),
      "ml" = list(
        can_displayed = can_displayed_analysis(project, "ml"),
        menueItem = shinydashboard::menuItem(text = "Machine Learning", tabName = "ml", icon = icon("cogs"))
      ),
      "summary" = list(
        can_displayed = can_displayed_analysis(project, "summary"),
        menueItem = shinydashboard::menuItem(text = "Summary", tabName = "summary", icon = icon("file-alt"))
      )
    ) %>%
      purrr::keep(~ .x$can_displayed) %>%
      purrr::map(~ pluck(.x, "menueItem"))

    output$pipeline_menue <- shinydashboard::renderMenu({
      shinydashboard::sidebarMenu(pipeline_menue)
    })
  })

  # Modules
  input_mod <- shiny::callModule(input_mod, "input_mod", project = project)
  start_mod <- shiny::callModule(start_mod, "start_mod", project = project, input_mod = input_mod)
  logs_mod <- shiny::callModule(logs_mod, "logs_mod", project = project)
  qc_mod <- shiny::callModule(qc_mod, "qc_mod", project = project)
  denoising_mod <- shiny::callModule(denoising_mod, "denoising_mod", project = project)
  phylotyping_mod <- shiny::callModule(phylotyping_mod, "phylotyping_mod", project = project)
  features_mod <- shiny::callModule(features_mod, "features_mod", project = project)
  correlation_mod <- shiny::callModule(correlation_mod, "correlation_mod", project = project)
  projects_mod <- shiny::callModule(projects_mod, "projects_mod")
  interactions_mod <- shiny::callModule(interactions_mod, "interactions_mod")
  infections_mod <- shiny::callModule(infections_mod, "infections_mod")
  statistics_mod <- shiny::callModule(statistics_mod, "statistics_mod", project = project)
  ml_mod <- shiny::callModule(ml_mod, "ml_mod", project = project)
  summary_mod <- shiny::callModule(summary_mod, "summary_mod", project = project)
}
