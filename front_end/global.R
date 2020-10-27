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
# Script with global objects available in all parts of the shiny app
#

source("init.R")

FUN_EXPLORER_VERSION <- "CI"

# Directotry where all user data is stored (raw reads, project meta data, files created from back end)
# this must be an absolute path w/p '~'
USERDAT_DIR <- Sys.getenv("DANIEL_USERDAT_DIR")
DB_DIR <- Sys.getenv("DANIEL_DB_DIR")
# CSV file listing available reference data bases
REF_DB_CSV <- paste0(DB_DIR, "/reference_db/db.csv")
PHYLO_DB_CSV <- paste0(DB_DIR, "/phylogenies/db.csv")

# miliseconds to check for new results
CHECK_NEW_RESULTS_TIME <- 5000

message("Connect to daniel_db")

daniel_db_con <-
  paste0(DB_DIR, "/daniel_db/daniel_db.sqlite") %>%
  DBI::dbConnect(RSQLite::SQLite(), .)

projects_tbl <-
  daniel_db_con %>%
  DBI::dbReadTable("projects") %>%
  tibble::as_tibble()

# lazy load for speed up
external_attributes_tbl <- dplyr::tbl(daniel_db_con, "attributes")
external_samples_tbl <-
  dplyr::tbl(daniel_db_con, "samples") %>%
  dplyr::left_join(external_attributes_tbl, by = "sample_id")

base::options(
  shiny.maxRequestSize = 10 * 1024 * 1024^2,
  shiny.port = 3838,
  shiny.host = "0.0.0.0",
  knitr.table.format = "html",
  knitr.kable.NA = " "
)
shiny::addResourcePath("img", "www/img/")

# Parameter description e.g. for popovers
params_tbl <-
  paste0(DB_DIR, "/etc/params.csv") %>%
  readr::read_csv()

#
# Default settings
#

showNotification <- function(...) shiny::showNotification(duration = 10, ...)

#
# Default ggplot theme
#
na_color <- "#666666"
scale_fill_discrete <- function(...) ggplot2::scale_fill_viridis_d(na.value = na_color, ...)
scale_fill_continuous <- function(...) ggplot2::scale_fill_viridis_c(na.value = na_color, ...)
scale_color_discrete <- function(...) ggplot2::scale_color_viridis_d(na.value = na_color, ...)
scale_color_continuous <- function(...) ggplot2::scale_color_viridis_c(na.value = na_color, ...)
scale_colour_discrete <- function(...) ggplot2::scale_colour_viridis_d(na.value = na_color, ...)
scale_colour_continuous <- function(...) ggplot2::scale_colour_viridis_c(na.value = na_color, ...)

bg_color <- "#F9F9F9"
theme_my <-
  ggplot2::theme_minimal(base_size = 16) +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 0.8),
    axis.line.y = ggplot2::element_line(size = 0.8),
    axis.ticks = ggplot2::element_line(colour = "black", size = 0.8),
    axis.text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank()
  )
ggplot2::theme_set(theme_my)
ggplot2::update_geom_defaults("bar", base::list(fill = bg_color))

#
# Misc Functions
#

update_label <- function(input_obj) {
  # TODO: Not an isolated function. Side effect: params_tbl needed

  input_id_ns <-
    input_obj %>%
    as.character() %>%
    stringr::str_extract("id=\"[A-z_-]+") %>%
    stringr::str_remove("id=\"")

  input_id <- stringr::str_extract(input_id_ns, "[A-z_]+$")

  obj <-
    params_tbl %>%
    dplyr::filter(id == input_id) %>%
    dplyr::slice(1) %>%
    as.list() %>%
    purrr::map(~ stringr::str_replace_all(.x, "~", ", "))

  content <-
    shiny::div(
      class = "param-label",
      shiny::strong(obj$label),
      shiny::div(obj$description),
      shiny::div(
        class = "space-flex",
        shiny::span(obj$min %>% {
          ifelse(is.na(.), "", paste0("Minimum: ", .))
        }),
        shiny::span(obj$default %>% {
          ifelse(is.na(.), "", paste0("Default: ", .))
        }),
        shiny::span(obj$max %>% {
          ifelse(is.na(.), "", paste0("Maximum: ", .))
        })
      )
    ) %>%
    as.character() %>%
    stringr::str_remove_all("\n")

  # update label
  input_class <- shiny::tagGetAttribute(input_obj$children[[1]], "class")
  if (input_class == "checkbox") {
    input_obj$children[[1]]$children[[2]] <- input_obj$children[[1]]$children[[1]]
    input_obj$children[[1]]$children[[1]] <- shiny::HTML(content)
  } else {
    input_obj$children[[1]]$children[[1]] <- shiny::HTML(content)
  }

  input_obj
}

sanitize_expression_string <- function(s) {
  s %>%
    # remove parentheses
    stringr::str_remove_all("[()]+") %>%
    # remove infix operators
    stringr::str_remove_all("%[^%]+%")
}

withSpinner <- function(x) shinycssloaders::withSpinner(ui_element = x, color = "lightgrey", type = 4)

resume_project_modal_UI <- shiny::modalDialog(
  title = "Resume project",
  easyClose = TRUE,
  shiny::div("Please enter the id of the project to be resumed."),
  shiny::textInput("resume_project_id", label = "Project ID", placeholder = "00000000-aaaa-0000-aaaa-000000000000"),
  shiny::actionButton("resume_project_ok", label = "Load")
)

load_to_env <- function(path, env = new.env()) {
  path %>%
    file.exists() %>%
    shiny::req()

  load(path, env)
  return(env)
}
