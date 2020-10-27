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
# Infections DB module
#

infections_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h1("Database of fungal infections"),
    shiny::div(
      "This database consists of clinical isolates with suspicious fungal infections from the ",
      shiny::tags$a("NRZMyk", href = "https://www.nrz-myk.de/home.html"),
      "database"
    ),
    shiny::h2("Browse database"),
    DT::dataTableOutput(outputId = ns("infections_tbl"), width = "100%") %>% withSpinner(),
    shiny::h2("Download database"),
    shiny::downloadButton(
      outputId = ns("downlaod_db_csv"),
      label = "infections (CSV)"
    )
  )
}

infections_mod <- function(input, output, session) {
  infections_tbl <-
    daniel_db_con %>%
    DBI::dbReadTable("infections") %>%
    tibble::as_tibble()

  output$infections_tbl <- DT::renderDataTable(
    expr = {
      infections_tbl %>%
        dplyr::mutate_at(
          .vars = c("species", "synonyms"),
          .funs = ~ .x %>%
            purrr::map_if(is.na, ~"", .else = ~ sprintf("<i>%s</i>", .x)) %>%
            as.character()
        )
    },
    escape = FALSE
  )

  output$downlaod_db_csv <- shiny::downloadHandler(
    filename = "DAnIEL_infections_db.csv",
    content = function(file) {
      infections_tbl %>%
        readr::write_csv(file)
    }
  )
}
