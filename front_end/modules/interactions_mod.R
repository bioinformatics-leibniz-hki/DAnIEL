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
# Interactions DB module
#

interactions_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::h1("Database of fungal interactions"),
    shiny::div(
      paste0(
        "We created a manually curated database about fungal interactions. ",
        "Published papers were queried for the fungal species name and any of these terms: Disease, bacteria, immune system, and cytokine. ",
        "All interactions reported in these papers were added to this database"
      )
    ),
    shiny::h2("Browse database"),
    DT::dataTableOutput(outputId = ns("interactions_tbl"), width = "100%") %>% withSpinner(),
    shiny::h2("Download database"),
    tags$p(
      paste0(
        "The database of fungal interactions consists of a table and a BibTex file describing the interactions and their references, respectively. ",
        "Medical Subject Headings (MeSH) are used whenever applicable."
      )
    ),
    shiny::downloadButton(
      outputId = ns("download_db_csv"),
      label = "Interactions (CSV)"
    ),
    shiny::downloadButton(
      outputId = ns("download_db_bib"),
      label = "References (BibTeX)"
    )
  )
}

interactions_mod <- function(input, output, session) {
  interactions_tbl <-
    daniel_db_con %>%
    DBI::dbReadTable("interactions") %>%
    tibble::as_tibble()

  output$interactions_tbl <- DT::renderDataTable(
    expr = {
      interactions_tbl %>%
        dplyr::mutate(
          pubmed_id = pubmed_id %>% sprintf("<a href='https://www.ncbi.nlm.nih.gov/pubmed/%s'>%s<a/>", ., .),
          fungus = fungus %>% sprintf("<i>%s</i>", .),
          interaction = ifelse(
            test = is.na(partner_mesh_id),
            yes = partner,
            no = sprintf("<a href='https://id.nlm.nih.gov/mesh/%s.html'>%s<a/>", partner_mesh_id, partner_mesh_term)
          )
        ) %>%
        dplyr::select(Fungus = fungus, Interaction = interaction, Direction = interaction_type, Reference = pubmed_id, comment)
    },
    escape = FALSE
  )

  output$download_db_csv <- shiny::downloadHandler(
    filename = "DAnIEL_interactions_db.csv",
    content = function(file) {
      interactions_tbl %>%
        readr::write_csv(file)
    }
  )

  output$download_db_bib <- shiny::downloadHandler(
    filename = "DAnIEL_interactions_db.bib",
    content = function(file) {
      paste0(DB_DIR, "/daniel_db/interactions.bib") %>%
        readr::read_file() %>%
        readr::write_file(file)
    }
  )
}
