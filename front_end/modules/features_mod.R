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
# Features module
#

features_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::h2("Interactive alpha diversity"),
    shiny::div("Feature counts pooled at provided taxonomic rank are used to calculate diversity for each sample."),
    boxplot_mod_UI(ns("boxplot_mod")),
    shiny::h2("Interactive beta diversity"),
    shiny::div(
      paste(
        "Feature counts pooled at provided taxonomic rank are used to calculate dissimilarity for each pair of samples.",
        "This dissimilarity is approximately plotted in the two dimensional plane."
      )
    ),
    ordination_mod_UI(ns("ordination_mod")),
    shiny::h2("Download"),
    shiny::downloadButton(
      outputId = ns("download_features_raw"),
      label = "Raw feature profile (XLSX)"
    ),
    shiny::downloadButton(
      outputId = ns("download_features_norm"),
      label = "Normalized feature profile (XLSX)"
    ),
    shiny::downloadButton(
      outputId = ns("download_features_meta"),
      label = "Feature meta data (XLSX)"
    )
  )
}

features_mod <- function(input, output, session, project) {
  features_params <- project$params$features_params[[project$selected_params$features]]
  features_dir <- paste0(project$project_dir, "/features/", stringr::str_replace_all(project$selected_params$features, " ", "_"), "/")

  output$static_report <- function() {
    paste0(features_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  raw_features_tbl <- shiny::reactive({
    file_path <- paste0(features_dir, "/features.raw.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    readr::read_csv(file_path)
  })

  norm_features_tbl <- shiny::reactive({
    file_path <- paste0(features_dir, "/features.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    readr::read_csv(file_path)
  })

  meta_features_tbl <- shiny::reactive({
    file_path <- paste0(features_dir, "/features.meta.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    readr::read_csv(file_path)
  })

  samples_tbl <- shiny::reactive({
    file_path <- paste0(project$project_dir, "/input/samples.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    readr::read_csv(file_path)
  })

  alphadiv_tbl <- shiny::reactive({
    raw_features_tbl() %>% danielLib::get_alpha_diversity_all()
  })

  features_phy <- shiny::reactive({
    file_path <- paste0(features_dir, "/phyloseq.rds")

    file_path %>%
      file.exists() %>%
      shiny::req()

    features_phy <- readr::read_rds(file_path)

    samples_tbl <-
      features_phy %>%
      phyloseq::sample_data() %>%
      as.data.frame() %>%
      tibble::as_tibble() %>%
      danielLib::guess_coltypes() %>%
      dplyr::mutate_if(is.factor, ~ .x %>%
        as.character() %>%
        replace_na("other")) %>%
      danielLib::guess_coltypes()

    samples_phy <- phyloseq::sample_data(samples_tbl)
    phyloseq::sample_names(samples_phy) <- samples_tbl[[1]]

    phyloseq::phyloseq(
      features_phy %>% phyloseq::otu_table(),
      features_phy %>% phyloseq::tax_table(),
      samples_phy,
      tryCatch(features_phy %>% phyloseq::phy_tree(), error = function(x) NULL)
    )
  })

  boxplot_features_mod <- shiny::callModule(
    module = boxplot_mod,
    id = "boxplot_mod",
    features_tbl = alphadiv_tbl,
    samples_tbl = samples_tbl
  )

  ordination_mod <- shiny::callModule(
    module = ordination_mod,
    id = "ordination_mod",
    features_phy = features_phy
  )

  output$download_features_raw <- shiny::downloadHandler(
    filename = "raw_features.xlsx",
    content = function(file) {
      writexl::write_xlsx(x = raw_features_tbl(), path = file)
    }
  )

  output$download_features_norm <- shiny::downloadHandler(
    filename = "norm_features.xlsx",
    content = function(file) {
      writexl::write_xlsx(x = norm_features_tbl(), path = file)
    }
  )

  output$download_features_meta <- shiny::downloadHandler(
    filename = "meta_features.xlsx",
    content = function(file) {
      writexl::write_xlsx(x = meta_features_tbl(), path = file)
    }
  )
}
