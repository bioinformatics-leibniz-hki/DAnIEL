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
# Phylotyping module
#

phylotyping_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::h2("Download"),
    shiny::downloadButton(
      outputId = ns("download_phylotypes"),
      label = "Phylotypes (XLSX)"
    ),
    shiny::h2("Phylotypes"),
    DT::dataTableOutput(outputId = ns("phylotyped_tbl"), height = 400, width = "100%") %>% withSpinner()
  )
}

phylotyping_mod <- function(input, output, session, project) {
  phylotyping_params <- project$params$phylotyping_params[[project$selected_params$phylotyping]]
  phylotyping_dir <- paste0(project$project_dir, "/phylotyping/", stringr::str_replace_all(project$selected_params$phylotyping, " ", "_"), "/")

  output$static_report <- function() {
    paste0(phylotyping_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  phylotyped_tbl <- shiny::reactiveVal()
  phylotyped_tbl_obs <- shiny::observe({
    shiny::invalidateLater(CHECK_NEW_RESULTS_TIME)
    base::message("Refresh phylotypes")
    phylotyped_tbl_path <- base::paste0(
      project$project_dir,
      "/phylotyping/",
      stringr::str_replace_all(project$selected_params$phylotyping, " ", "_"),
      "/phylotyped.csv"
    )
    if (base::file.exists(phylotyped_tbl_path)) {
      phylotyped_tbl_path %>%
        readr::read_csv() %>%
        phylotyped_tbl()
    }
  })
  shiny::observeEvent(
    eventExpr = phylotyped_tbl,
    handlerExpr = {
      # no need to continue looking for qc file
      phylotyped_tbl_obs$suspend()
    }
  )

  denoised_tbl <- shiny::reactiveVal()
  denoised_tbl_obs <- shiny::observe({
    shiny::invalidateLater(CHECK_NEW_RESULTS_TIME)
    base::message("Refresh denoised")
    denoised_tbl_path <- base::paste0(
      project$project_dir,
      "/denoising/",
      stringr::str_replace_all(project$selected_params$denoising, " ", "_"),
      "/denoised.csv"
    )
    if (base::file.exists(denoised_tbl_path)) {
      denoised_tbl_path %>%
        readr::read_csv() %>%
        denoised_tbl()
    }
  })
  
  shiny::observeEvent(
    eventExpr = denoised_tbl,
    handlerExpr = {
      # no need to continue looking for qc file
      denoised_tbl_obs$suspend()
    }
  )

  output$phylotyped_tbl <- DT::renderDataTable({
    shiny::need(!is.null(phylotyped_tbl()), "Please wait until phylotyping is finished") %>% shiny::validate()

   phylotyped_tbl() %>%
      dplyr::select(-kingdom) %>%
      dplyr::arrange(phylum, class, order, family, genus, species, strain) %>%
      # italic species names
      dplyr::mutate(
        strain = case_when(
          # UNITE SH ID
          base::grepl("^SH[0-9]+", strain) ~ base::sprintf(
            "<a href='https://unite.ut.ee/bl_forw_sh.php?sh_name=%s'>%s</a", strain, strain
          ),
          # NCBI Accession
          base::grepl("^[A-Z]{1,2}[0-9]{5,8}(\\.[0-9]+)?$", strain) ~ base::sprintf(
            "<a href='https://www.ncbi.nlm.nih.gov/nuccore/%s'>%s</a", strain, strain
          ),
          # Unknown reference database. No link annotation.
          TRUE ~ strain
        ),
        species = base::ifelse(is.na(species), "", base::sprintf("<i>%s</i>", species)),
        genus = base::ifelse(is.na(genus), "", base::sprintf("<i>%s</i>", genus))
      ) %>%
      DT::datatable(
        escape = FALSE,
        rownames = FALSE,
        options = list(
          dom = "Bfrtip",
          fnDrawCallback = htmlwidgets::JS("function(){HTMLWidgets.staticRender();}")
        )
      ) %>%
      DT::formatRound("confidence", 2) %>%
      DT::formatStyle("confidence", color = DT::styleInterval(c(0.8), c("#ff0000", "#000000"))) %>%
      sparkline::spk_add_deps()
  })

  output$download_phylotypes <- shiny::downloadHandler(
    filename = "phylotypes.xlsx",
    content = function(file) {
      base::paste0(
        project$project_dir,
        "/phylotyping/",
        stringr::str_replace_all(project$selected_params$phylotyping, " ", "_"),
        "/phylotyped.csv"
      ) %>%
        readr::read_csv() %>%
        writexl::write_xlsx(file)
    }
  )

  # sunburst plot
  #
  # phylotyped_tbl %>%
  #   # aggregate to species level
  #   dplyr::group_by(phylum, class, order, family, genus, species) %>%
  #   count() %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate_at(taxa, ~ifelse(is.na(.x), "unclassified", .x)) %>%
  #   # create sunburst plot
  #   dplyr::mutate(
  #     path = paste(phylum, class, order, family, genus, species, sep = "-"),
  #     pop = n # population
  #   ) %>%
  #   dplyr::select(path, pop) %>%
  #   sunburstR::sunburst(colors = viridisLite::viridis(10), width = "100%", height = "500px")
}
