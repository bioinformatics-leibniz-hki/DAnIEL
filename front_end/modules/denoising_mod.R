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
# Denoising overview module
#

denoising_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::div(
      id = ns("interactive"),
      shiny::h2("Interactive analysis"),
      shiny::h3("Alignment of denoised sequences"),
      msaR::msaROutput(outputId = ns("msa_denoised"), width = "100%", height = "800px"),
      shiny::h3("Alpha diversity"),
      shiny::div("Representative sequences (ASV or OTUs) of denoised reads are used to calculate diversity for each sample."),
      boxplot_mod_UI(ns("boxplot_mod")),
      shiny::h2("Download"),
      shiny::downloadButton(
        outputId = ns("download_rep_seqs"),
        label = "Denoised sequences (FASTA)"
      ),
      shiny::downloadButton(
        outputId = ns("download_denoised_profile"),
        label = "Denoised profile (XLSX)"
      )
    )
  )
}

denoising_mod <- function(input, output, session, project) {
  shinyjs::hide(id = "interactive")

  denoising_params <- project$params$denoising_params[[project$selected_params$denoising]]
  denoising_dir <- paste0(project$project_dir, "/denoising/", stringr::str_replace_all(project$selected_params$denoising, " ", "_"), "/")

  samples_tbl <- shiny::reactive({
    file_path <-
      paste0(project$project_dir, "/input/samples.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    danielLib::read_csv_guess_coltypes(file_path)
  })

  # show interactive analysis if factor groups provided
  shiny::observeEvent(
    eventExpr = c(samples_tbl, denoised_tbl),
    handlerExpr = {
      shiny::req(!is.null(samples_tbl()) && !is.null(denoised_tbl()))

      sample_groups <-
        samples_tbl() %>%
        dplyr::filter(sample_id %in% denoised_tbl()$sample_id) %>%
        danielLib::guess_coltypes() %>%
        purrr::keep(is.factor) %>%
        purrr::keep(~ .x %>%
          setdiff(NA) %>%
          unique() %>%
          length() > 2) %>%
        colnames()

      if (sample_groups %>% length() > 0) {
        shiny::updateSelectInput(session, "sample_group", choices = sample_groups, selected = sample_groups[1])
        shinyjs::show("interactive")
      }
    }
  )

  denoised_tbl <- shiny::reactive({
    file_path <-
      paste0(denoising_dir, "/denoised.csv")

    file_path %>%
      file.exists() %>%
      shiny::req()

    readr::read_csv(file_path)
  })

  alphadiv_tbl <- shiny::reactive({
    shiny::req(denoised_tbl() %>% nrow() > 0)
    shinyjs::show("interactive")

    danielLib::get_alpha_diversity_all(denoised_tbl())
  })

  boxplot_denoising_mod <- shiny::callModule(
    module = boxplot_mod,
    id = "boxplot_mod",
    features_tbl = alphadiv_tbl,
    samples_tbl = samples_tbl
  )

  output$static_report <- function() {
    paste0(denoising_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  output$msa_denoised <- msaR::renderMsaR({
    denoised_fasta_msa_path <-
      paste0(denoising_dir, "/denoised.aligned.fasta")

    shiny::need(file.exists(denoised_fasta_msa_path), message = "Alignment file not found") %>%
      shiny::validate()

    msaR::msaR(
      msa = denoised_fasta_msa_path,
      menu = TRUE,
      overviewbox = TRUE,
      overviewboxHeight = 80,
      seqlogo = FALSE,
      rowheight = 15,
      alignmentHeight = 600
    )
  })

  output$download_rep_seqs <- shiny::downloadHandler(
    filename = "denoised.fasta",
    content = function(file) {
      base::paste0(denoising_dir, "/denoised.fasta") %>%
        readr::read_file() %>%
        readr::write_file(file)
    }
  )

  output$download_denoised_profile <- shiny::downloadHandler(
    filename = "denoised.xlsx",
    content = function(file) {
      base::paste0(denoising_dir, "/denoised.csv") %>%
        readr::read_csv() %>%
        writexl::write_xlsx(file)
    }
  )
}
