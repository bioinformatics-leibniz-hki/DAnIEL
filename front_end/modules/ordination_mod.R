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
# Ordination module
#

ordination_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::fluidRow(
      shiny::flowLayout(
        shiny::selectInput(
          inputId = ns("features_grouping"),
          label = "Feature grouping",
          choices = NULL
        ),
        shiny::selectInput(
          inputId = ns("samples_grouping"),
          label = "Sample grouping",
          choices = NULL
        ),
        shiny::selectInput(
          inputId = ns("samples_groups"),
          label = "Sample groups",
          choices = NULL,
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = ns("ord_method"),
          label = "Ordination method",
          choices = c(
            "Principal Coordinates Analysis (PCoA)" = "PCoA",
            "Non-metric multidimensional scaling (NMDS)" = "NMDS",
            "canonical correspondence analysis (CCA)" = "CCA"
          ),
          selected = "PCoA"
        ),
        shiny::selectInput(
          inputId = ns("ord_distance"),
          label = "Ordination distance",
          choices = c(
            "Bray–Curtis" = "bray",
            "Unweighted UniFrac" = "unifrac",
            "Weighted UniFrac" = "wunifrac"
          ),
          selected = "bray"
        )
      )
    ),
    plot_mod_UI(ns("plot_mod"))
  )
}

ordination_mod <- function(input, output, session, features_phy, default_n_selected = 5) {
  shiny::observeEvent(
    eventExpr = features_phy,
    handlerExpr = {
      features_groupings <-
        features_phy() %>%
        phyloseq::tax_table() %>%
        colnames() %>%
        rev() %>%
        union("None")

      samples_groupings <-
        features_phy() %>%
        phyloseq::sample_data() %>%
        as.data.frame() %>%
        as_tibble() %>%
        danielLib::guess_coltypes() %>%
        purrr::keep(~ is.factor(.x)) %>%
        colnames() %>%
        union("None")

      shiny::updateSelectInput(session, "features_grouping",
        choices = features_groupings,
        selected = features_groupings[[1]]
      )

      shiny::updateSelectInput(session, "samples_grouping",
        choices = samples_groupings,
        selected = samples_groupings[[1]]
      )
    }
  )

  shiny::observeEvent(
    eventExpr = input$samples_grouping,
    handlerExpr = {
      shiny::req({
        input$samples_grouping != ""
      })

      if (input$samples_grouping == "None") {
        # BUG: Plot not shown if samples grouping None selected
        # WORKARROUND: do not update shiny input samples_groups
        # shiny::updateSelectInput(session, "samples_groups", choices = "", selected = "")
        shinyjs::hide("samples_groups")
      } else {
        samples_groups <-
          features_phy() %>%
          phyloseq::sample_data() %>%
          as.data.frame() %>%
          danielLib::guess_coltypes() %>%
          dplyr::pull(input$samples_grouping) %>%
          unique()

        shinyjs::show("samples_groups")
        shiny::updateSelectInput(session, "samples_groups",
          choices = samples_groups,
          selected = samples_groups %>% head(default_n_selected)
        )
      }
    }
  )

  filtered_features_phy <- shiny::reactive({
    shiny::req({
      input$samples_grouping != ""
    })
    shiny::req({
      !is.null(input$samples_groups)
    })

    # no filtering if no grouping
    if (input$samples_grouping == "None") {
      features_phy()
    } else {
      filtered_samples_tbl <-
        features_phy() %>%
        phyloseq::sample_data() %>%
        as.data.frame() %>%
        tibble::as_tibble() %>%
        danielLib::guess_coltypes() %>%
        dplyr::filter_at(input$samples_grouping, ~ .x %in% input$samples_groups) %>%
        dplyr::mutate_if(is.factor, ~ .x %>%
          as.character() %>%
          replace_na("other")) %>%
        danielLib::guess_coltypes()

      shiny::req(filtered_samples_tbl %>% nrow() > 0)

      filtered_samples_phy <- phyloseq::sample_data(filtered_samples_tbl)
      phyloseq::sample_names(filtered_samples_phy) <- filtered_samples_tbl[[1]]

      phyloseq::phyloseq(
        features_phy() %>% phyloseq::otu_table(),
        features_phy() %>% phyloseq::tax_table(),
        filtered_samples_phy,
        tryCatch(features_phy() %>% phyloseq::phy_tree(), error = function(e) NULL)
      )
    }
  })

  # BUG: plot not displayed on startup
  # WORKARROUND: force eval of filtered_features_phy
  shiny::observe(filtered_features_phy())

  ordination_plt <- shiny::reactive({
    has_tree <- tryCatch(filtered_features_phy() %>% phyloseq::phy_tree(), error = function(e) FALSE, finally = TRUE)

    if (input$ord_distance %in% c("unifrac", "wunifrac")) {
      shiny::need(
        expr = has_tree,
        message = "Phylogenetic tree is missing"
      ) %>% shiny::validate()
    }

    shiny::need(
      expr = all(filtered_features_phy() %>% phyloseq::otu_table() %>% dim() >= c(3, 3)),
      message = "At least three samples and features needed"
    ) %>% shiny::validate()

    filtered_features_phy() %>%
      danielLib::plot_beta_diversity(
        samples_grouping = input$samples_grouping %>% switch(., "None" = NULL, .),
        features_grouping = input$features_grouping %>% switch(., "None" = NULL, .),
        ord_method = input$ord_method,
        ord_distance = input$ord_distance
      )
  })

  plot_mod <- shiny::callModule(
    module = plot_mod,
    id = "plot_mod",
    plt = ordination_plt
  )
}
