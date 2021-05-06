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
# abundance module
#

abundance_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    shiny::p(
      "Taxon abundance profile clustered by sample simillarity using Manhattan distance.",
      "Displayed sparseness can be controlled using the minimum number of taxa per sample."
    ),
    shiny::fluidRow(
      shiny::flowLayout(
        shiny::selectInput(
          inputId = ns("samples_grouping"),
          label = "Sample grouping",
          choices = NULL
        ),
        shiny::selectInput(
          inputId = ns("annotation_groupings"),
          label = "Annotation groupings",
          multiple = TRUE,
          choices = NULL
        ),
        shiny::numericInput(
          inputId = ns("min_taxa_per_sample"),
          label = "Min taxa per sample",
          min = 0,
          step = 1,
          value = 5
        ),
        shiny::selectInput(
          inputId = ns("clustering_method"),
          label = "Clustering method",
          choices = c(
            "single linkage" = "single",
            "average linkage (UPGMA)" = "average",
            "median linkage (WPGMC)" = "median",
            "mcquitty linkage (WPGMA)" = "mcquitty",
            "complete linkage" = "complete"
          ),
          selected = "average"
        ),
        shiny::checkboxGroupInput(
          inputId = ns("show_labels"),
          label = "Show labels",
          choices = c("taxa", "samples"),
          selected = c(TRUE, TRUE)
        ),
        shiny::radioButtons(
          inputId = ns("normalization_method"),
          label = "Normalization method",
          choices = "raw",
          selected = "raw"
        )
      )
    ),
    plot_mod_UI(
      id = ns("plot_mod"),
      height = "800px"
    )
  )
}

abundance_mod <- function(input, output, session, norm_features_tbl, raw_features_tbl, samples_tbl, features_params) {
  shiny::observeEvent(
    eventExpr = samples_tbl,
    handlerExpr = {
      sample_groupings <-
        samples_tbl() %>%
        danielLib::guess_coltypes() %>%
        purrr::keep(~ is.factor(.x)) %>%
        colnames() %>%
        union("None")

      shiny::updateSelectInput(
        session = session,
        inputId = "samples_grouping",
        choices = sample_groupings,
        selected = sample_groupings[[1]]
      )

      annotation_groupings <-
        samples_tbl() %>%
        purrr::discard(is.character) %>%
        purrr::discard(~ .x %>%
          is.na() %>%
          all()) %>%
        colnames()

      shiny::updateSelectInput(
        session = session,
        inputId = "annotation_groupings",
        choices = annotation_groupings,
        selected = annotation_groupings
      )
    }
  )

  shiny::updateRadioButtons(
    session = session,
    inputId = "normalization_method",
    choices = c("raw", features_params$normalization_method)
  )

  n_max_displayed_items <- 20
  shiny::updateCheckboxGroupInput(
    session = session,
    inputId = "show_labels",
    selected = c(
      shiny::isolate(raw_features_tbl) %>% shiny::isolate() %>% ncol() < n_max_displayed_items + 1,
      shiny::isolate(raw_features_tbl) %>% shiny::isolate() %>% nrow() < n_max_displayed_items
    )
  )

  filtered_samples <- shiny::reactive({
    raw_features_tbl() %>%
      tidyr::pivot_longer(-sample_id, names_to = "taxon", values_to = "abundance") %>%
      dplyr::filter(abundance > 0) %>%
      dplyr::count(sample_id) %>%
      dplyr::filter(n >= input$min_taxa_per_sample) %>%
      dplyr::pull(sample_id)
  })

  features_tbl <- shiny::reactive({
    if (input$normalization_method == "raw") {
      raw_features_tbl()
    } else if (input$normalization_method == features_params$normalization_method) {
      norm_features_tbl()
    }
  })


  heatmap <- shiny::reactive({
    features_tbl <-
      features_tbl() %>%
      dplyr::filter(sample_id %in% filtered_samples()) %>%
      tibble::column_to_rownames("sample_id")

    # color scale limits
    color <-
      features_tbl %>%
      tidyr::gather(feature, abundance) %>%
      dplyr::pull(abundance) %>%
      base::abs() %>%
      base::max() %>%
      {
        max_fill <- .
        min_fill <- ifelse(input$normalization_method == "clr", -max_fill, 0)

        if (input$normalization_method == "clr") {
          fill <- c(min_fill, 0, max_fill)
          fill_colors <- c(scales::muted("blue"), "white", scales::muted("red"))
          circlize::colorRamp2(fill, fill_colors)
        } else {
          viridis::viridis(100)
        }
      }

    samples_split <-
      samples_tbl() %>%
      dplyr::filter(sample_id %in% filtered_samples()) %>%
      purrr::pluck(input$samples_grouping)

    if (is.null(samples_split)) {
      samples_title <- "samples"
    } else {
      samples_title <- samples_split %>%
        unique() %>%
        as.character()
    }

    if (is.null(input$annotation_groupings)) {
      samples_annot <- NULL
    } else {
      samples_annot <-
        samples_tbl() %>%
        dplyr::filter(sample_id %in% filtered_samples()) %>%
        dplyr::select_at(input$annotation_groupings) %>%
        dplyr::mutate_if(
          .predicate = ~ is.character(.x) || is.factor(.x),
          .funs = ~ .x %>%
            as.character() %>%
            tidyr::replace_na("NA")
        ) %>%
        as.data.frame() %>% # can not use just tibble
        ComplexHeatmap::HeatmapAnnotation(
          df = .,
          annotation_name_side = "left"
        )
    }

    features_tbl %>%
      as.matrix() %>%
      t() %>%
      {
        # hide axis tick labels if wished
        .x <- .
        if (!"taxa" %in% input$show_labels) {
          rownames(.x) <- NULL
        }
        if (!"samples" %in% input$show_labels) {
          colnames(.x) <- NULL
        }
        .x
      } %>%
      ComplexHeatmap::Heatmap(
        # general
        name = input$normalization_method,
        col = color,
        border = TRUE,

        # features (rows)
        row_title = features_params$taxonomic_rank,
        row_names_gp = grid::gpar(fontface = "italic"),
        row_names_max_width = grid::unit(Inf, "cm"),
        row_names_side = "right",
        clustering_distance_rows = "manhattan",
        clustering_method_rows = input$clustering_method,

        # samples (columns)
        column_split = samples_split,
        column_title = samples_title,
        top_annotation = samples_annot,
        clustering_distance_columns = "manhattan",
        clustering_method_columns = input$clustering_method
      )
  })

  plot_mod <- shiny::callModule(
    module = plot_mod,
    id = "plot_mod",
    plt = heatmap,
    ggplot = FALSE
  )
}
