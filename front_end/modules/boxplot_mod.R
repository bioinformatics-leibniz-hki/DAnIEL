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
# Boxplot module
#

boxplot_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::fluidRow(
      shiny::flowLayout(
        shiny::selectInput(
          inputId = ns("samples_grouping"),
          label = "Sample grouping",
          choices = "not available"
        ),
        shiny::selectInput(
          inputId = ns("samples_groups"),
          label = "Sample groups",
          choices = "not available",
          multiple = TRUE
        ),
        shiny::selectInput(
          inputId = ns("features"),
          label = "Features",
          choices = "not available",
          multiple = TRUE
        )
      )
    ),
    shiny::p("Significance is calculated using Wilcoxon test for binary outcomes and using Kruskal-Wallis and Dunn's test otherwise."),
    plot_mod_UI(ns("plot_mod"))
  )
}

#' Boxplot
#' @param tbl long tibble with columns key, value and one for grouping
#' @param grouping column name of tbl used for grouping rows
plot_boxplot <- function(tbl, grouping, sample_id) {
  groups <-
    tbl %>%
    pluck(grouping) %>%
    unique()

  facets <-
    tbl %>%
    pull(key) %>%
    unique()

  plt <-
    tbl %>%
    ggplot2::ggplot(aes_string(grouping, "value", color = grouping)) +
    ggiraph::geom_point_interactive(aes_string(tooltip = sample_id)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~key, ncol = 2, strip.position = "left", scales = "free") +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      strip.placement = "outside",
      legend.position = "bottom"
    ) +
    ggplot2::guides(color = guide_legend(ncol = 1)) +
    ggplot2::labs(x = "", y = "")
  
  res_plt <- dplyr::case_when(
    length(groups) == 2 ~ "binary",
    length(groups) > 2 ~ "multi",
    TRUE ~ "simple"
  ) %>%
    switch(
      "simple" = plt,
      "binary" = {
        plt +
          ggpubr::stat_compare_means(
            comparisons = groups %>% as.character() %>% {
              list(c(.[1], .[2]))
            },
            symnum.args = list(cutpoints = c(0, 0.001, 0.01, 0.05, 1), symbols = c("\u2605\u2605\u2605", "\u2605\u2605", "\u2605", "ns")),
            hide.ns = FALSE,
            method = "wilcox"
          )
      },
      "multi" = {
        for (facet in facets) {
          plt <-
            plt +
            danielLib::stat_compare_medians(
              data = tbl,
              g = grouping,
              x = "value",
              facet_key = "key",
              facet_val = facet,
              step.increase	= 0.1
            )
        }
        plt
      }
    )

  res_plt
}

#' @param features_tbl reactive wide tibble containing values to plot
#' @param samples_tbl reactive wide tibble containing meta data of rows in features_tbl
#' @param sample_id colum present in features_tbl and samples_tbl used for joining
#' @param default_n_selected number of elements selected by default in multi select inputs
boxplot_mod <- function(input, output, session, features_tbl, samples_tbl, sample_id = "sample_id", default_n_selected = 5) {
  changed_samples_tbl <- shiny::reactive({
    samples_tbl() %>%
      dplyr::mutate_all(~ .x %>%
        as.character() %>%
        replace_na("other")) %>%
      danielLib::guess_coltypes()
  })

  groupings <- shiny::reactive({
    changed_samples_tbl() %>%
      select_if(is.factor) %>%
      colnames() %>%
      setdiff(sample_id)
  })

  groups <- shiny::reactive({
    changed_samples_tbl() %>%
      purrr::pluck(input$samples_grouping) %>%
      unique()
  })

  features <- shiny::reactive({
    features_tbl() %>%
      colnames() %>%
      setdiff(sample_id)
  })

  tbl <- shiny::reactive({
    shiny::req(
      # all selected groups must be in samples_tbl
      setdiff(input$samples_grouping, changed_samples_tbl() %>% colnames()) %>% length() == 0
    )

    changed_samples_tbl() %>%
      dplyr::select_at(c(sample_id, input$samples_grouping)) %>%
      dplyr::filter_at(input$samples_grouping, ~ .x %in% input$samples_groups) %>%
      dplyr::inner_join(
        {
          features_tbl() %>%
            tidyr::gather(key, value, -sample_id) %>%
            dplyr::filter(key %in% input$features)
        },
        by = sample_id
      ) %>%
      readr::type_convert()
  })

  # BUG: Plot is not shown on startup and only if inputs changed manually
  # WORARROUND: Trigger eval of tbl using observe
  shiny::observe(tbl())

  boxplot <- shiny::reactive({
    shiny::need(
      expr = tbl() %>% nrow() > 0,
      message = "No data available"
    ) %>% shiny::validate()

    shiny::need(
      expr = input$samples_groups %>% length() > 0,
      message = "No group selected"
    ) %>% shiny::validate()

    plot_boxplot(tbl(), input$samples_grouping, sample_id)
  })

  plot_mod <- shiny::callModule(
    module = plot_mod,
    id = "plot_mod",
    plt = boxplot
  )

  shiny::observeEvent(
    eventExpr = groupings,
    handlerExpr = {
      shiny::updateSelectInput(session, "samples_grouping", choices = groupings(), selected = groupings()[1])
    }
  )

  shiny::observeEvent(
    eventExpr = input$samples_grouping,
    handlerExpr = {
      shiny::updateSelectInput(session, "samples_groups", choices = groups(), selected = groups() %>% head(default_n_selected))
    }
  )

  shiny::observeEvent(
    eventExpr = features,
    handlerExpr = {
      shiny::updateSelectInput(session, "features", choices = features(), selected = features() %>% head(default_n_selected))
    }
  )
}
