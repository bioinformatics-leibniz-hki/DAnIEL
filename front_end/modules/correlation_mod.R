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
# Correlation analysis module
#

filter_correlation_results <- function(res_tbl, input) {
  res_tbl %>%
    # filter by sample group
    dplyr::filter(sample_group == input$sample_group) %>%
    # filter by significance, pass through of NAs due to lacking p values in BAnOCC
    dplyr::filter(p_value <= input$max_pvalue | is.na(p_value)) %>%
    # filter by correlation coefficient
    dplyr::filter(cor <= input$exclude_cor[1] | cor >= input$exclude_cor[2])
}

draw_correlation_network <- function(cor_results_tbl, feature_meta_tbl, color_rank = "order") {
  features <- union(cor_results_tbl$feature_a, cor_results_tbl$feature_b)

  # draw network
  feature_tooltip <- function(feature) {
    paste(
      "<p>",
      sprintf("<i>%s</i>", feature),
      shiny::a(href = paste0("https://www.ncbi.nlm.nih.gov/taxonomy/?term=", feature), "NCBI Taxonomy"),
      shiny::a(href = paste0("https://en.wikipedia.org/wiki/", feature %>% str_replace_all(" ", "_") %>% str_remove("[ ]*sp.$")), "Wikipedia"),
      shiny::a(href = paste0("http://www.mycobank.org/name/", feature %>% str_remove("[ ]*sp.$")), "Mycobank"),
      sep = "</p> <p>"
    )
  }

  edge_tooltip <- function(feature_a, feature_b, cor, p_value, q_value) {
    base::paste(
      base::sprintf("Correlation between <i>%s</i>", feature_a),
      base::sprintf("and <i>%s</i>", feature_b),
      base::sprintf("P value: %.3g", p_value),
      base::sprintf("Adjusted p value: %.3g", q_value),
      base::sprintf("Correlation:  %.3g", cor),
      sep = "<br>"
    )
  }

  feature_meta_tbl <-
    feature_meta_tbl %>%
    dplyr::filter(feature %in% features)

  feature_colors <- base::mapply(
    function(x, y) {
      y
    },
    feature_meta_tbl[[color_rank]] %>% unique(),
    feature_meta_tbl[[color_rank]] %>% unique() %>% length() %>% viridis::viridis(),
    SIMPLIFY = FALSE, USE.NAMES = TRUE
  )

  cor_color <- function(x, limits = NULL) {
    pal <- grDevices::colorRampPalette(c("#3A3A98", "#FFFFFF", "#832424"))(100)
    if (is.null(limits)) limits <- range(x)
    pal[findInterval(x, seq(limits[1], limits[2], length.out = length(pal) + 1), all.inside = TRUE)]
  }

  cor_span <- cor_results_tbl$cor %>%
    abs() %>%
    max() %>%
    c(-., .)

  nodes_tbl <-
    feature_meta_tbl %>%
    dplyr::rowwise() %>%
    dplyr::transmute(
      id = feature,
      label = sprintf("<i>%s</i>", feature),
      title = feature_tooltip(feature)
    ) %>%
    # switch from rowwise to factor annotation
    dplyr::ungroup() %>%
    dplyr::mutate(
      color = feature_colors[feature_meta_tbl[[color_rank]]] %>% as.character()
    )

  edges_tbl <-
    cor_results_tbl %>%
    dplyr::transmute(
      from = feature_a,
      to = feature_b,
      width = 8,
      color = cor_color(cor, cor_span),
      title = edge_tooltip(feature_a, feature_b, cor, p_value, q_value)
    )

  visNetwork(nodes_tbl, edges_tbl) %>%
    visNodes(font = list(multi = "html")) %>%
    visOptions(highlightNearest = TRUE) %>%
    visLayout(randomSeed = 1337)
  # visLegend(
  #   useGroups = FALSE,
  #   addNodes = data.frame(
  #     label = names(feature_colors),
  #     color = as.character(feature_colors)
  #   )
  # )
}

correlation_mod_UI <- function(id) {
  ns <- shiny::NS(id)

  shiny::fluidPage(
    width = NULL,
    shiny::uiOutput(ns("static_report")),
    shiny::div(
      id = ns("interactive"),
      shiny::h2("Interactive analysis"),
      shiny::fluidRow(
        shiny::flowLayout(
          shiny::selectInput(
            inputId = ns("sample_group"),
            label = "Sample group",
            selected = "all",
            choices = "all"
          ),
          shiny::sliderInput(
            inputId = ns("exclude_cor"),
            label = "Exclude correlation range",
            min = -1,
            max = 1,
            step = 0.05,
            value = c(-0.2, 0.2)
          ),
          shiny::numericInput(
            inputId = ns("max_pvalue"),
            label = "Max p value",
            min = 0,
            max = 1,
            step = 1e-3,
            value = 0.05
          ),
          shiny::selectInput(
            inputId = ns("color_rank"),
            label = "Rank for coloring nodes",
            choices = c(
              "Species" = "species", "Genus" = "genus", "Family" = "family",
              "Order" = "order", "Class" = "class", "Phylum" = "phylum"
            ),
            selected = "order"
          )
        )
      ),
      shiny::tabsetPanel(
        type = "tabs",
        shiny::tabPanel("Network", visNetwork::visNetworkOutput(outputId = ns("network")) %>% withSpinner()),
        shiny::tabPanel("Table", DT::dataTableOutput(outputId = ns("table"), width = "100%") %>% withSpinner())
      )
    ),
    shiny::div(
      id = ns("download-banocc"),
      shiny::h2("Download"),
      shiny::div("Model download can take up to a couple of minutes."),
      shiny::downloadButton(
        outputId = ns("download_banocc_models"),
        label = "BAnOCC models (RDS)"
      ),
    )
  )
}

correlation_mod <- function(input, output, session, project) {
  shinyjs::hide(id = "interactive")

  analysis_params <- project$params$analysis_params[[project$selected_params$analysis]]

  if (!is.null(analysis_params) && analysis_params$correlation_method == "banocc") {
    shinyjs::hide(id = "max_pvalue")
  }

  if (!is.null(analysis_params) && analysis_params$correlation_method != "banocc") {
    shinyjs::hide(id = "download-banocc")
  }

  analysis_dir <- paste(
    USERDAT_DIR,
    project$project_id,
    "analysis",
    stringr::str_replace_all(project$selected_params$analysis, " ", "_"),
    collapse = "",
    sep = "/"
  )

  correlation_dir <- paste0(analysis_dir, "/correlation/")

  # load correlation groups
  sample_groups <-
    shiny::reactive({
      file_path <- paste0(correlation_dir, "/groups.txt")

      file_path %>%
        file.exists() %>%
        shiny::req()

      sample_groups <-
        readr::read_lines(file_path) %>%
        # correlation result must be available
        purrr::keep(~ paste0(correlation_dir, .x, "/results.csv") %>% file.exists())

      shiny::updateSelectInput(session, "sample_group", choices = sample_groups, selected = sample_groups[1])

      sample_groups
    })

  output$static_report <- function() {
    paste0(correlation_dir, "report.html") %>%
      shiny::includeHTML() %>%
      shiny::HTML()
  }

  cor_results_tbl <- shiny::reactiveVal()
  filtered_cor_results_tbl <- shiny::reactiveVal()
  feature_meta_tbl <- shiny::reactiveVal()

  cor_ready_obs <- shiny::observe({
    shiny::invalidateLater(CHECK_NEW_RESULTS_TIME)
    base::message("Refresh correlation")

    feature_meta_path <- base::paste(
      project$project_dir,
      "features",
      stringr::str_replace_all(project$selected_params$features, " ", "_"),
      "features.meta.csv",
      sep = "/"
    )

    base::file.exists(feature_meta_path) %>%
      shiny::req()

    sample_groups() %>%
      shiny::req()

    sample_groups() %>%
      purrr::map(~ {
        .x %>%
          paste(correlation_dir, ., "results.csv", sep = "/") %>%
          readr::read_csv() %>%
          dplyr::mutate(sample_group = .x)
      }) %>%
      dplyr::bind_rows() %>%
      cor_results_tbl()

    feature_meta_path %>%
      readr::read_csv() %>%
      feature_meta_tbl()

    # init filtering
    cor_results_tbl() %>%
      dplyr::filter(sample_group == sample_groups()[1]) %>%
      filtered_cor_results_tbl()

    shinyjs::show("interactive")
  })

  shiny::observeEvent(
    eventExpr = cor_results_tbl,
    handlerExpr = {
      cor_ready_obs$suspend()
    }
  )

  shiny::observeEvent(
    eventExpr = input$sample_group,
    handlerExpr = {
      if (!is.null(cor_results_tbl())) {
        cor_results_tbl() %>%
          filter_correlation_results(input) %>%
          filtered_cor_results_tbl()
      }
    }
  )

  shiny::observeEvent(
    eventExpr = input$exclude_cor,
    handlerExpr = {
      if (!is.null(cor_results_tbl())) {
        cor_results_tbl() %>%
          filter_correlation_results(input) %>%
          filtered_cor_results_tbl()
      }
    }
  )

  shiny::observeEvent(
    eventExpr = input$max_pvalue,
    handlerExpr = {
      if (!is.null(cor_results_tbl())) {
        cor_results_tbl() %>%
          filter_correlation_results(input) %>%
          filtered_cor_results_tbl()
      }
    }
  )

  output$download_banocc_models <- shiny::downloadHandler(
    filename = "models.rds",
    content = function(filename) {
      files <- list.files(correlation_dir, pattern = "rds$", recursive = TRUE, full.names = TRUE)
      names <- files %>%
        purrr::map_chr(base::dirname) %>%
        stringr::str_extract("[A-z]+$")
      models <-
        files %>%
        purrr::map(~ .x %>% readr::read_rds()) %>%
        magrittr::set_names(names)

      readr::write_rds(models, filename, compress = "gz")
    }
  )

  output$table <- DT::renderDataTable({
    shiny::need(!is.null(filtered_cor_results_tbl()), "Please wait until correlation analysis is finished") %>% shiny::validate()
    shiny::need(
      expr = filtered_cor_results_tbl() %>% dim() %>% purrr::pluck(1) > 0,
      message = "No correlations to display. Please relax thresholds."
    ) %>% shiny::validate()

    filtered_cor_results_tbl() %>%
      dplyr::rename(
        `Feature A` = feature_a,
        `Feature B` = feature_b,
        `Correlation strength` = cor,
        `P value` = p_value,
        `Adjusted p value` = q_value
      )
  })

  output$network <- visNetwork::renderVisNetwork({
    shiny::need(!is.null(filtered_cor_results_tbl()), "Please wait until correlation analysis is finished") %>% shiny::validate()
    shiny::need(
      expr = filtered_cor_results_tbl() %>% dim() %>% purrr::pluck(1) > 0,
      message = "No correlations to display. Please relax thresholds."
    ) %>% shiny::validate()

    draw_correlation_network(filtered_cor_results_tbl(), feature_meta_tbl(), input$color_rank)
  })
}
