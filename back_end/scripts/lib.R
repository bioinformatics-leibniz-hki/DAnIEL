# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Shorten a string to fixed size
#' @param s string to shorten
#' @param maxchars maximum length of shortened string
str_shorten <- function(s, maxchars = 15) {
  s %>% {
    ifelse(nchar(.) > maxchars, str_sub(., 1, maxchars) %>% paste0(., "..."), .)
  }
}
#' Reads csv file and guessses column classes
#' @param csv_path path of csv file
#' @return tibble with
read_csv_guess_coltypes <- function(csv_path) {
  readr::read_csv(csv_path) %>% guess_coltypes()
}
plot_interactive_correlation_network <- function(cor_results_tbl, feature_meta_tbl, color_rank = "order") {
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

  visNetwork::visNetwork(nodes_tbl, edges_tbl) %>%
    visNetwork::visNodes(font = list(multi = "html")) %>%
    visNetwork::visOptions(highlightNearest = TRUE) %>%
    visNetwork::visLayout(randomSeed = 1337)
  # visNetwork::visLegend(
  #   useGroups = FALSE,
  #   addNodes = data.frame(
  #     label = names(feature_colors),
  #     color = as.character(feature_colors)
  #   )
  # )
}
#' Guess column classes from a tibble
guess_coltypes <- function(tbl) {
  # temp ungroup
  groups <- tbl %>% group_vars()
  tbl <- tbl %>% dplyr::ungroup()

  factor_cols <-
    tbl %>%
    tidyr::gather(k, v) %>%
    dplyr::filter(!is.na(v)) %>%
    dplyr::group_by(k, v) %>%
    dplyr::count() %>%
    # exclude columns in which each element is unique or always the same
    dplyr::filter(n > 1 & n < dim(tbl)[1]) %>%
    dplyr::pull(k) %>%
    base::unique()

  tbl %>%
    dplyr::mutate_at(.vars = factor_cols, factor) %>%
    dplyr::group_by_at(groups)
}
#'  Generates correlation entwork plot of one group
#'  @param graph graph to draw. igraph or tbl_graph.
#'  @param limits limits for edge color scale (correlation coefficient)
#'  @param node_colors Named vector for coloring nodes
#'  @param color_title Title of node color guide
#'  @param title Title of the plot
#'  @param low_edge_color color for lower limit of edge color scale (correlation coefficient)
#'  @param high_edge_color color for higher limit of edge color scale (correlation coefficient)
plot_correlation_network <- function(graph, limits, node_colors, color_title, title = "",
                                     low_edge_color = "#3A3A98", high_edge_color = "#832424", node_size = 3) {
  graph %>%
    ggraph::ggraph(layout = "nicely") +
    ggraph::geom_edge_link(aes(color = cor), width = 1) +
    # transform node color to factor to use scale_color_manual(drop = FALSE)
    # to force all entries into legend
    # ggraph does not provide aes mappings for x and y
    ggiraph::geom_point_interactive(mapping = aes(x, y, tooltip = tooltip, color = factor(color, names(node_colors))), size = node_size) +
    ggraph::scale_edge_color_gradient2(low = low_edge_color, high = high_edge_color, midpoint = 0, limits = limits) +
    ggplot2::scale_color_manual(values = node_colors, labels = names(node_colors), drop = FALSE) +
    ggplot2::labs(title = title, color = color_title) +
    ggplot2::theme_void()
}
plot_girafe <- function(ggobj, width = 6, height = NULL, ...) {
  if (is.null(height)) height <- width

  options <- list(
    ggiraph::opts_zoom(max = 10)
  )
  ggiraph::girafe(ggobj = ggobj, options = options, width = width, height = height, ...)
}
#' @param vec vector to be summarized
#' @param percentage assumes vec are percentages
#' @param num_gmt number formater
#' @return a summary text e.g. from 1% to 2% (Average=1.5%)
summary_text <- function(vec, percentage = FALSE, num_fmt = "g") {
  vec %>%
    summary() %>%
    as.list() %>%
    {
      min <- .$`Min.`
      max <- .$`Max.`
      mean <- .$`Mean`
      perc_s <- ifelse(percentage, "%%", "")
      fmt <- paste0("from %", num_fmt, perc_s, " to %", num_fmt, perc_s, " (Average %", num_fmt, perc_s, ")")

      sprintf(fmt, min, max, mean)
    }
}
#' Plot abundance clustermap
#' @param features_tbl tibble with taxa abundances. Must have column sample_id
#' @param normalization_method string of normalization method used as legend title
#' @param taxonomic_rank string used as y axis title. Taxa are printed in italic if this is genus or species
#' @param clustering_distance clustering distance passed to ComplexHeatmap::Heatmap
plot_abundance_clustermap <- function(features_tbl, normalization_method, taxonomic_rank, clustering_distance = "spearman") {
  cluster_features <- features_tbl %>% ncol() > 2 # substract sample name column
  cluster_samples <- features_tbl %>% nrow() > 1

  generate_heatmap <- function(features_tbl, ...) {
    # color scale limits
    fill_range <-
      features_tbl %>%
      tidyr::gather(feature, abundance, -sample_id) %>%
      dplyr::pull(abundance) %>%
      base::abs() %>%
      base::max()

    # italic species and genus names
    fontface <- base::ifelse(taxonomic_rank %in% c("species", "genus"), "italic", "plain")

    features_tbl %>%
      magrittr::set_rownames(.$sample_id) %>%
      dplyr::select(-sample_id) %>%
      base::as.matrix() %>%
      base::t() %>%
      ComplexHeatmap::Heatmap(
        # general
        name = normalization_method,
        col = circlize::colorRamp2(c(-fill_range, 0, fill_range), c(scales::muted("blue"), "white", scales::muted("red"))),

        # features (rows)
        row_title = taxonomic_rank,
        row_names_gp = grid::gpar(fontface = fontface),
        row_names_max_width = grid::unit(Inf, "cm"),
        row_names_side = "right",

        # samples (columns)
        column_title = "sample",
        column_names_max_height = grid::unit(Inf, "cm"),
        column_names_centered = TRUE,
        ...
      )
  }

  purrr::when(
    cluster_features && cluster_samples ~ "both",
    cluster_features ~ "features",
    cluster_samples ~ "samples",
    TRUE ~ "none"
  ) %>%
    switch(
      "both" = generate_heatmap(
        features_tbl,
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        clustering_distance_rows = clustering_distance,
        clustering_distance_columns = clustering_distance
      ),
      "samples" = generate_heatmap(
        features_tbl,
        cluster_rows = TRUE,
        clustering_distance_rows = clustering_distance
      ),
      "features" = generate_heatmap(
        features_tbl,
        cluster_columns = TRUE,
        clustering_distance_columns = clustering_distance
      ),
      "none" = generate_heatmap(
        features_tbl,
        cluster_rows = FALSE,
        cluster_columns = FALSE
      ),
      NULL
    )
}
#' supress any output from a function
#' @seealso  https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quietly <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
#' Get correlation graph and calculate topology properties
#' @param edges_tbl edge tibble with columns from and to
#' @param nodes_tbl node tibble with column name
get_correlation_graph <- function(edges_tbl, nodes_tbl) {
  nodes <- union(edges_tbl$from, edges_tbl$to) %>% unique()
  nodes_tbl <- nodes_tbl %>% dplyr::filter(name %in% nodes)

  graph <-
    edges_tbl %>%
    tidygraph::tbl_graph(nodes = nodes_tbl, directed = FALSE) %>%
    tidygraph::activate(nodes) %>%
    dplyr::mutate(
      `Betweeness centrality` = tidygraph::centrality_betweenness(),
      `Closeness centrality` = tidygraph::centrality_closeness(),
      `Node degree` = tidygraph::centrality_degree()
    )

  return(graph)
}
#' Plot alpha diversity box plot
#' @param abundance_tbl abudnance tibble with colum sample_id. Raw denoised counts preferred.
#' @param samples_tbl samples meta data tibble with column sample_id. Only needed if grouping is provided
#' @param grouping name of column in samples_tbl used to group samples
#' @param singnif_bars show significance bars (Kruskal_Wallis and post hoc Dunn's test)
#' @return ggplot boxplot of alpha diversities
plot_alpha_diversity <- function(abundance_tbl, samples_tbl = NULL, grouping = NULL, singnif_bars = TRUE) {
  alphadiv_tbl <-
    abundance_tbl %>%
    get_alpha_diversity_all()

  if (is.null(grouping)) {
    alpha_div_plt <-
      alphadiv_tbl %>%
      tidyr::gather(metric, value, -sample_id) %>%
      ggplot2::ggplot(aes(x = "", y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::labs(y = "Alpha diversity", x = "")
    return(alpha_div_plt)
  }

  tbl <-
    alphadiv_tbl %>%
    tidyr::gather(metric, value, -sample_id) %>%
    dplyr::inner_join(samples_tbl, by = "sample_id") %>%
    dplyr::rename_at(grouping, ~"sample_group")

  alpha_div_plt <-
    tbl %>%
    mutate(sample_group = sample_group %>% as.character()) %>%
    ggplot2::ggplot(aes(sample_group, value, color = sample_group)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::theme(axis.text.x = element_blank()) +
    ggplot2::labs(x = grouping, y = "Alpha diversity", color = grouping)

  if (!singnif_bars) {
    return(alpha_div_plt)
  }

  if (samples_tbl %>% purrr::pluck(grouping) %>% unique() %>% length() < 2) {
    warning("Grouping variable has less than two levels. Significance calculation will be ignored. ")
    return(alpha_div_plt)
  }

  for (facet_val in tbl$metric %>% unique()) {
    alpha_div_plt <-
      alpha_div_plt +
      # must be run for each facet group
      danielLib::stat_compare_medians(data = tbl, g = "sample_group", x = "value", facet_key = "metric", facet_val = facet_val)
  }
  return(alpha_div_plt)
}
#' Add median comparison P-values to a ggplot
#' @param data data used by ggplot
#' @param g name of grouping column used for dunn.test
#' @param x name of value column used for dunn.test
#' @param facet column of data used for facetting
#' @param signif_bar_y_spacing fraction of plot hight used as spacer between two significance bars
#' @param max_pvalue maximum adjusted p value of by dunn.test to display
stat_compare_medians <- function(data = NULL, g = NULL, x = NULL, facet_key = NULL, facet_val = NULL,
                                 max_pvalue = 0.05, signif_bar_y_spacing = 0.1, ...) {
  # TODO: Data is not inherited from ggrobj and must be provided. Should be drawn from grob instead

  if (!is.null(facet_key)) {
    data <- dplyr::filter_at(data, facet_key, ~ .x == facet_val)
  }

  max <- data[[x]] %>% max()

  stat_tbl <-
    data %>%
    # replace NA which are discarded by dunn.test anyway
    dplyr::mutate_at(g, ~ str_replace_na(.x, "NA")) %>%
    do(
      quietly({
        dunn.test::dunn.test(x = .[[x]], g = .[[g]], kw = FALSE)
      }) %>%
        as.list() %>%
        tibble::as_tibble() %>%
        tidyr::separate(comparisons, into = c("group1", "group2"), sep = " - ") %>%
        dplyr::mutate(label = P.adjusted %>% significance_label())
    ) %>%
    dplyr::filter(P.adjusted <= max_pvalue) %>%
    dplyr::mutate(y.position = max + row_number() * max * signif_bar_y_spacing) %>%
    dplyr::mutate_at(c("group1", "group2"), ~ ifelse(.x == "NA", NA, .x))

  if (!is.null(facet_key)) {
    stat_tbl <-
      stat_tbl %>%
      dplyr::mutate(!!facet_key := facet_val)
  }

  ggpubr::stat_pvalue_manual(data = stat_tbl, ...)
}
get_consensus_tbl <- function(msa_path, tree) {
  #' count base occurences of MSA sequences
  seqs <- treeio::read.fasta(msa_path)
  seqs_tbl <-
    tree$tip.label %>%
    base::lapply(function(x) {
      bases <- base::as.character(seqs[[x]])
      tibble::tibble(seq = x, pos = seq(length(bases)), base = bases)
    }) %>%
    dplyr::bind_rows()

  seqs_count_tbl <-
    seqs_tbl %>%
    dplyr::group_by(pos, base) %>%
    dplyr::count()

  consensus_tbl <-
    seqs_count_tbl %>%
    dplyr::group_by(pos) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::select(pos, consensus_base = base)

  tbl <-
    seqs_tbl %>%
    dplyr::left_join(seqs_count_tbl) %>%
    dplyr::left_join(consensus_tbl) %>%
    dplyr::mutate(base = case_when(
      base == "-" ~ "-",
      base == consensus_base ~ "#",
      TRUE ~ base
    ))

  res_tbl <-
    tbl %>%
    dplyr::group_by(seq, pos) %>%
    dplyr::slice(1) %>%
    dplyr::select(-n, -consensus_base) %>%
    tidyr::spread(pos, base) %>%
    base::as.data.frame() %>%
    magrittr::set_rownames(.$seq) %>%
    dplyr::select(-seq)

  return(res_tbl)
}
#' Filters existing samples
#' @return sampels tbl with filtered samples
filter_samples <- function(samples_tbl, projects, projects_filter_query = "") {
  filtered_samples_tbl <-
    samples_tbl %>%
    dplyr::filter(bioproject_id %in% projects) %>%
    dplyr::collect()

  attributes <- filtered_samples_tbl %>%
    pull(attribute_key) %>%
    unique()

  filtered_samples_tbl %>%
    tidyr::spread(attribute_key, attribute_val) %>%
    # filter samples from existing cohorts if filter query provided
    purrr::when(
      projects_filter_query != "" ~ {
        dplyr::filter(., parse(
          text = projects_filter_query %>% sanitize_expression_string()
        ) %>% eval())
      },
      TRUE ~ .
    ) %>%
    dplyr::select(sample_id, project = bioproject_id, one_of(attributes)) %>%
    # remove columns which have the same value. Keep also sample_id and project
    dplyr::group_by(sample_id, project) %>%
    dplyr::select_if(~ unique(.x) %>% length() > 1) %>%
    guess_coltypes() %>%
    select_if(is.factor) %>%
    ungroup()
}
#' Calculates all alpha diversity metrics
#' @param tbl abundance tibble with column sample_id. Denoised raw counts preferred.
#' @return alpha diversity tibble with columns sample_id, shannon, simpson, invsimpson
get_alpha_diversity_all <- function(tbl, measures = c("Chao1", "Shannon", "InvSimpson"), ...) {
  tbl %>%
    set_rownames(.$sample_id) %>%
    select(-sample_id) %>%
    phyloseq::otu_table(taxa_are_rows = FALSE) %>%
    phyloseq::estimate_richness(measures = measures, ...) %>%
    as_tibble(rownames = "sample_id") %>%
    select(-se.chao1)
}
#' Plots ROC over resamples
#' @param model feature selection object (e.g. caret sbf, rfe. train works as well)
#' @param folds show ROC of all resamples as well
plot_roc <- function(fs, folds = FALSE) {
  # objects of train class have different locations
  # of prediction table and observatipon levels
  switch(
    EXPR = fs %>% class() %>% pluck(1),
    "sbf" = {
      obs_levels <<- fs %>% pluck("obsLevels")
      pred_tbl <<- fs$pred$predictions
    },
    "rfe" = {
      obs_levels <<- fs %>% pluck("fit", "levels")
      pred_tbl <<- fs$pred
    },
    "train" = {
      obs_levels <<- fs %>% pluck("levels")
      pred_tbl <<- fs$pred
    }
  )

  pred_tbl <-
    pred_tbl %>%
    dplyr::mutate(
      obs_int = obs %>% base::as.integer() %>% magrittr::subtract(1),
      pred_int = pred %>% base::as.integer() %>% magrittr::subtract(1)
    ) %>%
    dplyr::rename(pred_dbl = obs_levels[1]) %>%
    tibble::as_tibble()

  roc <- pROC::roc(pred_tbl, response = "obs_int", predictor = "pred_dbl", direction = ">", levels = c(0, 1))

  if (folds) {
    pred_tbl %>%
      # add pooled data
      bind_rows(pred_tbl %>% mutate(Resample = "pooled")) %>%
      ggplot2::ggplot(aes(d = obs_int, m = pred_dbl, color = Resample)) +
      plotROC::geom_roc(n.cuts = 0) +
      ggplot2::geom_abline(intercept = 0) +
      ggplot2::annotate(
        "text",
        x = 1, y = 0, hjust = 1, size = 8,
        label = roc$auc %>% round(2) %>% magrittr::multiply_by(100) %>% sprintf(fmt = "AUC=%.1f%%")
      ) +
      ggplot2::labs(
        x = "False positive rate",
        y = "Trure positive rate"
      )
  } else {
    roc %>%
      pROC::ggroc() +
      ggplot2::geom_abline(intercept = 1) +
      ggplot2::annotate(
        "text",
        x = 0, y = 0, hjust = 1, size = 8,
        label = roc$auc %>% round(2) %>% magrittr::multiply_by(100) %>% sprintf(fmt = "AUC=%.1f%%")
      )
  }
}
#' Post hoc box plot
#'
#' @param feature_tbl featute table
#' @param post_stat_results_tbl statistics results table of post doc tests
#' @param current_feature current feature used to filter feature_tbl
#' @param group_var grouping variable aka x axis of this plot
#' @return Post hoc box plot (ggplot2)
post_hoc_box_plot <- function(feature_tbl, post_stat_results_tbl, current_feature, group_var) {
  plot_tbl <-
    feature_tbl %>%
    tidyr::gather(feature, abundance, -sample_id) %>%
    dplyr::filter(feature == current_feature) %>%
    dplyr::left_join(samples_tbl, by = "sample_id") %>%
    dplyr::rename(x = group_var, y = abundance)

  max_y <-
    plot_tbl %>%
    dplyr::pull(y) %>%
    max()

  span_y <-
    plot_tbl %>%
    dplyr::pull(y) %>%
    min() %>%
    abs() %>%
    magrittr::add(abs(max_y))

  stat_tbl <-
    post_stat_results_tbl %>%
    dplyr::filter(feature == current_feature) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(group_a) %>%
    dplyr::transmute(
      group1 = group_a,
      group2 = group_b,
      y.position = dim(.)[1] %>% seq() %>% sapply(function(x) (x * 0.1 * span_y) + max_y),
      p = significance_label(p.value),
    ) %>%
    dplyr::filter(!p == "ns")

  plt <-
    plot_tbl %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_boxplot() +
    ggpubr::stat_pvalue_manual(data = stat_tbl) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(face = "italic")
    ) +
    ggplot2::labs(
      title = current_feature,
      y = "Abundance",
      x = group_var
    )

  return(plt)
}
#' Create significance label with stars
#' Three, two or one star for p value less than 0.001, 0.01, and 0.05, respectively
#' @param p.value p value
#' @param include_value write p value in addition to stars. Format `.2e` is used.
significance_label <- function(p.value, include_value = FALSE) {
  if (include_value) {
    p.formatted <- p.value %>% format(., digits = 2, scientific = TRUE)
    dplyr::case_when(
      p.value < 0.001 ~ paste0(p.formatted, " \u2605\u2605\u2605"),
      p.value < 0.01 ~ paste0(p.formatted, " \u2605\u2605"),
      p.value < 0.05 ~ paste0(p.formatted, " \u2605"),
      TRUE ~ paste0(p.formatted, "")
    )
  } else {
    dplyr::case_when(
      p.value < 0.001 ~ "\u2605\u2605\u2605",
      p.value < 0.01 ~ "\u2605\u2605",
      p.value < 0.05 ~ "\u2605",
      TRUE ~ ""
    )
  }
}
#' Get selected parameters for all steps based on project$selected_analysis_params
#' @param  project list of DAnIEL file project.json
get_selected_params <- function(project) {
  get_selected_params_name <- function(cur_step, next_step, selected_params) {
    project$params[[paste0(next_step, "_params")]][[selected_params[[next_step]]]][[paste0("selected_", cur_step, "_params")]]
  }

  selected_params <- list()
  selected_params$analysis <- project$selected_analysis_params
  selected_params$features <- get_selected_params_name("features", "analysis", selected_params)
  selected_params$phylotyping <- get_selected_params_name("phylotyping", "features", selected_params)
  selected_params$denoising <- get_selected_params_name("denoising", "phylotyping", selected_params)
  selected_params$qc <- get_selected_params_name("qc", "denoising", selected_params)

  return(selected_params)
}

significance_label <- function(p.value, include_value = FALSE) {
  if (include_value) {
    p.formatted <- p.value %>% format(., digits = 2, scientific = TRUE)
    dplyr::case_when(
      p.value <= 0.001 ~ paste0(p.formatted, " \u2605\u2605\u2605"),
      p.value <= 0.01 ~ paste0(p.formatted, " \u2605\u2605"),
      p.value <= 0.05 ~ paste0(p.formatted, " \u2605"),
      TRUE ~ paste0(p.formatted, "")
    )
  } else {
    dplyr::case_when(
      p.value <= 0.001 ~ "\u2605\u2605\u2605",
      p.value <= 0.01 ~ "\u2605\u2605",
      p.value <= 0.05 ~ "\u2605",
      TRUE ~ ""
    )
  }
}
get_node_topology_tbl <- function(groups_cor_tbl, node_topology_properties = c("Betweeness centrality", "Node degree")) {
  if (is.null(groups_cor_tbl)) {
    return(NULL)
  }

  node_topology_tbl <-
    groups_cor_tbl %>%
    {
      map2(.$graph, .$correlation_group, ~ .x %>%
        tidygraph::activate(nodes) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(correlation_group = .y))
    } %>%
    dplyr::bind_rows() %>%
    dplyr::select(name, correlation_group, !!node_topology_properties) %>%
    tidyr::gather(key, value, -name, -correlation_group)

  return(node_topology_tbl)
}
#' Generates boxplot of network topology properties
#' @param node_topology_tbl tibble about network topology with columns name, correlation_group, key, and value
plot_correlation_topology <- function(node_topology_tbl) {
  topology_plt <-
    node_topology_tbl %>%
    ggplot(aes(correlation_group, value)) +
    geom_boxplot() +
    facet_wrap(~key, scales = "free_y") +
    labs(x = "Group", y = "")

  if (node_topology_tbl$correlation_group %>% unique() %>% length() == 1) {
    return(topology_plt)
  }

  # add comparisons if multiple correlation groups are required
  topology_plt <-
    topology_plt +
    danielLib::stat_compare_medians(data = node_topology_tbl, facet_key = "key", facet_val = "Node degree", g = "correlation_group", x = "value") +
    danielLib::stat_compare_medians(data = node_topology_tbl, facet_key = "key", facet_val = "Betweeness centrality", g = "correlation_group", x = "value") +
    danielLib::stat_compare_medians(data = node_topology_tbl, facet_key = "key", facet_val = "Closeness centrality", g = "correlation_group", x = "value")
  return(topology_plt)
}
kable <- function(df, caption = "", ...) {
  df %>%
    dplyr::ungroup() %>%
    # format p value columns
    dplyr::mutate_at(vars(matches("[pqPQ][._ ]+[vV]alue")), function(x) significance_label(x, include_value = TRUE)) %>%
    # format numeric columns
    # dplyr::mutate_if(base::is.numeric, list(~ sprintf("%g", .x))) %>%
    # format logical columns
    dplyr::mutate_if(base::is.logical, list(~ ifelse(., "yes", kableExtra::cell_spec("no", color = "red")))) %>%
    # format feature columns
    dplyr::mutate_at(vars(tidyselect::matches("^[Ff]eature[s]?[ ]?[AB]?$")), function(x) sprintf("<i>%s</i>", x)) %>%
    knitr::kable(caption = caption, escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      ...
    )
}
#' Plot sample attributes distribution
#' Count values of factor and logical columns.
#' Plots their counts as `ggiraph` enabled ggplot
#' @param samples_tbl sample meta data table with attribute columns
#' @param min_frac_show minimum faction of x axis to show a label
plot_samples_distribution <- function(samples_tbl, min_frac_show = 0.2, bg_color = "transparent") {
  samples_tbl %>%
    # select factor columns only
    dplyr::select_if(base::sapply(., base::is.factor) | base::sapply(., base::is.logical)) %>%
    dplyr::mutate_all(~ .x %>%
      as.character() %>%
      replace_na("NA")) %>%
    tidyr::gather(attribute_key, attribute_val) %>%
    dplyr::group_by(attribute_key, attribute_val) %>%
    dplyr::count() %>%
    dplyr::group_by(attribute_key) %>%
    dplyr::mutate(label = ifelse(n > min_frac_show * sum(n), attribute_val, "")) %>%
    ggplot(aes(attribute_key, n, tooltip = attribute_val, label = label)) +
    ggiraph::geom_bar_interactive(stat = "identity", color = "black", fill = bg_color) +
    ggplot2::geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = FALSE) +
    ggplot2::labs(y = "Sample", x = "Attribute")
}
plot_ordination_adonis <- function(features_phy, ord_method = "PCoA", ord_distance = "bray", adonis_group = NULL) {
  features_ord <- phyloseq::ordinate(features_phy, method = ord_method, distance = ord_distance)

  axes_labels <-
    switch(
      ord_method,
      PCoA = {
        c(
          sprintf("PCo1 (%.1f%%)", features_ord$values$Relative_eig[1] * 100),
          sprintf("PCo2 (%.1f%%)", features_ord$values$Relative_eig[2] * 100)
        )
      },
      c(paste(ord_method, "1"), paste(ord_method, "2"))
    )

  if (is.null(adonis_group)) {
    phyloseq::plot_ordination(physeq = features_phy, ordination = features_ord, justDF = TRUE, type = "biplot") %>%
      as_tibble() %>%
      magrittr::set_colnames(colnames(.) %>% .[3:length(.)] %>% c("axis1", "axis2", .)) %>%
      ggplot2::ggplot(mapping = aes(axis1, axis2)) +
      # draw taxa arrows
      ggplot2::geom_segment(
        data = . %>% dplyr::filter(id.type == "Taxa"),
        mapping = aes(xend = 0, yend = 0, color = genus),
        arrow = arrow(ends = "first"),
        linejoin = "round"
      ) +
      # draw sample points
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      ggiraph::geom_point_interactive(
        data = . %>% dplyr::filter(id.type == "Samples"),
        mapping = aes(axis1, axis2, tooltip = sprintf("Sample: %s", sample_id)),
        size = 3
      ) +
      ggplot2::labs(
        x = axes_labels[1],
        y = axes_labels[2],
        color = "group",
        title = paste("Ordination Biplot"),
        subtitle = paste(ord_distance, ord_method)
      )
  } else {
    #
    # Adonis ordination
    #
    adonis_meta <-
      features_phy %>%
      phyloseq::sample_data() %>%
      tibble::as_tibble() %>%
      dplyr::select(sample_id, adonis_group = !!adonis_group)

    adonis_text <-
      vegan::adonis(
        formula = phyloseq::distance(features_phy, method = ord_distance) ~ adonis_group,
        data = adonis_meta,
        permutations = 10e3
      ) %>%
      purrr::pluck("aov.tab") %>%
      tibble::as_tibble(rownames = "var") %>%
      dplyr::rename(p_value = `Pr(>F)`) %>%
      dplyr::filter(var == "adonis_group") %>%
      dplyr::mutate(
        p_text = dplyr::case_when(
          p_value < 1e-3 ~ "\u2605\u2605\u2605",
          p_value < 1e-2 ~ "\u2605\u2605",
          p_value < 0.05 ~ "\u2605",
          TRUE ~ "NS"
        )
      ) %>%
      dplyr::pull(p_text) %>%
      dplyr::first() %>%
      sprintf("Adonis: %s", .)


    phyloseq::plot_ordination(physeq = features_phy, ordination = features_ord, justDF = TRUE, type = "biplot") %>%
      as_tibble() %>%
      dplyr::mutate(adonis_group = .[[adonis_group]]) %>%
      magrittr::set_colnames(colnames(.) %>% .[3:length(.)] %>% c("axis1", "axis2", .)) %>%
      ggplot2::ggplot(mapping = aes(axis1, axis2)) +
      # draw taxa arrows
      ggplot2::geom_segment(
        data = . %>% dplyr::filter(id.type == "Taxa"),
        mapping = aes(xend = 0, yend = 0, color = genus),
        arrow = arrow(ends = "first"),
        linejoin = "round"
      ) +
      # draw sample points
      ggnewscale::new_scale_color() +
      ggnewscale::new_scale_fill() +
      ggplot2::geom_point(
        data = . %>% dplyr::filter(id.type == "Samples"),
        mapping = aes(color = adonis_group),
        size = 3
      ) +
      ggplot2::annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, label = adonis_text) +
      ggplot2::labs(
        x = axes_labels[1],
        y = axes_labels[2],
        color = "group",
        title = paste("Ordination Biplot"),
        subtitle = paste(ord_distance, ord_method)
      )
  }
}
#' set default styles for ggplo2
setup_style <- function() {
  scale_fill_discrete <- function(...) ggplot2::scale_fill_viridis_d(...)
  scale_fill_continuous <- function(...) ggplot2::scale_fill_viridis_c(...)
  scale_color_discrete <- function(...) ggplot2::scale_color_viridis_d(...)
  scale_color_continuous <- function(...) ggplot2::scale_color_viridis_c(...)
  scale_colour_discrete <- function(...) ggplot2::scale_colour_viridis_d(...)
  scale_colour_continuous <- function(...) ggplot2::scale_colour_viridis_c(...)

  theme_my <-
    ggplot2::theme_minimal(base_size = 16) +
    ggplot2::theme(
      axis.line.x = ggplot2::element_line(size = 0.8),
      axis.line.y = ggplot2::element_line(size = 0.8),
      axis.ticks = ggplot2::element_line(colour = "black", size = 0.8),
      axis.text = ggplot2::element_text(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
    )
  ggplot2::theme_set(theme_my)
  ggplot2::update_geom_defaults("bar", base::list(fill = "lightgrey"))
}
#' Create analysis report
#'
#' The report includes some chunks only if the corresponding analysis method was performed
#' This is done using conditional R markdown chunks with the asis engine (as is)
#' This engine, however, obviously do not support the execution of R code inside that chunk
#' Therefore, dynamic variables inside these chunks are reported as placeholders {{name}}
#' This placeholders have to be replaced in post production
#'
#' @param project_dir path of project directory the report shoulb be based on
#' @param out_html path to output report html file
report <- function(project_dir, out_html, template_rmd = system.file("extdata", "report.Rmd", package = "danielLib")) {
  tmp_dir <- tempdir()
  raw_report_html <- paste0(tmp_dir, "/report.html")
  report_placehodler_csv <- paste0(tmp_dir, "/report.csv")

  params <- list(
    project_dir = project_dir,
    report_placehodler_csv = report_placehodler_csv
  )

  rmarkdown::render(
    input = template_rmd,
    params = params,
    envir = new.env(),
    output_format = "html_document",
    output_file = raw_report_html,
    intermediates_dir = tmp_dir
  )

  report_html <- readr::read_file(raw_report_html)
  report_placehodler_tbl <- readr::read_csv(report_placehodler_csv)

  # replace all placeholders
  res <-
    dim(report_placehodler_tbl)[1] %>%
    seq() %>%
    lapply(function(i) {
      report_html <<- gsub(
        pattern = sprintf("{{%s}}", report_placehodler_tbl$placeholder[[i]]),
        replacement = report_placehodler_tbl$value[[i]],
        x = report_html,
        fixed = TRUE
      )
      return(TRUE)
    })

  readr::write_file(report_html, out_html)
}
#' Create nested correlation tibble containing plots and graphs
#' @param  correlation_dir Path to correlation directory of DAnIEL. Must contain file groups.txt and a subdirectiry for each group
get_groups_cor_tbl <- function(
                               correlation_dir, nodes_tbl, features_grouping_col = "order",
                               n_features = 8, max_p_value = 0.05, min_abs_cor = 0.1) {
  correlation_groups <-
    sprintf("%s/groups.txt", correlation_dir) %>%
    readr::read_lines() %>%
    # correlation result must be available
    purrr::keep(~ paste0(correlation_dir, .x, "/results.csv") %>% file.exists())

  edges_tbl <-
    tibble::tibble(correlation_group = correlation_groups) %>%
    dplyr::mutate(
      edges_tbl_path = sprintf("%s/%s/results.csv", correlation_dir, correlation_group),
      edges_tbl = purrr::map2(edges_tbl_path, correlation_group, ~ readr::read_csv(.x) %>% dplyr::mutate(correlation_group = .y))
    ) %>%
    dplyr::pull(edges_tbl) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(from = feature_a, to = feature_b) %>%
    # filter by p value if provided. This is needed to pass banocc results
    dplyr::filter(if (p_value %>% is.na() %>% all()) TRUE else p_value <= max_p_value) %>%
    dplyr::filter(abs(cor) >= min_abs_cor)

  if (edges_tbl %>% nrow() == 0) {
    warning("Filtered edge table must not be empty. Return NULL instead.")
    return(NULL)
  }

  cor_limits <- edges_tbl$cor %>%
    abs() %>%
    max() %>%
    {
      c(-., .)
    }

  # get most frequent node groups
  feature_groups <-
    edges_tbl %>%
    {
      c(.$from, .$to)
    } %>%
    tibble::tibble(feature = .) %>%
    group_by(feature) %>%
    dplyr::count() %>%
    dplyr::inner_join(nodes_tbl, by = "feature") %>%
    dplyr::group_by_at(features_grouping_col) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::arrange(-n) %>%
    utils::head(n_features) %>%
    purrr::pluck(features_grouping_col)

  node_colors <-
    viridisLite::viridis(length(feature_groups)) %>%
    magrittr::set_names(feature_groups) %>%
    magrittr::inset("other", "grey")

  # same node tibble for all groups to uniform color coding
  nodes_tbl <-
    nodes_tbl %>%
    mutate_at(features_grouping_col, ~ ifelse(.x %in% feature_groups, .x, "other")) %>%
    dplyr::mutate(
      color = .[[features_grouping_col]] %>% factor(levels = names(node_colors)),
      tooltip = feature
    ) %>%
    dplyr::rename(name = feature)

  nested_cor_tbl <-
    edges_tbl %>%
    dplyr::group_by(correlation_group) %>%
    tidyr::nest() %>%
    dplyr::rename(edges_tbl = data) %>%
    dplyr::mutate(
      graph = purrr::map(edges_tbl, ~ danielLib::get_correlation_graph(edges_tbl = .x, nodes_tbl = nodes_tbl)),
      plt = purrr::map2(graph, correlation_group, ~ danielLib::plot_correlation_network(
        .x, .y,
        limits = cor_limits, node_colors = node_colors, color_title = features_grouping_col
      ))
    )

  return(nested_cor_tbl)
}
