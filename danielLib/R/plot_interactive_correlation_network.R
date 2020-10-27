# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
