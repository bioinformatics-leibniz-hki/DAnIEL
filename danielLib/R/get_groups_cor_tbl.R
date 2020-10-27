# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
