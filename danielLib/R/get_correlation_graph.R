# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
