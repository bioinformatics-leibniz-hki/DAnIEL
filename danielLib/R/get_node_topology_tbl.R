# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
