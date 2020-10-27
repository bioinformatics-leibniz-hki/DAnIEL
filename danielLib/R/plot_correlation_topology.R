# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
