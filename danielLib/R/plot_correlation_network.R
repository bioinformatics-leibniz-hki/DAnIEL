# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.


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
