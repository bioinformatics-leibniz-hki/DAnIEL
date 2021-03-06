% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_correlation_network.R
\name{plot_correlation_network}
\alias{plot_correlation_network}
\title{Generates correlation entwork plot of one group
 @param graph graph to draw. igraph or tbl_graph.
 @param limits limits for edge color scale (correlation coefficient)
 @param node_colors Named vector for coloring nodes
 @param color_title Title of node color guide
 @param title Title of the plot
 @param low_edge_color color for lower limit of edge color scale (correlation coefficient)
 @param high_edge_color color for higher limit of edge color scale (correlation coefficient)}
\usage{
plot_correlation_network(
  graph,
  limits,
  node_colors,
  color_title,
  title = "",
  low_edge_color = "#3A3A98",
  high_edge_color = "#832424",
  node_size = 3
)
}
\description{
Generates correlation entwork plot of one group
 @param graph graph to draw. igraph or tbl_graph.
 @param limits limits for edge color scale (correlation coefficient)
 @param node_colors Named vector for coloring nodes
 @param color_title Title of node color guide
 @param title Title of the plot
 @param low_edge_color color for lower limit of edge color scale (correlation coefficient)
 @param high_edge_color color for higher limit of edge color scale (correlation coefficient)
}
