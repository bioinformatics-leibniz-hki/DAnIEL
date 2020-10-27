# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

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
