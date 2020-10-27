# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

plot_girafe <- function(ggobj, width = 6, height = NULL, ...) {
  if (is.null(height)) height <- width

  options <- list(
    ggiraph::opts_zoom(max = 10)
  )
  ggiraph::girafe(ggobj = ggobj, options = options, width = width, height = height, ...)
}
