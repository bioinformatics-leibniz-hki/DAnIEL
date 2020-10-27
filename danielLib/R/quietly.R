# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' supress any output from a function
#' @seealso  https://r.789695.n4.nabble.com/Suppressing-output-e-g-from-cat-td859876.html
quietly <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}
