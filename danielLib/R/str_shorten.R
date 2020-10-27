# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Shorten a string to fixed size
#' @param s string to shorten
#' @param maxchars maximum length of shortened string
str_shorten <- function(s, maxchars = 15) {
  s %>% {
    ifelse(nchar(.) > maxchars, str_sub(., 1, maxchars) %>% paste0(., "..."), .)
  }
}
