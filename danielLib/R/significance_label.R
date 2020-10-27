# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Create significance label with stars
#' Three, two or one star for p value less than 0.001, 0.01, and 0.05, respectively
#'
#' @param p.value p value
#' @param include_value write p value in addition to stars. Format `.2e` is used.
significance_label <- function(p.value, include_value = FALSE) {
  if (include_value) {
    p.formatted <- p.value %>% format(., digits = 2, scientific = TRUE)
    dplyr::case_when(
      p.value < 0.001 ~ paste0(p.formatted, " \u2605\u2605\u2605"),
      p.value < 0.01 ~ paste0(p.formatted, " \u2605\u2605"),
      p.value < 0.05 ~ paste0(p.formatted, " \u2605"),
      TRUE ~ paste0(p.formatted, "")
    )
  } else {
    dplyr::case_when(
      p.value < 0.001 ~ "\u2605\u2605\u2605",
      p.value < 0.01 ~ "\u2605\u2605",
      p.value < 0.05 ~ "\u2605",
      TRUE ~ ""
    )
  }
}
