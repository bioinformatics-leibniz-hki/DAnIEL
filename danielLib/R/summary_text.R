# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' @param vec vector to be summarized
#' @param percentage assumes vec are percentages
#' @param num_gmt number formater
#' @return a summary text e.g. from 1% to 2% (Average=1.5%)
summary_text <- function(vec, percentage = FALSE, num_fmt = "g") {
  vec %>%
    summary() %>%
    as.list() %>%
    {
      min <- .$`Min.`
      max <- .$`Max.`
      mean <- .$`Mean`
      perc_s <- ifelse(percentage, "%%", "")
      fmt <- paste0("from %", num_fmt, perc_s, " to %", num_fmt, perc_s, " (Average %", num_fmt, perc_s, ")")

      sprintf(fmt, min, max, mean)
    }
}
