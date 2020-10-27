# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

kable <- function(df, caption = "", ...) {
  df %>%
    dplyr::ungroup() %>%
    # format p value columns
    dplyr::mutate_at(vars(matches("[pqPQ][._ ]+[vV]alue")), function(x) significance_label(x, include_value = TRUE)) %>%
    # format numeric columns
    # dplyr::mutate_if(base::is.numeric, list(~ sprintf("%g", .x))) %>%
    # format logical columns
    dplyr::mutate_if(base::is.logical, list(~ ifelse(., "yes", kableExtra::cell_spec("no", color = "red")))) %>%
    # format feature columns
    dplyr::mutate_at(vars(tidyselect::matches("^[Ff]eature[s]?[ ]?[AB]?$")), function(x) sprintf("<i>%s</i>", x)) %>%
    knitr::kable(caption = caption, escape = FALSE) %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed", "responsive"),
      full_width = FALSE,
      ...
    )
}
