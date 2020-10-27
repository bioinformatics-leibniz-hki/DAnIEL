# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Guess column classes from a tibble.
#' Columns sample_id and project will always be of type character and factor, respectively
guess_coltypes <- function(tbl) {
  # temp ungroup
  groups <- tbl %>% group_vars()
  tbl <- tbl %>% dplyr::ungroup()

  factor_cols <-
    tbl %>%
    tidyr::gather(k, v) %>%
    dplyr::filter(!is.na(v)) %>%
    dplyr::group_by(k, v) %>%
    dplyr::count() %>%
    # exclude columns in which each element is unique or always the same
    dplyr::filter(n > 1 & n < dim(tbl)[1]) %>%
    dplyr::pull(k) %>%
    base::unique()

  tbl %>%
    dplyr::mutate_at(.vars = factor_cols, factor) %>%
    dplyr::mutate_at(vars(contains("sample_id")), as.character) %>%
    dplyr::mutate_at(vars(contains("project")), as.factor) %>%
    # regroup last to allow mutate probable grouping columns
    dplyr::group_by_at(groups)
}
