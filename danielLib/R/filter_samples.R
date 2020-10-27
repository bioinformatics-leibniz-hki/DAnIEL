# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.


#' Filters existing samples
#' @return sampels tbl with filtered samples
filter_samples <- function(samples_tbl, projects, projects_filter_query = "") {
  filtered_samples_tbl <-
    samples_tbl %>%
    dplyr::filter(bioproject_id %in% projects) %>%
    dplyr::collect()

  attributes <- filtered_samples_tbl %>%
    pull(attribute_key) %>%
    unique()

  filtered_samples_tbl %>%
    tidyr::spread(attribute_key, attribute_val) %>%
    # filter samples from existing cohorts if filter query provided
    purrr::when(
      projects_filter_query != "" ~ {
        dplyr::filter(., parse(
          text = projects_filter_query %>% sanitize_expression_string()
        ) %>% eval())
      },
      TRUE ~ .
    ) %>%
    dplyr::select(sample_id, project = bioproject_id, one_of(attributes)) %>%
    # remove columns which have the same value. Keep also sample_id and project
    dplyr::group_by(sample_id, project) %>%
    dplyr::select_if(~ unique(.x) %>% length() > 1) %>%
    guess_coltypes() %>%
    dplyr::select_if(is.factor) %>%
    dplyr::ungroup()
}
