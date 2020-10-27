# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Add median comparison P-values to a ggplot
#' @param data data used by ggplot
#'
#' @param g name of grouping column used for dunn.test
#' @param x name of value column used for dunn.test
#' @param facet column of data used for facetting
#' @param signif_bar_y_spacing fraction of plot hight used as spacer between two significance bars
#' @param max_pvalue maximum adjusted p value of by dunn.test to display
stat_compare_medians <- function(data = NULL, g = NULL, x = NULL, facet_key = NULL, facet_val = NULL,
                                 max_pvalue = 0.05, signif_bar_y_spacing = 0.1, ...) {
  # TODO: Data is not inherited from ggrobj and must be provided. Should be drawn from grob instead

  if (!is.null(facet_key)) {
    data <- dplyr::filter_at(data, facet_key, ~ .x == facet_val)
  }

  max <- data[[x]] %>% max()

  stat_tbl <-
    data %>%
    # replace NA which are discarded by dunn.test anyway
    dplyr::mutate_at(g, ~ str_replace_na(.x, "NA")) %>%
    do(
      quietly({
        dunn.test::dunn.test(x = .[[x]], g = .[[g]], kw = FALSE)
      }) %>%
        as.list() %>%
        tibble::as_tibble() %>%
        tidyr::separate(comparisons, into = c("group1", "group2"), sep = " - ") %>%
        dplyr::mutate(label = P.adjusted %>% significance_label())
    ) %>%
    dplyr::filter(P.adjusted <= max_pvalue) %>%
    dplyr::mutate(y.position = max + row_number() * max * signif_bar_y_spacing) %>%
    dplyr::mutate_at(c("group1", "group2"), ~ ifelse(.x == "NA", NA, .x))

  if (!is.null(facet_key)) {
    stat_tbl <-
      stat_tbl %>%
      dplyr::mutate(!!facet_key := facet_val)
  }

  ggpubr::stat_pvalue_manual(data = stat_tbl, ...)
}
