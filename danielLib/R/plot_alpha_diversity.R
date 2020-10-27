# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Plot alpha diversity box plot
#' @param abundance_tbl abudnance tibble with colum sample_id. Raw denoised counts preferred.
#' @param samples_tbl samples meta data tibble with column sample_id. Only needed if grouping is provided
#' @param grouping name of column in samples_tbl used to group samples
#' @param singnif_bars show significance bars (Kruskal_Wallis and post hoc Dunn's test)
#' @return ggplot boxplot of alpha diversities
plot_alpha_diversity <- function(abundance_tbl, samples_tbl = NULL, grouping = NULL, singnif_bars = TRUE) {
  alphadiv_tbl <-
    abundance_tbl %>%
    get_alpha_diversity_all()

  if (is.null(grouping)) {
    alpha_div_plt <-
      alphadiv_tbl %>%
      tidyr::gather(metric, value, -sample_id) %>%
      ggplot2::ggplot(aes(x = "", y = value)) +
      ggplot2::geom_boxplot() +
      ggplot2::facet_wrap(~metric, scales = "free_y") +
      ggplot2::labs(y = "Alpha diversity", x = "")
    return(alpha_div_plt)
  }

  tbl <-
    alphadiv_tbl %>%
    tidyr::gather(metric, value, -sample_id) %>%
    dplyr::inner_join(samples_tbl, by = "sample_id") %>%
    dplyr::rename_at(grouping, ~"sample_group")

  alpha_div_plt <-
    tbl %>%
    mutate(sample_group = sample_group %>% as.character()) %>%
    ggplot2::ggplot(aes(sample_group, value, color = sample_group)) +
    ggplot2::geom_boxplot() +
    ggplot2::facet_wrap(~metric, scales = "free_y") +
    ggplot2::theme(axis.text.x = element_blank()) +
    ggplot2::labs(x = grouping, y = "Alpha diversity", color = grouping)

  if (!singnif_bars) {
    return(alpha_div_plt)
  }

  if (samples_tbl %>% purrr::pluck(grouping) %>% unique() %>% length() < 2) {
    warning("Grouping variable has less than two levels. Significance calculation will be ignored. ")
    return(alpha_div_plt)
  }

  for (facet_val in tbl$metric %>% unique()) {
    alpha_div_plt <-
      alpha_div_plt +
      # must be run for each facet group
      danielLib::stat_compare_medians(data = tbl, g = "sample_group", x = "value", facet_key = "metric", facet_val = facet_val)
  }
  return(alpha_div_plt)
}
