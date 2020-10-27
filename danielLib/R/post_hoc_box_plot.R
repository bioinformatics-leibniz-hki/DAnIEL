# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.


#' Post hoc box plot
#'
#' @param feature_tbl featute table
#' @param post_stat_results_tbl statistics results table of post doc tests
#' @param current_feature current feature used to filter feature_tbl
#' @param group_var grouping variable aka x axis of this plot
#' @return Post hoc box plot (ggplot2)
post_hoc_box_plot <- function(feature_tbl, post_stat_results_tbl, current_feature, group_var) {
  plot_tbl <-
    feature_tbl %>%
    tidyr::gather(feature, abundance, -sample_id) %>%
    dplyr::filter(feature == current_feature) %>%
    dplyr::left_join(samples_tbl, by = "sample_id") %>%
    dplyr::rename(x = group_var, y = abundance)

  max_y <-
    plot_tbl %>%
    dplyr::pull(y) %>%
    max()

  span_y <-
    plot_tbl %>%
    dplyr::pull(y) %>%
    min() %>%
    abs() %>%
    magrittr::add(abs(max_y))

  stat_tbl <-
    post_stat_results_tbl %>%
    dplyr::filter(feature == current_feature) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(group_a) %>%
    dplyr::transmute(
      group1 = group_a,
      group2 = group_b,
      y.position = dim(.)[1] %>% seq() %>% sapply(function(x) (x * 0.1 * span_y) + max_y),
      p = significance_label(p.value),
    ) %>%
    dplyr::filter(!p == "ns")

  plt <-
    plot_tbl %>%
    ggplot2::ggplot(aes(x = x, y = y)) +
    ggplot2::geom_boxplot() +
    ggpubr::stat_pvalue_manual(data = stat_tbl) +
    ggplot2::theme(
      text = ggplot2::element_text(size = 20),
      plot.title = ggplot2::element_text(face = "italic")
    ) +
    ggplot2::labs(
      title = current_feature,
      y = "Abundance",
      x = group_var
    )

  return(plt)
}
