# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Plot sample attributes distribution
#' Count values of factor and logical columns.
#' Plots their counts as `ggiraph` enabled ggplot
#' @param samples_tbl sample meta data table with attribute columns
#' @param min_frac_show minimum faction of x axis to show a label
plot_samples_distribution <- function(samples_tbl, min_frac_show = 0.2, bg_color = "transparent") {
  samples_tbl %>%
    # select factor columns only
    dplyr::select_if(base::sapply(., base::is.factor) | base::sapply(., base::is.logical)) %>%
    dplyr::mutate_all(~ .x %>%
      as.character() %>%
      replace_na("NA")) %>%
    tidyr::gather(attribute_key, attribute_val) %>%
    dplyr::group_by(attribute_key, attribute_val) %>%
    dplyr::count() %>%
    dplyr::group_by(attribute_key) %>%
    dplyr::mutate(label = ifelse(n > min_frac_show * sum(n), attribute_val, "")) %>%
    ggplot(aes(attribute_key, n, tooltip = attribute_val, label = label)) +
    ggiraph::geom_bar_interactive(stat = "identity", color = "black", fill = bg_color) +
    ggplot2::geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::coord_flip() +
    ggplot2::guides(fill = FALSE) +
    ggplot2::labs(y = "Sample", x = "Attribute")
}
