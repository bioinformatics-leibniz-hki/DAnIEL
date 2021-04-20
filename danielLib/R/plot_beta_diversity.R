# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Plot beta diversity ordination
#' @param features_phy phyloseq object cotaining features, sample meta data. Must also contain a phylogenetic tree if required by ord_distance (e.g. UniFrac)
#' @param ord_method method used for dimension reduction in ordination. Equivalent to method in phyloseq::ordinate.
#' @param ord_distance method used to calculate distance between samples and features. Equivalent to distance in phyloseq::ordinate.
#' @param features_grouping column name of otu table in  features_phy used to color feature arrows in biplot. NULL for no coloring.
#' @param samples_grouping column name of samples meta data in features_phy used to group samples in adonis PERMANOVA test. NULL for no test.
plot_beta_diversity <- function(features_phy, ord_method = "PCoA", ord_distance = "bray", features_grouping = NULL, samples_grouping = NULL) {
  features_ord <- phyloseq::ordinate(features_phy, method = ord_method, distance = ord_distance)
  features_column <-
    features_phy %>%
    phyloseq::tax_table() %>%
    as.data.frame() %>%
    colnames() %>%
    purrr::pluck(1)

  axes_labels <-
    switch(
      ord_method,
      PCoA = {
        c(
          sprintf("PCo1 (%.1f%%)", features_ord$values$Relative_eig[1] * 100),
          sprintf("PCo2 (%.1f%%)", features_ord$values$Relative_eig[2] * 100)
        )
      },
      c(paste(ord_method, "1"), paste(ord_method, "2"))
    )

  plt <-
    phyloseq::plot_ordination(physeq = features_phy, ordination = features_ord, justDF = TRUE, type = "biplot") %>%
    as_tibble() %>%
    magrittr::set_colnames(colnames(.) %>% .[3:length(.)] %>% c("axis1", "axis2", .)) %>%
    ggplot2::ggplot(mapping = aes(axis1, axis2)) +

    # draw sample points
    ggiraph::geom_point_interactive(
      data = . %>% dplyr::filter(id.type == "Samples"),
      mapping = aes_string(tooltip = "sample_id", color = samples_grouping),
      size = 3
    ) +
    ggplot2::scale_color_hue() +
    ggnewscale::new_scale_color() +

    # draw taxa arrows
    ggiraph::geom_segment_interactive(
      data = . %>% dplyr::filter(id.type == "Taxa"),
      mapping = ggplot2::aes_string(xend = 0, yend = 0, color = features_grouping, tooltip = features_column),
      arrow = arrow(ends = "first"),
      linejoin = "round"
    ) +
    ggplot2::scale_color_viridis_d() +
    ggplot2::labs(
      x = axes_labels[1],
      y = axes_labels[2]
    )

  # Add adonis ordination
  adonis_meta <-
    features_phy %>%
    phyloseq::sample_data() %>%
    tibble::as_tibble() %>%
    dplyr::select(sample_id, samples_grouping = !!samples_grouping)

  adonis_groups_count <- adonis_meta$samples_grouping %>%
    unique() %>%
    length()

  if (!is.null(samples_grouping) && adonis_groups_count >= 2) {
    adonis_text <-
      vegan::adonis(
        formula = phyloseq::distance(features_phy, method = ord_distance) ~ samples_grouping,
        data = adonis_meta,
        permutations = 10e3
      ) %>%
      purrr::pluck("aov.tab") %>%
      tibble::as_tibble(rownames = "var") %>%
      dplyr::rename(p_value = `Pr(>F)`) %>%
      dplyr::filter(var == "samples_grouping") %>%
      dplyr::mutate(
        p_label = dplyr::case_when(
          p_value < 1e-3 ~ "\u2605\u2605\u2605",
          p_value < 1e-2 ~ "\u2605\u2605",
          p_value < 0.05 ~ "\u2605",
          TRUE ~ "NS"
        ),
        p_text = sprintf("Adonis p=%.2e %s", p_value, p_label)
      ) %>%
      dplyr::pull(p_text) %>%
      dplyr::first()

    plt <- plt + ggplot2::annotate("text", x = -Inf, y = Inf, hjust = 0, vjust = 1, label = adonis_text)
  }

  plt
}
