# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Plot abundance clustermap
#' @param features_tbl tibble with taxa abundances. Must have column sample_id
#' @param normalization_method string of normalization method used as legend title
#' @param taxonomic_rank string used as y axis title. Taxa are printed in italic if this is genus or species
#' @param clustering_distance clustering distance passed to ComplexHeatmap::Heatmap
plot_abundance_clustermap <- function(features_tbl, normalization_method, taxonomic_rank, clustering_distance = "spearman") {
  cluster_features <- features_tbl %>% ncol() > 2 # substract sample name column
  cluster_samples <- features_tbl %>% nrow() > 1

  generate_heatmap <- function(features_tbl, ...) {
    # color scale limits
    fill_range <-
      features_tbl %>%
      tidyr::gather(feature, abundance, -sample_id) %>%
      dplyr::pull(abundance) %>%
      base::abs() %>%
      base::max()

    # italic species and genus names
    fontface <- base::ifelse(taxonomic_rank %in% c("species", "genus"), "italic", "plain")

    features_tbl %>%
      magrittr::set_rownames(.$sample_id) %>%
      dplyr::select(-sample_id) %>%
      base::as.matrix() %>%
      base::t() %>%
      ComplexHeatmap::Heatmap(
        # general
        name = normalization_method,
        col = circlize::colorRamp2(c(-fill_range, 0, fill_range), c(scales::muted("blue"), "white", scales::muted("red"))),

        # features (rows)
        row_title = taxonomic_rank,
        row_names_gp = grid::gpar(fontface = fontface),
        row_names_max_width = grid::unit(Inf, "cm"),
        row_names_side = "right",

        # samples (columns)
        column_title = "sample",
        column_names_max_height = grid::unit(Inf, "cm"),
        column_names_centered = TRUE,
        ...
      )
  }

  purrr::when(
    cluster_features && cluster_samples ~ "both",
    cluster_features ~ "features",
    cluster_samples ~ "samples",
    TRUE ~ "none"
  ) %>%
    switch(
      "both" = generate_heatmap(
        features_tbl,
        cluster_rows = TRUE,
        cluster_columns = TRUE,
        clustering_distance_rows = clustering_distance,
        clustering_distance_columns = clustering_distance
      ),
      "samples" = generate_heatmap(
        features_tbl,
        cluster_rows = TRUE,
        clustering_distance_rows = clustering_distance
      ),
      "features" = generate_heatmap(
        features_tbl,
        cluster_columns = TRUE,
        clustering_distance_columns = clustering_distance
      ),
      "none" = generate_heatmap(
        features_tbl,
        cluster_rows = FALSE,
        cluster_columns = FALSE
      ),
      NULL
    )
}
