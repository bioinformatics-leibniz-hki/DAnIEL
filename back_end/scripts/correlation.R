#!/usr/bin/env Rscript

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#
# BAnOCC correlation analysis
#

require(tidyverse)
require(magrittr)
require(optparse)
require(Hmisc)

args <- base::list(
  optparse::make_option(
    opt_str = "--features-csv",
    type = "character",
    default = "/dev/stdin",
    help = "Feature abundance matrix. At least 5 rows and at least 2 columns (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-o", "--results-csv"),
    type = "character",
    default = "/dev/stdout",
    help = "Output correlation table with statistics (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-m", "--method"),
    type = "character",
    default = "spearman",
    help = "Correlation method (spearman or pearson)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "BAnOCC correlation between features"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#     features_norm_csv = "~/prj/6-its-web-server/userdat/poster/features/Species_low_prev/features.csv",
#     results_csv = "~/prj/6-its-web-server/userdat/poster/analysis/Default_analysis/correlation/results.csv",
#     method = "spearman"
# )

#' Triangulize matrix to tibble
#' Keep only unique correlation pairs
#' Remove autocorrelations
#' @param unit_name name of value column
triangulize_cor_mat <- function(cor_mat, unit_name) {
  cor_mat %>%
    dplyr::as_tibble(rownames = "feature_a") %>%
    tidyr::gather(feature_b, !!unit_name, -feature_a) %>%
    # correlation matrix is symmetric. Keep only one triangle
    dplyr::rowwise() %>%
    dplyr::mutate(comp = base::c(feature_a, feature_b) %>% base::sort() %>% base::paste0(collapse = "")) %>%
    dplyr::group_by(comp) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-comp) %>%
    # filter obvious auto correlations
    dplyr::filter(!feature_a == feature_b)
}

feature_mat <-
  readr::read_csv(args$features_csv) %>%
  magrittr::set_rownames(.$sample_id) %>%
  dplyr::select(-sample_id) %>%
  base::as.matrix()

cor <- Hmisc::rcorr(x = feature_mat, type = args$method)

result_tbl <-
  cor$r %>%
  triangulize_cor_mat(unit_name = "cor") %>%
  dplyr::full_join(
    y = triangulize_cor_mat(cor$P, unit_name = "p_value"),
    by = c("feature_a", "feature_b")
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    q_value = stats::p.adjust(p_value, method = "fdr")
  )

readr::write_csv(result_tbl, args$results_csv)
