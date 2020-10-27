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
# summarize sparcc results
#

require(tidyverse)
require(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = c("-c", "--cor-tsv"),
    type = "character",
    help = "SparCC correlation coefficients (TSV matrix)"
  ),
  optparse::make_option(
    opt_str = c("-p", "--pval-tsv"),
    type = "character",
    help = "SparCC p values (TSV matrix)"
  ),
  optparse::make_option(
    opt_str = c("-o", "--out-csv"),
    type = "character",
    default = "/dev/stdout",
    help = "Summary output (CSV table)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Summarize SparCC p value and correlation results"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(cor_tsv = "cor.tsv", pval_tsv = "pval.tsv", pval_adjust = "fdr", out_csv = "cor.csv")

cor_tbl <-
  readr::read_tsv(args$cor_tsv) %>%
  dplyr::rename(feature_a = `#OTU ID`) %>%
  tidyr::gather(feature_b, cor, -feature_a)

pval_tbl <-
  readr::read_tsv(args$pval_tsv) %>%
  dplyr::rename(feature_a = `#OTU ID`) %>%
  tidyr::gather(feature_b, p_value, -feature_a) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(q_value = stats::p.adjust(p_value, method = args$pval_adjust))

tbl <-
  cor_tbl %>%
  dplyr::full_join(pval_tbl, by = c("feature_a", "feature_b")) %>%
  # correlation matrix is symmetric. Keep only one triangle
  dplyr::rowwise() %>%
  dplyr::mutate(comp = base::c(feature_a, feature_b) %>% base::sort() %>% base::paste0(collapse = "")) %>%
  dplyr::group_by(comp) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::select(-comp) %>%
  # filter obvious auto correlations
  dplyr::filter(!feature_a == feature_b)

write_csv(tbl, args$out_csv)
