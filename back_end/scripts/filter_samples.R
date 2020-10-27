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
# Filter feature abundance profile using a R expression
#

library(tidyverse)

args <- base::list(
  optparse::make_option(
    opt_str = "--in-features-csv",
    type = "character",
    help = "Input feature abundance matrix. At least 5 rows and at least 2 columns (CSV)"
  ),
  optparse::make_option(
    opt_str = "--in-samples-csv",
    type = "character",
    help = "Input sample meta data. One sample per row (CSV)"
  ),
  optparse::make_option(
    opt_str = "--in-raw-features-csv",
    type = "character",
    help = "Input raw feature abundance matrix. At least 5 rows and at least 2 columns. Count data only (CSV)"
  ),
  optparse::make_option(
    opt_str = "--out-features-csv",
    type = "character",
    help = "Output feature abundance matrix. At least 5 rows and at least 2 columns (CSV)"
  ),
  optparse::make_option(
    opt_str = "--out-raw-features-csv",
    type = "character",
    help = "Output raw feature abundance matrix. At least 5 rows and at least 2 columns. Count data only (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-q", "--query"),
    type = "character",
    help = "Query to filter samples. Columns of samples meta data table allowed. (R expression)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Filter feature abundance profile using a R expression"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#     in_samples_csv = "/sbidata/server/daniel/userdat/example/input/samples.csv",
#     in_raw_features_csv = "/sbidata/server/daniel/userdat/example/features/Default_features/features.raw.csv",
#     in_features_csv = "/sbidata/server/daniel/userdat/example/features/Default_features/features.csv",
#     out_raw_features_csv = "/sbidata/server/daniel/userdat/example/analysis/Default_analysis/correlation/features.raw.csv",
#     out_features_csv = "/sbidata/server/daniel/userdat/example/analysis/Default_analysis/correlation/features.csv",
#     query = ""
# )

samples_tbl <- readr::read_csv(args$in_samples_csv)
features_tbl <- readr::read_csv(args$in_features_csv)
raw_features_tbl <- readr::read_csv(args$in_raw_features_csv)
query_expr <- args$query %>% {
  parse(text = .)
}

samples <- samples_tbl$sample_id
if (!query_expr %>% purrr::is_empty()) {
  samples <-
    samples_tbl %>%
    dplyr::filter(query_expr %>% eval()) %>%
    dplyr::pull(sample_id)
}

features_tbl %>%
  dplyr::filter(sample_id %in% samples) %>%
  readr::write_csv(args$out_features_csv)

raw_features_tbl %>%
  dplyr::filter(sample_id %in% samples) %>%
  readr::write_csv(args$out_raw_features_csv)
