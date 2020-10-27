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

library(tidyverse)
library(magrittr)
library(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = "--samples-meta-csv",
    type = "character",
    help = "Samples meta data table (CSV)"
  ),
  optparse::make_option(
    opt_str = "--features-csv",
    type = "character",
    help = "Normalized feature profile table (CSV)"
  ),
  optparse::make_option(
    opt_str = "--features-raw-csv",
    type = "character",
    help = "Raw feature profile table (CSV)"
  ),
  optparse::make_option(
    opt_str = "--correlation-dir",
    type = "character",
    help = "Main correlation directory"
  ),
  optparse::make_option(
    opt_str = "--correlation-grouping",
    type = "character",
    default = "all",
    help = "Column name of sample meta data table used for grouping. Use 'all' to create just one group of all samples"
  ),
  optparse::make_option(
    opt_str = "--min-samples",
    type = "integer",
    default = 2,
    help = "Min number of samples per group with any feature prevalent"
  ),
  optparse::make_option(
    opt_str = "--min-features",
    type = "integer",
    default = 5,
    help = "Min number of features per group with any feature prevalent. Sparcc needs 5."
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Creatates files needed for group wise correlation analyzes"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#   correlation_grouping = "disease",
#   min_samples = 2,
#   min_features = 5,
#   correlation_dir = "analysis/Selected_Analysis/correlation/",
#   samples_meta_csv = "input/samples.csv",
#   features_csv = "features/Selected_Features/features.csv",
#   features_raw_csv = "features/Selected_Features/features.raw.csv"
# )

#' Extract per group features profiles
#' @param abundance_col name of abudnance column in tbl
#' @param tbl table with columns sample_id, feature, and a abundance column
#' @param features character list of feature column names
get_features_tbl <- function(tbl, abundance_col, features) {
  tbl %>%
    dplyr::select(sample_id, feature, !!abundance_col) %>%
    dplyr::distinct_all() %>%
    tidyr::spread(feature, !!abundance_col, fill = 0) %>%
    dplyr::select(sample_id, features)
}

#' Nest table for grouping
#' @param tbl table with columns
#' @param grouping column name used for grouping. Use all for no grouping
nest_tbl <- function(tbl, grouping = "all") {
  if (grouping == "all") {
    # no real nesting needed
    dummy_nested_tbl <-
      tbl %>%
      dplyr::mutate(group = "all") %>%
      dplyr::group_by(group) %>%
      tidyr::nest()

    return(dummy_nested_tbl)
  }

  correlation_groups <-
    tbl %>%
    purrr::pluck(grouping) %>%
    setdiff(NA) %>%
    unique()

  nested_tbl <-
    tbl %>%
    dplyr::group_by_at(grouping) %>%
    tidyr::nest() %>%
    dplyr::rename_at(grouping, ~"group")

  return(nested_tbl)
}

samples_tbl <- readr::read_csv(args$samples_meta_csv, col_types = cols(sample_id = "c", project = "f"))
features_tbl <- readr::read_csv(args$features_csv, col_types = cols(sample_id = "c", project = "f"))
features_raw_tbl <- readr::read_csv(args$features_raw_csv, col_types = cols(sample_id = "c", project = "f"))

tbl <-
  list(
    features_tbl %>% tidyr::gather(feature, norm_abundance, -sample_id),
    features_raw_tbl %>% tidyr::gather(feature, raw_abundance, -sample_id)
  ) %>%
  purrr::reduce(~ dplyr::full_join(.x, .y, by = c("sample_id", "feature"))) %>%
  dplyr::inner_join(samples_tbl, by = "sample_id") %>%
  dplyr::arrange(sample_id)

# create per group feature profiles
nested_tbl <-
  tbl %>%
  nest_tbl(grouping = args$correlation_grouping) %>%
  mutate(
    features = purrr::map(data, ~ .x %>%
      filter(raw_abundance > 0) %>%
      pull(feature) %>%
      unique()),
    samples = purrr::map(data, ~ .x %>%
      filter(raw_abundance > 0) %>%
      pull(sample_id) %>%
      unique()),
    n_features = purrr::map_int(features, ~ .x %>%
      simplify() %>%
      length()),
    n_samples = purrr::map_int(samples, ~ .x %>%
      simplify() %>%
      length()),
    features_raw_tbl = purrr::map2(data, features, ~ get_features_tbl(.x, "raw_abundance", .y)),
    features_norm_tbl = purrr::map2(data, features, ~ get_features_tbl(.x, "norm_abundance", .y)),
  ) %>%
  dplyr::filter(
    # filter by requirements
    n_features >= args$min_features &
      n_samples >= args$min_samples &
      # ignore samples not assigned to any group
      !is.na(group)
  )

# create per group directory
nested_tbl$group %>%
  purrr::map_chr(~ sprintf("%s/%s", args$correlation_dir, .x)) %>%
  purrr::walk(~ dir.create(.x, showWarnings = FALSE, recursive = TRUE))

# create groups files
nested_tbl$group %>%
  readr::write_lines(sprintf("%s/groups.txt", args$correlation_dir))

# save per group features profiles
purrr::walk2(
  nested_tbl$features_raw_tbl,
  nested_tbl$group,
  ~ readr::write_csv(.x, sprintf("%s/%s/features.raw.csv", args$correlation_dir, .y))
)
purrr::walk2(
  nested_tbl$features_norm_tbl,
  nested_tbl$group,
  ~ readr::write_csv(.x, sprintf("%s/%s/features.csv", args$correlation_dir, .y))
)
