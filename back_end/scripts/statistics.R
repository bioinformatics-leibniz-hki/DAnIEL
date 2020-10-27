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
# Statistics
# - Two group statistics (paired and unpaired)
# - automatated detection of test dependend on number of unique values in meta data table
# - Analysis of Variance
# - Correlation test
# IN  Table about normalized abnundance profile and sample meta data
# OUT Table summarizing statistic results
#

library(magrittr)
library(stats)
library(effsize)
library(tidyverse)
library(purrr)
library(optparse)
library(dunn.test)
library(broom)

args <- base::list(
  optparse::make_option(
    opt_str = c("-f", "--features-csv"),
    type = "character",
    help = "Normalized feature abundance profile (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-s", "--samples-csv"),
    type = "character",
    help = "Sample meta data table (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-o", "--out-csv"),
    type = "character",
    help = "output statistics summary (CSV)"
  ),
  optparse::make_option(
    opt_str = "--two-groups-stat-func",
    type = "character",
    default = "stats::wilcox.test",
    help = "Two groups test to apply (R function)"
  ),
  optparse::make_option(
    opt_str = "--one-way-aov-stat-func",
    type = "character",
    default = "stats::kruskal.test",
    help = "One way Analysis of Variance test to apply (R function)"
  ),
  optparse::make_option(
    opt_str = "--cor_stat_func",
    type = "character",
    default = "stats::cor.test",
    help = "Correlation test to apply (R function)"
  ),
  optparse::make_option(
    opt_str = "--max-pvalue",
    type = "double",
    default = 0.05,
    help = "Maximum p value to count significance (0-1)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Statistics (two groups, one way AOV, and correlation tests)"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#    samples_csv = "/sbidata/server/daniel/userdat/example/input/samples.csv",
#    features_csv = "/sbidata/server/daniel/userdat/example/features/Example_Features/features.csv",
#    out_csv = "/sbidata/server/daniel/userdat/example/analysis/Example_Analysis/statistics/stat.csv",
#    two_groups_stat_func = "stats::wilcox.test",
#    one_way_aov_stat_func = "stats::kruskal.test",
#    cor_stat_func = "stats::cor.test",
#    max_pvalue = 0.05
# )

# parse statistics functions
args$two_groups_stat_func %<>% parse(text = .) %>% eval()
args$one_way_aov_stat_func %<>% parse(text = .) %>% eval()
args$cor_stat_func %<>% parse(text = .) %>% eval()

#' Guess column classes from a tibble.
#' Columns sample_id and project will always be of type character and factor, respectively
#' TODO: replace this function with danielLib::read_csv_guess_coltypes
#' ISSUE: danielLib is not available in anaconda for snakemake
read_csv_guess_coltypes <- function(csv_path) {
  tbl <- readr::read_csv(csv_path)

  # temp ungroup
  groups <- tbl %>% group_vars()
  tbl <- tbl %>% dplyr::ungroup()

  factor_cols <-
    tbl %>%
    tidyr::gather(k, v) %>%
    dplyr::filter(!is.na(v)) %>%
    dplyr::group_by(k, v) %>%
    dplyr::count() %>%
    # exclude columns in which each element is unique or always the same
    dplyr::filter(n > 1 & n < dim(tbl)[1]) %>%
    dplyr::pull(k) %>%
    base::unique()

  tbl %>%
    dplyr::mutate_at(.vars = factor_cols, factor) %>%
    dplyr::group_by_at(groups) %>%
    dplyr::mutate_at(vars(contains("sample_id")), as.character) %>%
    dplyr::mutate_at(vars(contains("project")), as.factor)
}

features_tbl <-
  readr::read_csv(args$features_csv, col_types = cols(sample_id = "c")) %>%
  tidyr::gather(feature, abundance, -sample_id)

samples_tbl <-
  read_csv_guess_coltypes(args$samples_csv) %>%
  dplyr::filter(sample_id %in% features_tbl$sample_id)

test_cols <-
  samples_tbl %>%
  tidyr::gather(k, v, -sample_id) %>%
  dplyr::group_by(k, v) %>%
  dplyr::count() %>%
  dplyr::group_by(k) %>%
  dplyr::count() %>%
  dplyr::filter(
    # more than one unique obervation per group column
    n > 1 &&
      # remove group columns w/o replicas
      n < length(samples_tbl$sample_id) &&
      # exclude demultiplexing info columns
      !k %in% c("barcode_file", "barcode_seq")
  ) %>%
  dplyr::pull(k)

if (test_cols %>% length() == 0) {
  tibble() %>% readr::write_csv(args$out_csv)
  quit()
}

samples_tbl %<>% dplyr::select(sample_id, dplyr::contains("pair_id"), test_cols)

logical_cols <-
  samples_tbl %>%
  dplyr::select_if(base::is.logical) %>%
  base::colnames()

factor_cols <-
  samples_tbl %>%
  dplyr::select_if(base::is.factor) %>%
  base::colnames() %>%
  base::setdiff(c("barcode_seq", "barcode_file", "pair_id"))

numeric_cols <-
  samples_tbl %>%
  dplyr::select_if(base::is.numeric) %>%
  base::colnames() %>%
  base::setdiff("pair_id")

binary_factor_cols <-
  factor_cols %>%
  base::sapply(
    function(x) {
      samples_tbl %>%
        purrr::pluck(x) %>%
        base::unique() %>%
        purrr::discard(function(x) base::is.na(x)) %>%
        base::length()
    }
  ) %>%
  purrr::keep(function(x) x == 2) %>%
  base::names()

# gather columns by test
one_way_aov_cols <- factor_cols %>% base::setdiff(binary_factor_cols)
two_groups_cols <- logical_cols %>% base::union(binary_factor_cols)
cor_cols <- numeric_cols

#' try any function which works with broom::tidy
#' @param f function to try
#' @return tibble. either tidy if
try_func <- function(func, paired, ...) {
  tryCatch(
    expr = func(...) %>% broom::tidy(),
    error = function(e) tibble::tibble(error = e$message)
  )
}

get_valid_pair_ids <- function(data) {
  data %>%
    dplyr::group_by(pair_id) %>%
    dplyr::count() %>%
    dplyr::filter(n == 2) %>%
    dplyr::pull(pair_id)
}

#' performes paired tests whenever possible
#' performs unpaired tests else
#' @param  tbl tibble with at least partially paired column pair_id
paired_two_groups_test <- function(tbl, func = stats::wilcox.test, ...) {
  if (!"pair_id" %in% base::colnames(tbl)) {
    stop("column pair_id not found")
  }
  res_tbl <- tibble(error = character(), paired = logical())

  group_vals <- tbl$group_val %>%
    na.omit() %>%
    unique()

  # samples might get excluded during upstream steps e.g. QC
  # only pairs with exactly two valid samples are tested
  valid_pair_ids <- get_valid_pair_ids(tbl)

  paired_samples <-
    tbl %>%
    dplyr::filter(pair_id %in% valid_pair_ids) %>%
    dplyr::pull(sample_id) %>%
    base::unique()

  paired_tbl <-
    tbl %>%
    dplyr::filter(pair_id %in% valid_pair_ids) %>%
    # arrange for paired test
    dplyr::arrange(pair_id, group_val)

  # test if there are exactly 2 value columns for paired testing
  # return error tibble otherwise
  if (paired_tbl %>% colnames() %>% setdiff(c(NA, "sample_id", "pair_id")) %>% length() == 2) {
    paired_res_tbl <-
      try_func(
        f = func,
        formula = abundance ~ group_val,
        data = paired_tbl,
        paired = TRUE,
        ...
      ) %>%
      dplyr::mutate(
        paired = TRUE,
        sample_ids = paired_tbl$sample_id %>% unique() %>% paste(collapse = ",")
      )

    res_tbl %<>% dplyr::bind_rows(paired_res_tbl)
  } else {
    res_tbl %<>% dplyr::add_row(error = "grouping factor must have exactly 2 levels", paired = TRUE)
  }

  # perform unpaired test on other samples
  unpaired_tbl <-
    tbl %>%
    # samples must not used in paired test already
    dplyr::filter(!sample_id %in% paired_samples)

  if (unpaired_tbl$group_val %>% unique() %>% setdiff(NA) %>% length() == 2) {
    unpaired_res_tbl <-
      unpaired_tbl %>%
      try_func(
        f = func,
        formula = abundance ~ group_val,
        data = .,
        paired = FALSE
      ) %>%
      dplyr::mutate(
        paired = FALSE,
        sample_ids = unpaired_tbl$sample_id %>% unique() %>% paste(collapse = ",")
      )

    res_tbl %<>% dplyr::bind_rows()
  } else {
    res_tbl %<>% dplyr::add_row(error = "grouping factor must have exactly 2 levels", paired = FALSE)
  }
  return(res_tbl)
}

#' performs two group statistics
#' @param tbl tibble with required columns: sample_id, group_val, abundance. Column pair_id is ooptional for paired tests.
#' @param func test fucntion
#' @return tibble with eitehr error message or tidy test result
two_groups_test <- function(tbl, func = stats::wilcox.test, ...) {
  # #test if grouping column contains exact two unique values. NAs are getting ignored
  # if (!tbl$group_val %>%
  #   base::unique() %>%
  #   base::setdiff(NA) %>%
  #   base::length() %>%
  #   magrittr::equals(2)) {
  #   return(tibble::tibble(error = "grouping factor must have exactly 2 levels"))
  # }

  # paired tests
  if ("pair_id" %in% base::colnames(tbl)) {
    paired_two_groups_test(tbl, func, ...)
  } else {
    # only unpaired tests
    tbl %>%
      try_func(
        f = func,
        formula = abundance ~ group_val,
        data = .,
        paired = FALSE,
        ...
      ) %>%
      dplyr::mutate(
        paired = FALSE,
        sample_ids = tbl$sample_id %>% unique() %>% paste(collapse = ",")
      )
  }
}

#' performs statistics with more than two groups (onw way analysis of variance)
#' @param tbl tibble with required columns: sample_id, group_val with >= 3 unique values, abundance.
#' @param func test fucntion
#' @return tibble with eitehr error message or tidy test result
one_way_aov_test <- function(tbl, func = stats::kruskal.test, ...) {
  try_func(
    f = func,
    formula = abundance ~ as.factor(group_val),
    data = tbl,
    ...
  ) %>%
    dplyr::mutate(
      paired = FALSE,
      sample_ids = tbl$sample_id %>% unique() %>% paste(collapse = ",")
    )
}

#' performs correlation tests
#' @param tbl tibble with required columns: sample_id, numeric group_val, abundance.
#' @param func test fucntion
#' @return tibble with eitehr error message or tidy test result
cor_test <- function(tbl, func = stats::cor.test, method = "spearman", ...) {
  # BUG: does not work with try_func. Workarround: explicit code
  tryCatch(
    expr = {
      func(
        formula = ~ group_val + abundance,
        data = tbl,
        method = method,
        paired = NA,
        ...
      ) %>%
        broom::tidy() %>%
        dplyr::mutate(
          paired = NA,
          sample_ids = tbl$sample_id %>% unique() %>% paste(collapse = ",")
        )
    },
    error = function(e) tibble::tibble(error = e$message)
  )
}

#' tidy effsize objects
#' @param x object of class effsize
tidy.effsize <- function(x) {
  tibble(
    estimate = x$estimate,
    conf.low = x$conf.int[["lower"]],
    conf.high = x$conf.int[["upper"]],
    method = x$method
  )
}

glance.effsize <- tidy.effsize

#' Log foldchange to estimate effect size
#' - func of foldchages if data is paired, foldchange of func elsewise
#' - NAs are ignored
#' @param data tibble with columns group_val (2 unique values, NA will ignored) and abundance
#' @param base base of the logarithm
#' @param func function to summarize fold changes
#' @return fold change
foldchange.log <- function(data, base = 2, func = base::mean) {
  foldchange.log.unpaired <- function(data) {
    data %>%
      # aggregate abundance
      dplyr::summarise(abundance_func = func(abundance)) %>%
      dplyr::summarise(
        # log_fc = log(B/A), A is the lexicographically first of the two groups
        log_fc = base::log(dplyr::nth(abundance_func, 2), base = base) - base::log(dplyr::nth(abundance_func, 1), base = base)
      ) %>%
      # replace 0/0=NaN with 0
      plyr::mutate(log_fc = log_fc %>% ifelse(is.nan(.), 0, .)) %>%
      dplyr::pull(log_fc)
  }

  foldchange.log.paired <- function(data) {
    x <-
      data %>%
      dplyr::filter(pair_id %in% get_valid_pair_ids(.)) %>%
      dplyr::filter(!is.na(group_val))

    if (x$group_val %>% unique() %>% length() == 2) {
      x %>%
        dplyr::select(-sample_id) %>%
        tidyr::spread(group_val, abundance) %>%
        magrittr::set_colnames(c("pair_id", "a", "b")) %>%
        # filter for valid pairs
        dplyr::filter(!(base::is.na(a) | base::is.na(b))) %>%
        # Calculate fold change and replace 0/0=NaN with 0
        dplyr::mutate(log_fc = base::log(b / a, base = base) %>% base::ifelse(base::is.nan(.), 0, .)) %>%
        # aggreate foldchange
        dplyr::pull(log_fc) %>%
        func()
    } else {
      return(NA)
    }
  }

  data %>%
    dplyr::filter(!(is.na(abundance) | is.na(group_val))) %>%
    dplyr::group_by(group_val) %>%
    purrr::when(
      "pair_id" %in% colnames(.) ~ foldchange.log.paired(.),
      foldchange.log.unpaired(.)
    )
}

#' adjusts p value if column p.value exists
#' @param tbl tidy tibble with test results
#' @param grouping_vars p value correction will take place inside these groupings
adjust_pval_if_possible <- function(tbl, grouping_vars = "group_var") {
  tbl %<>% dplyr::group_by(.dots = grouping_vars)
  if ("p.value" %in% colnames(tbl)) {
    tbl %>% dplyr::mutate(q.value = stats::p.adjust(p.value, method = "fdr"))
  } else {
    tbl
  }
}

#' calculates effect size of two groups
#' @param data tibble with columns group_val (2 unique values, NA will ignored), pair_id (optional) and abundance
two_groups_effsize <- function(data) {
  if (data$group_val %>% unique() %>% setdiff(NA) %>% length() != 2) {
    tibble(error = "grouping factor must have exactly 2 levels")
  } else {
    cliff_res <- effsize::cliff.delta(abundance ~ group_val, data = data) %>% tidy()
    fc_res <- foldchange.log(data) %>% tibble(estimate = ., method = "Log2 FC")

    higher_in <-
      fc_res$estimate %>%
      purrr::when(
        is.na(.) ~ NA,
        . < 0 ~ data$group_val %>%
          unique() %>%
          pluck(1),
        . > 0 ~ data$group_val %>%
          unique() %>%
          pluck(2)
      )

    cliff_res %>%
      dplyr::bind_rows(fc_res) %>%
      dplyr::select(method, estimate) %>%
      tidyr::spread(method, estimate) %>%
      dplyr::mutate(higher.in = higher_in)
  }
}

#' supress output e.g. from function cat
hush <- function(code) {
  sink("/dev/null")
  tmp <- code
  sink()
  return(tmp)
}

#' tidies dunn.test::dunn.test and adds effect sizes
dunn_test <- function(data, method = "bh", ...) {
  dunn_res_l <-
    hush(dunn.test::dunn.test(x = data$abundance, g = data$group_val, method = method, kw = FALSE, ...)) %>% as.list()

  #' grabs the nth element of list groups splitted by ' - '
  grab_group <- function(groups, n) {
    groups %>%
      stringr::str_split(" - ") %>%
      lapply(function(x) pluck(x, n)) %>%
      purrr::simplify()
  }

  post_hoc_effsize <- function(group_a, group_b) {
    data %>%
      dplyr::filter(group_val %in% c(group_a, group_b)) %>%
      two_groups_effsize()
  }

  tibble::tibble(
    term = dunn_res_l$comparisons,
    group_a = dunn_res_l$comparisons %>% grab_group(1),
    group_b = dunn_res_l$comparisons %>% grab_group(2),
    statistic = dunn_res_l$Z,
    p.value = dunn_res_l$P,
    method = "Dunn's Test",
  ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(q.value = p.value %>% stats::p.adjust(method = "fdr")) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(effsize = purrr::map2(group_a, group_b, ~ post_hoc_effsize(group_a, group_b))) %>%
    tidyr::unnest(effsize)
}

#' counts samples by unique values
#' @param group_var colum name of grouping
group_counts <- function(group_var) {
  samples_tbl %>%
    purrr::pluck(group_var) %>%
    base::table() %>%
    tibble::as_tibble() %>%
    dplyr::transmute(x = sprintf("%s(N=%s)", ., n)) %>%
    dplyr::pull(x) %>%
    base::paste(collapse = ", ")
}

#' Unify effect size columns
#' Reshapes the column containing effect size into two columns effsize_unit and effsize_value
#' @param test_tbl tibble of tidy test results
#' @param effsize_unit column name in test_tbl containing effect size
unify_effsize_cols <- function(test_tbl, effsize_col, effsize_unit) {
  test_tbl %<>% dplyr::mutate(effsize_unit = !!effsize_unit)

  if (effsize_col %in% colnames(test_tbl)) {
    test_tbl %>% dplyr::rename("effsize_value" = matches(effsize_col))
  } else {
    test_tbl %>% dplyr::mutate("effsize_value" = NA)
  }
}


test_tbl <- tibble()

if (two_groups_cols %>% length() > 0) {
  # test for exact 2 groups
  # Column pair_id used to indicate paired testing
  # if pair_id is NA, unpaired test will be done
  two_groups_test_tbl <-
    samples_tbl %>%
    dplyr::select(sample_id, two_groups_cols, dplyr::contains("pair_id")) %>%
    tidyr::gather(group_var, group_val, -sample_id, -dplyr::contains("pair_id")) %>%
    dplyr::inner_join(features_tbl, by = "sample_id") %>%
    dplyr::group_by(group_var, feature) %>%
    tidyr::nest() %>%
    dplyr::mutate(test_type = "two groups") %>%
    # individual tests
    dplyr::mutate(
      test = purrr::map(data, ~ two_groups_test(.x, func = args$two_groups_stat_func)),
      effsize = purrr::map(data, purrr::possibly(two_groups_effsize, NA)),
      group_a = purrr::map_chr(data, ~ .x %>%
        pull(group_val) %>%
        unique() %>%
        pluck(1)),
      group_b = purrr::map_chr(data, ~ .x %>%
        pull(group_val) %>%
        unique() %>%
        pluck(2))
    ) %>%
    tidyr::unnest(test, effsize) %>%
    unify_effsize_cols(effsize_col = "Cliff's Delta", effsize_unit = "Cliff's Delta") %>%
    # remove effsize error columns
    dplyr::select(-dplyr::matches("error[0-9]+")) %>%
    adjust_pval_if_possible()

  test_tbl %<>% dplyr::bind_rows(two_groups_test_tbl)
}

if (one_way_aov_cols %>% length() > 0) {
  # test for at least 3 groups. No need for paired data here.
  one_way_aov_test_tbl <-
    samples_tbl %>%
    dplyr::select(sample_id, one_way_aov_cols) %>%
    tidyr::gather(group_var, group_val, -sample_id) %>%
    dplyr::inner_join(features_tbl, by = "sample_id") %>%
    dplyr::group_by(group_var, feature) %>%
    tidyr::nest() %>%
    dplyr::mutate(test_type = "pre one way AOV") %>%
    # individual tests
    dplyr::mutate(test = purrr::map(data, function(data) one_way_aov_test(data, func = args$one_way_aov_stat_func))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(test) %>%
    adjust_pval_if_possible()

  one_way_aov_post_hoc_test_groups_tbl <-
    one_way_aov_test_tbl %>%
    dplyr::filter(p.value <= args$max_pvalue) %>%
    dplyr::select(group_var, feature)

  one_way_aov_post_hoc_test_tbl <-
    samples_tbl %>%
    dplyr::select(sample_id, one_way_aov_cols) %>%
    tidyr::gather(group_var, group_val, -sample_id) %>%
    dplyr::inner_join(features_tbl, by = "sample_id") %>%
    # filter significant pre tests
    dplyr::inner_join(one_way_aov_post_hoc_test_groups_tbl, by = c("group_var", "feature")) %>%
    dplyr::group_by(group_var, feature) %>%
    tidyr::nest() %>%
    dplyr::mutate(test_type = "post one way AOV")

  # BUG: purrr::map does not work.
  # WORARROUND: use lapply instead
  one_way_aov_post_hoc_test_tbl$test <- one_way_aov_post_hoc_test_tbl$data %>% lapply(dunn_test)

  one_way_aov_post_hoc_test_tbl %<>%
    unnest(test) %>%
    dplyr::select(-c(term))

  test_tbl %<>%
    dplyr::bind_rows(one_way_aov_test_tbl) %>%
    dplyr::bind_rows(one_way_aov_post_hoc_test_tbl)
}

if (cor_cols %>% length() > 0) {
  cor_test_tbl <-
    samples_tbl %>%
    dplyr::select(sample_id, cor_cols) %>%
    tidyr::gather(group_var, group_val, -sample_id) %>%
    dplyr::inner_join(features_tbl, by = "sample_id") %>%
    dplyr::group_by(group_var, feature) %>%
    tidyr::nest() %>%
    dplyr::mutate(test_type = "correlation") %>%
    # individual tests
    dplyr::mutate(test = purrr::map(data, function(data) cor_test(data, func = args$cor_stat_func, method = "spearman"))) %>%
    dplyr::select(-data) %>%
    tidyr::unnest(test) %>%
    adjust_pval_if_possible() %>%
    unify_effsize_cols(effsize_col = "estimate", effsize_unit = "Spearman's rho")

  test_tbl %<>% dplyr::bind_rows(cor_test_tbl)
}

test_tbl %<>%
  dplyr::rowwise() %>%
  dplyr::mutate(group_counts = group_counts(group_var))

test_tbl %>%
  dplyr::select(-dplyr::matches("data")) %>%
  readr::write_csv(args$out_csv, na = "")
