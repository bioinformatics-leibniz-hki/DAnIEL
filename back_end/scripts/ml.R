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
# Machine Learning
#

library(tidyverse)
library(caret)
library(doParallel)
library(optparse)
library(purrr)

args <- list(
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
    opt_str = c("-o", "--out-rdata"),
    default = "/dev/stdout",
    type = "character",
    help = "output ml objects (RData)"
  ),
  optparse::make_option(
    opt_str = c("-t", "--threads"),
    type = "integer",
    default = 5,
    help = "output ml objects (RData)"
  ),
  optparse::make_option(
    opt_str = "--min-samples-per-class",
    type = "integer",
    default = 3,
    help = "Mininum number of samples of every target classes. Modelling will be skipped otherwise."
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Machine learning"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#   features_csv = "/sbidata/server/daniel/userdat/test_1/features/Selected_Features/features.csv",
#   samples_csv = "/sbidata/server/daniel/userdat/test_1/input/samples.csv",
#   out_rdata = "/sbidata/server/daniel/userdat/test_1/analysis/Selected_Analysis/ml/ml.RData",
#   threads = 20,
#   min_samples_per_class = 3
# )

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

doParallel::registerDoParallel(cores = args$threads)

features_tbl <- readr::read_csv(args$features_csv, col_types = cols(sample_id = "c"))
samples_tbl <-
  read_csv_guess_coltypes(args$samples_csv) %>%
  # caret needs factors and no logical variables
  dplyr::mutate_if(is.logical, ~ ifelse("true", "false") %>% as.factor())

tbl <-
  samples_tbl %>%
  dplyr::inner_join(features_tbl, by = "sample_id")

features <-
  features_tbl %>%
  colnames() %>%
  setdiff("sample_id")

test_cols <-
  samples_tbl %>%
  dplyr::filter(sample_id %in% tbl$sample_id) %>%
  tidyr::gather(k, v, -sample_id) %>%
  dplyr::filter(!is.na(v)) %>%
  dplyr::group_by(k, v) %>%
  dplyr::count() %>%
  dplyr::group_by(k) %>%
  dplyr::count() %>%
  dplyr::filter(
    # more than one unique grouping value
    n > 1 &&
      # remove group columns w/o replicas
      n < samples_tbl %>%
        dplyr::pull(sample_id) %>%
        unique() %>%
        length() &&
      # exclude demultiplexing info columns
      !k %in% c("barcode_file", "barcode_seq")
  ) %>%
  dplyr::pull(k)

if (test_cols %>% length() == 0) {
  save.image(args$out_rdata)
  quit()
}

binary_cols_tbl <-
  tbl %>%
  dplyr::select(test_cols) %>%
  dplyr::select_if(is.factor) %>%
  dplyr::summarise_all(~ dplyr::n_distinct(., na.rm = TRUE)) %>%
  tidyr::gather(col, n)
binary_cols <- ifelse(
  "n" %in% colnames(binary_cols_tbl),
  binary_cols_tbl %>%
    dplyr::filter(n == 2) %>%
    dplyr::pull(col),
  character()
) %>% setdiff(NA)

# fators with at least 3 levels
many_factor_cols <-
  tbl %>%
  dplyr::select(test_cols) %>%
  dplyr::select_if(is.factor) %>%
  dplyr::select(-binary_cols) %>%
  purrr::map_if(purrr::is_empty, ~NULL, ~ {
    . %>%
      dplyr::summarise_all(~ dplyr::n_distinct(., na.rm = TRUE)) %>%
      tidyr::gather(col, n) %>%
      dplyr::filter(n > 2) %>%
      dplyr::pull(col)
  }) %>%
  names()

#' actual targets
tested_cols <- union(binary_cols, many_factor_cols)

#
# set up ML parameters
#

set.seed(42)
train_seeds <- tbl$sample_id %>%
  unique() %>%
  length() %>%
  seq() %>%
  lapply(function(x) {
    sample.int(20)
  })
sbf_seeds <- sample.int(7)

# General parameters
tuneLength <- 20
numFeatures <- 20
default_resampling <- list(method = "cv", number = 5)
default_allowParallel <- TRUE
default_binary_class_metric <- "ROC"
default_multi_class_metric <- "Accuracy"

# Training parameters
train_ctrl <- caret::trainControl(
  method = default_resampling$method,
  number = default_resampling$number,
  returnResamp = "all",
  savePredictions = "final",
  allowParallel = default_allowParallel,
  classProbs = TRUE
)

binary_train_ctrl <- train_ctrl
binary_train_ctrl$summaryFunction <- caret::twoClassSummary

multi_train_ctrl <- train_ctrl
multi_train_ctrl$summaryFunction <- caret::multiClassSummary

# SBF parameters (Caret selection by filter)
sbf_funcs <- caretSBF
sbf_funcs$fit <- function(...) caretSBF$fit(tuneLength = tuneLength, ...)
sbf_funcs$filter <- function(score, x, y) rank(score) <= numFeatures
sbf_ctrl <- caret::sbfControl(
  functions = sbf_funcs,
  method = default_resampling$method,
  number = default_resampling$number,
  returnResamp = "all",
  allowParallel = default_allowParallel,
  saveDetails = TRUE,
  verbose = TRUE
)

binary_sbf_ctrl <- sbf_ctrl
binary_sbf_ctrl$functions$fit <- function(...) sbf_ctrl$functions$fit(metric = default_binary_class_metric, ...)
binary_sbf_ctrl$functions$summary <- caret::twoClassSummary

multi_sbf_ctrl <- sbf_ctrl
multi_sbf_ctrl$functions$fit <- function(...) sbf_ctrl$functions$fit(metric = default_multi_class_metric, ...)
multi_sbf_ctrl$functions$summary <- caret::multiClassSummary

# RFE parameters (caret recursive feature elemination)
rfe_funcs <- caretFuncs
rfe_funcs$fit <- function(...) caretFuncs$fit(tuneLength = tuneLength, ...)
rfe_funcs$summary <- defaultSummary
rfe_funcs$selectSize <- function(x, metric = "ROC", tol = 5, maximize = TRUE) {
  # adapted from caret::pickSizeTolerance
  if (!maximize) {
    best <- min(x[, metric])
    perf <- (x[, metric] - best) / best * 100
    flag <- perf <= tol
  }
  else {
    best <- max(x[, metric])
    perf <- (best - x[, metric]) / best * 100
    flag <- perf <= tol
  }
  min(x[flag, "Variables"])
}
rfe_funcs$pred <- function(object, x) {
  # adapted from https://stackoverflow.com/questions/21649172/roc-in-rfe-in-caret-package-for-r
  tmp <- predict(object, x)
  if (object$modelType == "Classification" &
    !is.null(object$modelInfo$prob)) {
    out <- cbind(
      data.frame(pred = tmp),
      as.data.frame(predict(object, x, type = "prob"))
    )
  } else {
    out <- tmp
  }
  out
}
rfe_ctrl <- caret::rfeControl(
  functions = rfe_funcs,
  method = default_resampling$method,
  number = default_resampling$number,
  allowParallel = default_allowParallel,
  saveDetails = TRUE,
  returnResamp = "all"
)

binary_rfe_ctrl <- rfe_ctrl
binary_rfe_ctrl$functions$fit <- function(...) rfe_ctrl$functions$fit(metric = default_binary_class_metric, ...)
binary_rfe_ctrl$functions$summary <- caret::twoClassSummary
# binary_rfe_ctrl$functions$selectSize <- function (...) rfe_ctrl$functions$selectSize(metric = default_binary_class_metric, ...)

multi_rfe_ctrl <- rfe_ctrl
multi_rfe_ctrl$functions$fit <- function(...) rfe_ctrl$functions$fit(metric = default_multi_class_metric, ...)
multi_rfe_ctrl$functions$summary <- caret::multiClassSummary
# multi_rfe_ctrl$functions$selectSize <- function (...) rfe_ctrl$functions$selectSize(metric = default_multi_class_metric, ...)

do_feature_selection <- function(ml_tbl, fs_method, clf_method, ...) {
  is_binary_target_class <-
    ml_tbl$target %>%
    unique() %>%
    length() %>%
    magrittr::equals(2)

  ml_res <-
    switch(
      EXPR = paste0(is_binary_target_class, "|", fs_method),
      # binary classification
      "TRUE|train" = caret::train(target ~ ., data = ml_tbl, trControl = binary_train_ctrl, method = clf_method, ...),
      "TRUE|sbf" = caret::sbf(target ~ ., data = ml_tbl, trControl = binary_train_ctrl, sbfControl = binary_sbf_ctrl, method = clf_method, ...),
      "TRUE|rfe" = caret::rfe(target ~ ., data = ml_tbl, trControl = binary_train_ctrl, rfeControl = binary_rfe_ctrl, method = clf_method, ...),
      # multi factor classification
      "FALSE|train" = caret::train(target ~ ., data = ml_tbl, trControl = multi_train_ctrl, method = clf_method, ...),
      "FALSE|sbf" = caret::sbf(target ~ ., data = ml_tbl, trControl = multi_train_ctrl, sbfControl = multi_sbf_ctrl, method = clf_method, ...),
      "FALSE|rfe" = caret::rfe(target ~ ., data = ml_tbl, trControl = multi_train_ctrl, rfeControl = multi_rfe_ctrl, method = clf_method, ...),
      # default
      NULL
    )

  return(ml_res)
}

prep_ml_tbl <- function(ml_tbl, target_col) {
  ml_tbl %>%
    dplyr::select(target_col, features) %>%
    dplyr::rename(target = target_col) %>%
    dplyr::filter(!is.na(target)) %>%
    # factor levels must be R variable names
    dplyr::mutate(target = target %>% make.names() %>% as.factor())
}

# set up models
ml_in_tbl <-
  dplyr::bind_rows(
    tibble::tibble(target = binary_cols, type = "binary classification"),
    tibble::tibble(target = many_factor_cols, type = "many factor classification")
  ) %>%
  # expand grid of parameter space
  tidyr::crossing(
    clf_method = c("svmRadial", "rf"),
    fs_method = c("train", "sbf", "rfe")
  ) %>%
  dplyr::mutate(
    model_id = dplyr::row_number(),
    ml_tbl = purrr::map(target, ~ prep_ml_tbl(tbl, .x))
  )

# run models
ml_out_tbl <-
  ml_in_tbl %>%
  # Keep only target with more than one sample in all classes
  dplyr::mutate(n_samples_smallest_group = ml_tbl %>% purrr::map_int(~ {
    .x %>%
      dplyr::group_by(target) %>%
      dplyr::count() %>%
      dplyr::pull(n) %>%
      min()
  })) %>%
  dplyr::filter(n_samples_smallest_group >= args$min_samples_per_class) %>%
  dplyr::mutate(
    fs_obj = {
      purrr::pmap(list(ml_tbl, fs_method, clf_method), ~ do_feature_selection(..1, fs_method = ..2, clf_method = ..3))
    }
  )

#
# evaluate models
#

# model descriptions
get_final_model <- function(model) {
  switch(model %>% class() %>% first(),
    "train" = {
      model %>% purrr::pluck("finalModel")
    },
    model %>% purrr::pluck("fit", "finalModel")
  )
}

get_features_count <- function(model, ml_tbl) {
  tryCatch(
    expr = model %>% predictors() %>% length(),
    error = function(e) {
      ml_tbl %>%
        colnames() %>%
        setdiff("target") %>%
        length()
    }
  )
}

get_params <- function(model) {
  switch(
    EXPR = model %>% class() %>% first(),
    "randomForest" = {
      tibble(mtry = model$mtry, ntree = model$ntree)
    },
    # default
    model %>%
      pluck("param") %>%
      as_tibble()
  ) %>%
    gather(param_key, param_value)
}

params_tbl_to_string <- function(params_tbl) {
  paste(params_tbl$param_key, params_tbl$param_value %>% sprintf("%g", .), sep = "=", collapse = ", ")
}

get_results <- function(model) {
  model %>%
    pluck("results") %>%
    tibble::as_tibble() %>%
    # unify metric names from binary and multi classification results
    dplyr::rename_at(vars(contains("ROC")), function(x) stringr::str_replace(x, "ROC", "AUC")) %>%
    dplyr::rename_all(recode,
      "Mean_Sensitivity" = "Sens", "Mean_Specificity" = "Spec",
      "Mean_SensitivitySD" = "SensSD", "Mean_SpecificitySD" = "SpecSD"
    ) %>%
    # select only basic metrics, deselect columns like prAUC
    dplyr::select(matches("^AUC|Sens|Spec")) %>%
    # report only best tuned AUC
    dplyr::arrange(-AUC) %>%
    dplyr::slice(1)
}

# detailed view on models
ml_res_tbl <-
  ml_out_tbl %>%
  dplyr::mutate(model = purrr::map(fs_obj, get_final_model)) %>%
  dplyr::mutate(results = purrr::map(fs_obj, get_results)) %>%
  tidyr::unnest(results, .drop = FALSE) %>%
  dplyr::ungroup()

# tidy view on models
models_tbl <- tryCatch(
  {
    ml_res_tbl %>%
      dplyr::transmute(
        model_id = model_id,
        target = target,
        fs_method = fs_method,
        AUC = AUC,
        Sens = Sens,
        Spec = Spec,
        clf_method = clf_method,
        n_features = purrr::map2_int(model, ml_tbl, get_features_count),
        params = purrr::map_chr(model, ~ .x %>%
          get_params() %>%
          params_tbl_to_string())
      )
  },
  error = function(e) tibble()
)


# feature importance of best RF models
rf_importance_tbl <-
  ml_res_tbl %>%
  dplyr::filter(clf_method == "rf") %>%
  dplyr::group_by(target) %>%
  dplyr::arrange(-AUC) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  dplyr::transmute(
    target = target,
    model_id = model_id,
    feature_importance = purrr::map(model, ~ pluck(.x, "importance") %>% tibble::as_tibble(rownames = "feature"))
  ) %>%
  tidyr::unnest() %>%
  dplyr::group_by(target, model_id) %>%
  dplyr::filter(MeanDecreaseGini > 0) %>%
  dplyr::arrange(-MeanDecreaseGini)

save.image(args$out_rdata)
doParallel::stopImplicitCluster()
