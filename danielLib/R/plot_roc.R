# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Plots ROC over resamples
#' @param model feature selection object (e.g. caret sbf, rfe. train works as well)
#' @param folds show ROC of all resamples as well
plot_roc <- function(fs, folds = FALSE) {
  # objects of train class have different locations
  # of prediction table and observatipon levels
  switch(
    EXPR = fs %>% class() %>% pluck(1),
    "sbf" = {
      obs_levels <<- fs %>% pluck("obsLevels")
      pred_tbl <<- fs$pred$predictions
    },
    "rfe" = {
      obs_levels <<- fs %>% pluck("fit", "levels")
      pred_tbl <<- fs$pred
    },
    "train" = {
      obs_levels <<- fs %>% pluck("levels")
      pred_tbl <<- fs$pred
    }
  )

  pred_tbl <-
    pred_tbl %>%
    dplyr::mutate(
      obs_int = obs %>% base::as.integer() %>% magrittr::subtract(1),
      pred_int = pred %>% base::as.integer() %>% magrittr::subtract(1)
    ) %>%
    dplyr::rename(pred_dbl = obs_levels[1]) %>%
    tibble::as_tibble()

  roc <- pROC::roc(pred_tbl, response = "obs_int", predictor = "pred_dbl", levels = c(0, 1))

  # reverse predictor outcomes if values for controls > cases
  if (roc$direction == ">") {
    pred_tbl <-
      pred_tbl %>%
      dplyr::mutate(
        pred_dbl = 1 - pred_dbl,
        pred_int = 1 - pred_int
      )
  }

  res_plt <- NULL
  if (folds) {
    res_plt <-
      pred_tbl %>%
      # add pooled data
      bind_rows(pred_tbl %>% mutate(Resample = "pooled")) %>%
      ggplot2::ggplot(aes(d = obs_int, m = pred_dbl, color = Resample))
  } else {
    res_plt <-
      pred_tbl %>%
      ggplot2::ggplot(aes(d = obs_int, m = pred_dbl))
  }

  res_plt <-
    res_plt +
    plotROC::geom_roc(n.cuts = 0) +
    ggplot2::geom_abline(intercept = 0) +
    ggplot2::annotate(
      "text",
      x = 1, y = 0, hjust = 1, size = 8,
      label = roc$auc %>% round(2) %>% magrittr::multiply_by(100) %>% sprintf(fmt = "AUC=%.1f%%")
    ) +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      x = "False positive rate",
      y = "True positive rate"
    )

  res_plt
}
