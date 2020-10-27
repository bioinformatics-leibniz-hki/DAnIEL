# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Get selected parameters for all steps based on project$selected_analysis_params
#' @param  project list of DAnIEL file project.json
get_selected_params <- function(project) {
  get_selected_params_name <- function(cur_step, next_step, selected_params) {
    project$params[[paste0(next_step, "_params")]][[selected_params[[next_step]]]][[paste0("selected_", cur_step, "_params")]]
  }

  selected_params <- list()
  selected_params$analysis <- project$selected_analysis_params
  selected_params$features <- get_selected_params_name("features", "analysis", selected_params)
  selected_params$phylotyping <- get_selected_params_name("phylotyping", "features", selected_params)
  selected_params$denoising <- get_selected_params_name("denoising", "phylotyping", selected_params)
  selected_params$qc <- get_selected_params_name("qc", "denoising", selected_params)

  return(selected_params)
}

significance_label <- function(p.value, include_value = FALSE) {
  if (include_value) {
    p.formatted <- p.value %>% format(., digits = 2, scientific = TRUE)
    dplyr::case_when(
      p.value <= 0.001 ~ paste0(p.formatted, " \u2605\u2605\u2605"),
      p.value <= 0.01 ~ paste0(p.formatted, " \u2605\u2605"),
      p.value <= 0.05 ~ paste0(p.formatted, " \u2605"),
      TRUE ~ paste0(p.formatted, "")
    )
  } else {
    dplyr::case_when(
      p.value <= 0.001 ~ "\u2605\u2605\u2605",
      p.value <= 0.01 ~ "\u2605\u2605",
      p.value <= 0.05 ~ "\u2605",
      TRUE ~ ""
    )
  }
}
