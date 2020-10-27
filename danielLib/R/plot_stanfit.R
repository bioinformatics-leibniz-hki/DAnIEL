# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.


#' Create traceplot of stan fits
#' @param fits list of objects of class stanfit. Names of this list are used to name the models
#' @param param_regex string containing regular expression to filter stan parameters
#' @param param_count first parameters of stan models to plot
plot_stanfit <- function(fits, param_regex = "^O", param_count = 10) {
  pars <-
    fits %>%
    purrr::map(names) %>%
    purrr::simplify() %>%
    purrr::keep(~ str_detect(.x, param_regex)) %>%
    head(param_count)

  warmup <-
    fits %>%
    first() %>%
    attributes() %>%
    purrr::pluck("stan_args", 1, "warmup")

  data <-
    fits %>%
    purrr::map(
      ~ .x %>%
        rstan::traceplot(pars = pars, inc_warmup = TRUE) %>%
        purrr::pluck("data") %>%
        tibble::as_tibble()
    ) %>%
    tibble::enframe() %>%
    tidyr::unnest(cols = c(value)) %>%
    dplyr::filter(parameter %>% stringr::str_detect(param_regex)) %>%
    # normalization
    dplyr::group_by(name, parameter) %>%
    dplyr::mutate(value = value %>% scale())

  data %>%
    ggplot2::ggplot(aes(iteration, value, color = chain)) +
    ggplot2::geom_vline(xintercept = warmup, color = "black") +
    ggplot2::geom_smooth(method = "loess", size = 0.3, se = FALSE) +
    ggplot2::facet_grid(parameter ~ name, scales = "free") +
    ggplot2::labs(
      x = "Iteration",
      y = "Value (z-scaled)",
      color = "Chain"
    )
}
