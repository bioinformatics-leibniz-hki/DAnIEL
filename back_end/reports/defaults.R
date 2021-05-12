#!/usr/bin/env R

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

library(knitr)
library(tidyverse)
library(cowplot)
library(kableExtra)
library(formattable)
library(ggiraph)
library(ggraph)
library(tidygraph)
library(danielLib)

demux_meta_cols <- c("barcode_seq", "barcode_file")

knitr::opts_knit$set(
  root.dir = params$project_dir
)

knitr::opts_chunk$set(
  echo = FALSE,
  comment = NA,
  message = FALSE,
  error = TRUE, # prevent rmarkdown::render from aborting
  include = TRUE,
  out.width = "100%",
  dpi = 300,
  fig.align = "center"
)

options(
  knitr.kable.NA = " ",
  warn = -1
)

set.seed(1337)

knitr::knit_engines$set(rmd = function(options) {
  #
  # Knitr engine for RMarkdown code chunks insiude Rmarkdown documents
  # - like asis, but evaluates `r{...}``expressions
  # - Usecase: conditional evaluation of Rmarkdown code
  #
  if (options$eval) {
    code <- paste(options$code, collapse = "\n")
    expressions <-
      code %>%
      # extract rmarkdown R expressions
      stringr::str_extract_all("`r \\{[^}]*\\}`") %>%
      tibble::tibble(rmd_expr = .) %>%
      tidyr::unnest() %>%
      # evaluate expressions
      dplyr::rowwise() %>%
      dplyr::mutate(
        expr = rmd_expr %>% stringr::str_remove_all("`r \\{|\\}`"),
        eval = eval(parse(text = expr)) %>% as.character()
      )

    # replace expressions
    for (i in expressions %>%
      dim() %>%
      purrr::pluck(1) %>%
      seq(length.out = .)) {
      code <-
        gsub(expressions$rmd_expr[[i]], as.character(expressions$eval[[i]]), code, fixed = TRUE)
    }

    return(code)
  }
})

theme_my <-
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    axis.line.x = ggplot2::element_line(size = 0.8),
    axis.line.y = ggplot2::element_line(size = 0.8),
    axis.ticks = ggplot2::element_line(colour = "black", size = 0.8),
    axis.text = ggplot2::element_text(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major = ggplot2::element_blank(),
  )
ggplot2::theme_set(theme_my)

scale_fill_discrete <- function(...) ggplot2::scale_fill_viridis_d(...)
scale_fill_continuous <- function(...) ggplot2::scale_fill_viridis_c(...)
scale_color_discrete <- function(...) ggplot2::scale_color_viridis_d(...)
scale_color_continuous <- function(...) ggplot2::scale_color_viridis_c(...)
scale_colour_discrete <- function(...) ggplot2::scale_colour_viridis_d(...)
scale_colour_continuous <- function(...) ggplot2::scale_colour_viridis_c(...)

n_show_max_items <- 50