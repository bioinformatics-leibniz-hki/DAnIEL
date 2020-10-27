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

require(tools)
require(tidyverse)
require(optparse)
require(BiocParallel)

args <- base::list(
  optparse::make_option(
    opt_str = c("-d", "--directory"),
    type = "character",
    default = "/dev/stdin",
    help = "Directory containing files to calcualte checksum"
  ),
  optparse::make_option(
    opt_str = c("-o", "--out-csv"),
    type = "character",
    default = "/dev/stdout",
    help = "Output table containing basenames and md5 checksums of files (CSV)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Create md5 checksums of files"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(directory = "input/reads", out_csv = "/dev/stdout")

args$directory %>%
  base::list.files(full.names = TRUE) %>%
  base::grep(pattern = "\\.csv$", invert = TRUE, value = TRUE) %>%
  BiocParallel::bplapply(
    BPPARAM = BiocParallel::MulticoreParam(progressbar = TRUE, workers = 5),
    FUN = tools::md5sum
  ) %>%
  purrr::simplify() %>%
  tibble::enframe(name = "file", value = "md5sum") %>%
  dplyr::mutate(file = base::basename(file)) %>%
  readr::write_csv(args$out_csv)
