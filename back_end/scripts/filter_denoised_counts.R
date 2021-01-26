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
# Filter raw denoised counts
#

library(tidyverse)

args <- base::commandArgs(trailingOnly = TRUE)
raw_denoised_csv <- args[1]
denoised_fasta <- args[2]
denoised_csv <- args[3]

filtered_asvs <-
  denoised_fasta %>%
  readr::read_lines() %>%
  purrr::keep(~ .x %>% stringr::str_detect("^>")) %>%
  purrr::map_chr(~ .x %>% stringr::str_remove("^>"))

if(length(filtered_asvs) == 0) {
  stringr::str_glue("touch {denoised_csv}") %>% system()
  quit()
}

denoised_tbl <-
  raw_denoised_csv %>%
  readr::read_csv() %>%
  dplyr::select(sample_id, filtered_asvs) %>%
  tidyr::pivot_longer(-sample_id, names_to = "asv", values_to = "abundance") %>%
  dplyr::group_by(sample_id) %>%
  dplyr::filter(sum(abundance) > 0)

denoised_tbl %>%
  tidyr::pivot_wider(names_from = asv, values_from = abundance, values_fill = list(abundance = 0)) %>%
  readr::write_csv(denoised_csv)
