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
# Combining results from PIPITS
#

library(tidyverse)
library(Biostrings)

dir <- base::commandArgs(trailingOnly = TRUE)[1]

denoised_tbl <-
  dir %>%
  paste0("/pipits/3-process/otu_table.txt") %>%
  readr::read_tsv(skip = 1) %>%
  dplyr::rename(denoised = `#OTU ID`) %>%
  tidyr::gather(sample_id, abundance, -denoised) %>%
  dplyr::mutate(abundance = abundance %>% as.numeric()) %>%
  dplyr::filter(!sample_id == "taxonomy")

denoised_tbl %>%
  tidyr::spread(denoised, abundance) %>%
  readr::write_csv(paste0(dir, "/raw_denoised.csv"))

read_counts_tbl <-
  list(
    prepped = "1-seqprep/prepped.fasta",
    its = "2-funits/ITS.fasta"
  ) %>%
  purrr::map(~ paste0(dir, "/pipits/", .x)) %>%
  purrr::map(Biostrings::readDNAStringSet) %>%
  # count sequences per sample using FASTA header
  purrr::map(names) %>%
  purrr::map(~ .x %>%
    str_remove("\\_[0-9]+$") %>%
    table()) %>%
  purrr::map2(names(.), ~ .x %>%
    tibble::as_tibble() %>%
    dplyr::mutate(step = .y)) %>%
  purrr::reduce(bind_rows) %>%
  magrittr::set_colnames(c("sample_id", "n", "step")) %>%
  tidyr::spread(step, n) %>%
  # add final denoised counts
  dplyr::full_join(
    denoised_tbl %>% dplyr::group_by(sample_id) %>% dplyr::summarize(final = sum(abundance))
  ) %>%
  dplyr::select(sample_id, prepped, its, final) %>%
  dplyr::filter(!sample_id == "taxonomy")

read_counts_tbl %>%
  readr::write_csv(paste0(dir, "/read_counts.csv"))
