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
# Combining results from samplewise DADA2 runs
#

library(Biostrings)
library(tidyverse)
library(magrittr)
library(scales)
library(svglite)

dir <- base::commandArgs(trailingOnly = TRUE)[1]

denoised_fasta_paths <-
  base::list.files(dir, recursive = TRUE, full.names = TRUE) %>%
  base::grep("denoised.fasta$", ., value = TRUE) %>%
  # failed samples may create an empty fasta file. Discard these
  purrr::discard(~ file.size(.) == 0) %>%
  # remove summary if already exsisted
  purrr::discard(~ .x %>% str_detect("//[A-z_]*denoised.fasta"))

sample_asv_seqs <-
  denoised_fasta_paths %>%
  base::lapply(Biostrings::readDNAStringSet)

sample_asv_counts_tbl <-
  denoised_fasta_paths %>%
  stringr::str_replace("denoised.fasta$", "denoised.csv") %>%
  base::lapply(function(x) {
    readr::read_csv(x, col_types = cols(.default = "c")) %>%
      tidyr::gather(sample_asv, abundance, -sample_id)
  }) %>%
  dplyr::bind_rows() %>%
  readr::type_convert()

asv_tbl <-
  sample_asv_seqs %>%
  lapply(function(x) {
    table(x) %>%
      tibble::as_tibble() %>%
      magrittr::set_colnames(c("seq", "n")) %>%
      dplyr::mutate(
        sample_asv = base::names(x),
        sample_id = base::names(x[1]) %>% stringr::str_remove("ASV[0-9]+_")
      ) %>%
      # ASV is uniqe to a sample by definition. No need to keep counts
      dplyr::select(-n)
  }) %>%
  dplyr::bind_rows() %>%
  readr::type_convert() %>%
  dplyr::full_join(sample_asv_counts_tbl, by = c("sample_asv", "sample_id")) %>%
  dplyr::full_join(
    tibble(seq = .$seq %>% base::unique()) %>% dplyr::mutate(asv = base::paste0("ASV", dplyr::row_number())),
    by = "seq"
  )

# denoised fasta sequences
asv_seqs <- asv_tbl$seq %>% base::unique()
names(asv_seqs) <- asv_tbl$asv %>% base::unique()
asv_seqs %>%
  Biostrings::DNAStringSet() %>%
  Biostrings::writeXStringSet(base::paste0(dir, "/raw_denoised.fasta"))

# denoised abundance profile
asv_profile_tbl <-
  asv_tbl %>%
  dplyr::group_by(asv, sample_id) %>%
  dplyr::summarise(abundance = sum(abundance)) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(asv = factor(asv, levels = names(asv_seqs))) %>%
  dplyr::arrange(asv) %>%
  tidyr::spread(asv, abundance, fill = 0) %T>%
  readr::write_csv(paste0(dir, "/raw_denoised.csv"))

#' @param x list with elements fwd_dada, rev_dada and file
get_error_tbl <- function(x, mate = "fwd") {
  switch(mate,
    "fwd" = {
      x$fwd_dada$err_out
    },
    "rev" = {
      x$rev_dada$err_out
    }
  ) %>%
    tibble::as_tibble(rownames = "transition") %>%
    tidyr::gather(consensus_qual_score, err_freq, -transition) %>%
    dplyr::mutate(
      sample_id = {
        x$file %>%
          stringr::str_split("/") %>%
          purrr::pluck(1) %>%
          utils::tail(2) %>%
          utils::head(1)
      },
      mate = mate,
      transition = transition %>% stringr::str_replace("2", ">") %>% base::as.factor(),
      consensus_qual_score = base::as.integer(consensus_qual_score),
    )
}

# summary of error models
dada2_res_l <-
  base::list.files(dir, pattern = "dada2_res.RDS$", recursive = TRUE, full.names = TRUE) %>%
  # remove summary if already exsisted
  base::setdiff(base::paste0(dir, "/dada2_res.RDS$")) %>%
  base::lapply(function(x) {
    res <- readr::read_rds(x)
    res$file <- x
    res
  })

dada2_err_tbl <-
  dada2_res_l %>%
  tibble::enframe() %>%
  tidyr::expand_grid(mate = c("fwd", "rev")) %>%
  dplyr::mutate(error_tbl = value %>% map2(mate, purrr::possibly(get_error_tbl, NA))) %>%
  dplyr::filter(!error_tbl %>% is.na()) %>%
  dplyr::pull(error_tbl) %>%
  dplyr::bind_rows()

c("asv_profile_tbl", "dada2_res_l", "dada2_err_tbl") %>%
  sapply(get) %>%
  readr::write_rds(base::paste0(dir, "/denoised.rds"))

# merge sample wise read counts
base::list.files(dir, recursive = TRUE, full.names = TRUE) %>%
  base::grep("read_counts.csv$", ., value = TRUE) %>%
  # remove summary if already exsisted
  base::setdiff(paste0(dir, "/read_counts.csv")) %>%
  purrr::map(~ readr::read_csv(.x, col_types = cols(.default = "c"))) %>%
  dplyr::bind_rows() %>%
  readr::write_csv(paste0(dir, "/read_counts.csv"))