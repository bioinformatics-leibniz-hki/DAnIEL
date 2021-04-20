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

require(tidyverse)
require(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = "--n-seqs",
    type = "integer",
    default = 50
  ),
  optparse::make_option(
    opt_str = "--in-denoised-csv",
    type = "character"
  ),
  optparse::make_option(
    opt_str = "--in-fasta",
    type = "character"
  ),
  optparse::make_option(
    opt_str = "--out-fasta",
    type = "character",
    default = "/dev/stdout"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Filters fasta MSA to prevalent repseqs"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)
  
denoised_tbl <-
  args$in_denoised_csv %>%
  readr::read_csv() %>%
  possibly(tidyr::gather, NULL)(denoised, abundance, -sample_id)

repseqs <-
  denoised_tbl %>%
  dplyr::group_by(denoised) %>%
  dplyr::filter(abundance > 0) %>%
  dplyr::count() %>%
  dplyr::arrange(-n) %>%
  head(args$n_seqs) %>%
  dplyr::pull(denoised)

# Do not need dependencies e.g. BioStrings
args$in_fasta %>%
  read_file() %>%
  head() %>%
  read_delim(delim = "\n>", col_names = "fasta") %>%
  mutate(
    type = c("header", "seq") %>% rep(28714/2),
    seq_id = (row_number() / 2) %>% ceiling()
  ) %>%
  pivot_wider(names_from = type, values_from = fasta) %>%
  mutate(denoised_id = header %>% str_extract("(ASV|OTU)[0-9]+")) %>%
  filter(denoised_id %in% repseqs) %>%
  transmute(
    fasta = header %>% map2_chr(seq, ~ paste0(.x, "\n", .y))
  ) %>%
  pull(fasta) %>%
  write_lines(args$out_fasta)
