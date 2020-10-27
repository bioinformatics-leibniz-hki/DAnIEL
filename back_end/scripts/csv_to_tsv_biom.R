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
# csv to biom tsv
#

require(optparse)
require(tidyverse)

args <- base::list(
  optparse::make_option(
    c("-i", "--in-csv-file"),
    type = "character",
    default = "/dev/stdin",
    help = "CSV of count table. one row per sample."
  ),
  optparse::make_option(
    c("-o", "--out-tsv-file"),
    type = "character",
    default = "/dev/stdout",
    help = "BIOM TSV of count table. one column per sample."
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Converting CSV count table with one row per sample to BIOM TSV format"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(in_csv_file = "features/Species_low_prev/features.raw.csv", out_tsv_file = "/dev/stdout")

args$in_csv_file %>%
  readr::read_csv() %>%
  tidyr::gather(`#OTU ID`, abundance, -sample_id) %>%
  tidyr::spread(sample_id, abundance) %>%
  readr::write_tsv(args$out_tsv_file)
