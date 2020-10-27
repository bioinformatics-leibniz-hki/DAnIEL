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
# Converts taxonomy.tsv as produced by qiime tools export to unified csv format as used in the pipeline
#

require(tidyverse)

args <- base::commandArgs(trailingOnly = TRUE)
qiime2_tax_tsv_path <- base::ifelse(is.na(args[1]), "/dev/stdin", args[1])
phylotyped_csv_path <- base::ifelse(is.na(args[2]), "/dev/stdout", args[2])

extract_rank <- function(lineage_str, rank) {
  if (rank == "t") {
    lineage_str %>%
      stringr::str_extract("t__.*$") %>%
      stringr::str_remove("t__") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_trim()
  } else {
    lineage_str %>%
      stringr::str_extract(sprintf("%s__[^;]+(;|$)", rank)) %>%
      stringr::str_remove("s__Fungi_sp.") %>%
      stringr::str_remove(sprintf("%s__", rank)) %>%
      stringr::str_remove(";$") %>%
      stringr::str_replace_all("_", " ") %>%
      stringr::str_trim()
  }
}

qiime2_tax_tsv_path %>%
  readr::read_tsv() %>%
  dplyr::rename(
    sequence = `Feature ID`,
    taxon = Taxon,
    confidence = Confidence
  ) %>%
  dplyr::mutate(
    strain = extract_rank(taxon, "t"),
    species = extract_rank(taxon, "s"),
    genus = extract_rank(taxon, "g"),
    family = extract_rank(taxon, "f"),
    order = extract_rank(taxon, "o"),
    class = extract_rank(taxon, "c"),
    phylum = extract_rank(taxon, "p"),
    kingdom = extract_rank(taxon, "k")
  ) %>%
  dplyr::select(-taxon) %>%
  readr::write_csv(phylotyped_csv_path)
