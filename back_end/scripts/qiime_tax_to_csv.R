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
require(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = "--qiime2-tax-tsv-path",
    type = "character",
    default = "/dev/stdin",
    help = "Path to input tsv taxonomy table file produced by QIIME2"
  ),
  optparse::make_option(
    opt_str = "--phylotyped-csv-path",
    type = "character",
    default = "/dev/stdout",
    help = "Path to output csv taxonomy table file, one column per taxrank"
  ),
  optparse::make_option(
    opt_str = "--replace-na-taxa",
    type = "logical",
    default = FALSE,
    action = "store_true",
    help = "Replace not assigned taxa by it's higher taxon name followed by sp."
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Qiime tsv taxonomy table to csv, one column per taxrank"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

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

replace_taxon <- function(taxon, upstream_taxon) {
  if(! is.na(taxon)) return(taxon)
  if(is.na(upstream_taxon)) stop("Both taxon and upstream taxon are NA")
  
  upstream_taxon %>% str_remove_all("( sp.)+") %>% paste0(" sp.")
}

tbl <-
  args$qiime2_tax_tsv_path %>%
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
  dplyr::select(-taxon)

if(args$replace_na_taxa) {
  tbl <-
    tbl %>%
    rowwise() %>%
    mutate(
      kingdom = kingdom %>% {.x <- .; .x %>% is.na() %>% ifelse("Fungi", .x)},
      phylum = phylum %>% replace_taxon(kingdom),
      class = class %>% replace_taxon(phylum),
      order = order %>% replace_taxon(`class`),
      family = family %>% replace_taxon(order),
      genus = genus %>% replace_taxon(family),
      species = species %>% replace_taxon(genus),
      strain = strain %>% replace_taxon(species)
    ) 
}

tbl %>% readr::write_csv(args$phylotyped_csv_path)
