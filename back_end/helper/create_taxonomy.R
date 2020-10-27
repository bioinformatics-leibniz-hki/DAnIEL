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
# Create taxonomy with names and DB IDs from OTT
# see: https://tree.opentreeoflife.org/about/taxonomy-version/ott3.0
#

library(tidyverse)

args <- commandArgs(trailingOnly = TRUE)

tbl <-
  read_delim(args[1], delim = "|") %>%
  mutate_all(str_trim) %>%
  select(
    ott_id = `uid\t`,
    parent_ott_id = `\tparent_uid\t`,
    taxon = `\tname\t`,
    rank = `\trank\t`,
    source = `\tsourceinfo\t`
  ) %>%
  mutate(
    ncbi_id = str_extract(source, "ncbi:[0-9]+") %>% str_remove("ncbi:"),
    silva_id = str_extract(source, "silva:[A-Z][0-9]+") %>% str_remove("silva:"),
    worms_id = str_extract(source, "worms:[0-9]+") %>% str_remove("worms:"),
    gbif_id = str_extract(source, "gbif:[0-9]+") %>% str_remove("gbif:"),
    irmng_id = str_extract(source, "irmng:[0-9]+") %>% str_remove("irmng:"),
    is_leaf = grepl("- terminal$", rank),
    rank = str_remove(rank, " - terminal$"),
  )

tbl <-
  tbl %>%
  select(tax_id = ott_id, taxon, rank) %>%
  filter(rank %in% c("kindom", "phylum", "order", "class", "family", "genus", "species")) %>%
  mutate(rank = as.factor(rank)) %>%
  readr::write_csv(args[2])
