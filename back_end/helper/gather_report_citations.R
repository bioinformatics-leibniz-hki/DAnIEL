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
# Gathers all citation in rmarkdown documents used by [@pmid123456]
#

rmd_dir <- "/sbidata/server/daniel/src/dev/back_end/reports/"

list.files(rmd_dir, recursive = TRUE, pattern = "\\.Rmd$", full.names = TRUE) %>%
  map(
    ~ .x %>%
      read_file() %>%
      str_extract_all("pmid[0-9]+") %>%
      flatten_chr() %>%
      str_remove_all("pmid")
  ) %>%
  flatten_chr() %>%
  unique() %>%
  write_lines("citations.txt")
