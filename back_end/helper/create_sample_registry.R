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
# Summarizes all projects generated by create_project.R
#

library(tidyverse)

projects_dir <- "/sbidata/server/daniel/db/projects/"

csv_files <-
  list.files(projects_dir, recursive = TRUE, pattern = "\\.csv$", full.names = TRUE) %>%
  grep("db\\.csv$", ., value = TRUE, invert = TRUE)

# gather meta data files generated by grabseqs sra
sra_meta_tbl <-
  csv_files %>%
  grep("\\/meta\\.csv$", ., value = TRUE) %>%
  lapply(read_csv) %>%
  purrr::reduce(bind_rows) %>%
  dplyr::rename(sample_id = Run)

biosamples_meta_tbl <-
  csv_files %>%
  grep("\\/meta\\.csv$", ., value = TRUE, invert = TRUE) %>%
  lapply(function(x) {
    project <-
      x %>%
      str_split("/") %>%
      simplify() %>%
      tail(3) %>%
      dplyr::first()

    read_csv(x, col_types = cols(.default = "c")) %>%
      add_column(project = project, .after = 1)
  }) %>%
  purrr::reduce(bind_rows)

biosamples_meta_tbl %>%
  inner_join(sra_meta_tbl, by = "sample_id") %>%
  # make sample_id unique
  group_by(sample_id) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  rename_all(~ str_replace_all(.x, " ", "_")) %>%
  write_csv(paste0(projects_dir, "/samples.csv"))
