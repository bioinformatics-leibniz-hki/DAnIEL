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

library(tidyverse)
library(jsonlite)

dir <- "/sbidata/server/daniel/db/interactions/fun_interaction_db_1/"

get_label_mesh <- function(mesh_id) {
  Sys.sleep(0.1)

  # do REST query
  mesh_id %>%
    paste0("https://id.nlm.nih.gov/mesh/", ., ".json") %>%
    read_json() %>%
    pluck("label", "@value")
}

interactions_tbl <-
  paste0(dir, "/interactions.csv") %>%
  read_csv()

interactions_mesh_ids <- interactions_tbl$partner %>%
  unique() %>%
  setdiff(NA)

mesh_tbl <-
  interactions_mesh_ids %>%
  tibble(mesh_id = .) %>%
  rowwise() %>%
  mutate(term = mesh_id %>% get_label_mesh())
