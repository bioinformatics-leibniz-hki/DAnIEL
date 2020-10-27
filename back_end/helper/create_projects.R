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
# Create projects summary
#

library(tidyverse)
library(magrittr)
library(pbmclapply)
library(readODS)
library(xml2)
library(rentrez)

bioproject_meta_data <- function(bioproject_id) {
  Sys.sleep(0.3)

  project <-
    bioproject_id %>%
    sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=bioproject&retmode=xml&id=%s", .) %>%
    read_xml() %>%
    as_list()

  tibble(
    title = project$RecordSet$DocumentSummary$Project$ProjectDescr$Title %>% simplify(),
    description = project$RecordSet$DocumentSummary$Project$ProjectDescr$Description %>% simplify()
  )
}

# get projects
projects_query <- read_lines("query.txt") %>% paste(collapse = " ")
projects_search <- entrez_search("bioproject", projects_query, use_history = TRUE, retmax = 100e3 - 1)

entrez_fetch_cached <- function(xml_path, ...) {
  if (file.exists(xml_path)) {
    message("load from cache file")
    xml_path %>%
      read_file() %>%
      read_xml() %>%
      return()
  }

  doc <-
    entrez_fetch(...) %>%
    as_xml_document()

  write_xml(doc, xml_path)
  return(doc)
}

projects_xml <- entrez_fetch_cached(
  xml_path = "projects.xml",
  db = "bioproject",
  web_history = projects_search$web_history,
  rettype = "xml",
  retmax = 100e3 - 1
)

xml_find_all_as <- function(projects_xml, xpath, as.func = as.character) {
  projects_xml %>%
    xml_find_all(xpath) %>%
    xml2::xml_contents() %>%
    as.func()
}

xml_find_first_as <- function(projects_xml, xpath, as.func = as.character) {
  projects_xml %>%
    xml_find_first(xpath) %>%
    xml2::xml_contents() %>%
    as.func() %>%
    {
      ifelse(length(.) == 0, NA, .)
    }
}

projects_tbl <-
  tibble(
    bioproject_id = projects_xml %>% xml_find_all_as("//RecordSet/DocumentSummary/Project/ProjectID/ArchiveID/@accession"),
    bioproject_uid = projects_xml %>% xml_find_all_as("//RecordSet/DocumentSummary/@uid"),
    title = projects_xml %>% xml_find_all_as("//RecordSet/DocumentSummary/Project/ProjectDescr/Title"),
    description = projects_xml %>% xml_find_all_as("//RecordSet/DocumentSummary/Project/ProjectDescr/Description")
  )

entrez_fetch_sra <- function(bioproject_uid, .pb = NULL) {
  Sys.sleep(0.3)
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()

  query <- sprintf("\"%s\"[BioProject]", bioproject_uid)
  search <- entrez_search("sra", query, use_history = TRUE, retmax = 100e3 - 1)
  fetch <- entrez_fetch_cached(
    xml_path = paste0("xml/", bioproject_uid, ".xml"),
    db = "sra",
    web_history = search$web_history,
    rettype = "xml",
    retmax = 100e3 - 1
  )

  fetch %>% as_xml_document()
}

samples_xmls <-
  projects_tbl %>%
  pull(bioproject_uid) %>%
  unique() %>%
  rev() %>%
  map(entrez_fetch_sra, .pb = projects_tbl %>% dim() %>% pluck(1) %>% progress_estimated()) %>%
  map(xml_children) %>%
  flatten()

samples_tbl <-
  samples_xmls %>%
  head(100) %>%
  pbmcapply::pbmclapply(function(x) {
    tibble(
      run_id = x %>% xml_find_first_as("RUN_SET/RUN/@accession"),
      bioproject_id = x %>% xml_find_first_as("STUDY/IDENTIFIERS/EXTERNAL_ID"),
      biosample_id = x %>% xml_find_first_as("SAMPLE/IDENTIFIERS/EXTERNAL_ID"),
      title = x %>% xml_find_first_as("EXPERIMENT/TITLE"),
      library_name = x %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_NAME"),
      library_selection = x %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_SELECTION"),
      library_strategy = x %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_STRATEGY"),
      library_source = x %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_SOURCE"),
      library_layout = x %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_LAYOUT"),
      platform = x %>% xml_find_first("EXPERIMENT/PLATFORM/*") %>% xml_name() %>% str_remove_all("(<)|(/>)"),
      instrument_model = x %>% xml_find_first_as("EXPERIMENT/PLATFORM/*/INSTRUMENT_MODEL"),
      spots = x %>% xml_find_first_as("RUN_SET/RUN/@total_spots"),
      bases = x %>% xml_find_first_as("RUN_SET/RUN/@bases"),
      attributes = list(
        x %>%
          xml_find_all("SAMPLE/SAMPLE_ATTRIBUTES/SAMPLE_ATTRIBUTE") %>%
          map(~ tibble(attribute_key = xml_find_all_as(.x, "TAG"), attribute_val = xml_find_all_as(.x, "VALUE"))) %>%
          when(!is_empty(.) ~ bind_rows(.), tibble())
      )
    )
  }) %>%
  reduce(bind_rows)



#
# entrez link istead of search for [Bioproject]
# - not used because can not create web history object
#
# entrez_link_project <- function(bioproject_uid, ...) {
#   Sys.sleep(0.2)
#
#   bioproject_uid %>%
#     entrez_link(dbfrom = "bioproject", db = "sra", id = ., retmax = 100e3-1, ...) %>%
#     pluck("links", "bioproject_sra")
# }
# # associate SRA runs
# samples_link_tbl <-
#   projects_tbl %>%
#   select(bioproject_uid) %>%
#   distinct() %>%
#   mutate(sra_uids = bioproject_uid %>% map(entrez_link_project)) %>%
#   filter(sra_uids %>% map_int(length) > 0)
#
# samples_fetch_tbl <-
#   samples_link_tbl %>%
#   mutate(sra_xml = sra_uids %>% map(~ entrez_fetch(db = "sra", id = .x, rettype = "xml") %>% read_xml()))
#
# samples_tbl <-
#   # gather all samples
#   samples_fetch_tbl %>%
#   pluck("sra_xml") %>%
#   map(xml_children) %>%
#   flatten() %>%
#   tibble(xml_node = .) %>%
#   rowwise() %>%
#   # extract sample properties
#   mutate(
#     run_id = xml_node %>% xml_find_first_as("RUN_SET/RUN/@accession"),
#     bioproject_id = xml_node %>% xml_find_first_as("STUDY/IDENTIFIERS/EXTERNAL_ID"),
#     biosample_id = xml_node %>% xml_find_first_as("SAMPLE/IDENTIFIERS/EXTERNAL_ID"),
#     title =  xml_node %>% xml_find_first_as("EXPERIMENT/TITLE"),
#     library_name = xml_node %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_NAME"),
#     library_selection = xml_node %>% xml_find_first_as("EXPERIMENT/DESIGN/LIBRARY_DESCRIPTOR/LIBRARY_SELECTION"),
#     spots = xml_node %>% xml_find_first_as("RUN_SET/RUN/@total_spots")
#   )
