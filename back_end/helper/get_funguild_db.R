#! /usr/bin/env R

#
# Get FUNGuild data base
#

library(tidyverse)
library(jsonlite)

get_taxranks <- function(current_rank, direction = "downstream") {
  taxranks <- c(
    "phylum" = 7, "class" = 6, "order" = 5, "family" = 4,
    "genus" = 3, "species" = 2
  )

  switch(direction,
    "with_downstream" = c(
      current_rank, taxranks[taxranks < taxranks[current_rank]] %>% names()
    ),
    "downstream" = taxranks[taxranks < taxranks[current_rank]] %>% names(),
    "with_upstream" = c(
      current_rank,
      taxranks[taxranks > taxranks[current_rank]] %>% names()
    ),
    "upstream" = taxranks[taxranks > taxranks[current_rank]] %>% names(),
    stop("direction unknown")
  )
}

extract_species_genus_names <- function(data) {
  data %>%
    mutate(
      species = case_when(
        !is.na(species) ~ species,
        # annotation is at least as species level
        taxonomicLevel >= 20 ~ taxon %>% str_extract("^[A-z]+ [A-z]+"),
        TRUE ~ ""
      ) %>% na_if(""),
      genus = case_when(
        !is.na(genus) ~ genus,
        # annotation is at least as genus level
        taxonomicLevel >= 13 ~ taxon %>% str_extract("^[A-z]+"),
        TRUE ~ ""
      ) %>% na_if(""),
    ) 
}

left_join_tax_rank <- function(this_tbl, lineages_tbl = lineages_tbl, rank = "species") {
  other_tbl <-
    lineages_tbl %>%
    distinct_at(get_taxranks(rank, direction = "with_upstream"))

  this_tbl %>%
    mutate(!!rank := ifelse(taxon %in% other_tbl[[rank]], taxon, NA)) %>%
    left_join(other_tbl)
}

get_lineage <- function(taxon, rank) {
  lineages_tbl %>% filter_at(rank, ~ .x == taxon) %>% distinct_at(get_taxranks(rank, direction = "with_upstream"))
}

get_lineages <- function(taxon) {
  # case_when does not work: Error during wrapup: must be a list, not a character vector.
  if(taxon %in% lineages_tbl$species) return(get_lineage(taxon, "species"))
  if(taxon %in% lineages_tbl$genus) return(get_lineage(taxon, "genus"))
  if(taxon %in% lineages_tbl$family) return(get_lineage(taxon, "family"))
  if(taxon %in% lineages_tbl$order) return(get_lineage(taxon, "order"))
  if(taxon %in% lineages_tbl$class) return(get_lineage(taxon, "class"))
  if(taxon %in% lineages_tbl$phylum) return(get_lineage(taxon, "phylum"))
}

# adopted from https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url
# added suffix [A-z0-9]+ to exclude strings ending with e.g. )
url_regex <- paste0(
  "(?!mailto:)(?:(?:http|https|ftp)://)(?:\\S+(?::\\S*)?@)?(?:(?:(?:[1-9]\\d?|1\\d\\d|2[01]\\d|22[0-3])",
  "(?:\\.(?:1?\\d{1,2}|2[0-4]\\d|25[0-5])){2}(?:\\.(?:[0-9]\\d?|1\\d\\d|2[0-4]\\d|25[0-4]))|(?:(?:[a-z\\u00a1-\\uffff0-9]+-?)*",
  "[a-z\\u00a1-\\uffff0-9]+)(?:\\.(?:[a-z\\u00a1-\\uffff0-9]+-?)*[a-z\\u00a1-\\uffff0-9]+)*(?:\\.(?:[a-z\\u00a1-\\uffff]{2,})))|localhost)",
  "(?::\\d{2,5})?(?:(/|\\?|#)[^\\s]*)?[A-z0-9]+"
)

lineages_tbl <-
  "/sbidata/server/daniel/latest/db/phylogenies/index_fungorum_2016/taxonomy.csv.gz" %>%
  read_csv()

funguild_db <-
  "http://www.stbates.org/funguild_db_2.php" %>%
  read_lines() %>%
  enframe() %>%
  # remove tags
  filter(!value %>% str_starts("<")) %>%
  # extract the one line with the content
  pull(value) %>%
  first() %>%
  str_remove("</body>$") %>%
  parse_json() %>%
  # list to table
  enframe() %>%
  unnest_wider(value) %>%
  rename(idx_fungorum_uuid = `guid`) %>%
  mutate_all(partial(na_if, y = "NULL")) %>%
  type_convert() %>%
  mutate(lineages = taxon %>% map(get_lineages)) %>%
  unnest(lineages) %>%
  # get species and genus names if they aren't in the lineage table
  extract_species_genus_names() %>%
  mutate(url = citationSource %>% str_extract(url_regex))
