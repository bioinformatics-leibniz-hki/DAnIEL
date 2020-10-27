# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Calculates all alpha diversity metrics
#' @param tbl abundance tibble with column sample_id. Denoised raw counts preferred.
#' @return alpha diversity tibble with columns sample_id, shannon, simpson, invsimpson
get_alpha_diversity_all <- function(tbl, measures = c("Chao1", "Shannon", "InvSimpson"), ...) {
  res <-
    tbl %>%
    set_rownames(.$sample_id) %>%
    select(-sample_id) %>%
    phyloseq::otu_table(taxa_are_rows = FALSE) %>%
    phyloseq::estimate_richness(measures = measures) %>%
    as_tibble(rownames = "sample_id") %>%
    select(-se.chao1)

  # phyloseq::estimate_richness reformats sample_id
  # e.g. with prefix X if sample_id is numeric
  # Undo this behavior
  if (tbl$sample_id %>% str_detect("^[0-9]") %>% all()) {
    res <- res %>% mutate_at("sample_id", ~ stringr::str_remove(.x, "^X"))
  }

  res
}
