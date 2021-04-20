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
  tbl %>%
    magrittr::set_rownames(.$sample_id) %>%
    dplyr::select(-sample_id) %>%
    phyloseq::otu_table(taxa_are_rows = FALSE) %>%
    phyloseq::estimate_richness(measures = measures) %>%
    tibble::as_tibble(rownames = "sample_id") %>%
    dplyr::select(-se.chao1) %>%
    dplyr::mutate(sample_id = tbl$sample_id)
}
