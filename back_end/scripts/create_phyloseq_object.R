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
# Creates phyloseq objet from otu table, sample meta data and phylogenetic tree
#

require(tidyverse)
require(magrittr)
require(phyloseq)
require(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = "--features-csv",
    type = "character",
    help = "Feature abundance table (Raw counts)"
  ),
  optparse::make_option(
    opt_str = "--taxonomic-rank",
    type = "character",
    default = "species",
    help = "Taxonomic rank of feature abudnance table"
  ),
  optparse::make_option(
    opt_str = "--samples-csv",
    type = "character",
    help = "Samples meta data CSV table"
  ),
  optparse::make_option(
    opt_str = "--taxonomy-csv",
    type = "character",
    help = "Lineage CSV table contaning tax ID and taxon names for each rank"
  ),
  optparse::make_option(
    opt_str = "--tree-nwk",
    type = "character",
    help = "Phylogenetic newick tree"
  ),
  optparse::make_option(
    opt_str = "--out-rds",
    type = "character",
    help = "Phloseq RDS object"
  ),
  optparse::make_option(
    opt_str = "--ignore-phylogeny",
    type = "logical",
    default = FALSE,
    help = "Does not save phylogeny information in phyloseq object to keep features not part of the phylogeny"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Creates phyloseq objet from otu table, sample meta data and phylogenetic tree"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#   features_csv = "~/prj/6-its-web-server/userdat/poster/features/Genus_low_prev/features.raw.csv",
#   taxonomic_rank = "genus",
#   samples_csv = "~/prj/6-its-web-server/userdat/poster/input/samples.csv",
#   taxonomy_csv = "~/prj/6-its-web-server/db/phylogenies/index_fungorum_2016/taxonomy.csv.gz",
#   tree_nwk = "~/prj/6-its-web-server/db/phylogenies/index_fungorum_2016/genus.newick",
#   out_rds = "~/prj/6-its-web-server/userdat/poster/features/Genus_low_prev//phyloseq.rds"
# )

taxonomic_ranks <- c("species" = 1, "genus" = 2, "family" = 3, "order" = 4, "class" = 5, "phylum" = 6)
used_taxonomic_ranks <- taxonomic_ranks[taxonomic_ranks >= taxonomic_ranks[args$taxonomic_rank]] %>% names()

features_otu_phy <-
  read_csv(args$features_csv) %>%
  set_rownames(.$sample_id) %>%
  select(-sample_id) %>%
  phyloseq::otu_table(taxa_are_rows = FALSE)

# CSV table with rows species, genus, ...
# must be sorted descending by NA to get correct ncbi tax id
# e.g. taxonomy_tbl %>% dplyr::arrange(desc(is.na(superkingdom)), desc(is.na(phylum)), ..., desc(is.na(species)))
taxonomy_tbl <-
  readr::read_csv(args$taxonomy_csv, col_types = cols(.default = col_character())) %>%
  dplyr::select(used_taxonomic_ranks) %>%
  dplyr::filter_at(1, all_vars(. %in% taxa_names(features_otu_phy))) %>%
  dplyr::group_by_at(1) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup()

features_tax_phy <-
  taxonomy_tbl %>%
  base::as.matrix() %>%
  phyloseq::tax_table()

phyloseq::taxa_names(features_tax_phy) <- taxonomy_tbl %>% pluck(used_taxonomic_ranks[1])

features_tree_phy <- phyloseq::read_tree(args$tree_nwk)
features_tree_phy$tip.label <- features_tree_phy$tip.label %>% str_replace_all("_", " ")

samples_tbl <- readr::read_csv(args$samples_csv)
samples_phy <- phyloseq::sample_data(samples_tbl)
phyloseq::sample_names(samples_phy) <- samples_tbl[[1]]

if (args$ignore_phylogeny) {
  features_phy <-
    phyloseq::phyloseq(features_otu_phy, samples_phy) %>%
    # filter samples w/o any counts in loaded phylogenetic tree
    # this is required for vegan to calculate eigenvectors
    phyloseq::prune_samples(phyloseq::sample_sums(.) > 0, .) %>%
    phyloseq::prune_taxa(phyloseq::taxa_sums(.) > 0, .)

  base::saveRDS(features_phy, args$out_rds)
} else {
  features_phy <-
    phyloseq::phyloseq(features_otu_phy, features_tax_phy, samples_phy, features_tree_phy) %>%
    # filter samples w/o any counts in loaded phylogenetic tree
    # this is required for vegan to calculate eigenvectors
    phyloseq::prune_samples(phyloseq::sample_sums(.) > 0, .) %>%
    phyloseq::prune_taxa(phyloseq::taxa_sums(.) > 0, .)

  base::saveRDS(features_phy, args$out_rds)
}
