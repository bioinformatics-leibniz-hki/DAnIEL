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
# Feature Generation
#

require(tidyverse)
require(magrittr)
require(optparse)
require(compositions)
require(vegan)
require(metagenomeSeq)

args <- base::list(
  optparse::make_option(
    opt_str = "--denoised-csv",
    type = "character",
    help = "Abundance profile of denoised sequences (csv)"
  ),
  optparse::make_option(
    opt_str = "--phylotyped-csv",
    type = "character",
    help = "Taxonomic annotations of denosied sequences (csv)"
  ),
  optparse::make_option(
    opt_str = "--samples-meta-csv",
    type = "character",
    help = "Samples meta data table (CSV)"
  ),
  optparse::make_option(
    opt_str = c("-r", "--taxonomic-rank"),
    type = "character",
    help = "Taxonomic rank to pool denoised sequences together"
  ),
  optparse::make_option(
    opt_str = c("-a", "--min-abundance-perc"),
    type = "double",
    help = "Minimum abundance percentage to count as being prevalent (%)"
  ),
  optparse::make_option(
    opt_str = c("-p", "--min-prevalence-perc"),
    type = "double",
    help = "Minimum percentage of samples a taxon must be prevalent (%)"
  ),
  optparse::make_option(
    opt_str = "--group-prevalence",
    type = "character",
    default = "all",
    help = "A feature is prevalent if it is prevalent in any group specified by the sample meta data column"
  ),
  optparse::make_option(
    opt_str = c("-m", "--normalization-method"),
    type = "character",
    help = "Normalization method (one of clr, tss, rarefaction)"
  ),
  optparse::make_option(
    opt_str = c("-u", "--unknown-strategy"),
    type = "character",
    default = "remove",
    action = "store_true",
    help = "Strategy to deal with unassigned sequences. One of remove or infer:"
  ),
  optparse::make_option(
    opt_str = c("-o", "--out-norm-csv"),
    type = "character",
    default = "/dev/stdout",
    help = "normalized profile table output (CSV)"
  ),
  optparse::make_option(
    opt_str = "--out-css-csv",
    type = "character",
    default = "/dev/null",
    help = "CSS normalized profile table output (CSV). Used for ordination."
  ),
  optparse::make_option(
    opt_str = "--out-raw-csv",
    type = "character",
    default = "/dev/null",
    help = "raw profile table output (CSV)"
  ),
  optparse::make_option(
    opt_str = "--out-meta-csv",
    type = "character",
    default = "/dev/null",
    help = "Features meta data table output (CSV)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Generate features from denoised and phylotyped profiles using filtering and normalization"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#    denoised_csv = "denoising/My_denoising/denoised.csv",
#    phylotyped_csv = "phylotyping/My_phylotyping/phylotyped.csv",
#    samples_meta_csv = "input/samples.csv",
#    taxonomic_rank = "species",
#    min_abundance_perc = 0.01,
#    min_prevalence_perc = 5,
#    group_prevalence = "bodysite",
#    normalization_method = "clr",
#    remove_unassigned = TRUE,
#    out_norm_csv = "features/Species_low_prev/features.csv",
#    out_css_csv = "features/Species_low_prev/features.css.csv",
#    out_raw_csv = "features/Species_low_prev/features.raw.csv",
#    out_meta_csv = "features/Species_low_prev/features.meta.csv"
#  )

if (!args$normalization_method %in% c("rarefaction", "tss", "clr", "css")) {
  stop(base::sprintf("Normalization method %s not implemented.", args$normalization_method))
}

denoised_tbl <- readr::read_csv(args$denoised_csv)
samples_tbl <- readr::read_csv(args$samples_meta_csv)

replace_na_taxon <- function(taxon, upsteam_taxon) {
  if (!is.na(taxon)) {
    return(taxon)
  }
  if (is.na(upsteam_taxon)) {
    warning(stringr::str_glue("Unable to replace {taxon}. Keep NA"))
    return(NA)
  }

  upsteam_taxon %>%
    str_remove_all("( sp[.]?)+") %>%
    paste0(" sp.")
}

phylotyped_tbl <-
  readr::read_csv(args$phylotyped_csv) %>%
  dplyr::mutate_all(~ .x %>%
    stringr::str_detect("^unclassified$|^unknwon$|^unidentified$|( sp[.]?$)") %>%
    ifelse(yes = NA, no = .x))

phylotyped_tbl <-
  switch(args$unknown_strategy,
    "infer" = {
      phylotyped_tbl %>%
        dplyr::filter(!is.na(kingdom)) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(
          phylum = replace_na_taxon(phylum, kingdom),
          class = replace_na_taxon(class, phylum),
          order = replace_na_taxon(order, class),
          family = replace_na_taxon(family, order),
          genus = replace_na_taxon(genus, family),
          species = replace_na_taxon(species, genus),
          strain = replace_na_taxon(strain, species)
        ) %>%
        dplyr::ungroup()
    },
    "remove" = {
      phylotyped_tbl %>%
        filter_at(args$taxonomic_rank, ~ !is.na(.x))
    },
    stop(stringr::str_glue("Unknown strategy '{args$unknown_strategy}' not implemented!"))
  )

tbl <-
  denoised_tbl %>%
  tidyr::gather(sequence, abundance, -sample_id) %>%
  dplyr::inner_join(phylotyped_tbl, by = "sequence") %>%
  # Pooling at given tax rank
  dplyr::group_by(.dots = c("sample_id", args$taxonomic_rank)) %>%
  dplyr::summarise(abundance = sum(abundance)) %>%
  dplyr::mutate(abundance_perc = abundance / sum(abundance) * 100) %>%
  dplyr::rename(taxon = !!args$taxonomic_rank) %>%
  dplyr::ungroup()


prevalent_taxa <-
  switch(
    args$group_prevalence,
    "all" = {
      # get prevalent species without grouping samples
      n_samples <-
        tbl$sample_id %>%
        base::unique() %>%
        base::length()

      tbl %>%
        dplyr::group_by(taxon) %>%
        dplyr::filter(abundance_perc >= args$min_abundance_perc) %>%
        dplyr::count() %>%
        dplyr::mutate(perc_prevalent = n / n_samples * 100) %>%
        dplyr::filter(perc_prevalent >= args$min_prevalence_perc) %>%
        dplyr::pull(taxon)
    },
    {
      # Get prevalent features by grouping samples
      tbl %>%
        dplyr::inner_join(samples_tbl, by = "sample_id") %>%
        dplyr::mutate(is_abundant = abundance_perc >= args$min_abundance_perc) %>%
        dplyr::group_by(taxon, !!sym(args$group_prevalence), is_abundant) %>%
        dplyr::count() %>%
        spread(is_abundant, n, fill = 0) %>%
        # calculate percentage of prevalence using sample group counts after qc
        mutate(perc_prevalent = `TRUE` / (`TRUE` + `FALSE`) * 100) %>%
        dplyr::filter(perc_prevalent >= args$min_prevalence_perc) %>%
        dplyr::pull(taxon) %>%
        unique() %>%
        as.character()
    }
  )

norm_table <- function(abundance_tbl, normalization_method, prevalent_taxa, remove_unassigned = TRUE) {
  abundance_tbl <- abundance_tbl %>% dplyr::select(sample_id, taxon, abundance, -abundance_perc)

  # normalize table
  norm_tbl <- switch(
    normalization_method,
    "tss" = {
      abundance_tbl %>%
        dplyr::group_by(sample_id) %>%
        dplyr::mutate(abundance = abundance / sum(abundance))
    },
    "clr" = {
      abundance_tbl %>%
        dplyr::group_by(sample_id) %>%
        # compositions version 1.40.3 needed. Subscript out of bounds error otherwise
        dplyr::mutate(abundance = compositions::clr(abundance))
    },
    "raw" = {
      # no normalization
      abundance_tbl
    },
    "css" = {
      # keep only samples with at least two features
      samples_keep <-
        abundance_tbl %>%
        dplyr::group_by(sample_id) %>%
        dplyr::filter(abundance > 0) %>%
        dplyr::count() %>%
        dplyr::filter(n >= 2) %>%
        dplyr::pull(sample_id) %>%
        base::unique()

      counts <-
        abundance_tbl %>%
        dplyr::filter(sample_id %in% samples_keep) %>%
        tidyr::spread(sample_id, abundance) %>%
        dplyr::mutate(taxon = ifelse(is.na(taxon), "NA", taxon)) %>%
        magrittr::set_rownames(.$taxon) %>%
        dplyr::select(-taxon) %>%
        base::as.matrix()

      raw_exp <- metagenomeSeq::newMRexperiment(counts)
      p <- metagenomeSeq::cumNormStatFast(raw_exp)
      metagenomeSeq::cumNorm(raw_exp, p) %>%
        metagenomeSeq::MRcounts(norm = TRUE) %>%
        dplyr::as_tibble(rownames = "taxon") %>%
        tidyr::gather(sample_id, abundance, -taxon)
    },
    "rarefaction" = {
      # get min number of reads
      rarefaction_depth <-
        abundance_tbl %>%
        dplyr::group_by(sample_id) %>%
        dplyr::summarise(abundance = sum(abundance)) %>%
        dplyr::pull(abundance) %>%
        base::min()

      abundance_tbl %>%
        tidyr::spread(taxon, abundance) %>%
        dplyr::select(-sample_id) %>%
        vegan::rrarefy(sample = rarefaction_depth) %>%
        tibble::as_tibble() %>%
        dplyr::mutate(sample_id = abundance_tbl$sample_id %>% unique()) %>%
        tidyr::gather(taxon, abundance, -sample_id)
    }
  )

  norm_tbl %<>%
    dplyr::filter(
      # pnly keep prevalent samples and taxa
      taxon %in% prevalent_taxa
    )

  if (remove_unassigned) {
    norm_tbl %>%
      dplyr::filter(!is.na(taxon))
  } else {
    norm_tbl
  }
}

norm_tbl <- norm_table(tbl, args$normalization_method, prevalent_taxa)
css_tbl <- norm_table(tbl, "css", prevalent_taxa)
raw_tbl <- norm_table(tbl, "raw", prevalent_taxa)

# write normalized output
norm_tbl %>%
  tidyr::spread(taxon, abundance, fill = 0) %>%
  readr::write_csv(args$out_norm_csv)

# write css normalized output
css_tbl %>%
  tidyr::spread(taxon, abundance, fill = 0) %>%
  readr::write_csv(args$out_css_csv)

# write raw count output
raw_tbl %>%
  tidyr::spread(taxon, abundance, fill = 0) %>%
  readr::write_csv(args$out_raw_csv)

# create feature meta data
ranks <- c("strain", "species", "genus", "family", "order", "class", "phylum", "kingdom")
selected_ranks <- list(
  strain = ranks[1:8], species = ranks[2:8], genus = ranks[3:8], family = ranks[4:8],
  order = ranks[5:8], class = ranks[6:8], phylum = ranks[7:8], kingdom = ranks[8:8]
)

feature_meta_tbl <-
  phylotyped_tbl %>%
  dplyr::mutate(feature = .[[!!args$taxonomic_rank]]) %>%
  dplyr::select(feature, selected_ranks[[args$taxonomic_rank]]) %>%
  dplyr::group_by(feature) %>%
  dplyr::slice(1) %>%
  dplyr::filter(!base::is.na(feature) & feature %in% prevalent_taxa)
readr::write_csv(feature_meta_tbl, args$out_meta_csv)
