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
# Denoising of trimmed amplicon sequencing data using DADA2
#
# - This script is based on the official DADA2 ITS Pipeline Workflow (1.8):
#   https://benjjneb.github.io/dada2/ITS_workflow.htlm
# - Trimming and primer removal, however, was skipped. The script requires qc reads instead.
# - Additional filtering is still included to remove N's in the middle of the read
#

require(dada2)
require(ShortRead)
require(Biostrings)
require(tidyverse)
require(optparse)

args <- base::list(
  optparse::make_option(
    c("-i", "--in-dir"),
    type = "character", default = ".",
    help = "directory containing qc reads with suffix _1.fq.gz and _2.fq.gz"
  ),
  optparse::make_option(
    c("-o", "--out-dir"),
    type = "character", default = "out",
    help = "output directory"
  )
) %>%
  optparse::OptionParser(
    description = "Denoising of trimmed amplicon sequencing data using DADA2"
  ) %>%
  optparse:parse_args(convert_hyphens_to_underscores = TRUE)

filtered_reads_dir <- base::paste0(args$out_dir, "/filtered_reads")

base::dir.create(args$out_dir)

qc_reads_paths <- base::list.files(args$in_dir, full.names = TRUE) %>%
  base::grep("\\.qc\\.fq\\.gz$", ., value = TRUE)
qc_samples <- qc_reads_paths %>%
  stringr::str_extract("[^\\/]*$") %>%
  stringr::str_remove_all("_(1|2)\\.qc\\.fq\\.gz") %>%
  base::unique()

tbl <- tibble::tibble(sample_id = qc_samples) %>%
  ddplyr::mutate(
    fwd_mate_exists = base::paste0(args$in_dir, "/", sample_id, "_1.qc.fq.gz") %>% base::file.exists(),
    rev_mate_exists = base::paste0(args$in_dir, "/", sample_id, "_2.qc.fq.gz") %>% base::file.exists(),
  ) %>%
  dplyr::filter(fwd_mate_exists && rev_mate_exists)

filtered_fwd_paths <- tbl$sample_id %>% base::paste0(filtered_reads_dir, "/", ., "_1.filtered.fq.gz")
filtered_rev_paths <- tbl$sample_id %>% base::paste0(filtered_reads_dir, "/", ., "_2.filtered.fq.gz")

message("Filter reads...")
filtered <- dada2::filterAndTrim(
  fwd = tbl$sample_id %>% base::paste0(args$in_dir, "/", ., "_1.qc.fq.gz"),
  filt = filtered_fwd_paths,
  rev = tbl$sample_id %>% base::paste0(args$in_dir, "/", ., "_2.qc.fq.gz"),
  filt.rev = filtered_rev_paths,
  maxN = 0,
  maxEE = c(2, 2),
  truncQ = 2,
  minLen = 50,
  rm.phix = TRUE,
  compress = TRUE,
  multithread = TRUE
)

# learning error model
message("Learn error model...")
fwd_errors <- dada2::learnErrors(filtered_fwd_paths, multithread = TRUE)
rev_errors <- dada2::learnErrors(filtered_rev_paths, multithread = TRUE)

fwd_errors_plt <- dada2::plotErrors(fwd_errors, nominalQ = TRUE)
rev_errors_plt <- dada2::plotErrors(rev_errors, nominalQ = TRUE)

# dereplication
message("Dereplicate reads...")
fwd_derep <- dada2::derepFastq(filtered_fwd_paths, verbose = TRUE)
rev_derep <- dada2::derepFastq(filtered_rev_paths, verbose = TRUE)

# sample inference
message("Inferring samples...")
fwd_dada <- dada2::dada(fwd_derep, err = fwd_errors, multithread = TRUE)
rev_dada <- dada2::dada(rev_derep, err = rev_errors, multithread = TRUE)
merged_pairs <- dada2::mergePairs(fwd_dada, fwd_derep, rev_dada, rev_derep, verbose = TRUE)
seqtab <- dada2::makeSequenceTable(merged_pairs)
seqtab_nochim <- dada2::removeBimeraDenovo(seqtab, method = "consensus", multithread = TRUE, verbose = TRUE)
chimeric_asvs <- base::setdiff(
  seqtab %>% base::colnames() %>% Biostrings::DNAStringSet(),
  seqtab_nochim %>% base::colnames() %>% Biostrings::DNAStringSet()
)
base::names(chimeric_asvs) <- base::paste0("chimASV", base::seq(base::length(chimeric_asvs)))
chimeric_asvs %>% Biostrings::writeXStringSet(base::paste0(args$out_dir, "/chimeric_asvs.fasta"))

# create summary
message("Summarize output...")

asv_seqs <- seqtab_nochim %>%
  base::colnames() %>%
  magrittr::set_names(nm = paste0("ASV", seq(length(.)))) %>%
  Biostrings::DNAStringSet() %T>%
  Biostrings::writeXStringSet(paste0(denoised_dir, "/denoised.fasta"))

asv_tbl <- seqtab_nochim %>%
  tibble::as_tibble(rownames = "sample_id") %>%
  magrittr::set_colnames(c("sample_id", paste0("ASV", seq(length(.) - 1)))) %>%
  # filtered read file name to sample_id
  dplyr::mutate(sample_id = sample_id %>% str_remove("_1\\.filtered\\.fq\\.gz$")) %T>%
  readr::write_csv(paste0(denoised_dir, "/denoised.csv"))



getN <- function(x) sum(dada2::getUniques(x))
clean_names <- function(x) {
  base::names(x) %>% stringr::str_remove("_.*")
}
read_counts_tbl <- base::rbind(
  base::t(filtered) %>% magrittr::set_colnames(tbl$sample_id),
  base::sapply(fwd_derep, getN) %>% magrittr::set_names(tbl$sample_id),
  base::sapply(rev_derep, getN) %>% magrittr::set_names(tbl$sample_id),
  base::sapply(fwd_dada, getN) %>% magrittr::set_names(tbl$sample_id),
  base::sapply(rev_dada, getN) %>% magrittr::set_names(tbl$sample_id),
  base::sapply(merged_pairs, getN) %>% magrittr::set_names(tbl$sample_id),
  base::rowSums(seqtab_nochim) %>% magrittr::set_names(tbl$sample_id)
) %>%
  magrittr::set_rownames(
    c(
      "qc", "filtered", "dereplicated_fwd", "dereplicated_rev",
      "denoised_fwd", "denoised_rev", "inferred", "final"
    )
  ) %>%
  base::t() %>%
  tibble::as_tibble(rownames = "sample_id") %>%
  readr::write_csv(base::paste0(args$out_dir, "/read_counts.csv"))

base::save.image(base::paste0(args$out_dir, "/dada2_session.RData"))
