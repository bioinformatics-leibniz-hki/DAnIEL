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
require(magrittr)
require(optparse)

args <- base::list(
  optparse::make_option(
    c("-f", "--fwd-qc-fastq"),
    type = NULL,
    help = "Fastq file of qc forward read. Gzipped allowed."
  ),
  optparse::make_option(
    c("-r", "--rev-qc-fastq"),
    type = "character",
    help = "Fastq file of qc reverse read. Gzipped allowed."
  ),
  optparse::make_option(
    c("-s", "--sample-name"),
    type = "character",
    default = "sample",
    help = "Name of sample"
  ),
  optparse::make_option(
    "--max-n",
    type = "integer",
    default = 0,
    help = "Sequences with more than maxN Ns will be discarded"
  ), 
  optparse::make_option(
    "--min-q",
    type = "integer",
    default = 0,
    help = "Reads contain a quality score less than minQ will be discarded"
  ),
  optparse::make_option(
    "--max-ee",
    type = "character",
    default = "2,2",
    help = "Reads with higher than maxEE 'expected errors' will be discarded. Values for fwd and rev read separated by ','"
  ),
  optparse::make_option(
    "--trunc-q",
    type = "integer",
    default = 0,
    help = "Truncate reads at the first instance of a quality score less than or equal to truncQ"
  ),
  optparse::make_option(
    c("-o", "--out-dir"),
    type = "character",
    default = "out",
    help = "output directory"
  ),
  optparse::make_option(
    c("-m", "--min-read-length"),
    type = "double",
    default = 50,
    help = "Minimum read length"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Denoising of trimmed amplicon sequencing data using DADA2"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

fwd_filtered_fastq <- base::paste0(args$out_dir, "/", args$sample_name, "_1.filtered.fq.gz")
rev_filtered_fastq <- base::paste0(args$out_dir, "/", args$sample_name, "_2.filtered.fq.gz")

# separate max erros for fws and rev read
args$max_ee <- args$max_ee %>% str_split(",") %>% pluck(1) %>% map_dbl(as.double)

base::dir.create(args$out_dir, recursive = TRUE)

filtered <- dada2::filterAndTrim(
  fwd = args$fwd_qc_fastq,
  filt = fwd_filtered_fastq,
  rev = args$rev_qc_fastq,
  filt.rev = rev_filtered_fastq,
  minQ = args$min_q,
  maxN = args$max_n,
  maxEE = args$max_ee,
  truncQ = args$trunc_q,
  minLen = args$min_read_length,
  rm.phix = TRUE,
  compress = TRUE,
  multithread = TRUE
)

# learning error model
fwd_errors <- dada2::learnErrors(fwd_filtered_fastq, multithread = TRUE)
rev_errors <- dada2::learnErrors(rev_filtered_fastq, multithread = TRUE)

fwd_errors_plt <- dada2::plotErrors(fwd_errors, nominalQ = TRUE)
rev_errors_plt <- dada2::plotErrors(rev_errors, nominalQ = TRUE)

# dereplication
fwd_derep <- dada2::derepFastq(fwd_filtered_fastq, verbose = TRUE)
rev_derep <- dada2::derepFastq(rev_filtered_fastq, verbose = TRUE)

# sample inference
fwd_dada <- dada2::dada(fwd_derep, err = fwd_errors, multithread = TRUE)
rev_dada <- dada2::dada(rev_derep, err = rev_errors, multithread = TRUE)

merged_pairs <- dada2::mergePairs(fwd_dada, fwd_derep, rev_dada, rev_derep, verbose = TRUE)
seqtab <- dada2::makeSequenceTable(merged_pairs)
seqtab_nochim <- dada2::removeBimeraDenovo(seqtab, method = "consensus", multithread = TRUE, verbose = TRUE)
chimeric_asvs <- base::setdiff(
  seqtab %>% base::colnames() %>% Biostrings::DNAStringSet(),
  seqtab_nochim %>% base::colnames() %>% Biostrings::DNAStringSet()
)

if (base::length(chimeric_asvs) > 0) {
  base::names(chimeric_asvs) <- base::paste0("chimASV", base::seq(base::length(chimeric_asvs)), "_", args$sample_name)
  chimeric_asvs %>%
    Biostrings::DNAStringSet() %>%
    Biostrings::writeXStringSet(filepath = base::paste0(args$out_dir, "/chimeric_asvs.fasta"))
}

# create summary
asv_seqs <- seqtab_nochim %>%
  base::colnames() %>%
  magrittr::set_names(nm = paste0("ASV", seq(length(.)), "_", args$sample_name)) %>%
  Biostrings::DNAStringSet() %T>%
  Biostrings::writeXStringSet(filepath = base::paste0(args$out_dir, "/denoised.fasta"))

asv_tbl <- tibble::tibble(sample_id = args$sample_name) %>%
  dplyr::bind_cols(tibble::as_tibble(seqtab_nochim)) %>%
  magrittr::set_colnames(c("sample_id", base::paste0("ASV", seq(length(.) - 1), "_", args$sample_name))) %>%
  # filtered read file name to sample_id
  dplyr::mutate(sample_id = sample_id %>% str_remove("_1\\.filtered\\.fq\\.gz$")) %T>%
  readr::write_csv(paste0(args$out_dir, "/denoised.csv"))

getN <- function(x) base::sum(dada2::getUniques(x))
read_counts <- base::list(
  sample_id = args$sample_name,
  qc = filtered[, "reads.in"],
  filtered = filtered[, "reads.out"],
  dereplicated_fwd = getN(fwd_derep),
  dereplicated_rev = getN(rev_derep),
  denoised_fwd = getN(fwd_dada),
  denoised_rev = getN(rev_dada),
  inferred = base::rowSums(seqtab),
  final = base::rowSums(seqtab_nochim)
)

read_counts %>%
  tibble::as_tibble() %>%
  readr::write_csv(paste0(args$out_dir, "/read_counts.csv"))

res <- base::list(
  fwd_dada = fwd_dada,
  rev_dada = rev_dada
  # save ggplot objects of individual models
  # fwd_errors_plt = fwd_errors_plt,
  # rev_errors_plt = fwd_errors_plt
)

base::saveRDS(res, base::paste0(args$out_dir, "/dada2_res.RDS"))
