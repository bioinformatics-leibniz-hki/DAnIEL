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
# Excluding samples failed QC
# 1. Summarizes selected qc tests
# 2. Creates directory with soft links of samples passed all tests
#

require(tidyverse)
require(magrittr)
require(jsonlite)
require(optparse)

args <- base::list(
  optparse::make_option(
    opt_str = "--qc-dir",
    type = "character",
    help = "QC directory"
  ),
  optparse::make_option(
    opt_str = "--qc-exclusion-criteria",
    type = "character",
    help = "QC exclusion criteria, separated by comma"
  ),
  optparse::make_option(
    opt_str = "--min-qc-read-count",
    type = "integer",
    help = "Minimum abundance percentage to count as being prevalent (%)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "QC tests"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

args$qc_exclusion_criteria <-
  args$qc_exclusion_criteria %>%
  stringr::str_split(",") %>%
  purrr::simplify()

# args <- list(
#   qc_dir = "~/prj/3-human-paper-cohort/runs/human-its1/qc/UNITE_primers_QC/",
#   min_qc_read_count = 1000,
#   qc_exclusion_criteria = c("fastqc_adapter_content_failed", "min_qc_read_count_failed", "fastqc_per_base_sequence_quality_failed")
# )

after_multiqc_fastqc_path <- base::paste(args$qc_dir, "after/multiqc_report_data/multiqc_fastqc.txt", sep = "/")
out_dir <- base::paste(args$qc_dir, "final", sep = "/")


if (!base::dir.exists(out_dir)) {
  base::dir.create(out_dir)
}

tests_tbl <-
  readr::read_tsv(after_multiqc_fastqc_path, col_types = cols()) %>%
  dplyr::mutate(
    mate_id = Sample %>% stringr::str_extract("_[12]\\.qc$") %>% stringr::str_extract("[12]"),
    sample_id = Sample %>% stringr::str_remove("_[12]\\.qc$"),
  ) %>%
  dplyr::transmute(
    mate_id = mate_id,
    sample_id = sample_id,
    min_qc_read_count_failed = `Total Sequences` < args$min_qc_read_count,
    fastqc_adapter_content_failed = adapter_content == "fail",
    fastqc_per_base_n_content_failed = per_base_n_content == "fail",
    fastqc_per_base_sequence_quality_failed = per_base_sequence_quality == "fail",
    fastqc_per_sequence_quality_scores_failed = per_sequence_quality_scores == "fail",
    fastqc_sequence_length_distribution_failed = sequence_length_distribution == "fail"
  )

# Ignore missing per tile sequence quality (E.g. not available in NCBI SRA FASTQ header)
if ("per_tile_sequence_quality" %in% colnames(tests_tbl)) {
  tests_tbl <- dplyr::mutate(tests_tbl, fastqc_per_tile_sequence_quality_failed = per_tile_sequence_quality == "fail")
} else {
  warning("FastQC per tile sequence tile test not available. Will be ignored.")
  tests_tbl <- dplyr::mutate(tests_tbl, fastqc_per_tile_sequence_quality_failed = "")
}

excluded_samples_tbl <-
  tests_tbl %>%
  dplyr::select(sample_id, mate_id, args$qc_exclusion_criteria) %>%
  tidyr::gather(test, test_result, -sample_id, -mate_id) %>%
  dplyr::group_by(sample_id) %>%
  dplyr::filter(test_result == TRUE) %>%
  dplyr::select(-test_result) %>%
  dplyr::rename(reason = test) %T>%
  readr::write_csv(paste(out_dir, "excluded_samples.csv", sep = "/"))

excluded_samples <-
  excluded_samples_tbl %>%
  dplyr::pull(sample_id) %>%
  base::unique()

# link reads passed all qc tests
reads_after_dir <- base:::paste0(args$qc_dir, "after")
reads_out_dir <- base::paste(out_dir, "reads", sep = "/")

if (!base::dir.exists(reads_out_dir)) {
  base::dir.create(reads_out_dir)
}

res <-
  base::list.files(reads_after_dir, pattern = ".*\\.qc\\.fq\\.gz") %>%
  # remove excluded samples
  base::setdiff(excluded_samples %>% paste0(., "_1.qc.fq.gz")) %>%
  base::setdiff(excluded_samples %>% paste0(., "_2.qc.fq.gz")) %>%
  base::lapply(function(x) {
    # soft link to final qc directory
    from_path <- base::paste(reads_after_dir, x, sep = "/")
    to_path <- base::paste(reads_out_dir, x, sep = "/")
    # soft link may not work inside docker volume. Hardlinks used instead.
    base::file.link(from_path, to_path)
  })

# write list of passed QC samples
tests_tbl %>%
  dplyr::pull(sample_id) %>%
  base::setdiff(excluded_samples) %>%
  base::paste(collapse = "\n") %>%
  readr::write_file(base::paste(out_dir, "final_samples.txt", sep = "/"))
