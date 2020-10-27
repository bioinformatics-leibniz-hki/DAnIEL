#! /usr/bin/env Rscript

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
# Demultiplexing of reads
#
# IN:   Directory with multiplexed read files
# OUT:  Demultiplexed sequences
#

library(tidyverse)
library(ShortRead)

args <- base::commandArgs(trailingOnly = TRUE)

samples_meta_path <- args[1]
muxed_reads_dir <- args[2]
demuxed_reads_dir <- args[3]

samples_meta_tbl <- readr::read_csv(samples_meta_path, col_types = cols())

# Is demultiplexing required ?
if (!(base::intersect(c("barcode_seq", "barcode_file"), base::colnames(samples_meta_tbl)) %>%
  base::length() == 2)) {
  message("No demultiplexing required.")
  base::quit()
}

# multiplexed fiels required according to sampels meta data table
muxed_file_paths <-
  samples_meta_tbl$barcode_file %>%
  base::unique() %>%
  purrr::discard(is.na) %>%
  # collapse patterns to one regex
  base::paste(collapse = "|") %>%
  base::list.files(muxed_reads_dir, pattern = ., full.names = TRUE)

get_read_paths <- function(barcode_file, muxed_file_paths) {
  base::grep(barcode_file, muxed_file_paths, value = TRUE)
}

mux_tbl <-
  tibble::tibble(barcode_file = unique(samples_meta_tbl$barcode_file)) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    fwd_muxed_path = get_read_paths(barcode_file, muxed_file_paths) %>% purrr::pluck(1),
    rev_muxed_path = get_read_paths(barcode_file, muxed_file_paths) %>% purrr::pluck(2),
    is_ok = get_read_paths(barcode_file, muxed_file_paths) %>% base::length() == 2
  )

# sanity check: must find exactly two read files for each barcode
corrupted_barcodes <-
  mux_tbl %>%
  dplyr::filter(!is_ok) %>%
  dplyr::pull(barcode_file)

if (base::length(corrupted_barcodes) > 0) {
  corrupted_barcodes %>%
    base::paste(collapse = ", ") %>%
    base::sprintf("Barcodes of table and uploaded read files are not uniquiley associated. (%s)", .) %>%
    stop()
}

tbl <-
  samples_meta_tbl %>%
  dplyr::inner_join(mux_tbl, by = "barcode_file") %>%
  # drop samples with no demux information
  tidyr::drop_na(barcode_seq, barcode_file, fwd_muxed_path, rev_muxed_path)

#
# extract reads for each sample
#

read_fastq_sample <- function(muxed_fastq_path, barcode_seq) {
  seqs <- ShortRead::readFastq(muxed_fastq_path)
  seqs[ShortRead::idFilter(regex = barcode_seq)(seqs)]
}

read_name <- function(read) {
  read %>%
    ShortRead:id() %>%
    base::as.character() %>%
    stringr::str_extract("^[^ ]+")
}

base::mapply(
  FUN = function(sample_id, fwd_muxed_path, rev_muxed_path, barcode_seq) {
    read_fastq_sample(purrr::pluck(fwd_muxed_path, 1), barcode_seq) %>%
      ShortRead::writeFastq(base::sprintf("%s/%s_1.raw.fq.gz", demuxed_reads_dir, sample_id))

    read_fastq_sample(pluck(rev_muxed_path, 1), barcode_seq) %>%
      ShortRead::writeFastq(base::sprintf("%s/%s_2.raw.fq.gz", demuxed_reads_dir, sample_id))
  },
  sample_id = tbl$sample_id,
  fwd_muxed_path = tbl$fwd_muxed_path,
  rev_muxed_path = tbl$rev_muxed_path,
  barcode_seq = tbl$barcode_seq
)
