# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' count base occurences of MSA sequences
get_consensus_tbl <- function(msa_path, tree) {
  seqs <- treeio::read.fasta(msa_path)
  seqs_tbl <-
    tree$tip.label %>%
    base::lapply(function(x) {
      bases <- base::as.character(seqs[[x]])
      tibble::tibble(seq = x, pos = seq(length(bases)), base = bases)
    }) %>%
    dplyr::bind_rows()

  seqs_count_tbl <-
    seqs_tbl %>%
    dplyr::group_by(pos, base) %>%
    dplyr::count()

  consensus_tbl <-
    seqs_count_tbl %>%
    dplyr::group_by(pos) %>%
    dplyr::filter(n == max(n)) %>%
    dplyr::select(pos, consensus_base = base)

  tbl <-
    seqs_tbl %>%
    dplyr::left_join(seqs_count_tbl) %>%
    dplyr::left_join(consensus_tbl) %>%
    dplyr::mutate(base = case_when(
      base == "-" ~ "-",
      base == consensus_base ~ "#",
      TRUE ~ base
    ))

  res_tbl <-
    tbl %>%
    dplyr::group_by(seq, pos) %>%
    dplyr::slice(1) %>%
    dplyr::select(-n, -consensus_base) %>%
    tidyr::spread(pos, base) %>%
    base::as.data.frame() %>%
    magrittr::set_rownames(.$seq) %>%
    dplyr::select(-seq)

  return(res_tbl)
}
