# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Reads csv file and guessses column classes
#' TODO: Refactor code to use readr::read_csv and danielLib::guess_coltypes directly
#' @param csv_path path of csv file
#' @return tibble with factor column project and character column sample_id
read_csv_guess_coltypes <- function(csv_path) {
  readr::read_csv(csv_path) %>% danielLib::guess_coltypes()
}
