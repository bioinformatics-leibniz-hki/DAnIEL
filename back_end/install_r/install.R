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

options(
  Ncpus = parallel::detectCores(),
  repos = c("https://packagemanager.rstudio.com/all/__linux__/bionic/238"),
  pkgType = "source"
)


install.packages("devtools")
library(devtools)

cran_pkgs <- readLines("back_end/install_r/cran.txt")
bioconductor_pkgs <- readLines("back_end/install_r/bioconductor.txt")

devtools::install_cran(cran_pkgs, upgrade = "always")
devtools::install_bioc(bioconductor_pkgs, upgrade = "always")
