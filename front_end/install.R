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
# Installing front end dependencies
#

libpath <- .libPaths()[1]
message(base::paste("Installing R packages to ", libpath))

Ncpus <- 15

# install CRAN packages
pkgs <- c(
  "jsonlite", "devtools", "shiny", "shinyjs", "shinycssloaders", "visNetwork", "caret", "lplyr",
  "sparkline", "shinydashboard", "tidyverse", "magrittr", "DT", "viridis", "formattable", "shinyBS",
  "readr", "writexl", "utils", "optparse", "lplyr", "BiocManager", "kableExtra", "bsplus", "ggiraph",
  "grid", "scales", "htmlwidgets", "uuid", "phyloseq", "ggrepel", "ggnewscale", "ggpubr", "shinythemes",
  "dunn.test", "svglite"
)

utils::install.packages(pkgs, lib = libpath, Ncpus = Ncpus)

# install devtools packages
# devtools::install_github("CannaData/shinyCAPTCHA", upgrade = "never")

# install Bioconductor packages
bioc_pkgs <- c("ggtree", "Biostrings", "ComplexHeatmap", "phyloseq")
BiocManager::install(bioc_pkgs, Ncpus = Ncpus)
