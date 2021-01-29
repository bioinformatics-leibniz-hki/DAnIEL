#!/usr/bin/env Rscript

# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena,Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#
# Installing front end dependencies
#

options(
  Ncpus = as.integer(system("nproc", intern = TRUE))
)

libpath <- .libPaths()[1]
message(base::paste(
  "Installing R packages to ",
  libpath
))


# install CRAN packages
pkgs <- c(
  "dunn.test",
  "grid",
  "readr",
  "sparkline",
  "BiocManager",
  "DT",
  "bsplus",
  "caret",
  "devtools",
  "formattable",
  "ggiraph",
  "ggnewscale",
  "ggpubr",
  "ggrepel",
  "htmlwidgets",
  "jsonlite",
  "kableExtra",
  "lplyr",
  "lplyr",
  "magrittr",
  "optparse",
  "phyloseq",
  "scales",
  "shiny",
  "shinyBS",
  "shinycssloaders",
  "shinydashboard",
  "shinyjs",
  "shinythemes",
  "svglite",
  "tidyverse",
  "utils",
  "uuid",
  "viridis",
  "visNetwork",
  "writexl"
)

utils::install.packages(pkgs, lib = libpath)

devtools::install_version(
  package = "msaR",
  version = "0.5.0",
  repos = "http://cran.rstudio.com",
  upgrade = "never"
)

# install Bioconductor packages
bioc_pkgs <- c(
  "ggtree",
  "Biostrings",
  "ComplexHeatmap",
  "phyloseq"
)
BiocManager::install(bioc_pkgs)
