#!/usr/bin/env R

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
# Initialization script run at every start of the shiny front end app
#

library(msaR)
library(jsonlite)
library(lplyr)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(tidyverse)
library(magrittr)
library(DT)
library(readr)
library(writexl)
library(utils)
library(optparse)
library(digest)
library(httr)
library(Biostrings)
library(treeio)
library(sparkline)
library(grid)
library(circlize)
library(scales)
library(visNetwork)
library(danielLib)
library(formattable)
library(kableExtra)
library(shinycssloaders)
library(caret)
library(ggpubr)
library(bsplus)
library(ggiraph)
library(shinyBS)

# load modules
loaded_modules_msg <-
  list.files("modules", full.names = TRUE) %>% purrr::map(
    function(file) {
      tryCatch(
        {
          source(file)
          sprintf("sucsessfully loaded module %s", file)
        },
        error = function(e) e
      )
    }
  )
