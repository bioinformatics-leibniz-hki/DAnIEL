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
# CLI API to generate analysis reports
#

library(tidyverse)
library(rmarkdown)

args <- base::list(
  optparse::make_option(
    opt_str = c("-p", "--project-dir"),
    type = "character",
    help = "Project directory"
  ),
  optparse::make_option(
    opt_str = c("-r", "--reports-dir"),
    type = "character",
    default = "",
    help = "reports directory"
  ),
  optparse::make_option(
    opt_str = c("-d", "--db-dir"),
    type = "character",
    help = "Database directory"
  ),
  optparse::make_option(
    opt_str = c("-i", "--in-rmd"),
    type = "character",
    help = "R Markdown document"
  ),
  optparse::make_option(
    opt_str = c("-t", "--tmp-dir"),
    type = "character",
    default = "/tmp/",
    help = "Directory for knitr intermediate files"
  ),
  optparse::make_option(
    opt_str = c("-o", "--out-html"),
    type = "character",
    help = "output report (HTML)"
  )
) %>%
  optparse::OptionParser(
    option_list = .,
    description = "Knit R Markdown report"
  ) %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

# args <- list(
#   in_rmd = "/sbidata/server/daniel/src/back_end/reports/summary.Rmd",
#   out_html = "/sbidata/server/daniel/userdat/example/",
#   project_dir = "/sbidata/server/daniel/userdat/example/",
#   db_dir = "/sbidata/server/daniel/db/",
#   tmp_dir = "/sbidata/server/daniel/userdat/example/"
# )

rmarkdown::render(
  input = args$in_rmd,
  output_file = args$out_html,
  output_format = "html_document",
  intermediates_dir = args$tmp_dir,
  params = list(
    project_dir = args$project_dir,
    reports_dir = args$reports_dir,
    db_dir = args$db_dir
  )
)
