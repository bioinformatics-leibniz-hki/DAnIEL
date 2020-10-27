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
# Main file to start shiny front end app
#

args <- base::list(
  optparse::make_option(
    c("-d", "--server-dir"),
    type = "character", default = ".",
    help = "path to front end web server directory"
  )
) %>%
  optparse::OptionParser(usage = "%prog [options]") %>%
  optparse::parse_args(convert_hyphens_to_underscores = TRUE)

if (!is.null(args$server_dir)) {
  base::setwd(args$server_dir)
}

# load objects for shiny app
source("global.R")
source("ui.R")
source("server.R")

# run shiny app
message(base::paste0("Start app at ", base::getwd()))
shiny::shinyApp(ui = ui, server = server)
