# Copyright by Daniel Loos
#
# Research Group Systems Biology and Bioinformatics - Head: Assoc. Prof. Dr. Gianni Panagiotou
# https://www.leibniz-hki.de/en/systembiologie-und-bioinformatik.html
# Leibniz Institute for Natural Product Research and Infection Biology - Hans Knöll Institute (HKI)
# Adolf-Reichwein-Straße 23, 07745 Jena, Germany
#
# The project code is licensed under BSD 2-Clause.
# See the LICENSE file provided with the code for the full license.

#' Create analysis report
#'
#' The report includes some chunks only if the corresponding analysis method was performed
#' This is done using conditional R markdown chunks with the asis engine (as is)
#' This engine, however, obviously do not support the execution of R code inside that chunk
#' Therefore, dynamic variables inside these chunks are reported as placeholders {{name}}
#' This placeholders have to be replaced in post production
#'
#' @param project_dir path of project directory the report shoulb be based on
#' @param out_html path to output report html file
report <- function(project_dir, out_html, template_rmd = system.file("extdata", "report.Rmd", package = "danielLib")) {
  tmp_dir <- tempdir()
  raw_report_html <- paste0(tmp_dir, "/report.html")
  report_placehodler_csv <- paste0(tmp_dir, "/report.csv")

  params <- list(
    project_dir = project_dir,
    report_placehodler_csv = report_placehodler_csv
  )

  rmarkdown::render(
    input = template_rmd,
    params = params,
    envir = new.env(),
    output_format = "html_document",
    output_file = raw_report_html,
    intermediates_dir = tmp_dir
  )

  report_html <- readr::read_file(raw_report_html)
  report_placehodler_tbl <- readr::read_csv(report_placehodler_csv)

  # replace all placeholders
  res <-
    dim(report_placehodler_tbl)[1] %>%
    seq() %>%
    lapply(function(i) {
      report_html <<- gsub(
        pattern = sprintf("{{%s}}", report_placehodler_tbl$placeholder[[i]]),
        replacement = report_placehodler_tbl$value[[i]],
        x = report_html,
        fixed = TRUE
      )
      return(TRUE)
    })

  readr::write_file(report_html, out_html)
}
