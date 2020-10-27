# !/usr/bin/env R

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
# Module to display and download a plot
#

download_image_modal <- function(ns) {
  shiny::modalDialog(
    title = "Download image",
    easyClose = TRUE,
    shiny::selectInput(
      inputId = ns("file_type"),
      label = "File type",
      choices = c("png", "jpg", "tiff", "pdf", "svg"),
      selected = "png"
    ),
    shiny::numericInput(
      inputId = ns("width"),
      label = "Width (cm)",
      value = 15
    ),
    shiny::numericInput(
      inputId = ns("height"),
      label = "Height (cm)",
      value = 15
    ),
    shiny::conditionalPanel(
      condition = "input.file_type.match('png|jpg|tiff')",
      ns = ns,
      shiny::numericInput(
        inputId = ns("dpi"),
        label = "Resolution (dpi)",
        value = 300
      )
    ),
    shiny::downloadButton(
      outputId = ns("download_image"),
      label = "download"
    )
  )
}

plot_mod_UI <- function(id) {
  ns <- shiny::NS(id)
  shiny::fluidPage(
    ggiraph::girafeOutput(
      outputId = ns("plot"),
    ) %>% withSpinner(),
    shiny::actionButton(
      inputId = ns("show_download_modal"),
      label = "Download plot",
      icon = shiny::icon("download")
    )
  )
}

#' @param plt Shiny reactive ggplot object
plot_mod <- function(input, output, session, plt) {
  filename <- shiny::reactive(paste0("image.", input$file_type))

  shiny::observeEvent(
    eventExpr = input$show_download_modal,
    handlerExpr = {
      shiny::showModal(download_image_modal(session$ns))
    }
  )

  output$plot <- ggiraph::renderGirafe({
    plt() %>%
      ggiraph::girafe(ggobj = ., options = list(ggiraph::opts_toolbar(saveaspng = FALSE)))
  })

  output$download_image <- shiny::downloadHandler(
    filename = filename,
    content = function(filename) {
      ggsave <- function(...) {
        ggplot2::ggsave(filename = filename, plot = plt(), width = input$width, height = input$height, dpi = input$dpi, units = "cm", ...)
      }

      if (filename %>% endsWith("pdf")) {
        ggsave(device = cairo_ps)
      } else {
        ggsave()
      }
    }
  )
}
