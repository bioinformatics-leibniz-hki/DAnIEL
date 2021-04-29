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
    shiny::div(
      id = ns("div_plot_ggplot"),
      ggiraph::girafeOutput(
        outputId = ns("plot_ggplot"),
      ) %>% withSpinner()
    ),

    shiny::div(
      id = ns("div_plot"),
      shiny::plotOutput(
        outputId = ns("plot"),
      ) %>% withSpinner()
    ),

    shiny::actionButton(
      inputId = ns("show_download_modal"),
      label = "Download plot",
      icon = shiny::icon("download")
    )
  )
}

#' @param plt Shiny reactive plot object
#' @param ggplot TRUE if interactive ggplot ggplot object should be displayed or FALSE for general plots
#' @param error character of error message to be displayed instead of plt and NULL otherwise.
plot_mod <- function(input, output, session, plt, ggplot = TRUE, error = NULL) {
  filename <- shiny::reactive(paste0("image.", input$file_type))

  if (ggplot) {
    shinyjs::show("div_plot_ggplot")
    shinyjs::hide("div_plot")
  } else {
    shinyjs::show("div_plot")
    shinyjs::hide("div_plot_ggplot")
  }

  shiny::observeEvent(
    eventExpr = input$show_download_modal,
    handlerExpr = {
      shiny::showModal(download_image_modal(session$ns))
    }
  )

  output$plot_ggplot <- ggiraph::renderGirafe({
    if (ggplot) {
      shiny::need(is.null(error), error) %>% shiny::validate()
      shiny::need("ggplot" %in% class(plt()), "Object must be of class ggplot") %>% shiny::validate()

      plt() %>%
        ggiraph::girafe(ggobj = ., options = list(ggiraph::opts_toolbar(saveaspng = FALSE)))
    } else {
      # Dummy plot
      ggplot2::ggplot()
    }
  })

  output$plot <- shiny::renderPlot({
    if (!ggplot) {
      plt()
    } else {
      # Dummy plot
      ggplot2::ggplot()
    }
  })

  output$download_image <- shiny::downloadHandler(
    filename = filename,
    content = function(filename) {
      format <- filename %>% stringr::str_extract("[A-z]+$")
      width <- input$width
      height <- input$height
      dpi <- input$dpi

      dev_func <- switch(format,
        "png" = grDevices::png,
        "jpg" = grDevices::jpeg,
        "tiff" = grDevices::tiff,
        "pdf" = grDevices::cairo_pdf,
        "svg" = grDevices::svg
      )
      
      # cairo devices use inches
      if(format %in% c("svg", "pdf")) {
        width <- width / 2.54
        height <- height / 2.54
      }

      if (format %in% c("png", "jpg", "tiff")) {
        dev_func <- purrr::partial(dev_func, res = dpi, units = "cm")
      }

      dev_func(filename = filename, width = width, height = height)
      print(plt())
      dev.off()
    }
  )
}
