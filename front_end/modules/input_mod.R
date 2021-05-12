# !/usr/bin/env Rscript

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
# Shiny module to upload files to web server.
# Includes visualizations of the uploaded files
#

#' Load excel or csv files
#' @return tibble with guessed coltypes or NULL if reading fails
load_samples_tbl <- function(path) {
  path_extension <-
    path %>%
    basename() %>%
    stringr::str_remove("^[^.]+\\.")

  tbl <- tryCatch(
    switch(
      path_extension,
      "csv" = readr::read_csv(path),
      "xlsx" = readxl::read_excel(path)
    ),
    error = function(e) NULL
  )

  if (is.null(tbl)) {
    return(NULL)
  }

  tbl %>%
    danielLib::guess_coltypes()
}

update_read_found_columns <- function(read_dir, tbl) {
  read_files <- base::list.files(read_dir)
  # first colum must be the sample id
  sample_ids <- tbl[[1]]
  tbl <-
    tbl %>%
    dplyr::mutate(
      fwd_mate_found = read_files %>% base::grep(paste0(sample_ids, "_1"), .) %>% base::length() == 1,
      rev_mate_found = read_files %>% base::grep(base::paste0(sample_ids, "_2"), .) %>% base::length() == 1,
      single_read_found = read_files %>% base::grep(sample_ids, .) %>% base::length() == 1
    )
  return(tbl)
}

text_to_vector <- function(text) {
  #' Performs splitting and trimming of a text (e.g. from multi line textbox) to get a vector of unique entries
  text %>%
    stringr::str_split("\n") %>%
    base::unlist() %>%
    stringr::str_trim() %>%
    base::setdiff("")
}

input_mod_UI <- function(id) {
  ns <- NS(id)

  shiny::fluidPage(
    shiny::h1("Input"),
    shiny::div(
      paste0(
        "This page is about uploading own samples (FASTQ raw reads) and it's meta data (Excel or CSV). ",
        "Furthermore, external samples from the data base NCBI SRA can be added to the project."
      )
    ),
    shiny::h2("Upload local samples"),
    shiny::fileInput(
      inputId = ns("samples_meta_table"),
      label = "Samples meta data",
      accept = c(".xlsx", ".csv")
    ) %>% update_label(),
    shiny::fileInput(
      inputId = ns("local_files"),
      label = "Local raw read files",
      multiple = TRUE,
      accept = "*.fq.gz"
    ) %>% update_label(),
    shiny::h2("Add external samples"),
    shiny::textAreaInput(
      inputId = ns("sra_ids"),
      label = "SRA accessions (one id per line)",
      placeholder = "SRR5098341\nSRR5098371\nSRR5098409",
      height = "150px"
    ) %>% update_label(),
    shiny::selectInput(
      inputId = ns("project"),
      label = "External project",
      choices = "",
      multiple = FALSE
    ) %>% update_label(),
    shiny::textInput(
      inputId = ns("project_filter_query"),
      label = "Filter query for external projects",
      placeholder = "total_spots >= 1000"
    ) %>% update_label(),
    shiny::actionButton(
      inputId = ns("add_samples"),
      label = "Add external samples",
      icon = icon("plus")
    ),
    shiny::h2("Samples overview"),
    shiny::fluidRow(
      DT::dataTableOutput(outputId = ns("show_samples_meta_table"), height = 500, width = "100%") %>% withSpinner(),
    ),
    shiny::fluidRow(
      ggiraph::girafeOutput(outputId = ns("samples_distribution"), height = 500) %>% withSpinner()
    )
  )
}

save_samples_tbl <- function(samples_meta_csv_path, samples_tbl) {
  if (!base::file.exists(samples_meta_csv_path)) {
    samples_meta_csv_path %>%
      stringr::str_remove("[^/]+$") %>%
      dir.create(recursive = TRUE)
  }

  readr::write_csv(samples_tbl, samples_meta_csv_path)
}

input_mod <- function(input, output, session, project) {
  project_json_path <- base::paste0(project$project_dir, "/input/project.json")
  samples_meta_csv_path <- base::paste0(project$project_dir, "/input/samples.csv")

  shiny::updateTextAreaInput(
    session = session,
    inputId = "sra_ids",
    value = paste(project$input$sra_ids, collapse = "\n")
  )

  projects_l <- projects_tbl$bioproject_id
  names(projects_l) <- projects_tbl$name

  # external projects
  projects <- shiny::reactiveVal()
  list() %>% projects()

  shiny::updateSelectInput(
    session = session,
    inputId = "project",
    choices = projects_l,
    selected = ""
  )

  shiny::updateTextInput(
    session = session,
    inputId = "project_filter_query",
    value = project$input$projects$filter_query
  )

  samples_tbl <- shiny::reactiveVal()
  # try to load saved meta data table once at startup
  if (base::file.exists(samples_meta_csv_path)) {
    load_samples_tbl(samples_meta_csv_path) %>% samples_tbl()
  } else {
    tibble(sample_id = character(), project = factor()) %>% samples_tbl()
  }

  shiny::observeEvent(
    eventExpr = input$local_files,
    handlerExpr = {
      if (project$project_id == "example") {
        shiny::showNotification(
          ui = "The example project is read only and already processed",
          type = "warning",
          duration = 15
        )
        return()
      }

      if (!all(input$local_files$name %>% str_detect("_[12]\\.f(ast)?q(\\.gz)?$"))) {
        shiny::showNotification(
          ui = base::paste0(
            "Only paired FASTQ files accepted following pattern {sample id}_{mate}.{file extension}.",
            "File extension must be one of .fastq, .fq, fastq.gz or fq.gz"
          ),
          type = "error",
          duration = 15
        )
        return()
      }

      uploaded_sample_ids <-
        input$local_files %>%
        dplyr::pull(name) %>%
        stringr::str_remove("_[12].*$") %>%
        unique() %>%
        # split muxed samples
        map_if(~ .x %>% str_detect("^muxed"), ~ .x %>%
          str_split("_") %>%
          simplify()) %>%
        simplify() %>%
        .[. != "muxed"]

      added_samples_tbl <-
        uploaded_sample_ids %>%
        setdiff(samples_tbl() %>% dplyr::pull(sample_id)) %>%
        tibble::tibble(sample_id = ., project = "local" %>% as.factor())

      if (added_samples_tbl %>% nrow() > 0) {
        samples_tbl() %>%
          dplyr::bind_rows(added_samples_tbl) %>%
          dplyr::mutate_at("project", factor) %>%
          samples_tbl()
      }
    }
  )

  shiny::observeEvent(
    eventExpr = input$samples_meta_table$datapath,
    handlerExpr = {
      if (project$project_id == "example") {
        shiny::showNotification(
          ui = "The example project is read only and already processed",
          type = "warning",
          duration = 15
        )
        return()
      }

      uploaded_samples_tbl <-
        input$samples_meta_table$datapath %>%
        load_samples_tbl()

      if (is.null(uploaded_samples_tbl)) {
        shiny::showNotification(
          ui = "Table could not be loaded",
          type = "error",
          duration = 15
        )
        return()
      }

      if (!"sample_id" %in% colnames(uploaded_samples_tbl)) {
        shiny::showNotification(
          ui = "Uploaded table must contain column sample_id",
          type = "error",
          duration = 15
        )
        return()
      }

      # add uploaded samples meta data table
      samples_tbl() %>%
        dplyr::filter(!sample_id %in% uploaded_samples_tbl$sample_id) %>%
        dplyr::full_join(uploaded_samples_tbl) %>%
        dplyr::mutate(project = factor(project)) %>%
        samples_tbl()

      project_input_dir <- base::paste(project$project_dir, "input", sep = "/")
      if (!base::dir.exists(project_input_dir)) {
        base::dir.create(project_input_dir, recursive = TRUE)
      }

      shiny::showNotification(
        ui = "Uploaded table added",
        type = "default",
        duration = 15
      )

      samples_tbl() %>%
        write_csv(samples_meta_csv_path)
    }
  )

  shiny::observeEvent(
    eventExpr = input$project_filter_query,
    handlerExpr = {
      shiny::updateTextInput(
        session = session,
        inputId = "project_filter_query",
        value = input$project_filter_query %>% sanitize_expression_string()
      )
    }
  )

  # add external SRA ids
  shiny::observeEvent(
    eventExpr = input$add_samples,
    handlerExpr = {
      if (project$project_id == "example") {
        return()
      }

      # no SRA id provided
      if (input$sra_ids %>% str_trim() == "") {
        return()
      }

      input_sra_samples <-
        input$sra_ids %>%
        stringr::str_split("\n") %>%
        purrr::simplify() %>%
        map_chr(stringr::str_trim) %>%
        unique()

      accepted_sra_samples <-
        input_sra_samples %>%
        keep(~ str_detect(.x, "^[A-Z]RR[0-9]+$"))

      discarded_sra_samples <-
        samples_tbl() %>%
        dplyr::filter(project == "SRA") %>%
        dplyr::pull(sample_id) %>%
        setdiff(accepted_sra_samples)

      if (length(input_sra_samples) != length(accepted_sra_samples)) {
        shiny::showNotification(
          ui = "Invalid SRA id(s) detected",
          type = "error",
          duration = 15
        )
        return()
      }

      sra_samples_tbl <-
        accepted_sra_samples %>%
        tibble::tibble(sample_id = ., project = "SRA") %>%
        # add meta data only if not provided
        dplyr::filter(!sample_id %in% {
          samples_tbl() %>% pull(sample_id)
        })

      # add new samples to meta data table
      samples_tbl() %>%
        mutate(project = as.character(project)) %>%
        dplyr::full_join(sra_samples_tbl, by = c("sample_id", "project")) %>%
        dplyr::filter(!sample_id %in% discarded_sra_samples) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(project = factor(project)) %>%
        samples_tbl()

      shiny::updateTextInput(
        session = session,
        inputId = "sra_ids",
        value = accepted_sra_samples %>% paste(collapse = "\n")
      )

      accepted_sra_samples %>%
        length() %>%
        sprintf("%s SRA sample(s) added", .) %>%
        shiny::showNotification(ui = ., duration = 15)

      discarded_sra_samples %>%
        length() %>%
        sprintf("%s SRA sample(s) removed", .) %>%
        shiny::showNotification(ui = ., duration = 15)
    }
  )

  # add external projects
  shiny::observeEvent(
    eventExpr = input$add_samples,
    handlerExpr = {
      if (project$project_id == "example") {
        shiny::showNotification(
          ui = "The example project is read only and already processed",
          type = "warning",
          duration = 15
        )
        return()
      }

      # no external sample provided
      if (input$project %>% str_trim() == "") {
        return()
      }

      projects_samples_tbl <- try(
        danielLib::filter_samples(
          samples_tbl = external_samples_tbl,
          projects = input$project,
          projects_filter_query = input$project_filter_query
        ),
        {
          shiny::showNotification("Filter query invalid", duration = 15, type = "error")
          return()
        }
      )

      # add new samples to meta data table
      samples_tbl() %>%
        dplyr::full_join(projects_samples_tbl) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(project = factor(project)) %>%
        samples_tbl()

      # add new samples to projects
      projects() %>%
        magrittr::inset(input$project, list(
          # nested list needed for magrittr inset
          list(
            filter_query = input$project_filter_query,
            samples = projects_samples_tbl$sample_id
          )
        )) %>%
        projects()

      save_samples_tbl(samples_meta_csv_path, samples_tbl())

      # clear UI
      shiny::updateTextInput(
        session = session,
        inputId = "project_filter_query",
        value = ""
      )

      shiny::updateSelectInput(
        session = session,
        inputId = "project",
        selected = ""
      )

      projects_samples_tbl %>%
        nrow() %>%
        sprintf("%s external sample(s) added", .) %>%
        shiny::showNotification(ui = ., duration = 15)
    }
  )

  output$show_samples_meta_table <- DT::renderDataTable({
    shiny::need(
      expr = samples_tbl() %>% dim() %>% purrr::pluck(1) > 0,
      message = "No samples meta data table available"
    ) %>% shiny::validate()

    show_tbl <- samples_tbl()

    logical_columns <- show_tbl %>%
      base::sapply(., class) %>%
      tibble::enframe(name = "key") %>%
      dplyr::filter(value == "logical") %>%
      dplyr::pull(key)

    if (length(logical_columns) > 0) {
      # modify samples tibble to highlight binary columns
      for (lgl_col in logical_columns) {
        # lgl to int conversion for DT style formatting
        show_tbl[[paste0(lgl_col, "_int")]] <-
          base::ifelse(
            test = dplyr::pull(show_tbl, base::eval(lgl_col)),
            yes = 1,
            no = 0
          )
      }

      dummy_col_numbers <-
        show_tbl %>%
        base::colnames() %in% base::paste0(logical_columns, "_int") %>%
        base::which()

      DT::datatable(
        data = show_tbl,
        options = base::list(
          # hide logical dummy columns with _int suffix
          columnDefs = base::list(base::list(
            visible = FALSE, targets = dummy_col_numbers
          )),
          scrollX = TRUE
        )
      ) %>%
        DT::formatStyle(
          columns = logical_columns,
          valueColumns = logical_columns %>% base::paste0(., "_int"),
          color = DT::styleEqual(c("0", "1"), c("red", "green"))
        )
    } else {
      # no highlighting
      DT::datatable(data = show_tbl)
    }
  })

  # show cohort overview plot
  output$samples_distribution <- ggiraph::renderGirafe({
    shiny::need(
      expr = samples_tbl() %>% dim() %>% purrr::pluck(1) > 0,
      message = "No samples meta data table available"
    ) %>% shiny::validate()

    samples_distribution_plt <-
      samples_tbl() %>%
      danielLib::plot_samples_distribution()

    ggiraph::girafe(ggobj = samples_distribution_plt)
  })

  # save uploaded read files
  shiny::observeEvent(
    eventExpr = input$local_files,
    handlerExpr = {
      if (project$project_id == "example") {
        return()
      }

      if (! any(input$local_files$name %>% stringr::str_detect("_[12]\\.f(ast)?q(\\.gz)?$"))) {
        return()
      }

      reads_dir <- base::paste0(project$project_dir, "/input/reads/")
      if (!base::dir.exists(reads_dir)) base::dir.create(reads_dir, recursive = TRUE)

      to_file_paths <-
        input$local_files$name %>%
        # unify file extension
        stringr::str_replace("fastq$", "fq") %>%
        stringr::str_replace("fastq\\.gz$", "fq.gz") %>%
        stringr::str_replace("fq$", "raw.fq") %>%
        stringr::str_replace("fq.gz$", "raw.fq.gz") %>%
        purrr::map_chr(~ base::paste0(reads_dir, .x))

      # save uploaded files. Preserve file names.
      base::file.copy(
        from = input$local_files$datapath,
        to = to_file_paths
      )

      # Ensure uploaded file is compressed
      to_file_paths %>%
        purrr::discard(~ .x %>% str_ends("gz")) %>%
        purrr::walk(R.utils::gzip)
    }
  )

  # expose variables needed in other modules
  vals <- shiny::reactiveValues()
  shiny::observe({
    vals$projects <- projects
    vals$local_files <- input$local_files
    vals$sra_ids <- input$sra_ids
    vals$samples_tbl <- samples_tbl
  })
  return(vals)
}
