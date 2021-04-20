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
# Main UI script of Shiny front end app
#

shiny::addResourcePath("img", "www/img/")

dashboardHeader <- shinydashboard::dashboardHeader(
  titleWidth = 180,
  title = shiny::img(src = "img/DAnIEL_logo.png", width = "80%", margin = "auto", style = "display:inline"),
  tags$li(
    class = "dropdown",
    tags$ol(
      shiny::div("Current project ID:"),
      shiny::textOutput("project_bar")
      # shiny proxy sign out
      # a(shiny::icon("sign-out-alt"), "Sign out", href = LOGOUT_URL, style = "color:black")
    )
  ),
  tags$li(
    class = "dropdown",
    a(
      style = "padding-top:10px; padding-bottom:10px;",
      href = "https://www.leibniz-hki.de/en/",
      shiny::img(src = "img/HKI.jpg", title = "HKI", height = "30px")
    )
  ),
  tags$li(
    class = "dropdown",
    a(
      style = "padding-top:10px; padding-bottom:10px;",
      href = "https://www.leibniz-gemeinschaft.de/en.html",
      shiny::img(src = "img/Leibniz.jpg", title = "Leibniz Association", height = "30px")
    )
  )
)

dashboardSidebar <- shinydashboard::dashboardSidebar(
  width = 180,
  shinydashboard::sidebarMenu(
    id = "sidebar_menue",
    shinydashboard::menuItem(text = "Home", tabName = "home", icon = icon("home")),
    shinydashboard::menuItem(text = "Tutorial", tabName = "tutorial", icon = icon("life-ring")),
    shinydashboard::menuItem(text = "Legal", tabName = "legal", icon = icon("balance-scale")),
    shiny::h4("Knowledge base", class = "sidebar_header"),
    shinydashboard::menuItem(text = "Reference", tabName = "reference", icon = icon("cog")),
    shinydashboard::menuItem(text = "Interactions", tabName = "interactions", icon = icon("database")),
    shinydashboard::menuItem(text = "Infections", tabName = "infections", icon = icon("database")),
    shinydashboard::menuItem(text = "Projects", tabName = "projects", icon = icon("database")),
    shiny::h4("Project", class = "sidebar_header"),
    shinydashboard::menuItem(text = "Input", tabName = "input", icon = icon("file")),
    shinydashboard::menuItem(text = "Start", tabName = "start", icon = icon("play")),
    shiny::h4("Results", class = "sidebar_header"),
    shinydashboard::sidebarMenuOutput("pipeline_menue") %>% withSpinner()
  )
)

dashboardBody <- shinydashboard::dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "daniel.css"),
    tags$link(
      rel = "stylesheet",
      href = "https://use.fontawesome.com/releases/v5.7.2/css/all.css",
      integrity = "sha384-fnmOCqbTlWIlj8LyTjo7mOUStjsKC4pOpQbqyi7RrhN7udi9RwhKkMHpvLbHG9Sr",
      crossorigin = "anonymous"
    ),
    tags$link(
      rel = "shortcut icon",
      href = "img/favicon.ico",
      type = "image/vnd.microsoft.icon"
    )
  ),
  shinyjs::useShinyjs(),
  shinydashboard::tabItems(
    shinydashboard::tabItem(
      tabName = "home",
      shiny::includeHTML("www/home_head.html"),
      shiny::div(
        align = "center",
        shiny::actionButton(inputId = "go_tutorial", label = "Get started", class = "home_panel_element"),
        shiny::actionButton(inputId = "new_project", label = "Start project", class = "home_panel_element"),
        shiny::actionButton(inputId = "resume_project", label = "Resume project", class = "home_panel_element")
      ),
      shiny::includeHTML("www/home_tail.html")
    ),
    shinydashboard::tabItem(
      tabName = "tutorial",
      shiny::fluidPage(
        shiny::includeHTML("www/tutorial.html")
      )
    ),
    shinydashboard::tabItem(
      tabName = "legal",
      shiny::fluidPage(
        shiny::includeHTML("www/legal.html")
      )
    ),
    shinydashboard::tabItem(
      tabName = "reference",
      shiny::fluidPage(
        shiny::includeHTML("www/reference.html")
      )
    ),
    shinydashboard::tabItem(tabName = "projects", projects_mod_UI("projects_mod")),
    shinydashboard::tabItem(tabName = "interactions", interactions_mod_UI("interactions_mod")),
    shinydashboard::tabItem(tabName = "infections", infections_mod_UI("infections_mod")),
    shinydashboard::tabItem(tabName = "input", input_mod_UI("input_mod")),
    shinydashboard::tabItem(tabName = "start", start_mod_UI("start_mod")),
    shinydashboard::tabItem(tabName = "logs", logs_mod_UI("logs_mod")),
    shinydashboard::tabItem(tabName = "qc", qc_mod_UI("qc_mod")),
    shinydashboard::tabItem(tabName = "denoising", denoising_mod_UI("denoising_mod")),
    shinydashboard::tabItem(tabName = "phylotyping", phylotyping_mod_UI("phylotyping_mod")),
    shinydashboard::tabItem(tabName = "features", features_mod_UI("features_mod")),
    shinydashboard::tabItem(tabName = "correlation", correlation_mod_UI("correlation_mod")),
    shinydashboard::tabItem(tabName = "statistics", statistics_mod_UI("statistics_mod")),
    shinydashboard::tabItem(tabName = "ml", ml_mod_UI("ml_mod")),
    shinydashboard::tabItem(tabName = "summary", summary_mod_UI("summary_mod"))
  )
)

ui <- shinydashboard::dashboardPage(
  header = dashboardHeader,
  sidebar = dashboardSidebar,
  body = dashboardBody,
  skin = "black",
  tags$head(
    shiny::HTML("<title>DAnIEL ITS analysis</title>"),
    shiny::includeCSS("www/custom.css"),
    bsplus::use_bs_tooltip(),
    bsplus::use_bs_popover(),
    shiny::withMathJax()
  )
)
