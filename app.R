library(rmarkdown)
library(markdown)
# shiny app
library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
# library(shinymanager)
library(shinyvalidate)

# Plotting
library(ggplot2)
library(gridExtra)
library(plotly)
library(heatmaply)
library(ggpubr)
library(ggupset)
library(ellipse)
library(waiter)

# text
library(stringr)

# Tables
library(readxl)

# Colors
library(grDevices)
library(RColorBrewer)

# Statistics
library(stats)
library(glmnet)
library(pcaMethods)

# General
library(reshape2)
library(dplyr)
library(tidyr)

# metrics
library(googledrive)
library(googlesheets4)

#------------------------------------------------------- needed for metrics ---
# options(
#   # whenever there is one account token found, use the cached token
#   gargle_oauth_email = TRUE,
#   # specify auth tokens should be stored in a hidden directory ".secrets"
#   gargle_oauth_cache = ".secrets"
# )

# get the id of the file to edit
# sheet_id <- googledrive::drive_get(path = "neurolipidatlas")$id

#-------------------------------------------------------- Tool tip settings ----
# Set up for showing tooltips.
# Use as: shiny::span(your shiny element,
#                     `data-toggle` = "tooltip",
#                     `data-placement` = "right",
#                     title = "Text of tooltip.")
css <- "
.tooltip {
  pointer-events: none;
}
.tooltip > .tooltip-inner {
  pointer-events: none;
  background-color: #73AD21;
  color: #FFFFFF;
  border: 1px solid green;
  padding: 10px;
  font-size: 25px;
  font-style: italic;
  text-align: justify;
  margin-left: 0;
  max-width: 1000px;
}
.tooltip > .arrow::before {
  border-right-color: #73AD21;
}
"

js <- "
$(function () {
  $('[data-toggle=tooltip]').tooltip()
})
"

#------------------------------------------------------------- Setup header ----
header_ui = function() {

  # Get data from the description file
  desc = read.delim("DESCRIPTION", header = FALSE)

  # Extract and capitalise name
  name = stringr::str_split(desc[1,1], ":")[[1]][2]
  name = toupper(trimws(name))

  # Extract version
  version = gsub("[^0-9.-]", "", desc[3,1])
  header = paste(name, "|", version, sep = " ")
  # bs4Dash::dashboardHeader(title = header)
  bs4Dash::dashboardHeader(
    # add title to main grey bar
    tags$li(shiny::htmlOutput(outputId = "main_title"),
            class = "dropdown",
            style = "list-style-type: none; width: 100%; text-align: center; color: #0255e9;"),

    # add logo in upper left corner
    title = bs4Dash::dashboardBrand(
      title = img(src = "./images/logo-neurolipid-atlas.png",
                  title = "Neurolipid Atlas",
                  height = "60px")
    )
  )
}

#------------------------------------------------------------- Setup footer ----
footer_ui = function() {

  # Get data from the description file
  desc = read.delim("DESCRIPTION", header = FALSE)

  # Extract and capitalise name
  name = stringr::str_split(desc[1,1], ":")[[1]][2]
  name = toupper(trimws(name))

  # Extract version
  version = gsub("[^0-9.-]", "", desc[3,1])
  header = paste(name, "|", version, sep = " ")
  bs4Dash::dashboardFooter(left = header)
}

#------------------------------------------------------------ Setup sidebar ----

sidebar_ui = function() {
  bs4Dash::dashboardSidebar(
    skin = "light",
    bs4Dash::sidebarMenu(
      bs4Dash::menuItem(
        text = "Data",
        tabName = "data",
        icon = shiny::icon("l")
      ),
      bs4Dash::menuItem(
        text = "QC",
        tabName = "qc",
        icon = shiny::icon("q")
      ),
      bs4Dash::menuItem(
        text = "Help",
        icon = shiny::icon("question"),
        tabName = 'help_single_omics'
      ),
      bs4Dash::menuItem(
        text = "About",
        tabName = "about",
        icon = shiny::icon("question")
      ),
      bs4Dash::menuItem(
        text = "iSODA",
        tabName = "iSODA",
        icon = shiny::icon("i")
      )
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(
    waiter::useWaiter(),
    waiter::waiterPreloader(html = waiter::spin_fading_circles()),

    # Detect UI functions
    shinyjs::useShinyjs(),
    shinybrowser::detect(),

    shiny::tags$style(".fa-bars {color:#0255e9}"),

    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "data",
        lipidomics_ui(id = 'mod_exp_1')
      ),
      bs4Dash::tabItem(
        tabName = "qc",
        qc_ui(id = 'mod_qc')
      ),
      bs4Dash::tabItem(
        tabName = "help_single_omics",
        help_single_omics_ui(id = 'mod_help_single_omics')
      ),
      bs4Dash::tabItem(
        tabName = "about",
        about_ui(id = 'mod_about')
      ),
      bs4Dash::tabItem(
        tabName = "iSODA",
        isoda_ui(id = "mod_isoda")
      )
    )
  )
}

#----------------------------------------------------------------------- UI ----
header = header_ui()
footer = footer_ui()
sidebar = sidebar_ui()
body = body_ui()
ui = bs4Dash::dashboardPage(header = header,
                            sidebar = sidebar,
                            body = body,
                            footer = footer,
                            freshTheme = "custom.css",
                            dark = NULL,
                            help = NULL)

#------------------------------------------------------------------- Server ----
server = function(input, output, session) {

  options(shiny.maxRequestSize=300*1024^2)

  module_controler = shiny::reactiveValues(
    r6_exp = shiny::reactiveValues(),
    dims = list(
      x_box = 0.9,
      y_box = 0.72,
      x_plot = 0.8,
      y_plot = 0.69,
      x_plot_full = 0.95,
      y_plot_full = 0.91,
      xpx_total = NULL,
      ypx_total = NULL
    )
  )

  output$main_title <- shiny::renderUI({
    req(!is.null(module_controler$r6_exp$name))

    # show nice title
    HTML(
      paste0(
        "<b>",
        unique(module_controler$r6_exp$tables$raw_meta$experimentTitle),
        "</b>"
      )
    )
  })

  # Single omics modules
  shiny::observe({
    shiny::req(module_controler,
               session)
    print_tm(NULL, "App starting")

    # moved from serv_lipidomics.R to here, app is loaded only once now
    module_controler$xpx_total = shinybrowser::get_width()
    module_controler$ypx_total = shinybrowser::get_height()
    module_controler$xbs = 12
    module_controler$xpx = shinybrowser::get_width()
    module_controler$ypx = shinybrowser::get_height()

    # get the session client data
    client_data <- session$clientData

    # get the url parameter
    # for easy development
    query <- list("experimentId" = NULL)
    query <- shiny::parseQueryString(client_data$url_search)
    # simple sanity check
    if (!is.null(query[["experimentId"]])) {
      print_tm(NULL, paste("experimentId from URL:", query[["experimentId"]]))
      if(!grepl(pattern = "NLA_[0-9]{3}", #"^.{3}_2[1-9][0-9]{4}_[0-9]{2}$",
                x = query[["experimentId"]])) {
        query[["experimentId"]] <- "NLA_005"
      }
    } else {
      # for easy development
      print_tm(NULL, "Default experimentId: NLA_005")
      query[["experimentId"]] <- "NLA_005" # "VDK_220223_01"
    }
    experiment_id = query[["experimentId"]]

    if(!is.null(query[["experimentId"]])) {
      # Create lipidomics r6 object
      module_controler$r6_exp = example_lipidomics(name = "Lips_1",
                                                   id = NA,
                                                   slot = "exp_1",
                                                   experiment_id = experiment_id)

      # lipidomics server
      lipidomics_server(id = "mod_exp_1",
                        module_controler = shiny::isolate(module_controler))
                        # sheet_id = sheet_id)
      # QC
      qc_server(id = "mod_qc",
                module_controler = shiny::isolate(module_controler))
    }
  })

  # help
  about_server(id = 'mod_about', main_output = output)
}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
