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

#-------------------------------------------------------- Tool tip settings ----
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
        text = "About",
        tabName = "about",
        icon = shiny::icon("question")
      )
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(

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
        tabName = "about",
        about_ui(id = 'mod_about')
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
# ui = shinymanager::secure_app(bs4Dash::dashboardPage(header, sidebar, body))
#------------------------------------------------------------------- Server ----

server = function(input, output, session) {

  options(shiny.maxRequestSize=300*1024^2)

  module_controler = shiny::reactiveValues(

    r6_exp = NULL,

    dims = list(
      x_box = 0.9,
      y_box = 0.70,
      x_plot = 0.8,
      y_plot = 0.67,
      x_plot_full = 0.95,
      y_plot_full = 0.91,
      xpx_total = NULL,
      ypx_total = NULL
    )
  )

  # get client data, can not access url_search here, it is a reactive value
  client_data <- session$clientData

  # read the master database file
  db_data <- as.data.frame(readxl::read_xlsx(path = "./data/Database/SampleMasterfile.xlsx",
                                             sheet = 1))

  # Single omics modules
  shiny::observe({
    req(client_data,
        db_data)

    print("Rico: app starting")

    # get the url parameter
    # for easy development
    query <- list("experimentId" = NULL)
    query <- shiny::parseQueryString(client_data$url_search)
    # simple sanity check
    if (!is.null(query[["experimentId"]])) {
      print_tm(NULL, paste("experimentId from URL:", query[["experimentId"]]))
      if(!grepl(pattern = "^.{3}_2[1-9][0-9]{4}_[0-9]{2}$",
                x = query[["experimentId"]])) {
        query[["experimentId"]] <- NULL
      }
    } else {
      # for easy development
      query[["experimentId"]] <- "VDK_220223_01"
    }
    experiment_id = query[["experimentId"]]

    print(paste("Rico: experimentId:", query[["experimentId"]]))

    if(!is.null(query[["experimentId"]])) {
      # get the batches for the samples belonging to the experiment
      data_files = unique(db_data$batchNumber[db_data$experimentId == query[["experimentId"]]])
      data_files = data_files[!is.na(data_files)]

      # Create lipidomics r6 object
      module_controler$r6_exp = example_lipidomics(name = "Lips_1",
                                                   id = id,
                                                   slot = "exp_1",
                                                   experiment_id = experiment_id)

      # server stuff is created here, should the data be passed here?
      lipidomics_server(id = "mod_exp_1",
                        module_controler = module_controler)

      # QC
      qc_server(id = "mod_qc",
                module_controler = module_controler)
    }
  })

  # help
  about_server(id = 'mod_about', main_output = output)
}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
