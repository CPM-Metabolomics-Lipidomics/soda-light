# shiny app
library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
library(shinymanager)

# Plotting
library(ggplot2)
library(gridExtra)
library(plotly)
library(visNetwork)
library(heatmaply)
library(ggpubr)
library(ggupset)
library(networkD3)
library(igraph)

# text
library(stringr)

# Tables
library(DT)
library(readxl)

# Colors
library(grDevices)
library(RColorBrewer)

# Statistics
library(stats)
library(glmnet)
library(pcaMethods)

# Omics
library(org.Hs.eg.db)
library(clusterProfiler)
library(enrichplot)
library(ggridges)
library(MOFA2)
library(basilisk)
library(SNFtool)

# General
library(reshape2)
library(dplyr)

# reticulate::use_condaenv(condaenv = 'mofa_1')



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
  bs4Dash::dashboardHeader(title = header)
}

#------------------------------------------------------------ Setup sidebar ----

sidebar_ui = function() {
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(

      bs4Dash::menuItem(
        text = "Data",
        tabName = "data",
        icon = shiny::icon("l")
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

    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "data",
        lipidomics_ui(id = 'mod_exp_1')
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
sidebar = sidebar_ui()
body = body_ui()
ui = bs4Dash::dashboardPage(header, sidebar, body)
# ui = shinymanager::secure_app(bs4Dash::dashboardPage(header, sidebar, body))
#------------------------------------------------------------------- Server ----

server = function(input, output, session) {

  # Basic authentification
  # res_auth = shinymanager::secure_server(
  #   check_credentials = shinymanager::check_credentials(db = data.frame(
  #     user = c("user1", "user2"),
  #     password = c("1234", "monkey"),
  #     admin = c(FALSE, FALSE))
  #   )
  # )

  options(shiny.maxRequestSize=300*1024^2)

  module_controler = shiny::reactiveValues(
    dims = list(
      x_box = 0.9,
      y_box = 0.75,
      x_plot = 0.8,
      y_plot = 0.70,
      x_plot_full = 0.95,
      y_plot_full = 0.91,
      xpx_total = NULL,
      ypx_total = NULL
    )
  )

  # help
  about_server(id = 'mod_about', main_output = output)

  # read the master database file
  db_data <- as.data.frame(readxl::read_xlsx(path = "./data/Database/SampleMasterfile.xlsx",
                                             sheet = 1))
  # Single omics modules
  # this doesn't stop executing
  shiny::observe({
    query <- list("experimentId" = NULL)
    print("Rico: app starting")

    print("Rico: get URL parameter")
    # get the url parameter
    query <- parseQueryString(session$clientData$url_search)
    # simple sanity check
    if (!is.null(query[["experimentId"]])) {
      print_tm(NULL, paste("experimentId from URL:", query[["experimentId"]]))
      if(!grepl(pattern = "^VDK_2[123][0-9]{4}_[0-9]{2}$",
                x = query[["experimentId"]])) {
        query[["experimentId"]] <- NULL
      }
    } else {
      query[["experimentId"]] <- "VDK_230228_02"
    }
    print(paste("Rico: experimentId", query[["experimentId"]]))

    if(!is.null(query[["experimentId"]])) {
      # get the batches for the samples belonging to the experiment
      data_files = unique(db_data$batchNumber[db_data$experimentId == query[["experimentId"]]])
      data_files = data_files[!is.na(data_files)]

      # server stuff is created here, should the data be passed here?
      lipidomics_server(id = "mod_exp_1",
                        data_files = data_files,
                        experiment_id = query[["experimentId"]],
                        module_controler = module_controler)
    }
  })


}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
