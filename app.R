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
# library(googledrive)
# library(googlesheets4)

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
    bs4Dash::sidebarMenuOutput(outputId = "render_menu"),
    shiny::column(
      width = 12,
      br(),
      br(),
      br(),
      p(strong("Tip: "), "click", shiny::icon("gears"), " icon to change the visualization settings!",
        style = "color:#0255e9;")
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

    shiny::uiOutput(outputId = "render_tabs")
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

  options(shiny.maxRequestSize = 300 * 1024^2)

  # module_controler = shiny::reactiveValues(
  #   r6_exp = shiny::reactiveValues(),
  #   dims = list(
  #     x_box = 0.9,
  #     y_box = 0.72,
  #     x_plot = 0.8,
  #     y_plot = 0.69,
  #     x_plot_full = 0.95,
  #     y_plot_full = 0.91,
  #     xpx_total = NULL,
  #     ypx_total = NULL
  #   )
  # )

  # output$main_title <- shiny::renderUI({
  #   req(!is.null(module_controler$r6_exp$name))
  #
  #   # show nice title
  #   HTML(
  #     paste0(
  #       "<b>",
  #       unique(module_controler$r6_exp$tables$raw_meta$experimentTitle),
  #       "</b>"
  #     )
  #   )
  # })

  controler <- shiny::reactive({
    mod_controler <- list(
      r6_exp = NULL,
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

    return(mod_controler)
  })

  experimentId <- shiny::reactive({
    query <- list("experimentId" = NULL)
    client_data <- session$clientData
    query <- shiny::parseQueryString(client_data$url_search)

    # simple sanity check
    if (!is.null(query[["experimentId"]])) {
      print_tm(NULL, paste("experimentId from URL:", query[["experimentId"]]))
      if(!grepl(pattern = "NLA_[0-9]{3}",
                x = query[["experimentId"]])) {
        query[["experimentId"]] <- "NLA_005"
      }
    } else {
      # for easy development
      print_tm(NULL, "Default experimentId: NLA_005")
      query[["experimentId"]] <- "NLA_005" # "VDK_220223_01"
    }

    return(query)
  })

  # Single omics modules
  shiny::observe({
    # shiny::req(module_controler,
    #            session)
    print_tm(NULL, "App starting")

    # moved from serv_lipidomics.R to here, app is loaded only once now
    # module_controler$xpx_total = shinybrowser::get_width()
    # module_controler$ypx_total = shinybrowser::get_height()
    # module_controler$xbs = 12
    # module_controler$xpx = shinybrowser::get_width()
    # module_controler$ypx = shinybrowser::get_height()

    # module_controler1$xpx_total = shinybrowser::get_width()
    # module_controler1$ypx_total = shinybrowser::get_height()
    # module_controler1$xbs = 12
    # module_controler1$xpx = shinybrowser::get_width()
    # module_controler1$ypx = shinybrowser::get_height()

    # get the experiment id
    # experiment_id = experimentId()

    output$render_tabs <- shiny::renderUI({
      default_tabs <- list(
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

      data_tabs <- lapply(1:3, function(x) {
        bs4Dash::tabItem(
          tabName = paste0("data", x),
          lipidomics_ui(id = paste0("mod_exp_", x))
        )
      })
      all_tabs <- c(data_tabs, default_tabs)

      do.call(bs4Dash::tabItems, all_tabs)
    })


    output$render_menu <- bs4Dash::renderMenu({
      sub_items_list <- lapply(1:3, function(x) {
        bs4Dash::menuSubItem(
          text = paste0("Data ", x),
          tabName = paste0("data", x)
        )
      })

      shiny::tagList(
        bs4Dash::sidebarMenu(
          bs4Dash::menuItem(
            .list = sub_items_list,
            text = "Data",
            icon = shiny::icon("l"),
            startExpanded = TRUE
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
            selected = TRUE,
            icon = shiny::icon("question")
          ),
          bs4Dash::menuItem(
            text = "iSODA",
            tabName = "iSODA",
            icon = shiny::icon("i")
          )
        )
      )
    })


    # one time working
    experimentId <- c("NLA_005", "NLA_007")
    for(a in 1:length(experimentId)) {
      module_controler <- controler()
      print_tm(NULL, paste0("Experiment: ", experimentId[a]))
      # Create lipidomics r6 object
      module_controler$r6_exp = example_lipidomics(name = paste0("Lips_", a),
                                                   id = NA,
                                                   slot = paste0("exp_", a),
                                                   experiment_id = experimentId[a])
      # lipidomics server
      lipidomics_server(id = paste0("mod_exp_", a),
                        module_controler = shiny::isolate(module_controler))


      # QC
      # qc_server(id = "mod_qc",
      #           module_controler = shiny::isolate(module_controler))
    }
  })

  # help
  about_server(id = 'mod_about', main_output = output)
}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
