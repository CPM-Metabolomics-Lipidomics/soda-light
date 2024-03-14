############################################################# Single omics ####

#----------------------------------------------------- Help Single omics UI ----
help_single_omics_ui = function(id){
  ns = shiny::NS(id)

  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Visualization",
      shiny::includeMarkdown("./man/single_omics/visualization.md")
    )
  )
}

#------------------------------------------------- Help Single omics server ----

help_single_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

