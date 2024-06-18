############################################################# Single omics ####

#----------------------------------------------------- Help Single omics UI ----
help_single_omics_ui = function(id){
  ns = shiny::NS(id)

  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Data preprocessing",
      shiny::includeMarkdown("./man/preprocessing.md")
    ),
    shiny::tabPanel(
      title = "Lipid classes",
      shiny::includeMarkdown("./man/lipidclasses.md")
    ),
    shiny::tabPanel(
      title = "Visualization",
      shiny::includeMarkdown("./man/visualization.md")
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

