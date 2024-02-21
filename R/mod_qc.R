
#----------------------------------------------------------------- QC UI ----
qc_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidRow(
    shiny::p("QC page wil be shown here!")
  )
}

#----------------------------------------------------------- QC server ----

qc_server = function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      print("Rico: qc started")
    }
  )
}

