
#----------------------------------------------------------------- iSODA UI ----
isoda_ui = function(id){
  ns = shiny::NS(id)

  shiny::includeMarkdown("./man/isoda.md")

}

#------------------------------------------------------------- iSODA server ----

isoda_server = function(id, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

    }
  )
}

