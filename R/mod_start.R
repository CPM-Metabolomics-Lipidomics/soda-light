
#----------------------------------------------------------------- Start UI ----
start_ui = function(id){
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::h1('SODA - Simple Omics Data Analysis')
    ),
    shiny::fluidRow(
      shiny::column(
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        width = 6,
        shiny::h3('Start by creating some experiments:'),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = ns('exp_name'),
              label = 'Exp. name',
              placeholder = 'lips_1',
              width = '100%'
            ),
            shiny::selectInput(
              inputId = ns('exp_select'),
              label = 'Select a study',
              choices = 'Select a study',
              width = '100%'
            )
          ),
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns('del_exp'),
              label = 'Delete exp.',
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = '100%'
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyWidgets::actionBttn(
              inputId = ns('add_exp'),
              label = "Add exp.",
              style = "material-flat",
              color = 'success',
              block = T,
              icon = icon("check")
            )
          ),
          shiny::column(
            width = 6,
            shinyWidgets::actionBttn(
              inputId = ns('remove_exp'),
              label = 'Remove exp.',
              style = "material-flat",
              color = 'danger',
              block = T,
              icon = icon("x")
            )
          )
        )
      ),
      shiny::column(
        width = 1
      ),
      shiny::column(
        width = 5,
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::htmlOutput(outputId = ns("db_info"))
      )
    )
  )
}

#------------------------------------------------------------- Start server ----

start_server = function(id, main_input, main_output, main_session, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # read the master database file
      db_data <- readxl::read_xlsx(path = "./data/databaseInfo.xlsx",
                                   sheet = 1)

      # update the study select
      shiny::observe({
        req(db_data)

        shiny::updateSelectInput(inputId = "exp_select",
                                 choices = c("Select a study",
                                             db_data$DataSetName))
      })

      # show the study information
      output$db_info <- shiny::renderUI({
        req(db_data,
            input$exp_select)

        exp_select <- input$exp_select

        if(exp_select == "Select a study") {
          "Please select a study!"
        } else {
          db_data$Information[db_data$DataSetName == exp_select]
        }
      })

      shiny::observeEvent(input$exp_select, {
        select_exp <- input$exp_select

        if(select_exp == "Select a study") {
          shinyjs::disable("add_exp")
        } else {
          shinyjs::enable("add_exp")
        }
      })

      # Create experiments
      shiny::observeEvent(input$add_exp,{
        exp_name = input$exp_name

        if (exp_name %in% unname(unlist(module_controler$exp_names))) {
          print_t('ERROR: experiment already exists.')
          return()
        }

        if (exp_name == '') {
          exp_name = "lips" #experiment_switch(input$exp_type)
          counter = 1
          while (paste0(exp_name, '_', counter) %in% unname(unlist(module_controler$exp_names))) {
            counter = counter + 1
          }
          exp_name = paste0(exp_name, '_', counter)
        }

        if (!grepl("^[a-zA-Z0-9_]+$", exp_name)) {
          print_t('ERROR: only alphanumeric and underscores accepted')
          return()
        }

        slot  = names(module_controler$slot_taken)[!sapply(module_controler$slot_taken, base::isTRUE)][1]
        exp_type = "Lipidomics" # input$exp_type
        main_output[[slot]] = bs4Dash::renderMenu({
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem(text = exp_name,
                              tabName = slot,
                              icon = icon(tolower(substr(exp_type, 1, 1))))
          )
        })

        module_controler$slot_taken[[slot]] = TRUE
        module_controler$exp_names[[slot]] = exp_name
        module_controler$exp_types[[slot]] = exp_type
        module_controler$exp_r6[[slot]] = r6_switch(exp_type = exp_type,
                                                    name = exp_name,
                                                    id = paste0('mod_', slot),
                                                    slot = slot,
                                                    data_file = db_data$DataFileName[db_data$DataSetName == input$exp_select],
                                                    meta_file = db_data$MetaFileName[db_data$DataSetName == input$exp_select])

        if (sum(sapply(module_controler$slot_taken, base::isTRUE)) >= 6) {
          shinyjs::disable("add_exp")
        }

        print_t(paste0('Added ', input$exp_name, ' (', exp_type, ')'))

        created_modules = unname(unlist(module_controler$exp_names))
        created_modules = created_modules[!is.na(created_modules)]
        shiny::updateSelectInput(
          inputId = 'del_exp',
          choices = created_modules
        )

        shiny::updateTextInput(
          inputId = 'exp_name',
          value = character(0)
        )

      })

      shiny::observeEvent(input$remove_exp, {
        shiny::req(input$del_exp)

        for (mod in input$del_exp) {
          print_t(paste0('Removing ', mod))
          exp_id = names(which(module_controler$exp_names == mod))[1]
          purge_module_inputs(id = exp_id, input_object = main_input)
          events = names(session$userData[[paste0('mod_', exp_id)]])
          for (e in events) {
            session$userData[[paste0('mod_', exp_id)]][[e]]$destroy()
          }
          main_output[[exp_id]] = NULL
          module_controler$slot_taken[[exp_id]] = FALSE
          module_controler$module_loaded[[exp_id]] = FALSE
          module_controler$exp_types[exp_id] = list(NULL)
          module_controler$exp_names[exp_id] = list(NULL)
          module_controler$exp_r6[exp_id] = list(NULL)
        }

        shiny::updateSelectInput(
          inputId = 'del_exp',
          selected = character(0),
          choices = unname(unlist(module_controler$exp_names))
        )

        shinyjs::enable('add_exp')
      })
    }
  )
}

