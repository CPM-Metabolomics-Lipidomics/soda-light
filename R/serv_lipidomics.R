#----------------------------------------------------- Lipidomics utilities ----

plotbox_switch_ui_lips = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_class_distribution" = class_distribution_ui,
                                          "select_class_comparison" = class_comparison_ui,
                                          "select_volcano_plot" = volcano_plot_ui,
                                          "select_heatmap" = heatmap_ui,
                                          "select_pca" = pca_ui,
                                          "select_fa_analysis_plot" = fa_analysis_ui,
                                          "select_fa_composition" = fa_comp_ui
    )
    )
  }
  return(ui_functions)
}

plotbox_switch_server_lips = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_class_distribution" = class_distribution_server,
                                                  "select_class_comparison" = class_comparison_server,
                                                  "select_volcano_plot" = volcano_plot_server,
                                                  "select_heatmap" = heatmap_server,
                                                  "select_pca" = pca_server,
                                                  "select_fa_analysis_plot" = fa_analysis_server,
                                                  "select_fa_composition" = fa_comp_server
    )
    )
  }
  return(server_functions)
}

plot_one_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, input, output, session)
  }
}


plot_two_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, input, output, session)
  }
}

plot_three_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, input, output, session)
  }
}

plot_four_lips = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui_lips(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server_lips(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, input, output, session)
  }
}


reset_sample_filters = function(input, session, r6) {
  # Set all checkboxes to False
  shinyWidgets::updateCheckboxGroupButtons(
    session = session,
    inputId = "non_samples_selection",
    selected = character(0)
  )

  # Set manual row selection to None and update
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$meta_filtered),
    selected = character(0)
  )


  # Set the metacolumn value to None and update
  shiny::updateSelectInput(
    session = session,
    inputId = "exclusion_meta_val",
    choices = unique(r6$tables$meta_filtered[,input$exclusion_meta_col]),
    selected = character(0)
  )

  # Set metadata row exclusion to None
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exclusion_meta_row",
    selected = character(0)
  )
}

update_sample_filters = function(input, session, r6) {
  # Update input for the manual exclusion
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$raw_meta)
  )

  # Update available groups to filter
  if (input$exclusion_meta_col != "") {
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  }
}

update_lipid_filters = function(input, session, r6, prog_bars = T) {
  # Update class selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "class_selection",
    choices = unique(r6$tables$feature_table$lipid_class),
    selected = character(0)
  )

  # Update manual selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "manual_selection",
    # choices = remaining_cols,
    choices = colnames(r6$tables$raw_data),
    selected = character(0)
  )

  if (prog_bars) {
    shinyWidgets::updateProgressBar(
      session = session,
      id = "col_count_bar",
      value = ncol(r6$tables$raw_data),
      total = ncol(r6$tables$imp_data)
    )

    shinyWidgets::updateProgressBar(
      session = session,
      id = "row_count_bar_data",
      value = nrow(r6$tables$raw_data),
      total = nrow(r6$tables$imp_data)
    )
  }


}

sample_row_selection = function(input, r6) {
  # Initialise selection
  selected_rows = c()

  # Get blank rows
  if ("Blanks" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_blanks)
  }

  # Get QC rows
  if ("QCs" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_qcs)
  }

  # Get Pool rows
  if ("Pools" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_pools)
  }

  # Add metadata and manual exclusions
  selected_rows = c(selected_rows,input$exclusion_meta_row,input$selection_manual)
  selected_rows = sort(unique(selected_rows))

  return(selected_rows)
}

#-------------------------------------------------------- Lipidomics ui ----
lipidomics_ui = function(id) {
  ns = shiny::NS(id)

  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Info",
      # download form
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::HTML(paste0("<p>Several tables are available for download. Select a table below and click <i>Download table</i> to download the selected table.<br>",
                             "<b>Note:</b> The imported tables are unfiltered and the other tables are filtered!</p>")),
          shiny::selectInput(
            inputId = ns("info_download_table_select"),
            label = "Select table:",
            choices = list(
              "Data tables" = list(
                "Imported data table" = "Imported data table",
                "Raw data table" = "Raw data table",
                "Total normalized data table" = "Total normalized table"),
              "Meta data tables" = list(
                "Imported meta data table" = "Imported metadata table",
                "Raw meta data table" = "Raw metadata table")
            )
          ),
          shiny::downloadButton(
            outputId = ns("info_download_data"),
            label = "Download table"
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::hr(style = "border-top: 1px solid #7d7d7d; width: 75%;"),
          bs4Dash::tabsetPanel(
            shiny::tabPanel(
              title = "Experiment info",
              DT::dataTableOutput(
                outputId = ns("info_experiment_table")
              )
            ),
            shiny::tabPanel(
              title = "Meta data",
              DT::dataTableOutput(
                outputId = ns("info_metadata_table")
              )
            ),
            shiny::tabPanel(
              title = "Data overview",
              p("An overview of the totals per lipid class and sample."),
              DT::dataTableOutput(
                outputId = ns("info_sum_table")
              )
            )
          )

        )
      )
    ),
    shiny::tabPanel(
      title = "Visualize data",
      # shiny::uiOutput(
      #   outputId = ns('visualize_data_ui')
      shiny::fluidRow(
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                             label = NULL,
                                             status = "info",
                                             choices = lipidomics_plot_list(),
                                             selected = "select_class_distribution",
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE
          )
        ),
        shiny::column(
          width = 1,
          bs4Dash::actionButton(inputId = ns("clear_plots"),
                                label = "Clear",
                                status = "danger",
                                icon = icon("x"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(
            outputId = ns("plotbox_field")
          )
        )
      )
    ),
    selected = "Visualize data"
  )
}

#-------------------------------------------------------- Lipidomics server ----

lipidomics_server = function(id, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      # Get lipidomics r6 object
      r6 = module_controler$r6_exp
      m = "Lips_1"

      #------------------------------------------------- Metadata upload server ----
      # Preview all / subset switch
      session$userData[[id]]$select_meta_table = shiny::observeEvent(input$select_meta_table, {
        shiny::req(r6$tables$imp_meta)

        data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
        output$metadata_preview_table = renderDataTable({
          DT::datatable(data_table, options = list(paging = TRUE))
        })
      })

      # Get ID
      session$userData[[id]]$id_select_meta = shiny::observeEvent(input$select_id_meta, {
        shiny::req(r6$tables$imp_meta,
                   input$select_id_meta)
        if (r6$preloaded_data) {return()}
        print_tm(m, 'Meta - Setting ID column')

        if (length(r6$tables$imp_meta[,input$select_id_meta]) == length(unique(r6$tables$imp_meta[,input$select_id_meta]))) {

          r6$indices$id_col_meta = input$select_id_meta
          r6$set_raw_meta()
          update_sample_filters(input = input, session = session, r6 = r6)

          # Update metadata column input
          shiny::updateSelectInput(
            session = session,
            inputId = "exclusion_meta_col",
            choices = colnames(r6$tables$raw_meta)
          )

          shiny::updateSelectInput(
            inputId = 'select_meta_table',
            choices = c('Imported metadata table', 'Raw metadata table'),
            selected = 'Raw metadata table'
          )
        } else {
          print_tm(m, 'ERROR: Non-unique IDs in ID column')
          r6$tables$raw_meta = NULL
          shiny::updateSelectInput(
            inputId = 'select_meta_table',
            choices = c('Imported metadata table'),
            selected = 'Imported metadata table'
          )
        }
      })

      # Group col selection
      session$userData[[id]]$select_group_col = shiny::observeEvent(
        c(input$select_group_col,
          input$selection_keep,
          input$selection_drop,
          input$selection_keep,
          input$reset_meta), {
          shiny::req(r6$tables$raw_meta)

          print_tm(m, 'Meta - Setting group column')

          data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
          r6$indices$group_col = input$select_group_col
          groups = unique_na_rm(r6$tables$imp_meta[, input$select_group_col])
          freq = data.frame(table(base::factor(na.omit(data_table[, input$select_group_col]), levels = groups)))
          names(freq) = c("value", "count")

          output$group_distribution_preview = shiny::renderPlot(
            ggplot2::ggplot(data = freq, aes(x = value, y = count)) +
              geom_bar(stat = "identity", fill="blue")+
              geom_text(aes(label=count), vjust=0.5, hjust = -0.5, size=6)+
              xlab(NULL) +
              ylab(NULL) +
              ylim(0,max(freq$count)+10) +
              theme_minimal() +
              coord_flip() +
              labs(title = 'Groups distribution')+
              theme(
                plot.title = element_text(size=17, hjust = 0.5),
                axis.text.x = element_text(size = 15),
                axis.text.y = element_text(size = 15)
              )
          )
        })

      # Batch col selection
      session$userData[[id]]$select_batch_col = shiny::observeEvent(input$select_batch_col, {
        shiny::req(r6$tables$imp_meta,
                   input$select_batch_col)

        print_tm(m, 'Meta - Setting batch column')
        r6$indices$batch_col = input$select_batch_col
      })

      # Type col selection
      session$userData[[id]]$select_type_col = shiny::observeEvent(
        c(input$select_type_col,
          input$blank_pattern,
          input$qc_pattern,
          input$pool_pattern,
          input$select_id_meta), {
            shiny::req(c(r6$tables$imp_meta,
                         input$blank_pattern,
                         input$qc_pattern,
                         input$pool_pattern))

            print_tm(m, 'Meta - Setting type column')
            if ((input$select_type_col != "") & (!is.null(input$blank_pattern)) & (!is.null(input$qc_pattern)) & (!is.null(input$pool_pattern))) {
              r6$indices$type_col = input$select_type_col
              type_vector = r6$tables$imp_meta[, input$select_type_col]
              blank_idx = grep(pattern = input$blank_pattern,
                               x = type_vector,
                               ignore.case = TRUE)
              qc_idx = grep(pattern = input$qc_pattern,
                            x = type_vector,
                            ignore.case = TRUE)
              pool_idx = grep(pattern = input$pool_pattern,
                              x = type_vector,
                              ignore.case = TRUE)

              sample_idx = 1:nrow(r6$tables$imp_meta)
              sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

              r6$indices$idx_blanks = blank_idx
              r6$indices$idx_qcs = qc_idx
              r6$indices$idx_pools = pool_idx
              r6$indices$idx_samples = sample_idx

              r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, input$select_id_meta]
              r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, input$select_id_meta]
              r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, input$select_id_meta]
              r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, input$select_id_meta]
            }
          })

      # Type col selection
      session$userData[[id]]$select_type_col = shiny::observeEvent(
        c(input$select_type_col,
          input$blank_pattern,
          input$qc_pattern,
          input$pool_pattern,
          input$select_id_meta,
          input$select_meta_table,
          input$selection_drop,
          input$selection_keep,
          input$reset_meta), {
            shiny::req(r6$tables$raw_meta,
                       input$select_type_col)

            print_tm(m, 'Meta - Updating type plot.')

            data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
            if (input$select_meta_table == 'Imported metadata table') {
              blank_idx = intersect(rownames(data_table), r6$indices$idx_blanks)
              qc_idx = intersect(rownames(data_table), r6$indices$idx_qcs)
              pool_idx = intersect(rownames(data_table), r6$indices$idx_pools)
              sample_idx = intersect(rownames(data_table), r6$indices$idx_samples)
            } else {
              blank_idx = intersect(rownames(data_table), r6$indices$rownames_blanks)
              qc_idx = intersect(rownames(data_table), r6$indices$rownames_qcs)
              pool_idx = intersect(rownames(data_table), r6$indices$rownames_pools)
              sample_idx = intersect(rownames(data_table), r6$indices$rownames_samples)
            }

            data_table[, input$select_type_col] = 'Samples'
            data_table[blank_idx, input$select_type_col] = 'Blanks'
            data_table[qc_idx, input$select_type_col] = 'QCs'
            data_table[pool_idx, input$select_type_col] = 'Pools'

            freq_table = data.frame(table(base::factor(data_table[, input$select_type_col], levels = c('Pools', 'QCs', 'Blanks', 'Samples'))))
            names(freq_table) = c("value", "count")

            output$type_distribution_preview = shiny::renderPlot(
              ggplot2::ggplot(data = freq_table, aes(x = value, y = count)) +
                geom_bar(stat = "identity", fill="blue")+
                geom_text(aes(label=count), vjust=-0.5, hjust = 0.5, size=6)+
                xlab(NULL) +
                ylab(NULL) +
                ylim(0,max(freq_table$count)+10) +
                theme_minimal() +
                labs(title = 'Type distribution')+
                theme(
                  plot.title = element_text(size=17, hjust = 0.5),
                  axis.text.x = element_text(size = 15),
                  axis.text.y = element_text(size = 15)
                )
            )
          })

      # Update the metadata value once a metadata column is selected
      session$userData[[id]]$exclusion_meta_col = shiny::observeEvent(c(input$exclusion_meta_col),{
        if(input$exclusion_meta_col != "") {
          shiny::updateSelectInput(
            session = session,
            inputId = "exclusion_meta_val",
            choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
            selected = character(0)
          )
        }
      })


      # Update the rows to filter once a metadata value is selected
      session$userData[[id]]$exclusion_meta_val = shiny::observeEvent(c(input$exclusion_meta_val),{
        if (!is.null(input$exclusion_meta_val)) {
          bool_vector = c()
          for (value in input$exclusion_meta_val) {
            bool_vector[[length(bool_vector) + 1]] = r6$tables$raw_meta[,input$exclusion_meta_col] == value
          }
          bool_vector = Reduce("|", bool_vector)

          shiny::updateSelectizeInput(
            session = session,
            inputId = "exclusion_meta_row",
            choices = rownames(r6$tables$raw_meta)[bool_vector],
            selected = rownames(r6$tables$raw_meta)[bool_vector]
          )
        }
      })

      # Clear button
      session$userData[[id]]$clear_filters = shiny::observeEvent(input$clear_filters, {
        print_tm(m, 'Clearing metadata filters')
        reset_sample_filters(input = input, session = session, r6 = r6)
      })

      # Reset button
      session$userData[[id]]$reset_meta = shiny::observeEvent(input$reset_meta, {
        print_tm(m, 'Reseting metadata table')

        r6$set_raw_meta()
        update_sample_filters(input = input, session = session, r6 = r6)
      })

      # Drop button
      session$userData[[id]]$selection_drop = shiny::observeEvent(input$selection_drop, {
        print_tm(m, 'Dropping selected samples')

        selected_rows = sample_row_selection(input = input, r6 = r6)
        if (!is.null(selected_rows)){
          r6$tables$raw_meta = drop_rows(data_table = r6$tables$raw_meta,
                                         rows = selected_rows)
        }
        reset_sample_filters(input = input, session = session, r6 = r6)
        update_sample_filters(input = input, session = session, r6 = r6)
      })

      # Keep button
      session$userData[[id]]$selection_keep = shiny::observeEvent(input$selection_keep, {
        print_tm(m, 'Keeping selected samples')

        selected_rows = sample_row_selection(input = input, r6 = r6)
        if (!is.null(selected_rows)){
          r6$tables$raw_meta = keep_rows(data_table = r6$tables$raw_meta,
                                         rows = selected_rows)
        }
        reset_sample_filters(input = input, session = session, r6 = r6)
        update_sample_filters(input = input, session = session, r6 = r6)
      })

      # Row count progress bar
      session$userData[[id]]$row_count_bar_meta = shiny::observeEvent(
        c(input$selection_keep,
          input$selection_drop,
          input$reset_meta,
          input$select_meta_table), {
            data_table = table_switch(table_name = input$select_meta_table, r6 = r6)

            shinyWidgets::updateProgressBar(
              session = session,
              id = "row_count_bar_meta",
              value = nrow(data_table),
              total = nrow(r6$tables$imp_meta)
            )
          })

      # Download meta table
      dl_meta_table = shiny::reactiveValues(
        name = NULL,
        table = NULL
      )

      session$userData[[id]]$download_metatable = shiny::observeEvent(c(input$select_meta_table, input$reset_meta, input$selection_keep, input$selection_drop) , {
        shiny::req(r6$tables$raw_meta)
        dl_meta_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_meta_table, " ", "_"), ".csv"))
        dl_meta_table$table = table_switch(input$select_meta_table, r6)
      })

      #----------------------------------------------------- Data upload server ----
      # Preview all / subset switch
      session$userData[[id]]$select_data_table = shiny::observeEvent(input$select_data_table, {
        shiny::req(r6$tables$imp_data)

        data_table = table_switch(table_name = input$select_data_table, r6 = r6)

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {

          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(data_table),
            total = ncol(r6$tables$imp_data)
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(data_table),
            total = nrow(r6$tables$imp_data)
          )

          output$lipid_class_summary = shiny::renderPlot(
            lipidomics_summary_plot(r6, data_table)
          )

        }

        output$data_preview_table = renderDataTable({
          DT::datatable(data_table, options = list(paging = TRUE))
        })

      })

      # Preview all / subset switch
      session$userData[[id]]$select_data_table = shiny::observeEvent(input$select_data_table, {
        shiny::req(r6$tables$imp_data)

        data_table = table_switch(table_name = input$select_data_table, r6 = r6)

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(data_table),
            total = ncol(r6$tables$imp_data)
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(data_table),
            total = nrow(r6$tables$imp_data)
          )

          output$lipid_class_summary = shiny::renderPlot(
            lipidomics_summary_plot(r6, data_table)
          )

        }

        output$data_preview_table = renderDataTable({
          DT::datatable(data_table, options = list(paging = TRUE))
        })

      })

      # Feature filters
      session$userData[[id]]$row_col_data = shiny::observeEvent(
        c(input$apply_imputation,
          input$impute_before,
          input$na_imputation,
          input$imputation_min_values,
          input$apply_filtering,
          input$blank_multiplier,
          input$sample_threshold,
          input$group_threshold,
          input$normalise_to_col,
          input$reset_data_table,
          input$selection_drop,
          input$selection_keep,
          input$reset_meta), {

            shiny::req(r6$tables$raw_data)
            # if (r6$preloaded_data) {return()}
            print_tm(m, 'Data - Updating data tables')
            r6$set_raw_data(apply_imputation = input$apply_imputation,
                            impute_before = input$impute_before,
                            apply_filtering = input$apply_filtering,
                            imputation_function = input$na_imputation,
                            val_threshold = as.numeric(input$imputation_min_values),
                            blank_multiplier = as.numeric(input$blank_multiplier),
                            sample_threshold = as.numeric(input$sample_threshold),
                            group_threshold = as.numeric(input$group_threshold),
                            norm_col = input$normalise_to_col)

            r6$derive_data_tables()

            if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {
              data_table = table_switch(input$select_data_table, r6)
              output$lipid_class_summary = shiny::renderPlot(
                lipidomics_summary_plot(r6, data_table)
              )
            }


            # Update class selection
            shiny::updateSelectizeInput(
              session = session,
              inputId = "class_selection",
              choices = unique(r6$tables$feature_table$lipid_class),
              selected = character(0)
            )

            # Update manual selection
            shiny::updateSelectizeInput(
              session = session,
              inputId = "manual_selection",
              choices = colnames(r6$tables$raw_data),
              selected = character(0)
            )

            data_table = table_switch(table_name = input$select_data_table, r6 = r6)

            if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


              shinyWidgets::updateProgressBar(
                session = session,
                id = "col_count_bar",
                value = ncol(data_table),
                total = ncol(r6$tables$imp_data)
              )

              shinyWidgets::updateProgressBar(
                session = session,
                id = "row_count_bar_data",
                value = nrow(data_table),
                total = nrow(r6$tables$imp_data)
              )
            }
            output$data_preview_table = renderDataTable({
              DT::datatable(data_table, options = list(paging = TRUE))
            })
          })

      # Drop features
      session$userData[[id]]$feature_drop = shiny::observeEvent(input$drop_cols,{
        shiny::req(r6$tables$feature_table)
        print_tm(m, 'Data - Dropping features')
        selected_species = rownames(r6$tables$feature_table)[which(r6$tables$feature_table$lipid_class %in% input$class_selection)]
        selected_species = unique(c(selected_species, input$manual_selection))
        r6$tables$raw_data = drop_cols(data_table = r6$tables$raw_data, cols = selected_species)
        r6$indices$excluded_cols = c(r6$indices$excluded_cols, selected_species)

        r6$derive_data_tables()

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {
          data_table = table_switch(input$select_data_table, r6)
          output$lipid_class_summary = shiny::renderPlot(
            lipidomics_summary_plot(r6, data_table)
          )
        }

        # Update class selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          choices = unique(r6$tables$feature_table$lipid_class),
          selected = character(0)
        )

        # Update manual selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$raw_data),
          selected = character(0)
        )

        if (input$select_data_table == 'Raw data table') {
          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(r6$tables$raw_data),
            total = ncol(r6$tables$imp_data)
          )
          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(r6$tables$raw_data),
            total = nrow(r6$tables$imp_data)
          )
        }
      })

      # Keep features
      session$userData[[id]]$keep_cols = shiny::observeEvent(input$keep_cols,{
        shiny::req(r6$tables$feature_table)
        print_tm(m, 'Data - Keeping features')
        selected_species = rownames(r6$tables$feature_table)[which(r6$tables$feature_table$lipid_class %in% input$class_selection)]
        selected_species = unique(c(selected_species, input$manual_selection))
        selected_species = colnames(r6$tables$raw_data)[!(colnames(r6$tables$raw_data) %in% selected_species)]
        r6$indices$excluded_cols = c(r6$indices$excluded_cols, selected_species)

        r6$tables$raw_data = drop_cols(data_table = r6$tables$raw_data, cols = selected_species)

        r6$derive_data_tables()

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {
          data_table = table_switch(input$select_data_table, r6)
          output$lipid_class_summary = shiny::renderPlot(
            lipidomics_summary_plot(r6, data_table)
          )
        }

        # Update class selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          choices = unique(r6$tables$feature_table$lipid_class),
          selected = character(0)
        )

        # Update manual selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$raw_data),
          selected = character(0)
        )

        if (input$select_data_table == 'Raw data table') {
          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(r6$tables$raw_data),
            total = ncol(r6$tables$imp_data)
          )
          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(r6$tables$raw_data),
            total = nrow(r6$tables$imp_data)
          )
        }
      })

      # Reset feature filter
      session$userData[[id]]$reset_data_table = shiny::observeEvent(input$reset_data_table,{
        r6$indices$excluded_cols = NULL
      })

      # Reset filters
      session$userData[[id]]$clear_data_filters = shiny::observeEvent(input$clear_data_filters,{
        # Reset class selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          selected = character(0)
        )

        # Reset manual selection
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          selected = character(0)
        )
      })

      # Download data table
      dl_data_table = shiny::reactiveValues(
        name = NULL,
        table = NULL
      )

      session$userData[[id]]$download_datatable = shiny::observeEvent(c(input$select_data_table, input$reset_data_table, input$keep_cols, input$drop_cols, input$reset_meta, input$selection_keep, input$selection_drop) , {
        shiny::req(r6$tables$raw_data)
        if (r6$preloaded_data) {return()}
        dl_data_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_data_table, " ", "_"), ".csv"))
        dl_data_table$table = table_switch(input$select_data_table, r6)
      })

      #------------------------------------------------------------ Info server ----
      # Download data and meta tables
      output$info_download_data <- shiny::downloadHandler(
        filename = function() {
          req(input$info_download_table_select)

          file_name <- table_name_switch(table_name = input$info_download_table_select)

          timestamped_name(file_name = paste0(file_name, ".csv"))
        },
        content = function(file_name) {
          req(input$info_download_table_select)

          data_table <- table_switch(table_name = input$info_download_table_select,
                                     r6 = r6)

          write.csv(data_table, file_name)
        }
      )


      # show the lipid class table
      output$info_sum_table <- DT::renderDataTable({
        req(r6$tables$class_table)

        class_table <- as.data.frame(r6$tables$class_table)
        lipidclasses <- colnames(class_table)
        class_table$Total <- rowSums(class_table, na.rm = TRUE)
        class_table$`Sample ID` <- rownames(class_table)
        # arrange the column names
        class_table <- class_table[, c("Sample ID", lipidclasses, "Total")]

        DT::datatable(data = class_table,
                      rownames = FALSE,
                      options = list(dom = "t",
                                     pageLength = nrow(class_table),
                                     ordering = FALSE),
                      selection = "none") |>
          DT::formatRound(columns = 2:ncol(class_table),
                          digits = 1)
      })


      output$info_experiment_table <- DT::renderDataTable({
        req(r6$tables$raw_meta)

        raw_meta <- r6$tables$raw_meta
        # remove NA's
        raw_meta <- as.data.frame(apply(raw_meta, 2, function(x) {
          x[x == "NA" | is.na(x)] <- ""
          x
        }))

        exp_info_table <- data.frame(
          title = c(
            "Experiment title",
            "Sample type",
            "Genotype",
            "Parental cell line / Brain region",
            "Cell line name",
            "Culture conditions",
            "Harvest date",
            "Gender",
            "Treatment / Diagnosis",
            "Contributing lab",
            "Machine"
          ),
          value = c(
            unique(r6$tables$raw_meta$experimentTitle),
            paste(unique(raw_meta$sampleType), collapse = ", "),
            paste(unique(raw_meta$genoType), collapse = ", "),
            paste(unique(raw_meta$parentCellLineBrainregion), collapse = ", "),
            paste(unique(raw_meta$cellLineName), collapse = ", "),
            paste(unique(raw_meta$cultureConditions), collapse = ", "),
            paste(unique(raw_meta$harvestDate), collapse = ", "),
            paste(unique(raw_meta$sex), collapse = ", "),
            paste(unique(raw_meta$treatmentDiagnosis), collapse = ", "),
            paste(unique(raw_meta$lab), collapse = ", "),
            paste(unique(paste0("Sciex QTRAP ", raw_meta$Machine)), collapse = ", ")
          )
        )

        DT::datatable(
          data = exp_info_table,
          rownames = FALSE,
          colnames = c("", ""),
          options = list(dom = "t",
                         pageLength = nrow(exp_info_table),
                         ordering = FALSE),
          selection = "none"
        ) |>
          DT::formatStyle(
            columns = "title",
            fontWeight = "bold"
          )
      })


      output$info_metadata_table <- DT::renderDataTable({
        req(r6$tables$raw_meta)

        meta_data <- r6$tables$raw_meta[, c("sampleId", "sampleType", "genoType",
                                            "parentCellLineBrainregion", "cellLineName",
                                            "cultureConditions", "harvestDate",
                                            "sex", "treatmentDiagnosis")]

        # remove NA's
        meta_data <- as.data.frame(apply(meta_data, 2, function(x) {
          x[x == "NA" | is.na(x)] <- ""
          x
        }))

        DT::datatable(
          data = meta_data,
          rownames = FALSE,
          colnames = c("Sample ID" = "sampleId",
                       "Sample type" = "sampleType",
                       "Genotype" = "genoType",
                       "Parental cell line / Brain region" = "parentCellLineBrainregion",
                       "Cell line name" = "cellLineName",
                       "Culture condtions" = "cultureConditions",
                       "Harvest date" = "harvestDate",
                       "Gender" = "sex",
                       "Treatment / Diagnosis" = "treatmentDiagnosis"),
          options = list(dom = "t",
                         pageLength = nrow(meta_data),
                         ordering = FALSE),
          selection = "none"
        )

      })
      #-------------------------------------------------- Visualize data server ----
      # Initialise dimensions object
      dimensions_obj = shiny::reactiveValues(
        x_box = module_controler$dims$x_box,
        y_box = module_controler$dims$y_box,
        x_plot = module_controler$dims$x_plot,
        y_plot = module_controler$dims$y_plot,
        x_plot_full = module_controler$dims$x_plot_full,
        y_plot_full = module_controler$dims$y_plot_full,
        xpx_total = shinybrowser::get_width(),
        ypx_total = shinybrowser::get_height(),
        xbs = 12,
        xpx = shinybrowser::get_width(),
        ypx = shinybrowser::get_height()
      )

      color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)
      # Plotting events
      class_distribution_events(r6, dimensions_obj, color_palette, input, output, session)
      class_comparison_events(r6, dimensions_obj, color_palette, input, output, session)
      volcano_plot_events(r6, dimensions_obj, color_palette, input, output, session)
      heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
      pca_events(r6, dimensions_obj, color_palette, input, output, session)
      fa_analysis_events(r6, dimensions_obj, color_palette, input, output, session)
      fa_comp_events(r6, dimensions_obj, color_palette, input, output, session)

      session$userData[[id]]$showPlots = shiny::observeEvent(input$showPlots,{
        # Update x dimensions in px and bs, and y in px
        if (length(input$showPlots) < 2) {
          dimensions_obj$xbs = 12
          dimensions_obj$xpx = shinybrowser::get_width()
          dimensions_obj$ypx = shinybrowser::get_height()
        } else if (length(input$showPlots) == 2) {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()/2.2
        }

        # Display plot boxes
        print_tm(m, paste0("Plot selection: ", paste(input$showPlots, collapse = ", ")))
        if (length(input$showPlots) == 1) {
          plot_one_lips(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$showPlots,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$showPlots) == 2) {
          plot_two_lips(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$showPlots,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$showPlots) == 3) {
          plot_three_lips(r6 = r6,
                          dimensions_obj = dimensions_obj,
                          selection_list = input$showPlots,
                          input = input,
                          output = output,
                          session = session)

        } else if (length(input$showPlots) >= 4) {
          plot_four_lips(r6 = r6,
                         dimensions_obj = dimensions_obj,
                         selection_list = input$showPlots,
                         input = input,
                         output = output,
                         session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = setdiff(unname(lipidomics_plot_list()), input$showPlots)
          )

        }

        if ((length(input$showPlots) > 1) & (length(input$showPlots) < 4)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = NULL
          )
        } else if (length(input$showPlots) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = input$showPlots
          )
        }
      })

      session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
        print_tm(m, "Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "showPlots",
          disabled = FALSE,
          selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })
    })
}
