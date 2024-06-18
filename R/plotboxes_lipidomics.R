#------------------------------------------------------- Class distribution ----

class_distribution_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Class distribution: generating plot.")

  if (input$class_distribution_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_distribution(width = width,
                             height = height)
}


class_distribution_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class distribution: spawning plot.")
  output$class_distribution_plot = plotly::renderPlotly({
    r6$plots$class_distribution
    plotly::config(r6$plots$class_distribution, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('class_distribution'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


class_distribution_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_distribution",
                 label = "Class distribution",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_distribution_server = function(r6, input, output, session) {
  ns = session$ns
  print_tm(r6$name, "Class distribution : START.")

  # Generate UI
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("class_distribution_dataset"),
        label = "Select table",
        choices = r6$hardcoded_settings$class_distribution$datasets,
        selected = r6$params$class_distribution$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_distribution_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$class_distribution$group_col
      ),
      shiny::selectizeInput(
        inputId = ns('class_distribution_color_palette'),
        label = "Color palette",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$class_distribution$color_palette,
        multiple = FALSE
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_distribution_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$class_distribution$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_distribution_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}

class_distribution_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  # input validation
  iv_class_distribution <- shinyvalidate::InputValidator$new()
  iv_class_distribution$add_rule("class_distribution_dataset", shinyvalidate::sv_required())
  iv_class_distribution$add_rule("class_distribution_metacol", shinyvalidate::sv_required())
  iv_class_distribution$add_rule("class_distribution_color_palette", shinyvalidate::sv_required())
  iv_class_distribution$add_rule("class_distribution_img_format", shinyvalidate::sv_optional())
  iv_class_distribution$add_rule("class_distribution_dataset",
                                 iv_check_select_input,
                                 choices = r6$hardcoded_settings$class_distribution$datasets,
                                 name_plot = r6$name,
                                 message = "Class distribution: Incorrect dataset selected!")
  iv_class_distribution$add_rule("class_distribution_metacol",
                                 iv_check_select_input,
                                 choices = r6$hardcoded_settings$meta_column,
                                 name_plot = r6$name,
                                 message = "Class distribution: Incorrect group column selected!")
  iv_class_distribution$add_rule("class_distribution_color_palette",
                                 iv_check_select_input,
                                 choices = r6$hardcoded_settings$color_palette,
                                 name_plot = r6$name,
                                 message = "Class distribution: Incorrect color palette selected!")
  iv_class_distribution$add_rule("class_distribution_img_format",
                                 iv_check_select_input,
                                 choices = r6$hardcoded_settings$image_format,
                                 name_plot = r6$name,
                                 message = "Class distribution: Incorrect image format selected!")

  # Generate the plot
  shiny::observeEvent(c(input$class_distribution_dataset,
                        input$class_distribution_metacol,
                        input$class_distribution_color_palette,
                        input$class_distribution_img_format), {
    shiny::req(iv_class_distribution$is_valid())

    if(r6$name == "Error") {
      output$class_distribution_message <- shiny::renderText({
        "Error! No data available!"
      })
    } else {

      print_tm(r6$name, "Class distribution: Updating params...")

      r6$param_class_distribution(dataset = input$class_distribution_dataset,
                                  group_col = input$class_distribution_metacol,
                                  color_palette = input$class_distribution_color_palette,
                                  img_format = input$class_distribution_img_format)

      base::tryCatch({
        class_distribution_generate(r6, color_palette, dimensions_obj, input)
        class_distribution_spawn(r6, input$class_distribution_img_format, output)
      },error=function(e){
        print_tm(r6$name, 'Class distribution: ERROR.')
      },finally={}
      )
    }

  })

  # Download associated table
  output$download_class_distribution_table = shiny::downloadHandler(
    filename = function(){timestamped_name("class_distribution_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$class_distribution_table, file_name)
    }
  )

  # Expanded boxes
  class_distribution_proxy = plotly::plotlyProxy(outputId = "class_distribution_plot",
                                                 session = session)

  shiny::observeEvent(input$class_distribution_plotbox,{
    if (input$class_distribution_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#--------------------------------------------------------- Class comparison ----

class_comparison_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Class comparison: generating plot.")

  if (input$class_comparison_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_comparison(width = width,
                           height = height)
}
class_comparison_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class comparison: spawning plot.")
  output$class_comparison_plot = plotly::renderPlotly({
    r6$plots$class_comparison
    plotly::config(r6$plots$class_comparison, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_name('class_comparison'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}
class_comparison_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_comparison",
                 label = "Class comparison",
                 dimensions_obj = dimensions_obj,
                 session = session)

}
class_comparison_server = function(r6, input, output, session) {

  ns = session$ns
  print_tm(r6$name, "Class comparison: START.")

  output$class_comparison_sidebar_ui = shiny::renderUI({
    shiny::tagList(

      shiny::selectInput(
        inputId = ns("class_comparison_dataset"),
        label = "Select table",
        choices = r6$hardcoded_settings$class_comparison$datasets,
        selected = r6$params$class_comparison$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_comparison_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$class_comparison$group_col
      ),
      shiny::selectizeInput(
        inputId = ns('class_comparison_color_palette'),
        label = "Color palette",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$class_comparison$color_palette,
        multiple = FALSE
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_comparison_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$class_comparison$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_comparison_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}
class_comparison_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  # input validation
  iv_class_comparison <- shinyvalidate::InputValidator$new()
  iv_class_comparison$add_rule("class_comparison_dataset", shinyvalidate::sv_required())
  iv_class_comparison$add_rule("class_comparison_metacol", shinyvalidate::sv_required())
  iv_class_comparison$add_rule("class_comparison_color_palette", shinyvalidate::sv_required())
  iv_class_comparison$add_rule("class_comparison_img_format", shinyvalidate::sv_optional())
  iv_class_comparison$add_rule("class_comparison_dataset",
                               iv_check_select_input,
                               choices = r6$hardcoded_settings$class_comparison$datasets,
                               name_plot = r6$name,
                               message = "Class comparison: Incorrect dataset selected!")
  iv_class_comparison$add_rule("class_comparison_metacol",
                               iv_check_select_input,
                               choices = r6$hardcoded_settings$meta_column,
                               name_plot = r6$name,
                               message = "Class comparison: Incorrect group column selected!")
  iv_class_comparison$add_rule("class_comparison_color_palette",
                               iv_check_select_input,
                               choices = r6$hardcoded_settings$color_palette,
                               name_plot = r6$name,
                               message = "Class comparison: Incorrect color palette selected!")
  iv_class_comparison$add_rule("class_comparison_img_format",
                               iv_check_select_input,
                               choices = r6$hardcoded_settings$image_format,
                               name_plot = r6$name,
                               message = "Class comparison: Incorrect image format selected!")

  # Generate the plot
  shiny::observeEvent(c(input$class_comparison_dataset, input$class_comparison_metacol, input$class_comparison_color_palette, input$class_comparison_img_format), {
    shiny::req(iv_class_comparison$is_valid())

    if(r6$name == "Error") {
      output$class_comparison_message <- shiny::renderText({
        "Error! No data available!"
      })
    } else {
      print_tm(r6$name, "Class comparison: Updating params...")

      r6$param_class_comparison(dataset = input$class_comparison_dataset,
                                group_col = input$class_comparison_metacol,
                                color_palette = input$class_comparison_color_palette,
                                img_format = input$class_comparison_img_format)

      base::tryCatch({
        class_comparison_generate(r6, color_palette, dimensions_obj, input)
        class_comparison_spawn(r6, input$class_comparison_img_format, output)
      },error=function(e){
        print_tm(r6$name, 'Class comparison: error, missing data.')
      },finally={}
      )
    }
  })


  # Download associated table
  output$download_class_comparison_table = shiny::downloadHandler(
    filename = function(){timestamped_name("class_comparison_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$class_distribution_table, file_name)
    }
  )


  # Expanded boxes
  class_comparison_proxy = plotly::plotlyProxy(outputId = "class_comparison_plot",
                                               session = session)

  shiny::observeEvent(input$class_comparison_plotbox,{
    if (input$class_comparison_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- Volcano plot ----

volcano_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Volcano plot: generating plot.")

  if (input$volcano_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_volcano_table(data_table = table_switch(input$volcano_plot_tables, r6))

  r6$plot_volcano(width = width,
                  height = height)

}


volcano_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Volcano plot: spawning plot.")
  output$volcano_plot_plot = plotly::renderPlotly({
    r6$plots$volcano_plot
    plotly::config(r6$plots$volcano_plot, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_name('volcano_plot'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
  })
}


volcano_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "volcano_plot",
                 label = "Volcano plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


volcano_plot_server = function(r6, input, output, session) {

  ns = session$ns
  print_tm(r6$name, "Volcano plot: START.")

  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('volcano_plot_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$volcano_plot$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_tables"),
        label = "Select data table",
        choices = r6$hardcoded_settings$volcano_plot$datasets,
        selected = r6$params$volcano_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$volcano_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[,r6$params$volcano_plot$group_col]),
        selected = c(r6$params$volcano_plot$group_1, r6$params$volcano_plot$group_2),
        multiple = TRUE
      ),
      shiny::span(
        shiny::selectizeInput(
          inputId = ns('volcano_plot_feature_metadata'),
          label = "Feature metadata",
          choices = r6$hardcoded_settings$volcano_plot$feature_metadata,
          selected = r6$params$volcano_plot$feature_metadata,
          multiple = FALSE
        ),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Note:\nTail 1:\n\t* TG total number of carbons.\n\t* PA total number of carbons.\nTail 2:\n\t* TG Number of carbons of one of the tails.\n\t* CE, PA, LPC, LPE 0 number of carbons."
      ),
      # shiny::selectizeInput(
      #   inputId = ns('volcano_plot_annotation_terms'),
      #   label = "Feature annotations",
      #   choices = NULL,
      #   selected = NULL,
      #   multiple = TRUE
      # ),
      shiny::selectizeInput(
        inputId = ns('volcano_plot_color_palette'),
        label = "Feature metadata colors",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$volcano_plot$color_palette,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_function"),
        label = "FC function",
        choices = r6$hardcoded_settings$volcano_plot$calc_func,
        selected = r6$params$volcano_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_test"),
        label = "Select test",
        choices = r6$hardcoded_settings$volcano_plot$test_func,
        selected = r6$params$volcano_plot$selected_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_adjustment"),
        label = "Select adjustment",
        choices = r6$hardcoded_settings$volcano_plot$adjustment_func,
        selected = r6$params$volcano_plot$adjustment,
        multiple = FALSE
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_displayed_plot"),
        label = 'Displayed plot',
        choices = r6$hardcoded_settings$volcano_plot$display_plot,
        selected = r6$params$volcano_plot$displayed_plot,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns("volcano_plot_p_val_threshold"),
        label = 'p-value threshold',
        value = r6$params$volcano_plot$p_val_threshold,
        width = '100%'
      ),

      shiny::textInput(
        inputId = ns("volcano_plot_fc_threshold"),
        label = 'FC threshold',
        value = r6$params$volcano_plot$fc_threshold,
        width = '100%'
      ),

      shiny::textInput(
        inputId = ns("volcano_plot_marker_size"),
        label = 'Marker size',
        value = r6$params$volcano_plot$marker_size,
        width = '100%'
      ),

      shiny::sliderInput(
        inputId = ns("volcano_plot_opacity"),
        label = 'Opacity',
        min = 0.1,
        max = 1.0,
        value = r6$params$volcano_plot$opacity,
        step = 0.1,
        width = '100%'
      ),
      # shiny::actionButton(
      #   inputId = ns("volcano_feature_select"),
      #   label = "Save selection",
      #   width = "100%"
      # ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$volcano_plot$img_format,
        width = "100%"),

      shiny::fluidRow(
        shiny::downloadButton(
          outputId = ns("download_volcano_table"),
          label = "Download associated table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_volcano_subtable"),
          label = "Download selection",
          style = "width:50%;"
        )
      )
    )
  })
}

volcano_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  # input validation
  iv_volcano_plot <- shinyvalidate::InputValidator$new()
  iv_volcano_plot$add_rule("volcano_plot_auto_refresh", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_tables", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_metacol", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_color_palette", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_metagroup", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_feature_metadata", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_function", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_test", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_adjustment", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_displayed_plot", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_p_val_threshold", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_fc_threshold", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_marker_size", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_opacity", shinyvalidate::sv_required())
  iv_volcano_plot$add_rule("volcano_plot_img_format", shinyvalidate::sv_optional())
  iv_volcano_plot$add_rule("volcano_plot_auto_refresh",
                           iv_check_select_input,
                           choices = c(FALSE, TRUE),
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect dataset selected!")
  iv_volcano_plot$add_rule("volcano_plot_tables",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$volcano_plot$datasets,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect dataset selected!")
  iv_volcano_plot$add_rule("volcano_plot_metacol",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$meta_column,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect group column selected!")
  iv_volcano_plot$add_rule("volcano_plot_color_palette",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$color_palette,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect color palette selected!")
  iv_volcano_plot$add_rule("volcano_plot_img_format",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$image_format,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect image palette selected!")
  # iv_volcano_plot$add_rule("volcano_plot_metagroup",
  #                          iv_check_select_input,
  #                          choices = unique(r6$tables$raw_meta[, r6$params$volcano_plot$group_col]),
  #                          name_plot = r6$name,
  #                          message = "Volcano plot: Incorrect groups selected!")
  iv_volcano_plot$add_rule("volcano_plot_feature_metadata",
                           iv_check_select_input,
                           choices = c("None", unique(colnames(r6$tables$feature_table))),
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect feature metadata selected!")
  iv_volcano_plot$add_rule("volcano_plot_function",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$volcano_plot$calc_func,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect FC function selected!")
  iv_volcano_plot$add_rule("volcano_plot_test",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$volcano_plot$test_func,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect test function selected!")
  iv_volcano_plot$add_rule("volcano_plot_adjustment",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$volcano_plot$adjustment_func,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect adjustment function selected!")
  iv_volcano_plot$add_rule("volcano_plot_displayed_plot",
                           iv_check_select_input,
                           choices = r6$hardcoded_settings$volcano_plot$display_plot,
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect display plot selected!")
  iv_volcano_plot$add_rule("volcano_plot_p_val_threshold",
                           iv_check_numeric_input,
                           check_range = c(0, 1),
                           name_plot = r6$name,
                           message = "Volcano plot: p-value threshold not numeric or outside range!")
  iv_volcano_plot$add_rule("volcano_plot_fc_threshold",
                           iv_check_numeric_input,
                           check_range = c(0, 100),
                           name_plot = r6$name,
                           message = "Volcano plot: FC threshold (0, 100) not numeric or outside range!")
  iv_volcano_plot$add_rule("volcano_plot_marker_size",
                           iv_check_numeric_input,
                           check_range = c(1, 30),
                           name_plot = r6$name,
                           message = "Volcano plot: marker size (1, 30) not numeric or outside range!")
  iv_volcano_plot$add_rule("volcano_plot_opacity",
                           iv_check_numeric_input,
                           check_range = c(0.1, 1),
                           name_plot = r6$name,
                           message = "Volcano plot: opacity (0.1, 1) not numeric or outside range!")

  # auto-update selected groups
  shiny::observeEvent(input$volcano_plot_metacol, {
    if(r6$name == "Error") {
      output$volcano_plot_message <- shiny::renderText({
        "Error! No data available!"
      })
    } else {
      shiny::updateSelectizeInput(
        inputId = "volcano_plot_metagroup",
        session = session,
        choices = unique(r6$tables$raw_meta[, input$volcano_plot_metacol]),
        selected = unique(r6$tables$raw_meta[, input$volcano_plot_metacol])[c(1, 2)]
      )
    }
  })

  # make the plot
  shiny::observeEvent(
    c(shiny::req(length(input$volcano_plot_metagroup) == 2),
      input$volcano_plot_auto_refresh,
      input$volcano_plot_tables,
      input$volcano_plot_function,
      input$volcano_plot_adjustment,
      input$volcano_plot_test,
      input$volcano_plot_displayed_plot,
      input$volcano_plot_feature_metadata,
      input$volcano_plot_color_palette,
      input$volcano_plot_p_val_threshold,
      input$volcano_plot_fc_threshold,
      input$volcano_plot_marker_size,
      input$volcano_plot_opacity,
      input$volcano_plot_img_format
    ), {
      if(r6$name == "Error") {
        output$volcano_plot_message <- shiny::renderText({
          "Error! No data available!"
        })
      } else {
        shiny::req(iv_volcano_plot$is_valid(),
                   length(input$volcano_plot_metagroup) == 2)

        shinyjs::hide(id = "volcano_plot_message")
        output$volcano_plot_message <- shiny::renderText({return(NULL)})

        if (!input$volcano_plot_auto_refresh) {
          r6$params$volcano_plot$auto_refresh = input$volcano_plot_auto_refresh
          return()
        }
        print_tm(r6$name, "Volcano plot: Updating params...")

        # Is the column multivalue?
        if (input$volcano_plot_feature_metadata %in% names(r6$tables$feature_list)) {
          if (length(input$volcano_plot_annotation_terms) > 0) {
            feature_metadata = match_go_terms(terms_list = input$volcano_plot_annotation_terms,
                                              sparse_table = r6$tables$feature_list[[input$volcano_plot_feature_metadata]]$sparse_matrix)
          } else {
            return()
          }
        } else {
          feature_metadata = input$volcano_plot_feature_metadata
        }

        r6$param_volcano_plot(auto_refresh = input$volcano_plot_auto_refresh,
                              data_table = input$volcano_plot_tables,
                              adjustment = input$volcano_plot_adjustment,
                              group_col = input$volcano_plot_metacol,
                              group_1 = input$volcano_plot_metagroup[1],
                              group_2 = input$volcano_plot_metagroup[2],
                              feature_metadata = input$volcano_plot_feature_metadata,
                              color_palette = input$volcano_plot_color_palette,
                              displayed_plot = input$volcano_plot_displayed_plot,
                              p_val_threshold = input$volcano_plot_p_val_threshold,
                              fc_threshold = input$volcano_plot_fc_threshold,
                              marker_size = input$volcano_plot_marker_size,
                              opacity = input$volcano_plot_opacity,
                              selected_function = input$volcano_plot_function,
                              selected_test = input$volcano_plot_test,
                              img_format = input$volcano_plot_img_format)

        base::tryCatch({
          volcano_plot_generate(r6, color_palette, dimensions_obj, input)
          volcano_plot_spawn(r6, input$volcano_plot_img_format, output)
        },
        error = function(e) {
          if(grepl(x = e,
                   pattern = "not enough observations")) {
            output$volcano_plot_plot <- plotly::renderPlotly({return(NULL)})
            shinyjs::show(id = "volcano_plot_message")
            output$volcano_plot_message <- shiny::renderText({
              "Error: Not enough samples in one or both groups!"
            })
          }
          print_tm(r6$name, 'Volcano plot: ERROR.')
          print(e)
        },
        finally = {}
        )
      }

    })


  # Save selection
  # shiny::observeEvent(input$volcano_feature_select, {
  #   print_tm(r6$name, "Volcano plot: saving selection.")
  #   volcano_selection = plotly::event_data(
  #     "plotly_selected"
  #   )
  #   if (is.null(volcano_selection)){
  #     print_tm(r6$name, "Brushed points appear here (double-click to clear)")
  #   }else {
  #     r6$slice_volcano_table(
  #       x = volcano_selection[3][[1]],
  #       y = volcano_selection[4][[1]],
  #       x_col = "log2_fold_change",
  #       y_col = adjustment_switch(input$volcano_plot_adjustment)
  #     )
  #   }
  # })

  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table, file_name)
    }
  )

  output$download_volcano_subtable = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table_selection.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table_slice, file_name)
    }
  )

  # Expanded boxes
  volcano_plot_proxy = plotly::plotlyProxy(outputId = "volcano_plot_plot",
                                           session = session)

  shiny::observeEvent(input$volcano_plot_plotbox,{
    if (input$volcano_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#-----------------------------------------------------------FA analysis index ----
fa_analysis_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Fatty acid analysis index plot: generating plot.")

  if (input$fa_analysis_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_fa_analysis(width = width,
                      height = height)
}

fa_analysis_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fatty acid analysis index: spawning plot.")

  output$fa_analysis_plot = plotly::renderPlotly({
    r6$plots$fa_analysis_plot
    plotly::config(r6$plots$fa_analysis_plot, toImageButtonOptions = list(format = format,
                                                                          filename = timestamped_name('fa_analysis'),
                                                                          height = NULL,
                                                                          width = NULL,
                                                                          scale = 1))
  })
}

fa_analysis_ui = function(dimensions_obj, session) {
  # add function to show bs4dash with plotting function
  get_plotly_box(id = "fa_analysis",
                 label = "Fatty acid analysis",
                 dimensions_obj = dimensions_obj,
                 session = session)
}

fa_analysis_server = function(r6, input, output, session) {
  ns = session$ns
  print_tm(r6$name, "Fatty acid analysis index: START.")

  # set some UI
  output$fa_analysis_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("fa_analysis_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$fa_analysis_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("fa_analysis_selected_view"),
        label = "Select view",
        choices = c("FA overview per lipid class" = "lipidclass",
                    "Lipid class overview per FA" = "fa"),
        selected = r6$params$fa_analysis$selected_view,
        multiple = FALSE
      ),
      shiny::uiOutput(outputId = ns("fa_analysis_selected_view_ui")),
      shiny::span(
        shinyWidgets::materialSwitch(
          inputId = ns("fa_analysis_fa_norm"),
          label = "Fatty acid normalisation",
          status = "success",
          right = TRUE,
          value = r6$params$fa_analysis_plot$fa_norm
        ),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Normalize the data by the total of the fatty acids."
      ),
      shiny::selectizeInput(
        inputId = ns('fa_analysis_color_palette'),
        label = "Color palette",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$fa_analysis_plot$color_palette,
        multiple = FALSE
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_analysis_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$fa_analysis_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_fa_analysis_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

  output$fa_analysis_selected_view_ui <- shiny::renderUI({
    req(input$fa_analysis_selected_view)

    selected_view <- input$fa_analysis_selected_view

    # get unique FA's, ignore PA
    feature_table <- r6$tables$feature_table[r6$tables$feature_table$lipid_class != "PA", ]
    fa_tails <- c(
      paste0(feature_table$carbons_1[feature_table$lipid_class != "TG"], ":", feature_table$unsat_1[feature_table$lipid_class != "TG"]),
      paste0(feature_table$carbons_2, ":", feature_table$unsat_2)
    )

    fa_tails <- unique(fa_tails)
    fa_tails <- sort(fa_tails[fa_tails != "0:0"])

    if(selected_view == "lipidclass") {
      shiny::selectizeInput(
        inputId = ns("fa_analysis_selected_lipidclass"),
        label = "Select lipid class",
        choices = c("All (incl. TG)" = "All",
                    "All (excl. TG)" = "All_noTG",
                    unique(r6$tables$feature_table$lipid_class)[!(unique(r6$tables$feature_table$lipid_class) %in% c("PA"))]),
        selected = r6$params$fa_analysis$selected_lipidclass,
        multiple = FALSE
      )
    } else if(selected_view == "fa") {
      shiny::selectizeInput(
        inputId = ns("fa_analysis_selected_fa"),
        label = "Select fatty acid",
        choices = fa_tails,
        selected = r6$params$fa_analysis$selected_fa,
        multiple = TRUE
      )
    }
  })
}


fa_analysis_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  # get unique FA's, ignore PA
  feature_table <- r6$tables$feature_table[r6$tables$feature_table$lipid_class != "PA", ]
  fa_tails <- c(
    paste0(feature_table$carbons_1[feature_table$lipid_class != "TG"], ":", feature_table$unsat_1[feature_table$lipid_class != "TG"]),
    paste0(feature_table$carbons_2, ":", feature_table$unsat_2)
  )

  fa_tails <- unique(fa_tails)
  fa_tails <- sort(fa_tails[fa_tails != "0:0"])

  iv_fa_analysis <- shinyvalidate::InputValidator$new()
  iv_fa_analysis$add_rule("fa_analysis_metacol", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_selected_view", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_selected_lipidclass", shinyvalidate::sv_optional())
  iv_fa_analysis$add_rule("fa_analysis_selected_fa", shinyvalidate::sv_optional())
  iv_fa_analysis$add_rule("fa_analysis_color_palette", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_img_format", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_metacol",
                          iv_check_select_input,
                          choices = r6$hardcoded_settings$meta_column,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect group column selected!")
  iv_fa_analysis$add_rule("fa_analysis_selected_view",
                          iv_check_select_input,
                          choices = c("lipidclass", "fa"),
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect view selected!")
  iv_fa_analysis$add_rule("fa_analysis_selected_lipidclass",
                          iv_check_select_input,
                          choices = c("All", "All_noTG", unique(r6$tables$feature_table$lipid_class)[!(unique(r6$tables$feature_table$lipid_class) %in% c("PA"))]),
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect lipid class selected!")
  iv_fa_analysis$add_rule("fa_analysis_selected_fa",
                          iv_check_select_input,
                          choices = fa_tails,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect fatty acid tail selected!")
  iv_fa_analysis$add_rule("fa_analysis_color_palette",
                          iv_check_select_input,
                          choices = r6$hardcoded_settings$color_palette,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect color palette selected!")
  iv_fa_analysis$add_rule("fa_analysis_img_format",
                          iv_check_select_input,
                          choices = r6$hardcoded_settings$image_format,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect image format selected!")

  # Generate the plot
  shiny::observeEvent(c(input$fa_analysis_metacol,
                        input$fa_analysis_selected_view,
                        input$fa_analysis_selected_lipidclass,
                        input$fa_analysis_selected_fa,
                        input$fa_analysis_fa_norm,
                        input$fa_analysis_color_palette,
                        input$fa_analysis_img_format), {
    shiny::req(iv_fa_analysis$is_valid())

                          if(r6$name == "Error") {
                            output$fa_analysis_message <- shiny::renderText({
                              "Error! No data available!"
                            })
                          } else {
                            if(input$fa_analysis_selected_view == "lipidclass") {
                              shiny::req(input$fa_analysis_selected_lipidclass)
                            } else if(input$fa_analysis_selected_view == "fa") {
                              shiny::req(input$fa_analysis_selected_fa)
                            }

                            print_tm(r6$name, "Fatty acid analysis: Updating params...")

                            r6$param_fa_analysis_plot(data_table = r6$tables$total_norm_data,
                                                      feature_meta = r6$tables$feature_table,
                                                      sample_meta = r6$tables$raw_meta,
                                                      group_col = input$fa_analysis_metacol,
                                                      selected_view = input$fa_analysis_selected_view,
                                                      selected_lipidclass = input$fa_analysis_selected_lipidclass,
                                                      selected_fa = input$fa_analysis_selected_fa,
                                                      fa_norm = input$fa_analysis_fa_norm,
                                                      color_palette = input$fa_analysis_color_palette,
                                                      img_format = input$fa_analysis_img_format)

                            base::tryCatch({
                              fa_analysis_generate(r6, color_palette, dimensions_obj, input)
                              fa_analysis_spawn(r6, input$fa_analysis_img_format, output)
                            },
                            error = function(e) {
                              print_tm(r6$name, 'Fatty acid analysis error, missing data.')
                              print(e)
                            },
                            finally = {}
                            )
                          }
  })

  # Download associated table
  output$download_fa_analysis_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fa_analysis_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fa_analysis_table, file_name)
    }
  )

  # Expanded boxes
  fa_analysis_proxy = plotly::plotlyProxy(outputId = "fa_analysis_plot",
                                          session = session)

  shiny::observeEvent(input$fa_analysis_plotbox,{
    if (input$fa_analysis_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fa_analysis_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fa_analysis_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#----------------------------------------------------------------- Heat map ----

heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Heatmap: generating plot.")

  if (input$heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_heatmap(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                  height = dimensions_obj$ypx * dimensions_obj$y_plot)
}

heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly({
    r6$plots$heatmap
    plotly::config(r6$plots$heatmap, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_name('heatmap'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}


heatmap_ui = function(dimensions_obj, session) {
  get_plotly_box(id = "heatmap",
                 label = "Heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


heatmap_server = function(r6, input, output, session) {

  ns = session$ns
  print_tm(r6$name, "Heatmap: START.")

  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$heatmap$datasets,
        selected = r6$params$heatmap$dataset
      ),

      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::span(
            shinyWidgets::switchInput(inputId = ns("heatmap_impute"),
                                      label = "Impute missing",
                                      value = r6$params$heatmap$impute,
                                      onLabel = "YES",
                                      offLabel = "NO",
                                      labelWidth = "125px"
            ),
            `data-toggle` = "tooltip",
            `data-placement` = "right",
            title = "Missing values will be imputed with the minimum value of that sample."
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_samples"),
                                    label = "Cluster samples",
                                    value = r6$params$heatmap$cluster_samples,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_features"),
                                    label = "Cluster features",
                                    value = r6$params$heatmap$cluster_features,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("heatmap_map_rows"),
            label = "Annotate samples",
            multiple = TRUE,
            choices = r6$hardcoded_settings$meta_column,
            selected = r6$params$heatmap$map_sample_data
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("heatmap_map_cols"),
            label = "Annotate features",
            multiple = TRUE,
            choices = r6$hardcoded_settings$heatmap$map_cols,
            selected = r6$params$heatmap$map_feature_data
          )
        )
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::switchInput(inputId = ns("heatmap_apply_da"),
                                    label = "Apply",
                                    value = r6$params$heatmap$apply_da)
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(inputId = ns("heatmap_group_col_da"),
                                label = "Group",
                                choices = r6$hardcoded_settings$meta_column,
                                selected = r6$params$heatmap$group_column_da,
                                multiple = FALSE,
                                width = "100%")
        ),
        shiny::column(
          width = 6,
          shiny::span(
            shiny::sliderInput(inputId = ns("heatmap_alpha_da"),
                               label = "Alpha",
                               min = 0,
                               max = 0.99,
                               value = r6$params$heatmap$alpha_da,
                               step = 0.01,
                               width = "100%")
          ),
          `data-toggle` = "tooltip",
          `data-placement` = "right",
          title = "A higher value for alpha will result in a smaller amount of lipid species."
        )
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::span(
            shiny::numericInput(
              inputId = ns("heatmap_factor_height"),
              label = "Height heatmap multiplication",
              value = 2,
              min = 1,
              max = 5,
              step = 0.1,
              width = "100%"
            ),
            `data-toggle` = "tooltip",
            `data-placement` = "right",
            title = "Multiply the height of the heatmap by this factor to increase the height. 1 means the height of the box."
          ),
          shiny::selectInput(
            inputId = ns('heatmap_colors_palette'),
            label = 'Color palette',
            choices = r6$hardcoded_settings$color_palette,
            selected = r6$params$heatmap$color_palette,
            width = '100%'
          ),
          shinyWidgets::materialSwitch(
            inputId = ns('heatmap_reverse_palette'),
            label = 'Reverse palette',
            value = r6$params$heatmap$reverse_palette,
            right = TRUE,
            status = "primary"
          )
        )
      ),

      shiny::actionButton(
        inputId = ns("heatmap_run"),
        label = "Generate heatmap",
        width = "100%"
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("heatmap_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  iv_heatmap <- shinyvalidate::InputValidator$new()
  iv_heatmap$add_rule("heatmap_dataset", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_impute", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_cluster_samples", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_cluster_features", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_map_rows", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_map_cols", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_apply_da", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_group_col_da", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_alpha_da", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_colors_palette", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_reverse_palette", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_img_format", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_factor_height", shinyvalidate::sv_required())
  iv_heatmap$add_rule("heatmap_dataset",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$heatmap$datasets,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect data set selected!")
  iv_heatmap$add_rule("heatmap_impute",
                      iv_check_select_input,
                      choices = c(FALSE, TRUE),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect impute set!")
  iv_heatmap$add_rule("heatmap_cluster_samples",
                      iv_check_select_input,
                      choices = c(FALSE, TRUE),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect cluster samples set!")
  iv_heatmap$add_rule("heatmap_cluster_features",
                      iv_check_select_input,
                      choices = c(FALSE, TRUE),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect cluster features set!")
  iv_heatmap$add_rule("heatmap_map_rows",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$meta_column,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect map sample data selected!")
  # iv_heatmap$add_rule("heatmap_map_cols",
  #                     iv_check_select_input,
  #                     choices = r6$hardcoded_settings$heatmap$map_cols,
  #                     name_plot = r6$name,
  #                     message = "Heatmap: Incorrect map feature data selected!")
  iv_heatmap$add_rule("heatmap_apply_da",
                      iv_check_select_input,
                      choices = c(FALSE, TRUE),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect apply DA set!")
  iv_heatmap$add_rule("heatmap_group_col_da",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$meta_column,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect group column for DA selected!")
  iv_heatmap$add_rule("heatmap_alpha_da",
                      iv_check_numeric_input,
                      check_range = c(0, 1),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect alpha for DA set!")
  iv_heatmap$add_rule("heatmap_colors_palette",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$color_palette,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect color palette selected!")
  iv_heatmap$add_rule("heatmap_reverse_palette",
                      iv_check_select_input,
                      choices = c(FALSE, TRUE),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect reverse color palette selected!")
  iv_heatmap$add_rule("heatmap_img_format",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$image_format,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect image format selected!")
  iv_heatmap$add_rule("heatmap_factor_height",
                      iv_check_numeric_input,
                      check_range = c(1, 5),
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect multiplication factor for the height!")

  shiny::observeEvent(input$heatmap_dataset, {
    # disable feature annotation when a lipid class table is selected
    if(input$heatmap_dataset == "Class table z-scored" |
       input$heatmap_dataset == "Class table z-scored total normalized") {
      shinyjs::disable(id = "heatmap_map_cols")

      # update numeric input for factor height
      shiny::updateNumericInput(
        inputId = "heatmap_factor_height",
        label = "Height heatmap multiplication",
        value = 1,
        min = 1,
        max = 5,
        step = 0.1
      )
    } else {
      shinyjs::enable(id = "heatmap_map_cols")

      # update numeric input for factor height
      shiny::updateNumericInput(
        inputId = "heatmap_factor_height",
        label = "Height heatmap multiplication",
        value = 2,
        min = 1,
        max = 5,
        step = 0.1
      )
    }
  })

  shiny::observeEvent(c(input$heatmap_run), {
    req(input$heatmap_run,
        input$heatmap_img_format)

    # make sure no feature annotations will be supplied to heatmap when
    # a lipid class table is selected
    if(input$heatmap_dataset == "Class table z-scored" |
       input$heatmap_dataset == "Class table z-scored total normalized") {
      map_feature_data <- NULL
    } else {
      map_feature_data <- input$heatmap_map_cols
    }

    shinyjs::hide(id = "heatmap_message")
    output$heatmap_message <- shiny::renderText({return(NULL)})

    # disable run button
    shinyjs::disable(id = "heatmap_run")
    print_tm(r6$name, "Heatmap: Updating params...")

    r6$param_heatmap(dataset = input$heatmap_dataset,
                     impute = input$heatmap_impute,
                     cluster_samples = input$heatmap_cluster_samples,
                     cluster_features = input$heatmap_cluster_features,
                     map_sample_data = input$heatmap_map_rows,
                     map_feature_data = map_feature_data,
                     group_column_da = input$heatmap_group_col_da,
                     apply_da = input$heatmap_apply_da,
                     alpha_da = input$heatmap_alpha_da,
                     color_palette = input$heatmap_colors_palette,
                     reverse_palette = input$heatmap_reverse_palette,
                     factor_height = input$heatmap_factor_height,
                     img_format = input$heatmap_img_format)

    base::tryCatch({
      heatmap_generate(r6, color_palette, dimensions_obj, input)
      heatmap_spawn(r6, input$heatmap_img_format, output)
    },error=function(e){
      shinyjs::show(id = "heatmap_message")
      output$heatmap_message <- shiny::renderText({
        if(grepl(x = e,
                 pattern = "hclustfun\\(dist\\): NA\\/NaN\\/Inf")) {
          return("There are missing values in the data! Use imputation!")
        } else if(grepl(x = e,
                        pattern = "Heatmap: not enough groups")) {
          return("Error: not enough groups with a least 3 samples!")
        } else {
          return("An error occurred! Can not generate heatmap!")
        }
      })
      print_tm(r6$name, 'Heatmap: ERROR.')
      print(e)
      shinyjs::enable("heatmap_run")
    },finally={}
    )

    shinyjs::enable("heatmap_run")
  })

  output$heatmap_message <- shiny::renderText({
    if(r6$name == "Error") {
      "Error! No data available!"
    } else {
      "To generate a heatmap, go to the settings menu and click the generate heatmap button."
    }
  })

  # Download associated table
  output$download_heatmap_table = shiny::downloadHandler(
    filename = function(){timestamped_name("heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$heatmap_table, file_name)
    }
  )

  # Expanded boxes
  heatmap_proxy = plotly::plotlyProxy(outputId = "heatmap_plot",
                                      session = session)

  shiny::observeEvent(input$heatmap_plotbox,{
    if (input$heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}



#---------------------------------------------------------------------- PCA ----

pca_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "PCA: generating plot.")

  if (input$pca_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }


  r6$plot_pca(width = width,
              height = height)
}

pca_spawn = function(r6, format, output) {
  print_tm(r6$name, "PCA: spawning plot.")
  output$pca_plot = plotly::renderPlotly({
    r6$plots$pca_plot
    plotly::config(r6$plots$pca_plot, toImageButtonOptions = list(format= format,
                                                                  filename= timestamped_name('pca_plot'),
                                                                  height= NULL,
                                                                  width= NULL,
                                                                  scale= 1))
  })
}

pca_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "pca",
                 label = "PCA",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


pca_server = function(r6, input, output, session) {

  ns = session$ns
  print_tm(r6$name, "PCA: START.")

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shinyWidgets::materialSwitch(
        inputId = ns('pca_auto_refresh'),
        label = 'Auto-refresh',
        value = r6$params$pca$auto_refresh,
        right = TRUE,
        status = "success"
      ),
      shiny::selectInput(
        inputId = ns("pca_data_table"),
        label = "Select dataset",
        choices = r6$hardcoded_settings$pca$datasets,
        selected = r6$params$pca$data_table
      ),
      shiny::selectInput(
        inputId = ns("pca_sample_groups_col"),
        label = "Sample group column (color)",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$pca$sample_groups_col
      ),
      shiny::selectInput(
        inputId = ns("pca_sample_groups_col_shape"),
        label = "Sample group column (shape)",
        choices = c("", r6$hardcoded_settings$meta_column),
        selected = r6$params$pca$sample_groups_col_shape
      ),
      shiny::selectInput(
        inputId = ns("pca_feature_group"),
        label = "Feature group column",
        choices = r6$hardcoded_settings$pca$feature_metadata,
        selected = r6$params$pca$feature_groups_col
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h4("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::switchInput(inputId = ns("pca_apply_da"),
                                    label = "Apply",
                                    value = r6$params$pca$apply_da,
                                    width = "100%")
        )
      ),
      shiny::sliderInput(inputId = ns("pca_alpha_da"),
                         label = "Alpha",
                         min = 0,
                         max = 0.99,
                         value = r6$params$pca$alpha_da,
                         step = 0.01,
                         width = "100%"),
      shiny::selectInput(
        inputId = ns('pca_method'),
        label = 'PCA method',
        choices = r6$hardcoded_settings$pca$method,
        selected = r6$params$pca$pca_method,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('pca_npcs'),
        label = 'Number of PCs',
        value = r6$params$pca$nPcs,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('pca_displayed_pc_1'),
        label = 'Displayed PC (1)',
        value = r6$params$pca$displayed_pc_1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('pca_displayed_pc_2'),
        label = 'Displayed PC (2)',
        value = r6$params$pca$displayed_pc_2,
        width = '100%'
      ),
      shiny::span(
        shinyWidgets::materialSwitch(
          inputId = ns('pca_completeObs'),
          label = 'Complete observations',
          status = "success",
          right = TRUE,
          value = r6$params$pca$completeObs
        ),
        `data-toggle` = "tooltip",
        `data-placement` = "right",
        title = "Missing values will be replaced by estimated values."
      ),
      shiny::selectInput(
        inputId = ns('pca_displayed_plots'),
        label = 'Displayed plot',
        choices = r6$hardcoded_settings$pca$display_plot,
        selected = r6$params$pca$displayed_plots,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('pca_colors_palette'),
        label = 'Color palette',
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$pca$colors_palette,
        width = '100%'
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("pca_img_format"),
          label = "Image format",
          choices = r6$hardcoded_settings$image_format,
          selected = r6$params$pca$img_format,
          width = "100%"),
        shiny::downloadButton(
          outputId = ns("download_pca_scores_table"),
          label = "Download scores table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_pca_loadings_table"),
          label = "Download loadings table",
          style = "width:50%;"
        )
      )

    )
  })
}

pca_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  iv_pca <- shinyvalidate::InputValidator$new()
  iv_pca$add_rule("pca_auto_refresh", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_data_table", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_sample_groups_col", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_sample_groups_col_shape", shinyvalidate::sv_optional())
  iv_pca$add_rule("pca_feature_group", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_apply_da", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_alpha_da", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_method", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_npcs", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_displayed_pc_1", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_displayed_pc_2", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_completeObs", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_displayed_plots", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_colors_palette", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_img_format", shinyvalidate::sv_required())
  iv_pca$add_rule("pca_auto_refresh",
                  iv_check_select_input,
                  choices = c(FALSE, TRUE),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect auto refresh set")
  iv_pca$add_rule("pca_data_table",
                  iv_check_select_input,
                  choices = r6$hardcoded_settings$pca$datasets,
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect data set selected!")
  iv_pca$add_rule("pca_sample_groups_col",
                  iv_check_select_input,
                  choices = r6$hardcoded_settings$meta_column,
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect group column selected!")
  iv_pca$add_rule("pca_sample_groups_col_shape",
                  iv_check_select_input,
                  choices = c("", r6$hardcoded_settings$meta_column),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect group column selected for shape!")
  iv_pca$add_rule("pca_feature_group",
                  iv_check_select_input,
                  choices =  unique(colnames(r6$tables$feature_table)),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect feature group column selected!")
  iv_pca$add_rule("pca_apply_da",
                  iv_check_select_input,
                  choices =  c(FALSE, TRUE),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect apply DA set!")
  iv_pca$add_rule("pca_alpha_da",
                  iv_check_numeric_input,
                  check_range = c(0, 0.99),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect alpha DA set!")
  iv_pca$add_rule("pca_method",
                  iv_check_select_input,
                  choices =  r6$hardcoded_settings$pca$method,
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect pca method set!")
  iv_pca$add_rule("pca_npcs",
                  iv_check_numeric_input,
                  check_range = c(1, r6$params$pca$nPcs),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect number of PC's set!")
  iv_pca$add_rule("pca_displayed_pc_1",
                  iv_check_numeric_input,
                  check_range = c(1, r6$params$pca$nPcs),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect value for PC1 set!")
  iv_pca$add_rule("pca_displayed_pc_2",
                  iv_check_numeric_input,
                  check_range = c(1, r6$params$pca$nPcs),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect value for PC2 set!")
  iv_pca$add_rule("pca_completeObs",
                  iv_check_select_input,
                  choices =  c(FALSE, TRUE),
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect complete observations set!")
  iv_pca$add_rule("pca_displayed_plots",
                  iv_check_select_input,
                  choices =  r6$hardcoded_settings$pca$display_plot,
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect display plot selected!")
  iv_pca$add_rule("pca_img_format",
                  iv_check_select_input,
                  choices =  r6$hardcoded_settings$image_format,
                  name_plot = r6$name,
                  message = "PCA plot: Incorrect image format selected!")

  shiny::observeEvent(c(input$pca_auto_refresh,
                        input$pca_data_table,
                        input$pca_sample_groups_col,
                        input$pca_sample_groups_col_shape,
                        input$pca_feature_group,
                        input$pca_apply_da,
                        input$pca_alpha_da,
                        input$pca_method, input$pca_npcs,
                        input$pca_displayed_pc_1,
                        input$pca_displayed_pc_2,
                        input$pca_completeObs,
                        input$pca_displayed_plots,
                        input$pca_colors_palette,
                        input$pca_img_format), {

                          if(r6$name == "Error") {
                            output$pca_message <- shiny::renderText({
                              "Error! No data available!"
                            })
                          } else {
                            shiny::req(iv_pca$is_valid())

                            if (!input$pca_auto_refresh) {
                              r6$params$pca$auto_refresh = input$pca_auto_refresh
                              return()
                            }

                            print_tm(r6$name, "PCA: Updating params...")

                            r6$param_pca(auto_refresh = input$pca_auto_refresh,
                                         data_table = input$pca_data_table,
                                         sample_groups_col = input$pca_sample_groups_col,
                                         sample_groups_col_shape = input$pca_sample_groups_col_shape,
                                         feature_groups_col = input$pca_feature_group,
                                         apply_da = input$pca_apply_da,
                                         alpha_da = input$pca_alpha_da,
                                         pca_method = input$pca_method,
                                         nPcs = input$pca_npcs,
                                         displayed_pc_1 = input$pca_displayed_pc_1,
                                         displayed_pc_2 = input$pca_displayed_pc_2,
                                         completeObs = input$pca_completeObs,
                                         displayed_plots = input$pca_displayed_plots,
                                         colors_palette = input$pca_colors_palette,
                                         img_format = input$pca_img_format)

                            base::tryCatch({
                              pca_generate(r6, color_palette, dimensions_obj, input)
                              pca_spawn(r6, input$pca_img_format, output)
                            },error=function(e){
                              print_tm(r6$name, 'PCA: ERROR.')
                              print(e)
                            },finally={}
                            )
                          }
  })


  # Download associated tables
  output$download_pca_scores_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_scores_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_scores_table, file_name)
    }
  )
  output$download_pca_loadings_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_loadings_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_loadings_table, file_name)
    }
  )

  # Expanded boxes
  pca_proxy = plotly::plotlyProxy(outputId = "pca_plot",
                                  session = session)

  shiny::observeEvent(input$pca_plotbox,{
    if (input$pca_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#-----------------------------------------------------------FA composition  ----
fa_comp_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Fatty acid composition analysis: generating plot.")

  if (input$fa_comp_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_fa_comp(width = width,
                      height = height)
}

fa_comp_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fatty acid composition analysis: spawning plot.")

  output$fa_comp_plot = plotly::renderPlotly({
    r6$plots$fa_comp_plot
    plotly::config(r6$plots$fa_comp_plot, toImageButtonOptions = list(format = format,
                                                                          filename = timestamped_name('fa_comp'),
                                                                          height = NULL,
                                                                          width = NULL,
                                                                          scale = 1))
  })
}

fa_comp_ui = function(dimensions_obj, session) {
  # add function to show bs4dash with plotting function
  get_plotly_box(id = "fa_comp",
                 label = "Fatty acid composition analysis",
                 dimensions_obj = dimensions_obj,
                 session = session)
}

fa_comp_server = function(r6, input, output, session) {
  ns = session$ns
  print_tm(r6$name, "Fatty acid composition analysis index: START.")

  # set some UI
  output$fa_comp_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("fa_comp_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$fa_comp_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("fa_comp_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[, r6$params$fa_comp_plot$group_col]),
        selected = c(r6$params$fa_comp_plot$group_1, r6$params$fa_comp_plot$group_2),
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("fa_comp_selected_lipidclass"),
        label = "Select lipid class",
        choices = c("All", unique(r6$tables$feature_table$lipid_class)),
        selected = r6$params$fa_comp_plot$selected_lipidclass,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns('fa_comp_color_palette'),
        label = "Color palette",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$fa_comp_plot$color_palette,
        multiple = FALSE
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("fa_comp_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$fa_comp_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_fa_comp_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}


fa_comp_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  iv_fa_comp <- shinyvalidate::InputValidator$new()
  iv_fa_comp$add_rule("fa_comp_metacol", shinyvalidate::sv_required())
  iv_fa_comp$add_rule("fa_comp_metagroup", shinyvalidate::sv_required())
  iv_fa_comp$add_rule("fa_comp_selected_lipidclass", shinyvalidate::sv_required())
  iv_fa_comp$add_rule("fa_comp_color_palette", shinyvalidate::sv_required())
  iv_fa_comp$add_rule("fa_comp_img_format", shinyvalidate::sv_required())
  iv_fa_comp$add_rule("fa_comp_metacol",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$meta_column,
                      name_plot = r6$name,
                      message = "FA composition analysis: Incorrect group column selected!")
  # iv_fa_comp$add_rule("fa_comp_metagroup",
  #                     iv_check_select_input,
  #                     choices = unique(r6$tables$raw_meta[, r6$params$fa_comp_plot$group_col]),
  #                     name_plot = r6$name,
  #                     message = "FA composition analysis: Incorrect groups selected!")
  iv_fa_comp$add_rule("fa_comp_selected_lipidclass",
                      iv_check_select_input,
                      choices = c("All", unique(r6$tables$feature_table$lipid_class)),
                      name_plot = r6$name,
                      message = "FA composition analysis: Incorrect lipid class selected!")
  iv_fa_comp$add_rule("fa_comp_color_palette",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$color_palette,
                      name_plot = r6$name,
                      message = "FA composition analysis: Incorrect color palette selected!")
  iv_fa_comp$add_rule("fa_comp_img_format",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$image_format,
                      name_plot = r6$name,
                      message = "FA composition analysis: Incorrect image format selected!")

  # auto-update selected groups
  shiny::observeEvent(input$fa_comp_metacol, {
    if(r6$name == "Error") {
      output$fa_comp_message <- shiny::renderText({
        "Error! No data available!"
      })
    } else {
      shiny::updateSelectizeInput(
        inputId = "fa_comp_metagroup",
        session = session,
        choices = unique(r6$tables$raw_meta[, input$fa_comp_metacol]),
        selected = unique(r6$tables$raw_meta[, input$fa_comp_metacol])[c(1, 2)]
      )
    }
  })

  # Generate the plot
  shiny::observeEvent(
    c(shiny::req(length(input$fa_comp_metagroup) == 2),
      input$fa_comp_selected_lipidclass,
      input$fa_comp_color_palette,
      input$fa_comp_img_format),
    {
      shiny::req(iv_fa_comp$is_valid())

      if(r6$name == "Error") {
        output$fa_comp_message <- shiny::renderText({
          "Error! No data available!"
        })
      } else {
        print_tm(r6$name, "Fatty acid composition analysis: Updating params...")

        r6$param_fa_comp_plot(
          data_table = r6$tables$total_norm_data,
          sample_meta = r6$tables$raw_meta,
          feature_meta = r6$tables$feature_table,
          group_col = input$fa_comp_metacol,
          group_1 = input$fa_comp_metagroup[1],
          group_2 = input$fa_comp_metagroup[2],
          selected_lipidclass = input$fa_comp_selected_lipidclass,
          color_palette = input$fa_comp_color_palette,
          img_format = input$fa_comp_img_format
        )

        base::tryCatch({
          fa_comp_generate(r6, color_palette, dimensions_obj, input)
          fa_comp_spawn(r6, input$fa_comp_img_format, output)
        },
        error = function(e) {
          print_tm(r6$name, 'Fatty acid composition analysis error, missing data.')
          print(e)
        },
        finally = {}
        )
      }
    })

  # Download associated table
  output$download_fa_comp_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fa_composition_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fa_comp_table, file_name)
    }
  )

  # Expanded boxes
  fa_comp_proxy = plotly::plotlyProxy(outputId = "fa_comp_plot",
                                          session = session)

  shiny::observeEvent(input$fa_comp_plotbox,{
    if (input$fa_comp_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fa_comp_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fa_comp_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}