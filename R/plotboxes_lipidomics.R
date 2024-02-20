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


class_distribution_server = function(r6, output, session) {
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
  shiny::observeEvent(c(input$class_distribution_dataset, input$class_distribution_metacol, input$class_distribution_color_palette, input$class_distribution_img_format), {
    shiny::req(iv_class_distribution$is_valid())

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
class_comparison_server = function(r6, output, session) {

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


volcano_plot_server = function(r6, output, session) {

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

      shiny::selectizeInput(
        inputId = ns('volcano_plot_feature_metadata'),
        label = "Feature metadata",
        choices = c('None', colnames(r6$tables$feature_table)),
        selected = r6$params$volcano_plot$feature_metadata,
        multiple = FALSE
      ),
      # shiny::selectizeInput(
      #   inputId = ns('volcano_plot_annotation_terms'),
      #   label = "Feature annotations",
      #   choices = NULL,
      #   selected = NULL,
      #   multiple = TRUE
      # ),
      shinyWidgets::materialSwitch(
        inputId = ns('volcano_plot_keep_significant'),
        label = 'Keep only significant data',
        value = r6$params$volcano_plot$keep_significant,
        right = TRUE,
        status = "success"
      ),
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
  iv_volcano_plot$add_rule("volcano_plot_keep_significant", shinyvalidate::sv_optional())
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
  iv_volcano_plot$add_rule("volcano_plot_metagroup",
                           iv_check_select_input,
                           choices = unique(r6$tables$raw_meta[, r6$params$volcano_plot$group_col]),
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect group selected!")
  iv_volcano_plot$add_rule("volcano_plot_feature_metadata",
                           iv_check_select_input,
                           choices = c("None", unique(colnames(r6$tables$feature_table))),
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect feature metadata selected!")
  iv_volcano_plot$add_rule("volcano_plot_keep_significant",
                           iv_check_select_input,
                           choices = c(FALSE, TRUE),
                           name_plot = r6$name,
                           message = "Volcano plot: Incorrect keep significant selected!")
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
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[, input$volcano_plot_metacol]),
      selected = unique(r6$tables$raw_meta[, input$volcano_plot_metacol])[c(1, 2)]
    )
  })

  # shiny::observeEvent(input$volcano_plot_feature_metadata, {
  #   if (input$volcano_plot_feature_metadata %in% names(r6$tables$feature_list)) {
  #     shiny::updateSelectizeInput(
  #       inputId = "volcano_plot_annotation_terms",
  #       session = session,
  #       choices = r6$tables$feature_list[[input$volcano_plot_feature_metadata]]$feature_list,
  #       selected = character(0)
  #     )
  #   } else {
  #     shiny::updateSelectizeInput(
  #       inputId = "volcano_plot_annotation_terms",
  #       session = session,
  #       choices = NULL,
  #       selected = character(0)
  #     )
  #   }
  # })


  shiny::observeEvent(
    c(shiny::req(length(input$volcano_plot_metagroup) == 2),
      input$volcano_plot_auto_refresh,
      input$volcano_plot_tables,
      input$volcano_plot_function,
      input$volcano_plot_adjustment,
      input$volcano_plot_test,
      input$volcano_plot_displayed_plot,
      input$volcano_plot_feature_metadata,
      # input$volcano_plot_annotation_terms,
      input$volcano_plot_keep_significant,
      input$volcano_plot_color_palette,
      input$volcano_plot_p_val_threshold,
      input$volcano_plot_fc_threshold,
      input$volcano_plot_marker_size,
      input$volcano_plot_opacity,
      input$volcano_plot_img_format
    ), {
       shiny::req(iv_volcano_plot$is_valid(),
                 length(input$volcano_plot_metagroup) == 2)

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
                            keep_significant = input$volcano_plot_keep_significant,
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
      },error=function(e){
        print_tm(r6$name, 'Volcano plot: ERROR.')
      },finally={}
      )


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

#----------------------------------------------------------------- SI index ----
satindex_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Saturation index plot: generating plot.")

  if (input$satindex_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_satindex(width = width,
                   height = height)
}

satindex_spawn = function(r6, format, output) {
  print_tm(r6$name, "Saturation index: spawning plot.")

  output$satindex_plot = plotly::renderPlotly({
    r6$plots$satindex_plot
    plotly::config(r6$plots$satindex_plot, toImageButtonOptions = list(format = format,
                                                                       filename = timestamped_name('si_index'),
                                                                       height = NULL,
                                                                       width = NULL,
                                                                       scale = 1))
  })
}

satindex_ui = function(dimensions_obj, session) {
  # add function to show bs4dash with plotting function
  get_plotly_box(id = "satindex",
                 label = "Saturation index",
                 dimensions_obj = dimensions_obj,
                 session = session)
}

satindex_server = function(r6, output, session) {
  ns = session$ns
  print_tm(r6$name, "Saturation index: START.")

  # set some UI
  output$satindex_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("satindex_select_method"),
        label = "Select a method",
        choices = r6$hardcoded_settings$satindex$method,
        selected = r6$params$satindex_plot$method
      ),
      shiny::textOutput(outputId = ns("satindex_note")),
      shiny::selectInput(
        inputId = ns("satindex_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$satindex_plot$group_col
      ),
      shinyjs::hidden(shiny::selectizeInput(
        inputId = ns("satindex_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[,r6$params$satindex_plot$group_col]),
        selected = c(r6$params$satindex_plot$group_1, r6$params$satindex_plot$group_2),
        multiple = TRUE
      )),
      shinyjs::hidden(shiny::selectizeInput(
        inputId = ns("satindex_lipidclass"),
        label = "Select lipid class",
        choices = unique(r6$tables$feature_table$lipid_class),
        selected = r6$params$satindex_plot$selected_lipid_class,
        multiple = FALSE
      )),
      shiny::selectizeInput(
        inputId = ns('satindex_color_palette'),
        label = "Color palette",
        choices = r6$hardcoded_settings$color_palette,
        selected = r6$params$satindex_plot$color_palette,
        multiple = FALSE
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("satindex_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$satindex_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_satindex_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

satindex_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  # input validation
  iv_satindex <- shinyvalidate::InputValidator$new()
  iv_satindex$add_rule("satindex_select_method", shinyvalidate::sv_required())
  iv_satindex$add_rule("satindex_metacol", shinyvalidate::sv_required())
  iv_satindex$add_rule("satindex_metagroup", shinyvalidate::sv_required())
  iv_satindex$add_rule("satindex_lipidclass", shinyvalidate::sv_required())
  iv_satindex$add_rule("satindex_color_palette", shinyvalidate::sv_optional())
  iv_satindex$add_rule("satindex_img_format", shinyvalidate::sv_optional())
  iv_satindex$add_rule("satindex_select_method",
                       iv_check_select_input,
                       choices = r6$hardcoded_settings$satindex$method,
                       name_plot = r6$name,
                       message = "SI index: Incorrect method selected!")
  iv_satindex$add_rule("satindex_metacol",
                       iv_check_select_input,
                       choices = r6$hardcoded_settings$meta_column,
                       name_plot = r6$name,
                       message = "SI index: Incorrect group column selected!")
  iv_satindex$add_rule("satindex_metagroup",
                       iv_check_select_input,
                       choices = unique(r6$tables$raw_meta[, r6$params$satindex_plot$group_col]),
                       name_plot = r6$name,
                       message = "SI index: Incorrect group(s) selected!")
  iv_satindex$add_rule("satindex_lipidclass",
                       iv_check_select_input,
                       choices = unique(r6$tables$feature_table$lipid_class),
                       name_plot = r6$name,
                       message = "SI index: Incorrect group(s) selected!")
  iv_satindex$add_rule("satindex_color_palette",
                       iv_check_select_input,
                       choices = r6$hardcoded_settings$color_palette,
                       name_plot = r6$name,
                       message = "SI index: Incorrect color palette selected!")
  iv_satindex$add_rule("satindex_img_format",
                       iv_check_select_input,
                       choices = r6$hardcoded_settings$image_format,
                       name_plot = r6$name,
                       message = "SI index: Incorrect image format selected!")

  # Generate the plot
  shiny::observeEvent(c(shiny::req(length(input$satindex_metagroup) == 2),
                        input$satindex_metacol,
                        input$satindex_img_format,
                        input$satindex_select_method,
                        input$satindex_metagroup,
                        input$satindex_color_palette,
                        input$satindex_lipidclass), {
                          shiny::req(iv_satindex$is_valid())

                          print_tm(r6$name, "Saturation index: Updating params...")

                          if(input$satindex_select_method == "ratio") {
                            output$satindex_note <- shiny::renderText({
                              "Ref:  Cell Rep. 2018 Sep 4;24(10):2596-2605"
                            })
                          } else {
                            output$satindex_note <- shiny::renderText({
                              ""
                            })
                          }

                          # show / hide the group selection
                          if(input$satindex_select_method == "double bond") {
                            shinyjs::show(id = "satindex_metagroup")
                            shinyjs::show(id = "satindex_lipidclass")
                            shinyjs::hide(id = "satindex_color_palette")
                          } else {
                            shinyjs::hide(id = "satindex_metagroup")
                            shinyjs::hide(id = "satindex_lipidclass")
                            shinyjs::show(id = "satindex_color_palette")
                          }

                          r6$param_satindex_plot(data_table = r6$tables$raw_data,
                                                 feature_meta = r6$tables$feature_table,
                                                 sample_meta = r6$tables$raw_meta,
                                                 group_col = input$satindex_metacol,
                                                 group_1 = input$satindex_metagroup[1],
                                                 group_2 = input$satindex_metagroup[2],
                                                 selected_lipid_class = input$satindex_lipidclass,
                                                 color_palette = input$satindex_color_palette,
                                                 method = input$satindex_select_method,
                                                 img_format = input$satindex_img_format)

                          base::tryCatch({
                            satindex_generate(r6, color_palette, dimensions_obj, input)
                            satindex_spawn(r6, input$satindex_img_format, output)
                          },
                          error = function(e) {
                            print_tm(r6$name, 'Saturation index error, missing data.')
                            print(e)
                          },
                          finally = {}
                          )
                        })

  # Download associated table
  output$download_satindex_table = shiny::downloadHandler(
    filename = function() {
      if(input$satindex_select_method == "db") {
        timestamped_name(paste(input$satindex_select_method, input$satindex_lipidclass, "si_table.csv", sep = "_"))
      } else {
        timestamped_name(paste(input$satindex_select_method, "si_table.csv", sep = "_"))
      }
    },
    content = function(file_name){
      write.csv(r6$tables$satindex_table, file_name)
    }
  )

  # Expanded boxes
  satindex_proxy = plotly::plotlyProxy(outputId = "satindex_plot",
                                       session = session)

  shiny::observeEvent(input$satindex_plotbox,{
    if (input$satindex_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = satindex_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = satindex_proxy,
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

fa_analysis_server = function(r6, output, session) {
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
      shiny::selectInput(
        inputId = ns("fa_analysis_pathway"),
        label = "Select pathway",
        choices = r6$hardcoded_settings$fa_analysis$pathway,
        selected = r6$params$fa_analysis_plot$pathway,
        multiple = TRUE,
        width = "100%"),
      shiny::selectizeInput(
        inputId = ns("fa_analysis_selected_lipidclass"),
        label = "Select lipid class",
        choices = c("All", unique(r6$tables$feature_table$lipid_class)[!(unique(r6$tables$feature_table$lipid_class) %in% c("PA", "TG"))]),
        selected = r6$params$fa_analysis$selected_lipidclass,
        multiple = FALSE
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
}

fa_analysis_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  iv_fa_analysis <- shinyvalidate::InputValidator$new()
  iv_fa_analysis$add_rule("fa_analysis_metacol", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_pathway", shinyvalidate::sv_optional())
  iv_fa_analysis$add_rule("fa_analysis_selected_lipidclass", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_color_palette", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_img_format", shinyvalidate::sv_required())
  iv_fa_analysis$add_rule("fa_analysis_metacol",
                          iv_check_select_input,
                          choices = r6$hardcoded_settings$meta_column,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect group column selected!")
  iv_fa_analysis$add_rule("fa_analysis_pathway",
                          iv_check_select_input,
                          choices = r6$hardcoded_settings$fa_analysis$pathway,
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect pathway(s) selected!")
  iv_fa_analysis$add_rule("fa_analysis_selected_lipidclass",
                          iv_check_select_input,
                          choices = c("All", unique(r6$tables$feature_table$lipid_class)[!(unique(r6$tables$feature_table$lipid_class) %in% c("PA", "TG"))]),
                          name_plot = r6$name,
                          message = "FA analysis: Incorrect pathway(s) selected!")
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
                        input$fa_analysis_pathway,
                        input$fa_analysis_selected_lipidclass,
                        input$fa_analysis_color_palette,
                        input$fa_analysis_img_format), {
    shiny::req(iv_fa_analysis$is_valid())

    print_tm(r6$name, "Fatty acid analysis: Updating params...")

    r6$param_fa_analysis_plot(data_table = r6$tables$raw_data,
                              feature_meta = r6$tables$feature_table,
                              sample_meta = r6$tables$raw_meta,
                              group_col = input$fa_analysis_metacol,
                              pathway = input$fa_analysis_pathway,
                              selected_lipidclass = input$fa_analysis_selected_lipidclass,
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


heatmap_server = function(r6, output, session) {

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
          shinyWidgets::switchInput(inputId = ns("heatmap_impute"),
                                    label = "Impute missing",
                                    value = r6$params$heatmap$imputation,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
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
            label = "Map sample data",
            multiple = TRUE,
            choices = r6$hardcoded_settings$meta_column,
            selected = r6$params$heatmap$map_sample_data
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("heatmap_map_cols"),
            label = "Map feature data",
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
                                label = "Group column",
                                choices = r6$hardcoded_settings$meta_column,
                                selected = r6$params$heatmap$group_column_da,
                                multiple = FALSE,
                                width = "100%")
        ),
        shiny::column(
          width = 6,
          shiny::sliderInput(inputId = ns("heatmap_alpha_da"),
                             label = "Alpha",
                             min = 0,
                             max = 0.99,
                             value = r6$params$heatmap$alpha_da,
                             step = 0.01,
                             width = "100%")
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
        ),
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
  iv_heatmap$add_rule("heatmap_map_cols",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$heatmap$map_cols,
                      name_plot = r6$name,
                      message = "Heatmap: Incorrect map feature data selected!")
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

  shiny::observeEvent(c(input$heatmap_run,
                        input$heatmap_img_format), {
    req(input$heatmap_run)
    shinyjs::disable("heatmap_run")
    print_tm(r6$name, "Heatmap: Updating params...")

    r6$param_heatmap(dataset = input$heatmap_dataset,
                     impute = input$heatmap_impute,
                     cluster_samples = input$heatmap_cluster_samples,
                     cluster_features = input$heatmap_cluster_features,
                     map_sample_data = input$heatmap_map_rows,
                     map_feature_data = input$heatmap_map_cols,
                     group_column_da = input$heatmap_group_col_da,
                     apply_da = input$heatmap_apply_da,
                     alpha_da = input$heatmap_alpha_da,
                     color_palette = input$heatmap_colors_palette,
                     reverse_palette = input$heatmap_reverse_palette,
                     img_format = input$heatmap_img_format)

    base::tryCatch({
      heatmap_generate(r6, color_palette, dimensions_obj, input)
      heatmap_spawn(r6, input$heatmap_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Heatmap: ERROR.')
      print(e)
      shinyjs::enable("heatmap_run")
    },finally={}
    )

    shinyjs::enable("heatmap_run")
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


pca_server = function(r6, output, session) {

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
        label = "Sample group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$pca$sample_groups_col
      ),
      shiny::selectInput(
        inputId = ns("pca_feature_group"),
        label = "Feature group column",
        choices = colnames(r6$tables$feature_table),
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
      shinyWidgets::prettySwitch(
        inputId = ns('pca_completeObs'),
        label = 'Complete observations',
        value = r6$params$pca$completeObs
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
    shiny::req(iv_pca$is_valid())

    print("Rico: check DA")
    print(input$pca_apply_da)

    if (!input$pca_auto_refresh) {
      r6$params$pca$auto_refresh = input$pca_auto_refresh
      return()
    }

    print_tm(r6$name, "PCA: Updating params...")

    print(input$pca_data_table)
    r6$param_pca(auto_refresh = input$pca_auto_refresh,
                 data_table = input$pca_data_table,
                 sample_groups_col = input$pca_sample_groups_col,
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
    },finally={}
    )

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


#-------------------------------------------------------- Double bonds plot ----
double_bonds_generate_single = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Double bonds plot: generating plot.")

  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_dbplot_table_single(data_table = table_switch(input$double_bonds_dataset, r6),
                             dbplot_table = r6$tables$feature_table,
                             col_group = input$double_bonds_metacol,
                             used_function = input$double_bonds_function,
                             group_1 = input$double_bonds_metagroup[1])

  r6$plot_doublebonds_single(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             group_1 = input$double_bonds_metagroup[1],
                             width = width,
                             height = height)
}

double_bonds_generate_double = function(r6, colour_list, dimensions_obj, input, session) {
  print_tm(r6$name, "Double bonds plot: generating plot.")
  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_dbplot_table_double(data_table = table_switch(input$double_bonds_dataset, r6),
                             dbplot_table = r6$tables$feature_table,
                             col_group = input$double_bonds_metacol,
                             used_function =  input$double_bonds_function,
                             test = input$double_bonds_test,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2])

  selected_rows = rownames(r6$tables$dbplot_table)[r6$tables$dbplot_table["lipid_class"] == input$double_bonds_class]

  fc_limits = round(max(abs(r6$tables$dbplot_table[selected_rows, "log2_fold_change"])), 1) + 1
  r6$params$db_plot$fc_range = c(-fc_limits, fc_limits)
  if (fc_limits > 1) {
    r6$params$db_plot$fc_values = c(-1, 1)
  } else {
    r6$params$db_plot$fc_values = c(0, 0)
  }

  r6$params$db_plot$pval_range = c(0, round(max(r6$tables$dbplot_table[selected_rows, adjustment_switch(input$double_bonds_plot_adjustment)]), 1) + 1)
  r6$params$db_plot$pval_values = c(0, round(max(r6$tables$dbplot_table[selected_rows, adjustment_switch(input$double_bonds_plot_adjustment)]), 1) + 1)

  shiny::updateSliderInput(
    session = session,
    inputId = "log2_fc_slider",
    min = r6$params$db_plot$fc_range[1],
    max = r6$params$db_plot$fc_range[2],
    value = r6$params$db_plot$fc_values,
  )

  shiny::updateSliderInput(
    session = session,
    inputId = "min_log10_bh_pval_slider",
    max = r6$params$db_plot$pval_range[2],
    value = r6$params$db_plot$pval_values
  )

  r6$plot_doublebonds_double(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             adjustment = adjustment_switch(input$double_bonds_plot_adjustment),
                             fc_limits = input$log2_fc_slider,
                             pval_limits = input$min_log10_bh_pval_slider,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2],
                             width = width,
                             height = height)
}


double_bonds_generate_double_sliders = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Double bonds plot: generating plot.")
  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_doublebonds_double(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             adjustment = adjustment_switch(input$double_bonds_plot_adjustment),
                             fc_limits = input$log2_fc_slider,
                             pval_limits = input$min_log10_bh_pval_slider,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2],
                             width = width,
                             height = height)
}



double_bonds_spawn = function(r6, format, output) {
  print_tm(r6$name, "Double bonds plot: spawning plot.")
  output$double_bonds_plot = plotly::renderPlotly({
    r6$plots$double_bond_plot
    plotly::config(r6$plots$double_bond_plot, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_name('double_bond_plot'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}



double_bonds_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "double_bonds",
                 label = "Double bonds plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


double_bonds_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Double bonds plot: START.")

  output$double_bonds_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("double_bonds_dataset"),
        label = "Select data table",
        choices = r6$hardcoded_settings$db_plot$datasets,
        selected = r6$params$db_plot$dataset
      ),
      shiny::selectInput(
        inputId = ns("double_bonds_metacol"),
        label = "Select group column",
        choices = r6$hardcoded_settings$meta_column,
        selected = r6$params$db_plot$group_column
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_metagroup"),
        label = "Select group(s)",
        choices = unique(r6$tables$raw_meta[,r6$params$db_plot$group_column]),
        selected = r6$params$db_plot$selected_groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_class"),
        label = "Select lipid class",
        choices = unique(r6$tables$feature_table$lipid_class),
        selected = r6$params$db_plot$selected_lipid_class,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_carbon_select"),
        label = "Carbon count",
        choices = r6$hardcoded_settings$db_plot$carbon_select,
        selected = r6$params$db_plot$selected_carbon_chain,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_unsat_select"),
        label = "Unsaturation count",
        choices = r6$hardcoded_settings$db_plot$unsat_select,
        selected = r6$params$db_plot$selected_unsat,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_function"),
        label = "FC function",
        choices = r6$hardcoded_settings$db_plot$calc_func,
        selected = r6$params$db_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_test"),
        label = "Select test",
        choices = r6$hardcoded_settings$db_plot$test_func,
        selected = r6$params$db_plot$selected_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_plot_adjustment"),
        label = "Select adjustment",
        choices = r6$hardcoded_settings$db_plot$adjustment_func,
        selected = r6$params$db_plot$adjustment,
        multiple = FALSE
      ),
      shiny::sliderInput(
        inputId = ns("log2_fc_slider"),
        label = "Coloring : Log2(Fold change) (exlude)",
        min = r6$params$db_plot$fc_range[1],
        max = r6$params$db_plot$fc_range[2],
        value = r6$params$db_plot$fc_values,
        step = 0.1
      ),
      shiny::sliderInput(
        inputId = ns("min_log10_bh_pval_slider"),
        label = "Size : -Log10(p-value)",
        min = r6$params$db_plot$pval_range[1],
        max = r6$params$db_plot$pval_range[2],
        value = r6$params$db_plot$pval_values,
        step = 0.1
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("double_bonds_plot_img_format"),
        label = "Image format",
        choices = r6$hardcoded_settings$image_format,
        selected = r6$params$db_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_double_bond_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

db_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {
  iv_db_plot <- shinyvalidate::InputValidator$new()
  iv_db_plot$add_rule("double_bonds_dataset", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_metacol", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_metagroup", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_class", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_carbon_select", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_unsat_select", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_function", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_test", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_plot_adjustment", shinyvalidate::sv_required())
  iv_db_plot$add_rule("log2_fc_slider", shinyvalidate::sv_required())
  iv_db_plot$add_rule("min_log10_bh_pval_slider", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_plot_img_format", shinyvalidate::sv_required())
  iv_db_plot$add_rule("double_bonds_dataset",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$datasets,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect data set selected!")
  iv_db_plot$add_rule("double_bonds_metacol",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$meta_column,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect group column selected!")
  iv_db_plot$add_rule("double_bonds_metagroup",
                      iv_check_select_input,
                      choices = unique(r6$tables$raw_meta[, r6$params$db_plot$group_col]),
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect group(s) selected!")
  iv_db_plot$add_rule("double_bonds_class",
                      iv_check_select_input,
                      choices = unique(r6$tables$feature_table$lipid_class),
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect lipid class selected!")
  iv_db_plot$add_rule("double_bonds_carbon_select",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$carbon_select,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect carbon count selected!")
  iv_db_plot$add_rule("double_bonds_unsat_select",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$unsat_select,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect unsaturation count selected!")
  iv_db_plot$add_rule("double_bonds_function",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$calc_func,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect FC function selected!")
  iv_db_plot$add_rule("double_bonds_test",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$test_func,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect test function selected!")
  iv_db_plot$add_rule("double_bonds_plot_adjustment",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$db_plot$adjustment_func,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect adjustment function selected!")
  iv_db_plot$add_rule("double_bonds_plot_img_format",
                      iv_check_select_input,
                      choices = r6$hardcoded_settings$image_format,
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect image format selected!")
  iv_db_plot$add_rule("log2_fc_slider",
                      iv_check_numeric_range,
                      check_range = c(r6$params$db_plot$fc_range[1], r6$params$db_plot$fc_range[2]),
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect FC range set!")
  iv_db_plot$add_rule("min_log10_bh_pval_slider",
                      iv_check_numeric_range,
                      check_range = c(r6$params$db_plot$pval_range[1], r6$params$db_plot$pval_range[2]),
                      name_plot = r6$name,
                      message = "Double bond plot: Incorrect p-value range set!")

  # Group col selection
  shiny::observeEvent(input$double_bonds_metacol,{
    shiny::updateSelectizeInput(
      inputId = "double_bonds_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$double_bonds_metacol]),
      selected = unique(r6$tables$raw_meta[,input$double_bonds_metacol])[c(1,2)]
    )
  })

  # Double bonds plot SINGLE
  shiny::observeEvent(c(shiny::req(length(input$double_bonds_metagroup) == 1), input$double_bonds_class, input$double_bonds_dataset, input$double_bonds_function, input$double_bonds_plot_img_format, input$double_bonds_carbon_select, input$double_bonds_unsat_select), {
    shiny::req(iv_db_plot$is_valid())

    print_tm(r6$name, "Double bonds plot single: Updating params...")

    r6$params$db_plot$dataset = input$double_bonds_dataset
    r6$params$db_plot$group_column = input$double_bonds_metacol
    r6$params$db_plot$selected_groups = input$double_bonds_metagroup
    r6$params$db_plot$selected_lipid_class = input$double_bonds_class
    r6$params$db_plot$selected_carbon_chain = input$double_bonds_carbon_select
    r6$params$db_plot$selected_unsat = input$double_bonds_unsat_select
    r6$params$db_plot$selected_function = input$double_bonds_function
    r6$params$db_plot$img_format = input$double_bonds_plot_img_format

    base::tryCatch({
      double_bonds_generate_single(r6, color_palette, dimensions_obj, input)
      double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Double bonds plot: error, missing data.')
    },finally={}
    )

  })

  # Double bonds plot DOUBLE : Non-slider events
  shiny::observeEvent(c(input$double_bonds_dataset, input$double_bonds_metagroup, input$double_bonds_class, input$double_bonds_function, input$double_bonds_plot_adjustment, input$double_bonds_test, input$double_bonds_plot_img_format, input$double_bonds_carbon_select, input$double_bonds_unsat_select),{
    shiny::req(length(input$double_bonds_metagroup) == 2,
               iv_db_plot$is_valid())

    print_tm(r6$name, "Double bonds plot non-sliders: Updating params...")

    r6$params$db_plot$dataset = input$double_bonds_dataset
    r6$params$db_plot$group_column = input$double_bonds_metacol
    r6$params$db_plot$selected_groups = input$double_bonds_metagroup
    r6$params$db_plot$selected_lipid_class = input$double_bonds_class
    r6$params$db_plot$selected_carbon_chain = input$double_bonds_carbon_select
    r6$params$db_plot$selected_unsat = input$double_bonds_unsat_select
    r6$params$db_plot$selected_function = input$double_bonds_function
    r6$params$db_plot$adjustment = input$double_bonds_plot_adjustment
    r6$params$db_plot$selected_test = input$double_bonds_test
    r6$params$db_plot$img_format = input$double_bonds_plot_img_format

    double_bonds_generate_double(r6, colour_list, dimensions_obj, input, session)
    double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)

  })

  # Double bonds plot DOUBLE : Slider events
  shiny::observeEvent(c(input$log2_fc_slider, input$min_log10_bh_pval_slider),{
    shiny::req(length(input$double_bonds_metagroup) == 2,
               iv_db_plot$is_valid())

    print_tm(r6$name, "Double bonds plot sliders: Updating params...")

    r6$params$db_plot$fc_values = input$log2_fc_slider
    r6$params$db_plot$pval_values = input$min_log10_bh_pval_slider

    double_bonds_generate_double_sliders(r6, colour_list, dimensions_obj, input)
    double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)
  })

  # Download associated tables
  output$download_double_bond_table = shiny::downloadHandler(
    filename = function(){timestamped_name("double_bond_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$dbplot_table, file_name)
    }
  )

  # Expanded boxes
  double_bonds_proxy = plotly::plotlyProxy(outputId = "double_bonds_plot",
                                           session = session)

  shiny::observeEvent(input$double_bonds_plotbox,{
    if (input$double_bonds_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}