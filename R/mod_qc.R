
#----------------------------------------------------------------- QC UI ----
qc_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidPage(
    # shiny::fluidRow(
    #   shiny::p("QC page imported data wil be shown here!"),
    # ),
    waiter::autoWaiter(html = waiter::spin_fading_circles(),
                       color = rgb(0, 0, 1, 0.5)),
    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::textOutput(outputId = ns("qc_c_imp_rsd_text")),
                    plotly::plotlyOutput(outputId = ns("qc_c_imp_rsd"))),
      shiny::column(width = 4,
                    plotly::plotlyOutput(outputId = ns("qc_c_imp_violin"))),
      shiny::column(width = 4,
                    plotly::plotlyOutput(outputId = ns("qc_c_imp_trend")))
    ),
    shiny::fluidRow(shiny::column(width = 12),
                    shiny::hr(width = "75%")),
    shiny::fluidRow(
      shiny::column(width = 4,
                    shiny::textOutput(outputId = ns("qc_p_imp_rsd_text")),
                    plotly::plotlyOutput(outputId = ns("qc_p_imp_rsd"))),
      shiny::column(width = 4,
                    plotly::plotlyOutput(outputId = ns("qc_p_imp_violin"))),
      shiny::column(width = 4,
                    plotly::plotlyOutput(outputId = ns("qc_p_imp_trend")))
    )
  )

}

#----------------------------------------------------------- QC server ----

qc_server = function(id, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      # Get lipidomics r6 object
      r6 <- module_controler$r6_exp
      m <- r6$name

      print_tm(m, "QC server started")

      # rownames(r6$tables$imp_data) contains the names om the samples (sampleId)
      # QC_C is the QC cells
      # QC_P is the QC plasma

      #### imported data ####
      ### Quality control cells
      output$qc_c_imp_rsd_text <- renderText({
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_cells_table) == 0) {
          return("No QC cell samples present!")
        } else {
          return(NULL)
        }
      })


      output$qc_c_imp_rsd <- plotly::renderPlotly({
        ## QC cells histogram
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # get the sample id's of the quality control cells
        qc_ids <- r6$tables$imp_meta[tolower(r6$tables$imp_meta$sampleType) == "quality control cells", "analystId"]

        # if nothing found
        if(nrow(r6$tables$qc_cells_table) == 0) {
          return(NULL)
        }

        qc_data <- r6$tables$qc_cells_table |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        qc_data <- data.frame(rsd = tapply(qc_data, qc_data$lipid, function(x) {
          sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
        }))

        p <- qc_histogram(data = qc_data,
                          title = "QC cells")

        return(p)
      })


      output$qc_c_imp_violin <- plotly::renderPlotly({
        ## QC cells violin
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_cells_table) == 0) {
          return(NULL)
        }

        qc_data <- r6$tables$qc_cells_table |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        qc_data <- data.frame(rsd = tapply(qc_data, qc_data$lipid, function(x) {
          sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
        }))
        qc_data$lipid <- rownames(qc_data)

        p <- qc_rsd_violin(data = qc_data,
                           title = "QC cells")

        return(p)
      })


      output$qc_c_imp_trend <- plotly::renderPlotly({
        ## QC cells trend
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_cells_table) == 0) {
          return(NULL)
        }

        # prepare the trend data
        qc_data <- qc_prep_trend(data = r6$tables$qc_cells_table)

        p <- qc_trend_plot(data = qc_data,
                           title = "Trend plot QC cells")

        return(p)
      })


      ### Quality control plasma
      output$qc_p_imp_rsd_text <- shiny::renderText({
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_plasma_table) == 0) {
          return("No QC plasma samples present!")
        } else {
          return(NULL)
        }
      })


      output$qc_p_imp_rsd <- plotly::renderPlotly({
        ## QC plasma histogram
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_plasma_table) == 0) {
          return(NULL)
        }

        qc_data <- r6$tables$qc_plasma_table |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        qc_data <- data.frame(rsd = tapply(qc_data, qc_data$lipid, function(x) {
          sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
        }))

        p <- qc_histogram(data = qc_data,
                          title = "QC plasma")

        return(p)
      })


      output$qc_p_imp_violin <- plotly::renderPlotly({
        ## QC plasma violin
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # if nothing found
        if(nrow(r6$tables$qc_plasma_table) == 0) {
          return(NULL)
        }

        qc_data <- r6$tables$qc_plasma_table |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        qc_data <- data.frame(rsd = tapply(qc_data, qc_data$lipid, function(x) {
          sd(x$value, na.rm = TRUE) / mean(x$value, na.rm = TRUE)
        }))
        qc_data$lipid <- rownames(qc_data)

        p <- qc_rsd_violin(data = qc_data,
                           title = "QC plasma")

        return(p)
      })


      output$qc_p_imp_trend <- plotly::renderPlotly({
        ## QC plasma trend
        req(r6$tables$imp_data,
            r6$tables$imp_meta)

        # get the sample id's of the quality control plasma
        qc_ids <- r6$tables$imp_meta[tolower(r6$tables$imp_meta$sampleType) == "quality control plasma", "analystId"]

        # if nothing found
        if(nrow(r6$tables$qc_plasma_table) == 0) {
          return(NULL)
        }

        # prepare the trend data
        qc_data <- qc_prep_trend(data = r6$tables$qc_plasma_table)

        p <- qc_trend_plot(data = qc_data,
                           title = "Trend plot QC plasma")

        return(p)

        return(NULL)
      })


    }
  )
}

