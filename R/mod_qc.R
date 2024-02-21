
#----------------------------------------------------------------- QC UI ----
qc_ui = function(id){
  ns = shiny::NS(id)
  shiny::fluidPage(
    shiny::fluidRow(
      shiny::p("QC page imported data wil be shown here!"),
    ),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::plotOutput(outputId = ns("qc_c_imp_rsd"))),
      shiny::column(width = 2),
      shiny::column(width = 5,
                    shiny::plotOutput(outputId = ns("qc_p_imp_rsd")))
    ),
    shiny::fluidRow(
      shiny::column(width = 5,
                    shiny::plotOutput(outputId = ns("qc_c_imp_trend"))),
      shiny::column(width = 2),
      shiny::column(width = 5,
                    shiny::plotOutput(outputId = ns("qc_p_imp_trend")))
    )
  )

}

#----------------------------------------------------------- QC server ----

qc_server = function(id, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      print("Rico: qc started")

      # Get lipidomics r6 object
      r6 <- module_controler$r6_exp

      # rownames(r6$tables$imp_data) contains the names om the samples (sampleId)
      # QC_C is the QC cells
      # QC_P is the QC plasma

      ## imported data
      output$qc_c_imp_rsd <- shiny::renderPlot({
        ## QC cells histogram

        req(r6$tables$imp_data)

        qc_data <- r6$tables$imp_data[grepl(x = r6$tables$imp_data$ID,
                                            pattern = "^QC_C.*"), ] |>
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

      output$qc_p_imp_rsd <- shiny::renderPlot({
        ## QC plasma histogram

        req(r6$tables$imp_data)

        qc_data <- r6$tables$imp_data[grepl(x = r6$tables$imp_data$ID,
                                            pattern = "^QC_P.*"), ] |>
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

      output$qc_c_imp_trend <- shiny::renderPlot({
        ## QC cells trend

        req(r6$tables$imp_data)

        qc_data <- r6$tables$imp_data[grepl(x = r6$tables$imp_data$ID,
                                            pattern = "^QC_C.*"), ] |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        ref_qc <- qc_data[qc_data$ID == "QC_C_1", ]
        colnames(ref_qc)[3] <- "ref_value"

        qc_data <- merge(
          x = qc_data,
          y = ref_qc[, c("lipid", "ref_value")],
          by = "lipid",
          all.x = TRUE
        )

        qc_data$log2fc <- log2(qc_data$value / qc_data$ref_value)

        p <- qc_trend_plot(data = qc_data,
                           title = "Trend plot QC cells")

        return(p)
      })


      output$qc_p_imp_trend <- shiny::renderPlot({
        ## QC plasma trend

        req(r6$tables$imp_data)

        qc_data <- r6$tables$imp_data[grepl(x = r6$tables$imp_data$ID,
                                            pattern = "^QC_P.*"), ] |>
          tidyr::pivot_longer(cols = -ID,
                              names_to = "lipid",
                              values_to = "value")

        ref_qc <- qc_data[qc_data$ID == "QC_P_1", ]
        colnames(ref_qc)[3] <- "ref_value"

        qc_data <- merge(
          x = qc_data,
          y = ref_qc[, c("lipid", "ref_value")],
          by = "lipid",
          all.x = TRUE
        )

        qc_data$log2fc <- log2(qc_data$value / qc_data$ref_value)

        p <- qc_trend_plot(data = qc_data,
                           title = "Trend plot QC plasma")

        return(p)

        return(NULL)
      })


    }
  )
}

