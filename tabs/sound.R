#### Sound ####
soundTabUI <- function(id) {
  ns <- NS(id)
  json_uploaded_condition <- sprintf("output['%s']", ns("jsonUploaded"))
  tagList(
  fixedRow(column(
    width = 12,
    fileInput(
      ns("fileJSON"),
      NULL,
      accept = c(".json"),
      buttonLabel = "Upload json file",
      multiple = F,
      width = "1000px"
    )
  )),
  fixedRow(column(align = "left", width = 12, downloadButton(ns("downloadNotebook"), "Download jupyter notebook"))),
  fixedRow(column(
    width = 12,
    align = "left",
    radioButtons(
      ns("fileTypeSound"),
      "Select download plot type:",
      c(
        "pdf" = "pdf",
        "png" = "png",
        "eps" = "eps",
        "svg" = "svg"
      ),
      inline = TRUE,
      selected = "png"
    )
  )),
  fixedRow(column(
    width = 12, align = "center", h4("Sound Level at 1000 Hz")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    tableOutput(ns("sound table"))
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    h4(ns("Dynamic Range Compression Model"))
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    tableOutput(ns("Dynamic Range Compression Model"))
  )),      #### sound condition ####
  conditionalPanel(
  condition = json_uploaded_condition,
  #   fixedRow(
  #     column(
  #       width = 6,
  #       align = "center",
  #       shinycssloaders::withSpinner(plotOutput(
  #         "IRtmpFour", width = "100%", height = "95%"
  #       ),
  #       type = 4)
  #     ),
  #     column(
  #       width = 6,
  #       align = "center",
  #       shinycssloaders::withSpinner(plotOutput(
  #         "IRtmpFive", width = "100%", height = "95%"
  #       ),
  #       type = 4)
  #     )
  #   ),
    # fixedRow(
    #   column(
    #     width = 6,
    #     align = "center",
    #     downloadButton("downloadIRtmpFour", "Download")
    #   ),
    #   column(
    #     width = 6,
    #     align = "center",
    #     downloadButton("downloadIRtmpFive", "Download")
    #   )
    # ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput(ns("componentIIR0To10"), height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput(ns("componentIIR0To50"), height = "100%", width = "100%"),
          type = 4
        )
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIIR0To10"), "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIIR0To50"), "Download")
      )
    ),
    fixedRow(column(
      width = 6,
      align = "center",
      shinycssloaders::withSpinner(
        plotOutput(ns("componentIIR0To400"), height = "100%", width = "100%"),
        type = 4
      )
    )),
    fixedRow(column(
      width = 6,
      align = "center",
      downloadButton(ns("downloadComponentIIR0To400"), "Download")
    )),
    conditionalPanel(
    condition = json_uploaded_condition,
      fixedRow(
        column(
          width = 6,
          align = "center",
          shinycssloaders::withSpinner(
            plotOutput(ns("componentIRPSD"), height = "100%", width = "100%"),
            type = 4
          )
        ),
        column(
          width = 6,
          align = "center",
          shinycssloaders::withSpinner(
            plotOutput(ns("componentIR0To6"), height = "100%", width = "100%"),
            type = 4
          )
        ),
      ),
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIRPSD"), "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIR0To6"), "Download")
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput(ns("componentIR0To50"), height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput(ns("componentIR0To400"), height = "100%", width = "100%"),
          type = 4
        )
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIR0To50"), "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadComponentIR0To400"), "Download")
      )
    ),
    fixedRow(
      style = "margin-top:10px; margin-bottom:10px;",
      # column(
      #   width = 6,
      #   align = "center",
      #   shinycssloaders::withSpinner(
      #     plotOutput("cumSumPowerPlotSystem", height = "100%", width = "100%"),
      #     type = 4
      #   )
      # ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput(ns("cumSumPowerPlotComponent"), height = "100%", width = "100%"),
          type = 4
        )
      )
    ),
    fixedRow(
    #   column(
    #     width = 6,
    #     align = "center",
    #     downloadButton("downloadCumSumPowerPlotSystem", "Download")
    #   ),
      column(
        width = 6,
        align = "center",
        downloadButton(ns("downloadCumSumPowerPlotcomponent"), "Download")
      )
    ),
  ),
  
  ####  sound all ####
  conditionalPanel(
    condition = json_uploaded_condition,
    fixedRow(
      style = "margin-top:20px; margin-bottom:0px;",
      column(
        width = 6, align = "center",
        shinycssloaders::withSpinner(
          imageOutput(ns("sound_level_plot"), height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(width = 6, tags$div(
        h6(eq1_text, style = "padding-top:100px;"),
        # HTML("Based on Eq. 4 of Giannoulis et al. (2012)."),
        tags$a(href = reference, reference)
      )),
    )
  ),
  fixedRow(column(
    width = 6,
    align = "center",
    downloadButton(ns("downloadSoundLevelPlot"), "Download")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    shinycssloaders::withSpinner(
      imageOutput(ns("volume power variation"), width = "100%", height = "100%"),
      type = 4
    )
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton(ns("downloadVolumePowerVariation"), "Download")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    shinycssloaders::withSpinner(
      imageOutput(ns("power variation"), width = "100%", height = "100%"),
      type = 4
    )
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton(ns("downloadPowerVariation"), "Download")
  )),
  # fixedRow(column(
  #   width = 12,
  #   align = "center",
  #   shinycssloaders::withSpinner(imageOutput("record freq plot system", height = "100%"), type = 4)
  # )),
  # fixedRow(column(
  #   width = 12,
  #   align = "center",
  #   downloadButton("downloadRecordFreqPlotSystem", "Download")
  # )),
  fixedRow(column(
    width = 12,
    align = "center",
    shinycssloaders::withSpinner(imageOutput(ns("record freq plot component"), height = "100%"), type = 4)
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton(ns("downloadRecordFreqPlotComponent"), "Download")
  )),
  fluidRow(style = "padding-top:0px;",
           column(
             width = 12,
             align = "center",
             imageOutput(ns("autocorrelation"), height = "100%")
           )),
  fixedRow(
    width = 12,
    align = "center",
    actionButton(ns("do"), "Plot Autocorrelation")
  ),
  fixedRow(
    width = 12,
    align = "center",
    downloadButton(ns("downloadAutocorrelation"), "Download")
  )
  
)
}
