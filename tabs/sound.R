#### Sound ####
soundTab <- tabPanel(
  'Sound',
  fixedRow(column(
    width = 12,
    fileInput(
      "fileJSON",
      NULL,
      accept = c(".json"),
      buttonLabel = "Upload json file",
      multiple = F,
      width = "1000px"
    )
  )),
  fixedRow(column(align = "left", width = 12, downloadButton("downloadNotebook", "Download jupyter notebook"))),
  fixedRow(column(
    width = 12,
    align = "left",
    radioButtons(
      "fileTypeSound",
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
    tableOutput('sound table')
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    h4("Dynamic Range Compression Model")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    tableOutput('Dynamic Range Compression Model')
  )),      #### sound condition ####
  conditionalPanel(
    condition = "output.jsonUploaded",
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(plotOutput(
          "IRtmpFour", width = "100%", height = "95%"
        ),
        type = 4)
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(plotOutput(
          "IRtmpFive", width = "100%", height = "95%"
        ),
        type = 4)
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton("downloadIRtmpFour", "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton("downloadIRtmpFive", "Download")
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIIR0To10", height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIIR0To50", height = "100%", width = "100%"),
          type = 4
        )
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIIR0To10", "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIIR0To50", "Download")
      )
    ),
    fixedRow(column(
      width = 6,
      align = "center",
      shinycssloaders::withSpinner(
        plotOutput("componentIIR0To400", height = "100%", width = "100%"),
        type = 4
      )
    )),
    fixedRow(column(
      width = 6,
      align = "center",
      downloadButton("downloadComponentIIR0To400", "Download")
    )),
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIRPSD", height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIR0To6", height = "100%", width = "100%"),
          type = 4
        )
      ),
    ),
    
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIRPSD", "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIR0To6", "Download")
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIR0To50", height = "100%", width = "100%"),
          type = 4
        )
      ),
      column(
        width = 6,
        align = "center",
        shinycssloaders::withSpinner(
          plotOutput("componentIR0To400", height = "100%", width = "100%"),
          type = 4
        )
      )
    ),
    fixedRow(
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIR0To50", "Download")
      ),
      column(
        width = 6,
        align = "center",
        downloadButton("downloadComponentIR0To400", "Download")
      )
    ),
    fixedRow(
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
          plotOutput("cumSumPowerPlotComponent", height = "100%", width = "100%"),
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
        downloadButton("downloadCumSumPowerPlotcomponent", "Download")
      )
    ),
  ),
  
  ####  sound all ####
  fixedRow(
    column(
      width = 6,
      align = "center",
      shinycssloaders::withSpinner(
        imageOutput("sound level plot", width = "100%", height = "100%"),
        type = 4
      )
    ),
    column(width = 6, tags$div(
      h6(eq1_text, style = "padding-top:100px;"),
      # HTML("Based on Eq. 4 of Giannoulis et al. (2012)."),
      tags$a(href = reference, reference)
    )),
  ),
  fixedRow(column(
    width = 6,
    align = "center",
    downloadButton("downloadSoundLevelPlot", "Download")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    shinycssloaders::withSpinner(
      imageOutput("volume power variation", width = "100%", height = "100%"),
      type = 4
    )
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton("downloadVolumePowerVariation", "Download")
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    shinycssloaders::withSpinner(
      imageOutput("power variation", width = "100%", height = "100%"),
      type = 4
    )
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton("downloadPowerVariation", "Download")
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
    shinycssloaders::withSpinner(imageOutput("record freq plot component", height = "100%"), type = 4)
  )),
  fixedRow(column(
    width = 12,
    align = "center",
    downloadButton("downloadRecordFreqPlotComponent", "Download")
  )),
  fluidRow(style = "padding-top:0px;",
           column(
             width = 12,
             align = "center",
             imageOutput("autocorrelation", height = "100%")
           )),
  fixedRow(
    width = 12,
    align = "center",
    actionButton("do", "Plot Autocorrelation")
  ),
  fixedRow(
    width = 12,
    align = "center",
    downloadButton("downloadAutocorrelation", "Download")
  )
)

  