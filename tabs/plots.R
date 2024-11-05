library(plotly)



plotsTab <- tabPanel(
  'Plots',
  tags$style(HTML("
    .textpoint text {
      text-anchor: start;    /* Left-align each text line */
      white-space: pre;      /* Preserve whitespace */
    }
    /* Additional styles as needed for specific plot adjustments */
  ")),
 radioButtons(
    "fileType",
    "Select download file type:",
    c(
      "eps" = "eps",
      "pdf" = "pdf",
      "png" = "png",
      "svg" = "svg"
    ),
    inline = TRUE,
    selected = "png"
  ),
  radioButtons(
    "filterInput",
    "Select participants by reading speed:",
    c(
     'all' = 'all',
     'slowest 25%' = 'slowest',
     'fastest 75%' = 'fastest'
    ),
    inline = TRUE,
    selected = "all"
  ),
  
  conditionalPanel(condition='output.fileUploaded',
                   h3("Grade plots"),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(plotOutput("crowdingGradePlot", width = "100%", height = "100%"), type = 4),
                     shinycssloaders::withSpinner(plotOutput("rsvpGradePlot", width = "100%", height = "100%"), type = 4)
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadCrowdingGradePlot", "Download"),
                     downloadButton("downloadRsvpGradePlot", "Download")
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(plotOutput("acuityGradePlot", width = "100%", height = "100%"), type = 4)
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadAcuityGradePlot", "Download")
                   )
                   ),
  fixedRow( 
    shinycssloaders::withSpinner(
      plotOutput("corrMatrixPlot", width = "100%", height = "100%"), type = 4)),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadCorrMatrixPlot", "Download")
  ),
  #### histogram ####
  h3("Histograms"),
 shinycssloaders::withSpinner(uiOutput('histograms'),type=4),
  h3("Scatter diagrams"),
 shinycssloaders::withSpinner(uiOutput('scatters'),type=4),
  #### rsvp crowding plots ####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotlyOutput("rsvpCrowdingPeripheralAgePlot", height = '600px'), type = 4),
    shinycssloaders::withSpinner(plotlyOutput("rsvpCrowdingFovealAgePlot", height = '600px'), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRsvpCrowdingPeripheralAgePlot", "Download"),
    downloadButton("downloadRsvpCrowdingFovealAgePlot", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotlyOutput("rsvpCrowdingPeripheralGradePlot", height = '600px'), type = 4),
    shinycssloaders::withSpinner(plotlyOutput("rsvpCrowdingFovealGradePlot", height = '600px'), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRsvpCrowdingPeripheralGradePlot", "Download"),
    downloadButton("downloadRsvpCrowdingFovealGradePlot", "Download")
  ),
  
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotlyOutput("rsvpFovealAcuityAgePlot", height = '600px'), type = 4),
    shinycssloaders::withSpinner(plotlyOutput("rsvpFovealAcuityGradePlot", height = '600px'), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRsvpFovealAcuityAgePlot", "Download"),
    downloadButton("downloadRsvpFovealAcuityGradePlot", "Download")
  ),
  conditionalPanel('output.isPeripheralAcuity',
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(plotlyOutput("rsvpPeripheralAcuityAgePlot", height = '600px'), type = 4),
                     shinycssloaders::withSpinner(plotlyOutput("rsvpPeripheralAcuityGradePlot", height = '600px'), type = 4)
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadRsvpPeripheralAcuityAgePlot", "Download"),
                     downloadButton("downloadRsvpPeripheralAcuityGradePlot", "Download")
                   )
                   ),
  #### crowding ####
  h3("Crowding plots"),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("crowdingAvgPlot", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("crowdingScatterPlot", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadCrowdingAvgPlot", "Download"),
  #   downloadButton("downloadCrowdingScatterPlot", "Download")
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("SloanVsTimesMeanPlot", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("SloanVsTimesSDPlot", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadSloanVsTimesMeanPlot", "Download"),
  #   downloadButton("downloadSloanVsTimesSDPlot", "Download")
  # ),
  h3("Plots for Children"),
  uiOutput('plots'),
  #### regression ####
  h3("Reading vs crowding"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("regressionPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("regressionAcuityPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRegressionPlot", "Download"),
    downloadButton("downloadRegressionAcuityPlot", "Download")
  ),
  #### stairPlots #####
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("p1", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("p2", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadP1", "Download"),
  #   downloadButton("downloadP2", "Download")
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("p3", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadP3", "Download")
  # ),
  #### font size ####
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("readingVsXheightLog", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("readingVsXheightLinear", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadReadingVsXheightLog", "Download"),
  #   downloadButton("downloadReadingVsXheightLinear", "Download")
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("reading60cm1.2mmVs1.4mm", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("reading30cm1.2mmVs1.4mm", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadReading60cm1.2mmVs1.4mm", "Download"),
  #   downloadButton("downloadReading30cm1.2mmVs1.4mm", "Download")
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("reading60cmDiffVsAge", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadReading60cmDiffVsAge", "Download")
  # ),
  #### mean - median ####
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("meanPlot", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(plotOutput("medianPlot", width = "100%"), type = 4)
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadMeanPlot", "Download"),
  #   downloadButton("downloadMedianPlot", "Download")
  # ),

  #### regression fonts ####
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   shinycssloaders::withSpinner(plotOutput("regressionFontPlot", width = "100%"), type = 4),
  #   shinycssloaders::withSpinner(
  #     plotOutput("regressionFontPlotWithLabel", width = "100%"),
  #     type = 4
  #   )
  # ),
  # splitLayout(
  #   cellWidths = c("50%", "50%"),
  #   downloadButton("downloadRegressionFontPlot", "Download"),
  #   downloadButton("downloadRegressionFontPlotWithLabel", "Download")
  # ),
  #### retention ####
#   h3("Retention plots"),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     shinycssloaders::withSpinner(plotOutput("readingSpeedRetention", width = "100%"), type = 4)
#   ),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     downloadButton("downloadReadingSpeedRetention", "Download")
#   ),
#   h3("Test and Retest plots"),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     shinycssloaders::withSpinner(plotOutput("readingTestRetest", width = "100%"), type = 4),
#     shinycssloaders::withSpinner(plotOutput("crowdingTestRetest", width = "100%"), type = 4)
#   ),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     downloadButton("downloadReadingTestRetest", "Download"),
#     downloadButton("downloadCrowdingTestRetest", "Download")
#   ),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     shinycssloaders::withSpinner(plotOutput("rsvpReadingTestRetest", width = "100%"), type = 4)
#   ),
#   splitLayout(
#     cellWidths = c("50%", "50%"),
#     downloadButton("downloadRsvpReadingTestRetest", "Download")
#   )
)