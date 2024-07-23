plotsTab <- tabPanel(
  'Plots',
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
  fixedRow(column(
    width = 4,
    fileInput(
      "pretestXLSX",
      NULL,
      accept = c(".xlsx"),
      buttonLabel = "Upload the pretest xlsx file",
      multiple = F
    )
  )),
  conditionalPanel(condition='output.fileUploaded',
                   h3("Grade plots"),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(plotOutput("crowdingGradePlot", width = "100%"), type = 4),
                     shinycssloaders::withSpinner(plotOutput("rsvpGradePlot", width = "100%"), type = 4)
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadCrowdingGradePlot", "Download"),
                     downloadButton("downloadRsvpGradePlot", "Download")
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(plotOutput("acuityGradePlot", width = "100%"), type = 4)
                   ),
                   splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadAcuityGradePlot", "Download")
                   ),
                  fixedRow( 
                    shinycssloaders::withSpinner(
                      plotOutput("corrMatrixPlot", width = "100%", height = "100%"), type = 4)),
                  splitLayout(
                    cellWidths = c("50%", "50%"),
                    downloadButton("downloadCorrMatrixPlot", "Download")
                  )
                   ),
  
  #### crowding ####
  h3("Crowding plots"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("crowdingAvgPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("crowdingScatterPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadCrowdingAvgPlot", "Download"),
    downloadButton("downloadCrowdingScatterPlot", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("SloanVsTimesMeanPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("SloanVsTimesSDPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadSloanVsTimesMeanPlot", "Download"),
    downloadButton("downloadSloanVsTimesSDPlot", "Download")
  ),
  #### rsvp crowding plots ####
  h3("RSVP vs Crowding plots"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("rsvpCrowdingPeripheralPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("rsvpCrowdingFovealPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRsvpCrowdingPeripheralPlot", "Download"),
    downloadButton("downloadRsvpCrowdingFovealPlot", "Download")
  ),
  h3("Plots for British Children"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("crowdingAgePlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("repeatedLetterAgePlot", width = "100%"), type = 4)
  ),
  #### stairPlots #####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("p1", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("p2", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadP1", "Download"),
    downloadButton("downloadP2", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("p3", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadP3", "Download")
  ),
  #### font size ####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("readingVsXheightLog", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("readingVsXheightLinear", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadReadingVsXheightLog", "Download"),
    downloadButton("downloadReadingVsXheightLinear", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("reading60cm1.2mmVs1.4mm", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("reading30cm1.2mmVs1.4mm", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadReading60cm1.2mmVs1.4mm", "Download"),
    downloadButton("downloadReading30cm1.2mmVs1.4mm", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("reading60cmDiffVsAge", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadReading60cmDiffVsAge", "Download")
  ),
  #### mean - median ####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("meanPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("medianPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadMeanPlot", "Download"),
    downloadButton("downloadMedianPlot", "Download")
  ),
  #### regression ####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("regressionPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("regressionAndMeanPlot", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRegressionPlot", "Download"),
    downloadButton("downloadRegressionAndMeanPlot", "Download")
  ),
  #### regression fonts ####
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("regressionFontPlot", width = "100%"), type = 4),
    shinycssloaders::withSpinner(
      plotOutput("regressionFontPlotWithLabel", width = "100%"),
      type = 4
    )
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRegressionFontPlot", "Download"),
    downloadButton("downloadRegressionFontPlotWithLabel", "Download")
  ),
  ####fluency ####
  h3("Fluency plots"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("fluencyHistogram", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadFluencyHistogram", "Download")
  ),
  #### retention ####
  h3("Retention plots"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("retentionHistogram", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("readingSpeedRetention", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRetentionHistogram", "Download"),
    downloadButton("downloadReadingSpeedRetention", "Download")
  ),
  h3("Test and Retest plots"),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("readingTestRetest", width = "100%"), type = 4),
    shinycssloaders::withSpinner(plotOutput("crowdingTestRetest", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadReadingTestRetest", "Download"),
    downloadButton("downloadCrowdingTestRetest", "Download")
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    shinycssloaders::withSpinner(plotOutput("rsvpReadingTestRetest", width = "100%"), type = 4)
  ),
  splitLayout(
    cellWidths = c("50%", "50%"),
    downloadButton("downloadRsvpReadingTestRetest", "Download")
  )
)