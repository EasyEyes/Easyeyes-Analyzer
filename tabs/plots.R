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