library(plotly)

timingTab <- tabPanel(
  'Timing',
  tags$style(HTML("
    .textpoint text {
      text-anchor: start;
      white-space: pre;
    }
    @media print {
      .shiny-download-link {
        display: none;
      }
    }
  ")),
  
  conditionalPanel('output.isDuration', 
                   h3("Histograms"),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(imageOutput("durationHist", height = "100%"), type = 4),
                     shinycssloaders::withSpinner(imageOutput("latenessHist", height = "100%"), type = 4)
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downloadDurationHist", "Download"),
                     downloadButton("downloadLatenessHist", "Download")
                   )),
                   shinycssloaders::withSpinner(uiOutput("timingHistograms"), type = 4),
                   h3("Scatter diagrams"),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(imageOutput("durationByID", height = '100%'), type = 4),
                     shinycssloaders::withSpinner(imageOutput("durationByFont", height = '100%'), type = 4)
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downlaodDurationByID", "Download"),
                     downloadButton("downlaodDurationByFont", "Download")
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(imageOutput("latenessByID", height = '100%'), type = 4),
                     shinycssloaders::withSpinner(imageOutput("latenessByFont", height = '100%'), type = 4)
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downlaodLatenessByID", "Download"),
                     downloadButton("downlaodLatenessByFont", "Download")
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(imageOutput("durationWithFontPadding", height = '100%'), type = 4),
                     shinycssloaders::withSpinner(imageOutput("latenessWithFontPadding", height = '100%'), type = 4)
                   )),
                   fixedRow(splitLayout(
                     cellWidths = c("50%", "50%"),
                     downloadButton("downlaodDurationWithFontPadding", "Download"),
                     downloadButton("downlaodLatenessWithFontPadding", "Download")
                   )),
                   shinycssloaders::withSpinner(uiOutput('scatterTime'),type=4)
  )
)
