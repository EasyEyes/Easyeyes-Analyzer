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
  downloadButton("downloadAll", "Download All"),
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
  fixedRow(
    div(style = "display: flex; align-items: center; margin-left:12px;",
        HTML('<div style="font-size: 16px; margin-right: 5px;">Show only spacingDeg thresholds with at least</div>'),
        numericInput('NQuestTrials', NULL, value = 10, min = 1, width = '80px'),
        HTML('<div style="font-size: 16px; margin-left: 5px;">trials.</div>')
    )
  ),
  h3("Histograms"),
  shinycssloaders::withSpinner(uiOutput("timingHistograms"), type = 4),
  h3("Scatter diagrams"),
  conditionalPanel('output.isDuration', 
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
  ),
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
  shinycssloaders::withSpinner(uiOutput('scatterTime'),type=4)
)
