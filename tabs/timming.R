
timingTabUI <- function(id) {
  ns <- NS(id)
  duration_corr_condition <- sprintf("output['%s']", ns("isDurationCorrMatrixAvailable"))
  duration_condition <- sprintf("output['%s']", ns("isDuration"))
  tagList(
    h2("Duration Correlation Matrix"),
    conditionalPanel(
      duration_corr_condition,
      fixedRow(
        shinycssloaders::withSpinner(
          plotOutput(ns("durationCorrMatrixPlot"), width = "100%", height = "100%"), 
          type = 4
        )
      ),
      downloadButton(ns("downloadDurationCorrMatrixPlot"), "Download")
    ),
    conditionalPanel(duration_condition, 
                     h3("Histograms"),
                     shinycssloaders::withSpinner(uiOutput(ns("timingHistograms")), type = 4),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(imageOutput(ns("durationHist"), height = "100%"), type = 4),
                       shinycssloaders::withSpinner(imageOutput(ns("latenessHist"), height = "100%"), type = 4)
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton(ns("downloadDurationHist"), "Download"),
                       downloadButton(ns("downloadLatenessHist"), "Download")
                     )),
                     h3("Scatter diagrams"),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(imageOutput(ns("durationByID"), height = '100%'), type = 4),
                       shinycssloaders::withSpinner(imageOutput(ns("durationByFont"), height = '100%'), type = 4)
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton(ns("downlaodDurationByID"), "Download"),
                       downloadButton(ns("downlaodDurationByFont"), "Download")
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(imageOutput(ns("latenessByID"), height = '100%'), type = 4),
                       shinycssloaders::withSpinner(imageOutput(ns("latenessByFont"), height = '100%'), type = 4)
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton(ns("downlaodLatenessByID"), "Download"),
                       downloadButton(ns("downlaodLatenessByFont"), "Download")
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(imageOutput(ns("durationWithFontPadding"), height = '100%'), type = 4),
                       shinycssloaders::withSpinner(imageOutput(ns("latenessWithFontPadding"), height = '100%'), type = 4)
                     )),
                     fixedRow(splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton(ns("downlaodDurationWithFontPadding"), "Download"),
                       downloadButton(ns("downlaodLatenessWithFontPadding"), "Download")
                     )),
                     shinycssloaders::withSpinner(uiOutput(ns("scatterTime")),type=4),
                     shinycssloaders::withSpinner(uiOutput(ns("scatterTimeParticipant")),type=4)
    )
)
}
