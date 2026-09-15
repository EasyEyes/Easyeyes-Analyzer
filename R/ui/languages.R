# UI for the Languages tab.
#
# Renders seven plots (one line per language) plus a per-plot Download button.
# Plot cells match the Plots tab: imageOutput + spinner + Download.

languagesTabUI <- function(id) {
  ns <- NS(id)

  plot_block <- function(plot_id, download_id) {
    tags$div(
      shinycssloaders::withSpinner(
        imageOutput(ns(plot_id), width = "100%", height = "100%"),
        type = 4
      ),
      downloadButton(ns(download_id), "Download")
    )
  }

  tagList(
    fluidRow(
      column(
        12,
        h3("Languages"),
        br()
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      style = "overflow-x: hidden;",
      plot_block("readingSpeedPlot", "downloadReadingSpeed"),
      plot_block("readingProportionCorrectPlot", "downloadReadingProportionCorrect")
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      style = "overflow-x: hidden;",
      plot_block("rsvpSpeedPlot", "downloadRsvpSpeed"),
      plot_block("crowdingPlot", "downloadCrowding")
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      style = "overflow-x: hidden;",
      plot_block("comfortPlot", "downloadComfort"),
      plot_block("beautyPlot", "downloadBeauty")
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      style = "overflow-x: hidden;",
      plot_block("familiarityPlot", "downloadFamiliarity"),
      div()
    )
  )
}
