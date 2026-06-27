statTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    textOutput(ns("filename")),
    fluidRow(
      downloadButton(ns("thresholdOne"), "Download summary of each condition"),
      downloadButton(ns("thresholdTwo"), "Download thresholds (brief)"),
      downloadButton(ns("thresholdThree"), "Download individual results"),
      downloadButton(ns("participantInfoExcel"), "Download participant distance info")
    ),
    fluidRow(
      column(width = 12, align = "center", h3("Summary across participants"))
    ),
    fluidRow(
      column(width = 12, align = "center", tableOutput(ns("thresholdSummary")))
    ),
    fluidRow(
      column(12, align = "center", h3("Summary for each participant"))
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(tableOutput(ns("thresholdAll")), type = 4)
      )
    ),
    fluidRow(
      column(width = 12, align = "center", h3("Beauty and Comfort ratings"))
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(tableOutput(ns("ratings")), type = 4)
      )
    ),
    fluidRow(
      column(width = 12, align = "center", h3("Participant Information"))
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(tableOutput(ns("participantInfo")), type = 4)
      )
    ),
    fluidRow(
      column(width = 12, align = "center", h3("All questionAndAnswer"))
    ),
    fluidRow(
      column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(DT::dataTableOutput(ns("QA")), type = 4)
      )
    )
  )
}
