statTab <- tabPanel(
  'Stats',
    textOutput("filename"),
    fluidRow(
      downloadButton("thresholdOne", "Download Summary of each condition"),
      downloadButton("thresholdTwo", "Download Thresholds (brief)"),
      downloadButton("thresholdThree", "Download Thresholds")),
    fluidRow(column(12, align = "center", tableOutput('ex3'))),
    fluidRow(column(
      12, align = "center", h3("Summary across participants")
    )),
    fluidRow(column(12, align = "center", tableOutput('ex2'))),
    fluidRow(column(
      12, align = "center", h3("Summary for each participant")
    )),
    fluidRow(
      column(
        width = 12,
        align = "center",
        shinycssloaders::withSpinner(tableOutput('ex4'), type = 4)
      )
    )
)