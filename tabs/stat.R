statTab <- tabPanel(
  'Stats',
  textOutput("filename"),
  fluidRow(
    column(
      width = 2,
      downloadButton("thresholdOne", "Download all participant")
    ),
    column(
      width = 2,
      downloadButton("thresholdTwo", "Download each participant")
    ),
    column(
      width = 2,
      downloadButton("thresholdThree", "Download all threshold")
    )
  ),
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