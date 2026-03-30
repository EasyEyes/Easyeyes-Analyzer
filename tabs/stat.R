statTab <- tabPanel(
  'Stats',
  textOutput("filename"),
  fluidRow(
    downloadButton("thresholdOne", "Download summary of each condition"),
    downloadButton("thresholdTwo", "Download thresholds (brief)"),
    downloadButton("thresholdThree", "Download individual results"),
    downloadButton("participantInfoExcel", "Download participant distance info")),
    # downloadButton("ratingSummary", "Download Ratings Summary")),
  fluidRow(
    column(
      width = 12,
      align = "center",
      h3("Summary across participants")
    )
  ),
  fluidRow(
    column(
      width = 12, 
      align = "center",
      tableOutput('thresholdSummary'))),
  fluidRow(column(
    12, align = "center", h3("Summary for each participant")
  )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      shinycssloaders::withSpinner(tableOutput('thresholdAll'), type = 4)
    )
  ),
  fluidRow(
    column(
      width = 12, 
      align = "center", 
      h3("Beauty and Comfort ratings")
    )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      shinycssloaders::withSpinner(tableOutput('ratings'), type = 4)
    )
  ),
  fluidRow(
    column(
      width = 12, 
      align = "center", 
      h3("Participant Information")
    )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      shinycssloaders::withSpinner(tableOutput('participantInfo'), type = 4)
    )
  ),
  fluidRow(column(
    width = 12, 
    align = "center",
    h3("All questionAndAnswer")
  )
  ),
  fluidRow(
    column(
      width = 12,
      align = "center",
      shinycssloaders::withSpinner(DT::dataTableOutput('QA'), type = 4)
    )
  )
)