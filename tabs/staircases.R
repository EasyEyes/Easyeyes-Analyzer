staircasesTab <- tabPanel(
  'Staircases',
  conditionalPanel(
    condition = 'output.questData',
    h3("Staircases"),
    fixedRow(
      style = "margin-left:2px;",
      selectInput(
        'thresholdParameter',
        'thresholdParameter:',
        choices =  NULL,
        selected = NULL
      )
    ),
    fixedRow(
      style = "margin-left:2px;",
      shinycssloaders::withSpinner(plotOutput(
        'stairPlot', width = '100%', height = '100%'
      ), type = 4),
      fixedRow(style = "margin-left:2px;",
               splitLayout(
                 cellWidths = c("50%", "50%"),
                 downloadButton("downloadStairPlot", "Download")
               ))
    )
    
  )
)