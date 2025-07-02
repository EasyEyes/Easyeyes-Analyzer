plotsTab <- tabPanel(
  'Plots',
  fixedRow(shinycssloaders::withSpinner(
    plotOutput("corrMatrixPlot", width = "100%", height = "100%"),
    type = 4
  )),
  downloadButton("downloadCorrMatrixPlot", "Download"),
  #### histogram ####
  h2("Histograms"),
  shinycssloaders::withSpinner(uiOutput('histograms'), type = 4),
  conditionalPanel(
    'output.isGrade',
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRsvp',
        shinycssloaders::withSpinner(plotOutput("stackedRsvpPlot", height = "100%"), type = 4)
      ),
      conditionalPanel(
        'output.isCrowding',
        shinycssloaders::withSpinner(plotOutput("stackedCrowdingPlot", height = "100%"), type = 4)
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRsvp',
        downloadButton("downloadStackedRsvpPlot", "Download")
      ),
      conditionalPanel(
        'output.isCrowding',
        downloadButton("downloadStackedCrowdingPlot", "Download")
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isFovealAcuity',
        shinycssloaders::withSpinner(imageOutput("stackedFovealAcuityPlot", height = "100%"), type = 4)
      ),
      conditionalPanel(
        'output.isFovealCrowding',
        shinycssloaders::withSpinner(
          imageOutput("stackedFovealCrowdingPlot", height = "100%"),
          type = 4
        )
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isFovealAcuity',
        downloadButton("downloadStackedFovealAcuityPlot", "Download")
      ),
      conditionalPanel(
        'output.isFovealCrowding',
        downloadButton("downloadStackedFovealCrowdingPlot", "Download")
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        shinycssloaders::withSpinner(
          imageOutput("stackedFovealRepeatedPlot", height = "100%"),
          type = 4
        )
      ),
      conditionalPanel(
        'output.isPeripheralAcuity',
        shinycssloaders::withSpinner(
          imageOutput("stackedPeripheralAcuityPlot", height = "100%"),
          type = 4
        )
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        downloadButton("downloadStackedFovealRepeatedPlot", "Download")
      ),
      conditionalPanel(
        'output.isPeripheralAcuity',
        downloadButton("downloadStackedPeripheralAcuityPlot", "Download")
      )
    )
  ),
  h2("Scatter diagrams"),
  
  shinycssloaders::withSpinner(uiOutput('scatters'), type = 4),
  conditionalPanel(
    'output.isRsvp',
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel('output.isPeripheralCrowding',
        shinycssloaders::withSpinner(
        ggiraph::girafeOutput("rsvpCrowdingPeripheralGradePlot", height = '100%'),
        type = 4
      )),
      conditionalPanel('output.isCrowding',
      shinycssloaders::withSpinner(
        ggiraph::girafeOutput("rsvpResidualCrowding", height = '100%'),
        type = 4
      )
    )),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel('output.isPeripheralCrowding',downloadButton("downloadRsvpCrowdingPeripheralGradePlot", "Download")),
      conditionalPanel('output.isCrowding',downloadButton("downloadRsvpResidualCrowding", "Download"))
    ),

    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel('output.isFovealCrowding',
                       shinycssloaders::withSpinner(
        ggiraph::girafeOutput("rsvpCrowdingFovealGradePlot", height = '100%'),
        type = 4
      )),
      conditionalPanel('output.isFovealAcuity',
                       shinycssloaders::withSpinner(
        ggiraph::girafeOutput("rsvpFovealAcuityGradePlot", height = '100%'),
        type = 4
      ))
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel('output.isFovealCrowding',
                       downloadButton("downloadRsvpCrowdingFovealGradePlot", "Download")),
      conditionalPanel('output.isFovealAcuity',
                       downloadButton("downloadRsvpFovealAcuityGradePlot", "Download"))
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("rsvpRepeatedGradePlot", height = '100%'),
          type = 4
        )
      ),
      conditionalPanel(
        'output.isPeripheralAcuity',
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("rsvpPeripheralAcuityGradePlot", height = '100%'),
          type = 4
        )
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        downloadButton("downloadrsvpRepeatedGradePlot", "Download")
      ),
      conditionalPanel(
        'output.isPeripheralAcuity',
        downloadButton("downloadRsvpPeripheralAcuityGradePlot", "Download")
      )
    )
  ),
  
  #### crowding ####
  h2("Ordinary reading plots"),
  conditionalPanel(
    'output.isReading',
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isPeripheralCrowding',
      shinycssloaders::withSpinner(
        ggiraph::girafeOutput("ordinaryPeripheralCrowdingGradePlot", height = '600px'),
        type = 4
      )),
      conditionalPanel(
        'output.isFovealCrowding',
      shinycssloaders::withSpinner(
        ggiraph::girafeOutput("ordinaryFovealCrowdingGradePlot", height = '600px'),
        type = 4
      ))
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isPeripheralCrowding',
      downloadButton("downloadOrdinaryPeripheralCrowdingGradePlot", "Download")),
      conditionalPanel(
        'output.isFovealCrowding',
      downloadButton("downloadOrdinaryFovealCrowdingGradePlot", "Download"))
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isFovealAcuity',
      shinycssloaders::withSpinner(
        ggiraph::girafeOutput("ordinaryFovealAcuityGradePlot", height = '600px'),
        type = 4
      )),
      conditionalPanel(
        'output.isPeripheralAcuity',
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("ordinaryPeripheralAcuityGradePlot", height = '600px'),
          type = 4
        )
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isFovealAcuity',
      downloadButton("downloadOrdinaryFovealAcuityGradePlot", "Download")),
      conditionalPanel(
        'output.isPeripheralAcuity',
        downloadButton("downloadOrdinaryPeripheralAcuityGradePlot", "Download")
      )
    ),
    
    # Peripheral Acuity Plots
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("readingRepeatedGradePlot", height = '600px'),
          type = 4
        )
      )
    ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel(
        'output.isRepeated',
        downloadButton("downloadReadingRepeatedGradePlot", "Download")
      )
    )
  ),
  h2("Age plots"),
  uiOutput('plots')
)