plotsTab <- tabPanel(
  'Plots',
  conditionalPanel(
    'output.IsCameraResolutionXYTable',
    fixedRow(
      column(
        width = 6,
        shinycssloaders::withSpinner(
          tableOutput("cameraResolutionXYTable"),
          type = 4
        )
      )
    )
  ),
  conditionalPanel(
    'output.isCorrMatrixAvailable',
    h2("Correlation Matrix"),
    fixedRow(
      column(
        width = 6,
        shinycssloaders::withSpinner(
          plotOutput("corrMatrixPlot", width = "100%", height = "100%"),
          type = 4
        ),
        downloadButton("downloadCorrMatrixPlot", "Download")
      ),
      column(
        width = 6,
        shinycssloaders::withSpinner(
          plotOutput("nMatrixPlot", width = "100%", height = "100%"),
          type = 4
        ),
        downloadButton("downloadNMatrixPlot", "Download")
      )
    )
  ),
  #### histogram ####
  h2("Histograms"),
  shinycssloaders::withSpinner(uiOutput('histograms'), type = 4),
  
  #### dot plots ####
  h2("Histograms colored by participant"),
  shinycssloaders::withSpinner(uiOutput('dotPlots'), type = 4),
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
  h2("Violin plots"),
  shinycssloaders::withSpinner(uiOutput('violinPlots'), type = 4),
  h2("Font comparison plots"),
  shinycssloaders::withSpinner(uiOutput('fontComparisonPlots'), type = 4),
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
      conditionalPanel('output.isPeripheralCrowding',
                       shinycssloaders::withSpinner(
                         ggiraph::girafeOutput("rsvpCrowdingPeripheralFontPlot", height = '100%'),
                         type = 4
                       ))
      ),
    splitLayout(
      cellWidths = c("50%", "50%"),
      conditionalPanel('output.isPeripheralCrowding',downloadButton("downloadRsvpCrowdingPeripheralGradePlot", "Download")),
      conditionalPanel('output.isPeripheralCrowding',downloadButton("downloadRsvpCrowdingPeripheralFontPlot", "Download")),
    ),
    conditionalPanel('output.isCrowding',
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                     shinycssloaders::withSpinner(
                       ggiraph::girafeOutput("rsvpResidualCrowding", height = '100%'),
                       type = 4
                     )),
                     splitLayout(
                       cellWidths = c("50%", "50%"), 
                       downloadButton("downloadRsvpResidualCrowding", "Download"))
    ),
    conditionalPanel('output.isFovealCrowding',
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(
                         ggiraph::girafeOutput("rsvpCrowdingFovealGradePlot", height = '100%'),
                         type = 4
                       )),
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton("downloadRsvpCrowdingFovealGradePlot", "Download"))
    ),
    conditionalPanel('output.isFovealAcuity',
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(
                         ggiraph::girafeOutput("rsvpFovealAcuityGradePlot", height = '100%'),
                         type = 4
                       )),
                     splitLayout(
                       cellWidths = c("50%", "50%"),
                       downloadButton("downloadRsvpFovealAcuityGradePlot", "Download")
                     )
    ),
    conditionalPanel(
      'output.isPeripheralAcuity',
      splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("rsvpPeripheralAcuityFontPlot", height = '100%'),
          type = 4
        ),
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("rsvpPeripheralAcuityGradePlot", height = '100%'),
          type = 4
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton("downloadRsvpPeripheralAcuityFontPlot", "Download"),
        downloadButton("downloadRsvpPeripheralAcuityGradePlot", "Download")
      )
    ),
    conditionalPanel(
      'output.isRepeated',
      splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(
          ggiraph::girafeOutput("rsvpRepeatedGradePlot", height = '100%'),
          type = 4
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton("downloadrsvpRepeatedGradePlot", "Download")
      )
    )
  ),
    
    #### crowding ####
    h2("Ordinary reading plots"),
    conditionalPanel(
      'output.isReading',
      conditionalPanel(
        'output.isFovealCrowding',
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealCrowdingFontPlot", height = '600px'),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealCrowdingGradePlot", height = '600px'),
            type = 4
          )),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryFovealCrowdingFontPlot", "Download"),
          downloadButton("downloadOrdinaryFovealCrowdingGradePlot", "Download"))
      ),
      conditionalPanel(
        'output.isPeripheralCrowding',
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralCrowdingFontPlot", height = '600px'),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralCrowdingGradePlot", height = '600px'),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
            downloadButton("downloadOrdinaryPeripheralCrowdingFontPlot", "Download"),
            downloadButton("downloadOrdinaryPeripheralCrowdingGradePlot", "Download"))
      ),
      conditionalPanel(
        'output.isFovealAcuity',
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealAcuityFontPlot", height = '600px'),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealAcuityGradePlot", height = '600px'),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryFovealAcuityFontPlot", "Download"),
          downloadButton("downloadOrdinaryFovealAcuityGradePlot", "Download"))
      ),
      conditionalPanel(
        'output.isPeripheralAcuity',
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralAcuityFontPlot", height = '600px'),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralAcuityGradePlot", height = '600px'),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryPeripheralAcuityFontPlot", "Download"),
          downloadButton("downloadOrdinaryPeripheralAcuityGradePlot", "Download"))
      ),
      # Peripheral Acuity Plots
        conditionalPanel(
          'output.isRepeated',
          splitLayout(
            cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("readingRepeatedGradePlot", height = '600px'),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadReadingRepeatedGradePlot", "Download")
        )
      )
    ),
    h2("Age plots"),
    uiOutput('plots')
  )