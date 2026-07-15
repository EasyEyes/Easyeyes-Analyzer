plotsTabUI <- function(maxPlotsHistSlots = 36,
                       maxPlotsAgeSlots = 12,
                       maxPlotsScatterSlots = 30,
                       maxPlotsViolinSlots = 10,
                       maxPlotsFontComparisonSlots = 10) {

  plots_plot_cell <- function(prefix, download_prefix, i, show_title = TRUE) {
    availability_id <- paste0(
      "has",
      toupper(substr(prefix, 1, 1)),
      substr(prefix, 2, nchar(prefix)),
      i
    )
    conditionalPanel(
      condition = sprintf("output['%s']", availability_id),
      tags$div(
        if (isTRUE(show_title)) {
          tags$div(
            style = "font-weight: bold; font-size: 12px; color: #333; padding: 8px 4px 4px 4px; word-wrap: break-word; white-space: normal;",
            textOutput(paste0(prefix, "Title", i), inline = TRUE)
          )
        },
        shinycssloaders::withSpinner(
          imageOutput(paste0(prefix, i), width = "100%", height = "100%"),
          type = 4
        ),
        downloadButton(paste0(download_prefix, i), "Download")
      )
    )
  }

  two_column_plot_rows <- function(prefix, download_prefix, max_slots, show_title = TRUE) {
    rows <- list()
    for (i in seq(1, max_slots, by = 2)) {
      rows[[length(rows) + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        style = "overflow-x: hidden;",
        plots_plot_cell(prefix, download_prefix, i, show_title = show_title),
        if (i + 1 <= max_slots) {
          plots_plot_cell(prefix, download_prefix, i + 1, show_title = show_title)
        } else {
          ""
        }
      )
    }
    rows
  }

  six_column_plot_rows <- function(prefix, download_prefix, max_slots, show_title = FALSE) {
    rows <- list()
    n_per_row <- 6
    for (i in seq(1, max_slots, by = n_per_row)) {
      idx <- i:min(i + n_per_row - 1, max_slots)
      plot_cells <- lapply(idx, function(j) {
        plots_plot_cell(prefix, download_prefix, j, show_title = show_title)
      })
      if (length(plot_cells) < n_per_row) {
        plot_cells <- c(plot_cells, rep(list(""), n_per_row - length(plot_cells)))
      }
      rows[[length(rows) + 1]] <- do.call(splitLayout, c(
        list(
          cellWidths = rep("16.66%", n_per_row),
          style = "overflow-x: hidden; white-space: nowrap;"
        ),
        plot_cells
      ))
    }
    rows
  }

  tagList(
    conditionalPanel(
      "output.isCorrMatrixAvailable",
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
    tags$div(six_column_plot_rows("hist", "downloadHist", maxPlotsHistSlots, show_title = FALSE)),

    conditionalPanel(
      "output.isGrade",
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isRsvp",
          shinycssloaders::withSpinner(plotOutput("stackedRsvpPlot", height = "100%"), type = 4)
        ),
        conditionalPanel(
          "output.isCrowding",
          shinycssloaders::withSpinner(plotOutput("stackedCrowdingPlot", height = "100%"), type = 4)
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isRsvp",
          downloadButton("downloadStackedRsvpPlot", "Download")
        ),
        conditionalPanel(
          "output.isCrowding",
          downloadButton("downloadStackedCrowdingPlot", "Download")
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isFovealAcuity",
          shinycssloaders::withSpinner(imageOutput("stackedFovealAcuityPlot", height = "100%"), type = 4)
        ),
        conditionalPanel(
          "output.isFovealCrowding",
          shinycssloaders::withSpinner(
            imageOutput("stackedFovealCrowdingPlot", height = "100%"),
            type = 4
          )
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isFovealAcuity",
          downloadButton("downloadStackedFovealAcuityPlot", "Download")
        ),
        conditionalPanel(
          "output.isFovealCrowding",
          downloadButton("downloadStackedFovealCrowdingPlot", "Download")
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isRepeated",
          shinycssloaders::withSpinner(
            imageOutput("stackedFovealRepeatedPlot", height = "100%"),
            type = 4
          )
        ),
        conditionalPanel(
          "output.isPeripheralAcuity",
          shinycssloaders::withSpinner(
            imageOutput("stackedPeripheralAcuityPlot", height = "100%"),
            type = 4
          )
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isRepeated",
          downloadButton("downloadStackedFovealRepeatedPlot", "Download")
        ),
        conditionalPanel(
          "output.isPeripheralAcuity",
          downloadButton("downloadStackedPeripheralAcuityPlot", "Download")
        )
      )
    ),
    h2("Violin plots"),
    tags$div(two_column_plot_rows("violin", "downloadViolin", maxPlotsViolinSlots)),
    h2("Font comparison plots"),
    tags$div(two_column_plot_rows("fontComparison", "downloadFontComparison", maxPlotsFontComparisonSlots)),
    h2("Scatter diagrams"),
    tags$div(two_column_plot_rows("scatter", "downloadScatter", maxPlotsScatterSlots)),
    conditionalPanel(
      "output.isRsvp",
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isPeripheralCrowding",
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpCrowdingPeripheralGradePlot", height = "100%"),
            type = 4
          )
        ),
        conditionalPanel(
          "output.isPeripheralCrowding",
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpCrowdingPeripheralFontPlot", height = "100%"),
            type = 4
          )
        )
      ),
      splitLayout(
        cellWidths = c("50%", "50%"),
        conditionalPanel(
          "output.isPeripheralCrowding",
          downloadButton("downloadRsvpCrowdingPeripheralGradePlot", "Download")
        ),
        conditionalPanel(
          "output.isPeripheralCrowding",
          downloadButton("downloadRsvpCrowdingPeripheralFontPlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isCrowding",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpResidualCrowding", height = "100%"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadRsvpResidualCrowding", "Download")
        )
      ),
      conditionalPanel(
        "output.isFovealCrowding",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpCrowdingFovealGradePlot", height = "100%"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadRsvpCrowdingFovealGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isFovealAcuity",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpFovealAcuityGradePlot", height = "100%"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadRsvpFovealAcuityGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isPeripheralAcuity",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpPeripheralAcuityFontPlot", height = "100%"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpPeripheralAcuityGradePlot", height = "100%"),
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
        "output.isRepeated",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("rsvpRepeatedGradePlot", height = "100%"),
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
      "output.isReading",
      conditionalPanel(
        "output.isFovealCrowding",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealCrowdingFontPlot", height = "600px"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealCrowdingGradePlot", height = "600px"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryFovealCrowdingFontPlot", "Download"),
          downloadButton("downloadOrdinaryFovealCrowdingGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isPeripheralCrowding",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralCrowdingFontPlot", height = "600px"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralCrowdingGradePlot", height = "600px"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryPeripheralCrowdingFontPlot", "Download"),
          downloadButton("downloadOrdinaryPeripheralCrowdingGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isFovealAcuity",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealAcuityFontPlot", height = "600px"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryFovealAcuityGradePlot", height = "600px"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryFovealAcuityFontPlot", "Download"),
          downloadButton("downloadOrdinaryFovealAcuityGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isPeripheralAcuity",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralAcuityFontPlot", height = "600px"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("ordinaryPeripheralAcuityGradePlot", height = "600px"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadOrdinaryPeripheralAcuityFontPlot", "Download"),
          downloadButton("downloadOrdinaryPeripheralAcuityGradePlot", "Download")
        )
      ),
      conditionalPanel(
        "output.isRepeated",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            ggiraph::girafeOutput("readingRepeatedGradePlot", height = "600px"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadReadingRepeatedGradePlot", "Download")
        )
      )
    ),
    h2("Font plots"),
    conditionalPanel(
      "output.isReading || output.isRsvp",
      conditionalPanel(
        "output.isPeripheralCrowding",
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            imageOutput("fontAggregatedReadingRsvpCrowdingPlot", height = "100%"),
            type = 4
          ),
          shinycssloaders::withSpinner(
            imageOutput("fontAggregatedOrdinaryReadingCrowdingPlot", height = "100%"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadFontAggregatedReadingRsvpCrowdingPlot", "Download"),
          downloadButton("downloadFontAggregatedOrdinaryReadingCrowdingPlot", "Download")
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          shinycssloaders::withSpinner(
            imageOutput("fontAggregatedRsvpCrowdingPlot", height = "100%"),
            type = 4
          )
        ),
        splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton("downloadFontAggregatedRsvpCrowdingPlot", "Download")
        )
      )
    ),
    h2("Age plots"),
    tags$div(two_column_plot_rows("age", "downloadAge", maxPlotsAgeSlots))
  )
}
