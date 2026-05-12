distanceTabUI <- function(id, maxDistanceDotSlots = 20, maxDistanceScatterSlots = 20) {
  ns <- NS(id)

  distance_plot_cell <- function(prefix, download_prefix, i) {
    availability_id <- paste0("has", toupper(substr(prefix, 1, 1)), substr(prefix, 2, nchar(prefix)), i)
    conditionalPanel(
      condition = sprintf("output['%s']", ns(availability_id)),
      tags$div(
        tags$div(
          style = "font-weight: bold; font-size: 12px; color: #333; padding: 8px 4px 4px 4px; word-wrap: break-word; white-space: normal;",
          textOutput(ns(paste0(prefix, "Title", i)), inline = TRUE)
        ),
        shinycssloaders::withSpinner(
          imageOutput(ns(paste0(prefix, i)), width = "100%", height = "100%"),
          type = 4
        ),
        downloadButton(ns(paste0(download_prefix, i)), "Download")
      )
    )
  }

  two_column_plot_rows <- function(prefix, download_prefix, max_slots) {
    rows <- list()
    for (i in seq(1, max_slots, by = 2)) {
      rows[[length(rows) + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        style = "overflow-x: hidden;",
        distance_plot_cell(prefix, download_prefix, i),
        if (i + 1 <= max_slots) distance_plot_cell(prefix, download_prefix, i + 1) else ""
      )
    }
    rows
  }

  tagList(
    conditionalPanel(
      sprintf("output['%s']", ns("IsMergedParticipantDistanceTable")),
      h2("Participant table"),
      fixedRow(
        column(
          width = 12,
          shinycssloaders::withSpinner(
            DT::dataTableOutput(ns("mergedParticipantDistanceTable")),
            type = 4
          ),
          downloadButton(ns("downloadParticipantDistanceInfo"), "Download")
        )
      )
    ),
    h2("Histograms colored by participant"),
    tags$div(two_column_plot_rows("dot", "downloadDot", maxDistanceDotSlots)),
    h2("Scatter diagrams"),
    tags$div(two_column_plot_rows("distanceScatter", "downloadDistanceScatter", maxDistanceScatterSlots))
  )
}
