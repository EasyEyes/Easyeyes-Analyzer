staircasesTabUI <- function(id) {
  ns <- NS(id)
  quest_data_condition <- sprintf("output['%s']", ns("questData"))
  tagList(
    conditionalPanel(
      condition = quest_data_condition,
      h3("Staircases"),
      fixedRow(
        style = "margin-left:2px;",
        shinycssloaders::withSpinner(plotOutput(
          ns("stairPlot"), width = "100%", height = "100%"
        ), type = 4),
        fixedRow(style = "margin-left:2px;",
                 splitLayout(
                   cellWidths = c("50%", "50%"),
                   downloadButton(ns("downloadStairPlot"), "Download")
                 ))
      )
    )
  )
}
