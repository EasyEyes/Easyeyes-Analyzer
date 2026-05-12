qualityTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("Histograms"),
    shinycssloaders::withSpinner(uiOutput(ns("qualityHistograms")), type = 4),
    h2("Scatter diagrams"),
    shinycssloaders::withSpinner(uiOutput(ns("qualityScatters")), type = 4)
  )
}
