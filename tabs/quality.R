qualityTab <- tabPanel(
  'Quality',
  h2("Histograms"),
  shinycssloaders::withSpinner(uiOutput('qualityHistograms'), type = 4),
  h2("Scatter diagrams"),
  shinycssloaders::withSpinner(uiOutput('qualityScatters'), type = 4)
)

