formSpreeTab <- tabPanel(
  'FormSpree',
  fixedRow(
    shinycssloaders::withSpinner((DT::dataTableOutput('formSpreeDashboard')), type=4)
  )
)