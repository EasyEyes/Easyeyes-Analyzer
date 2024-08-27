formSpreeTab <- tabPanel(
  'FormSpree',
  fixedRow(column(width = 1, 
                 align = 'center',
                 checkboxInput('listFontParameters', 'List only font parameters', value = FALSE, width = NULL))),
  fixedRow(
    shinycssloaders::withSpinner((DT::dataTableOutput('formSpreeDashboard')), type=4)
  )
)