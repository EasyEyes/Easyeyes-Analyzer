formSpreeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fixedRow(column(width = 1,
                   align = 'center',
                   checkboxInput(ns('listFontParameters'), 'List only font parameters', value = FALSE, width = NULL))),
    fixedRow(
      shinycssloaders::withSpinner((DT::dataTableOutput(ns('formSpreeDashboard'))), type=4)
    )
  )
}