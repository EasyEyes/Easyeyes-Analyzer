sessionTab <- tabPanel(
  'Sessions',
  tags$head(# Note the wrapping of the string in HTML()
    tags$style(
      HTML(
        ".navbar{
             margin-bottom: 10px
             }
             .dataTables_filter{
             float: left !important;
             }
             .form-group {
             margin-bottom: 10px;
             }
             .form-group .form-control {
             width: 750px;
             margin-right: 0px;
             padding-right:0px;
             }
             .container-fluid {
             padding-left:5px;
             padding-right:0px;
             }
             .radio-inline {
             font-size:16px;
             }
             .control-label {
             font-size:16px;
             }
            "
      )
    )),
  tags$head(
    tags$script(type = "text/javascript",
                src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  fluidRow(column(
    width = 4,
    fileInput(
      "file",
      NULL,
      accept = c(".csv", ".zip"),
      buttonLabel = "Select CSV files or ZIP file",
      multiple = T
    )
  )),
  fixedRow(column(
    width = 4,
    fileInput(
      "fileProlific",
      NULL,
      accept = c(".csv"),
      buttonLabel = "Prolific csv file",
      multiple = T
    )
  )),
  fixedRow(
    column(
      width = 2,
      align = "left",
      downloadButton("report", "Download report")
    ),
    column(
      width = 1,
      align = "left",
      downloadButton("sessionCsv", "Download csv")
    ),
    column(width = 1, div(
      HTML(
        '<input type="text" id="search" style="height: 33px; margin-left: 100px;"/>'
      )
    ))
  ),
  textOutput("instruction"),
  shinycssloaders::withSpinner(DT::dataTableOutput('ex1'), type = 4)
)
