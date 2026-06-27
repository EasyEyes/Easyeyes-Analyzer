
sessionTab <- tabPanel(
  'Sessions',
    fixedRow(
      column(
        width = 2,
        align = "left",
        downloadButton("report", "Download HTML")
      ),
      column(
        width = 1,
        align = "left",
        downloadButton("sessionCsv", "Download xlsx")
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
