
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
      )),
      column(
        width = 3,
        offset = 5,
        div(
          class = "latest-session-toggle",
          style = "margin-top: 6px;",
          checkboxInput(
            "latestSessionOnly",
            "Latest session only",
            value = FALSE
          )
        )
      )
    ),
    textOutput("instruction"),
    shinycssloaders::withSpinner(DT::dataTableOutput('ex1'), type = 4)
)
