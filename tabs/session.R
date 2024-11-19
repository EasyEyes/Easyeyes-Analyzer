sessionTab <- tabPanel(
  'Sessions',
  tags$head(# Note the wrapping of the string in HTML()
    tags$style(
      HTML(
        ".navbar {
          margin-bottom: 10px;
        }
        .dataTables_filter {
          float: left !important;
        }
        .form-group {
          margin-bottom: 10px;
        }
        .form-group .form-control {
          width: 750px;
          margin-right: 0px;
          padding-right: 0px;
        }
        .container-fluid {
          padding-left: 5px;
          padding-right: 0px;
        }
        .radio-inline {
          font-size: 16px;
        }
        .control-label {
          font-size: 16px;
        }
        #fileStatusMessage {
          margin-bottom: 10px;
          font-weight: bold;
        }
        .shiny-file-input-progress {
          display: none !important;
        }
        .custom-loading-message {
          position: fixed;
          top: 50%;
          left: 50%;
          transform: translate(-50%, -50%);
          background: white;
          border: 2px solid #ccc;
          padding: 20px;
          width: 23%;
          text-align: center;
          border-radius: 10px;
          font-size: 16px;
          font-weight: bold;
          color: black;
          z-index: 10000;
          display: none;
          box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        }
        .overlay-background {
          position: fixed;
          top: 0;
          left: 0;
          width: 100%;
          height: 100%;
          background: rgba(0, 0, 0, 0.5);
          z-index: 9999;
          display: none;
        }
            "
      )),
    
    tags$script(HTML(
      "
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'file') {
            $('.overlay-background').show(); // Show the overlay
            $('.custom-loading-message').text('Reading file(s)...').show(); // Show the popup
          }
        });

        $(document).on('shiny:idle', function() {
          $('.overlay-background').hide(); // Hide the overlay
          $('.custom-loading-message').hide(); // Hide the popup
        });
  
        "
    ))
  ),
  tags$head(
    tags$script(type = "text/javascript",
                src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  tags$div(class = "overlay-background"),
  tags$div(class = "custom-loading-message"),
  fluidRow(column(
    width = 4,
    fileInput(
      "file",
      NULL,
      accept = c(".csv", ".zip", '.xlsx'),
      buttonLabel = "Select CSV files or ZIP file",
      multiple = T
    ),
    textOutput("fileStatusMessage")
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