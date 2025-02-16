
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
        div.datatable-wide {
        padding-left: 0;
        padding-right: 0;
        }
        .sweet-alert {
          position: fixed !important;  /* Ensures it's positioned relative to the viewport */
          top: 50% !important;  /* Moves it down by 50% of the viewport height */
          left: 50% !important;  /* Moves it right by 50% of the viewport width */
          transform: translate(-50%, -50%) !important;  /* Centers it properly */
          
          font-family: 'Arial', sans-serif !important; /* Match compiler pop-up font */
          font-size: 18px !important; /* Match compiler pop-up text size */
          font-weight: bold;
          color: black !important;
          box-shadow: none !important;
          border-radius: 8px !important;
          width: 500px !important;  /* Reduce width */
          height: 100px !important; /* Reduce height */
          padding: 0px !important;  /* Reduce inner spacing */
          margin: 0px !important;
        }

      .sweet-alert h2 {
          font-size: 20px !important; /* Adjust the title font size */
          color: #494949 !important;
      }
      "
      )),
  #   
  #   tags$script(HTML(
  #     "
  #       $(document).on('shiny:inputchanged', function(event) {
  #         if (event.name === 'file') {
  #           $('.overlay-background').show(); // Show the overlay
  #           $('.custom-loading-message').text('Reading file(s)...').show(); // Show the popup
  #         }
  #       });
  # 
  #       $(document).on('shiny:idle', function() {
  #         $('.overlay-background').hide(); // Hide the overlay
  #         $('.custom-loading-message').hide(); // Hide the popup
  #       });
  # 
  #       "
  #   ))
  tags$script(type = "text/javascript",
              src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
  # tags$div(class = "overlay-background"),
  # tags$div(class = "custom-loading-message"),
  fluidRow(column(
    width = 4,
    fileInput(
      "file",
      NULL,
      accept = c(".csv", ".zip", '.xlsx'),
      buttonLabel = "Select CSV files or ZIP file",
      multiple = T,
      width = '1000px'
    ) |>
      tagAppendAttributes(
        onInput = "
                  const id = $(this).find('input[type=\"file\"]').attr('id');
                  Shiny.setInputValue(id + '_click', Math.random());
        "
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
