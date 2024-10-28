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
             #fileStatusMessage {
              margin-bottom: 10px;
              font-weight: bold;
             }
             .shiny-file-input-progress  {
              display: none !important;
             }
            .custom-loading-message {
                 color: black;
                 font-size: 12px;
                 margin-top: 10px;
            }
            #fileStatusMessage {
                 margin-bottom: 10px;
                 font-weight: bold;
            }
            "
      )),
    
      tags$script(HTML(
        "
        $(document).on('shiny:inputchanged', function(event) {
          if (event.name === 'file') {
            let fileInputContainer = $('#file').closest('.form-group');
            fileInputContainer.find('.custom-loading-message').remove();
            fileInputContainer.append('<div class=\"custom-loading-message\">Reading...</div>');
          }
        });
        
        $(document).on('shiny:idle', function() {
          let fileInputContainer = $('#file').closest('.form-group');
        fileInputContainer.find('.custom-loading-message').remove();
        });
  
        "
      ))
    ),
  tags$head(
    tags$script(type = "text/javascript",
                src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML")
  ),
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
