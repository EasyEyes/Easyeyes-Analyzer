##### Profiles #####
profileTabUI <- function(id) {
  ns <- NS(id)
  total_data_condition <- sprintf("input['%s']", ns("totalData"))
  tagList(
  tags$script(HTML(sprintf("window.easyeyesProfileNS = '%s';", ns("")))),
  tags$head(
    tags$script(src = "https://www.gstatic.com/firebasejs/10.6.0/firebase-app-compat.js"),
    tags$script(src = "https://www.gstatic.com/firebasejs/10.6.0/firebase-firestore-compat.js"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/chart.js"),
    tags$script(src = "firebase-config.js"),
    tags$script(src = "firestore.js"),
  ),
  fixedRow(
    column(
      width = 12,
      align = "left",
      radioButtons(
        ns("fileProfile"),
        "Select download file type:",
        c(
          "eps" = "eps",
          "pdf" = "pdf",
          "png" = "png",
          "svg" = "svg"
        ),
        inline = TRUE,
        selected = "png"
      )
    ),
    div(
      column(width = 12,
             align = "left",
             div(
               HTML(
                 "<div class='row'>
                 <label style='font-size: 16px; margin-left: 15px; margin-right: 10px'>  Filter by device: </label>
                 <input style='font-size: 16px; margin-right: 10px;' type='radio' name='filterType' id='deviceBool' checked></input>
                 <label style='font-size: 16px; margin-left: 15px; margin-right: 10px'>  Filter by screen: </label>
                 <input style='font-size: 16px; margin-right: 10px;' type='radio' name='filterType' id='screenBool'></input>
                 <br>
                 <label style='font-size: 16px; margin-left: 15px; margin-right: 10px'>  Transducer: </label><select style='margin-right: 20px;' id='transducer'></select>
                 <label style='font-size: 16px; margin-right: 10px'>OEM: </label><select style='margin-right: 20px;' id='OEM'></select>
                 <label style='font-size: 16px; margin-right: 10px'>Model Name: </label><select style='margin-right: 20px;' id='Model'></select>
                 <label style='font-size: 16px; margin-right: 10px'>Model Number: </label><select style='margin-right: 20px'; id='IDs'></select>
                 <label style='font-size: 16px; margin-right: 10px'>51 degrees ID: </label><select style='margin-right: 20px;' id='deviceID'></select>
                 <br>
                 <label style='font-size: 16px; margin-left: 15px; margin-right: 10px'>Aspect Ratio: </label><select style='margin-right: 20px;' id='aspectRatio'></select>
                 <label style='font-size: 16px; margin-right: 10px'>Screen Pixel Size: </label><select style='margin-right: 20px;' id='screenPx'></select>
                 <br>
                 <input style='font-size: 16px; margin-left: 15px; margin-right: 10px;' type='checkbox' id='filterBool'> </input>
                  <label style='font-size: 16px; margin-right: 10px'>Correction max SD (dB): </label><input type='number' id='SDTolerance'> </input>
                 </div>
              "
               )
             )),
      actionButton(ns("refreshButton"), "Get profile list", 
                   style="color: white; background-color: blue; width: 150px; margin-left: 15px;"),
      actionButton(ns("plotButton"), "Plot", 
                   style="color: white; background-color: #008000; width: 100px; margin-left: 15px;"),
    ),
    fixedRow(style = "margin-left:2px;",
             column(
               width = 12,
               align = "left",
               checkboxGroupInput(
                 inputId = ns("profileSelection"),
                 label = "",
                 inline = TRUE,
                 choices = NULL,
                 selected = NULL
               )
             )),
    conditionalPanel(
      total_data_condition,
      fixedRow(style = "margin-left:2px;",
               column(
                 width = 12,
                 align = "left",
                 actionButton(ns("doProfile"), "Plot selected profiles")
               ))
    ),
    fixedRow(style = "margin-left:2px;",
             column(
               width = 6,
               align = "left",
               plotOutput(ns("profilePlot"), height = "100%", width = "100%")
             ),
             column(
               width = 6,
               align = "left",
               plotOutput(ns("shiftedProfilePlot"), height = "100%", width = "100%")
             )
    ),
    conditionalPanel(
      total_data_condition,
      fixedRow(style = "margin-left:2px;", 
               column(
                 width = 6,
                 align = "left",
                 downloadButton(ns("downloadProfilePlot"), "Download")
               ),
               column(
                 width = 6,
                 align = "left",
                 downloadButton(ns("downloadShiftedProfilePlot"), "Download")
               ))
    ),
    conditionalPanel(
      total_data_condition,
      fixedRow(style = "margin-left:2px;",
               column(
                 width = 6,
                 align = "left",
                 plotOutput(ns("profileAvgPlot"), height = "100%", width = "100%")
               )),
      fixedRow(style = "margin-left:2px;", 
               column(
                 width = 6,
                 align = "left",
                 downloadButton(ns("downloadProfileAvgPlot"), "Download")
               )),
      fixedRow(style = "margin-left:40px;",
               column(width = 6, align = 'left',
                      textOutput(ns("profileAverageTitle")))),
      fixedRow(style = "margin-left:40px;",
               column(
                 width = 6,
                 align = "left",
                 tableOutput(ns("profileAverage"))
               ))),
    
    fixedRow(style = "margin-left:2px;",
             column(tableOutput(ns("summaryStats")), 
                    width = 12, align = "left")),
    DT::dataTableOutput(ns("profiles")),
    conditionalPanel(
      total_data_condition,
      fixedRow(style = "margin-left:2px;",
               column(width = 12, 
                      align = "left", 
                      downloadButton(ns("downloadProfileTable"), "download profile table"))
               )
    ),
    HTML('<table id="dataTable" class="display"></table>')
  )
)
}
