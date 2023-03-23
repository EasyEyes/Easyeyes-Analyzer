#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
require(foreach)
require(dplyr)
require(readr)
require(ggplot2)
require(stringr)
require(emojifont)
require(DT)
source("preprocess.R")
source("summary_table.R")
source("threshold_and_warning.R")
source("random_rgb.R")
source("mean_median_plot.R")
library(shiny)
options(shiny.maxRequestSize=70*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage( 
                 navbarPage(
                   tags$head(
                     # Note the wrapping of the string in HTML()
                     tags$style(HTML("
                    .dataTables_filter{
        float: left !important;
    }
                     .form-group {
                     margin-bottom: 10px;
                     }
                                     "))),
                   title = 'EasyEyes Analysis',
                   
                   tabPanel('Error Report', 
                            fileInput("file", NULL, accept = ".csv",buttonLabel = "Select CSV files", multiple = T),
                            div(style = "margin-top: -10px"),
                            fluidRow(column(width = 2,downloadButton("report", "Download report")), column(width = 3,textInput('file_name', NULL, "error report"))),
                            DT::dataTableOutput('ex1')),
                   tabPanel('Threshold',  
                            fluidRow(tableOutput('ex3')),
                            fluidRow(tableOutput('ex2'))),
                   tabPanel('Plots', 
                            plotOutput("meanPlot", width = "100%"),
                            downloadButton("downloadMeanPlot", "Download plot one"), 
                            plotOutput("medianPlot", width = "100%"),
                            downloadButton("downloadMedianPlot", "Download plot two"))
))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$file_name <- renderText({input$file_name})
  data_list <- reactive({
    require(input$file)
    return(read_files(input$file$data)[[1]])
  })
  summary_list <- reactive({
    require(input$file)
    return(read_files(input$file$data)[[2]])
  })
  summary_table <- reactive({
    generate_summary_table(data_list())
  })
  threshold_and_warnings <- reactive({
    require(input$file)
    return(generate_threshold(data_list(), summary_list()))
  })
  meanPlot <- reactive({
    require(input$file)
    mean_plot(data_list(), summary_list())
  })
  
  medianPlot <- reactive({
    require(input$file)
    median_plot(data_list(), summary_list())
  })
  
  observeEvent(input$file, 
               {
                 participants <- reactive({
                   unique(summary_table()$`Pavlovia ID`)
                   })
                 output$ex1 <- DT::renderDataTable(
                   datatable(
                     summary_table(),
                     selection = 'none',
                     filter = "top",
                     escape = FALSE,
                     width = "180%",
                     options = list(
                       autoWidth = FALSE,
                       paging = FALSE,
                       scrollX=TRUE,
                       columnDefs = list(
                         list(visible = FALSE, targets = c(0)),
                         list(targets = c(9),
                              width = '150px',
                              className = 'details-control1',
                              render = JS(
                                "function(data, type, row, meta) {",
                                "return type === 'display' && data.length > 20 ?",
                                "data.substr(0, 20) + '...' : data;",
                                "}")),
                         list(targets = c(10),
                              width = '150px',
                              className = 'details-control2',
                              render = JS(
                                "function(data, type, row, meta) {",
                                "return type === 'display' && data.length > 20 ?",
                                "data.substr(0, 20) + '...' : data;",
                                "}")),
                         list(targets = c(1), render = JS(
                           "function(data, type, row, meta) {",
                           "return type === 'display' && data.length > 6 ?",
                           "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                           "}"), className = 'information-control1'),
                         list(targets = c(2), render = JS(
                           "function(data, type, row, meta) {",
                           "return type === 'display' && data.length > 6 ?",
                           "'<span title=\"' + data + '\">' + data.substr(0, 6) + '...</span>' : data;",
                           "}"), className = 'information-control2'),
                         list(width = '250px', targets = c(5, 6), className = 'dt-center'),
                         list(width = '50px', targets = c(3,4,7,8,11), className = 'dt-center'),
                         list(width = '200px', targets = c(12))
                       )
                     ),
                     callback = JS("
                   table.column(9).nodes().to$().css({cursor: 'pointer'});
                  var format1 = function(d) {
                  return'<p>' + d[9] + '</p>'
                  };
                  table.on('click', 'td.details-control1', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format1(row.data())).show();
                  }
                  })
                  table.column(10).nodes().to$().css({cursor: 'pointer'});
                  var format2 = function(d) {
                  return'<p>' + d[10] + '</p>'
                  };
                  table.on('click', 'td.details-control2', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format2(row.data())).show();
                  }
                  })
                  table.column(1).nodes().to$().css({cursor: 'pointer'});
                  var format3 = function(d) {
                  return'<p>' + d[1] + '</p> <p>' + d[2] + '</p>'
                  };
                  table.column(2).nodes().to$().css({cursor: 'pointer'});
                  var format4 = function(d) {
                  return'<p>' + d[1] + '</p> <p>' + d[2] + '</p>'
                  };
                  table.on('click', 'td.information-control1', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format3(row.data())).show();
                  }
                  });
                  table.on('click', 'td.information-control2', function() {
                  var td = $(this), row = table.row(td.closest('tr'));
                  if (row.child.isShown()) {
                  row.child.hide();
                  } else {
                  row.child(format4(row.data())).show();
                  }
                  });"
                                   
                     )) %>%
                     formatStyle(names(summary_table()), color = 'black', lineHeight="10px") %>% 
                     formatStyle(names(summary_table())[-1], 
                                 'Pavlovia ID',
                                 backgroundColor = styleEqual(participants(), random_rgb(length(participants()))))
                 )
                 
                 output$ex2 <- renderTable(
                   threshold_and_warnings()[[2]] %>% select(-thresholdParameter)
                 )
                 output$ex3 <- renderTable(
                   threshold_and_warnings()[[1]]
                 )
                 output$meanPlot <- renderPlot({
                   meanPlot() +
                     theme(legend.position = "right", 
                           legend.box = "vertical", 
                           legend.justification = c(1,1),
                           legend.margin = margin(-0.4),
                           legend.key.size = unit(4.5, "mm"),
                           legend.title = element_text(size=25),
                           legend.text = element_text(size=25),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.title = element_text(size = 25),
                           axis.text = element_text(size = 25),
                           axis.line = element_line(colour = "black"),
                           plot.title = element_text(size=25)) +
                     coord_fixed(ratio = 1)
                   }, res = 96)
                 output$medianPlot <- renderPlot({medianPlot() + 
                     theme(legend.position = "right", 
                           legend.box = "vertical", 
                           legend.justification = c(1,1),
                           legend.margin = margin(-0.4),
                           legend.key.size = unit(4.5, "mm"),
                           legend.title = element_text(size=25),
                           legend.text = element_text(size=25),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), axis.title = element_text(size = 25),
                           axis.text = element_text(size = 25),
                           axis.line = element_line(colour = "black"),
                           plot.title = element_text(size=25)) + 
                     coord_fixed(ratio = 1)
                   }, res = 96)
               })
  
  
  
  output$report <- downloadHandler(
    filename = function(){ ifelse(input$file_name == "", "error report.html", paste0(input$file_name, '.html'))},
    content = function(file) {
      tempReport <- file.path(tempdir(), "error report.Rmd")
      file.copy("error report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = summary_table() %>% mutate(experiment = ifelse(input$file_name == "", "Error Report", input$file_name)),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadMeanPlot <- downloadHandler(
    filename = 'mean.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = meanPlot() + coord_fixed(ratio = 1), device = device)
    })
  output$downloadMedianPlot <- downloadHandler(
    filename = 'median.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = medianPlot() + coord_fixed(ratio = 1), device = device)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
