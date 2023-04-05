#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

data_table_call_back = "
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

library(shiny)
require(foreach)
require(dplyr)
require(readr)
require(ggplot2)
require(stringr)
require(emojifont)
require(DT)


source("preprocess.R")
source("./error report/summary_table.R")
source("threshold_and_warning.R")
source("./error report/random_rgb.R")
source("./plotting/mean_median_plot.R")
source("./plotting/regression_plot.R")
source("./plotting/histogram.R")
source("./plotting/crowding_plot.R")
library(bslib)
options(shiny.maxRequestSize=70*1024^2)

ui <- navbarPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
    .navbar{
    margin-bottom: 10px
    }
      .dataTables_filter{
      float: left !important;
      }
  .form-group {
  margin-bottom: 10px;
  margin-top: 10px;
  }
  .container-fluid {
  padding-left:5px;
  padding-right:0px;
  }
  "))),
  title = 'EasyEyes Analysis',
  tabPanel('Error Report', 
           span(textOutput("experiment"), style="font-size:20px; margin-top:0px"),
           fileInput("file", NULL, accept = ".csv", buttonLabel = "Select CSV files", multiple = T),
           div(style = "margin-top: -10px"),
           fluidRow(column(width = 2,downloadButton("report", "Download report"))),
           textOutput("instruction"),
           DT::dataTableOutput('ex1')),
  tabPanel('Threshold',  
           fluidRow(tableOutput('ex3')),
           fluidRow(tableOutput('ex2'))),
  tabPanel('Plots', 
           splitLayout(cellWidths = c("50%", "50%"),
                       plotOutput("meanPlot", width = "100%"),
                       plotOutput("medianPlot", width = "100%")),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadMeanPlot", "Download"), 
                       downloadButton("downloadMedianPlot", "Download")
                       ),
           splitLayout(cellWidths = c("50%", "50%"),
                       plotOutput("regressionPlot", width = "100%"),
                       plotOutput("fluencyHistogram", width = "100%")
                       ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRegressionPlot", "Download"),
                       downloadButton("downloadFluencyHistogram", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       plotOutput("retentionHistogram", width = "100%"),
                       plotOutput("crowdingScatterPlot", width = "100%")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRetentionHistogram", "Download"),
                       downloadButton("downloadCrowdingScatterPlot", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       plotOutput("crowdingAvgPlot", width = "100%"),
                       # plotOutput("crowdingScatterPlot", width = "100%")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadCrowdingAvgPlot", "Download"),
                       # downloadButton("downloadCrowdingScatterPlot", "Download")
           )
           
  )
)

server <- function(input, output, session) {
  output$file_name <- renderText({input$file_name})
  files <- reactive({
    require(input$file)
    return(read_files(input$file))
  })
  data_list <- reactive({
    return(files()[[1]])
  })
  
  summary_list <- reactive({
    return(files()[[2]])
  })
  
  experiment_names <- reactive({
    return(trimws(files()[[3]]))
  })
  
  readingCorpus <- reactive({
    return(trimws(files()[[4]]))
  })
  
  summary_table <- reactive({
    generate_summary_table(data_list())
  })
  
  threshold_and_warnings <- reactive({
    require(input$file)
    return(generate_threshold(data_list(), summary_list()))
  })
  
  df_list <- reactive({
    return(generate_rsvp_reading_crowding_fluency(data_list(), summary_list()))
    })
  
  reaing_rsvp_crowding_df <- reactive({return(get_mean_median_df(df_list()))})
  
  meanPlot <- reactive({
    require(input$file)
    mean_plot(reaing_rsvp_crowding_df())
  })
  
  medianPlot <- reactive({
    require(input$file)
    median_plot(reaing_rsvp_crowding_df())
  })
  
  crowdingBySide <- reactive({
    crowding_by_side(df_list()[[2]])
  })
  
  crowdingPlot <- reactive({
    crowding_scatter_plot(crowdingBySide())
  })
  
  crowdingAvgPlot <- reactive({
    crowding_mean_scatter_plot(crowdingBySide())
  })
  
  regressionPlot <- reactive({
    regression_plot(df_list())
  })
  
  fluency_histogram <- reactive({
    get_fluency_histogram(df_list()[[4]])
  })
  
  retention_histogram <- reactive({
    get_reading_retention_histogram(data_list(), df_list()[[1]])
  })
  
  observeEvent(input$file, 
               {
                 set.seed(2023)
                 participants <- reactive({
                   unique(summary_table()$`Pavlovia session ID`)
                   })
                 output$ex1 <- DT::renderDataTable(
                   datatable(
                     summary_table(),
                     class = list(stripe = FALSE),
                     selection = 'none',
                     filter = "top",
                     escape = FALSE,
                     width = "200%",
                     options = list(
                       autoWidth = FALSE,
                       paging = FALSE,
                       scrollX=TRUE,
                       columnDefs = list(
                         list(visible = FALSE, targets = c(0)),
                         list(targets = c(9),
                              width = '500px',
                              className = 'details-control1',
                              render = JS(
                                "function(data, type, row, meta) {",
                                "return type === 'display' && data.length > 20 ?",
                                "data.substr(0, 20) + '...' : data;",
                                "}")),
                         list(targets = c(10),
                              width = '250px',
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
                     callback = JS(
                       data_table_call_back
                     )) %>%
                     formatStyle(names(summary_table()), lineHeight="15px") %>% 
                     formatStyle(names(summary_table())[-1],
                                 'Pavlovia session ID',
                                 backgroundColor = styleEqual(participants(), random_rgb(length(participants()))))
                 )
                 
                 output$ex2 <- renderTable(
                   threshold_and_warnings()[[2]] %>% select(-thresholdParameter)
                 )
                 output$ex3 <- renderTable(
                   threshold_and_warnings()[[1]]
                 )
                 output$instruction <- renderText("In each column’s heading, to the right are buttons to sort up and down, and below is a text box for selection.")
                 output$experiment <- renderText(experiment_names())
                 plt_theme <- theme(legend.position = "right", 
                                    legend.box = "vertical", 
                                    legend.justification = c(1,1),
                                    legend.margin = margin(-0.4),
                                    legend.key.size = unit(4.5, "mm"),
                                    legend.title = element_text(size=14),
                                    legend.text = element_text(size=14),
                                    panel.grid.major = element_blank(), 
                                    panel.grid.minor = element_blank(),
                                    panel.background = element_blank(), 
                                    axis.title = element_text(size = 14),
                                    axis.text = element_text(size = 14),
                                    axis.line = element_line(colour = "black"),
                                    plot.title = element_text(size=14))
                 output$meanPlot <- renderPlot({
                   meanPlot() + 
                     plt_theme + 
                     coord_fixed(ratio = 1) + 
                     labs(title = paste(c("Mean", experiment_names(), readingCorpus()), collapse = "\n"))
                   }, res = 96)
                 output$medianPlot <- renderPlot({
                   medianPlot() + 
                     plt_theme + 
                     coord_fixed(ratio = 1) + 
                     labs(title = paste(c("Mean", experiment_names(), readingCorpus()), collapse = "\n"))
                   }, res = 96)
                 output$regressionPlot <- renderPlot({
                   regressionPlot() + plt_theme + coord_fixed(ratio = 1)
                 })
                 output$fluencyHistogram <- renderPlot({
                   fluency_histogram() + plt_theme
                 })
                 output$retentionHistogram <- renderPlot({
                   retention_histogram() + plt_theme
                 })
                 output$crowdingScatterPlot <- renderPlot({
                   crowdingPlot()
                 })
                 output$crowdingAvgPlot <- renderPlot({
                   crowdingAvgPlot() + plt_theme + coord_fixed(ratio = 1)
                 })
               })
  
  
  downloadtheme <- theme(legend.position = "right", 
                         legend.box = "vertical", 
                         legend.justification = c(1,1),
                         legend.margin = margin(-0.4),
                         legend.key.size = unit(4.5, "mm"),
                         legend.title = element_text(size=16),
                         legend.text = element_text(size=16),
                         panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank(), axis.title = element_text(size = 16),
                         axis.text = element_text(size = 16),
                         axis.line = element_line(colour = "black"),
                         plot.title = element_text(size=16))
  
  output$report <- downloadHandler(
    filename = function(){ ifelse(experiment_names() == "", "error report.html", paste0(experiment_names(), ".html"))},
    content = function(file) {
      tempReport <- file.path(tempdir(), "error report.Rmd")
      file.copy("error report.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        params = summary_table() %>% mutate(experiment = experiment_names()),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$downloadMeanPlot <- downloadHandler(
    filename = 'mean.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 250, units = "in")
      }
      ggsave(file, plot = meanPlot() + downloadtheme + coord_fixed(ratio = 1), device = device)
    })
  output$downloadMedianPlot <- downloadHandler(
    filename = 'median.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = medianPlot() + downloadtheme + coord_fixed(ratio = 1), device = device, dpi = 150)
    })
  output$downloadRegressionPlot<- downloadHandler(
    filename = 'regression.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = regressionPlot() + downloadtheme + coord_fixed(ratio = 1), device = device, dpi = 150)
    })
  output$downloadFluencyHistogram<- downloadHandler(
    filename = 'fluency.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = fluency_histogram() + downloadtheme, device = device)
    })
  output$downloadRetentionHistogram<- downloadHandler(
    filename = 'retention.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = retention_histogram() + downloadtheme, device = device)
    })
  
  output$downloadCrowdingScatterPlot<- downloadHandler(
    filename = 'crowding_left_vs_right.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = crowdingPlot(), device = device)
    })
  output$downloadCrowdingAvgPlot <- downloadHandler(
    filename = 'average_crowding_left_vs_right.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 150, units = "in")
      }
      ggsave(file, plot = crowdingAvgPlot() + downloadtheme + coord_fixed(ratio = 1), device = device)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
