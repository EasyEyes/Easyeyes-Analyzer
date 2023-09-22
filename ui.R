#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('./constant.R')
library(shiny)
library(svglite)
# Define UI for application that draws a histogram
shinyUI(navbarPage(
  title = textOutput("app_title"),
  tabPanel('Sessions', 
           # span(textOutput("experiment"), style="font-size:20px; margin-top:0px"),
           tags$head(
             # Note the wrapping of the string in HTML()
             tags$style(HTML(".navbar{
             margin-bottom: 10px
             }
             .dataTables_filter{
             float: left !important;
             }
             .form-group {
             margin-bottom: 10px;
             }
             .form-group .form-control {
             width: 150px;
             }
             .container-fluid {
             padding-left:5px;
             padding-right:0px;
             }"))),
           tags$head(
             tags$script(
               type = "text/javascript",
               src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"
             )
           ),
           fluidRow(
                    column(width = 3, fileInput("file", NULL, accept = c(".csv", ".zip"), buttonLabel = "Select CSV files or ZIP file", multiple = T)),
                    column(width = 2, downloadButton("report", "Download report")),
                    column(width = 2, downloadButton("sessionCsv", "Download csv")),
                    column(width = 2, textInput("search", label = NULL))
           ),
           textOutput("instruction"),
           shinycssloaders::withSpinner(DT::dataTableOutput('ex1'), type = 4)),
  tabPanel('Stats',  
           fluidRow(column(width = 2, downloadButton("thresholdOne", "Download all participant")),
                    column(width = 2, downloadButton("thresholdTwo", "Download each participant")),
                    column(width = 2, downloadButton("thresholdThree", "Download all threshold"))),
           fluidRow(column(12, align = "center", tableOutput('ex3'))),
           fluidRow(column(12, align = "center", h3("Summary across participants"))),
           fluidRow(column(12, align = "center", tableOutput('ex2'))),
           fluidRow(column(12, align = "center", h3("Summary for each participant"))),
           fluidRow(column(width = 12,
                           align = "center",
                           shinycssloaders::withSpinner(tableOutput('ex4'), type = 4)))),
  tabPanel('Plots', 
           radioButtons("fileType", "Select download file type:",
                        c("pdf" = "pdf",
                          "eps" = "eps",
                          "svg" = "svg"),
                        inline = TRUE, selected = "pdf"),
           #### font size ####
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("readingVsXheightLog", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("readingVsXheightLinear", width = "100%"), type = 4)
                       ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadReadingVsXheightLog", "Download"),
                       downloadButton("downloadReadingVsXheightLinear", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("reading60cm1.2mmVs1.4mm", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("reading30cm1.2mmVs1.4mm", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadReading60cm1.2mmVs1.4mm", "Download"),
                       downloadButton("downloadReading30cm1.2mmVs1.4mm", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("reading60cmDiffVsAge", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadReading60cmDiffVsAge", "Download")
           ),
           #### mean - median ####
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("meanPlot", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("medianPlot", width = "100%"), type = 4)),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadMeanPlot", "Download"), 
                       downloadButton("downloadMedianPlot", "Download")
           ),
           #### regression ####
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("regressionPlot", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("regressionAndMeanPlot", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRegressionPlot", "Download"),
                       downloadButton("downloadRegressionAndMeanPlot", "Download")
           ),
           #### regression fonts ####
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("regressionFontPlot", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("regressionFontPlotWithLabel", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRegressionFontPlot", "Download"),
                       downloadButton("downloadRegressionFontPlotWithLabel", "Download")
           ),
           ####fluency ####
           h3("Fluency plots"),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("fluencyHistogram", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadFluencyHistogram", "Download")
           ),
           #### retention ####
           h3("Retention plots"),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("retentionHistogram", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("readingSpeedRetention", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRetentionHistogram", "Download"),
                       downloadButton("downloadReadingSpeedRetention", "Download")
           ),
           #### crowding ####
           h3("Crowding plots"),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("crowdingAvgPlot", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("crowdingScatterPlot", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadCrowdingAvgPlot", "Download"),
                       downloadButton("downloadCrowdingScatterPlot", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("SloanVsTimesMeanPlot", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("SloanVsTimesSDPlot", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadSloanVsTimesMeanPlot", "Download"),
                       downloadButton("downloadSloanVsTimesSDPlot", "Download")
           ),
           h3("Test and Retest plots"),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("readingTestRetest", width = "100%"), type = 4),
                       shinycssloaders::withSpinner(plotOutput("crowdingTestRetest", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadReadingTestRetest", "Download"),
                       downloadButton("downloadCrowdingTestRetest", "Download")
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       shinycssloaders::withSpinner(plotOutput("rsvpReadingTestRetest", width = "100%"), type = 4)
           ),
           splitLayout(cellWidths = c("50%", "50%"),
                       downloadButton("downloadRsvpReadingTestRetest", "Download")
           )
  ),
  tabPanel('Sound Calibration',  
           fluidRow(column(width = 4, downloadButton("sound_data", "Download sound calibration data"))),
           fluidRow(column(width = 4, fileInput("IRcsv", NULL, accept = c(".csv"), buttonLabel = "Upload IR csv file", multiple = F))),
           conditionalPanel(condition = "output.fileUploaded",
                            fluidRow(
                              column(width = 6, plotOutput("IRtmpOne", width = "100%")),
                            column(width = 6, plotOutput("IRtmpTwo", width = "100%"))),
                            fluidRow(column(width = 6,plotOutput("IRtmpThree", width = "100%"))),
                                                         ),
           fluidRow(column(width = 4, fileInput("fileJSON", NULL, accept = c(".json"), buttonLabel = "Upload IR json file", multiple = T))),
           conditionalPanel(condition = "output.jsonUploaded",
                            fluidRow(column(width = 12, align = "center", selectInput("jsonResult", "Select a result", choices = NULL))),
                            fluidRow(
                              column(width = 6, plotOutput("IRtmpFour", width = "100%")),
                              column(width = 6, plotOutput("IRtmpFive", width = "100%"))),
                            fluidRow(column(width = 6,plotOutput("IRtmpSix", width = "100%"))),
           ),
           fluidRow(column(width = 12,align = "center", h4("Sound Level at 1000 Hz"))),
           fluidRow(column(width = 12,align = "center", tableOutput('sound table'))),
           fluidRow(column(width = 12,align = "center", h4("Dynamic Range Compression Model"))),
           fluidRow(column(width = 12,align = "center", tableOutput('Dynamic Range Compression Model'))),
           fluidRow(column(width = 12,align = "center", h4("Plots"))),
           fluidRow(column(width = 12,align = "center", radioButtons("fileTypeSound", "Select download plot type:",
                                                                     c("pdf" = "pdf",
                                                                       "eps" = "eps",
                                                                       "svg" = "svg"),
                                                                     inline = TRUE, selected = "pdf"))),
           fluidRow(
                    column(width = 6,align = "right", 
                           shinycssloaders::withSpinner(plotOutput("sound level plot", width = "100%"), type = 4)),
                    column(width = 6, tags$div(h6(eq1_text, style="padding-top:100px;"),
                                               HTML("Based on Eq. 4 of Giannoulis et al. (2012)."),
                                               tags$a(href=reference,reference)))),
           fluidRow(column(width = 12,align = "center", downloadButton("downloadSoundLevelPlot", "Download"))),
           fluidRow(column(width = 3),
                    column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("record freq plot", width = "100%"), type = 4)),
                    column(width = 3)),
           fluidRow(column(width = 12,align = "center", downloadButton("downloadRecordFreqPlot", "Download"))),
           fluidRow(column(width = 12, align = "center", selectInput("round", "Select rounds", choices = NULL))),
           fluidRow(column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("impulseResponsePlot", width = "80%"))),
                    column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("microphoneRecordingPlot", width = "80%")))),
           fluidRow(column(width = 6,align = "center", downloadButton("downloadImpulseResponsePlot", "Download")),
                    column(width = 6,align = "center", downloadButton("downloadMicrophoneRecordingPlot", "Download"))),
           fluidRow(column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("impulseResponsePlotA", width = "80%"))),
                    column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("impulseResponsePlotB", width = "80%")))),
           fluidRow(column(width = 6,align = "center", downloadButton("downloadImpulseResponsePlotA", "Download")),
                    column(width = 6,align = "center", downloadButton("downloadImpulseResponsePlotB", "Download"))),
           fluidRow(column(width = 6,align = "center", 
                           shinycssloaders::withSpinner(plotOutput("impulseResponsePlotC", width = "80%")))
                    ),
           fluidRow(column(width = 6,align = "center", downloadButton("downloadImpulseResponsePlotC", "Download"))
                    )
  ),
  tabPanel('Bits',  
           h3("All blocks"),
           textOutput('all participant I'),
           # splitLayout(cellWidths = c("50%", "50%"),
           #             tableOutput('all participant prob')
           # ),
           h3("Each block"),
           tableOutput('each block I')
           )
  ))
