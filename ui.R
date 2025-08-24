# Tab modules
source('./constant.R')
source('./tabs/session.R')
source('./tabs/stat.R')
source('./tabs/plots.R')
source('./tabs/staircases.R')
source('./tabs/sound.R')
source('./tabs/profile.R')
source('./tabs/formSpreeDash.R')
source('./tabs/timming.R')
source('./tabs/quality.R')
source('./tabs/anova.R')
# packages
library(shiny)
library(shinytitle)
library(svglite)
library(shinycssloaders)
library(shinyjs)
library(shinyalert)
shinyUI(
  navbarPage(
    id = "navbar",
    title = div(textOutput("app_title"),
                div(id = "timer", "00:00:00")),
    use_shiny_title(),
    header = tagList(
      tags$head(
        tags$script(type = "text/javascript",
                    src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.7/MathJax.js?config=TeX-MML-AM_CHTML"),
        tags$script(src = "ui.js"),
        tags$script(src = "controlPanel.js"),
        tags$link(rel = "stylesheet", type = "text/css", href = "ui.css")
      ),
      div(
        style = "background-color: #e6f2ff;border-radius: 10px; padding: 3pt; margin-bottom: 3pt;",
        id = "controlPanel",
        useShinyjs(),
        div(
          style = "margin-left:12px;",
          div(
            fileInput(
              "file",
              NULL,
              accept = c(".csv", ".zip", '.xlsx'),
              buttonLabel = "Select CSV files or ZIP file",
              multiple = T,
              width = '1000px'
            ) |>
              tagAppendAttributes(onInput = "const id = $(this).find('input[type=\"file\"]').attr('id');
                         Shiny.setInputValue(id + '_click', Math.random());")
          ),
          htmlOutput("fileStatusMessage"),
          radioButtons(
            "fileType",
            "Select download file type:",
            c(
              "eps" = "eps",
              "pdf" = "pdf",
              "png" = "png",
              "svg" = "svg"
            ),
            inline = TRUE,
            selected = "png"
          ),
          downloadButton("downloadAll", "Download All"),
          radioButtons(
            "filterInput",
            "Select participants by reading speed:",
            c(
              'all' = 'all',
              'slowest 25%' = 'slowest',
              'fastest 75%' = 'fastest'
            ),
            inline = TRUE,
            selected = "all"
          ),
          radioButtons(
            "skillFilter",
            "Select participants by skill level:",
            c(
              "all participants"    = "all",
              "only skilled readers"   = "skilled",
              "only unskilled readers" = "unskilled"
            ),
            inline = TRUE,
            selected = "all"
          ),
          div(
            style = "display: flex; align-items: center; flex-wrap: wrap; gap: 5px;",
            tags$span("Exclude spacingDeg thresholds with fewer than"),
            numericInput(
              'NQuestTrials',
              NULL,
              value = 10,
              min = 1,
              width = '60px'
            ),
            tags$span("good Quest trials.")
          ),
          div(
            style = "display: flex; align-items: center; flex-wrap: wrap; gap: 5px;",
            tags$span("Exclude thresholds with fewer than"),
            numericInput(
              'NWrongTrials',
              NULL,
              value = 0,
              min = 0,
              width = '60px'
            ),
            tags$span("wrong Quest trials.")
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            tags$span("Exclude thresholds with QUEST SD >"),
            numericInput(
              'maxQuestSD',
              NULL,
              value = 0.2,
              min = 0,
              width = '60px'
            ),
            tags$span(".")
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            tags$span("Exclude ordinary reading speeds over"),
            numericInput(
              'maxReadingSpeed',
              NULL,
              value = 10000,
              min = 0,
              width = '90px'
            ),
            tags$span("words/min.")
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            tags$span("  Exclude participants with rulerCm <"),
            numericInput(
              'minRulerCm',
              NULL,
              value = 91,
              min = 0,
              width = '90px'
            ),
            tags$span(" cm")
          ),
          div(
            style = "display: flex; align-items: center; gap: 5px;",
            tags$span("calibrateTrackDistanceCheckLengthSDLogAllowed:"),
            numericInput(
              'calibrateTrackDistanceCheckLengthSDLogAllowed',
              NULL,
              value = 0.015,
              min = 0,
              width = '90px'
            )
          ),
          checkboxGroupInput(
            inputId = "conditionName",
            label = "Include these conditionNames:",
            inline = FALSE,
            choices = NULL,
            selected = NULL
          ),
          div(
            id = "thresholdParameterSelector",
            style = "display: none;",
            selectInput(
              'thresholdParameter',
              'thresholdParameter:',
              choices =  NULL,
              selected = NULL
            )
          ),
          HTML("In all plots, unless otherwise indicated: <br> 
     <ul>
         <li>Each point represents all the relevant data for one participant.</li>
         <li>Each font (and language) is represented by a different color.</li>
     </ul>")
        )
      )
    ),
    # tabs
    tabPanel("Sessions", value = "Sessions", sessionTab),
    tabPanel("Stats", value = "Stats", statTab),
    tabPanel("Plots", value = "Plots", plotsTab),
    tabPanel("ANOVA", value = "Anova", anovaTab),
    tabPanel("Quality", value = "Quality", qualityTab),
    tabPanel("Timing", value = 'Timing', timingTab),
    tabPanel("Staircases", value = "Staircases", staircasesTab),
    tabPanel("Sound", value = "Sound", soundTab),
    tabPanel("Profile", value = "Profile", profileTab),
    tabPanel("FormSpree", value = "FormSpree", formSpreeTab)
  )
)
