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
        style = "background-color: #e6f2ff;border-radius: 10px;",
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
            tags$span("good trials.")
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
          checkboxGroupInput(
            inputId = "conditionName",
            label = "conditionName",
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
          )
        )
      )
    ),
    # tabs
    tabPanel("Sessions", value = "Sessions", sessionTab),
    tabPanel("Stats", value = "Stats", statTab),
    tabPanel("Plots", value = "Plots", plotsTab),
    tabPanel("Timing", value = 'Timing', timingTab),
    tabPanel("Staircases", value = "Staircases", staircasesTab),
    tabPanel("Sound", value = "Sound", soundTab),
    tabPanel("Profile", value = "Profile", profileTab),
    tabPanel("FormSpree", value = "FormSpree", formSpreeTab)
  )
)
