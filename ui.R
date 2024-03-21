# Tab modules
source('./constant.R')
source('./tabs/session.R')
source('./tabs/stat.R')
source('./tabs/plots.R')
source('./tabs/sound.R')
source('./tabs/profile.R')
# packages
library(shiny)
library(svglite)
library(shinycssloaders)
# Define UI for application that draws a histogram
shinyUI(
  navbarPage(
    title = textOutput("app_title"),
    sessionTab,
    statTab,
    plotsTab,
    soundTab,
    profileTab,
   #### Bits ####
    tabPanel(
      'Bits',
      h3("All blocks"),
      textOutput('all participant I'),
      # splitLayout(cellWidths = c("50%", "50%"),
      #             tableOutput('all participant prob')
      # ),
      h3("Each block"),
      tableOutput('each block I')
    )
  )
)
