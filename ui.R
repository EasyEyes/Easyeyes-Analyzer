# Tab modules
source('./constant.R')
source('./tabs/session.R')
source('./tabs/stat.R')
source('./tabs/plots.R')
source('./tabs/sound.R')
source('./tabs/profile.R')
source('./tabs/formSpreeDash.R')

# packages
library(shiny)
library(svglite)
library(shinycssloaders)
library(plotly)
# Define UI for application that draws a histogram
shinyUI(
  
  navbarPage(
    title = textOutput("app_title"),
    sessionTab,
    statTab,
    plotsTab,
    soundTab,
    profileTab,
    formSpreeTab
  )
)
