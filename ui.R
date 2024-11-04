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
timer_js <- "
  let startTime = new Date().getTime();
  function updateTimer() {
    let now = new Date().getTime();
    let elapsed = now - startTime;
    let seconds = Math.floor((elapsed / 1000) % 60);
    let minutes = Math.floor((elapsed / (1000 * 60)) % 60);
    let hours = Math.floor((elapsed / (1000 * 60 * 60)) % 24);

    document.getElementById('timer').innerHTML = 
        (hours < 10 ? '0' + hours : hours) + ':' + 
        (minutes < 10 ? '0' + minutes : minutes) + ':' + 
        (seconds < 10 ? '0' + seconds : seconds);

    // Check every second
    setTimeout(updateTimer, 1000);
  }
  document.addEventListener('DOMContentLoaded', (event) => {
      updateTimer();
  });
"



shinyUI(
  
  navbarPage(
    title = div(
      textOutput("app_title"), 
      div(id = "timer", "00:00:00")
    ),
    header = tags$head(
      tags$style(type = "text/css",  "
        #timer {
          position: absolute;  
          top: 15px;  
          right: 15px;  
          font-size: 16px;
          font-weight: bold;
          color: #FFFFFF; 
          background-color: rgba(0, 0, 0, 0.7); 
          padding: 5px;
          border-radius: 5px;
          z-index: 9999;
        }
      "),
      tags$script(HTML(timer_js))
    ),
    # tabs
    sessionTab,
    statTab,
    plotsTab,
    soundTab,
    profileTab,
    formSpreeTab
  )
)
