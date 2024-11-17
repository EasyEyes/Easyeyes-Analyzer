# Tab modules
source('./constant.R')
source('./tabs/session.R')
source('./tabs/stat.R')
source('./tabs/plots.R')
source('./tabs/staircases.R')
source('./tabs/sound.R')
source('./tabs/profile.R')
source('./tabs/formSpreeDash.R')

# packages
library(shiny)
library(shinytitle)
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
    use_shiny_title(),
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
      tags$script(HTML(timer_js)),
      tags$script(HTML("
        // JavaScript function to update document title based on active tab
        function updateTitleBasedOnTab(tabName) {
          document.title = tabName + ' | EasyEyes Analysis';
        }

        // Set initial title to 'EasyEyes Analysis' with the default tab
        document.addEventListener('DOMContentLoaded', function() {
          updateTitleBasedOnTab('Session');  // Default tab name
        });

        // Update title whenever a tab is clicked
        $(document).on('shown.bs.tab', 'a[data-toggle=\"tab\"]', function (e) {
          var tabName = $(e.target).text();  // Get the active tab name
          updateTitleBasedOnTab(tabName);
        });
      "))
    ),
    
    # tabs
    tabPanel("Sessions", value = "Sessions", sessionTab),
    tabPanel("Stats", value = "Stats", statTab),
    tabPanel("Plots", value = "Plots", plotsTab),
    tabPanel("Staircases", value = "Staircases", staircasesTab),
    tabPanel("Sound", value = "Sound", soundTab),
    tabPanel("Profile", value = "Profile", profileTab),
    tabPanel("FormSpree", value = "FormSpree", formSpreeTab)
  )
)
