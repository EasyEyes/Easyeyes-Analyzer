distanceTab <- tabPanel(
  'Distance',
   conditionalPanel(
     'output.IsMergedParticipantDistanceTable',
     h2("Participant table"),
     fixedRow(
       column(
         width = 12,
        shinycssloaders::withSpinner(
          DT::dataTableOutput("mergedParticipantDistanceTable"),
          type = 4
        ),
         downloadButton("downloadParticipantDistanceInfo", "Download")
       )
     )
   ),
  #### histogram ####

   #### dot plots ####
   h2("Histograms colored by participant"),
   shinycssloaders::withSpinner(uiOutput('dotPlots'), type = 4),
   h2("Scatter diagrams"),
   shinycssloaders::withSpinner(uiOutput('distanceScatters'), type = 4)
  )