distanceTab <- tabPanel(
  'Distance',
   conditionalPanel(
     'output.IsMergedParticipantDistanceTable',
     h2("Participant Distance Information"),
     fixedRow(
       column(
         width = 12,
         shinycssloaders::withSpinner(
           tableOutput("mergedParticipantDistanceTable"),
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
   h2("Distance Scatter Diagrams"),
   shinycssloaders::withSpinner(uiOutput('distanceScatters'), type = 4)
  )