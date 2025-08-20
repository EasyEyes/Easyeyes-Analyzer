anovaTab <- tabPanel(
  'Anova',
  fluidRow(
    column(12,
      h3("ANOVA Analysis by Font"),
      p("Analysis of variance and pairwise comparisons between fonts for each measure.")
    )
  ),
  
  fluidRow(
    column(6,
      h4("Reading Speed"),
      h5("ANOVA Results"),
      tableOutput("readingANOVA"),
      h5("Pairwise Comparisons (Bonferroni corrected)"),
      tableOutput("readingPairwise")
    ),
    column(6,
      h4("Crowding Distance"),
      h5("ANOVA Results"),
      tableOutput("crowdingANOVA"),
      h5("Pairwise Comparisons (Bonferroni corrected)"),
      tableOutput("crowdingPairwise")
    )
  ),
  
  fluidRow(
    column(6,
      h4("RSVP Reading Speed"),
      h5("ANOVA Results"),
      tableOutput("rsvpANOVA"),
      h5("Pairwise Comparisons (Bonferroni corrected)"),
      tableOutput("rsvpPairwise")
    ),
    column(6,
      h4("Beauty Rating"),
      h5("ANOVA Results"),
      tableOutput("beautyANOVA"),
      h5("Pairwise Comparisons (Bonferroni corrected)"),
      tableOutput("beautyPairwise")
    )
  ),
  
  fluidRow(
    column(6,
      h4("Comfort Rating"),
      h5("ANOVA Results"),
      tableOutput("comfortANOVA"),
      h5("Pairwise Comparisons (Bonferroni corrected)"),
      tableOutput("comfortPairwise")
    ),
    column(6,
      # Empty column for layout balance
    )
  )
)