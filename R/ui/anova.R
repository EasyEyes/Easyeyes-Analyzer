anovaTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        12,
        h3("ANOVA Analysis by Font"),
        p("Analysis of variance and pairwise comparisons between fonts for each measure."),
        br(),
        downloadButton(ns("downloadAnovaTables"), "Download")
      )
    ),
    fluidRow(
      column(
        6,
        h4("Reading Speed"),
        h5("ANOVA Results"),
        tableOutput(ns("readingANOVA")),
        h5("Pairwise Comparisons (Bonferroni corrected)"),
        tableOutput(ns("readingPairwise"))
      ),
      column(
        6,
        h4("Crowding Distance"),
        h5("ANOVA Results"),
        tableOutput(ns("crowdingANOVA")),
        h5("Pairwise Comparisons (Bonferroni corrected)"),
        tableOutput(ns("crowdingPairwise"))
      )
    ),
    fluidRow(
      column(
        6,
        h4("RSVP Reading Speed"),
        h5("ANOVA Results"),
        tableOutput(ns("rsvpANOVA")),
        h5("Pairwise Comparisons (Bonferroni corrected)"),
        tableOutput(ns("rsvpPairwise"))
      ),
      column(
        6,
        h4("Beauty Rating"),
        h5("ANOVA Results"),
        tableOutput(ns("beautyANOVA")),
        h5("Pairwise Comparisons (Bonferroni corrected)"),
        tableOutput(ns("beautyPairwise"))
      )
    ),
    fluidRow(
      column(
        6,
        h4("Comfort Rating"),
        h5("ANOVA Results"),
        tableOutput(ns("comfortANOVA")),
        h5("Pairwise Comparisons (Bonferroni corrected)"),
        tableOutput(ns("comfortPairwise"))
      ),
      column(6)
    )
  )
}
