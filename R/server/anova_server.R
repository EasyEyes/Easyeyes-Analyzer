#### ANOVA server module ####
anovaTabServer <- function(id,
                           df_list,
                           experiment_names,
                           app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
    anova_results <- reactive({
      app_profile_time(app_profiler, "ANOVA results", {
        calculate_anova(df_list())
      })
    })

    render_anova_summary <- function(measure_key) {
      renderTable({
        measure <- anova_results()[[measure_key]]
        if (!is.null(measure$summary)) {
          measure$summary[[1]]
        }
      }, rownames = TRUE)
    }

    render_anova_pairwise <- function(measure_key) {
      renderTable({
        measure <- anova_results()[[measure_key]]
        if (!is.null(measure$pairwise)) {
          measure$pairwise$p.value
        }
      }, rownames = TRUE)
    }

    output$readingANOVA <- render_anova_summary("reading")
    output$crowdingANOVA <- render_anova_summary("crowding")
    output$rsvpANOVA <- render_anova_summary("rsvp")
    output$beautyANOVA <- render_anova_summary("beauty")
    output$comfortANOVA <- render_anova_summary("comfort")

    output$readingPairwise <- render_anova_pairwise("reading")
    output$crowdingPairwise <- render_anova_pairwise("crowding")
    output$rsvpPairwise <- render_anova_pairwise("rsvp")
    output$beautyPairwise <- render_anova_pairwise("beauty")
    output$comfortPairwise <- render_anova_pairwise("comfort")

    output$downloadAnovaTables <- downloadHandler(
      filename = function() {
        paste0(get_short_experiment_name(experiment_names()), "anova-tables.pdf")
      },
      content = function(file) {
        pdf(file, width = 8.5, height = 11, onefile = TRUE)

        anova_data <- anova_results()
        measures <- list(
          list(key = "reading", name = "Reading Speed"),
          list(key = "crowding", name = "Crowding Distance"),
          list(key = "rsvp", name = "RSVP Reading Speed"),
          list(key = "beauty", name = "Beauty Rating"),
          list(key = "comfort", name = "Comfort Rating")
        )

        for (measure in measures) {
          grid.newpage()
          grid.text(
            paste("ANOVA Analysis:", measure$name),
            x = 0.5, y = 0.95,
            gp = gpar(fontsize = 16, fontface = "bold"),
            just = "center"
          )

          measure_data <- anova_data[[measure$key]]

          if (!is.null(measure_data) && !is.null(measure_data$summary)) {
            if (is.list(measure_data$summary) && length(measure_data$summary) > 0) {
              anova_table <- measure_data$summary[[1]]
              if (!is.null(anova_table) && nrow(anova_table) > 0) {
                anova_grob <- tableGrob(
                  anova_table,
                  rows = rownames(anova_table),
                  theme = ttheme_default(
                    core = list(fg_params = list(cex = 0.6)),
                    colhead = list(fg_params = list(cex = 0.7, fontface = "bold")),
                    rowhead = list(fg_params = list(cex = 0.6, fontface = "bold"))
                  )
                )

                grid.text(
                  "ANOVA Results",
                  x = 0.5, y = 0.8,
                  gp = gpar(fontsize = 12, fontface = "bold"),
                  just = "center"
                )

                vp_anova <- viewport(x = 0.5, y = 0.65, width = 0.9, height = 0.15)
                pushViewport(vp_anova)
                grid.draw(anova_grob)
                popViewport()
              }
            }

            if (!is.null(measure_data$pairwise) && !is.null(measure_data$pairwise$p.value)) {
              pairwise_table <- measure_data$pairwise$p.value
              if (!is.null(pairwise_table) && nrow(pairwise_table) > 0) {
                pairwise_grob <- tableGrob(
                  pairwise_table,
                  rows = rownames(pairwise_table),
                  theme = ttheme_default(
                    core = list(fg_params = list(cex = 0.6)),
                    colhead = list(fg_params = list(cex = 0.7, fontface = "bold")),
                    rowhead = list(fg_params = list(cex = 0.6, fontface = "bold"))
                  )
                )

                grid.text(
                  "Pairwise Comparisons (Bonferroni corrected)",
                  x = 0.5, y = 0.45,
                  gp = gpar(fontsize = 12, fontface = "bold"),
                  just = "center"
                )

                vp_pairwise <- viewport(x = 0.5, y = 0.3, width = 0.9, height = 0.25)
                pushViewport(vp_pairwise)
                grid.draw(pairwise_grob)
                popViewport()
              }
            }
          } else {
            grid.text(
              "No data available for this measure",
              x = 0.5, y = 0.5,
              gp = gpar(fontsize = 12, col = "red"),
              just = "center"
            )
          }
        }

        dev.off()
      }
    )
  })
}
