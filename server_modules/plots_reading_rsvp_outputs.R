# Plots-tab ggiraph outputs (reading, RSVP, acuity, crowding).
# Called from server.R; must not live in timing_server (Timing tab uses namespaced outputs).

register_plots_reading_rsvp_ggiraph_outputs <- function(output,
                                                         experiment_names,
                                                         rsvpCrowding,
                                                         rsvpAcuityFoveal,
                                                         rsvpAcuityPeripheral,
                                                         rsvp_repeated_letter_crowding,
                                                         ordinaryAcuityFoveal,
                                                         ordinaryAcuityPeripheral,
                                                         ordinaryCrowdingPlots,
                                                         readingRepeatedPlots) {
  output$rsvpCrowdingPeripheralGradePlot <-
    ggiraph::renderGirafe({
      tryCatch({
        plot_with_title <- add_experiment_title(rsvpCrowding()$p_grade, experiment_names())
        ggiraph::girafe(ggobj = plot_with_title)
      }, error = function(e) {
        log_detailed_error(e, "rsvpCrowdingPeripheralGradePlot")
        error_plot <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("Error:", e$message),
            color = "red",
            size = 5,
            hjust = 0.5,
            vjust = 0.5
          ) +
          theme_void() +
          labs(subtitle = "rsvp-vs-peripheral-crowding-by-grade")
        ggiraph::girafe(ggobj = error_plot)
      })
    })

  output$rsvpCrowdingPeripheralFontPlot <-
    ggiraph::renderGirafe({
      tryCatch({
        plot_with_title <- add_experiment_title(rsvpCrowding()$p_font, experiment_names())
        ggiraph::girafe(ggobj = plot_with_title)
      }, error = function(e) {
        log_error("rsvpCrowding p_font: ", conditionMessage(e))
        error_plot <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("Error:", e$message),
            color = "red",
            size = 5,
            hjust = 0.5,
            vjust = 0.5
          ) +
          theme_void() +
          labs(subtitle = "rsvp-vs-peripheral-crowding-by-grade")
        ggiraph::girafe(ggobj = error_plot)
      })
    })

  output$rsvpResidualCrowding <-
    ggiraph::renderGirafe({
      tryCatch({
        plot_with_title <- add_experiment_title(rsvpCrowding()$residual, experiment_names())
        ggiraph::girafe(ggobj = plot_with_title)
      }, error = function(e) {
        log_error("rsvpCrowding residual: ", conditionMessage(e))
        error_plot <- ggplot() +
          annotate(
            "text",
            x = 0.5,
            y = 0.5,
            label = paste("Error:", e$message),
            color = "red",
            size = 5,
            hjust = 0.5,
            vjust = 0.5
          ) +
          theme_void() +
          labs(subtitle = "residual-rsvp-vs-residual-peripheral-crowding-by-grade")
        ggiraph::girafe(ggobj = error_plot)
      })
    })

  output$rsvpCrowdingFovealGradePlot <- ggiraph::renderGirafe({
    plot_with_title <- add_experiment_title(rsvpCrowding()$f_grade, experiment_names())
    ggiraph::girafe(ggobj = plot_with_title)
  })

  output$rsvpFovealAcuityGradePlot <- ggiraph::renderGirafe({
    acuity_plots <- rsvpAcuityFoveal()
    plot_with_title <- add_experiment_title(acuity_plots$grade, experiment_names())
    ggiraph::girafe(ggobj = plot_with_title)
  })

  output$rsvpPeripheralAcuityGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(rsvpAcuityPeripheral()$grade, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$rsvpPeripheralAcuityFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(rsvpAcuityPeripheral()$font, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$rsvpRepeatedGradePlot <- ggiraph::renderGirafe({
    plot_with_title <- add_experiment_title(rsvp_repeated_letter_crowding()[[2]], experiment_names())
    ggiraph::girafe(ggobj = plot_with_title)
  })

  output$ordinaryFovealAcuityGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryAcuityFoveal()$grade, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryFovealAcuityFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryAcuityFoveal()$font, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryPeripheralAcuityGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryAcuityPeripheral()$grade, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryPeripheralAcuityFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryAcuityPeripheral()$font, experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryPeripheralCrowdingFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[1]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryPeripheralCrowdingGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[3]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryFovealCrowdingFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[2]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$ordinaryFovealCrowdingGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[4]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })

  output$readingRepeatedGradePlot <- ggiraph::renderGirafe({
    plot_with_title <- add_experiment_title(readingRepeatedPlots()[[2]], experiment_names())
    ggiraph::girafe(ggobj = plot_with_title)
  })

  invisible(NULL)
}
