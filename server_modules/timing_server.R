#### Timing server module ####
timingTabServer <- function(id,
                            files,
                            conditionNames,
                            experiment_names,
                            fileType,
                            uploaded_file,
                            app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  #### duration data ####
  
  durationData <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Timing duration data", {
      get_duration_data(files()$data_list, conditionNames())
    })
  })

  durationCorrMatrix <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Timing duration correlation matrix", {
      get_duration_corr(files()$data_list, conditionNames())
    })
  })

  timingHistograms <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    l <- list()
    fileNames <- list()
    
    app_profile_time(app_profiler, "Timing histogram list", {
      append_hist_time(files()$data_list, l, fileNames, conditionNames())
    })
  })
  

  timingHistRenderCount <- reactiveVal(0)

  observeEvent(timingHistograms(), { timingHistRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(files(), { timingHistRenderCount(0) }, ignoreInit = TRUE)

  observe({
    total <- length(timingHistograms()$plotList)
    current <- timingHistRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      timingHistRenderCount(current + 1)
    }
  })

  scatterTime <- reactive({
    if (is.null(uploaded_file()) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    l <- list()
    fileNames <- list()
    
    app_profile_time(app_profiler, "Timing scatter list", {
      # Extract scatter plots using the append_scatter_time function
      scatter_time_plots <-
        append_scatter_time(files()$data_list, l, fileNames, conditionNames())
      
      # Apply experiment title to all plots
      updated_plots <- lapply(scatter_time_plots$plotList, function(plot) {
        if (!is.null(plot)) {
          plot <- plot + scale_color_manual(values = colorPalette)
          plot <- add_experiment_title(plot, experiment_names())
        }
        return(plot)
      })
      
      return(list(
        plotList = updated_plots,
        fileNames = scatter_time_plots$fileNames
      ))
    })
  })
  
  scatterTimeParticipant <- reactive({
    if (is.null(uploaded_file()) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    l <- list()
    fileNames <- list()
    
    app_profile_time(app_profiler, "Timing participant scatter list", {
      # Extract scatter plots using the append_scatter_time function
      scatter_time_plots <-
        append_scatter_time_participant(files()$data_list, l, fileNames, conditionNames())
      
      # Apply experiment title to all plots
      updated_plots <- lapply(scatter_time_plots$plotList, function(plot) {
        if (!is.null(plot)) {
          plot <- plot + scale_color_manual(values = colorPalette)
          plot <- add_experiment_title(plot, experiment_names())
        }
        return(plot)
      })
      
      return(list(
        plotList = updated_plots,
        fileNames = scatter_time_plots$fileNames
      ))
    })
  })
  

  duration_lateness_hist <- reactive({
    if (is.null(durationData())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Timing duration lateness histograms", {
      get_histogram_duration_lateness(durationData())
    })
  })
  

  output$isDuration <- reactive({
    return(nrow(durationData()) > 0)
  })
  
  output$isDurationCorrMatrixAvailable <- reactive({
    return(!is.null(durationCorrMatrix()))
  })
  outputOptions(output, 'isDurationCorrMatrixAvailable', suspendWhenHidden = FALSE)
  

  output$durationCorrMatrixPlot <- renderImage({
    # Check if duration correlation matrix data is available
    if (is.null(durationCorrMatrix())) {
      return(NULL)
    }
    
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      p <- add_experiment_title(durationCorrMatrix()$plot, experiment_names())
      ggsave(
        file = tmp_svg,
        plot = p,
        device = svglite,
        width = durationCorrMatrix()$width,
        height = durationCorrMatrix()$height
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- if (!is.null(durationCorrMatrix()$width) && durationCorrMatrix()$width > 0) (durationCorrMatrix()$height / durationCorrMatrix()$width) else 1
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
    }, error = function(e) {
      handle_plot_error(e, "durationCorrMatrixPlot", experiment_names(), "Duration Correlation Matrix")
    })
  }, deleteFile = TRUE)
  
  output$durationHist <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot =  duration_lateness_hist()$duration,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "durationHist", experiment_names(), "Duration Histogram")
    })
    
  }, deleteFile = TRUE)
  
  output$latenessHist <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot =  duration_lateness_hist()$lateness,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "latenessHist", experiment_names(), "Lateness Histogram")
    })
  }, deleteFile = TRUE)
  
  durationPlot <- reactive({
    app_profile_time(app_profiler, "Timing duration plots", {
      plot_duraction_sec(durationData())
    })
  })
  
  latenessPlot <- reactive({
    app_profile_time(app_profiler, "Timing lateness plots", {
      plot_Lateness_sec(durationData())
    })
  })
  
  output$durationByID <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot =  durationPlot()$participant,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "durationByID", experiment_names(), "Duration by Participant ID")
    })
  }, deleteFile = TRUE)
  
  output$durationByFont <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot =  durationPlot()$font,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "durationByFont", experiment_names(), "Duration by Font")
    })
  }, deleteFile = TRUE)
  
  output$durationWithFontPadding <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = durationPlot()$fontPadding,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      error_plot <- ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.6,
          label = paste("Error:", e$message),
          color = "red",
          size = 5,
          hjust = 0.5,
          vjust = 0.5
        ) +
        theme_void() +
        labs(subtitle =  
          'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding'
        )
      
      # Save the error plot to a temp file
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = error_plot,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    })
  }, deleteFile = TRUE)
  
  output$latenessByID <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot =  latenessPlot()$participant,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "latenessByID", experiment_names(), "Lateness by Participant ID")
    })
  }, deleteFile = TRUE)
  
  output$latenessByFont <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = latenessPlot()$font,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      handle_plot_error(e, "latenessByFont", experiment_names(), "Lateness by Font")
    })
  }, deleteFile = TRUE)
  
  output$latenessWithFontPadding <- renderImage({
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = latenessPlot()$fontPadding,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    }, error = function(e) {
      error_plot <- ggplot() +
        annotate(
          "text",
          x = 0.5,
          y = 0.6,
          label = paste("Error:", e$message),
          color = "red",
          size = 5,
          hjust = 0.5,
          vjust = 0.5
        ) +
        theme_void() +
        labs(subtitle = 'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding')
      
      # Save the error plot to a temp file
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = error_plot,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((4/6) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h/scale))
    })
  }, deleteFile = TRUE)

  #### rsvp ggiraph ####
  
  ordinaryAcuityFoveal <- reactive({
    plot_acuity_reading(df_list()$acuity, df_list()$reading, 'foveal')
  })
  
  ordinaryAcuityPeripheral <- reactive({
    plot_acuity_reading(df_list()$acuity, df_list()$reading, 'peripheral')
  })
  
  
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
          labs(subtitle='rsvp-vs-peripheral-crowding-by-grade')
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
          labs(subtitle='rsvp-vs-peripheral-crowding-by-grade')
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
          labs(subtitle='residual-rsvp-vs-residual-peripheral-crowding-by-grade')
        ggiraph::girafe(ggobj = error_plot)
      })
    })
  
  ordinaryCrowdingPlots <- reactive({
    plot_reading_crowding(df_list())
  })
  
  readingRepeatedPlots <- reactive({
    plot_reading_repeated_letter_crowding(df_list())
  })
  
  # Font-aggregated plots
  fontAggregatedReadingRsvpCrowding <- reactive({
    plot_font_aggregated_reading_rsvp_crowding(df_list())
  })
  
  fontAggregatedOrdinaryReadingCrowding <- reactive({
    plot_font_aggregated_ordinary_reading_crowding(df_list())
  })
  
  fontAggregatedRsvpCrowding <- reactive({
    plot_font_aggregated_rsvp_crowding(df_list())
  })

  output$rsvpCrowdingFovealGradePlot <- ggiraph::renderGirafe({
    plot_with_title <- add_experiment_title(rsvpCrowding()$f_grade, experiment_names())
    ggiraph::girafe(ggobj = plot_with_title)
  })
  
  output$rsvpFovealAcuityGradePlot <- ggiraph::renderGirafe({
    plot_with_title <- add_experiment_title(rsvpAcuityFoveal()[[2]], experiment_names())
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
      plot_with_title <- add_experiment_title(ordinaryAcuityFoveal()[[2]], experiment_names())
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
  
  # Peripheral Crowding Plots
  output$ordinaryPeripheralCrowdingFontPlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[1]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
      
      #temporarily change to colored by font
    })
  
  output$ordinaryPeripheralCrowdingGradePlot <-
    ggiraph::renderGirafe({
      plot_with_title <- add_experiment_title(ordinaryCrowdingPlots()[[3]], experiment_names())
      ggiraph::girafe(ggobj = plot_with_title)
    })
  
  # Font-aggregated plot outputs

  output$timingHistograms <- renderUI({
    out <- list()
    i <- 1
    
    while (i <= length(timingHistograms()$plotList) - 3) {
      # Create a row with 4 histograms
      out[[i]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          shinycssloaders::withSpinner(plotOutput(
            ns(paste0("timingHist", i)),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            ns(paste0("timingHist", i + 1)),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("timingHist", i + 2),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("timingHist", i + 3),
            width = "100%",
            height = "100%"
          ),
          type = 4)
        )
      # Create a row with 4 download buttons
      out[[i + 1]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          downloadButton(ns(paste0("downloadTimingHist", i)), 'Download'),
          downloadButton(ns(paste0("downloadTimingHist", i + 1)), 'Download'),
          downloadButton(ns(paste0("downloadTimingHist", i + 2)), 'Download'),
          downloadButton(ns(paste0("downloadTimingHist", i + 3)), 'Download')
        )
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(timingHistograms()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("timingHist", i + j - 1)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(ns(paste0("downloadTimingHist", i + j - 1)), 'Download')
      })
      
      # Fill remaining cells with empty space
      emptyCells <- 4 - remaining
      plotOutputs <- c(plotOutputs, rep("", emptyCells))
      downloadButtons <- c(downloadButtons, rep("", emptyCells))
      
      out[[length(out) + 1]] <-
        do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), plotOutputs))
      out[[length(out) + 1]] <-
        do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), downloadButtons))
    }
    
    # Render histograms and download handlers
    for (j in seq_along(timingHistograms()$plotList)) {
      local({
        ii <- j
        output[[paste0("timingHist", ii)]] <- renderImage({
          req(ii <= timingHistRenderCount())
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            # Don't add hist_theme to placeholder plots
            plot_to_save <- if (is_placeholder_plot(timingHistograms()$plotList[[ii]])) {
              timingHistograms()$plotList[[ii]]
            } else {
              timingHistograms()$plotList[[ii]] + hist_theme
            }
            ggsave(
              file = tmp_svg,
              plot = plot_to_save,
              device = svglite,
              width = 4,
              height = 3.5,
              unit = 'in',
              limitsize = FALSE
            )
            # Fit image to the container width to avoid horizontal scrollbars
            disp_w <- session$clientData[[paste0("output_", ns(paste0("timingHist", ii)), "_width")]]
            if (is.null(disp_w) || is.na(disp_w) || disp_w <= 0) disp_w <- 560
            scale <- 2
            png_w <- round(disp_w * scale)
            png_h <- round((3.5 / 4) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
          }, error = function(e) {
            log_error("Timing histogram render: ", conditionMessage(e))
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
              labs(subtitle=timingHistograms()$fileNames[[ii]])
            
            # Save the error plot to a temp file
            outfile <- tempfile(fileext = '.svg')
            ggsave(
              file = outfile,
              plot = error_plot,
              device = svglite,
              width = 6,
              height = 4,
              unit = 'in'
            )
            list(
              src = outfile,
              contenttype = 'svg',
              alt = paste0("Error in ", timingHistograms()$fileNames[[ii]])
            )
          })
          
        }, deleteFile = TRUE)
        outputOptions(output, paste0("timingHist", ii), suspendWhenHidden = TRUE)
        
        output[[paste0("downloadTimingHist", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              timingHistograms()$fileNames[[ii]],
              ii,
              '.',
              fileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(timingHistograms()$plotList[[ii]])) return(invisible(NULL))
              
              if (fileType() == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = timingHistograms()$plotList[[ii]] + hist_theme,
                  unit = "in",
                  width = 4,
                  # Reduced width
                  height = 3.5,
                  # Reduced height
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg,
                               file,
                               height = 900,
                               width = 900)  # Reduced resolution
              } else {
                ggsave(
                  file,
                  plot = timingHistograms()$plotList[[ii]] + hist_theme,
                  width = 4,
                  # Reduced width
                  height = 3.5,
                  # Reduced height
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    fileType() == "svg",
                    svglite::svglite,
                    fileType()
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  

  output$scatterTimeParticipant <- renderUI({
    out <- list()
    i = 1
    
    while (i <= length(scatterTimeParticipant()$plotList) - 1) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(
          plotOutput(
            ns(paste0("scatterTimeParticipant", i)),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        ),
        shinycssloaders::withSpinner(
          plotOutput(
            ns(paste0("scatterTimeParticipant", i + 1)),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        )
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(
          ns(paste0("downloadScatterTimeParticipant", i)),
          'Download'
        ),
        downloadButton(
          ns(paste0("downloadScatterTimeParticipant", i + 1)),
          'Download'
        )
      )
      i = i + 2
    }
    
    # Handle any remaining scatter plot
    if (i == length(scatterTimeParticipant()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(
          plotOutput(
            ns(paste0("scatterTimeParticipant", i)),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        )
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(
                                    ns(paste0("downloadScatterTimeParticipant", i)),
                                    'Download'
                                  ))
    }
    
    # Generate the plots and download handlers
    for (j in seq_along(scatterTimeParticipant()$plotList)) {
      local({
        ii <- j
        output[[paste0("scatterTimeParticipant", ii)]] <-
          renderImage({
            tryCatch({
              height_in <- 12.5
              tmp_svg <- tempfile(fileext = '.svg')
              ggsave(
                file = tmp_svg,
                plot = scatterTimeParticipant()$plotList[[ii]],
                device = svglite,
                width = 7,
                height = height_in,
                unit = 'in'
              )
              disp_w <- 700
              scale <- 2
              png_w <- disp_w * scale
              png_h <- round((height_in / 7) * png_w)
              outfile <- tempfile(fileext = ".png")
              rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
              list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
            }, error = function(e) {
              # Show error in a ggplot-friendly way
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
                labs(subtitle=scatterTimeParticipant()$fileNames[[ii]])
              
              # Save the error plot to a temp file
              tmp_svg <- tempfile(fileext = '.svg')
              ggsave(
                file = tmp_svg,
                plot = error_plot,
                device = svglite,
                width = 6,
                height = 4,
                unit = 'in'
              )
              disp_w <- 700
              scale <- 2
              png_w <- disp_w * scale
              png_h <- round((4 / 6) * png_w)
              outfile <- tempfile(fileext = ".png")
              rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
              list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
            })
            
          }, deleteFile = TRUE)
        
        
        output[[paste0("downloadScatterTimeParticipant", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterTimeParticipant()$fileNames[[ii]],
              ii,
              '.',
              fileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(scatterTimeParticipant()$plotList[[ii]])) return(invisible(NULL))
              
              if (fileType() == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = scatterTimeParticipant()$plotList[[ii]],
                  height = 12.5,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg, file, width = 1800)
              } else {
                ggsave(
                  file,
                  plot = scatterTimeParticipant()$plotList[[ii]],
                  height = 12.5,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    fileType() == "svg",
                    svglite::svglite,
                    fileType()
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  output$scatterTime <- renderUI({
    out <- list()
    i = 1
    
    while (i <= length(scatterTime()$plotList) - 1) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("scatterTime", i)),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("scatterTime", i + 1)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(ns(paste0("downloadScatterTime", i)), 'Download'),
        downloadButton(ns(paste0("downloadScatterTime", i + 1)), 'Download')
      )
      i = i + 2
    }
    
    # Handle any remaining scatter plot
    if (i == length(scatterTime()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("scatterTime", i)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(ns(paste0("downloadScatterTime", i)), 'Download'))
    }
    
    # Generate the plots and download handlers
    for (j in seq_along(scatterTime()$plotList)) {
      local({
        ii <- j
        output[[paste0("scatterTime", ii)]] <-
          renderImage({
            tryCatch({
              tmp_svg <- tempfile(fileext = '.svg')
              ggsave(
                file = tmp_svg,
                plot = scatterTime()$plotList[[ii]] +
                  plt_theme_scatter,
                width = 7,
                height = 6,
                unit = 'in',
                limitsize = F,
                device = svglite
              )
              disp_w <- 700
              scale <- 2
              png_w <- disp_w * scale
              png_h <- round((6 / 7) * png_w)
              outfile <- tempfile(fileext = ".png")
              rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
              list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
            }, error = function(e) {
              # Show error in a ggplot-friendly way
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
                labs(subtitle=scatterTime()$fileNames[[ii]])
              
              # Save the error plot to a temp file
              tmp_svg <- tempfile(fileext = '.svg')
              ggsave(
                file = tmp_svg,
                plot = error_plot,
                device = svglite,
                width = 6,
                height = 4,
                unit = 'in'
              )
              disp_w <- 700
              scale <- 2
              png_w <- disp_w * scale
              png_h <- round((4 / 6) * png_w)
              outfile <- tempfile(fileext = ".png")
              rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
              list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
            })
            
          }, deleteFile = TRUE)
        
        output[[paste0("downloadScatterTime", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterTime()$fileNames[[ii]],
              ii,
              '.',
              fileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(scatterTime()$plotList[[ii]])) return(invisible(NULL))
              
              if (fileType() == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = scatterTime()$plotList[[ii]] +
                    plt_theme_scatter,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg, file, width = 1800)
              } else {
                ggsave(
                  file,
                  plot = scatterTime()$plotList[[ii]] +
                    plt_theme_scatter,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    fileType() == "svg",
                    svglite::svglite,
                    fileType()
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  

  toListenTiming <- reactive({
    list(uploaded_file(), fileType())
  })

  observeEvent(toListenTiming(), {
    output$downlaodDurationByFont <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-font-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = durationPlot()$font,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = durationPlot()$font,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downlaodDurationByID <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-participant-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = durationPlot()$participant,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = durationPlot()$participant,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downlaodDurationWithFontPadding <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = durationPlot()$fontPadding,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = durationPlot()$fontPadding,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downlaodLatenessByFont <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-font-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = latenessPlot()$font,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = latenessPlot()$font,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downlaodLatenessByID <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-participant-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = latenessPlot()$participant,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = latenessPlot()$participant,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downlaodLatenessWithFontPadding <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = latenessPlot()$fontPadding,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = latenessPlot()$fontPadding,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downloadDurationHist <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-os-histogam',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = duration_lateness_hist()$duration,
            width = 6,
            height = 4,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = duration_lateness_hist()$duration,
            width = 6,
            height = 4,
            unit = "in",
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
    output$downloadLatenessHist <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-os-histogam',
        '.',
        fileType()
      ),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot =  duration_lateness_hist()$lateness,
            unit = "in",
            width = 6,
            height = 4,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = duration_lateness_hist()$lateness,
            unit = "in",
            width = 6,
            height = 4,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    

    output$downloadDurationCorrMatrixPlot <- downloadHandler(
      filename = paste0(get_short_experiment_name(experiment_names()),
                        'duration-correlation-matrix.',
                        fileType()),
      content = function(file) {
        if (fileType() == "png") {
          ggsave(
            "tmp.svg",
            plot =  durationCorrMatrix()$plot,
            width = durationCorrMatrix()$width,
            height = durationCorrMatrix()$height,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot =  durationCorrMatrix()$plot,
            width = durationCorrMatrix()$width,
            height = durationCorrMatrix()$height,
            unit = "in",
            limitsize = F,
            device = ifelse(
              fileType() == "svg",
              svglite::svglite,
              fileType()
            )
          )
        }
      }
    )
    
  })

  downloadSpecs <- reactive({
    specs <- list()
    duration_corr <- durationCorrMatrix()
    if (!is.null(duration_corr) && !is.null(duration_corr$plot)) {
      specs <- c(specs, list(plot_download_spec(
        plot = add_experiment_title(duration_corr$plot, experiment_names()),
        filename = "duration-correlation-matrix",
        width = duration_corr$width,
        height = duration_corr$height
      )))
    }

    duration_lateness <- duration_lateness_hist()
    if (!is.null(duration_lateness)) {
      specs <- c(specs, list(
        plot_download_spec(duration_lateness$duration, "targetMeasuredDurationSec-by-os-histogam", width = 6, height = 4),
        plot_download_spec(duration_lateness$lateness, "targetMeasuredLatenessSec-by-os-histogam", width = 6, height = 4)
      ))
    }

    durations <- durationPlot()
    if (!is.null(durations)) {
      specs <- c(specs, list(
        plot_download_spec(durations$font, "targetMeasuredDurationSec-by-font-plot"),
        plot_download_spec(durations$participant, "targetMeasuredDurationSec-by-participant-plot"),
        plot_download_spec(durations$fontPadding, "targetMeasuredDurationSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot")
      ))
    }

    latenesses <- latenessPlot()
    if (!is.null(latenesses)) {
      specs <- c(specs, list(
        plot_download_spec(latenesses$font, "targetMeasuredLatenessSec-by-font-plot"),
        plot_download_spec(latenesses$participant, "targetMeasuredLatenessSec-by-participant-plot"),
        plot_download_spec(latenesses$fontPadding, "targetMeasuredLatenessSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot")
      ))
    }

    specs <- c(
      specs,
      plot_list_download_specs(timingHistograms()$plotList, timingHistograms()$fileNames, theme = hist_theme, width = 4, height = 4, append_index = TRUE),
      plot_list_download_specs(scatterTime()$plotList, scatterTime()$fileNames, theme = plt_theme_scatter, width = 7, height = 7, append_index = TRUE),
      plot_list_download_specs(scatterTimeParticipant()$plotList, scatterTimeParticipant()$fileNames, width = 7, height = 12.5, append_index = TRUE)
    )

    specs
  })

  list(downloadSpecs = downloadSpecs)
  })
}
