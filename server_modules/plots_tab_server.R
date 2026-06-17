#### Plots tab server (no moduleServer) ####
# Registers Plots-tab reactives and render* outputs on the main output/session.

with_plots_histogram_theme <- function(plot) {
  if (is_placeholder_plot(plot)) {
    return(plot)
  }
  plot + hist_theme
}

save_plots_histogram <- function(file, plot, file_type) {
  plot <- with_plots_histogram_theme(plot)
  if (file_type == "png") {
    plot <- apply_direct_png_theme(plot, profile = "histogram")
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      device = ragg::agg_png,
      width = 3.5,
      height = 3.5,
      units = "in",
      dpi = 200,
      limitsize = FALSE
    )
  } else {
    ggplot2::ggsave(
      file,
      plot = plot,
      width = 3.5,
      height = 3.5,
      units = "in",
      limitsize = FALSE,
      device = if (file_type == "svg") svglite::svglite else file_type
    )
  }
}

register_plots_tab_server <- function(output,
                                      session,
                                      input,
                                      files,
                                      df_list,
                                      experiment_names,
                                      downloadFileType,
                                      corrMatrix,
                                      minDegPlots,
                                      conditionNames,
                                      minCQAccuracy,
                                      crowdingBySide,
                                      fontAggregatedReadingRsvpCrowding,
                                      fontAggregatedOrdinaryReadingCrowding,
                                      fontAggregatedRsvpCrowding,
                                      app_profiler = NULL) {

  crowdingPlot <- reactive({
    if (is.null(crowdingBySide())) {
      return(NULL)
    }
    crowding_scatter_plot(crowdingBySide())
  })
  foveal_peripheral_diag <- reactive({
    req(input$file)
    get_foveal_peripheral_diag(df_list()$crowding)
  })
  foveal_crowding_vs_acuity_diag <- reactive({
    req(input$file)
    get_foveal_acuity_diag(df_list()$crowding, df_list()$acuity)
  })

  agePlots <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }

    app_profile_time(app_profiler, "Plots age plot list", {
    l <- list()
    fileNames <- list()

    peripheral_crowding_age_plots <- get_peripheral_crowding_vs_age(df_list()$crowding)

    plot_calls <- list(
      list(plot = peripheral_crowding_age_plots[[1]], fname = 'peripheral-crowding-vs-age-by-grade'),
      list(plot = peripheral_crowding_age_plots[[2]], fname = 'peripheral-crowding-ave-vs-age-by-grade'),
      list(plot = get_foveal_crowding_vs_age(df_list()$crowding), fname = 'foveal-crowding-vs-age-by-grade'),
      list(plot = get_repeatedLetter_vs_age(df_list()$repeatedLetters), fname = 'repeated-letter-crowding-vs-age-by-grade'),
      list(plot = plot_reading_age(df_list()$reading), fname = 'reading-vs-age-by-grade'),
      list(plot = plot_rsvp_age(df_list()$rsvp), fname = 'rsvp-reading-vs-age-by-grade'),
      list(plot = get_foveal_acuity_vs_age(df_list()$acuity), fname = 'foveal-acuity-vs-age'),
      list(plot = get_peripheral_acuity_vs_age(df_list()$acuity), fname = 'peripheral-acuity-vs-age'),
      list(plot = plot_acuity_vs_age(df_list()), fname = 'acuity-vs-age'),
      list(plot = plot_crowding_vs_age(df_list()$crowding), fname = 'crowding-vs-age')
    )

    for (call in plot_calls) {
      p <- call$plot
      if (!is.null(p)) {
        p_with_theme <- p + plt_theme
        p_with_footnote <- add_experiment_title(p_with_theme, experiment_names())
      } else {
        p_with_footnote <- p
      }
      res <- append_plot_list(l, fileNames, p_with_footnote, call$fname)
      l <- res$plotList
      fileNames <- res$fileNames
    }

    list(plotList = l, fileNames = fileNames)
    })
  })

  histograms <- reactive({
  if (is.null(files())) {
    return(list(plotList = list(), fileNames = list()))
  }

  app_profile_time(app_profiler, "Plots histogram list", {

  l         <- list()
  fileNames <- list()

  # OPTIMIZATION: Compute expensive functions once, use results multiple times
  acuity_hists <- get_acuity_hist(df_list()$acuity)      # Single function call
  crowding_hists <- get_crowding_hist(df_list()$crowding) # Single function call
  aud_plots <- plot_auditory_crowding(df_list()$quest_all_thresholds, df_list()$crowding)

  static_calls <- list(
    list(plot = aud_plots$hist,                                   fname = 'auditory-crowding-melody-db-histogram'),
    list(plot = acuity_hists[[1]],                                fname = 'foveal-acuity-histogram'),
    list(plot = acuity_hists[[2]],                                fname = 'peripheral-acuity-histogram'),
    list(plot = crowding_hists$foveal,                            fname = 'foveal-crowding-histogram'),
    list(plot = crowding_hists$peripheral,                        fname = 'peripheral-crowding-histogram'),
    list(plot = get_reading_hist(df_list()$rsvp),                 fname = 'rsvp-reading-speed-histogram'),
    list(plot = get_reading_hist(df_list()$reading),              fname = 'reading-speed-histogram'),
    list(plot = get_repeatedLetter_hist(df_list()$repeatedLetters), fname = 'repeated-letter-crowding-histogram'),
    list(plot = get_age_histogram(df_list()$age),                 fname = 'age-histogram'),
    list(plot = get_grade_histogram(df_list()$age),               fname = 'grade-histogram')
    # CQ accuracy histograms are added via reading_CQ_calls below
  )

  reading_CQ_hists <- get_reading_CQ_hist(df_list()$reading_pre, minCQAccuracy())

  # Build calls for CQ hist(s); handle per-condition list or single plot
  if (is.null(reading_CQ_hists)) {
    reading_CQ_calls <- list()
  } else if (is.list(reading_CQ_hists)) {
    reading_CQ_calls <- lapply(names(reading_CQ_hists), function(cond) {
      list(
        plot  = reading_CQ_hists[[cond]],
        fname = paste0('reading-CQ-accuracy-histogram-', cond)
      )
    })
  } else {
    reading_CQ_calls <- list(list(
      plot  = reading_CQ_hists,
      fname = 'reading-CQ-accuracy-histogram'
    ))
  }
  
  prop_hists <- get_prop_correct_hist_list(df_list()$quest_all_thresholds)
  
  prop_calls <- lapply(names(prop_hists), function(cond) {
    list(
      plot  = prop_hists[[cond]],
      fname = paste0('correct-trial-frac-histogram-', cond)
    )
  })


  all_calls <- c(static_calls, prop_calls, reading_CQ_calls)
  for (call in all_calls) {
    p <- add_experiment_title(call$plot, experiment_names())
    res <- append_plot_list(
      l, fileNames,
      p,
      call$fname
    )
    l         <- res$plotList
    fileNames <- res$fileNames
  }

  lists <- append_hist_list(files()$data_list, l, fileNames, experiment_names())

  list(
    plotList  = lists$plotList,
    fileNames = lists$fileNames
  )
  })
  })
  
  #### stacked histograms ####
  stackedPlots <- reactive({
    if (is.null(df_list()) | is.null(files())) {
      return(NULL)
    }

    app_profile_time(app_profiler, "Plots stacked histograms", {
    # Generate the stacked plots
    stacked <- generate_histograms_by_grade(df_list())
    
    # Return all plots, including the new ones
    list(
        rsvp_plot = stacked$rsvp_reading_plot,
        crowding_plot = stacked$peripheral_crowding_plot,
        foveal_acuity_plot = stacked$foveal_acuity_plot,
        foveal_crowding_plot = stacked$foveal_crowding_plot,
        foveal_repeated_plot = stacked$foveal_repeated_plot,
        peripheral_acuity_plot = stacked$peripheral_acuity_plot
      )
    })
  })
  
  #### voilin plots ####
  
  violinPlots <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    app_profile_time(app_profiler, "Plots violin plot list", {
    l <- list()
    fileNames <- list()
    violins <- plot_violins(df_list())
    plot_calls <- list(
      list(plot = violins$reading, fname = 'reading-violin-by-font-plot'),
      list(plot = violins$rsvp, fname = 'rsvp-violin-by-font-plot'),
      list(plot = violins$crowding, fname = 'crowding-violin-by-font-plot'),
      list(plot = violins$acuity, fname = 'acuity-violin-by-font-plot'),
      list(plot = violins$beauty, fname = 'beauty-violin-by-font-plot'),
      list(plot = violins$cmfrt, fname = 'comfort-violin-by-font-plot'),
      list(plot = violins$familiarity, fname = 'familiarity-violin-by-font-plot')
    )
    
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        # Avoid overriding color scale for plots that define their own font colors
        plot <- plot + scale_color_manual(values = colorPalette)
        plot <- add_experiment_title(plot, experiment_names())
      }
      res <- append_plot_list(l, fileNames, plot, call$fname)
      l <- res$plotList
      fileNames <- res$fileNames
    }
    
    list(
      plotList = l,
      fileNames = fileNames
    )
    })
  })
  
  #### fontComparisonPlots ####
  
  fontComparisonPlots <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    app_profile_time(app_profiler, "Plots font comparison list", {
    l <- list()
    fileNames <- list()
    font_comparisons <- plot_font_comparison(df_list(), colorFont())
    plot_calls <- list(
      list(plot = font_comparisons$reading, fname = 'reading-font-comparison-plot'),
      list(plot = font_comparisons$rsvp, fname = 'rsvp-font-comparison-plot'),
      list(plot = font_comparisons$crowding, fname = 'crowding-font-comparison-plot'),
      list(plot = font_comparisons$comfort, fname = 'comfort-font-comparison-plot'),
      list(plot = font_comparisons$beauty, fname = 'beauty-font-comparison-plot'),
      list(plot = font_comparisons$acuity, fname = 'acuity-font-comparison-plot'),
      list(plot = font_comparisons$familiarity, fname = 'familiarity-font-comparison-plot')
    )
    
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        # Don't add color scale for font comparison plots since they use fill, not color
        plot <- add_experiment_title(plot, experiment_names())
      }
      res <- append_plot_list(l, fileNames, plot, call$fname)
      l <- res$plotList
      fileNames <- res$fileNames
    }
    
    list(
      plotList = l,
      fileNames = fileNames
    )
    })
  })
  scatterDiagrams <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    app_profile_time(app_profiler, "Plots scatter diagram list", {
    l <- list()
    fileNames <- list()

    # OPTIMIZATION: Compute expensive functions once, use results multiple times
    foveal_crowding_acuity_plots <- foveal_crowding_vs_acuity_diag()
    peripheral_plots <- peripheral_plot(df_list())
    #crowding_vs_acuity_plots <- crowding_vs_acuity_plot(df_list())
    regression_plots <- regression_reading_plot(df_list(), colorFont())
    test_retest_plots <- get_test_retest(df_list())
  aud_plots <- plot_auditory_crowding(df_list()$quest_all_thresholds, df_list()$crowding)
    
    plot_calls <- list(
      list(plot = aud_plots$scatter, fname = 'auditory-crowding-melody-db-vs-crowding-threshold'),
      list(plot = test_retest_plots$reading, fname = 'retest-test-reading'),
      list(plot = test_retest_plots$pCrowding, fname = 'retest-test-peripheral-crowding'),
      list(plot = test_retest_plots$pAcuity, fname = 'retest-test-peripheral-acuity'),
      list(plot = test_retest_plots$beauty, fname = 'retest-test-beauty'),
      list(plot = test_retest_plots$comfort, fname = 'retest-test-comfort'),
      list(plot = foveal_crowding_acuity_plots$foveal, fname = 'foveal-crowding-vs-foveal-acuity-grade-diagram'),
      list(plot = foveal_crowding_acuity_plots$peripheral, fname = 'foveal-crowding-vs-peripheral-acuity-grade-diagram'),
      list(plot = get_acuity_foveal_peripheral_diag(df_list()$acuity), fname = 'foveal-acuity-vs-peripheral-acuity-grade-diagram'),
      list(plot = foveal_peripheral_diag()$grade, fname = 'foveal-crowding-vs-peripheral-crowding-grade-diagram'),
      list(plot = peripheral_plots$grade, fname = 'peripheral-acuity-vs-peripheral-crowding-grade-diagram'),
      list(plot = peripheral_plots$font, fname = 'peripheral-acuity-vs-peripheral-crowding-font-diagram'),
      list(plot = crowdingPlot(), fname = 'peripheral_crowding_left_vs_right'),
      list(plot = regression_plots$foveal, fname = 'reading-rsvp-reading-vs-foveal-crowding'),
      list(plot = regression_plots$peripheral, fname = 'reading-rsvp-reading-vs-peripheral-crowding'),
      list(plot = regression_acuity_plot(df_list()), fname = 'ordinary-reading-rsvp-reading-vs-acuity'),
      list(plot = plot_reading_rsvp(df_list()$reading, df_list()$rsvp), fname = 'reading-vs-RSVP-reading-plot'),
      list(plot = get_crowding_vs_repeatedLetter(df_list()$crowding, df_list()$repeatedLetters)$grade, fname = 'crowding-vs-repeated-letters-crowding-grade'),
      list(plot = plot_badLatenessTrials_vs_memory(files()$data_list,conditionNames()), fname="badLatenessTrials-vs-deviceMemoryGB-by-participant"),
      list(plot = minDegPlots()$scatter, fname="foveal-crowding-vs-spacingMinDeg")
    )

    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        plot <- plot + scale_color_manual(values = colorPalette)
        plot <- add_experiment_title(plot, experiment_names())
        res <- append_plot_list(l, fileNames, plot, call$fname)
        l <- res$plotList
        fileNames <- res$fileNames
      }
    }
    
    comfort_beauty_plots <- list(
      list(plot = comfort_vs_crowding_scatter(df_list(), colorFont()), fname = 'comfort-vs-crowding-scatter'),
      list(plot = beauty_vs_crowding_scatter(df_list(), colorFont()), fname = 'beauty-vs-crowding-scatter'),
      list(plot = beauty_vs_comfort_scatter(df_list(), colorFont()), fname = 'beauty-vs-comfort-scatter'),
      list(plot = familiarity_vs_crowding_scatter(df_list(), colorFont()), fname = 'familiarity-vs-crowding-scatter')
    )
    
    for (call in comfort_beauty_plots) {
      if (!is.null(call$plot)) {
        plot <- add_experiment_title(call$plot, experiment_names())
        res <- append_plot_list(l, fileNames, plot, call$fname)
        l <- res$plotList
        fileNames <- res$fileNames
      }
    }

    list(
      plotList = l,
      fileNames = fileNames
    )
    })
  })
  # Progressive rendering for Plots tab renderImage slots (p*, hist*, scatter*).
  plotsRenderCount <- reactiveVal(0)
  histRenderCount <- reactiveVal(0)
  scatterRenderCount <- reactiveVal(0)

  observeEvent(agePlots(), { plotsRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(histograms(), { histRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(scatterDiagrams(), { scatterRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(files(), {
    plotsRenderCount(0)
    histRenderCount(0)
    scatterRenderCount(0)
  }, ignoreInit = TRUE)

  observe({
    total <- length(agePlots()$plotList)
    current <- plotsRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      plotsRenderCount(current + 1)
    }
  })

  observe({
    total <- length(histograms()$plotList)
    current <- histRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      histRenderCount(current + 1)
    }
  })

  observe({
    total <- length(scatterDiagrams()$plotList)
    current <- scatterRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      scatterRenderCount(current + 1)
    }
  })
  gradePlots <- reactive({
    if (is.null(files()) | is.null(df_list())) {
      return(histograms <NULL)
    }
    plot_rsvp_crowding_acuity(df_list())
  })

  output$isRsvp <- reactive({
    if ('rsvp' %in% names(df_list())) {
      return(nrow(df_list()$rsvp) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isRepeated <- reactive({
    if ('repeatedLetters' %in% names(df_list())) {
      return(nrow(df_list()$repeatedLetters) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isReading <- reactive({
    if ('reading' %in% names(df_list())) {
      return(nrow(df_list()$reading) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isCrowding <- reactive({
    if ('crowding' %in% names(df_list())) {
      return(nrow(df_list()$crowding) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isGrade <- reactive({
    if ('quest' %in% names(df_list())) {
      return(n_distinct(df_list()$quest$Grade) > 1)
    } else {
      return(FALSE)
    }
  })
  
  output$isFovealCrowding <- reactive({
    if ('crowding' %in% names(df_list())) {
      return(nrow(
        df_list()$crowding %>% filter(targetEccentricityXDeg == 0)
      ) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isPeripheralCrowding <- reactive({
    if ('crowding' %in% names(df_list())) {
      return(nrow(
        df_list()$crowding %>% filter(targetEccentricityXDeg != 0)
      ) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isAcuity <- reactive({
    if ('acuity' %in% names(df_list())) {
      return(nrow(df_list()$acuity) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isFovealAcuity <- reactive({
    if ('acuity' %in% names(df_list())) {
      return(nrow(df_list()$acuity %>%
                    filter(targetEccentricityXDeg == 0)) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isPeripheralAcuity <- reactive({
    if ('acuity' %in% names(df_list())) {
      peripheral <-
        df_list()$acuity %>% filter(targetEccentricityXDeg != 0)
      return(nrow(peripheral) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isCorrMatrixAvailable <- reactive({
    return(!is.null(corrMatrix()))
  })
  outputOptions(output, 'isCorrMatrixAvailable', suspendWhenHidden = FALSE)
  
  output$fileUploaded <- reactive({
    return(nrow(files()$pretest > 0))
  })
  
  output$questData <- reactive({
    if ('quest' %in% names(df_list())) {
      return(nrow(df_list()$quest > 0))
    }
    return(FALSE)
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)
  outputOptions(output, 'questData', suspendWhenHidden = FALSE)
  outputOptions(output, 'isGrade', suspendWhenHidden = FALSE)
  outputOptions(output, 'isPeripheralAcuity', suspendWhenHidden = FALSE)
  outputOptions(output, 'isReading', suspendWhenHidden = FALSE)
  outputOptions(output, 'isRsvp', suspendWhenHidden = FALSE)
  outputOptions(output, 'isRepeated', suspendWhenHidden = FALSE)
  outputOptions(output, 'isCrowding', suspendWhenHidden = FALSE)
  outputOptions(output, 'isFovealCrowding', suspendWhenHidden = FALSE)
  outputOptions(output, 'isPeripheralCrowding', suspendWhenHidden = FALSE)
  outputOptions(output, 'isAcuity', suspendWhenHidden = FALSE)
  outputOptions(output, 'isFovealAcuity', suspendWhenHidden = FALSE)
  
  #### color font ####
  colorFont <- reactive({
    app_profile_time(app_profiler, "Plots color font", {
    # Collect fonts from all relevant datasets and return a tibble(font, color)
    fonts <- unique(na.omit(c(
      if ('quest'        %in% names(df_list())) df_list()$quest$font else NULL,
      if ('reading'      %in% names(df_list())) df_list()$reading$font else NULL,
      if ('comfort'      %in% names(df_list())) df_list()$comfort$font else NULL,
      if ('beauty'       %in% names(df_list())) df_list()$beauty$font else NULL,
      if ('familiarity'  %in% names(df_list())) df_list()$familiarity$font else NULL
    )))
    fonts <- fonts[fonts != ""]
    if (length(fonts) == 0) {
      return(tibble(font = character(), color = character()))
    }
    # Assign colors deterministically by sorted font order
    fonts <- sort(unique(fonts))
    cols <- rep(colorPalette, length.out = length(fonts))
    tibble(font = fonts, color = cols)
    })
  })

  #### plots ####

  output$corrMatrixPlot <- renderImage({
    if (is.null(corrMatrix())) {
      return(NULL)
    }

    app_profile_time(app_profiler, "Plots correlation matrix image", {
    tryCatch({
      p <- add_experiment_title(corrMatrix()$plot, experiment_names())
      render_plots_display_png(
        p,
        width_in = corrMatrix()$width,
        height_in = corrMatrix()$height,
        disp_w = 700
      )
    }, error = function(e) {
      handle_plot_error(e, "corrMatrixPlot", experiment_names(), "Correlation Matrix Plot")
    })
    })
  }, deleteFile = TRUE)
  
  output$nMatrixPlot <- renderImage({
    if (is.null(corrMatrix())) {
      return(NULL)
    }

    app_profile_time(app_profiler, "Plots N matrix image", {
    tryCatch({
      p <- add_experiment_title(corrMatrix()$n_plot, experiment_names())
      render_plots_display_png(
        p,
        width_in = corrMatrix()$width,
        height_in = corrMatrix()$height,
        disp_w = 700
      )
    }, error = function(e) {
      handle_plot_error(e, "nMatrixPlot", experiment_names(), "N Matrix Plot")
    })
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedReadingRsvpCrowdingPlot <- renderImage({
    app_profile_time(app_profiler, "Plots font-aggregated reading RSVP crowding image", {
    tryCatch({
      plot <- fontAggregatedReadingRsvpCrowding()
      if (is.null(plot)) {
        plot <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data", hjust = 0.5, vjust = 0.5) +
          theme_void()
      } else {
        plot <- add_experiment_title(plot, experiment_names()) + plt_theme
      }
      render_plots_display_png(plot, width_in = 8, height_in = 6, disp_w = 700, limitsize = FALSE)
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedReadingRsvpCrowdingPlot", experiment_names(), "Font-aggregated reading vs peripheral crowding")
    })
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedOrdinaryReadingCrowdingPlot <- renderImage({
    app_profile_time(app_profiler, "Plots font-aggregated ordinary reading crowding image", {
    tryCatch({
      plot <- fontAggregatedOrdinaryReadingCrowding()
      if (is.null(plot)) {
        plot <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data", hjust = 0.5, vjust = 0.5) +
          theme_void()
      } else {
        plot <- add_experiment_title(plot, experiment_names()) + plt_theme
      }
      render_plots_display_png(plot, width_in = 8, height_in = 6, disp_w = 700, limitsize = FALSE)
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedOrdinaryReadingCrowdingPlot", experiment_names(), "Font-aggregated ordinary reading vs peripheral crowding")
    })
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedRsvpCrowdingPlot <- renderImage({
    app_profile_time(app_profiler, "Plots font-aggregated RSVP crowding image", {
    tryCatch({
      plot <- fontAggregatedRsvpCrowding()
      if (is.null(plot)) {
        plot <- ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data", hjust = 0.5, vjust = 0.5) +
          theme_void()
      } else {
        plot <- add_experiment_title(plot, experiment_names()) + plt_theme
      }
      render_plots_display_png(plot, width_in = 8, height_in = 6, disp_w = 700, limitsize = FALSE)
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedRsvpCrowdingPlot", experiment_names(), "Font-aggregated RSVP vs peripheral crowding")
    })
    })
  }, deleteFile = TRUE)
  
  #### age plots ####
  output$plots <- renderUI({
    plotList <- agePlots()$plotList
    fileNames <- agePlots()$fileNames
    n <- length(plotList)
    out <- list()
    if (n == 0) {
      return(out)
    }
    
    for (i in seq(from = 1, to = n, by = 2)) {
      if (i + 1 <= n) {
        out[[length(out) + 1]] <- splitLayout(
          cellWidths = c("50%", "50%"),
          withSpinner(plotOutput(paste0("p", i), width = "100%", height = "100%"), type = 4),
          withSpinner(plotOutput(paste0("p", i + 1), width = "100%", height = "100%"), type = 4)
        )
        out[[length(out) + 1]] <- splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton(paste0("downloadP", i), 'Download'),
          downloadButton(paste0("downloadP", i + 1), 'Download')
        )
      } else {
        out[[length(out) + 1]] <- splitLayout(
          cellWidths = c("50%", "50%"),
          withSpinner(plotOutput(paste0("p", i), width = "100%", height = "100%"), type = 4),
          div()
        )
        out[[length(out) + 1]] <- splitLayout(
          cellWidths = c("50%", "50%"),
          downloadButton(paste0("downloadP", i), 'Download'),
          div()
        )
      }
    }
    
    for (j in 1:length( plotList)) {
      local({
        i <- j
        output[[paste0("p", i)]] <-
          renderImage({
          req(i <= plotsRenderCount())
          app_profile_time(app_profiler, paste0("Plots age image ", i), {
          tryCatch({
            plot_to_save <- if (is_placeholder_plot(plotList[[i]])) plotList[[i]] else plotList[[i]] + plt_theme
            render_plots_display_png(plot_to_save, width_in = 6, height_in = 6, disp_w = 700, limitsize = FALSE)
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
              labs(subtitle=fileNames[[i]])
            render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
          })
          })
        
      }, deleteFile = TRUE)

        
        output[[paste0("downloadP", i)]] <- downloadHandler(
          filename = function() {
            idx <- i
            base <- if (!is.null(fileNames) && length(fileNames) >= idx && !is.null(fileNames[[idx]])) {
              fileNames[[idx]]
            } else {
              paste0("plot-", idx)
            }
            paste0(get_short_experiment_name(experiment_names()), base, ".", downloadFileType())
          },
          content = function(file) {
            # Skip download if plot is NULL/NA/placeholder
            if (i > length(plotList) || is_placeholder_plot(plotList[[i]])) return(invisible(NULL))
            plot <- plotList[[i]] + plt_theme
            savePlot(
              plot = plot,
              filename = file,
              fileType = downloadFileType(),
              width = 6,
              height = 4
            )
          }
        )
      })
    }
    
    return(out)
  })
  
  output$histograms <- renderUI({
    out    <- list()
    plots  <- histograms()$plotList
    files  <- histograms()$fileNames
    n      <- length(plots)
    nPerRow <- 6
    
    if (n == 0) {
      return(out)
    }
    
    for (i in seq(1, n, by = nPerRow)) {
      idx <- i:min(i + nPerRow - 1, n)
      
      # --- row of plots ---
      plot_cells <- lapply(idx, function(j) {
        shinycssloaders::withSpinner(
          plotOutput(paste0("hist", j),
                     width  = "100%",
                     height = "100%"),
          type = 4
        )
      })
      # pad out any missing cells so splitLayout stays stable
      if (length(plot_cells) < nPerRow)
        plot_cells <- c(plot_cells, rep("", nPerRow - length(plot_cells)))
      
      out[[length(out) + 1]] <- do.call(splitLayout, c(
        list(
          cellWidths = rep("16.66%", nPerRow),
          style      = "overflow-x: hidden; white-space: nowrap;"
        ),
        plot_cells
      ))
      
      # --- row of download buttons ---
      dl_cells <- lapply(idx, function(j) {
        downloadButton(paste0("downloadHist", j), "Download")
      })
      if (length(dl_cells) < nPerRow)
        dl_cells <- c(dl_cells, rep("", nPerRow - length(dl_cells)))
      
      out[[length(out) + 1]] <- do.call(splitLayout, c(
        list(
          cellWidths = rep("16.66%", nPerRow),
          style      = "overflow-x: hidden; white-space: nowrap;"
        ),
        dl_cells
      ))
    }
    
    # register each renderImage & downloadHandler (unchanged)
    for (j in seq_along(plots)) {
      local({
        jj <- j
       
      output[[paste0("hist", jj)]] <- renderImage({
        req(jj <= histRenderCount())
        app_profile_time(app_profiler, paste0("Plots histogram image ", jj), {
        plot_to_save <- with_plots_histogram_theme(plots[[jj]])
        disp_w <- session$clientData[[paste0("output_", "hist", jj, "_width")]]
        if (is.null(disp_w) || is.na(disp_w) || disp_w <= 0) disp_w <- 380
        render_plots_display_png(
          plot_to_save,
          width_in = 3.5,
          height_in = 3.5,
          disp_w = disp_w,
          disp_h = disp_w,
          png_theme_profile = "histogram",
          limitsize = FALSE
        )
          # tryCatch({
          #   outfile <- tempfile(fileext = '.svg')
          #   ggsave(
          #     file = outfile,
          #     plot =  plots[[jj]],
          #     device = svglite,
          #     width = 2.5,
          #     height = 2.5,
          #     unit = 'in'
          #   )
          #   list(src = outfile, contenttype = 'svg')
          # }, error = function(e) {
          #   error_plot <- ggplot() +
          #     annotate(
          #       "text",
          #       x = 0.5,
          #       y = 0.5,
          #       label = paste("Error:", e$message),
          #       color = "red",
          #       size = 5,
          #       hjust = 0.5,
          #       vjust = 0.5
          #     ) +
          #     theme_void() +
          #     labs(subtitle=files[[jj]])
          #   
          #   # Save the error plot to a temp file
          #   outfile <- tempfile(fileext = '.svg')
          #   ggsave(
          #     file = outfile,
          #     plot = error_plot,
          #     device = svglite,
          #     width = 2.5,
          #     height = 2.5,
          #     unit = 'in'
          #   )
          #   list(
          #     src = outfile,
          #     contenttype = 'svg',
          #     alt = paste0("Error in ", files[[jj]])
          #   )
          # })
        })
        }, deleteFile = TRUE)
        outputOptions(output, paste0("hist", jj), suspendWhenHidden = TRUE)
        
        output[[paste0("downloadHist", jj)]] <- downloadHandler(
          filename = function() paste0(
            get_short_experiment_name(experiment_names()),
            files[[jj]],
            ".", downloadFileType()
          ),
          content = function(file) {
            # Skip download if plot is NULL/NA/placeholder
            if (is_placeholder_plot(plots[[jj]])) return(invisible(NULL))

            save_plots_histogram(
              file = file,
              plot = plots[[jj]],
              file_type = downloadFileType()
            )
          }
        )
      })
    }
    
    return(out)
  })
  
  observeEvent(stackedPlots(), {
    # RSVP
    output$stackedRsvpPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked RSVP image", {
      base_plot <- stackedPlots()$rsvp_plot +
        plt_theme +
        theme(
          axis.text.x = element_text(),
          axis.ticks.x = element_line(),
          plot.title = element_text(size = 14, margin = margin(b = 1)),
          plot.margin = margin(
            t = 2,
            r = 5,
            b = 2,
            l = 5
          )
        ) +
        theme(
          legend.position = "top",
          legend.key.size = unit(2, "mm"),
          legend.title = element_text(size = 8),
          legend.text = element_text(size = 8),
          axis.text = element_text(size = 11),
          plot.title = element_text(size = 12, margin = margin(b = 2)),
          plot.margin = margin(5, 5, 5, 5, "pt")
        )
      p <- add_experiment_title(base_plot, experiment_names())
      render_plots_display_png(p, width_in = 6, height_in = 8, disp_w = 600)
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedRsvpPlot <- downloadHandler(
      filename = function() {
        paste0(
          get_short_experiment_name(experiment_names()),
          "histogram-of-rsvp-reading-stacked-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          base_plot <- stackedPlots()$rsvp_plot +
            plt_theme +
            theme(
              axis.text.x = element_text(),
              axis.ticks.x = element_line(),
              plot.title = element_text(size = 14, margin = margin(b = 1)),
              plot.margin = margin(
                t = 2,
                r = 5,
                b = 2,
                l = 5
              )
            ) +
            theme(
              legend.position = "top",
              legend.key.size = unit(2, "mm"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 8),
              axis.text = element_text(size = 11),
              plot.title = element_text(size = 12, margin = margin(b = 2)),
              plot.margin = margin(5, 5, 5, 5, "pt")
            )
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = tmp_svg,
            plot = plot_with_title,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          base_plot <- stackedPlots()$rsvp_plot +
            plt_theme +
            theme(
              axis.text.x = element_text(),
              axis.ticks.x = element_line(),
              plot.title = element_text(size = 14, margin = margin(b = 1)),
              plot.margin = margin(
                t = 2,
                r = 5,
                b = 2,
                l = 5
              )
            ) +
            theme(
              legend.position = "top",
              legend.key.size = unit(2, "mm"),
              legend.title = element_text(size = 8),
              legend.text = element_text(size = 8),
              axis.text = element_text(size = 11),
              plot.title = element_text(size = 12, margin = margin(b = 2)),
              plot.margin = margin(5, 5, 5, 5, "pt")
            )
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = file,
            plot = plot_with_title,
            device = ifelse(
              downloadFileType() == "svg",
              svglite::svglite,
              downloadFileType()
            ),
            width = 6,
            height = 8,
            unit = "in",
            limitsize = FALSE
          )
        }
      }
    )
    
    # Crowding
    output$stackedCrowdingPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked crowding image", {
      render_plots_display_png(
        stackedPlots()$crowding_plot + plt_theme + stacked_theme,
        width_in = 6,
        height_in = 8,
        disp_w = 600
      )
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedCrowdingPlot <- downloadHandler(
      filename = function() {
        paste0(
          get_short_experiment_name(experiment_names()),
          "histogram-of-peripheral-crowding-stacked-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          base_plot <- stackedPlots()$crowding_plot +
            plt_theme +
            stacked_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = tmp_svg,
            plot = plot_with_title,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          base_plot <- stackedPlots()$crowding_plot +
            plt_theme + stacked_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = file,
            plot = plot_with_title,
            device = ifelse(
              downloadFileType() == "svg",
              svglite::svglite,
              downloadFileType()
            ),
            width = 6,
            height = 8,
            unit = "in",
            limitsize = FALSE
          )
        }
      }
    )
    
    # Foveal Acuity
    output$stackedFovealAcuityPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked foveal acuity image", {
      render_plots_display_png(
        stackedPlots()$foveal_acuity_plot + plt_theme + stacked_theme,
        width_in = 6,
        height_in = 8,
        disp_w = 600
      )
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedFovealAcuityPlot <- downloadHandler(
      filename = function() {
        paste0(
          get_short_experiment_name(experiment_names()),
          "histogram-of-foveal-acuity-stacked-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          base_plot <- stackedPlots()$foveal_acuity_plot +
            plt_theme + stacked_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = tmp_svg,
            plot = plot_with_title,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          base_plot <- stackedPlots()$foveal_acuity_plot +
            plt_theme + stacked_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            filename = file,
            plot = plot_with_title,
            device = ifelse(
              downloadFileType() == "svg",
              svglite::svglite,
              downloadFileType()
            ),
            width = 8,
            height = 6,
            unit = "in",
            limitsize = FALSE
          )
        }
      }
    )
    
    # Foveal Crowding
    output$stackedFovealCrowdingPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked foveal crowding image", {
      render_plots_display_png(
        stackedPlots()$foveal_crowding_plot + plt_theme + stacked_theme,
        width_in = 6,
        height_in = 8,
        disp_w = 600
      )
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedFovealCrowdingPlot <- downloadHandler(
      filename = function() {
        paste0(
          get_short_experiment_name(experiment_names()),
          "histogram-of-foveal-crowding-stacked-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_crowding_plot +
              plt_theme + stacked_theme,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          ggsave(
            filename = file,
            plot = stackedPlots()$foveal_crowding_plot +
              plt_theme + stacked_theme,
            device = ifelse(
              downloadFileType() == "svg",
              svglite::svglite,
              downloadFileType()
            ),
            width = 6,
            height = 8,
            unit = "in",
            limitsize = FALSE
          )
        }
      }
    )
    
    # Foveal Repeated
    output$stackedFovealRepeatedPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked foveal repeated image", {
      render_plots_display_png(
        stackedPlots()$foveal_repeated_plot + plt_theme + stacked_theme,
        width_in = 6,
        height_in = 8,
        disp_w = 600
      )
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedFovealRepeatedPlot <- downloadHandler(
      filename = function() {
        paste0(
          get_short_experiment_name(experiment_names()),
          "histogram-of-foveal-repeated-letter-crowding-stacked-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_repeated_plot +
              plt_theme + stacked_theme,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          ggsave(
            filename = file,
            plot = stackedPlots()$foveal_repeated_plot +
              plt_theme + stacked_theme,
            device = ifelse(
              downloadFileType() == "svg",
              svglite::svglite,
              downloadFileType()
            ),
            width = 6,
            height = 8,
            unit = "in",
            limitsize = FALSE
          )
        }
      }
    )
    
    # Peripheral Acuity
    output$stackedPeripheralAcuityPlot <- renderImage({
      app_profile_time(app_profiler, "Plots stacked peripheral acuity image", {
      render_plots_display_png(
        stackedPlots()$peripheral_acuity_plot + plt_theme + stacked_theme,
        width_in = 6,
        height_in = 8,
        disp_w = 600
      )
      })
    }, deleteFile = TRUE)
    
    output$downloadStackedPeripheralAcuityPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "histogram-of-peripheral-acuity-stacked-by-grade.",
            downloadFileType()
          )
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            tmp_svg <- tempfile(fileext = ".svg")
            ggsave(
              filename = tmp_svg,
              plot = stackedPlots()$peripheral_acuity_plot +
                plt_theme + stacked_theme,
              device = svglite,
              width = 6,
              height = 8,
              unit = "in"
            )
            rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
          } else {
            ggsave(
              filename = file,
              plot = stackedPlots()$peripheral_acuity_plot +
                plt_theme + stacked_theme,
              device = ifelse(
                downloadFileType() == "svg",
                svglite::svglite,
                downloadFileType()
              ),
              width = 6,
              height = 8,
              unit = "in",
              limitsize = FALSE
            )
          }
        }
      )
  })
  
  output$scatters <- renderUI({
    out <- list()
    i = 1
    while (i <= length(scatterDiagrams()$plotList) - 1) {
      out[[i]] <-  splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("scatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("scatter", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadScatter", i), 'Download'),
        downloadButton(paste0("downloadScatter", i +
                                1), 'Download')
      )
      i = i + 2
    }
    if (i == length(scatterDiagrams()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("scatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatter", i), 'Download'))
    }
    for (j in 1:length(scatterDiagrams()$plotList)) {
      local({
        ii <- j
        output[[paste0("scatter", ii)]] <- renderImage({
          req(ii <= scatterRenderCount())

          app_profile_time(app_profiler, paste0("Plots scatter image ", ii), {
          tryCatch({
            plot_to_save <- if (is_placeholder_plot(scatterDiagrams()$plotList[[ii]])) {
              scatterDiagrams()$plotList[[ii]]
            } else {
              scatterDiagrams()$plotList[[ii]] + plt_theme_scatter
            }
            render_plots_display_png(plot_to_save, width_in = 7, height_in = 7, disp_w = 700, limitsize = FALSE)
          }, error = function(e) {
          handle_plot_error(e, paste0("scatter", ii), experiment_names(), scatterDiagrams()$fileNames[[ii]])
          })
          })
          
        }, deleteFile = TRUE)
        outputOptions(output, paste0("scatter", ii), suspendWhenHidden = TRUE)
        output[[paste0("downloadScatter", ii)]] <-
          downloadHandler(
            filename = function() paste0(
              get_short_experiment_name(experiment_names()),
              scatterDiagrams()$fileNames[[ii]],
              '.',
              downloadFileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(scatterDiagrams()$plotList[[ii]])) return(invisible(NULL))
              
              if (downloadFileType() == "png") {
                ggsave(
                  filename = file,
                  plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
                  device = ragg::agg_png,
                  width = 7,
                  height = 7,
                  units = "in",
                  dpi = 200,
                  limitsize = FALSE
                )
              } else {
                ggsave(
                  file,
                  plot = scatterDiagrams()$plotList[[ii]] +
                    plt_theme_scatter,
                  width = 7,
                  height = 7,
                  units = "in",
                  limitsize = F,
                  device = ifelse(
                    downloadFileType() == "svg",
                    svglite::svglite,
                    downloadFileType()
                  )
                )
              }
            }
          )
      })
    }
    return(out)
  })
  
  output$violinPlots <- renderUI({
    out <- list()
    i = 1
    while (i <= length(violinPlots()$plotList) - 1) {
      out[[i]] <-  splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("violin", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("violin", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadViolin", i), 'Download'),
        downloadButton(paste0("downloadViolin", i +
                                1), 'Download')
      )
      i = i + 2
    }
    if (i == length(violinPlots()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("violin", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadViolin", i), 'Download'))
    }
    
    # Generate the plots and download handlers
    for (j in 1:length(violinPlots()$plotList)) {
      local({
        ii <- j
        output[[paste0("violin", ii)]] <- renderImage({
          app_profile_time(app_profiler, paste0("Plots violin image ", ii), {
          tryCatch({
            render_plots_display_png(
              violinPlots()$plotList[[ii]] + plt_theme,
              width_in = 8,
              height_in = 6,
              disp_w = 700,
              limitsize = FALSE
            )
          }, error = function(e) {
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
              labs(subtitle=violinPlots()$fileNames[[ii]])
            render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
          })
          })
        }, deleteFile = TRUE)
        
        output[[paste0("downloadViolin", ii)]] <-
          downloadHandler(
            filename = function() paste0(
              get_short_experiment_name(experiment_names()),
              violinPlots()$fileNames[[ii]],
              '.',
              downloadFileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(violinPlots()$plotList[[ii]])) return(invisible(NULL))
              
              if (downloadFileType() == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = violinPlots()$plotList[[ii]] +
                    plt_theme,
                  width = 8,
                  height = 6,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg,
                               file,
                               width = 1800,
                               height = 1350)
              } else {
                ggsave(
                  file,
                  plot = violinPlots()$plotList[[ii]] +
                    plt_theme,
                  width = 8,
                  height = 6,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    downloadFileType() == "svg",
                    svglite::svglite,
                    downloadFileType()
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  output$fontComparisonPlots <- renderUI({
    out <- list()
    i = 1
    while (i <= length(fontComparisonPlots()$plotList) - 1) {
      out[[i]] <-  splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("fontComparison", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("fontComparison", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadFontComparison", i), 'Download'),
        downloadButton(paste0("downloadFontComparison", i + 1), 'Download')
      )
      i = i + 2
    }
    if (i == length(fontComparisonPlots()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("fontComparison", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadFontComparison", i), 'Download'))
    }
    
    # Generate the plots and download handlers
    for (j in 1:length(fontComparisonPlots()$plotList)) {
      local({
        ii <- j
        output[[paste0("fontComparison", ii)]] <- renderImage({
          app_profile_time(app_profiler, paste0("Plots font comparison image ", ii), {
          tryCatch({
            render_plots_display_png(
              fontComparisonPlots()$plotList[[ii]] + plt_theme,
              width_in = 8,
              height_in = 6,
              disp_w = 700,
              limitsize = FALSE
            )
          }, error = function(e) {
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
              labs(subtitle=fontComparisonPlots()$fileNames[[ii]])
            render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
          })
          })
        }, deleteFile = TRUE)
        
        output[[paste0("downloadFontComparison", ii)]] <-
          downloadHandler(
            filename = function() paste0(
              get_short_experiment_name(experiment_names()),
              fontComparisonPlots()$fileNames[[ii]],
              '.',
              downloadFileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(fontComparisonPlots()$plotList[[ii]])) return(invisible(NULL))
              
              if (downloadFileType() == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = fontComparisonPlots()$plotList[[ii]] +
                    plt_theme,
                  width = 8,
                  height = 6,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg,
                               file,
                               width = 1800,
                               height = 1350)
              } else {
                ggsave(
                  file,
                  plot = fontComparisonPlots()$plotList[[ii]] +
                    plt_theme,
                  width = 8,
                  height = 6,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    downloadFileType() == "svg",
                    svglite::svglite,
                    downloadFileType()
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })

  list(
    agePlots = agePlots,
    histograms = histograms,
    scatterDiagrams = scatterDiagrams,
    violinPlots = violinPlots,
    fontComparisonPlots = fontComparisonPlots
  )
}
