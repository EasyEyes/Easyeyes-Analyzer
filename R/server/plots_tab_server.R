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
                                      app_profiler = NULL,
                                      maxPlotsHistSlots = 36,
                                      maxPlotsAgeSlots = 12,
                                      maxPlotsScatterSlots = 30,
                                      maxPlotsViolinSlots = 10,
                                      maxPlotsFontComparisonSlots = 10) {

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
      list(plot = plot_crowding_vs_duration(df_list()$crowding), fname = 'crowding-vs-duration'),
      list(plot = plot_crowding_vs_duration_by_side(df_list()$crowding), fname = 'crowding-vs-duration-by-side'),
      list(plot = plot_crowding_vs_duration_by_participant(df_list()$crowding), fname = 'crowding-vs-duration-by-participant'),
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
  # Progressive rendering follows Plots tab page order:
  # histograms → violin → font comparison → scatter → age / RSVP later sections.
  plotsRenderCount <- reactiveVal(0)
  histRenderCount <- reactiveVal(0)
  histRenderedCount <- reactiveVal(0)
  violinRenderCount <- reactiveVal(0)
  violinRenderedCount <- reactiveVal(0)
  fontComparisonRenderCount <- reactiveVal(0)
  fontComparisonRenderedCount <- reactiveVal(0)
  scatterRenderCount <- reactiveVal(0)
  scatterRenderedCount <- reactiveVal(0)

  reset_downstream_render_counts <- function() {
    violinRenderCount(0)
    violinRenderedCount(0)
    fontComparisonRenderCount(0)
    fontComparisonRenderedCount(0)
    scatterRenderCount(0)
    scatterRenderedCount(0)
    plotsRenderCount(0)
  }

  # Reset progressive gates only on new uploads (Distance-tab pattern).
  # Do NOT reset on histograms()/df_list() invalidation — filter debounce and
  # plot-list rebuilds would restart hist rendering forever mid-flight.
  observeEvent(files(), {
    histRenderCount(0)
    histRenderedCount(0)
    reset_downstream_render_counts()
  }, ignoreInit = TRUE)

  observe({
    total <- min(length(histograms()$plotList), maxPlotsHistSlots)
    current <- histRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      histRenderCount(current + 1)
    }
  })

  histImagesReady <- reactive({
    total <- min(length(histograms()$plotList), maxPlotsHistSlots)
    is.null(total) || total <= 0 || histRenderedCount() >= total
  })

  observeEvent(histImagesReady(), {
    if (!isTRUE(histImagesReady())) return(invisible(NULL))
    violinRenderCount(0)
    violinRenderedCount(0)
  }, ignoreInit = TRUE)

  observe({
    req(histImagesReady())
    total <- min(length(violinPlots()$plotList), maxPlotsViolinSlots)
    current <- violinRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      violinRenderCount(current + 1)
    }
  })

  violinImagesReady <- reactive({
    if (!isTRUE(histImagesReady())) return(FALSE)
    total <- min(length(violinPlots()$plotList), maxPlotsViolinSlots)
    is.null(total) || total <= 0 || violinRenderedCount() >= total
  })

  observeEvent(violinImagesReady(), {
    if (!isTRUE(violinImagesReady())) return(invisible(NULL))
    fontComparisonRenderCount(0)
    fontComparisonRenderedCount(0)
  }, ignoreInit = TRUE)

  observe({
    req(violinImagesReady())
    total <- min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
    current <- fontComparisonRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      fontComparisonRenderCount(current + 1)
    }
  })

  fontComparisonImagesReady <- reactive({
    if (!isTRUE(violinImagesReady())) return(FALSE)
    total <- min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
    is.null(total) || total <= 0 || fontComparisonRenderedCount() >= total
  })

  observeEvent(fontComparisonImagesReady(), {
    if (!isTRUE(fontComparisonImagesReady())) return(invisible(NULL))
    scatterRenderCount(0)
    scatterRenderedCount(0)
  }, ignoreInit = TRUE)

  observe({
    req(fontComparisonImagesReady())
    total <- min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
    current <- scatterRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      scatterRenderCount(current + 1)
    }
  })

  scatterImagesReady <- reactive({
    if (!isTRUE(fontComparisonImagesReady())) return(FALSE)
    total <- min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
    is.null(total) || total <= 0 || scatterRenderedCount() >= total
  })

  # RSVP / ordinary / age sections sit below scatters on the page.
  laterSectionsReady <- reactive({
    isTRUE(scatterImagesReady())
  })

  observeEvent(scatterImagesReady(), {
    if (!isTRUE(scatterImagesReady())) return(invisible(NULL))
    plotsRenderCount(0)
  }, ignoreInit = TRUE)

  observe({
    req(scatterImagesReady())
    total <- min(length(agePlots()$plotList), maxPlotsAgeSlots)
    current <- plotsRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      plotsRenderCount(current + 1)
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
    req(laterSectionsReady())
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
    req(laterSectionsReady())
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
    req(laterSectionsReady())
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
  
  #### fixed histogram slots ####
  for (i in seq_len(maxPlotsHistSlots)) {
    local({
      ii <- i

      output[[paste0("hasHist", ii)]] <- reactive({
        # Keep placeholder "x name" plots visible (previous renderUI behavior).
        length(histograms()$plotList) >= ii
      })
      outputOptions(output, paste0("hasHist", ii), suspendWhenHidden = FALSE)

      output[[paste0("histTitle", ii)]] <- renderText({
        req(length(histograms()$fileNames) >= ii)
        histograms()$fileNames[[ii]]
      })

      output[[paste0("hist", ii)]] <- renderImage({
        req(ii <= histRenderCount())
        req(length(histograms()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Plots histogram image ", ii), {
          # Fixed display width: clientData widths reflow in the 6-column grid
          # as each hist appears, re-invalidating every prior hist renderImage
          # and looking like an infinite generation loop.
          disp_w <- 280
          tryCatch({
            plot_to_save <- with_plots_histogram_theme(histograms()$plotList[[ii]])
            result <- render_plots_display_png(
              plot_to_save,
              width_in = 3.5,
              height_in = 3.5,
              disp_w = disp_w,
              disp_h = disp_w,
              png_theme_profile = "histogram",
              limitsize = FALSE
            )
            if (isolate(histRenderedCount()) < ii) histRenderedCount(ii)
            result
          }, error = function(e) {
            if (isolate(histRenderedCount()) < ii) histRenderedCount(ii)
            error_plot <- ggplot() +
              annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = paste("Error:", e$message),
                color = "red",
                size = 4,
                hjust = 0.5,
                vjust = 0.5
              ) +
              theme_void()
            render_plots_display_png(
              error_plot,
              width_in = 3.5,
              height_in = 3.5,
              disp_w = disp_w,
              disp_h = disp_w,
              use_png_theme = FALSE,
              limitsize = FALSE
            )
          })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("hist", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadHist", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          histograms()$fileNames[[ii]],
          ".", downloadFileType()
        ),
        content = function(file) {
          req(length(histograms()$plotList) >= ii)
          if (is_placeholder_plot(histograms()$plotList[[ii]])) return(invisible(NULL))
          save_plots_histogram(
            file = file,
            plot = histograms()$plotList[[ii]],
            file_type = downloadFileType()
          )
        }
      )
    })
  }

  #### fixed age plot slots ####
  for (i in seq_len(maxPlotsAgeSlots)) {
    local({
      ii <- i

      output[[paste0("hasAge", ii)]] <- reactive({
        req(scatterImagesReady())
        length(agePlots()$plotList) >= ii
      })
      outputOptions(output, paste0("hasAge", ii), suspendWhenHidden = FALSE)

      output[[paste0("ageTitle", ii)]] <- renderText({
        req(scatterImagesReady())
        req(length(agePlots()$fileNames) >= ii)
        agePlots()$fileNames[[ii]]
      })

      output[[paste0("age", ii)]] <- renderImage({
        req(scatterImagesReady())
        req(ii <= plotsRenderCount())
        req(length(agePlots()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Plots age image ", ii), {
          tryCatch({
            plot_to_save <- if (is_placeholder_plot(agePlots()$plotList[[ii]])) {
              agePlots()$plotList[[ii]]
            } else {
              agePlots()$plotList[[ii]] + plt_theme
            }
            render_plots_display_png(plot_to_save, width_in = 6, height_in = 6, disp_w = 700, limitsize = FALSE)
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
              labs(subtitle = agePlots()$fileNames[[ii]])
            render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
          })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("age", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadAge", ii)]] <- downloadHandler(
        filename = function() {
          base <- if (!is.null(agePlots()$fileNames) && length(agePlots()$fileNames) >= ii && !is.null(agePlots()$fileNames[[ii]])) {
            agePlots()$fileNames[[ii]]
          } else {
            paste0("plot-", ii)
          }
          paste0(get_short_experiment_name(experiment_names()), base, ".", downloadFileType())
        },
        content = function(file) {
          req(length(agePlots()$plotList) >= ii)
          if (is_placeholder_plot(agePlots()$plotList[[ii]])) return(invisible(NULL))
          plot <- agePlots()$plotList[[ii]] + plt_theme
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

  observeEvent(stackedPlots(), {
    # RSVP
    output$stackedRsvpPlot <- renderImage({
      req(histImagesReady())
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
      req(histImagesReady())
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
      req(histImagesReady())
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
      req(histImagesReady())
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
      req(histImagesReady())
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
      req(histImagesReady())
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
  
  #### fixed scatter slots ####
  for (i in seq_len(maxPlotsScatterSlots)) {
    local({
      ii <- i

      output[[paste0("hasScatter", ii)]] <- reactive({
        req(fontComparisonImagesReady())
        length(scatterDiagrams()$plotList) >= ii
      })
      outputOptions(output, paste0("hasScatter", ii), suspendWhenHidden = FALSE)

      output[[paste0("scatterTitle", ii)]] <- renderText({
        req(fontComparisonImagesReady())
        req(length(scatterDiagrams()$fileNames) >= ii)
        scatterDiagrams()$fileNames[[ii]]
      })

      output[[paste0("scatter", ii)]] <- renderImage({
        req(fontComparisonImagesReady())
        req(ii <= scatterRenderCount())
        req(length(scatterDiagrams()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Plots scatter image ", ii), {
          tryCatch({
            plot_to_save <- if (is_placeholder_plot(scatterDiagrams()$plotList[[ii]])) {
              scatterDiagrams()$plotList[[ii]]
            } else {
              scatterDiagrams()$plotList[[ii]] + plt_theme_scatter
            }
            result <- render_plots_display_png(plot_to_save, width_in = 7, height_in = 7, disp_w = 700, limitsize = FALSE)
            if (isolate(scatterRenderedCount()) < ii) scatterRenderedCount(ii)
            result
          }, error = function(e) {
            if (isolate(scatterRenderedCount()) < ii) scatterRenderedCount(ii)
            handle_plot_error(e, paste0("scatter", ii), experiment_names(), scatterDiagrams()$fileNames[[ii]])
          })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("scatter", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadScatter", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          scatterDiagrams()$fileNames[[ii]],
          ".",
          downloadFileType()
        ),
        content = function(file) {
          req(fontComparisonImagesReady())
          req(length(scatterDiagrams()$plotList) >= ii)
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
              plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
              width = 7,
              height = 7,
              units = "in",
              limitsize = FALSE,
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

  #### fixed violin slots ####
  for (i in seq_len(maxPlotsViolinSlots)) {
    local({
      ii <- i

      output[[paste0("hasViolin", ii)]] <- reactive({
        req(histImagesReady())
        length(violinPlots()$plotList) >= ii
      })
      outputOptions(output, paste0("hasViolin", ii), suspendWhenHidden = FALSE)

      output[[paste0("violinTitle", ii)]] <- renderText({
        req(histImagesReady())
        req(length(violinPlots()$fileNames) >= ii)
        violinPlots()$fileNames[[ii]]
      })

      output[[paste0("violin", ii)]] <- renderImage({
        req(histImagesReady())
        req(ii <= violinRenderCount())
        req(length(violinPlots()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Plots violin image ", ii), {
          tryCatch({
            result <- render_plots_display_png(
              if (is_placeholder_plot(violinPlots()$plotList[[ii]])) {
                violinPlots()$plotList[[ii]]
              } else {
                violinPlots()$plotList[[ii]] + plt_theme
              },
              width_in = 8,
              height_in = 6,
              disp_w = 700,
              limitsize = FALSE
            )
            if (isolate(violinRenderedCount()) < ii) violinRenderedCount(ii)
            result
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
              labs(subtitle = violinPlots()$fileNames[[ii]])
            result <- render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
            if (isolate(violinRenderedCount()) < ii) violinRenderedCount(ii)
            result
          })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("violin", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadViolin", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          violinPlots()$fileNames[[ii]],
          ".",
          downloadFileType()
        ),
        content = function(file) {
          req(histImagesReady())
          req(length(violinPlots()$plotList) >= ii)
          if (is_placeholder_plot(violinPlots()$plotList[[ii]])) return(invisible(NULL))

          if (downloadFileType() == "png") {
            tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
            ggsave(
              tmp_svg,
              plot = violinPlots()$plotList[[ii]] + plt_theme,
              width = 8,
              height = 6,
              unit = "in",
              limitsize = FALSE,
              device = svglite
            )
            rsvg::rsvg_png(tmp_svg, file, width = 1800, height = 1350)
          } else {
            ggsave(
              file,
              plot = violinPlots()$plotList[[ii]] + plt_theme,
              width = 8,
              height = 6,
              unit = "in",
              limitsize = FALSE,
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

  #### fixed font comparison slots ####
  for (i in seq_len(maxPlotsFontComparisonSlots)) {
    local({
      ii <- i

      output[[paste0("hasFontComparison", ii)]] <- reactive({
        req(violinImagesReady())
        length(fontComparisonPlots()$plotList) >= ii
      })
      outputOptions(output, paste0("hasFontComparison", ii), suspendWhenHidden = FALSE)

      output[[paste0("fontComparisonTitle", ii)]] <- renderText({
        req(violinImagesReady())
        req(length(fontComparisonPlots()$fileNames) >= ii)
        fontComparisonPlots()$fileNames[[ii]]
      })

      output[[paste0("fontComparison", ii)]] <- renderImage({
        req(violinImagesReady())
        req(ii <= fontComparisonRenderCount())
        req(length(fontComparisonPlots()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Plots font comparison image ", ii), {
          tryCatch({
            result <- render_plots_display_png(
              if (is_placeholder_plot(fontComparisonPlots()$plotList[[ii]])) {
                fontComparisonPlots()$plotList[[ii]]
              } else {
                fontComparisonPlots()$plotList[[ii]] + plt_theme
              },
              width_in = 8,
              height_in = 6,
              disp_w = 700,
              limitsize = FALSE
            )
            if (isolate(fontComparisonRenderedCount()) < ii) fontComparisonRenderedCount(ii)
            result
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
              labs(subtitle = fontComparisonPlots()$fileNames[[ii]])
            result <- render_plots_display_png(error_plot, width_in = 6, height_in = 4, disp_w = 700, use_png_theme = FALSE)
            if (isolate(fontComparisonRenderedCount()) < ii) fontComparisonRenderedCount(ii)
            result
          })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("fontComparison", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadFontComparison", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          fontComparisonPlots()$fileNames[[ii]],
          ".",
          downloadFileType()
        ),
        content = function(file) {
          req(violinImagesReady())
          req(length(fontComparisonPlots()$plotList) >= ii)
          if (is_placeholder_plot(fontComparisonPlots()$plotList[[ii]])) return(invisible(NULL))

          if (downloadFileType() == "png") {
            tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
            ggsave(
              tmp_svg,
              plot = fontComparisonPlots()$plotList[[ii]] + plt_theme,
              width = 8,
              height = 6,
              unit = "in",
              limitsize = FALSE,
              device = svglite
            )
            rsvg::rsvg_png(tmp_svg, file, width = 1800, height = 1350)
          } else {
            ggsave(
              file,
              plot = fontComparisonPlots()$plotList[[ii]] + plt_theme,
              width = 8,
              height = 6,
              unit = "in",
              limitsize = FALSE,
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

  list(
    agePlots = agePlots,
    histograms = histograms,
    scatterDiagrams = scatterDiagrams,
    violinPlots = violinPlots,
    fontComparisonPlots = fontComparisonPlots,
    laterSectionsReady = laterSectionsReady
  )
}
