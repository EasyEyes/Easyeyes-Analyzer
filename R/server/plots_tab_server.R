#### Plots tab server (no moduleServer) ####
# Registers Plots-tab reactives and render* outputs on the main output/session.
#
# DESIGN: Plot image outputs (hist / violin / fontComparison / scatter / age)
# MUST use suspendWhenHidden = TRUE. Do not flip to FALSE to “fix” progress
# or speed first paint on Plots. While the user is on Sessions, Distance, etc.,
# Shiny must not spend CPU/memory rendering Plots PNGs — that delays the
# active tab (e.g. Sessions summary). Progress / unlockers are gated on
# navbar == "Plots" for the same reason. Availability outputs (hasHist, …)
# may use suspendWhenHidden = FALSE but must short-circuit when Plots is
# inactive so they do not build plot lists off-tab.
#
# DESIGN: Switching away from Plots and back must NOT rebuild the same PNGs.
# suspendWhenHidden re-runs renderImage when the tab is shown again; we keep
# a per-content-gen file cache and only reset gates / bump progress when
# files() or df_list() change — never on navbar alone.

send_plots_progress <- function(session, ...) {
  session$sendCustomMessage("plotsPageProgress", list(...))
}

format_plots_progress_elapsed <- function(elapsed_sec) {
  elapsed_sec <- suppressWarnings(as.numeric(elapsed_sec)[1])
  if (!is.finite(elapsed_sec) || elapsed_sec < 0) {
    elapsed_sec <- 0
  }
  total <- as.integer(round(elapsed_sec))
  mins <- total %/% 60L
  secs <- total %% 60L
  sprintf("%d:%02d", mins, secs)
}

# Profiler label like "Plots histogram image 3: foveal-acuity-histogram".
plots_profile_image_label <- function(kind, index, file_names) {
  title <- ""
  if (is.list(file_names) || is.character(file_names)) {
    if (length(file_names) >= index && !is.null(file_names[[index]])) {
      title <- as.character(file_names[[index]])[1]
    }
  }
  if (!nzchar(title) || is.na(title)) {
    return(paste0("Plots ", kind, " image ", index))
  }
  paste0("Plots ", kind, " image ", index, ": ", title)
}

with_plots_histogram_theme <- function(plot) {
  if (is_placeholder_plot(plot)) {
    return(plot)
  }
  is_ratio_pair <- isTRUE(attr(plot, "ratio_r_hist_pair", exact = TRUE))
  legend_spacer <- attr(plot, "ratio_r_hist_legend_spacer", exact = TRUE)
  plot <- plot + hist_theme
  # Paired Crowding:acuity size-ratio hists: keep 1/3/10 labels horizontal
  # (hist_theme tilts x text by default).
  if (is_ratio_pair) {
    plot <- plot +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, vjust = 1)
      )
    # Bar hist keeps an invisible spacer legend between title and panel.
    if (identical(legend_spacer, "top")) {
      plot <- plot +
        ggplot2::theme(
          legend.position = "top",
          legend.text = ggplot2::element_text(color = "white"),
          legend.key = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          legend.box.background = ggplot2::element_blank()
        )
    } else if (identical(legend_spacer, "bottom")) {
      plot <- plot +
        ggplot2::theme(
          legend.position = "bottom",
          legend.text = ggplot2::element_text(color = "white"),
          legend.key = ggplot2::element_blank(),
          legend.background = ggplot2::element_blank(),
          legend.box.background = ggplot2::element_blank()
        )
    }
    attr(plot, "ratio_r_hist_pair") <- TRUE
    attr(plot, "ratio_r_hist_legend_spacer") <- legend_spacer
  }
  plot
}

save_plots_histogram <- function(file, plot, file_type) {
  plot <- with_plots_histogram_theme(plot)
  save_plots_display_download(
    file = file,
    plot = plot,
    file_type = file_type,
    width_in = 3.5,
    height_in = 3.5,
    disp_w = 280,
    png_theme_profile = "histogram",
    limitsize = FALSE,
    vector_size_scale = 1.4
  )
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
                                      summary_table = NULL,
                                      minDeg_table = NULL,
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
    list(plot = get_grade_histogram(df_list()$age),               fname = 'grade-histogram'),
    list(
      plot = crowding_acuity_size_ratio_r_histogram(df_list(), colorFont()),
      fname = 'crowding-acuity-size-ratio-r-histogram'
    ),
    list(
      plot = crowding_acuity_size_ratio_r_dot_histogram(df_list(), colorFont()),
      fname = 'crowding-acuity-size-ratio-r-dot-histogram-by-font-group'
    )
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

  lists <- append_hist_list(
    participant_device = participant_device_for_hists(
      if (is.null(summary_table)) NULL else summary_table()
    ),
    minDeg = if (is.null(minDeg_table)) NULL else minDeg_table(),
    plot_list = l,
    fileNames = fileNames,
    experimentNames = experiment_names()
  )

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
      list(plot = violins$reading, fname = 'reading-violin-by-font-plot', keep_colors = FALSE),
      list(plot = violins$rsvp, fname = 'rsvp-violin-by-font-plot', keep_colors = FALSE),
      list(plot = violins$crowding, fname = 'crowding-violin-by-font-plot', keep_colors = FALSE),
      list(plot = violins$acuity, fname = 'acuity-violin-by-font-plot', keep_colors = FALSE),
      list(
        plot = violins$acuity_by_phrase_group,
        fname = 'acuity-violin-by-font-phrase-group-plot',
        keep_colors = TRUE
      ),
      list(plot = violins$beauty, fname = 'beauty-violin-by-font-plot', keep_colors = FALSE),
      list(plot = violins$cmfrt, fname = 'comfort-violin-by-font-plot', keep_colors = FALSE),
      list(plot = violins$familiarity, fname = 'familiarity-violin-by-font-plot', keep_colors = FALSE)
    )
    
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        # Avoid overriding color scale for plots that define their own colors
        if (!isTRUE(call$keep_colors)) {
          plot <- plot + scale_color_manual(values = colorPalette)
        }
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
      # No in-plot title: measure is on the y-axis; filename is shown above the image.
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
    crowding_duration_plots <- plot_crowding_vs_duration_plots(df_list()$crowding)
    
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
      list(plot = crowding_duration_plots$mean, fname = 'crowding-vs-duration'),
      list(plot = crowding_duration_plots$by_side, fname = 'crowding-vs-duration-by-side'),
      list(plot = crowding_duration_plots$by_participant, fname = 'crowding-vs-duration-by-participant'),
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
    
    # Shared prep for by-font / native-legend / full-row scatters (once per list build).
    # Remember fonts/ paths only for ee_* plots; systemfonts registration stays
    # deferred until PNG render (ensure_* skips faces already loaded).
    crowding24_paired_lims <- paired_xheight_and_ratio_plot_limits(df_list())
    crowding24_by_font_prep <- prepare_crowding_xheight_vs_acuity_xheight_by_font_data(
      df_list(),
      colorFont(),
      resolve_fonts = FALSE,
      paired_lims = crowding24_paired_lims
    )
    crowding24_by_font_prep_fonts <- crowding24_by_font_prep
    if (!is.null(crowding24_by_font_prep_fonts) &&
        !"plot_family" %in% names(crowding24_by_font_prep_fonts$data)) {
      crowding24_by_font_prep_fonts$data <- crowding24_by_font_prep_fonts$data %>%
        dplyr::mutate(plot_family = resolve_crowding24_plot_font_families(excel_font))
    }

    comfort_beauty_plots <- list(
      list(plot = comfort_vs_crowding_scatter(df_list(), colorFont()), fname = 'comfort-vs-crowding-scatter'),
      list(plot = beauty_vs_crowding_scatter(df_list(), colorFont()), fname = 'beauty-vs-crowding-scatter'),
      list(plot = beauty_vs_comfort_scatter(df_list(), colorFont()), fname = 'beauty-vs-comfort-scatter'),
      list(plot = familiarity_vs_crowding_scatter(df_list(), colorFont()), fname = 'familiarity-vs-crowding-scatter'),
      list(
        plot = acuity_geomean_vs_sd_scatter(df_list(), colorFont()),
        fname = 'acuity-geomean-vs-sd-log-acuity-by-font'
      ),
      # list(
      #   plot = acuity_vs_crowding_by_font_scatter(df_list(), colorFont()),
      #   fname = 'acuity-vs-crowding-by-font'
      # ),
      list(
        plot = crowding_vs_acuity_by_font_scatter(df_list(), colorFont()),
        fname = 'crowding-vs-acuity-by-font'
      ),
      # list(
      #   plot = crowding_acuity_size_ratio_vs_sd_log_acuity_scatter(df_list(), colorFont()),
      #   fname = 'crowding-acuity-size-ratio-vs-sd-log-acuity'
      # ),
      list(
        plot = crowding_acuity_size_ratio_vs_acuity_xheight_scatter(df_list(), colorFont()),
        fname = 'crowding-acuity-size-ratio-vs-acuity-xheight'
      ),
      list(
        plot = crowding_acuity_size_ratio_vs_acuity_xheight_by_category_scatter(
          df_list(),
          colorFont()
        ),
        fname = 'crowding-acuity-size-ratio-vs-acuity-xheight-colored-by-font-group'
      ),
      list(
        plot = crowding_xheight_vs_acuity_xheight_by_category_scatter(
          df_list(),
          colorFont()
        ),
        fname = 'crowding-xheight-vs-acuity-xheight-colored-by-font-group'
      ),
      list(
        plot = crowding_xheight_vs_acuity_xheight_by_font_scatter(
          df_list(),
          colorFont(),
          prep = crowding24_by_font_prep
        ),
        fname = 'crowding-xheight-vs-acuity-xheight-by-font'
      ),
      # Font-file native-legend / row / abbrev plots last: remembering+registering
      # fonts/ is expensive — do once via shared prep, register only at PNG time.
      list(
        plot = crowding_xheight_vs_acuity_xheight_by_font_native_legend_scatter(
          df_list(),
          colorFont(),
          prep = crowding24_by_font_prep_fonts
        ),
        fname = 'crowding-xheight-vs-acuity-xheight-by-font-native-legend'
      ),
      list(
        plot = crowding_xheight_and_ratio_font_row_scatter(
          df_list(),
          colorFont(),
          prep = crowding24_by_font_prep_fonts
        ),
        fname = 'crowding-xheight-and-size-ratio-by-font-row'
      ),
      list(
        plot = crowding_xheight_vs_acuity_xheight_scatter(df_list(), colorFont()),
        fname = 'crowding-xheight-vs-acuity-xheight'
      )
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
  ageRenderedCount <- reactiveVal(0)
  # Correlation / N matrix PNGs sit above histograms on the Plots page.
  corrMatrixRendered <- reactiveVal(FALSE)
  nMatrixRendered <- reactiveVal(FALSE)

  # Non-blocking Plots progress popup (www/plotsPageProgress.js).
  plotsProgressGen <- reactiveVal(0L)
  plotsProgressStartedAt <- reactiveVal(NULL)
  # Bumps only on files()/df_list() — identifies cached PNGs for this dataset.
  plotsContentGen <- reactiveVal(0L)
  plotsProgressStartedForContentGen <- reactiveVal(-1L)
  plotsPngCache <- new.env(parent = emptyenv())

  clear_plots_png_cache <- function() {
    keys <- ls(plotsPngCache, all.names = TRUE)
    for (key in keys) {
      item <- plotsPngCache[[key]]
      if (is.list(item) && is.character(item$src) && length(item$src) >= 1) {
        unlink(item$src[[1]], force = TRUE)
      }
      rm(list = key, envir = plotsPngCache)
    }
  }

  get_cached_plots_png <- function(key) {
    item <- plotsPngCache[[key]]
    if (!is.list(item) || !is.character(item$src) || length(item$src) < 1) {
      return(NULL)
    }
    if (!file.exists(item$src[[1]])) {
      return(NULL)
    }
    item
  }

  cache_plots_png <- function(key, result) {
    if (!is.list(result) || !is.character(result$src) || length(result$src) < 1) {
      return(result)
    }
    if (!file.exists(result$src[[1]])) {
      return(result)
    }
    stable <- tempfile(fileext = ".png")
    ok <- file.copy(result$src[[1]], stable, overwrite = TRUE)
    if (!isTRUE(ok)) {
      return(result)
    }
    old <- plotsPngCache[[key]]
    if (is.list(old) && is.character(old$src) && length(old$src) >= 1) {
      unlink(old$src[[1]], force = TRUE)
    }
    cached <- result
    cached$src <- stable
    plotsPngCache[[key]] <- cached
    cached
  }

  # Prefer cache on tab re-entry; otherwise render and store. deleteFile must
  # be FALSE on the renderImage so Shiny does not delete cached paths.
  with_plots_png_cache <- function(key, render_fn, on_hit = NULL) {
    cached <- get_cached_plots_png(key)
    if (!is.null(cached)) {
      if (is.function(on_hit)) {
        on_hit()
      }
      return(cached)
    }
    result <- render_fn()
    cache_plots_png(key, result)
  }

  reset_downstream_render_counts <- function() {
    violinRenderCount(0)
    violinRenderedCount(0)
    fontComparisonRenderCount(0)
    fontComparisonRenderedCount(0)
    scatterRenderCount(0)
    scatterRenderedCount(0)
    plotsRenderCount(0)
    ageRenderedCount(0)
  }

  start_plots_progress <- function(stage = "Preparing plots …") {
    gen <- as.integer(plotsProgressGen()) + 1L
    plotsProgressGen(gen)
    plotsProgressStartedAt(Sys.time())
    send_plots_progress(
      session,
      active = TRUE,
      done = FALSE,
      stage = stage,
      detail = "",
      timerReset = TRUE,
      elapsedSec = 0,
      generation = gen
    )
  }

  plots_progress_elapsed_sec <- function() {
    t0 <- plotsProgressStartedAt()
    if (is.null(t0)) {
      return(0)
    }
    as.numeric(difftime(Sys.time(), t0, units = "secs"))
  }

  # Push immediately from R render completions (not DOM visibility).
  push_plots_progress_now <- function(stage, detail = "", done = FALSE) {
    gen <- isolate(plotsProgressGen())
    if (gen <= 0L) {
      return(invisible(NULL))
    }
    send_plots_progress(
      session,
      active = !done,
      done = done,
      stage = stage,
      detail = detail,
      timerReset = FALSE,
      elapsedSec = plots_progress_elapsed_sec(),
      generation = gen
    )
  }

  # Unlock next slot only after previous R PNG finished (serial R progress).
  advance_render_gate <- function(unlocked_rv, done_rv, total) {
    if (is.null(total) || !is.finite(total) || total <= 0) {
      return(invisible(NULL))
    }
    current <- unlocked_rv()
    done <- done_rv()
    if (current >= total) {
      return(invisible(NULL))
    }
    if (done >= current) {
      invalidateLater(10, session)
      unlocked_rv(current + 1L)
    } else {
      invalidateLater(100, session)
    }
  }

  # Wait until progressive unlock reaches this slot WITHOUT taking a reactive
  # dependency on the unlock counter. req(ii <= renderCount()) would re-run
  # every earlier slot on each increment (O(n^2) PNG renders / "infinite loop").
  # Poll with invalidateLater; plotsProgressGen() still invalidates on reset.
  req_plots_slot_unlocked <- function(ii, unlocked_rv) {
    req_progressive_slot_unlocked(
      ii,
      unlocked_rv,
      session,
      generation = plotsProgressGen
    )
  }

  mark_stage_rendered <- function(done_rv, ii, stage, total) {
    if (isolate(done_rv()) < ii) {
      done_rv(ii)
    }
    detail <- if (is.finite(total) && total > 0) {
      sprintf("%d / %d", min(ii, total), total)
    } else {
      ""
    }
    push_plots_progress_now(stage, detail)
  }

  reset_plots_progressive_gates <- function() {
    histRenderCount(0)
    histRenderedCount(0)
    corrMatrixRendered(FALSE)
    nMatrixRendered(FALSE)
    reset_downstream_render_counts()
  }

  plots_tab_active <- reactive({
    isTRUE(input$navbar == "Plots")
  })

  bump_plots_content_gen <- function() {
    reset_plots_progressive_gates()
    clear_plots_png_cache()
    plotsContentGen(as.integer(plotsContentGen()) + 1L)
  }

  maybe_start_plots_progress_for_content <- function() {
    if (!isTRUE(isolate(plots_tab_active())) ||
        is.null(isolate(files()))) {
      return(invisible(NULL))
    }
    content_gen <- isolate(plotsContentGen())
    # Same dataset as last progress run — tab return must not restart.
    if (isolate(plotsProgressStartedForContentGen()) == content_gen) {
      return(invisible(NULL))
    }
    plotsProgressStartedForContentGen(content_gen)
    # Show immediately on Plots even while df_list()/threshold still cooking;
    # otherwise the tab looks broken for a long silent wait.
    if (is.null(isolate(df_list()))) {
      start_plots_progress("Preparing plots …")
    } else {
      start_plots_progress("Plotting correlation matrices …")
    }
  }

  # Data changes only — never reset on navbar / tab switches.
  observeEvent(files(), {
    bump_plots_content_gen()
    maybe_start_plots_progress_for_content()
  }, ignoreInit = TRUE)

  observeEvent(df_list(), {
    bump_plots_content_gen()
    maybe_start_plots_progress_for_content()
  }, ignoreInit = TRUE)

  # Entering Plots: start progress only if this content gen was never started
  # (e.g. data settled while user was on Sessions).
  observeEvent(plots_tab_active(), {
    maybe_start_plots_progress_for_content()
  }, ignoreInit = TRUE)

  # Skip matrix stage when there is no correlation matrix to draw.
  observe({
    req(plots_tab_active())
    invisible(plotsContentGen())
    if (is.null(corrMatrix())) {
      corrMatrixRendered(TRUE)
      nMatrixRendered(TRUE)
    }
  })

  matrixImagesReady <- reactive({
    invisible(plotsContentGen())
    isTRUE(corrMatrixRendered()) && isTRUE(nMatrixRendered())
  })

  mark_matrix_rendered <- function(which = c("corr", "n")) {
    which <- match.arg(which)
    if (which == "corr") {
      corrMatrixRendered(TRUE)
    } else {
      nMatrixRendered(TRUE)
    }
    done <- sum(c(isTRUE(isolate(corrMatrixRendered())), isTRUE(isolate(nMatrixRendered()))))
    push_plots_progress_now(
      "Plotting correlation matrices …",
      sprintf("%d / 2", done)
    )
  }

  observe({
    req(plots_tab_active())
    req(matrixImagesReady())
    total <- min(length(histograms()$plotList), maxPlotsHistSlots)
    advance_render_gate(histRenderCount, histRenderedCount, total)
  })

  histImagesReady <- reactive({
    if (!isTRUE(matrixImagesReady())) return(FALSE)
    total <- min(length(histograms()$plotList), maxPlotsHistSlots)
    is.null(total) || total <= 0 || histRenderedCount() >= total
  })

  observeEvent(histImagesReady(), {
    if (!isTRUE(histImagesReady())) return(invisible(NULL))
    violinRenderCount(0)
    violinRenderedCount(0)
  }, ignoreInit = TRUE)

  observe({
    req(plots_tab_active())
    req(histImagesReady())
    total <- min(length(violinPlots()$plotList), maxPlotsViolinSlots)
    advance_render_gate(violinRenderCount, violinRenderedCount, total)
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
    req(plots_tab_active())
    req(violinImagesReady())
    total <- min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
    advance_render_gate(fontComparisonRenderCount, fontComparisonRenderedCount, total)
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
    req(plots_tab_active())
    req(fontComparisonImagesReady())
    total <- min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
    advance_render_gate(scatterRenderCount, scatterRenderedCount, total)
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
    ageRenderedCount(0)
  }, ignoreInit = TRUE)

  # Age slots: unlock after prior R PNG finishes (ageRenderedCount tracks done).
  observe({
    req(plots_tab_active())
    req(scatterImagesReady())
    total <- min(length(agePlots()$plotList), maxPlotsAgeSlots)
    advance_render_gate(plotsRenderCount, ageRenderedCount, total)
  })

  ageImagesReady <- reactive({
    if (!isTRUE(scatterImagesReady())) return(FALSE)
    total <- min(length(agePlots()$plotList), maxPlotsAgeSlots)
    is.null(total) || total <= 0 || ageRenderedCount() >= total
  })

  # Drive the floating Plots progress window from R-side PNG completion counts,
  # only while the Plots tab is active (other tabs must not pay this cost).
  # Always read every counter so updates are not swallowed when a stage-ready
  # reactive stays FALSE across intermediate N/total bumps.
  observe({
    req(plots_tab_active())
    gen <- plotsProgressGen()
    if (gen <= 0L || is.null(files())) {
      return(invisible(NULL))
    }

    # Still loading thresholds / df_list after upload — keep the panel alive.
    if (is.null(df_list())) {
      send_plots_progress(
        session,
        active = TRUE,
        done = FALSE,
        stage = "Preparing plots …",
        detail = "Waiting for data …",
        timerReset = FALSE,
        elapsedSec = plots_progress_elapsed_sec(),
        generation = gen
      )
      return(invisible(NULL))
    }

    hist_done <- histRenderedCount()
    violin_done <- violinRenderedCount()
    font_done <- fontComparisonRenderedCount()
    scatter_done <- scatterRenderedCount()
    age_done <- ageRenderedCount()
    scatter_unlocked <- scatterRenderCount()
    corr_done <- corrMatrixRendered()
    n_done <- nMatrixRendered()

    elapsed <- plots_progress_elapsed_sec()
    push <- function(stage, detail = "", done = FALSE) {
      send_plots_progress(
        session,
        active = !done,
        done = done,
        stage = stage,
        detail = detail,
        timerReset = FALSE,
        elapsedSec = elapsed,
        generation = gen
      )
    }

    if (!isTRUE(corr_done && n_done)) {
      done_n <- sum(c(isTRUE(corr_done), isTRUE(n_done)))
      push("Plotting correlation matrices …", sprintf("%d / 2", done_n))
      return(invisible(NULL))
    }

    hist_total <- min(length(histograms()$plotList), maxPlotsHistSlots)
    if (!(is.null(hist_total) || hist_total <= 0 || hist_done >= hist_total)) {
      detail <- if (is.finite(hist_total) && hist_total > 0) {
        sprintf("%d / %d", min(hist_done, hist_total), hist_total)
      } else {
        "Building histogram list …"
      }
      push("Plotting histograms …", detail)
      return(invisible(NULL))
    }

    violin_total <- min(length(violinPlots()$plotList), maxPlotsViolinSlots)
    if (!(is.null(violin_total) || violin_total <= 0 || violin_done >= violin_total)) {
      detail <- if (is.finite(violin_total) && violin_total > 0) {
        sprintf("%d / %d", min(violin_done, violin_total), violin_total)
      } else {
        ""
      }
      push("Plotting violins …", detail)
      return(invisible(NULL))
    }

    font_total <- min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
    if (!(is.null(font_total) || font_total <= 0 || font_done >= font_total)) {
      detail <- if (is.finite(font_total) && font_total > 0) {
        sprintf("%d / %d", min(font_done, font_total), font_total)
      } else {
        ""
      }
      push("Plotting font comparisons …", detail)
      return(invisible(NULL))
    }

    scatter_total <- min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
    if (!(is.null(scatter_total) || scatter_total <= 0 || scatter_done >= scatter_total)) {
      fname <- ""
      if (scatter_unlocked >= 1L && length(scatterDiagrams()$fileNames) >= scatter_unlocked) {
        fname <- as.character(scatterDiagrams()$fileNames[[scatter_unlocked]])
      }
      if (grepl("crowding-xheight|native-legend", fname, ignore.case = TRUE)) {
        detail <- "Registering font files for crowding vs acuity legends …"
      } else if (is.finite(scatter_total) && scatter_total > 0) {
        detail <- sprintf("%d / %d", min(scatter_done, scatter_total), scatter_total)
      } else {
        detail <- "Building scatter list …"
      }
      push("Plotting scatter diagrams …", detail)
      return(invisible(NULL))
    }

    age_total <- min(length(agePlots()$plotList), maxPlotsAgeSlots)
    if (!(is.null(age_total) || age_total <= 0 || age_done >= age_total)) {
      detail <- if (is.finite(age_total) && age_total > 0) {
        sprintf("%d / %d", min(age_done, age_total), age_total)
      } else {
        ""
      }
      push("Plotting age diagrams …", detail)
      return(invisible(NULL))
    }

    push(
      sprintf("Done. %s", format_plots_progress_elapsed(elapsed)),
      detail = "",
      done = TRUE
    )
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
    # Re-run when dataset content gen changes (not on mere tab switches).
    content_gen <- plotsContentGen()
    if (is.null(corrMatrix())) {
      corrMatrixRendered(TRUE)
      return(NULL)
    }

    with_plots_png_cache(
      paste0("corr:", content_gen),
      on_hit = function() corrMatrixRendered(TRUE),
      render_fn = function() {
        app_profile_time(app_profiler, "Plots correlation matrix image", {
          tryCatch({
            p <- add_experiment_title(corrMatrix()$plot, experiment_names())
            result <- render_plots_display_png(
              p,
              width_in = corrMatrix()$width,
              height_in = corrMatrix()$height,
              disp_w = 700
            )
            mark_matrix_rendered("corr")
            result
          }, error = function(e) {
            mark_matrix_rendered("corr")
            handle_plot_error(e, "corrMatrixPlot", experiment_names(), "Correlation Matrix Plot")
          })
        })
      }
    )
  }, deleteFile = FALSE)
  # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
  outputOptions(output, "corrMatrixPlot", suspendWhenHidden = TRUE)
  
  output$nMatrixPlot <- renderImage({
    content_gen <- plotsContentGen()
    if (is.null(corrMatrix())) {
      nMatrixRendered(TRUE)
      return(NULL)
    }

    with_plots_png_cache(
      paste0("nmatrix:", content_gen),
      on_hit = function() nMatrixRendered(TRUE),
      render_fn = function() {
        app_profile_time(app_profiler, "Plots N matrix image", {
          tryCatch({
            p <- add_experiment_title(corrMatrix()$n_plot, experiment_names())
            result <- render_plots_display_png(
              p,
              width_in = corrMatrix()$width,
              height_in = corrMatrix()$height,
              disp_w = 700
            )
            mark_matrix_rendered("n")
            result
          }, error = function(e) {
            mark_matrix_rendered("n")
            handle_plot_error(e, "nMatrixPlot", experiment_names(), "N Matrix Plot")
          })
        })
      }
    )
  }, deleteFile = FALSE)
  # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
  outputOptions(output, "nMatrixPlot", suspendWhenHidden = TRUE)
  
  output$fontAggregatedReadingRsvpCrowdingPlot <- renderImage({
    req(laterSectionsReady())
    content_gen <- isolate(plotsContentGen())
    with_plots_png_cache(
      paste0("fontAggReadingRsvp:", content_gen),
      render_fn = function() {
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
      }
    )
  }, deleteFile = FALSE)
  
  output$fontAggregatedOrdinaryReadingCrowdingPlot <- renderImage({
    req(laterSectionsReady())
    content_gen <- isolate(plotsContentGen())
    with_plots_png_cache(
      paste0("fontAggOrdinary:", content_gen),
      render_fn = function() {
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
      }
    )
  }, deleteFile = FALSE)
  
  output$fontAggregatedRsvpCrowdingPlot <- renderImage({
    req(laterSectionsReady())
    content_gen <- isolate(plotsContentGen())
    with_plots_png_cache(
      paste0("fontAggRsvp:", content_gen),
      render_fn = function() {
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
      }
    )
  }, deleteFile = FALSE)
  
  #### fixed histogram slots ####
  for (i in seq_len(maxPlotsHistSlots)) {
    local({
      ii <- i

      output[[paste0("hasHist", ii)]] <- reactive({
        # Keep placeholder "x name" plots visible (previous renderUI behavior).
        # Do not build the histogram list while another tab is active.
        if (!isTRUE(plots_tab_active())) return(FALSE)
        length(histograms()$plotList) >= ii
      })
      outputOptions(output, paste0("hasHist", ii), suspendWhenHidden = FALSE)

      output[[paste0("histTitle", ii)]] <- renderText({
        req(length(histograms()$fileNames) >= ii)
        histograms()$fileNames[[ii]]
      })

      output[[paste0("hist", ii)]] <- renderImage({
        req_plots_slot_unlocked(ii, histRenderCount)
        req(length(histograms()$plotList) >= ii)
        content_gen <- isolate(plotsContentGen())
        with_plots_png_cache(
          paste("hist", content_gen, ii, sep = ":"),
          on_hit = function() {
            if (isolate(histRenderedCount()) < ii) {
              histRenderedCount(ii)
            }
          },
          render_fn = function() {
            app_profile_time(app_profiler, plots_profile_image_label("histogram", ii, histograms()$fileNames), {
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
                mark_stage_rendered(
                  histRenderedCount,
                  ii,
                  "Plotting histograms …",
                  min(length(histograms()$plotList), maxPlotsHistSlots)
                )
                result
              }, error = function(e) {
                mark_stage_rendered(
                  histRenderedCount,
                  ii,
                  "Plotting histograms …",
                  min(length(histograms()$plotList), maxPlotsHistSlots)
                )
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
          }
        )
      }, deleteFile = FALSE)
      # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
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
        if (!isTRUE(plots_tab_active())) return(FALSE)
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
        req_plots_slot_unlocked(ii, plotsRenderCount)
        req(length(agePlots()$plotList) >= ii)
        content_gen <- isolate(plotsContentGen())
        with_plots_png_cache(
          paste("age", content_gen, ii, sep = ":"),
          on_hit = function() {
            if (isolate(ageRenderedCount()) < ii) ageRenderedCount(ii)
          },
          render_fn = function() {
            app_profile_time(app_profiler, plots_profile_image_label("age", ii, agePlots()$fileNames), {
              result <- tryCatch({
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
              mark_stage_rendered(
                ageRenderedCount,
                ii,
                "Plotting age diagrams …",
                min(length(agePlots()$plotList), maxPlotsAgeSlots)
              )
              result
            })
          }
        )
      }, deleteFile = FALSE)
      # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
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
          save_plots_display_download(
            file = file,
            plot = agePlots()$plotList[[ii]] + plt_theme,
            file_type = downloadFileType(),
            width_in = 6,
            height_in = 6,
            disp_w = 700,
            limitsize = FALSE
          )
        }
      )
    })
  }

  observeEvent(stackedPlots(), {
    build_stacked_rsvp_plot <- function() {
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
      add_experiment_title(base_plot, experiment_names())
    }

    # RSVP
    output$stackedRsvpPlot <- renderImage({
      req(histImagesReady())
      app_profile_time(app_profiler, "Plots stacked RSVP image", {
      render_plots_display_png(build_stacked_rsvp_plot(), width_in = 6, height_in = 8, disp_w = 600)
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
        save_plots_display_download(
          file = file,
          plot = build_stacked_rsvp_plot(),
          file_type = downloadFileType(),
          width_in = 6,
          height_in = 8,
          disp_w = 600,
          limitsize = FALSE
        )
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
        save_plots_display_download(
          file = file,
          plot = stackedPlots()$crowding_plot + plt_theme + stacked_theme,
          file_type = downloadFileType(),
          width_in = 6,
          height_in = 8,
          disp_w = 600,
          limitsize = FALSE
        )
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
        save_plots_display_download(
          file = file,
          plot = stackedPlots()$foveal_acuity_plot + plt_theme + stacked_theme,
          file_type = downloadFileType(),
          width_in = 6,
          height_in = 8,
          disp_w = 600,
          limitsize = FALSE
        )
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
        save_plots_display_download(
          file = file,
          plot = stackedPlots()$foveal_crowding_plot + plt_theme + stacked_theme,
          file_type = downloadFileType(),
          width_in = 6,
          height_in = 8,
          disp_w = 600,
          limitsize = FALSE
        )
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
        save_plots_display_download(
          file = file,
          plot = stackedPlots()$foveal_repeated_plot + plt_theme + stacked_theme,
          file_type = downloadFileType(),
          width_in = 6,
          height_in = 8,
          disp_w = 600,
          limitsize = FALSE
        )
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
          save_plots_display_download(
            file = file,
            plot = stackedPlots()$peripheral_acuity_plot + plt_theme + stacked_theme,
            file_type = downloadFileType(),
            width_in = 6,
            height_in = 8,
            disp_w = 600,
            limitsize = FALSE
          )
        }
      )
  })
  
  #### fixed scatter slots ####
  for (i in seq_len(maxPlotsScatterSlots)) {
    local({
      ii <- i

      output[[paste0("hasScatter", ii)]] <- reactive({
        if (!isTRUE(plots_tab_active())) return(FALSE)
        req(fontComparisonImagesReady())
        length(scatterDiagrams()$plotList) >= ii
      })
      outputOptions(output, paste0("hasScatter", ii), suspendWhenHidden = FALSE)

      output[[paste0("scatterFullWidth", ii)]] <- reactive({
        if (!isTRUE(plots_tab_active())) return(FALSE)
        plots <- scatterDiagrams()
        if (is.null(plots) || length(plots$plotList) < ii) {
          return(FALSE)
        }
        isTRUE(attr(plots$plotList[[ii]], "plots_full_row", exact = TRUE))
      })
      outputOptions(output, paste0("scatterFullWidth", ii), suspendWhenHidden = FALSE)

      output[[paste0("scatterTitle", ii)]] <- renderText({
        req(fontComparisonImagesReady())
        req(length(scatterDiagrams()$fileNames) >= ii)
        scatterDiagrams()$fileNames[[ii]]
      })

      output[[paste0("scatter", ii)]] <- renderImage({
        req(fontComparisonImagesReady())
        req_plots_slot_unlocked(ii, scatterRenderCount)
        req(length(scatterDiagrams()$plotList) >= ii)
        content_gen <- isolate(plotsContentGen())
        with_plots_png_cache(
          paste("scatter", content_gen, ii, sep = ":"),
          on_hit = function() {
            if (isolate(scatterRenderedCount()) < ii) scatterRenderedCount(ii)
          },
          render_fn = function() {
            app_profile_time(app_profiler, plots_profile_image_label("scatter", ii, scatterDiagrams()$fileNames), {
              tryCatch({
                plot_obj <- scatterDiagrams()$plotList[[ii]]
                plot_to_save <- if (is_placeholder_plot(plot_obj)) {
                  plot_obj
                } else {
                  apply_plt_theme_scatter(plot_obj)
                }
                plot_to_save <- copy_png_axis_scale_attrs(plot_obj, plot_to_save)
                # All Plots-tab scatters: axis numbers +50%, axis titles +30%.
                plot_to_save <- tag_png_axis_scales(
                  plot_to_save,
                  axis_text = 1.5,
                  axis_title = 1.3
                )
                scatter_h <- attr(plot_obj, "plots_display_height_in", exact = TRUE)
                if (!is.numeric(scatter_h) || length(scatter_h) < 1 || !is.finite(scatter_h[1]) ||
                    scatter_h[1] <= 0) {
                  scatter_h <- 7
                } else {
                  scatter_h <- as.numeric(scatter_h[1])
                }
                scatter_w <- attr(plot_obj, "plots_display_width_in", exact = TRUE)
                if (!is.numeric(scatter_w) || length(scatter_w) < 1 || !is.finite(scatter_w[1]) ||
                    scatter_w[1] <= 0) {
                  scatter_w <- 7
                } else {
                  scatter_w <- as.numeric(scatter_w[1])
                }
                result <- render_plots_display_png(
                  plot_to_save,
                  width_in = scatter_w,
                  height_in = scatter_h,
                  disp_w = max(700, round(700 * (scatter_w / 7))),
                  limitsize = FALSE
                )
                mark_stage_rendered(
                  scatterRenderedCount,
                  ii,
                  "Plotting scatter diagrams …",
                  min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
                )
                result
              }, error = function(e) {
                mark_stage_rendered(
                  scatterRenderedCount,
                  ii,
                  "Plotting scatter diagrams …",
                  min(length(scatterDiagrams()$plotList), maxPlotsScatterSlots)
                )
                handle_plot_error(e, paste0("scatter", ii), experiment_names(), scatterDiagrams()$fileNames[[ii]])
              })
            })
          }
        )
      }, deleteFile = FALSE)
      # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
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

          plot_obj <- scatterDiagrams()$plotList[[ii]]
          plot_to_save <- apply_plt_theme_scatter(plot_obj)
          plot_to_save <- copy_png_axis_scale_attrs(plot_obj, plot_to_save)
          # All Plots-tab scatters: axis numbers +50%, axis titles +30%.
          plot_to_save <- tag_png_axis_scales(
            plot_to_save,
            axis_text = 1.5,
            axis_title = 1.3
          )
          scatter_h <- attr(plot_obj, "plots_display_height_in", exact = TRUE)
          if (!is.numeric(scatter_h) || length(scatter_h) < 1 || !is.finite(scatter_h[1]) ||
              scatter_h[1] <= 0) {
            scatter_h <- 7
          } else {
            scatter_h <- as.numeric(scatter_h[1])
          }
          scatter_w <- attr(plot_obj, "plots_display_width_in", exact = TRUE)
          if (!is.numeric(scatter_w) || length(scatter_w) < 1 || !is.finite(scatter_w[1]) ||
              scatter_w[1] <= 0) {
            scatter_w <- 7
          } else {
            scatter_w <- as.numeric(scatter_w[1])
          }
          save_plots_display_download(
            file = file,
            plot = plot_to_save,
            file_type = downloadFileType(),
            width_in = scatter_w,
            height_in = scatter_h,
            disp_w = max(700, round(700 * (scatter_w / 7))),
            limitsize = FALSE
          )
        }
      )
    })
  }

  #### fixed violin slots ####
  for (i in seq_len(maxPlotsViolinSlots)) {
    local({
      ii <- i

      output[[paste0("hasViolin", ii)]] <- reactive({
        if (!isTRUE(plots_tab_active())) return(FALSE)
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
        req_plots_slot_unlocked(ii, violinRenderCount)
        req(length(violinPlots()$plotList) >= ii)
        content_gen <- isolate(plotsContentGen())
        with_plots_png_cache(
          paste("violin", content_gen, ii, sep = ":"),
          on_hit = function() {
            if (isolate(violinRenderedCount()) < ii) violinRenderedCount(ii)
          },
          render_fn = function() {
            app_profile_time(app_profiler, plots_profile_image_label("violin", ii, violinPlots()$fileNames), {
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
                  text_scale = 1.4,
                  scale_axis_text = FALSE,
                  limitsize = FALSE
                )
                mark_stage_rendered(
                  violinRenderedCount,
                  ii,
                  "Plotting violins …",
                  min(length(violinPlots()$plotList), maxPlotsViolinSlots)
                )
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
                mark_stage_rendered(
                  violinRenderedCount,
                  ii,
                  "Plotting violins …",
                  min(length(violinPlots()$plotList), maxPlotsViolinSlots)
                )
                result
              })
            })
          }
        )
      }, deleteFile = FALSE)
      # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
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

          save_plots_display_download(
            file = file,
            plot = violinPlots()$plotList[[ii]] + plt_theme,
            file_type = downloadFileType(),
            width_in = 8,
            height_in = 6,
            disp_w = 700,
            text_scale = 1.4,
            scale_axis_text = FALSE,
            limitsize = FALSE
          )
        }
      )
    })
  }

  #### fixed font comparison slots ####
  for (i in seq_len(maxPlotsFontComparisonSlots)) {
    local({
      ii <- i

      output[[paste0("hasFontComparison", ii)]] <- reactive({
        if (!isTRUE(plots_tab_active())) return(FALSE)
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
        req_plots_slot_unlocked(ii, fontComparisonRenderCount)
        req(length(fontComparisonPlots()$plotList) >= ii)
        content_gen <- isolate(plotsContentGen())
        with_plots_png_cache(
          paste("fontComparison", content_gen, ii, sep = ":"),
          on_hit = function() {
            if (isolate(fontComparisonRenderedCount()) < ii) {
              fontComparisonRenderedCount(ii)
            }
          },
          render_fn = function() {
            app_profile_time(app_profiler, plots_profile_image_label("font comparison", ii, fontComparisonPlots()$fileNames), {
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
                  text_scale = 1.4,
                  limitsize = FALSE
                )
                mark_stage_rendered(
                  fontComparisonRenderedCount,
                  ii,
                  "Plotting font comparisons …",
                  min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
                )
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
                mark_stage_rendered(
                  fontComparisonRenderedCount,
                  ii,
                  "Plotting font comparisons …",
                  min(length(fontComparisonPlots()$plotList), maxPlotsFontComparisonSlots)
                )
                result
              })
            })
          }
        )
      }, deleteFile = FALSE)
      # DESIGN: keep TRUE — see file header (do not render Plots off-tab).
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

          save_plots_display_download(
            file = file,
            plot = fontComparisonPlots()$plotList[[ii]] + plt_theme,
            file_type = downloadFileType(),
            width_in = 8,
            height_in = 6,
            disp_w = 700,
            text_scale = 1.4,
            limitsize = FALSE
          )
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
