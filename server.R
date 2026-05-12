#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize = 3000 * 1024 ^ 2)
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(emojifont)
library(ggpubr)
library(shinyjs)
library(lubridate)
library(ggpp)
library(svglite)
library(patchwork)
library(grid)
library(gridExtra)
library(ggnewscale)
# library(showtext)
# library(systemfonts)
# Enables automatic font loading for showtext

# showtext_auto(F)

source("./other/logger.R")
is_local <- Sys.getenv("SHINY_PORT") == ""
init_logger(enabled = is_local)

source('constant.R')
source("preprocess.R")

source("threshold_and_warning.R")

source("./error report/random_rgb.R")
source("./error report/summary_table.R")
source("./error report/prolific.R")


source("./plotting/mean_median_plot.R")
source("./plotting/regression_plot.R")
source("./plotting/histogram.R")
source("./plotting/diagram.R")
source("./plotting/crowding_plot.R")
source("./plotting/test_retest.R")
source("./plotting/scatter_plots.R")
source("./plotting/readingPlots.R")
source("./plotting/customized_inplot_table.R")
source("./plotting/profile_plot.R")
source("./plotting/simulatedRSVP.R")
source("./plotting/font_aggregated_plots.R")
source("./plotting/acuityPlot.R")
source('./plotting/crowdingrsvp.R')
source('./plotting/repeated-letter-crowding.R')
source('./plotting/durationSecPlot.R')
source('./plotting/participant_styles.R')
source('./plotting/distancePlot.R')
source('./plotting/minDegPlot.R')
source('./plotting/violin_plot.R')
source('./plotting/font_comparision_plots.R')
source('./plotting/auditory_crowding.R')

source("./other/getBits.R")
source("./other/sound_plots.R")
source("./other/read_json.R")
source("./other/formSpree.R")
source("./other/utility.R")
source("./other/anova_by_font.R")
source("./server_modules/sound_server.R")
source("./server_modules/profile_server.R")
source("./server_modules/staircases_server.R")
source("./server_modules/timing_server.R")
source("./server_modules/quality_server.R")
source("./server_modules/distance_server.R")
source("./server_modules/form_spree_dash_server.R")

#### server code ####

shinyServer(function(input, output, session) {
  app_profiler <- create_app_profiler(
    name = paste0("session-", substr(session$token, 1, 6)),
    enabled = identical(Sys.getenv("EASYEYES_PROFILE", if (is_local) "true" else "false"), "true")
  )

  observeEvent(input$file_click,
               {
                 app_profiler$reset("file upload dialog opened")
                 shinyalert(
                   title = "Reading file(s) ...",
                   text = paste0(
                     '<div style="margin-top: 20px; padding: 0 10px;">',
                     '  <div style="background-color: #e0e0e0; border-radius: 8px; overflow: hidden; height: 28px; box-shadow: inset 0 1px 3px rgba(0,0,0,0.15);">',
                     '    <div id="file-progress-bar" style="width: 0%; height: 100%; background: linear-gradient(90deg, #004192, #0066cc); border-radius: 8px; transition: width 0.3s ease; display: flex; align-items: center; justify-content: flex-end; padding-right: 8px;">',
                     '      <span id="file-progress-pct" style="color: white; font-size: 13px; font-weight: bold;"></span>',
                     '    </div>',
                     '  </div>',
                     '  <p id="file-progress-detail" style="margin-top: 14px; color: #555; font-size: 14px;">Starting...</p>',
                     '</div>'
                   ),
                   size = "m",
                   closeOnEsc = FALSE,
                   closeOnClickOutside = FALSE,
                   html = TRUE,
                   type = "",
                   showConfirmButton = FALSE,
                   confirmButtonCol = "#004192",
                   showCancelButton = FALSE,
                   imageUrl = "",
                   animation = TRUE
                 )
               },
               ignoreNULL = FALSE,
               ignoreInit = TRUE)
  
  
  
  
  output$ex1 <- renderDataTable({
    datatable(tibble())
  })
  
  #### reactive objects ####
  output$filename <- renderText({
    if (length(input$file$datapath) > 0) {
      return(basename(input$file$name[1]))
    } else {
      return(NULL)
    }
  })
  
  files <- reactive({
    req(input$file)
    app_profiler$reset("file upload received")
    app_profiler$mark(
      "uploaded files available",
      paste(length(input$file$name), "file(s):", paste(basename(input$file$name), collapse = ", "))
    )
    check <- check_file_names(input$file)
    if (is.null(check)) {
      t <- app_profile_time(app_profiler, "read_files", {
        read_files(input$file, progress = function(value, message, detail) {
        session$sendCustomMessage('updateFileProgress', list(
          value = round(value * 100, 1),
          detail = detail,
          close = FALSE
        ))
      })
      })
      session$sendCustomMessage('updateFileProgress', list(
        value = 100,
        detail = "Processing results...",
        close = FALSE
      ))
      log_info("File reading complete")
      return(t)
    } else {
      closeAlert()
      log_warn("Invalid file name(s) uploaded")
      shinyalert(
        title = check,
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
        size = 'l',
        html = TRUE,
        type = "error",
        showConfirmButton = FALSE,
        showCancelButton = FALSE,
        animation = TRUE
      )
      return(NULL)
    }
  }) %>% bindCache(input$file$datapath)
  
  output$fileStatusMessage <- renderUI({
    if (!is.null(files())) {
      prolific_counts <-
        get_prolific_file_counts(files()$prolific, summary_table())
      prolific_file_count <- if (prolific_counts$prolific_count > 0) 1 else 0
      HTML(
        paste0(
          'Analyzed ',
          length(files()$data_list),
          ' sessions and ',
          ifelse(nrow(files()$pretest) > 0, 1, 0),
          ' pretest file.<br>',
          'Read ',
          prolific_file_count,
          ' prolific record.<br>',
          'Read ',
          prolific_counts$formSpree_count,
          ' formSpree records.'
        )
      )
    }
  })
  
  experiment_names <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    exp_names <-files()[[3]]
    
    return(exp_names)
  })

  readingCorpus <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    return(trimws(files()[[4]]))
  })
  
  app_title <- reactiveValues(default = "EasyEyes Analyze")
  output$app_title <- renderText({
    "EasyEyes Analyze"
  })
  
  soundTabServer("sound", app_profiler = app_profiler)
  profileTabServer("profile", app_profiler = app_profiler)
  formSpreeTabServer("formSpree", app_profiler = app_profiler)
  
  #### reactive dataframes ####
  
  summary_table <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "generate_summary_table", {
      generate_summary_table(
      files()$data_list,
      files()$stairs,
      files()$pretest,
      files()$prolific)
    })

  })

  minNQuestTrials <-reactive({input$NQuestTrials}) %>% debounce(1000)
  maxQuestSD <- reactive({input$maxQuestSD}) %>% debounce(1000)
  conditionNames <- reactive(input$conditionName) %>% debounce(5000)
  calibrateTrackDistanceCheckLengthSDLogAllowed <- 
    reactive({
      input$calibrateTrackDistanceCheckLengthSDLogAllowed
    }) %>% debounce(2000)
  minWrongTrials <- reactive(input$NWrongTrials) %>% debounce(5000)
  maxReadingSpeed <- reactive(input$maxReadingSpeed) %>% debounce(2000)
  minRulerCm <- reactive({input$minRulerCm}) %>% debounce(2000)
  minCQAccuracy <- reactive({input$minCQAccuracy}) %>% debounce(2000)
  df_list <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    df_list <- app_profile_time(app_profiler, "generate_threshold", {
      generate_threshold(
      files()$data_list,
      files()$summary_list,
      files()$df,
      files()$pretest,
      files()$stairs,
      files()$prolific,
      input$filterInput,
      input$skillFilter,
      minNQuestTrials(),
      minWrongTrials(),
      maxQuestSD(),
      conditionNames(),
      maxReadingSpeed(),
      minRulerCm(),
      minCQAccuracy()
    )
    })
    return(
      df_list
    )
  })

  crowdingBySide <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    crowding_by_side(df_list()$crowding)
  })
  
  reading_rsvp_crowding_df <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    return(get_mean_median_df(df_list()))
  })
  
  corrMatrix <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    getCorrMatrix(df_list(), files()$pretest)
  })
  
  minDegPlots <- reactive({
    if (is.null(files()) || is.null(df_list())) {
      return(NULL)
    }
    get_minDeg_plots(files()$data_list,
                     df_list()$acuity,
                     df_list()$crowding,
                     df_list()$quest)
  })

  downloadFileType <- reactive(input$fileType)
  
  distanceModule <- distanceTabServer(
    "distance",
    files = files,
    df_list = df_list,
    experiment_names = experiment_names,
    minRulerCm = minRulerCm,
    calibrateTrackDistanceCheckLengthSDLogAllowed = calibrateTrackDistanceCheckLengthSDLogAllowed,
    fileType = downloadFileType,
    uploaded_file = reactive(input$file),
    app_profiler = app_profiler
  )
  
  staircasesModule <- staircasesTabServer(
    "staircases",
    files = files,
    df_list = df_list,
    conditionNames = conditionNames,
    thresholdParameter = reactive(input$thresholdParameter),
    fileType = downloadFileType,
    app_profiler = app_profiler
  )
  timingModule <- timingTabServer(
    "timing",
    files = files,
    conditionNames = conditionNames,
    experiment_names = experiment_names,
    fileType = downloadFileType,
    uploaded_file = reactive(input$file),
    app_profiler = app_profiler
  )
  qualityModule <- qualityTabServer(
    "quality",
    files = files,
    df_list = df_list,
    conditionNames = conditionNames,
    minDegPlots = minDegPlots,
    experiment_names = experiment_names,
    fileType = downloadFileType,
    uploaded_file = reactive(input$file),
    app_profiler = app_profiler
  )
  
  #### reactive plots #####
  
  crowdingPlot <- reactive({
    if (is.null(crowdingBySide())) {
      return(NULL)
    }
    crowding_scatter_plot(crowdingBySide())
  })
  
  #### rsvpCrowding reactive ####
  
  rsvpCrowding <- reactive({
    req(input$file)
    plot_rsvp_crowding(df_list())
  })
  
  rsvpAcuityFoveal <- reactive({
    req(input$file)
    plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp, 'foveal')
  })
  
  rsvpAcuityPeripheral <- reactive({
    req(input$file)
    plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp, 'peripheral')
  })
  
  rsvp_repeated_letter_crowding <- reactive({
    req(input$file)
    plot_rsvp_repeated_letter_crowding(df_list())
  })
  
  foveal_peripheral_diag <- reactive({
    req(input$file)
    get_foveal_peripheral_diag(df_list()$crowding)
  })
  
  
  foveal_crowding_vs_acuity_diag <- reactive({
    req(input$file)
    get_foveal_acuity_diag(df_list()$crowding, df_list()$acuity)
  })

  readingSpeedRetention <- reactive({
    req(input$file)
    base_plot <- reading_speed_vs_retention(df_list()[[1]]) +
      labs(subtitle = "Reading vs retention") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
    add_experiment_title(base_plot, experiment_names())
  })
  
  agePlots <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }

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

    return(list(plotList = l, fileNames = fileNames))
  })

  histograms <- reactive({
  if (is.null(files())) {
    return(list(plotList = list(), fileNames = list()))
  }

  l         <- list()
  fileNames <- list()

  # OPTIMIZATION: Compute expensive functions once, use results multiple times
  acuity_hists <- get_acuity_hist(df_list()$acuity)      # Single function call
  crowding_hists <- get_crowding_hist(df_list()$crowding) # Single function call
  aud_plots <- plot_auditory_crowding(df_list()$quest_all_thresholds, df_list()$crowding)

  static_calls <- list(
    list(plot = aud_plots$hist + hist_theme,                                   fname = 'auditory-crowding-melody-db-histogram'),
    list(plot = acuity_hists[[1]] + hist_theme,                                fname = 'foveal-acuity-histogram'),
    list(plot = acuity_hists[[2]] + hist_theme,                                fname = 'peripheral-acuity-histogram'),
    list(plot = crowding_hists$foveal+ hist_theme,                             fname = 'foveal-crowding-histogram'),
    list(plot = crowding_hists$peripheral+ hist_theme,                         fname = 'peripheral-crowding-histogram'),
    list(plot = get_reading_hist(df_list()$rsvp)+ hist_theme,                  fname = 'rsvp-reading-speed-histogram'),
    list(plot = get_reading_hist(df_list()$reading)+ hist_theme,               fname = 'reading-speed-histogram'),
    list(plot = get_repeatedLetter_hist(df_list()$repeatedLetters)+ hist_theme,fname = 'repeated-letter-crowding-histogram'),
    list(plot = get_age_histogram(df_list()$age)+ hist_theme,                  fname = 'age-histogram'),
    list(plot = get_grade_histogram(df_list()$age)+ hist_theme,                fname = 'grade-histogram')
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

  #### stacked histograms ####
  stackedPlots <- reactive({
    if (is.null(df_list()) | is.null(files())) {
      return(NULL)
    }
    
    # Generate the stacked plots
    stacked <- generate_histograms_by_grade(df_list())
    
    # Return all plots, including the new ones
    return(
      list(
        rsvp_plot = stacked$rsvp_reading_plot,
        crowding_plot = stacked$peripheral_crowding_plot,
        foveal_acuity_plot = stacked$foveal_acuity_plot,
        foveal_crowding_plot = stacked$foveal_crowding_plot,
        foveal_repeated_plot = stacked$foveal_repeated_plot,
        peripheral_acuity_plot = stacked$peripheral_acuity_plot
      )
    )
  })
  
  #### voilin plots ####
  
  violinPlots <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
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
    
    return(list(
      plotList = l,
      fileNames = fileNames
    ))
  })
  
  #### fontComparisonPlots ####
  
  fontComparisonPlots <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
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
    
    return(list(
      plotList = l,
      fileNames = fileNames
    ))
  })
  scatterDiagrams <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
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

    return(list(
      plotList = l,
      fileNames = fileNames
    ))
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
  
  #### plots ####

  output$corrMatrixPlot <- renderImage({
    # Check if correlation matrix data is available
    if (is.null(corrMatrix())) {
      return(NULL)
    }
    
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      p <- add_experiment_title(corrMatrix()$plot, experiment_names())
      ggsave(
        file = tmp_svg,
        plot = p,
        device = svglite,
        width = corrMatrix()$width,
        height = corrMatrix()$height
      )
      # Convert SVG to PNG at display width preserving aspect
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- if (!is.null(corrMatrix()$width) && corrMatrix()$width > 0) (corrMatrix()$height / corrMatrix()$width) else 1
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
    }, error = function(e) {
      handle_plot_error(e, "corrMatrixPlot", experiment_names(), "Correlation Matrix Plot")
    })
  }, deleteFile = TRUE)
  
  output$nMatrixPlot <- renderImage({
    # Check if correlation matrix data is available
    if (is.null(corrMatrix())) {
      return(NULL)
    }
    
    tryCatch({
      tmp_svg <- tempfile(fileext = '.svg')
      p <- add_experiment_title(corrMatrix()$n_plot, experiment_names())
      ggsave(
        file   = tmp_svg,
        plot   = p,   
        device = svglite,
        width  = corrMatrix()$width,
        height = corrMatrix()$height
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- if (!is.null(corrMatrix()$width) && corrMatrix()$width > 0) (corrMatrix()$height / corrMatrix()$width) else 1
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
    }, error = function(e) {
      handle_plot_error(e, "nMatrixPlot", experiment_names(), "N Matrix Plot")
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedReadingRsvpCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedReadingRsvpCrowding()
      outfile <- tempfile(fileext = '.svg')
      if (!is.null(plot)) {
        plot_with_title <- add_experiment_title(plot, experiment_names())
        ggsave(
          file = outfile,
          plot = plot_with_title + plt_theme,
          width = 8,
          height = 6,
          unit = 'in',
          limitsize = FALSE,
          device = svglite
        )
      } else {
        writeLines('<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"><text x="50%" y="50%" text-anchor="middle" dy=".3em" fill="#666">No data</text></svg>', outfile)
      }
      list(src = outfile, contenttype = 'image/svg+xml')
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedReadingRsvpCrowdingPlot", experiment_names(), "Font-aggregated reading vs peripheral crowding")
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedOrdinaryReadingCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedOrdinaryReadingCrowding()
      outfile <- tempfile(fileext = '.svg')
      if (!is.null(plot)) {
        plot_with_title <- add_experiment_title(plot, experiment_names())
        ggsave(
          file = outfile,
          plot = plot_with_title + plt_theme,
          width = 8,
          height = 6,
          unit = 'in',
          limitsize = FALSE,
          device = svglite
        )
      } else {
        writeLines('<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"><text x="50%" y="50%" text-anchor="middle" dy=".3em" fill="#666">No data</text></svg>', outfile)
      }
      list(src = outfile, contenttype = 'image/svg+xml')
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedOrdinaryReadingCrowdingPlot", experiment_names(), "Font-aggregated ordinary reading vs peripheral crowding")
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedRsvpCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedRsvpCrowding()
      outfile <- tempfile(fileext = '.svg')
      if (!is.null(plot)) {
        plot_with_title <- add_experiment_title(plot, experiment_names())
        ggsave(
          file = outfile,
          plot = plot_with_title + plt_theme,
          width = 8,
          height = 6,
          unit = 'in',
          limitsize = FALSE,
          device = svglite
        )
      } else {
        writeLines('<?xml version="1.0"?><svg xmlns="http://www.w3.org/2000/svg"><text x="50%" y="50%" text-anchor="middle" dy=".3em" fill="#666">No data</text></svg>', outfile)
      }
      list(src = outfile, contenttype = 'image/svg+xml')
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedRsvpCrowdingPlot", experiment_names(), "Font-aggregated RSVP vs peripheral crowding")
    })
  }, deleteFile = TRUE)
  
  # Foveal Crowding Plots
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
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            # Don't add plt_theme to placeholder plots (it overrides their blank axes)
            plot_to_save <- if (is_placeholder_plot(plotList[[i]])) plotList[[i]] else plotList[[i]] + plt_theme
            ggsave(
              file = tmp_svg,
              plot = plot_to_save,
              width = 6,
              height = 6,
              unit = 'in',
              limitsize = F,
              device = svglite
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((6 / 6) * png_w)
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
              labs(subtitle=fileNames[[i]])
            
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
        tmp_svg <- tempfile(fileext = '.svg')
        # Don't add hist_theme to placeholder plots
        plot_to_save <- if (is_placeholder_plot(plots[[jj]])) plots[[jj]] else plots[[jj]] + hist_theme
        ggsave(
          file = tmp_svg,
          plot = plot_to_save,
          device = svglite,
          width = 3.5,
          height = 3.5,
          unit = 'in',
          limitsize = FALSE
        )
        # Fit image to the container width to avoid horizontal scrollbars
        disp_w <- session$clientData[[paste0("output_", "hist", jj, "_width")]]
        if (is.null(disp_w) || is.na(disp_w) || disp_w <= 0) disp_w <- 380
        scale <- 2
        png_w <- round(disp_w * scale)
        png_h <- round(disp_w * scale)
        outfile <- tempfile(fileext = ".png")
        rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
        list(src = outfile, contenttype = 'image/png', width = disp_w, height = disp_w)
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
            
            if (downloadFileType() == "png") {
              ggsave(
                filename = file,
                plot = plots[[jj]] + hist_theme,
                device = ragg::agg_png,
                width = 3.5,
                height = 3.5,
                units = "in",
                dpi = 200,
                limitsize = FALSE
              )
            } else {
              ggsave(
                file,
                plot   = plots[[jj]] + hist_theme,
                width  = 3.5,
                height = 3.5,
                units  = "in",
                limitsize = FALSE,
                device   = if (downloadFileType() == "svg") svglite::svglite else downloadFileType()
              )
            }
          }
        )
      })
    }
    
    return(out)
  })

  observeEvent(stackedPlots(), {
    # RSVP
    output$stackedRsvpPlot <- renderImage({
      outfile <- tempfile(fileext = '.svg')
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
      ggsave(
        filename = outfile,
        plot = p,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        filename = outfile,
        plot = stackedPlots()$crowding_plot +
          plt_theme +
          stacked_theme,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        filename = outfile,
        plot = stackedPlots()$foveal_acuity_plot +
          plt_theme + stacked_theme,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        filename = outfile,
        plot = stackedPlots()$foveal_crowding_plot +
          plt_theme + stacked_theme,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        filename = outfile,
        plot = stackedPlots()$foveal_repeated_plot +
          plt_theme + stacked_theme,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        filename = outfile,
        plot = stackedPlots()$peripheral_acuity_plot +
          plt_theme + stacked_theme,
        width = 6,
        height = 8,
        unit = "in",
        device = svglite
      )
      list(src = outfile, contenttype = 'svg')
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
          # outfile <- tempfile(fileext = '.svg')
          # ggsave(
          #   file = outfile,
          #   plot = scatterDiagrams()$plotList[[ii]] +
          #     plt_theme_scatter +
          #     scale_color_manual(values = colorPalette),
          #   width = 7,
          #   height = 7,
          #   unit = 'in',
          #   limitsize = F,
          #   device = svglite
          # )
          # 
          # list(src = outfile,
          #      contenttype = 'svg')
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            # Don't add plt_theme_scatter to placeholder plots
            plot_to_save <- if (is_placeholder_plot(scatterDiagrams()$plotList[[ii]])) {
              scatterDiagrams()$plotList[[ii]]
            } else {
              scatterDiagrams()$plotList[[ii]] + plt_theme_scatter
            }
            ggsave(
              file = tmp_svg,
              plot = plot_to_save,
              width = 7,
              height = 7,
              unit = 'in',
              limitsize = F,
              device = svglite
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- png_w
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = disp_w)
          }, error = function(e) {
          handle_plot_error(e, paste0("scatter", ii), experiment_names(), scatterDiagrams()$fileNames[[ii]])
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
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            ggsave(
              file = tmp_svg,
              plot = violinPlots()$plotList[[ii]] +
                plt_theme,
              width = 8,
              height = 6,
              unit = 'in',
              device = svglite,
              limitsize = FALSE
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((6 / 8) * png_w)
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
              labs(subtitle=violinPlots()$fileNames[[ii]])
            
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
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            ggsave(
              file = tmp_svg,
              plot = fontComparisonPlots()$plotList[[ii]] +
                plt_theme,
              width = 8,
              height = 6,
              unit = 'in',
              device = svglite,
              limitsize = FALSE
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((6 / 8) * png_w)
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
              labs(subtitle=fontComparisonPlots()$fileNames[[ii]])
            
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
  
  #### Event Handler ####
  
  observeEvent(files(),
               {
                 if (!is.null(files())) {
                   # Extract file names from the uploaded files
                   names <- input$file$name
                   
                   # Filter out the names that end with "zip" in a case-insensitive manner
                   zip_names <-
                     names[grepl("\\.zip$", names, ignore.case = TRUE)]
                   
                   if (length(zip_names) > 0) {
                     # If there are any ZIP files, use the name of the first ZIP file
                     app_title$default <-
                       tools::file_path_sans_ext(basename(zip_names[1]))
                   } else {
                     # If there are no ZIP files but there are CSV files, concatenate their names
                     csv_names <-
                       names[grepl("\\.csv$", names, ignore.case = TRUE)]
                     if (length(csv_names) > 0) {
                       concatenated_names <-
                         tools::file_path_sans_ext(basename(csv_names[1]))
                       app_title$default <- concatenated_names
                     } else {
                       # Fallback if no ZIP or CSV files are uploaded
                       app_title$default <- "EasyEyes Analyze"
                     }
                   }
                   output$app_title <- renderText({
                     ifelse(app_title$default == "",
                            "EasyEyes Analysis",
                            app_title$default)
                   })
                   session$sendCustomMessage("updateTitle", "Hello")
                                     set.seed(2023)
                  #### summary page ####
                  output$instruction <- renderText(instruction)
                  output$experiment <-
                    renderText(experiment_names())
                  
                  participants <-
                    unique(summary_table()$`Pavlovia session ID`)
                   prolific_id <-
                     unique(summary_table()$`Prolific participant ID`)
                   
                   output$ex1 <- DT::renderDataTable({
                     render_summary_datatable(summary_table(), participants, prolific_id)
                   })
                   
                   #### stats page ####
                   output$thresholdSummary <-
                     renderTable(df_list()$threshold)
                   output$thresholdAll <-
                     renderTable(df_list()$threshold_each)
                   output$QA <-
                     DT::renderDataTable(df_list()$QA)
                   output$ratings <-
                     renderTable(df_list()$ratings)
                   output$participantInfo <- renderTable({
                     df_list()$participant_info
                   }, width = "100%", spacing = "xs")
                   
                   updateSelectInput(
                     session,
                     'thresholdParameter',
                     choices = unique(files()$stairs$thresholdParameter),
                     selected = unique(files()$stairs$thresholdParameter)[1],
                   )
                   
                   updateCheckboxGroupInput(
                     session,
                     inputId = 'conditionName',
                     label = "Include these conditionNames:",
                     choices = sort(df_list()$conditionNames),
                     selected = df_list()$conditionNames,
                     inline = FALSE
                   )
                   
                   closeAlert()
                 }
               })
  
   #### anova page ####
  anova_results <- reactive({calculate_anova(df_list())})
  
  # ANOVA summary tables
  output$readingANOVA <- renderTable({
    if (!is.null(anova_results()$reading$summary)) {
      anova_results()$reading$summary[[1]]
    }
  }, rownames = TRUE)
  
  output$crowdingANOVA <- renderTable({
    if (!is.null(anova_results()$crowding$summary)) {
      anova_results()$crowding$summary[[1]]
    }
  }, rownames = TRUE)
  
  output$rsvpANOVA <- renderTable({
    if (!is.null(anova_results()$rsvp$summary)) {
      anova_results()$rsvp$summary[[1]]
    }
  }, rownames = TRUE)
  
  output$beautyANOVA <- renderTable({
    if (!is.null(anova_results()$beauty$summary)) {
      anova_results()$beauty$summary[[1]]
    }
  }, rownames = TRUE)
  
  output$comfortANOVA <- renderTable({
    if (!is.null(anova_results()$comfort$summary)) {
      anova_results()$comfort$summary[[1]]
    }
  }, rownames = TRUE)
  
  # Pairwise comparison tables
  output$readingPairwise <- renderTable({
    if (!is.null(anova_results()$reading$pairwise)) {
      anova_results()$reading$pairwise$p.value
    }
  }, rownames = TRUE)
  
  output$crowdingPairwise <- renderTable({
    if (!is.null(anova_results()$crowding$pairwise)) {
      anova_results()$crowding$pairwise$p.value
    }
  }, rownames = TRUE)
  
  output$rsvpPairwise <- renderTable({
    if (!is.null(anova_results()$rsvp$pairwise)) {
      anova_results()$rsvp$pairwise$p.value
    }
  }, rownames = TRUE)
  
  output$beautyPairwise <- renderTable({
    if (!is.null(anova_results()$beauty$pairwise)) {
      anova_results()$beauty$pairwise$p.value
    }
  }, rownames = TRUE)
  
  output$comfortPairwise <- renderTable({
    if (!is.null(anova_results()$comfort$pairwise)) {
      anova_results()$comfort$pairwise$p.value
    }
  }, rownames = TRUE)
  
  # Download handler for ANOVA tables PDF
  output$downloadAnovaTables <- downloadHandler(
    filename = function() {
      paste0(get_short_experiment_name(experiment_names()), "anova-tables.pdf")
    },
    content = function(file) {
      
      # Create a temporary PDF file
      pdf(file, width = 8.5, height = 11, onefile = TRUE)
      
      # Get ANOVA results
      anova_data <- anova_results()
      
      # Define the measures and their display names
      measures <- list(
        list(key = "reading", name = "Reading Speed"),
        list(key = "crowding", name = "Crowding Distance"), 
        list(key = "rsvp", name = "RSVP Reading Speed"),
        list(key = "beauty", name = "Beauty Rating"),
        list(key = "comfort", name = "Comfort Rating")
      )
      
      # Create a page for each measure
      for (measure in measures) {
        # Start a new page
        grid.newpage()
        
        # Create page title
        grid.text(
          paste("ANOVA Analysis:", measure$name),
          x = 0.5, y = 0.95,
          gp = gpar(fontsize = 16, fontface = "bold"),
          just = "center"
        )
        
        # Check if data exists for this measure
        measure_data <- anova_data[[measure$key]]
        
        if (!is.null(measure_data) && !is.null(measure_data$summary)) {
          # ANOVA Results table
          if (is.list(measure_data$summary) && length(measure_data$summary) > 0) {
            anova_table <- measure_data$summary[[1]]
            if (!is.null(anova_table) && nrow(anova_table) > 0) {
              # Create ANOVA table grob
              anova_grob <- tableGrob(
                anova_table,
                rows = rownames(anova_table),
                theme = ttheme_default(
                  core = list(fg_params = list(cex = 0.6)),
                  colhead = list(fg_params = list(cex = 0.7, fontface = "bold")),
                  rowhead = list(fg_params = list(cex = 0.6, fontface = "bold"))
                )
              )
              
              # Add ANOVA table title
              grid.text(
                "ANOVA Results",
                x = 0.5, y = 0.8,
                gp = gpar(fontsize = 12, fontface = "bold"),
                just = "center"
              )
              
              # Position ANOVA table
              vp_anova <- viewport(x = 0.5, y = 0.65, width = 0.9, height = 0.15)
              pushViewport(vp_anova)
              grid.draw(anova_grob)
              popViewport()
            }
          }
          
          # Pairwise comparisons table
          if (!is.null(measure_data$pairwise) && !is.null(measure_data$pairwise$p.value)) {
            pairwise_table <- measure_data$pairwise$p.value
            if (!is.null(pairwise_table) && nrow(pairwise_table) > 0) {
              # Create pairwise table grob
              pairwise_grob <- tableGrob(
                pairwise_table,
                rows = rownames(pairwise_table),
                theme = ttheme_default(
                  core = list(fg_params = list(cex = 0.6)),
                  colhead = list(fg_params = list(cex = 0.7, fontface = "bold")),
                  rowhead = list(fg_params = list(cex = 0.6, fontface = "bold"))
                )
              )
              
              # Add pairwise table title
              grid.text(
                "Pairwise Comparisons (Bonferroni corrected)",
                x = 0.5, y = 0.45,
                gp = gpar(fontsize = 12, fontface = "bold"),
                just = "center"
              )
              
              # Position pairwise table
              vp_pairwise <- viewport(x = 0.5, y = 0.3, width = 0.9, height = 0.25)
              pushViewport(vp_pairwise)
              grid.draw(pairwise_grob)
              popViewport()
            }
          }
        } else {
          # No data available message
          grid.text(
            "No data available for this measure",
            x = 0.5, y = 0.5,
            gp = gpar(fontsize = 12, col = "red"),
            just = "center"
          )
        }
      }
      
      # Close the PDF device
      dev.off()
    }
  )

  
  #### download handlers ####
  
  
  plotsTabDownloadSpecs <- reactive({
    specs <- list()
    correlation <- corrMatrix()
    if (!is.null(correlation) && !is.null(correlation$plot)) {
      specs <- c(specs, list(plot_download_spec(
        correlation$plot,
        "correlation-matrix",
        width = correlation$width,
        height = correlation$height
      )))
      if (!is.null(correlation$n_plot)) {
        specs <- c(specs, list(plot_download_spec(
          correlation$n_plot,
          "pairwise-count-matrix",
          width = correlation$width,
          height = correlation$height
        )))
      }
    }

    ggiraph_plots <- list(
      plotList = list(
        rsvpCrowding()$p_grade,
        rsvpCrowding()$f_grade,
        rsvpCrowding()$p_font,
        rsvpCrowding()$residual,
        rsvpCrowding()$f_font,
        rsvpAcuityFoveal()[[2]],
        rsvpAcuityPeripheral()[[2]],
        rsvp_repeated_letter_crowding()[[2]],
        ordinaryAcuityFoveal()[[2]],
        ordinaryAcuityPeripheral()[[2]],
        ordinaryCrowdingPlots()[[1]],
        ordinaryCrowdingPlots()[[2]],
        ordinaryCrowdingPlots()[[3]],
        ordinaryCrowdingPlots()[[4]],
        readingRepeatedPlots()[[2]]
      ),
      fileNames = list(
        "rsvp-vs-peripheral-crowding-by-grade",
        "rsvp-vs-foveal-crowding-by-grade",
        "rsvp-vs-peripheral-crowding-by-font",
        "residual-rsvp-vs-residual-peripheral-crowding-by-font",
        "rsvp-vs-foveal-crowding-by-font",
        "rsvp-foveal-acuity-by-grade",
        "rsvp-peripheral-acuity-by-grade",
        "rsvp-repeated-letter-by-grade",
        "ordinary-foveal-acuity-by-grade",
        "ordinary-peripheral-acuity-by-grade",
        "ordinary-peripheral-crowding-by-age",
        "ordinary-peripheral-crowding-by-grade",
        "ordinary-foveal-crowding-by-age",
        "ordinary-foveal-crowding-by-grade",
        "reading-repeated-letter-by-grade"
      )
    )

    c(
      specs,
      plot_list_download_specs(histograms()$plotList, histograms()$fileNames, theme = plt_theme),
      plot_list_download_specs(scatterDiagrams()$plotList, scatterDiagrams()$fileNames, theme = plt_theme_scatter),
      plot_list_download_specs(agePlots()$plotList, agePlots()$fileNames, theme = plt_theme),
      plot_list_download_specs(ggiraph_plots$plotList, ggiraph_plots$fileNames, theme = plt_theme_ggiraph),
      plot_list_download_specs(violinPlots()$plotList, violinPlots()$fileNames, theme = plt_theme, width = 8),
      plot_list_download_specs(fontComparisonPlots()$plotList, fontComparisonPlots()$fileNames, theme = plt_theme, width = 8)
    )
  })

  currentTabDownloadSpecs <- reactive({
    tab <- input$navbar
    if (is.null(tab)) {
      tab <- "Plots"
    }

    switch(
      tab,
      "Plots" = plotsTabDownloadSpecs(),
      "Distance" = distanceModule$downloadSpecs(),
      "Quality" = qualityModule$downloadSpecs(),
      "Timing" = timingModule$downloadSpecs(),
      "Staircases" = staircasesModule$downloadSpecs(),
      list()
    )
  })

  output$downloadAll = downloadHandler(
    filename = function() {
      tab <- input$navbar
      if (is.null(tab)) tab <- "plots"
      paste0(tolower(tab), "-plots.zip")
    },
    content = function(file) {
      tab <- input$navbar
      if (is.null(tab)) tab <- "Plots"
      save_download_specs_zip(
        specs = currentTabDownloadSpecs(),
        zip_file = file,
        fileType = downloadFileType(),
        prefix = get_short_experiment_name(experiment_names()),
        empty_message = paste0("No plot bundle is available for the ", tab, " tab.")
      )
    }
  )
  
  output$sessionCsv <- downloadHandler(
    filename = function() {
      ifelse(
        app_title$default == "EasyEyes Analysis",
        "sessions.xlsx",
        paste0(app_title$default, "-sessions.xlsx")
      )
    },
    content = function(filename) {
      
      openxlsx::write.xlsx(summary_table(), file = filename)
    }
  )
  
  output$report <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "error report.html",
        paste0(get_short_experiment_name(experiment_names()), "error-report.html")
      )
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "error report.Rmd")
      file.copy("rmd/error report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(
        tempReport,
        output_file = file,
        params = list(data = summary_table() %>% mutate(experiment = experiment_names())),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  
  output$thresholdOne <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Summary-of-each-condition.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "-Summary-of-each-condition.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(df_list()$threshold, file = filename)  # Using openxlsx
    }
  )
  
  output$thresholdTwo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Thresholds(brief).xlsx",
        paste0(get_short_experiment_name(experiment_names()), "-Thresholds(brief).xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(df_list()$threshold_each, file = filename)  # Using openxlsx
    }
  )
  
  output$thresholdThree <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "IndividualResults.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "-IndividualResults.xlsx")
      )
    },
    content = function(filename) {
      data <- df_list()$all_summary
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, "IndividualResults")
      openxlsx::writeData(wb, "IndividualResults", data)
      decimal_style <- openxlsx::createStyle(numFmt = "0.00")
      rating_cols <- which(names(data) %in% c("Comfort", "Beauty", "Familiarity"))
      for (col_idx in rating_cols) {
        openxlsx::addStyle(wb, "IndividualResults", decimal_style,
                           rows = 2:(nrow(data) + 1), cols = col_idx, gridExpand = TRUE)
      }
      openxlsx::saveWorkbook(wb, file = filename, overwrite = TRUE)
    }
  )
  
  output$participantInfoExcel <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "ParticipantDistanceInfo.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "-ParticipantDistanceInfo.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(distanceModule$mergedParticipantDistanceTable(), file = filename)  # Using openxlsx
    }
  )
  
  output$ratingSummary <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "RatingSummary.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "RatingSummary.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(df_list()$ratings, file = filename)  # Using openxlsx
    }
  )
  
  #### download plot handlers ####
  
  # download plot handlers
    ##### download handler for grade plots #####
    
    output$downloadCrowdingAgePlot <- downloadHandler(
      filename = function() paste0('crowding-vs-age',
                        '.',
                        downloadFileType()),
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = plot_crowding_vs_age(df_list()$crowding) + plt_theme,
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
            plot = plot_crowding_vs_age(df_list()$crowding) + plt_theme,
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
    
    output$downloadAcuityAgePlot <- downloadHandler(
      filename = function() paste0('acuity-vs-age',
                        '.',
                        downloadFileType()),
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = plot_acuity_vs_age(df_list()) + plt_theme,
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
            plot = plot_acuity_vs_age(df_list()) + plt_theme,
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
    
    output$downloadRsvpCrowdingPeripheralGradePlot <- downloadHandler(
      filename = function() paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-grade.', downloadFileType()),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$p_grade + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = downloadFileType(),
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpCrowdingPeripheralFontPlot <- downloadHandler(
      filename = function() paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-font.', downloadFileType()),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$p_font + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = downloadFileType(),
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpResidualCrowding <- downloadHandler(
      filename = function() paste(
        app_title$default,
        paste0(
          'residual-rsvp-vs-residual-peripheral-crowding-by-grade.',
          downloadFileType()
        ),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$residual + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = downloadFileType(),
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpCrowdingFovealGradePlot <- downloadHandler(
      filename = function() {
        paste0(
          experiment_names(),
          'rsvp-vs-foveal-crowding-by-grade.',
          downloadFileType()
        )
      },
      content = function(file) {
        plot <- rsvpCrowding()$f_grade + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = downloadFileType(),
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpFovealAcuityGradePlot <- downloadHandler(
      filename = function () {
        paste0(get_short_experiment_name(experiment_names()),
               'rsvp-vs-fovel-acuity-by-grade.',
               downloadFileType())
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          base_plot <- rsvpAcuityFoveal()[[2]] + plt_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            "tmp.svg",
            plot = plot_with_title,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2400,
                         height = 1800)
        } else {
          base_plot <- rsvpAcuityFoveal()[[2]] + plt_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          ggsave(
            file = file,
            plot = plot_with_title,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    
    output$downloadFactorOutAgePlot <- downloadHandler(
      filename = function () {
        paste0(get_short_experiment_name(experiment_names()),
               'rsvp-vs-crowding-factored-age.',
               downloadFileType())
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = factor_out_age_and_plot(df_list()) +
              plt_theme,
            width = 6,
            device = svglite,
            limitsize = FALSE
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2400,
                         height = 1800)
        } else {
          ggsave(
            file = file,
            plot = factor_out_age_and_plot(df_list()) +
              plt_theme,
            device = svg,
            width = 6
          )
        }
      }
    )
    
    
    output$downloadRsvpPeripheralAcuityGradePlot <-
      downloadHandler(
        filename = function () {
          paste0(get_short_experiment_name(experiment_names()),
                 'rsvp-vs-periphreal-acuity-by-grade.',
                 downloadFileType())
        },
        content = function(file) {
          base_plot <- rsvpAcuityPeripheral()$grade + plt_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          savePlot(plot = plot_with_title,
                   filename = file,
                   fileType = downloadFileType(),
                   width = 8,
                   height = 6
                   )
        }
      )
    
    output$downloadRsvpPeripheralAcuityFontPlot <-
      downloadHandler(
        filename = function () {
          paste0(get_short_experiment_name(experiment_names()),
                 'rsvp-vs-periphreal-acuity-by-font.',
                 downloadFileType())
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            base_plot <- rsvpAcuityPeripheral()$font + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              "tmp.svg",
              plot = plot_with_title,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg",
                           file,
                           width = 2400,
                           height = 1800)
          } else {
            base_plot <- rsvpAcuityPeripheral()$font + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              file = file,
              plot = plot_with_title,
              device = svg,
              width = 8,
              height = 6,
              dpi = 300
            )
          }
        }
      )
    
    output$downloadrsvpRepeatedGradePlot <- downloadHandler(
      filename = function () {
        paste0(
          experiment_names(),
          'rsvp-vs-repeated-letter-crowding-by-grade.',
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvp_repeated_letter_crowding()[[2]] +
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2400,
                         height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvp_repeated_letter_crowding()[[2]] +
              plt_theme,
            device = svg,
            width = 8,
            height = 6
          )
        }
      }
    )
    
    
    #### reading plots downlaod ####
    output$downloadOrdinaryPeripheralAcuityGradePlot <- downloadHandler(
      filename = function() paste0(
        get_short_experiment_name(experiment_names()),
        'reading-vs-peripheral-acuity-by-grade.',
        downloadFileType()
      ),
      content = function(file) {
        plot <- ordinaryAcuityPeripheral()$grade + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = downloadFileType(),
          width = 6
        )
      }
    )
    
    output$downloadOrdinaryPeripheralAcuityFontPlot <- downloadHandler(
      filename = function() paste0(
        get_short_experiment_name(experiment_names()),
        'reading-vs-peripheral-acuity-by-font.',
        downloadFileType()
      ),
      content = function(file) {
        plot <- ordinaryAcuityPeripheral()$font + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = downloadFileType(),
          width = 6
        )
      }
    )
    
    output$downloadOrdinaryFovealAcuityGradePlot <-
      downloadHandler(
        filename = function() {
          paste0("ordinary_foveal_acuity_grade", downloadFileType(), sep = ".")
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            ggsave(
              "tmp.svg",
              plot = ordinaryAcuityFoveal()[[2]] +
                plt_theme,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg",
                           file,
                           width = 2400,
                           height = 1800)
          } else {
            ggsave(
              file = file,
              plot =  ordinaryAcuityFoveal()[[2]] +
                plt_theme,
              device = svg,
              width = 8,
              height = 6,
            )
          }
        }
      )
    output$downloadOrdinaryFovealCrowdingFontPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "ordinary-reading-vs-foveal-crowding-by-font.",
            downloadFileType()
          )
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            ggsave(
              "tmp.svg",
              plot = ordinaryCrowdingPlots()[[2]] + plt_theme,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg", file,
                           width = 1800)
          } else {
            ggsave(
              file = file,
              plot =  ordinaryCrowdingPlots()[[2]] + plt_theme,
              device = svg,
              width = 6,
            )
          }
        }
      )
    
    
    output$downloadOrdinaryFovealCrowdingGradePlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "ordinary-reading-vs-foveal-crowding-by-grade.",
            downloadFileType()
          )
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            base_plot <- ordinaryCrowdingPlots()[[4]] + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              "tmp.svg",
              plot = plot_with_title,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg", file,
                           width = 1800)
          } else {
            base_plot <- ordinaryCrowdingPlots()[[4]] + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              file = file,
              plot = plot_with_title,
              device = svg,
              width = 6,
            )
          }
        }
      )
    
    # Font-aggregated plot download handlers
    output$downloadFontAggregatedReadingRsvpCrowdingPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "font-aggregated-reading-rsvp-vs-peripheral-crowding.",
            downloadFileType()
          )
        },
        content = function(file) {
          plot <- fontAggregatedReadingRsvpCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (downloadFileType() == "png") {
              ggsave(
                "tmp.svg",
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = svglite,
                limitsize = FALSE
              )
              rsvg::rsvg_png("tmp.svg",
                             file,
                             width = 2400,
                             height = 1800)
            } else {
              ggsave(
                file = file,
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = ifelse(
                  downloadFileType() == "svg",
                  svglite::svglite,
                  downloadFileType()
                ),
                limitsize = FALSE
              )
            }
          }
        }
      )
    
    output$downloadFontAggregatedOrdinaryReadingCrowdingPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "font-aggregated-ordinary-reading-vs-peripheral-crowding.",
            downloadFileType()
          )
        },
        content = function(file) {
          plot <- fontAggregatedOrdinaryReadingCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (downloadFileType() == "png") {
              ggsave(
                "tmp.svg",
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = svglite,
                limitsize = FALSE
              )
              rsvg::rsvg_png("tmp.svg",
                             file,
                             width = 2400,
                             height = 1800)
            } else {
              ggsave(
                file = file,
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = ifelse(
                  downloadFileType() == "svg",
                  svglite::svglite,
                  downloadFileType()
                ),
                limitsize = FALSE
              )
            }
          }
        }
      )
    
    output$downloadFontAggregatedRsvpCrowdingPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "font-aggregated-rsvp-vs-peripheral-crowding.",
            downloadFileType()
          )
        },
        content = function(file) {
          plot <- fontAggregatedRsvpCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (downloadFileType() == "png") {
              ggsave(
                "tmp.svg",
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = svglite,
                limitsize = FALSE
              )
              rsvg::rsvg_png("tmp.svg",
                             file,
                             width = 2400,
                             height = 1800)
            } else {
              ggsave(
                file = file,
                plot = plot_with_title,
                width = 8,
                height = 6,
                unit = "in",
                device = ifelse(
                  downloadFileType() == "svg",
                  svglite::svglite,
                  downloadFileType()
                ),
                limitsize = FALSE
              )
            }
          }
        }
      )
    
    output$downloadOrdinaryPeripheralCrowdingFontPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "ordinary-reading-vs-peripheral-crowding-by-font.",
            downloadFileType()
          )
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            base_plot <- ordinaryCrowdingPlots()[[1]] + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              "tmp.svg",
              plot = plot_with_title,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg",
                           file,
                           width = 2400,
                           height = 1800)
          } else {
            base_plot <- ordinaryCrowdingPlots()[[1]] + plt_theme
            plot_with_title <- add_experiment_title(base_plot, experiment_names())
            ggsave(
              file = file,
              plot = plot_with_title,
              device = svg,
              width = 8,
              height = 6,
            )
          }
        }
      )
    
    output$downloadOrdinaryPeripheralCrowdingGradePlot <-
      downloadHandler(
        filename = function() {
          paste0(
            get_short_experiment_name(experiment_names()),
            "ordinary-reading-vs-peripheral-crowding-by-grade.",
            downloadFileType()
          )
        },
        content = function(file) {
          if (downloadFileType() == "png") {
            ggsave(
              "tmp.svg",
              plot = ordinaryCrowdingPlots()[[3]] +
                plt_theme,
              width = 8,
              height = 6,
              device = svglite,
              limitsize = FALSE
            )
            rsvg::rsvg_png("tmp.svg",
                           file,
                           width = 2400,
                           height = 1800)
          } else {
            ggsave(
              file = file,
              plot =  ordinaryCrowdingPlots()[[3]] +
                plt_theme,
              device = svg,
              width = 8,
              height = 6,
            )
          }
        }
      )
    
    output$downloadReadingRepeatedGradePlot <- downloadHandler(
      filename =  function() {
        paste0(
          experiment_names(),
          "reading-vs-repeated-letter-crowding-by-grade.",
          downloadFileType()
        )
      },
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot = readingRepeatedPlots()[[2]] +
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2400,
                         height = 1800)
        } else {
          ggsave(
            file = file,
            plot = readingRepeatedPlots()[[2]] +
              plt_theme,
            device = svg,
            width = 8,
            height = 6
          )
        }
      }
    )
    
    output$downloadCorrMatrixPlot <- downloadHandler(
      filename = function() paste0(get_short_experiment_name(experiment_names()),
                        'correlation-matrix.',
                        downloadFileType()),
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave(
            "tmp.svg",
            plot =  corrMatrix()$plot,
            width = corrMatrix()$width,
            height = corrMatrix()$height,
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
            plot =  corrMatrix()$plot,
            width = corrMatrix()$width,
            height = corrMatrix()$height,
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
    
    output$downloadNMatrixPlot <- downloadHandler(
      filename = function() paste0(get_short_experiment_name(experiment_names()), "n-matrix.", downloadFileType()),
      content = function(file) {
        if (downloadFileType() == "png") {
          ggsave("tmp.svg",
                 plot   = corrMatrix()$n_plot,
                 width  = corrMatrix()$width,
                 height = corrMatrix()$height,
                 unit   = "in",
                 limitsize = FALSE,
                 device = svglite)
          rsvg::rsvg_png("tmp.svg", file,
                         width  = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot       = corrMatrix()$n_plot,
            width      = corrMatrix()$width,
            height     = corrMatrix()$height,
            unit       = "in",
            limitsize  = FALSE,
            device     = if (downloadFileType()=="svg") svglite::svglite else downloadFileType()
          )
        }
      }
    )
    


})
