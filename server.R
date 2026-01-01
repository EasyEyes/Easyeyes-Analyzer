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

#### server code ####

shinyServer(function(input, output, session) {
  observeEvent(input$file_click,
               {
                 shinyalert(
                   title = "Reading file(s) ...",
                   size = "s",
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
  
  
  
  
  #### formSpree ####
  formSpreeTable <-
    reactive(monitorFormSpree(input$listFontParameters))
  
  output$formSpreeDashboard <- renderDataTable({
    datatable(
      formSpreeTable(),
      extensions = 'FixedHeader',
      class = list(stripe = FALSE),
      selection = 'none',
      filter = "top",
      options = list(
        autoWidth = TRUE,
        fixedHeader = TRUE,
        pageLength = 150,
        paging = TRUE,
        # dom = 'lt',
        columnDefs = list(list(
          visible = FALSE, targets = c(0, ncol(formSpreeTable()))
        ))
      )
    ) %>%
      formatStyle(names(formSpreeTable()),
                  'hl',
                  backgroundColor = styleEqual(c(T, F), c('yellow', 'white')))
  })
  
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
    check <- check_file_names(input$file)
    if (is.null(check)) {
      t <- read_files(input$file)
      print('done read_files')
      return(t)
    } else {
      closeAlert()
      print('INVALID FILE NAME(S)')
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
  
  #### reactive dataframes ####
  
  summary_table <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    generate_summary_table(
      files()$data_list,
      files()$stairs,
      files()$pretest,
      files()$prolific)

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
    df_list <- generate_threshold(
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
    return(
      df_list
    )
  })
  
  distanceCalibration <- reactive({
    get_distance_calibration(files()$data_list, minRulerCm())
  }) %>% bindCache(input$file$datapath, minRulerCm())
  
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
  
  durationCorrMatrix <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    get_duration_corr(files()$data_list, conditionNames())
  })
  
  #### duration data ####
  
  durationData <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    get_duration_data(files()$data_list, conditionNames())
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
  
  ##### SOUND CALIBRATION ####
  
  # OPTIMIZATION: Parse JSON once, reuse across all sound calibration reactives
  # This eliminates 8+ redundant JSON parsing operations
  parsed_json <- reactive({
    if (is.null(input$fileJSON)) return(NULL)
    file_list <- input$fileJSON$data
    fromJSON(file_list[1], simplifyDataFrame = F)
  })
  
  irPlots <- reactive({
    req(parsed_json())
    get_ir_plots(parsed_json(), sound_data())
  })
  
  sound_data <- reactive({
    req(parsed_json())
    preprocessJSON(parsed_json())
  })
  
  iir <- reactive({
    req(parsed_json())
    read_iir_JSON(parsed_json())
  })
  
  iirPlots <- reactive({
    return(get_iir_plot(iir(), sound_data()))
  })
  
  # record_freq_system_plot <- reactive({
  #   plot_record_freq_system(sound_data())
  # })
  
  record_freq_component_plot <- reactive({
    plot_record_freq_component(sound_data())
  })
  
  componentIIRPlots <- reactive({
    req(parsed_json())
    plotComponentIIR(parsed_json(), sound_data())
  })
  
  cumSumPowerPlot <- reactive({
    req(parsed_json())
    getCumSumPowerPlot(parsed_json())
  })
  
  componentIR_PSD_plot <- reactive({
    req(parsed_json())
    plotComponentIRPSD(parsed_json(), sound_data())
  })
  
  recording_variation <- reactive({
    req(parsed_json())
    plot_power_variations(parsed_json(), sound_data())
  })
  
  volume_power_variation <- reactive({
    req(parsed_json())
    plot_volume_power_variations(parsed_json(), sound_data())
  })
  
  subtitleOne <- reactive({
    inputParameters <- sound_data()[[12]]
    subtitleOne <- paste0(
      "MLS: ",
      round(inputParameters$calibrateSoundBurstDb, 3),
      " dB, ampl. ",
      round(10 ^ (
        inputParameters$calibrateSoundBurstDb / 20
      ), 3),
      ", ",
      inputParameters$calibrateSoundBurstSec,
      " s, ",
      inputParameters$calibrateSoundBurstRepeats,
      "×",
      ", ",
      inputParameters$calibrateSoundHz,
      " Hz (",
      inputParameters$fs2,
      " Hz)"
    )
    
  })
  
  subtitleTwo <- reactive({
    inputParameters <- sound_data()[[12]]
    return(list(
      # system =  paste0(
      #   "Filtered MLS: ",
      #   format(
      #     round(
      #       inputParameters$calibrateSoundAttenuationComponentDb,
      #       1
      #     ),
      #     nsmall = 1
      #   ),
      #   " dB, ampl. ",
      #   round(inputParameters$filteredMLSMaxAbsSystem, 1),
      #   ", ",
      #   inputParameters$calibrateSoundMinHz,
      #   " – ",
      #   inputParameters$fMaxHzSystem,
      #   " Hz, ",
      #   format(round(
      #     inputParameters$attenuatorDBSystem,
      #     1
      #   ),
      #   nsmall = 1),
      #   " dB atten."
      # ),
      component = paste0(
        "Filtered MLS: ",
        (
          round(inputParameters$calibrateSoundAttenuationComponentDb, 1)
        ),
        " dB, ampl. ",
        round(inputParameters$filteredMLSMaxAbsComponent, 1),
        ", ",
        inputParameters$calibrateSoundMinHz,
        " – ",
        inputParameters$fMaxHzComponent,
        " Hz, ",
        format(round(
          inputParameters$attenuatorDBComponent,
          1
        ),
        nsmall = 1),
        " dB atten."
      )
    ))
  })
  
  subtitleThree <- reactive({
    inputParameters <- sound_data()[[12]]
    return(list(
      system = paste0(
        "IR: ",
        inputParameters$calibrateSoundIRSec,
        " s, IIR: ",
        inputParameters$calibrateSoundIIRSec,
        " s, ",
        inputParameters$calibrateSoundMinHz,
        " – ",
        inputParameters$fMaxHzSystem,
        " Hz"
      ),
      component = paste0(
        "IR: ",
        inputParameters$calibrateSoundIRSec,
        " s, IIR: ",
        inputParameters$calibrateSoundIIRSec,
        " s, ",
        inputParameters$calibrateSoundMinHz,
        " – ",
        inputParameters$fMaxHzComponent,
        " Hz"
      )
    ))
  })
  
  output$jsonUploaded <- reactive({
    print("Json File uplaoded")
    print(!is.null(input$fileJSON))
    return(!is.null(input$fileJSON))
  })
  
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
    print('inside agePlots')

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

    print('done age plots')
    return(list(plotList = l, fileNames = fileNames))
  })

  histograms <- reactive({
  if (is.null(files())) {
    return(list(plotList = list(), fileNames = list()))
  }
  print('inside histograms')

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

  print('before appending')
  lists <- append_hist_list(files()$data_list, l, fileNames, experiment_names())

  list(
    plotList  = lists$plotList,
    fileNames = lists$fileNames
  )
})

  histogramsQuality <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print('Generating quality histograms')
    l <- list()
    fileNames <- list()
    
    lists <- append_hist_quality(files()$data_list, l, fileNames, conditionNames())
    lists <- add_questsd_hist(df_list()$quest, lists)
    
    minDegHist <- minDegPlots()$hist_quality
    return(list(
      plotList = c(lists$plotList, minDegHist$plotList),
      fileNames = c(lists$fileNames, minDegHist$fileNames)
    ))
  })
  
  timingHistograms <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print('Generating timing histograms')
    l <- list()
    fileNames <- list()
    
    lists <-
      append_hist_time(files()$data_list, l, fileNames, conditionNames())
    return(lists)
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
    print('inside violin plots')
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
    print('inside font comparison plots')
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
  
  #### dotPlots ####
  dotPlots <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    
    l         <- list()
    fileNames <- list()
    
    # SD histogram and distance-related dot plots go here with larger sizing
    bs_vd <- bs_vd_hist(distanceCalibration())
    dist_plots <- plot_distance(distanceCalibration(), calibrateTrackDistanceCheckLengthSDLogAllowed())
    
    # Build static_calls list, starting with calibrated_over_median histogram first
    static_calls <- list()
    
    # Add calibrated_over_median histogram first if available
    if (!is.null(dist_plots$calibrated_over_median_hist) &&
        !is.null(dist_plots$calibrated_over_median_hist$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = dist_plots$calibrated_over_median_hist$plot,
        height = dist_plots$calibrated_over_median_hist$height,
        fname = 'factorVpxCm-over-median-histogram'
      )
    }
    
    # Add raw factorVpxCm histogram if available (2nd)
    if (!is.null(dist_plots$raw_factorVpxCm_hist) &&
        !is.null(dist_plots$raw_factorVpxCm_hist$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = dist_plots$raw_factorVpxCm_hist$plot,
        height = dist_plots$raw_factorVpxCm_hist$height,
        fname = 'raw-factorVpxCm-over-remeasured-histogram'
      )
    }
    
    # Add fVpx/widthVpx histogram if available (3rd)
    if (!is.null(dist_plots$fvpx_over_width_hist) &&
        !is.null(dist_plots$fvpx_over_width_hist$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = dist_plots$fvpx_over_width_hist$plot,
        height = dist_plots$fvpx_over_width_hist$height,
        fname = 'fvpx-over-horizontalVpx-histogram'
      )
    }
    
    # Add raw pxPerCm histogram if available
    if (!is.null(dist_plots$raw_pxPerCm_hist) &&
        !is.null(dist_plots$raw_pxPerCm_hist$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = dist_plots$raw_pxPerCm_hist$plot,
        height = dist_plots$raw_pxPerCm_hist$height,
        fname = 'raw-pxPerCm-over-remeasured-histogram'
      )
    }
    
    # Add raw objectMeasuredCm histogram if available
    if (!is.null(dist_plots$raw_objectMeasuredCm_hist) &&
        !is.null(dist_plots$raw_objectMeasuredCm_hist$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = dist_plots$raw_objectMeasuredCm_hist$plot,
        height = dist_plots$raw_objectMeasuredCm_hist$height,
        fname = 'raw-objectMeasuredCm-over-median-histogram'
      )
    }
    
    # Add other always-present plots
    static_calls[[length(static_calls) + 1]] <- list(plot = sizeCheckPlot()$sd_hist$plot, height = sizeCheckPlot()$sd_hist$height, fname = 'sd-log-density-histogram')
    static_calls[[length(static_calls) + 1]] <- list(plot = sizeCheckPlot()$ruler_hist$plot, height = sizeCheckPlot()$ruler_hist$height, fname = 'ruler-length-cm-dotplot')
    static_calls[[length(static_calls) + 1]] <- list(plot = objectCm_hist(df_list()$participant_info, distanceCalibration())$plot, height = objectCm_hist(df_list()$participant_info, distanceCalibration())$height, fname = 'object-length-cm-dotplot')
    
    # Conditionally add blindspot plots if they exist
    if (!is.null(bs_vd$mean_plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = bs_vd$mean_plot$plot, 
        height = bs_vd$mean_plot$height, 
        fname = 'blindspot-viewing-distance-mean-dotplot'
      )
    }
    
    if (!is.null(bs_vd$sd_plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = bs_vd$sd_plot$plot, 
        height = bs_vd$sd_plot$height, 
        fname = 'blindspot-viewing-distance-sd-dotplot'
      )
    }
    
    heights <- list()
    for (call in static_calls) {
      res <- append_plot_list(l, 
                              fileNames,
                              add_experiment_title(call$plot, experiment_names()), 
                              call$fname,
                              height = call$height,
                              heights = heights)
      l <- res$plotList
      fileNames <- res$fileNames
      heights <- res$heights
    }
    
    return(list(
      plotList = l,
      fileNames = fileNames,
      heights = heights
    ))
  }) %>% bindCache(input$file$datapath, minRulerCm(), calibrateTrackDistanceCheckLengthSDLogAllowed())
  
  # Progressive rendering controls for heavy grids
  dotRenderCount  <- reactiveVal(0)           # for output$dotPlots
  histRenderCount <- reactiveVal(0)          # for output$histograms
  plotsRenderCount <- reactiveVal(0)
  qualityHistRenderCount <- reactiveVal(0)
  timingHistRenderCount <- reactiveVal(0)
  scatterRenderCount <- reactiveVal(0)       # for output$scatters
  distanceScatterRenderCount <- reactiveVal(0) # for output$distanceScatters
  
  # Reset counters when data changes
  observeEvent(dotPlots(),     { dotRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(histograms(),   { histRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(agePlots(),     { plotsRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(histogramsQuality(), { qualityHistRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(timingHistograms(),  { timingHistRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(scatterDiagrams(),   { scatterRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(scatterDistance(),   { distanceScatterRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(files(), {
    dotRenderCount(0)
    histRenderCount(0)
    plotsRenderCount(0)
    qualityHistRenderCount(0)
    timingHistRenderCount(0)
    scatterRenderCount(0)
    distanceScatterRenderCount(0)
  }, ignoreInit = TRUE)
  
  # Incrementally allow more plots to render
  observe({
    total <- length(dotPlots()$plotList)
    current <- dotRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      dotRenderCount(current + 1)
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
    total <- length(agePlots()$plotList)
    current <- plotsRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      plotsRenderCount(current + 1)
    }
  })
  
  observe({
    total <- length(histogramsQuality()$plotList)
    current <- qualityHistRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      qualityHistRenderCount(current + 1)
    }
  })
  
  observe({
    total <- length(timingHistograms()$plotList)
    current <- timingHistRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      timingHistRenderCount(current + 1)
    }
  })
  
  observe({
    plots_obj <- scatterDiagrams()
    if (!is.list(plots_obj) || is.null(plots_obj$plotList)) return()
    total <- length(plots_obj$plotList)
    current <- scatterRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      scatterRenderCount(current + 1)
    }
  })
  
  observe({
    total <- length(scatterDistance()$plotList)
    current <- distanceScatterRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      distanceScatterRenderCount(current + 1)
    }
  })
  
  #### scatterDiagrams ####
  sizeCheckPlot <- reactive({
    plot_sizeCheck(distanceCalibration(),calibrateTrackDistanceCheckLengthSDLogAllowed())
  }) %>% bindCache(input$file$datapath, minRulerCm(), calibrateTrackDistanceCheckLengthSDLogAllowed())
  
  scatterDistance <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    print('inside distance scatter plots')
    l <- list()
    fileNames <- list()

    # Distance-specific plots
    distance_production_plots <- plot_distance_production(distanceCalibration(), df_list()$participant_info, calibrateTrackDistanceCheckLengthSDLogAllowed())
    distance_plots <- plot_distance(distanceCalibration(), calibrateTrackDistanceCheckLengthSDLogAllowed())
    ipd_plots <- plot_ipd_vs_eyeToFootCm(distanceCalibration())
    
    plot_calls <- list(
      list(plot = sizeCheckPlot()$density_vs_length$plot, height = sizeCheckPlot()$density_vs_length$height, fname = 'SizeCheckEstimatedPxPerCm-vs-SizeCheckRequestedCm-plot'),
      list(plot = sizeCheckPlot()$density_ratio_vs_sd$plot, height = sizeCheckPlot()$density_ratio_vs_sd$height, fname = 'ratio-vs-sdLogDensity-plot'),
      list(plot = distance_plots$credit_card_vs_requested$plot, height = distance_plots$credit_card_vs_requested$height, fname = 'calibrateTrackDistanceMeasuredCm-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_plots$credit_card_fraction$plot, height = distance_plots$credit_card_fraction$height, fname = 'calibrateTrackDistanceMeasuredCm-fraction-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_production_plots$raw_production_vs_requested$plot, height = distance_production_plots$raw_production_vs_requested$height, fname = 'calibrateTrackDistanceIndividualProduction-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_production_plots$individual_production_fraction$plot, height = distance_production_plots$individual_production_fraction$height, fname = 'calibrateTrackDistanceIndividualProduction-fraction-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_production_plots$error_vs_object_size$plot, height = distance_production_plots$error_vs_object_size$height, fname = 'error-vs-object-size-plot'),
      list(plot = distance_production_plots$error_vs_blindspot_diameter$plot, height = distance_production_plots$error_vs_blindspot_diameter$height, fname = 'error-vs-blindspot-diameter-plot'),
      list(plot = distance_plots$ipd_vs_requested$plot, height = distance_plots$ipd_vs_requested$height, fname = 'calibrateTrackDistanceIpdVpx-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_plots$ipd_product_vs_requested$plot, height = distance_plots$ipd_product_vs_requested$height, fname = 'ipd-product-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = distance_plots$fvpx_over_width_scatter$plot, height = distance_plots$fvpx_over_width_scatter$height, fname = 'fvpx-over-widthVpx-vs-camera-width-plot'),
      list(plot = distance_plots$calibrated_vs_mean$plot, height = distance_plots$calibrated_vs_mean$height, fname = 'calibrated-vs-mean-factorVpxCm-plot'),
      list(plot = distance_plots$calibrated_over_mean_vs_spot$plot, height = distance_plots$calibrated_over_mean_vs_spot$height, fname = 'calibrated-over-mean-factorVpxCm-vs-spot-diameter-plot'),
      list(plot = distance_plots$eye_feet_position$plot, height = distance_plots$eye_feet_position$height, fname = 'eye-feet-position-vs-distance-error-plot'),
      list(plot = distance_plots$foot_position_calibration$plot, height = distance_plots$foot_position_calibration$height, fname = 'foot-position-during-calibration-plot'),
      list(plot = ipd_plots$ipd_vs_eyeToFootCm$plot, height = ipd_plots$ipd_vs_eyeToFootCm$height, fname = 'ipd-vs-eyeToFootCm-plot'),
      list(plot = ipd_plots$ipdVpx_times_eyeToFootCm_vs_eyeToFootCm$plot, height = ipd_plots$ipdVpx_times_eyeToFootCm_vs_eyeToFootCm$height, fname = 'ipdVpx-times-eyeToFootCm-vs-eyeToFootCm-plot')

    )

    heights <- list()
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        plot <- plot + scale_color_manual(values = colorPalette)
        plot <- add_experiment_title(plot, experiment_names())
      }
      res <- append_plot_list(l, fileNames, plot, call$fname, height = call$height, heights = heights)
      l <- res$plotList
      fileNames <- res$fileNames
      heights <- res$heights
    }

    return(list(
      plotList = l,
      fileNames = fileNames,
      heights = heights
    ))
  }) %>% bindCache(input$file$datapath, minRulerCm(), calibrateTrackDistanceCheckLengthSDLogAllowed())

  scatterDiagrams <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    print('inside scatter plots')
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
  
  scatterQuality <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    print('inside scatter quality plots')
    l <- list()
    fileNames <- list()
    
    # OPTIMIZATION: Compute expensive functions once, use results multiple times
    crowdingPlots <- plotCrowdingStaircasesVsQuestTrials(df_list(), files()$stairs)
    quest_diag <- get_quest_diag(df_list()$quest)
    quest_sd_trials <- get_quest_sd_vs_trials(df_list()$quest_all_thresholds)
    
    plot_calls <- list(
      list(plot = crowdingPlots$fovealPlot, fname = 'foveal-crowding-staircases-threshold-vs-questTrials'),
      list(plot = crowdingPlots$peripheralPlot, fname = 'peripheral-crowding-staircases-threshold-vs-questTrials'),
      list(plot = quest_diag$grade, fname = 'quest-sd-vs-mean-grade-diagram'),
      list(plot = quest_sd_trials, fname = 'quest-sd-vs-quest-trials-grade-diagram')
    )
    
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        plot <- plot + scale_color_manual(values = colorPalette)
        plot <- add_experiment_title(plot, experiment_names())
      }
      res <- append_plot_list(l, fileNames, plot, call$fname)
      l <- res$plotList
      fileNames <- res$fileNames
    }
    
    minDegPlot <- minDegPlots()$scatter_quality
    
    # Apply experiment title to minDegPlot plots
    minDegPlot_updated <- lapply(minDegPlot$plotList, function(plot) {
      if (!is.null(plot)) {
        plot <- plot + scale_color_manual(values = colorPalette)
        plot <- add_experiment_title(plot, experiment_names())
      }
      return(plot)
    })
    
    return(list(
      plotList = c(l, minDegPlot_updated),
      fileNames = c(fileNames, minDegPlot$fileNames)
    ))
  })
  
  scatterTime <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print("inside ScatterTime")
    l <- list()
    fileNames <- list()
    
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
  
  scatterTimeParticipant <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print("inside ScatterTimeParticipant")
    l <- list()
    fileNames <- list()
    
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
  
  gradePlots <- reactive({
    if (is.null(files()) | is.null(df_list())) {
      return(histograms <NULL)
    }
    plot_rsvp_crowding_acuity(df_list())
  })
  
  duration_lateness_hist <- reactive({
    if (is.null(durationData())) {
      return(NULL)
    }
    get_histogram_duration_lateness(durationData())
  })
  
  cameraResolutionXYTable <- reactive({
    get_cameraResolutionXY(files()$data_list)
  })
  
  mergedParticipantDistanceTable <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(tibble())
    }
    get_merged_participant_distance_info(distanceCalibration(), df_list()$participant_info)
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
  
  output$isDuration <- reactive({
    return(nrow(durationData()) > 0)
  })
  
  output$isCorrMatrixAvailable <- reactive({
    return(!is.null(corrMatrix()))
  })
  outputOptions(output, 'isCorrMatrixAvailable', suspendWhenHidden = FALSE)
  
  output$isDurationCorrMatrixAvailable <- reactive({
    return(!is.null(durationCorrMatrix()))
  })
  outputOptions(output, 'isDurationCorrMatrixAvailable', suspendWhenHidden = FALSE)
  
  output$fileUploaded <- reactive({
    return(nrow(files()$pretest > 0))
  })
  
  output$questData <- reactive({
    if ('quest' %in% names(df_list())) {
      return(nrow(df_list()$quest > 0))
    }
    return(FALSE)
  })
  
  output$IsCameraResolutionXYTable <- reactive({
    return(nrow(cameraResolutionXYTable()) > 0)
  })
  outputOptions(output, 'IsCameraResolutionXYTable', suspendWhenHidden = FALSE)
  
  output$IsMergedParticipantDistanceTable <- reactive({
    return(nrow(mergedParticipantDistanceTable()) > 0)
  })
  outputOptions(output, 'IsMergedParticipantDistanceTable', suspendWhenHidden = FALSE)
  
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
  outputOptions(output, 'isDuration', suspendWhenHidden = FALSE)
  outputOptions(output, 'jsonUploaded', suspendWhenHidden = FALSE)
  
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
  #### cameraResolutionXYTable ####
  
  output$cameraResolutionXYTable <- renderTable({
    cameraResolutionXYTable()
  })
  
  #### Merged Participant Distance Table ####
  
  output$mergedParticipantDistanceTable <- DT::renderDataTable({
    table_data <- mergedParticipantDistanceTable()
    
    col_names <- names(table_data)
    
  
    named_widths <- c(
      "PavloviaParticipantID" = "100px",
      "Object" = "200px",      
      "Comment" = "500px"    
    )
    

    default_width <- "60px"
    
    # Build columnDefs list
    column_defs <- lapply(seq_along(col_names), function(i) {
      col_name <- col_names[i]
      width <- if (col_name %in% names(named_widths)) named_widths[[col_name]] else default_width
      list(width = width, targets = i - 1L)
    })
    # Compute indices (1-based for nth-child) of Object/Comment columns if present
    object_idx <- if ("Object" %in% names(table_data)) which(names(table_data) == "Object")[1] else NA_integer_
    comment_idx <- if ("Comment" %in% names(table_data)) which(names(table_data) == "Comment")[1] else NA_integer_
    # Build initComplete JS that only aligns existing columns
    js_align <- paste0(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '13px', 'padding': '2px 4px'});",
      "$(this.api().table().body()).css({'font-size': '12px', 'padding': '2px 4px'});",
      "$('td').css({'text-align': 'center'});",
      "$('th').css({'text-align': 'center'});",
      if (!is.na(object_idx)) sprintf("$('td:nth-child(%d)').css('text-align','left');$('th:nth-child(%d)').css('text-align','left');", object_idx, object_idx) else "",
      if (!is.na(comment_idx)) sprintf("$('td:nth-child(%d)').css('text-align','left');$('th:nth-child(%d)').css('text-align','left');", comment_idx, comment_idx) else "",
      "}"
    )
    datatable(
      table_data,
              class = list(stripe = FALSE, 'compact'),
              selection = 'none',
              extensions = 'FixedHeader',
              filter = "top",
              escape = FALSE,
              options = list(
                autoWidth = TRUE,
                paging = FALSE,
                scrollX = TRUE,
                fixedHeader = TRUE,
        columnDefs = column_defs,
        initComplete = JS(js_align)
      ),
      rownames = FALSE
    )
  })
  
  output$downloadParticipantDistanceInfo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "ParticipantDistanceInfo.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "ParticipantDistanceInfo.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(mergedParticipantDistanceTable(), file = filename)
    }
  )
  
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
    plot_duraction_sec(durationData())
  })
  
  latenessPlot <- reactive({
    plot_Lateness_sec(durationData())
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
        print(e)
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
        print(e)
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
  output$fontAggregatedReadingRsvpCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedReadingRsvpCrowding()
      if (!is.null(plot)) {
        outfile <- tempfile(fileext = '.svg')
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
        
        list(src = outfile,
             contenttype = 'image/svg+xml')
      } else {
        NULL
      }
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedReadingRsvpCrowdingPlot", experiment_names(), "Font-aggregated reading vs peripheral crowding")
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedOrdinaryReadingCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedOrdinaryReadingCrowding()
      if (!is.null(plot)) {
        outfile <- tempfile(fileext = '.svg')
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
        
        list(src = outfile,
             contenttype = 'image/svg+xml')
      } else {
        NULL
      }
    }, error = function(e) {
      handle_plot_error(e, "fontAggregatedOrdinaryReadingCrowdingPlot", experiment_names(), "Font-aggregated ordinary reading vs peripheral crowding")
    })
  }, deleteFile = TRUE)
  
  output$fontAggregatedRsvpCrowdingPlot <- renderImage({
    tryCatch({
      plot <- fontAggregatedRsvpCrowding()
      if (!is.null(plot)) {
        outfile <- tempfile(fileext = '.svg')
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
        
        list(src = outfile,
             contenttype = 'image/svg+xml')
      } else {
        NULL
      }
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
            ggsave(
              file = tmp_svg,
              plot = plotList[[i]] + plt_theme,
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
            paste0(get_short_experiment_name(experiment_names()), base, ".", input$fileType)
          },
          content = function(file) {
            if (i > length(plotList) || is.null(plotList[[i]])) return(invisible(NULL))
            plot <- plotList[[i]] + plt_theme
            savePlot(
              plot = plot,
              filename = file,
              fileType = input$fileType,
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
        ggsave(
          file = tmp_svg,
          plot =  plots[[jj]] + hist_theme,
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
          filename = paste0(
            get_short_experiment_name(experiment_names()),
            files[[jj]],
            ".", input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
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
                device   = if (input$fileType == "svg") svglite::svglite else input$fileType
              )
            }
          }
        )
      })
    }
    
    return(out)
  })

  output$dotPlots <- renderUI({
    out    <- list()
    plots   <- dotPlots()$plotList
    files   <- dotPlots()$fileNames
    heights <- dotPlots()$heights
    n      <- length(plots)
    
    if (n == 0) {
      return(out)
    }
    
    # Use 50% width for dot plots (2 per row)
    nPerRow <- 2
    
    for (i in seq(1, n, by = nPerRow)) {
      idx <- i:min(i + nPerRow - 1, n)
      
      # --- row of plots ---
      plot_cells <- lapply(idx, function(j) {
        shinycssloaders::withSpinner(
          imageOutput(paste0("dot", j),
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
          cellWidths = rep("50%", nPerRow),
          style      = "overflow-x: hidden; white-space: nowrap;"
        ),
        plot_cells
      ))
      
      # --- row of download buttons ---
      dl_cells <- lapply(idx, function(j) {
        downloadButton(paste0("downloadDot", j), "Download")
      })
      if (length(dl_cells) < nPerRow)
        dl_cells <- c(dl_cells, rep("", nPerRow - length(dl_cells)))
      
      out[[length(out) + 1]] <- do.call(splitLayout, c(
        list(
          cellWidths = rep("50%", nPerRow),
          style      = "overflow-x: hidden; white-space: nowrap;"
        ),
        dl_cells
      ))
    }
    
    # register each renderImage & downloadHandler
    for (j in seq_along(plots)) {
      local({
        jj <- j
       
      output[[paste0("dot", jj)]] <- renderImage({
          req(jj <= dotRenderCount())
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            height_in <- if (!is.null(heights) && length(heights) >= jj && !is.null(heights[[jj]])) heights[[jj]] else 4
            ggsave(
              file = tmp_svg,
              plot =  plots[[jj]],
              device = svglite,
              width = 6,
              height = height_in,
              unit = 'in',
              limitsize = FALSE
            )
            disp_w <- 600
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((height_in / 6) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
          }, error = function(e) {
            error_plot <- ggplot() +
              annotate(
                "text",
                x = 0.5,
                y = 0.5,
                label = paste("Error:", e$message),
                hjust = 0.5,
                vjust = 0.5
              ) +
              xlim(0, 1) +
              ylim(0, 1) +
              theme_void()
            
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
              alt = paste0("Error in ", files[[jj]])
            )
          })
        
      }, deleteFile = TRUE)
      outputOptions(output, paste0("dot", jj), suspendWhenHidden = TRUE)
        
        output[[paste0("downloadDot", jj)]] <- downloadHandler(
          filename = paste0(
            get_short_experiment_name(experiment_names()),
            files[[jj]],
            ".", input$fileType
          ),
          content = function(file) {
            # Match on-screen sizing: save SVG then rasterize at 2x scale
            height_in <- if (!is.null(heights) && length(heights) >= jj && !is.null(heights[[jj]])) heights[[jj]] else 4
            if (tolower(input$fileType) == "png") {
              tmp_svg <- tempfile(fileext = ".svg")
              ggsave(
                file = tmp_svg,
                plot = plots[[jj]],
                device = svglite,
                width = 6,
                height = height_in,
                units = "in",
                limitsize = FALSE
              )
              png_w <- 1200  # 6in * 200dpi * scale(1)
              png_h <- round((height_in / 6) * png_w)
              rsvg::rsvg_png(tmp_svg, file, width = png_w, height = png_h)
            } else {
              ggsave(
                file,
                plot   = plots[[jj]],
                width  = 6,
                height = height_in,
                units  = "in",
                limitsize = FALSE,
                device   = if (tolower(input$fileType) == "svg") svglite::svglite else input$fileType
              )
            }
          }
        )
      })
    }
    
    return(out)
  })

  output$qualityHistograms <- renderUI({
    out <- list()
    i <- 1
    
    while (i <= length(histogramsQuality()$plotList) - 3) {
      # Create a row with 4 histograms
      out[[i]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          shinycssloaders::withSpinner(plotOutput(
            paste0("qualityHist", i),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("qualityHist", i + 1),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("qualityHist", i + 2),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("qualityHist", i + 3),
            width = "100%",
            height = "100%"
          ),
          type = 4)
        )
      # Create a row with 4 download buttons
      out[[i + 1]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          downloadButton(paste0("downloadQualityHist", i), 'Download'),
          downloadButton(paste0("downloadQualityHist", i + 1), 'Download'),
          downloadButton(paste0("downloadQualityHist", i + 2), 'Download'),
          downloadButton(paste0("downloadQualityHist", i + 3), 'Download')
        )
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(histogramsQuality()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(
          paste0("qualityHist", i + j - 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(paste0("downloadQualityHist", i + j - 1), 'Download')
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
    for (j in seq_along(histogramsQuality()$plotList)) {
      local({
        ii <- j
        output[[paste0("qualityHist", ii)]] <- renderImage({
          req(ii <= qualityHistRenderCount())
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            ggsave(
              file = tmp_svg,
              plot = histogramsQuality()$plotList[[ii]] + hist_theme,
              device = svglite,
              width = 4,
              height = 3.5,
              unit = 'in',
              limitsize = FALSE
            )
            # Fit image to the container width to avoid horizontal scrollbars
            disp_w <- session$clientData[[paste0("output_", "qualityHist", ii, "_width")]]
            if (is.null(disp_w) || is.na(disp_w) || disp_w <= 0) disp_w <- 560
            scale <- 2
            png_w <- round(disp_w * scale)
            png_h <- round((3.5 / 4) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
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
              labs(subtitle=histogramsQuality()$fileNames[[ii]])
            
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
              alt = paste0("Error in ", histogramsQuality()$fileNames[[ii]])
            )
          })
          
        }, deleteFile = TRUE)
        outputOptions(output, paste0("qualityHist", ii), suspendWhenHidden = TRUE)
        
        output[[paste0("downloadQualityHist", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              histogramsQuality()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = histogramsQuality()$plotList[[ii]] + hist_theme,
                  unit = "in",
                  width = 4,
                  height = 3.5,
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
                  plot = histogramsQuality()$plotList[[ii]] + hist_theme,
                  width = 4,
                  # Reduced width
                  height = 3.5,
                  # Reduced height
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  output$timingHistograms <- renderUI({
    out <- list()
    i <- 1
    
    while (i <= length(timingHistograms()$plotList) - 3) {
      # Create a row with 4 histograms
      out[[i]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          shinycssloaders::withSpinner(plotOutput(
            paste0("timingHist", i),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            paste0("timingHist", i + 1),
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
          downloadButton(paste0("downloadTimingHist", i), 'Download'),
          downloadButton(paste0("downloadTimingHist", i + 1), 'Download'),
          downloadButton(paste0("downloadTimingHist", i + 2), 'Download'),
          downloadButton(paste0("downloadTimingHist", i + 3), 'Download')
        )
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(timingHistograms()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(
          paste0("timingHist", i + j - 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(paste0("downloadTimingHist", i + j - 1), 'Download')
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
            ggsave(
              file = tmp_svg,
              plot = timingHistograms()$plotList[[ii]] + hist_theme,
              device = svglite,
              width = 4,
              height = 3.5,
              unit = 'in',
              limitsize = FALSE
            )
            # Fit image to the container width to avoid horizontal scrollbars
            disp_w <- session$clientData[[paste0("output_", "timingHist", ii, "_width")]]
            if (is.null(disp_w) || is.na(disp_w) || disp_w <= 0) disp_w <- 560
            scale <- 2
            png_w <- round(disp_w * scale)
            png_h <- round((3.5 / 4) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
          }, error = function(e) {
            message(e)
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
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
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
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
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
                input$fileType == "svg",
                svglite::svglite,
                input$fileType
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
            ggsave(
              file = tmp_svg,
              plot = scatterDiagrams()$plotList[[ii]] +
                plt_theme_scatter,
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
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterDiagrams()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    return(out)
  })
  
  #### distance scatter plots ####
  
  output$distanceScatters <- renderUI({
    out <- list()
    i = 1
    while (i <= length(scatterDistance()$plotList) - 1) {
      out[[i]] <-  splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("distanceScatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("distanceScatter", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadDistanceScatter", i), 'Download'),
        downloadButton(paste0("downloadDistanceScatter", i +
                                1), 'Download')
      )
      i = i + 2
    }
    if (i == length(scatterDistance()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("distanceScatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadDistanceScatter", i), 'Download'))
    }
    for (j in 1:length(scatterDistance()$plotList)) {
      local({
        ii <- j
        output[[paste0("distanceScatter", ii)]] <- renderImage({
          req(ii <= distanceScatterRenderCount())
          tryCatch({
            height_in <- scatterDistance()$heights[[ii]]
            tmp_svg <- tempfile(fileext = '.svg')
            ggsave(
              file = tmp_svg,
              plot = scatterDistance()$plotList[[ii]] +
                plt_theme_scatter,
              width = 7,
              height = height_in,
              unit = 'in',
              limitsize = F,
              device = svglite
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((height_in / 7) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))

          }, error = function(e) {
            print(e)
          handle_plot_error(e, paste0("distanceScatter", ii), experiment_names(), scatterDistance()$fileNames[[ii]])
          })
          
        }, deleteFile = TRUE)
        outputOptions(output, paste0("distanceScatter", ii), suspendWhenHidden = TRUE)
        output[[paste0("downloadDistanceScatter", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterDistance()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              height_in <- scatterDistance()$heights[[ii]]
              if (tolower(input$fileType) == "png") {
                tmp_svg <- tempfile(fileext = ".svg")
                ggsave(
                  file = tmp_svg,
                  plot = scatterDistance()$plotList[[ii]] + plt_theme_scatter,
                  width = 7,
                  height = height_in,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                png_w <- 1400  # 7in * 200dpi
                png_h <- round((height_in / 7) * png_w)
                rsvg::rsvg_png(tmp_svg, file, width = png_w, height = png_h)
              } else {
                ggsave(
                  file,
                  plot = scatterDistance()$plotList[[ii]] + plt_theme_scatter,
                  width = 7,
                  height = height_in,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    tolower(input$fileType) == "svg",
                    svglite::svglite,
                    input$fileType
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
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              violinPlots()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
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
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              fontComparisonPlots()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  output$qualityScatters <- renderUI({
    out <- list()
    i = 1
    while (i <= length(scatterQuality()$plotList) - 1) {
      out[[i]] <-  splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("qualityScatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("qualityScatter", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadQualityScatter", i), 'Download'),
        downloadButton(paste0("downloadQualityScatter", i +
                                1), 'Download')
      )
      i = i + 2
    }
    if (i == length(scatterQuality()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("qualityScatter", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadQualityScatter", i), 'Download'))
    }
    for (j in 1:length(scatterQuality()$plotList)) {
      local({
        ii <- j
        output[[paste0("qualityScatter", ii)]] <- renderImage({
          tryCatch({
            tmp_svg <- tempfile(fileext = '.svg')
            ggsave(
              file = tmp_svg,
              plot = scatterQuality()$plotList[[ii]] +
                plt_theme_scatter,
              width = 7,
              height = 6,
              unit = 'in',
              device = svglite,
              limitsize = FALSE
            )
            disp_w <- 700
            scale <- 2
            png_w <- disp_w * scale
            png_h <- round((6 / 7) * png_w)
            outfile <- tempfile(fileext = ".png")
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
            list(src = outfile, contenttype = 'image/png', width = disp_w, height = round(png_h / scale))
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
              labs(subtitle=scatterQuality()$fileNames[[ii]])
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
        output[[paste0("downloadQualityScatter", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterQuality()$fileNames[[ii]],
              ii,
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = scatterQuality()$plotList[[ii]] +
                    plt_theme_scatter +
                    scale_color_manual(values = colorPalette),
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png(tmp_svg, file,
                               width = 1800)
              } else {
                ggsave(
                  file,
                  plot = scatterQuality()$plotList[[ii]] +
                    plt_theme_scatter +
                    scale_color_manual(values = colorPalette),
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
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
            paste0("scatterTimeParticipant", i),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        ),
        shinycssloaders::withSpinner(
          plotOutput(
            paste0("scatterTimeParticipant", i + 1),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        )
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(
          paste0("downloadScatterTimeParticipant", i),
          'Download'
        ),
        downloadButton(
          paste0("downloadScatterTimeParticipant", i + 1),
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
            paste0("scatterTimeParticipant", i),
            width = "100%",
            height = "1200px"
          ),
          type = 4
        )
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(
                                    paste0("downloadScatterTimeParticipant", i),
                                    'Download'
                                  ))
    }
    
    # Generate the plots and download handlers
    for (j in 1:length(scatterTimeParticipant()$plotList)) {
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
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
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
          paste0("scatterTime", i),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          paste0("scatterTime", i + 1),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(paste0("downloadScatterTime", i), 'Download'),
        downloadButton(paste0("downloadScatterTime", i + 1), 'Download')
      )
      i = i + 2
    }
    
    # Handle any remaining scatter plot
    if (i == length(scatterTime()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          paste0("scatterTime", i),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatterTime", i), 'Download'))
    }
    
    # Generate the plots and download handlers
    for (j in 1:length(scatterTime()$plotList)) {
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
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    
    return(out)
  })
  
  
  #### crowding stair plots
  stairPlot <- reactive({
    plotStaircases(files()$stairs,
                   input$thresholdParameter,
                   conditionNames())
  })
  
  output$stairPlot <- renderImage({
    tmp_svg <- tempfile(fileext = '.svg')
    ggsave(
      file = tmp_svg,
      plot = stairPlot()$plot + plt_theme,
      width = 8,
      height = stairPlot()$height,
      limitsize = F,
      unit = 'in',
      device = svglite
    )
    disp_w <- 700
    scale <- 2
    png_w <- disp_w * scale
    aspect <- (stairPlot()$height / 8)
    png_h <- round(png_w * aspect)
    outfile <- tempfile(fileext = ".png")
    rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
    list(src = outfile,
         contenttype = 'image/png',
         width = disp_w,
         height = round(png_h/scale))
  }, deleteFile = TRUE)
  
  output$downloadStairPlot <- downloadHandler(
    filename = paste0(input$thresholdParameter, '-staircases.', input$fileType),
    content = function(file) {
      if (input$fileType == "png") {
        tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
        ggsave(
          tmp_svg,
          plot = stairPlot()$plot + plt_theme,
          width = 8,
          height = stairPlot()$height,
          unit = "in",
          limitsize = F,
          device = svglite
        )
        rsvg::rsvg_png(tmp_svg,
                       file,
                       height = 225 * stairPlot()$height,
                       width = 1800,
        )
      } else {
        ggsave(
          file,
          plot = stairPlot()$plot + plt_theme,
          width = 8,
          height = stairPlot()$height,
          unit = "in",
          limitsize = F,
          device = ifelse(
            input$fileType  == "svg",
            svglite::svglite,
            input$fileType
          )
        )
      }
    }
  )
  
  
  #### IR and IIR json ####
  showAC <- reactiveVal(FALSE)
  observeEvent(input$do, {
    if (!showAC()) {
      showModal(modalDialog("Generating plot…", footer = NULL))

      output$autocorrelation <- renderImage({
        file_list <- input$fileJSON$data
        jsonFile  <- fromJSON(file_list[1], simplifyDataFrame = FALSE)
        p         <- get_autocorrelation_plot(jsonFile, sound_data())
        tmp_svg   <- tempfile(fileext = ".svg")
        ggsave(file = tmp_svg, plot = p, device = svglite::svglite, width = 8, height = 8, units = "in")
        removeModal()
        disp_w <- 700
        scale <- 2
        png_w <- disp_w * scale
        png_h <- png_w
        outfile <- tempfile(fileext = ".png")
        rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
        list(src = outfile, contenttype = "image/png", width = disp_w, height = disp_w, alt = "autocorrelation")
      }, deleteFile = TRUE)

      # flip flag, update button
      showAC(TRUE)
      updateActionButton(session, "do", label = "Hide autocorrelation")

    }
    else {
      output$autocorrelation <- NULL

      showAC(FALSE)
      updateActionButton(session, "do", label = "Plot autocorrelation")
    }
    
  })
  
  sound_level_plot <- reactive({
    plot_sound_level(sound_data())
  })
  
  observeEvent(input$fileJSON, {
    #### sound calibration server side####
    
    output$`sound table` <- renderTable(sound_data()$volume_task,
                                        digits = 1)
    
    output$`Dynamic Range Compression Model` <-
      renderTable(
        expr = sound_data()$DRCMforDisplay,
        rownames = TRUE,
        colnames = FALSE,
        digits = 1
      )
    
    output$sound_level_plot <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = sound_level_plot()[[1]],
        width = sound_level_plot()[[2]],
        height = sound_level_plot()[[3]],
        device = svglite::svglite,
        units = "in"
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- if (!is.null(sound_level_plot()[[2]]) && sound_level_plot()[[2]] > 0) (sound_level_plot()[[3]] / sound_level_plot()[[2]]) else 1
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "1000 Hz plot")
    }, deleteFile = TRUE)
    
    
    # output$`record freq plot system` <- renderImage({
    #   outfile <- tempfile(fileext = '.svg')
    #   ggsave(
    #     file = outfile,
    #     plot = record_freq_system_plot()$plot,
    #     height = record_freq_system_plot()$height,
    #     width = 8.5,
    #     units = "in"
    #   )
    #   list(src = outfile,
    #        contenttype = 'svg',
    #        alt = "Power Spectral Density")
    # }, deleteFile = TRUE)
    
    output$`record freq plot component` <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = record_freq_component_plot()$plot,
        height = record_freq_component_plot()$height,
        width = 8.5,
        units = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (record_freq_component_plot()$height / 8.5)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "Power Spectral Density")
    }, deleteFile = TRUE)
    
    output$`power variation` <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = recording_variation()$plot,
        height = recording_variation()$height,
        width = 8,
        units = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (recording_variation()$height / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "Power Spectral Density")
    }, deleteFile = TRUE)
    
    output$`volume power variation` <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = volume_power_variation()$plot,
        height = volume_power_variation()$height,
        width = 8,
        units = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (volume_power_variation()$height / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "Power variation")
    }, deleteFile = TRUE)
    
    
    
    
    #### IIR ####
    # output$IRtmpFour <- renderImage({
    #   outfile <- tempfile(fileext = '.svg')
    #   ggsave(file = outfile,
    #          plot = iirPlots()[[1]],
    #          height = iirPlots()[[3]])
    #   list(src = outfile,
    #        contenttype = 'svg',
    #        alt = "IIR one")
    # }, deleteFile = TRUE)
    # 
    # output$IRtmpFive <- renderImage({
    #   outfile <- tempfile(fileext = '.svg')
    #   ggsave(file = outfile,
    #          plot = iirPlots()[[2]],
    #          height = iirPlots()[[4]])
    #   list(src = outfile,
    #        contenttype = 'svg',
    #        alt = "IIR two")
    # }, deleteFile = TRUE)
    
    output$componentIIR0To10 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = componentIIRPlots()$ten +
          sound_theme_display,
        height = 6,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((6/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR 0 to 10 ms")
    }, deleteFile = TRUE)
    
    output$componentIIR0To50 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = componentIIRPlots()$fifty +
          sound_theme_display,
        height = 6,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((6/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR 0 to 50 ms")
    }, deleteFile = TRUE)
    
    output$componentIIR0To400 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = componentIIRPlots()$schroeder +
          sound_theme_display,
        height = 5,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((5/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR schroeder")
    }, deleteFile = TRUE)
    
    
    #### IR ####
    
    output$componentIRPSD <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = componentIR_PSD_plot()$plot,
        height =  componentIR_PSD_plot()$height,
        width = 8,
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (componentIR_PSD_plot()$height / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To6 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = irPlots()[[1]],
        height = 6,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((6/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To50 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = irPlots()[[2]],
        height = 6,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((6/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To400 <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = irPlots()[[3]],
        height = 5,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      png_h <- round((5/8) * png_w)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    # output$cumSumPowerPlotSystem <- renderImage({
    #   outfile <- tempfile(fileext = '.svg')
    #   ggsave(
    #     file = outfile,
    #     plot = cumSumPowerPlot()$p_system +
    #       add_transducerTable_system(
    #         sound_data()[[7]],
    #         c("left", "bottom"),
    #         subtitle = list(
    #           c(
    #             subtitleOne(),
    #             subtitleTwo()$component,
    #             subtitleThree()$component
    #           )
    #         ),
    #         leftShift = 0.02
    #       ) + sound_theme_display,
    #     height = cumSumPowerPlot()$height_system,
    #     unit = "in",
    #   )
    #   list(src = outfile,
    #        contenttype = 'svg',
    #        alt = "IIR two")
    # }, deleteFile = TRUE)
    
    output$cumSumPowerPlotComponent <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = cumSumPowerPlot()$p_component +
          add_transducerTable_component(
            sound_data()[[7]],
            c("left", "bottom"),
            subtitle = list(
              c(
                subtitleOne(),
                subtitleTwo()$component,
                subtitleThree()$component
              )
            ),
            transducerType = sound_data()$inputParameters$transducerTypeF,
            leftShift = 0.02
          ),
        height = cumSumPowerPlot()$height_component,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (cumSumPowerPlot()$height_component / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    
  })
  
  observeEvent(input$toShinyOptions, {
    json <- fromJSON(input$toShinyOptions)
    if (input$transducerType == "Loudspeakers") {
      options <- json$createDates
    } else {
      if ("isDefault" %in% names(json)) {
        options <-
          ifelse(json$isDefault,
                 paste0("default/", unlist(json$modelNumbers)),
                 json$createDates)
      } else {
        options <- json$createDates
      }
      
    }
    options <- sort(unique(options), decreasing = F)
    updateCheckboxGroupInput(
      session,
      'profileSelection',
      label = "select profiles",
      choices = options,
      selected = NULL,
      inline = FALSE
    )
    
    session$sendCustomMessage("options", "true")
    
    df <- get_profile_table(json, transducerType) %>%
      select(-label)
    output$summaryStats <- renderTable({
      get_profile_summary(df)
    }, rownames = T, colnames = T)
    output$profiles <- DT::renderDataTable(datatable(
      df,
      class = list(stripe = FALSE),
      selection = 'none',
      filter = "top",
      options = list(
        paging = FALSE,
        searching = TRUE,
        dom = 'Bfrtip',
        rowCallback = JS(rowCallback)
      )
    ))
    output$downloadProfileTable <- downloadHandler(
      filename = "profiles-table.csv",
      content = function(filename) {
        write.csv(df, file = filename, row.names = FALSE)
      }
    )
    
  })
  
  observeEvent(input$doProfile, {
    title = paste0(input$plotTitle, " ", input$transducerType)
    p <- getFilteredProfilePlots(
      input$transducerType,
      fromJSON(input$totalData),
      title,
      input$profileSelection
    )
    
    output$profilePlot <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = p$plot,
        height = p$height,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (p$height / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    json <- fromJSON(input$toShiny)
    
    df <- get_profile_table(json, get_profile_table) %>%
      filter(label %in% input$profileSelection) %>%
      select(-label)
    
    output$summaryStats <- renderTable({
      get_profile_summary(df)
    }, rownames = T, colnames = T)
    output$downloadProfileTable <- downloadHandler(
      filename = "profiles-table.csv",
      content = function(filename) {
        write.csv(df, file = filename, row.names = FALSE)
      }
    )
    
    output$profiles <- DT::renderDataTable(datatable(
      df,
      class = list(stripe = FALSE),
      selection = 'none',
      filter = "top",
      options = list(
        paging = FALSE,
        searching = TRUE,
        dom = 'Bfrtip',
        rowCallback = JS(rowCallback)
      )
    ))
    
    output$downloadProfilePlot <- downloadHandler(
      filename = paste0(
        gsub("[^[:alnum:]]", "_", tolower(input$plotTitle)),
        '.',
        input$fileProfile
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = p$plot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height = p$height * 300,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = p$plot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
    output$shiftedProfilePlot <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = p$shiftedPlot,
        height = p$height,
        width = 8,
        unit = "in",
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (p$height / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    output$downloadShiftedProfilePlot <- downloadHandler(
      filename = paste0(
        gsub("[^[:alnum:]]", "_", tolower(input$plotTitle)),
        "-shifted",
        '.',
        input$fileProfile
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = p$shiftedPlot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height = p$height * 300,
                         width = 1800,
          )
        } else {
          ggsave(
            file,
            plot = p$shiftedPlot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
    output$profileAvgPlot <- renderImage({
      tmp_svg <- tempfile(fileext = '.svg')
      ggsave(
        file = tmp_svg,
        plot = p$avgPlot,
        height = p$avgHeight,
        width = 8,
        unit = "in",
        limitsize = FALSE,
        device = svglite::svglite
      )
      disp_w <- 700
      scale <- 2
      png_w <- disp_w * scale
      aspect <- (p$avgHeight / 8)
      png_h <- round(png_w * aspect)
      outfile <- tempfile(fileext = ".png")
      rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
      list(src = outfile,
           contenttype = 'image/png',
           width = disp_w,
           height = round(png_h/scale),
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    output$downloadProfileAvgPlot <- downloadHandler(
      filename = paste0('average-of-profiles',
                        '.',
                        input$fileProfile),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = p$avgPlot,
            height = p$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height = p$avgHeight * 300,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = p$avgPlot,
            height = p$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
    output$profileAverage <- renderTable(p$tb)
    output$profileAverageTitle <- renderText(p$title)
    
  })
  
  profile_plot <- reactive({
    title = paste0(input$plotTitle, " ", input$transducerType)
    getProfilePlots(input$transducerType,
                    fromJSON(input$totalData),
                    title)
  })
  
  observeEvent(input$totalData, {
    json <- fromJSON(input$toShiny)
    if (input$transducerType == "Loudspeakers") {
      options <- json$createDates
    } else {
      if ("isDefault" %in% names(json)) {
        options <-
          ifelse(
            json$isDefault,
            paste0(
              "default/",
              unlist(json$modelNumbers),
              "/",
              json$createDates
            ),
            json$createDates
          )
      } else {
        options <- json$createDates
      }
      
    }
    options <- sort(unique(options), decreasing = F)
    updateCheckboxGroupInput(
      session,
      'profileSelection',
      label = "select profiles",
      choices = options,
      selected = NULL,
      inline = FALSE
    )
    
    session$sendCustomMessage("options", "true")
    
    df <- get_profile_table(json, transducerType) %>%
      select(-label)
    output$summaryStats <- renderTable({
      get_profile_summary(df)
    }, rownames = T, colnames = T)
    output$profiles <- DT::renderDataTable(datatable(
      df,
      class = list(stripe = FALSE),
      selection = 'none',
      filter = "top",
      options = list(
        paging = FALSE,
        searching = TRUE,
        dom = 'Bfrtip',
        rowCallback = JS(rowCallback)
      )
    ))
    output$downloadProfileTable <- downloadHandler(
      filename = "profiles-table.csv",
      content = function(filename) {
        write.csv(df, file = filename, row.names = FALSE)
      }
    )
    output$profilePlot <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = profile_plot()$plot,
        height = profile_plot()$height,
        width = 8,
        unit = "in",
        limitsize = FALSE
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    output$shiftedProfilePlot <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = profile_plot()$shiftedPlot,
        height = profile_plot()$height,
        width = 8,
        unit = "in",
        limitsize = FALSE
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    output$profileAvgPlot <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = profile_plot()$avgPlot,
        height = profile_plot()$avgHeight,
        width = 8,
        unit = "in",
        limitsize = FALSE
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "profile plot")
    }, deleteFile = TRUE)
    
    output$profileAverage <- renderTable(profile_plot()$tb)
    
    output$profileAverageTitle <-
      renderText(profile_plot()$title)
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
  
  
  output$downloadAll = downloadHandler(
    filename = 'plots.zip',
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      fileNames <- c()
      
      # Save correlation matrices
      short_exp_name <- get_short_experiment_name(experiment_names())
      if (!is.null(corrMatrix()$plot)) {
        savePlot(corrMatrix()$plot, 
                 paste0(short_exp_name, "correlation-matrix.", input$fileType),
                 input$fileType, 
                 width = corrMatrix()$width, 
                 height = corrMatrix()$height)
        
        savePlot(corrMatrix()$n_plot, 
                 paste0(short_exp_name, "pairwise-count-matrix.", input$fileType),
                 input$fileType, 
                 width = corrMatrix()$width, 
                 height = corrMatrix()$height)
      }
     
      if (!is.null(durationCorrMatrix()$plot)) {
        savePlot(durationCorrMatrix()$plot, 
                 paste0(short_exp_name, "duration-correlation-matrix.", input$fileType),
                 input$fileType, 
                 width = durationCorrMatrix()$width, 
                 height = durationCorrMatrix()$height)
      }
      
      fileNames <- c(
        paste0(short_exp_name, "correlation-matrix.", input$fileType),
        paste0(short_exp_name, "duration-correlation-matrix.", input$fileType),
        paste0(short_exp_name, "n-matrix.", input$fileType)
      )
      
      # Save histograms
      if (length(histograms()$plotList) > 0) {
        for (i in seq_along(histograms()$plotList)) {
          plotFileName <- paste0(short_exp_name, histograms()$fileNames[[i]], '.', input$fileType)
          
          savePlot(histograms()$plotList[[i]] + plt_theme, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save dot plots
      if (length(dotPlots()$plotList) > 0) {
        for (i in seq_along(dotPlots()$plotList)) {
          plotFileName <- paste0(short_exp_name, dotPlots()$fileNames[[i]], '.', input$fileType)
          
          # Use larger dimensions for SD histogram to make legend readable
          if (grepl("sd-log-density-histogram", dotPlots()$fileNames[[i]])) {
            savePlot(dotPlots()$plotList[[i]], plotFileName, input$fileType, width = 12, height = 8)
          } else {
            savePlot(dotPlots()$plotList[[i]], plotFileName, input$fileType, width = 8, height = 6)
          }
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save scatter diagrams
      if (length(scatterDiagrams()$plotList) > 0) {
        for (i in seq_along(scatterDiagrams()$plotList)) {
          plotFileName <- paste0(short_exp_name, scatterDiagrams()$fileNames[[i]], '.', input$fileType)
          savePlot(scatterDiagrams()$plotList[[i]] + plt_theme_scatter, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save distance scatter plots
      if (length(scatterDistance()$plotList) > 0) {
        for (i in seq_along(scatterDistance()$plotList)) {
          plotFileName <- paste0(short_exp_name, scatterDistance()$fileNames[[i]], '.', input$fileType)
          savePlot(scatterDistance()$plotList[[i]] + plt_theme_scatter, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save age plots
      if (length(agePlots()$plotList) > 0) {
        for (i in seq_along(agePlots()$plotList)) {
          plotFileName <- paste0(short_exp_name, agePlots()$fileNames[[i]], '.', input$fileType)
          savePlot(agePlots()$plotList[[i]] + plt_theme, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save ggriaph plots
      # Create a list with plotList and title sublists
      ggiraph_plots <- list(
        plotList = list(
          # RSVP Crowding plots
          rsvpCrowding()$p_grade,                   
          rsvpCrowding()$f_grade,   
          rsvpCrowding()$p_font,                   
          rsvpCrowding()$residual,                   
          rsvpCrowding()$f_font,                  
          
          # RSVP Acuity plots
          rsvpAcuityFoveal()[[2]],             
          rsvpAcuityPeripheral()[[2]],         
          
          # RSVP Repeated Letter plots
          rsvp_repeated_letter_crowding()[[2]],  
          
          # Ordinary Acuity plots
          ordinaryAcuityFoveal()[[2]],           
          ordinaryAcuityPeripheral()[[2]],       
          
          # Ordinary Crowding plots
          ordinaryCrowdingPlots()[[1]],         
          ordinaryCrowdingPlots()[[2]], 
          ordinaryCrowdingPlots()[[3]],          
          ordinaryCrowdingPlots()[[4]],          
          
          # Reading Repeated plots
          readingRepeatedPlots()[[2]]            
        ),
        
        fileNames = list(
          # RSVP Crowding titles
          "rsvp-vs-peripheral-crowding-by-grade",
          "rsvp-vs-foveal-crowding-by-grade",
          "rsvp-vs-peripheral-crowding-by-font",
          "residual-rsvp-vs-residual-peripheral-crowding-by-font", 
          "rsvp-vs-foveal-crowding-by-font",
          
          # RSVP Acuity titles
          "rsvp-foveal-acuity-by-grade",
          "rsvp-peripheral-acuity-by-grade",
          
          # RSVP Repeated Letter titles
          "rsvp-repeated-letter-by-grade",
          
          # Ordinary Acuity titles
          "ordinary-foveal-acuity-by-grade",
          "ordinary-peripheral-acuity-by-grade",
          
          # Ordinary Crowding title
          "ordinary-peripheral-crowding-by-age",
          "ordinary-peripheral-crowding-by-grade",
          "ordinary-foveal-crowding-by-age",
          "ordinary-foveal-crowding-by-grade",
          
          # Reading Repeated titles
          "reading-repeated-letter-by-grade"
        )
      )
      if (length(ggiraph_plots$plotList) > 0) {
        for (i in seq_along(ggiraph_plots$plotList)) {

          print(ggiraph_plots$fileNames[[i]])
          if (!is.null(ggiraph_plots$plotList[[i]])) {
            plotFileName <- paste0(short_exp_name, ggiraph_plots$fileNames[[i]], '.', input$fileType)
            savePlot(ggiraph_plots$plotList[[i]] + plt_theme_ggiraph, plotFileName, input$fileType)
            fileNames <- c(fileNames, plotFileName)
          }
        }
      }
      
      # Save violin plots
      if (length(violinPlots()$plotList) > 0) {
        for (i in seq_along(violinPlots()$plotList)) {
          plotFileName <- paste0(short_exp_name, violinPlots()$fileNames[[i]], '.', input$fileType)
          savePlot(violinPlots()$plotList[[i]], plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save font comparison plots
      if (length(fontComparisonPlots()$plotList) > 0) {
        for (i in seq_along(fontComparisonPlots()$plotList)) {
          plotFileName <- paste0(short_exp_name, fontComparisonPlots()$fileNames[[i]], '.', input$fileType)
          savePlot(fontComparisonPlots()$plotList[[i]], plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      zip(file, fileNames)
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
        paste0(get_short_experiment_name(experiment_names()), "Summary-of-each-condition.xlsx")
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
        paste0(get_short_experiment_name(experiment_names()), "Thresholds(brief).xlsx")
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
        "Thresholds.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "Thresholds.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(df_list()$all_summary, file = filename)  # Using openxlsx
    }
  )
  
  output$participantInfoExcel <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "ParticipantDistanceInfo.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "ParticipantDistanceInfo.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(mergedParticipantDistanceTable(), file = filename)  # Using openxlsx
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
  
  output$`sound_data` <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "sound-calibration.xlsx",
        # Change file extension to .xlsx
        paste0(get_short_experiment_name(experiment_names()), "sound-calibration.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(all_sound_data()[[1]], file = filename)  # Use openxlsx instead of write.csv
    }
  )
  
  #### update download handler ####
  
  toListen <- reactive({
    list(input$file, input$fileType)
  })
  
  
  
  toListenSound <- reactive({
    list(input$fileJSON, input$fileTypeSound)
  })
  
  toListenProfile <- reactive({
    list(input$totalData, input$fileProfile)
  })
  
  
  observeEvent(toListenProfile(), {
    output$downloadProfilePlot <- downloadHandler(
      filename = paste0(
        gsub("[^[:alnum:]]", "_", tolower(input$plotTitle)),
        '.',
        input$fileProfile
      ),
      content = function(file) {
        if (input$fileProfile == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = profile_plot()$plot,
            height =  profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height = profile_plot()$height * 300,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot =  profile_plot()$plot,
            height =   profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
    output$downloadShiftedProfilePlot <- downloadHandler(
      filename = paste0(
        gsub("[^[:alnum:]]", "_", tolower(input$plotTitle)),
        "-shifted",
        '.',
        input$fileProfile
      ),
      content = function(file) {
        if (input$fileProfile == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot =  profile_plot()$shiftedPlot,
            height = profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height =  profile_plot()$height * 300,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = profile_plot()$shiftedPlot,
            height = profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
    output$downloadProfileAvgPlot <- downloadHandler(
      filename = paste0('average-of-profiles',
                        '.',
                        input$fileProfile),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = profile_plot()$avgPlot,
            height = profile_plot()$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png(tmp_svg,
                         file,
                         height = profile_plot()$avgHeight * 300,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = profile_plot()$avgPlot,
            height = profile_plot()$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileProfile == "svg",
              svglite::svglite,
              input$fileProfile
            )
          )
        }
      }
    )
    
  })
  observeEvent(toListenSound(), {
    output$file_name <-
      renderPrint({
        input$fileJSON$name
      })
    output$downloadSoundLevelPlot <- downloadHandler(
      filename = paste(
        paste0('sound-level-at-1000Hz.', input$fileTypeSound),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
          ggsave(
            tmp_svg,
            plot = sound_level_plot()[[1]],
            width = sound_level_plot()[[2]],
            height = sound_level_plot()[[3]],
            unit = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png(tmp_svg, file,
                         height = 2000,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = sound_level_plot()[[1]],
            width = sound_level_plot()[[2]],
            height = sound_level_plot()[[3]],
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadRecordFreqPlotSystem <- downloadHandler(
      filename = paste0(
        'loudspeaker+microphone-correction.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = record_freq_system_plot()$plot,
            height = record_freq_system_plot()$height,
            width = 8.5,
            dpi = 100,
            units = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2000,
                         height = 4000)
        } else {
          ggsave(
            file,
            plot = record_freq_system_plot()$plot,
            height = record_freq_system_plot()$height,
            width = 8.5,
            dpi = 100,
            units = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadRecordFreqPlotComponent <- downloadHandler(
      filename = paste0(
        sound_data()[[12]]$transducerTypeF,
        '-correction-plot.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = record_freq_component_plot()$plot,
            height = record_freq_component_plot()$height,
            width = 8.5,
            units = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 2000,
                         height = 4000)
        } else {
          ggsave(
            file,
            plot = record_freq_component_plot()$plot,
            height = record_freq_component_plot()$height,
            width = 8.5,
            units = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    # output$downloadIRtmpFour <- downloadHandler(
    #   filename = paste0(
    #     'power-spectral-density-of-system-iir.',
    #     input$fileTypeSound
    #   ),
    #   content = function(file) {
    #     if (input$fileTypeSound == "png") {
    #       ggsave(
    #         "tmp.svg",
    #         plot = iirPlots()[[1]],
    #         height = iirPlots()[[3]],
    #         units = "in",
    #         device = svglite::svglite
    #       )
    #       rsvg::rsvg_png("tmp.svg",
    #                      file,
    #                      width = 1800,
    #                      height = 1800)
    #     } else {
    #       ggsave(
    #         file,
    #         plot = iirPlots()[[1]],
    #         height = iirPlots()[[3]],
    #         device = ifelse(
    #           input$fileTypeSound == "svg",
    #           svglite::svglite,
    #           input$fileTypeSound
    #         )
    #       )
    #     }
    #   }
    # )
    # 
    # output$downloadIRtmpFive <- downloadHandler(
    #   filename = paste0(
    #     'power-spectral-density-of-',
    #     sound_data()[[12]]$transducerTypeF,
    #     "-iir.",
    #     input$fileTypeSound
    #   ),
    #   content = function(file) {
    #     if (input$fileTypeSound == "png") {
    #       ggsave(
    #         "tmp.svg",
    #         plot = iirPlots()[[2]],
    #         height = iirPlots()[[4]],
    #         device = svglite::svglite
    #       )
    #       rsvg::rsvg_png("tmp.svg",
    #                      file,
    #                      width = 1800,
    #                      height = 1800)
    #     } else {
    #       ggsave(
    #         file,
    #         plot = iirPlots()[[2]],
    #         height = iirPlots()[[4]],
    #         device = ifelse(
    #           input$fileTypeSound == "svg",
    #           svglite::svglite,
    #           input$fileTypeSound
    #         )
    #       )
    #     }
    #   }
    # )
    
    output$downloadComponentIIR0To10 <- downloadHandler(
      filename = paste0(
        "6-ms-of-",
        sound_data()[[12]]$transducerTypeF,
        "-inverse-impulse-response",
        '.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = componentIIRPlots()$ten +
              sound_theme_display,
            height = 6,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = componentIIRPlots()$ten +
              sound_theme_display,
            height = 6,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIIR0To50 <- downloadHandler(
      filename = paste0(
        "50-ms-of-",
        sound_data()[[12]]$transducerTypeF,
        "-inverse-impulse-response",
        '.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = componentIIRPlots()$fifty +
              sound_theme_display,
            height = 6,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = componentIIRPlots()$fifty +
              sound_theme_display,
            height = 6,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIIR0To400 <- downloadHandler(
      filename = paste0(
        "Schroeder-plot-of-",
        sound_data()[[12]]$transducerTypeF,
        "-inverse-impulse-response",
        '.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = componentIIRPlots()$schroeder +
              sound_theme_display,
            height = 5,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = componentIIRPlots()$schroeder +
              sound_theme_display,
            height = 5,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIR0To6 <- downloadHandler(
      filename = paste0(
        '6-ms-of-',
        sound_data()[[12]]$transducerTypeF,
        '-Impulse-Response.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = irPlots()[[1]],
            device = svglite,
            height = 6,
            unit = "in",
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = irPlots()[[1]],
            height = 6,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIR0To50 <- downloadHandler(
      filename = paste0(
        '50-ms-of-',
        sound_data()[[12]]$transducerTypeF,
        '-Impulse-Response.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = irPlots()[[2]],
            height = 6,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = irPlots()[[2]],
            height = 6,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIR0To400 <- downloadHandler(
      filename = paste0(
        "Schroeder-plot-of-",
        sound_data()[[12]]$transducerTypeF,
        "-impulse-response",
        '.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot =  irPlots()[[3]],
            height = 5,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = irPlots()[[3]],
            height = 5,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadComponentIRPSD <- downloadHandler(
      filename = paste0(
        sound_data()[[12]]$transducerTypeF,
        '-profile.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = componentIR_PSD_plot()$plot,
            height =  componentIR_PSD_plot()$height,
            width = 8,
            dpi = 100,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = componentIR_PSD_plot()$plot,
            height =  componentIR_PSD_plot()$height,
            width = 8,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    # output$downloadCumSumPowerPlotSystem <- downloadHandler(
    #   filename = paste0(
    #     'cumulative-power-of-system-corrected-mls.',
    #     input$fileTypeSound
    #   ),
    #   content = function(file) {
    #     if (input$fileTypeSound == "png") {
    #       ggsave(
    #         "tmp.svg",
    #         plot = cumSumPowerPlot()$p_system +
    #           add_transducerTable_system(
    #             sound_data()[[7]],
    #             c("left", "bottom"),
    #             subtitle = list(
    #               c(
    #                 subtitleOne(),
    #                 subtitleTwo()$component,
    #                 subtitleThree()$component
    #               )
    #             ),
    #             leftShift = 0.02
    #           ),
    #         height = cumSumPowerPlot()$height_system,
    #         units = "in",
    #         device = svglite::svglite
    #       )
    #       rsvg::rsvg_png("tmp.svg",
    #                      file,
    #                      width = 1800,
    #                      height = 1800)
    #     } else {
    #       ggsave(
    #         file,
    #         plot = cumSumPowerPlot()$p_system +
    #           add_transducerTable_system(
    #             sound_data()[[7]],
    #             c("left", "bottom"),
    #             subtitle = list(
    #               c(
    #                 subtitleOne(),
    #                 subtitleTwo()$component,
    #                 subtitleThree()$component
    #               )
    #             ),
    #             leftShift = 0.02
    #           ),
    #         height = cumSumPowerPlot()$height_system,
    #         device = ifelse(
    #           input$fileTypeSound == "svg",
    #           svglite::svglite,
    #           input$fileTypeSound
    #         )
    #       )
    #     }
    #   }
    # )
    
    output$downloadCumSumPowerPlotcomponent <- downloadHandler(
      filename = paste0(
        'cumulative-power-of-component-corrected-mls.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = cumSumPowerPlot()$p_component +
              add_transducerTable_component(
                sound_data()[[7]],
                c("left", "bottom"),
                subtitle = list(
                  c(
                    subtitleOne(),
                    subtitleTwo()$component,
                    subtitleThree()$component
                  )
                ),
                transducerType = sound_data()$inputParameters$transducerTypeF,
                leftShift = 0.02
              ),
            height = cumSumPowerPlot()$height_component,
            unit = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = cumSumPowerPlot()$p_component +
              add_transducerTable_component(
                sound_data()[[7]],
                c("left", "bottom"),
                subtitle = list(
                  c(
                    subtitleOne(),
                    subtitleTwo()$component,
                    subtitleThree()$component
                  )
                ),
                transducerType = sound_data()$inputParameters$transducerTypeF,
                leftShift = 0.02
              ),
            height = cumSumPowerPlot()$height_component,
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    output$downloadPowerVariation <- downloadHandler(
      filename = paste0(
        'power-variation-in-wideband-recordings.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = recording_variation()$plot,
            height = recording_variation()$height,
            width = 8,
            unit = "in",
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = recording_variation()$plot,
            height = recording_variation()$height,
            width = 8,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadVolumePowerVariation <- downloadHandler(
      filename = paste0(
        'power-variation-in-1000hz-recordings.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = volume_power_variation()$plot,
            height = volume_power_variation()$height,
            width = 8,
            unit = "in",
            dpi = 100,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
                         file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = volume_power_variation()$plot,
            height = volume_power_variation()$height,
            width = 8,
            unit = "in",
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadAutocorrelation <- downloadHandler(
      # (1) make filename a function so the “.png” or “.svg” is applied at click‑time
      filename = function() {
        paste0("autocorrelation.", input$fileTypeSound)
      },
      # (2) explicitly tell the browser what it’s getting
      contentType = switch(
        input$fileTypeSound,
        png = "image/png",
        svg = "image/svg+xml"
      ),
      content = function(file) {
        # generate the plot
        files  <- input$fileJSON$data
        json   <- fromJSON(files[1], simplifyDataFrame = FALSE)
        plotAC <- get_autocorrelation_plot(json, sound_data())
        
        if (input$fileTypeSound == "png") {
          # direct PNG raster device (avoids huge SVG → rsvg limits)
          png(
            filename = file,
            width    = 8*300,  # 8 inches × 300 DPI
            height   = 8*300,
            res      = 300
          )
          print(plotAC)
          dev.off()
        } else {
          # vector (SVG)
          ggsave(
            filename = file,
            plot     = plotAC,
            device   = "svg",
            width    = 8,
            height   = 8,
            units    = "in"
          )
        }
      }
    )
    
    
    
    
  })
  
  # download plots handlers
  
  observeEvent(toListen(), {
    ##### download handler for grade plots #####
    
    output$downloadCrowdingAgePlot <- downloadHandler(
      filename = paste0('crowding-vs-age',
                        '.',
                        input$fileType),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
      
    )
    
    output$downlaodDurationByFont <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-font-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downlaodDurationByID <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-participant-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downlaodDurationWithFontPadding <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downlaodLatenessByFont <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-font-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downlaodLatenessByID <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-participant-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downlaodLatenessWithFontPadding <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-vs-fontNominalSizePx*(1+fontPadding)-by-font-plot',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downloadDurationHist <- downloadHandler(
      filename = paste0(
        'targetMeasuredDurationSec-by-os-histogam',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downloadLatenessHist <- downloadHandler(
      filename = paste0(
        'targetMeasuredLatenessSec-by-os-histogam',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downloadAcuityAgePlot <- downloadHandler(
      filename = paste0('acuity-vs-age',
                        '.',
                        input$fileType),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    ##### renderUI download handlers #####
    
    for (j in 1:length(agePlots()$plotList)) {
      local({
        ii <- j
        
        output[[paste0("downloadP", ii)]] <- downloadHandler(
          filename = paste0(
            experiment_names(),
            agePlots()$fileNames[[ii]],
            '.',
            input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
              ggsave(
                "tmp.svg",
                plot = agePlots()$plotList[[ii]] + plt_theme,
                height = 6,
                width = 6,
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
                plot = agePlots()$plotList[[ii]] + plt_theme,
                height = 6,
                width = 6,
                unit = "in",
                limitsize = F,
                device = ifelse(
                  input$fileType == "svg",
                  svglite::svglite,
                  input$fileType
                )
              )
            }
          }
        )
      })
    }
    for (j in 1:length(histograms()$plotList)) {
      local({
        ii <- j
        output[[paste0("downloadHist", ii)]] <- downloadHandler(
          filename = paste0(
            experiment_names(),
            histograms()$fileNames[[ii]],
            '.',
            input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
              ggsave(
                "tmp.svg",
                plot = histograms()$plotList[[ii]] + plt_theme,
                height = 6,
                width = 6,
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
                plot = histograms()$plotList[[ii]] + plt_theme,
                height = 6,
                width = 6,
                unit = "in",
                limitsize = F,
                device = ifelse(
                  input$fileType == "svg",
                  svglite::svglite,
                  input$fileType
                )
              )
            }
          }
        )
      })
    }
    for (j in 1:length(scatterDiagrams()$plotList)) {
      local({
        ii <- j
        
        output[[paste0("downloadScatter", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              scatterDiagrams()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
                ggsave(
                  "tmp.svg",
                  plot = scatterDiagrams()$plotList[[ii]] +
                    plt_theme_scatter,
                  height = 8,
                  width = 12,
                  unit = "in",
                  limitsize = F,
                  device = svglite
                )
                rsvg::rsvg_png("tmp.svg",
                               file,
                               height = 1600,
                               width = 2400)
              } else {
                ggsave(
                  file,
                  plot = scatterDiagrams()$plotList[[ii]] +
                    plt_theme_scatter,
                  height = 8,
                  width = 12,
                  unit = "in",
                  limitsize = F,
                  device = ifelse(
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    for (j in 1:length(fontComparisonPlots()$plotList)){
      local({
        ii <- j
        output[[paste0("downloadFontComparison", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              fontComparisonPlots()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
      })
    }
    for (j in 1:length(violinPlots()$plotList)){
      local({
        ii <- j
        output[[paste0("downloadViolin", ii)]] <-
          downloadHandler(
            filename = paste0(
              get_short_experiment_name(experiment_names()),
              violinPlots()$fileNames[[ii]],
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
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
                    input$fileType == "svg",
                    svglite::svglite,
                    input$fileType
                  )
                )
              }
            }
          )
        })
    }
    
    
    output$downloadRsvpCrowdingPeripheralGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-grade.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$p_grade + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = input$fileType,
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpCrowdingPeripheralFontPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-font.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$p_font + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = input$fileType,
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpResidualCrowding <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0(
          'residual-rsvp-vs-residual-peripheral-crowding-by-grade.',
          input$fileType
        ),
        sep = "-"
      ),
      content = function(file) {
        base_plot <- rsvpCrowding()$residual + plt_theme
        plot_with_title <- add_experiment_title(base_plot, experiment_names())
        savePlot(
          plot = plot_with_title,
          filename = file,
          fileType = input$fileType,
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
          input$fileType
        )
      },
      content = function(file) {
        plot <- rsvpCrowding()$f_grade + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = input$fileType,
          width = 8,
          height = 6
        )
      }
    )
    
    output$downloadRsvpFovealAcuityGradePlot <- downloadHandler(
      filename = function () {
        paste0(get_short_experiment_name(experiment_names()),
               'rsvp-vs-fovel-acuity-by-grade.',
               input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
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
               input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
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
                 input$fileType)
        },
        content = function(file) {
          base_plot <- rsvpAcuityPeripheral()$grade + plt_theme
          plot_with_title <- add_experiment_title(base_plot, experiment_names())
          savePlot(plot = plot_with_title,
                   filename = file,
                   fileType = input$fileType,
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
                 input$fileType)
        },
        content = function(file) {
          if (input$fileType == "png") {
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
      filename = paste0(
        get_short_experiment_name(experiment_names()),
        'reading-vs-peripheral-acuity-by-grade.',
        input$fileType
      ),
      content = function(file) {
        plot <- ordinaryAcuityPeripheral()$grade + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = input$fileType,
          width = 6
        )
      }
    )
    
    output$downloadOrdinaryPeripheralAcuityFontPlot <- downloadHandler(
      filename = paste0(
        get_short_experiment_name(experiment_names()),
        'reading-vs-peripheral-acuity-by-font.',
        input$fileType
      ),
      content = function(file) {
        plot <- ordinaryAcuityPeripheral()$font + plt_theme
        savePlot(
          plot = plot,
          filename = file,
          fileType = input$fileType,
          width = 6
        )
      }
    )
    
    output$downloadOrdinaryFovealAcuityGradePlot <-
      downloadHandler(
        filename = function() {
          paste0("ordinary_foveal_acuity_grade", input$fileType, sep = ".")
        },
        content = function(file) {
          if (input$fileType == "png") {
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
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
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
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
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
            input$fileType
          )
        },
        content = function(file) {
          plot <- fontAggregatedReadingRsvpCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (input$fileType == "png") {
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
                  input$fileType == "svg",
                  svglite::svglite,
                  input$fileType
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
            input$fileType
          )
        },
        content = function(file) {
          plot <- fontAggregatedOrdinaryReadingCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (input$fileType == "png") {
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
                  input$fileType == "svg",
                  svglite::svglite,
                  input$fileType
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
            input$fileType
          )
        },
        content = function(file) {
          plot <- fontAggregatedRsvpCrowding()
          if (!is.null(plot)) {
            plot_with_title <- add_experiment_title(plot + plt_theme, experiment_names())
            if (input$fileType == "png") {
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
                  input$fileType == "svg",
                  svglite::svglite,
                  input$fileType
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
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
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
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
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
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
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
      filename = paste0(get_short_experiment_name(experiment_names()),
                        'correlation-matrix.',
                        input$fileType),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    
    output$downloadNMatrixPlot <- downloadHandler(
      filename = function() paste0(get_short_experiment_name(experiment_names()), "n-matrix.", input$fileType),
      content = function(file) {
        if (input$fileType == "png") {
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
            device     = if (input$fileType=="svg") svglite::svglite else input$fileType
          )
        }
      }
    )
    
    
    output$downloadDurationCorrMatrixPlot <- downloadHandler(
      filename = paste0(get_short_experiment_name(experiment_names()),
                        'duration-correlation-matrix.',
                        input$fileType),
      content = function(file) {
        if (input$fileType == "png") {
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
              input$fileType == "svg",
              svglite::svglite,
              input$fileType
            )
          )
        }
      }
    )
    

  })
  
  # download jupyter notebook handler
  output$downloadNotebook <- downloadHandler(filename <-
                                               function() {
                                                 paste("sound_calibration_analysis", "ipynb", sep = ".")
                                               },
                                             
                                             content <-
                                               function(file) {
                                                 file.copy("notebooks/sound_calibration_analysis.ipynb", file)
                                               })
  
})
