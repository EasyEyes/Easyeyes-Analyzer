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
source("./plotting/acuityPlot.R")
source('./plotting/crowdingrsvp.R')
source('./plotting/repeated-letter-crowding.R')
source('./plotting/durationSecPlot.R')
source('./plotting/distancePlot.R')
source('./plotting/minDegPlot.R')

source("./other/getBits.R")
source("./other/sound_plots.R")
source("./other/read_json.R")
source("./other/formSpree.R")
source("./other/utility.R")

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
  })
  
  output$fileStatusMessage <- renderUI({
    if (!is.null(files())) {
      prolific_counts <-
        get_prolific_file_counts(prolific(), summary_table())
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
  
  prolific <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    t <- find_prolific_from_files(input$file)
    return(t)
  })
  
  data_list <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    t <- files()[[1]]
    return(t)
  })
  
  summary_list <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    t <- files()[[2]]
    return(t)
  })
  experiment_names <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    exp_names <-
      trimws(files()[[3]])  # Extract and trim whitespace
    
    # Split names based on hyphen (-)
    name_list <- unlist(strsplit(exp_names, "-"))
    
    if (length(name_list) > 1) {
      return(paste(name_list[1], "Etc. "))
    } else {
      return(name_list)
    }
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
    generate_summary_table(files()$data_list, files()$stairs)
  })
  
  threshold_and_warnings <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    return(
      generate_threshold(
        files()$data_list,
        summary_list(),
        files()$pretest,
        files()$stairs,
        files()$df,
        minNQuestTrials(),
        minWrongTrials(),
        maxQuestSD(),
        conditionNames(),
        maxReadingSpeed()
      )
    )
  })
  
  minNQuestTrials <-reactive({input$NQuestTrials}) %>% debounce(1000)
  maxQuestSD <- reactive({input$maxQuestSD}) %>% debounce(1000)
  conditionNames <- reactive(input$conditionName) %>% debounce(5000)
  minWrongTrials <- reactive(input$NWrongTrials) %>% debounce(5000)
  maxReadingSpeed <- reactive(input$maxReadingSpeed) %>% debounce(2000)
  
  df_list <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    df_list <- generate_rsvp_reading_crowding_fluency(
      files()$data_list,
      summary_list(),
      files()$pretest,
      files()$stairs,
      input$filterInput,
      input$skillFilter,
      minNQuestTrials(),
      minWrongTrials(),
      maxQuestSD(),
      conditionNames(),
      maxReadingSpeed()
    )
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
  
  ##### SOUND CALIBRATION ####
  
  irPlots <- reactive({
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    get_ir_plots(jsonFile, sound_data())
  })
  
  sound_data <- reactive({
    if (is.null(input$fileJSON))
      return(NULL)
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    return(preprocessJSON(jsonFile))
  })
  
  iir <- reactive({
    if (is.null(input$fileJSON))
      return(NULL)
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    return(read_iir_JSON(jsonFile))
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
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    plotComponentIIR(jsonFile, sound_data())
  })
  
  
  cumSumPowerPlot <- reactive({
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    getCumSumPowerPlot(jsonFile)
  })
  
  componentIR_PSD_plot <- reactive({
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    plotComponentIRPSD(jsonFile, sound_data())
  })
  
  recording_variation <- reactive({
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    plot_power_variations(jsonFile, sound_data())
  })
  
  volume_power_variation <- reactive({
    file_list <- input$fileJSON$data
    jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
    plot_volume_power_variations(jsonFile, sound_data())
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
  
  # reading_vs_font_size <- reactive({
  #   plot_rsvp_vs_x_height(df_list()[[3]])
  # })
  #
  # reading_60cm_1.2mm_vs_1.4mm <- reactive({
  #   get_60cm_scatter(df_list()[[3]])
  # })
  #
  # reading_30cm_1.2mm_vs_1.4mm <- reactive({
  #   get_30cm_scatter(df_list()[[3]])
  # })
  #
  # reading_diff_60cm_vs_age <- reactive({
  #   plot_60cm_speed_diff_vs_age(df_list()[[3]])
  # })
  
  meanPlot <- reactive({
    req(input$file)
    mean_plot(reading_rsvp_crowding_df()) +
      labs(title = paste(c(
        experiment_names(),
        "Reading Speed, mean"
      ), collapse = "\n")) +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  
  medianPlot <- reactive({
    if (is.null(reading_rsvp_crowding_df())) {
      return(NULL)
    }
    median_plot(reading_rsvp_crowding_df()) +
      labs(title = paste(c(
        experiment_names(),
        "Reading Speed, median"
      ),
      collapse = "\n")) +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  
  crowdingPlot <- reactive({
    if (is.null(crowdingBySide())) {
      return(NULL)
    }
    crowding_scatter_plot(crowdingBySide())
  })
  
  crowdingAvgPlot <- reactive({
    if (is.null(crowdingBySide())) {
      return(NULL)
    }
    crowding_mean_scatter_plot(crowdingBySide())  +
      coord_fixed(ratio = 1) +
      labs(title = paste(
        c(experiment_names(),
          "Crowding, left vs. right, by font"),
        collapse = "\n"
      ))
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
  
  two_fonts_plots <- reactive({
    req(input$file)
    get_two_fonts_plots(df_list()$crowding)
  })
  
  
  foveal_peripheral_diag <- reactive({
    req(input$file)
    get_foveal_peripheral_diag(df_list()$crowding)
  })
  
  
  foveal_crowding_vs_acuity_diag <- reactive({
    req(input$file)
    get_foveal_acuity_diag(df_list()$crowding, df_list()$acuity)
  })
  
  regressionFontPlot <- reactive({
    req(input$file)
    regression_font(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Reading vs crowding")
  })
  
  regressionFontPlotWithLabel <- reactive({
    req(input$file)
    regression_font_with_label(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Reading vs crowding")
  })
  
  readingTestRetest <- reactive({
    req(input$file)
    get_test_retest_reading(df_list()[[1]]) +
      labs(title = experiment_names(),
           subtitle = "Test retest of reading") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  crowdingTestRetest <- reactive({
    req(input$file)
    get_test_retest_crowding(df_list()[[2]]) +
      labs(title = experiment_names(),
           subtitle = "Test retest of crowding") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  
  rsvpReadingTestRetest <- reactive({
    req(input$file)
    get_test_retest_rsvp(df_list()[[3]])  +
      labs(title = experiment_names(),
           subtitle = "Test retest of rsvp reading") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  
  readingSpeedRetention <- reactive({
    req(input$file)
    reading_speed_vs_retention(df_list()[[1]]) +
      labs(title = experiment_names(),
           subtitle = "Reading vs retention") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  
  agePlots <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    print('inside agePlots')

    l <- list()
    fileNames <- list()

    plot_calls <- list(
      list(plot = get_peripheral_crowding_vs_age(df_list()$crowding)[[1]], fname = 'peripheral-crowding-vs-age-by-grade'),
      list(plot = get_peripheral_crowding_vs_age(df_list()$crowding)[[2]], fname = 'peripheral-crowding-ave-vs-age-by-grade'),
      list(plot = get_foveal_crowding_vs_age(df_list()$crowding), 'foveal-crowding-vs-age-by-grade'),
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
      res <- append_plot_list(l, fileNames, p + plt_theme, call$fname)
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

  static_calls <- list(
    list(plot = get_acuity_hist(df_list()$acuity)[[1]],            fname = 'foveal-acuity-histogram'),
    list(plot = get_acuity_hist(df_list()$acuity)[[2]],            fname = 'peripheral-acuity-histogram'),
    list(plot = get_crowding_hist(df_list()$crowding)$foveal,     fname = 'foveal-crowding-histogram'),
    list(plot = get_crowding_hist(df_list()$crowding)$peripheral, fname = 'peripheral-crowding-histogram'),
    list(plot = get_reading_hist(df_list()$rsvp),                  fname = 'rsvp-reading-speed-histogram'),
    list(plot = get_reading_hist(df_list()$reading),               fname = 'reading-speed-histogram'),
    list(plot = get_repeatedLetter_hist(df_list()$repeatedLetters),fname = 'repeated-letter-crowding-histogram'),
    list(plot = get_age_histogram(df_list()$age),                  fname = 'age-histogram'),
    list(plot = get_grade_histogram(df_list()$age),                fname = 'grade-histogram')
  )

 
  prop_hists <- get_prop_correct_hist_list(df_list()$quest_all_thresholds)
  prop_calls <- lapply(names(prop_hists), function(cond) {
    list(
      plot  = prop_hists[[cond]],
      fname = paste0('correct-trial-frac-histogram-', cond)
    )
  })


  all_calls <- c(static_calls, prop_calls)
  for (call in all_calls) {
    res <- append_plot_list(
      l, fileNames,
      call$plot + hist_theme,
      call$fname
    )
    l         <- res$plotList
    fileNames <- res$fileNames
  }

  print('before appending')
  lists <- append_hist_list(data_list(), l, fileNames)

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
    
    lists <- append_hist_quality(data_list(), l, fileNames, conditionNames())
    lists <- add_questsd_hist(df_list()$quest, lists)
    
    minDegHist <-
      get_minDeg_plots(data_list(),
                       df_list()$acuity,
                       df_list()$crowding,
                       df_list()$quest)$hist_quality
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
      append_hist_time(data_list(), l, fileNames, conditionNames())
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
  
  #### scatterDiagrams ####
  
  scatterDiagrams <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    print('inside scatter plots')
    l <- list()
    fileNames <- list()

    plot_calls <- list(
      list(plot = plot_distance(files()$data_list), fname = 'calibrateTrackDistanceMeasuredCm-vs-calibrateTrackDistanceRequestedCm-plot'),
      list(plot = foveal_crowding_vs_acuity_diag()$foveal, fname = 'foveal-crowding-vs-foveal-acuity-grade-diagram'),
      list(plot = foveal_crowding_vs_acuity_diag()$peripheral, fname = 'foveal-crowding-vs-peripheral-acuity-grade-diagram'),
      list(plot = get_acuity_foveal_peripheral_diag(df_list()$acuity), fname = 'foveal-acuity-vs-peripheral-acuity-grade-diagram'),
      list(plot = foveal_peripheral_diag()$grade, fname = 'foveal-crowding-vs-peripheral-crowding-grade-diagram'),
      list(plot = peripheral_plot(df_list())$grade, fname = 'peripheral-acuity-vs-peripheral-crowding-grade-diagram'),
      list(plot = peripheral_plot(df_list())$font, fname = 'peripheral-acuity-vs-peripheral-crowding-font-diagram'),
      list(plot = crowdingPlot(), fname = 'peripheral_crowding_left_vs_right'),
      list(plot = regression_reading_plot(df_list())$foveal, fname = 'reading-rsvp-reading-vs-foveal-crowding'),
      list(plot = regression_reading_plot(df_list())$peripheral, fname = 'reading-rsvp-reading-vs-peripheral-crowding'),
      list(plot = regression_acuity_plot(df_list()), fname = 'ordinary-reading-rsvp-reading-vs-acuity'),
      list(plot = plot_reading_rsvp(df_list()$reading, df_list()$rsvp), fname = 'reading-vs-RSVP-reading-plot'),
      list(plot = get_crowding_vs_repeatedLetter(df_list()$crowding, df_list()$repeatedLetters)$grade, fname = 'crowding-vs-repeated-letters-crowding-grade')
    )

    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        plot <- plot + scale_color_manual(values = colorPalette)
      }
      res <- append_plot_list(l, fileNames, plot, call$fname)
      l <- res$plotList
      fileNames <- res$fileNames
    }

    lists = append_scatter_list(data_list(), l, fileNames, conditionNames())
    minDegPlot <-
      get_minDeg_plots(data_list(),
                       df_list()$acuity,
                       df_list()$crowding,
                       df_list()$quest)$scatter

    return(list(
      plotList = c(lists$plotList, minDegPlot$plotList),
      fileNames = c(lists$fileNames, minDegPlot$fileNames)
    ))
  })
  
  scatterQuality <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    print('inside scatter quality plots')
    i = 1
    l <- list()
    fileNames <- list()
    
    crowdingPlots <-
      plotCrowdingStaircasesVsQuestTrials(df_list(), files()$stairs)
    if (!is.null(crowdingPlots$fovealPlot)) {
      l[[i]] <- crowdingPlots$fovealPlot +  scale_color_manual(values = colorPalette)
      fileNames[[i]] <-
        'foveal-crowding-staircases-threshold-vs-questTrials'
      i <- i + 1
    }
    
    # Add peripheral plot to the list
    if (!is.null(crowdingPlots$peripheralPlot)) {
      l[[i]] <- crowdingPlots$peripheralPlot + scale_color_manual(values = colorPalette)
      fileNames[[i]] <-
        'peripheral-crowding-staircases-threshold-vs-questTrials'
      i <- i + 1
    }

    t <- get_quest_diag(df_list()$quest)$grade
    if (!is.null(t)) {
      l[[i]] = t + scale_color_manual(values = colorPalette)
      fileNames[[i]] = 'quest-sd-vs-mean-grade-diagram'
      i = i + 1
    }
    
    t <- get_quest_sd_vs_trials(df_list()$quest_all_thresholds)
    if (!is.null(t)) {
      l[[i]] <- t + scale_color_manual(values = colorPalette)
      fileNames[[i]] <- 'quest-sd-vs-quest-trials-grade-diagram'
      i <- i + 1
    }
    
    minDegPlot <-
      get_minDeg_plots(data_list(),
                       df_list()$acuity,
                       df_list()$crowding,
                       df_list()$quest)$scatter_quality
    return(list(
      plotList = c(l, minDegPlot$plotList),
      fileNames = c(fileNames, minDegPlot$fileNames)
    ))
  })
  
  scatterTime <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print("inside ScatterTime")
    i <- 1
    l <- list()
    fileNames <- list()
    
    # Extract scatter plots using the append_scatter_time function
    scatter_time_plots <-
      append_scatter_time(data_list(), l, fileNames, conditionNames())
    
    return(scatter_time_plots)
  })
  
  scatterTimeParticipant <- reactive({
    if (is.null(input$file) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    
    print("inside ScatterTimeParticipant")
    i <- 1
    l <- list()
    fileNames <- list()
    
    # Extract scatter plots using the append_scatter_time function
    scatter_time_plots <-
      append_scatter_time_participant(data_list(), l, fileNames, conditionNames())
    
    return(scatter_time_plots)
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
  outputOptions(output, 'isDuration', suspendWhenHidden = FALSE)
  outputOptions(output, 'jsonUploaded', suspendWhenHidden = FALSE)
  
  #### plots ####
  
  output$corrMatrixPlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  corrMatrix()$plot,
      device = svglite,
      width = corrMatrix()$width,
      height = corrMatrix()$height
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$nMatrixPlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file   = outfile,
      plot   = corrMatrix()$n_plot,   
      device = svglite,
      width  = corrMatrix()$width,
      height = corrMatrix()$height
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$durationCorrMatrixPlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  durationCorrMatrix()$plot,
      device = svg,
      width = durationCorrMatrix()$width,
      height = durationCorrMatrix()$height
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$durationHist <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  duration_lateness_hist()$duration,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        annotate(
          "text",
          x = 0.5,
          y = 0.4,
          label = "Plot ID: durationHist",
          size = 4,
          fontface = "italic"
        ) +
        theme_void() +
        ggtitle('targetMeasuredDurationSec by os histogram')
      
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
      
      list(src = outfile,
           contenttype = 'svg',
           alt = "Error in Duration Histogram")
    })
    
  }, deleteFile = TRUE)
  
  output$latenessHist <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  duration_lateness_hist()$lateness,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        annotate(
          "text",
          x = 0.5,
          y = 0.4,
          label = "Plot ID: latenessHist",
          size = 4,
          fontface = "italic"
        ) +
        theme_void() +
        ggtitle('targetMeasuredLatenessSec by os histogram')
      
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
      
      list(src = outfile,
           contenttype = 'svg',
           alt = "Error in Lateness Histogram")
    })
    
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  durationPlot <- reactive({
    plot_duraction_sec(durationData())
  })
  
  latenessPlot <- reactive({
    plot_Lateness_sec(durationData())
  })
  
  output$durationByID <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  durationPlot()$participant,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by participant'
        )
      
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
      list(src = outfile,
           contenttype = 'svg')
    })
  }, deleteFile = TRUE)
  
  output$durationByFont <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  durationPlot()$font,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by font'
        )
      
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
      list(src = outfile,
           contenttype = 'svg')
    })
  }, deleteFile = TRUE)
  
  output$durationWithFontPadding <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = durationPlot()$fontPadding,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding'
        )
      
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
      list(src = outfile,
           contenttype = 'svg')
    })
  }, deleteFile = TRUE)
  
  output$latenessByID <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  latenessPlot()$participant,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by participant'
        )
      
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
    })
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$latenessByFont <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = latenessPlot()$font,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by font'
        )
      
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
      list(src = outfile,
           contenttype = 'svg')
    })
  }, deleteFile = TRUE)
  
  output$latenessWithFontPadding <- renderImage({
    tryCatch({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = latenessPlot()$fontPadding,
        device = svglite,
        width = 6,
        height = 4,
        unit = 'in'
      )
      list(src = outfile,
           contenttype = 'svg')
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
        ggtitle(
          'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding'
        )
      
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
      list(src = outfile,
           contenttype = 'svg')
    })
  }, deleteFile = TRUE)
  
  #### crowding ####
  
  output$crowdingAvgPlot <-
    renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  crowdingAvgPlot() + plt_theme,
        device = svg,
        width = 6,
        height = 4
      )
      
      list(src = outfile,
           contenttype = 'svg')
    }, deleteFile = TRUE)
  
  
  output$SloanVsTimesMeanPlot <-
    renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot =  two_fonts_plots()[[1]] +
          labs(
            title = experiment_names(),
            subtitle = paste0("Crowding ", two_fonts_plots()$title, ", mean")
          ) +
          plt_theme,
        device = svg,
        width = 7,
        height = 4
      )
      
      list(src = outfile,
           contenttype = 'svg')
    }, deleteFile = TRUE)
  
  output$SloanVsTimesSDPlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  two_fonts_plots()[[2]] +
        labs(
          title = experiment_names(),
          subtitle = paste0("Crowding ", two_fonts_plots()$title, ", sd")
        ) +
        plt_theme,
      device = svg,
      width = 7,
      height = 4
    )
    list(src = outfile,
         contenttype = 'svg')
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
        ggiraph::girafe(ggobj = rsvpCrowding()$p_grade)
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
          ggtitle('rsvp-vs-peripheral-crowding-by-grade')
        ggiraph::girafe(ggobj = error_plot)
      })
    })
  
  output$rsvpCrowdingPeripheralFontPlot <-
    ggiraph::renderGirafe({
      tryCatch({
        ggiraph::girafe(ggobj = rsvpCrowding()$p_font)
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
          ggtitle('rsvp-vs-peripheral-crowding-by-grade')
        ggiraph::girafe(ggobj = error_plot)
      })
    })
 
  output$rsvpResidualCrowding <-
    ggiraph::renderGirafe({
      tryCatch({
        ggiraph::girafe(ggobj = rsvpCrowding()$residual)
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
          ggtitle('residual-rsvp-vs-residual-peripheral-crowding-by-grade')
        ggiraph::girafe(ggobj = error_plot)
      })
    })
  
  ordinaryCrowdingPlots <- reactive({
    plot_reading_crowding(df_list())
  })
  
  readingRepeatedPlots <- reactive({
    plot_reading_repeated_letter_crowding(df_list())
  })

  output$rsvpCrowdingFovealGradePlot <- ggiraph::renderGirafe({
    ggiraph::girafe(ggobj = rsvpCrowding()$f_grade)
  })
  
  output$rsvpFovealAcuityGradePlot <- ggiraph::renderGirafe({
    ggiraph::girafe(ggobj = rsvpAcuityFoveal()[[2]])
  })
  
  output$rsvpPeripheralAcuityGradePlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = rsvpAcuityPeripheral()$grade)
    })
  
  output$rsvpPeripheralAcuityFontPlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = rsvpAcuityPeripheral()$font)
    })
  
  output$rsvpRepeatedGradePlot <- ggiraph::renderGirafe({
    ggiraph::girafe(ggobj = rsvp_repeated_letter_crowding()[[2]])
  })
  
  output$ordinaryFovealAcuityGradePlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryAcuityFoveal()[[2]])
    })
  
  output$ordinaryPeripheralAcuityGradePlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryAcuityPeripheral()$grade)
    })
  
  output$ordinaryPeripheralAcuityFontPlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryAcuityPeripheral()$font)
    })
  
  # Peripheral Crowding Plots
  output$ordinaryPeripheralCrowdingFontPlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryCrowdingPlots()[[1]])
      
      #temporarily change to colored by font
    })
  
  output$ordinaryPeripheralCrowdingGradePlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj =  ordinaryCrowdingPlots()[[3]])  
    })
  
  # Foveal Crowding Plots
  output$ordinaryFovealCrowdingFontPlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryCrowdingPlots()[[2]]) 
    })
  
  output$ordinaryFovealCrowdingGradePlot <-
    ggiraph::renderGirafe({
      ggiraph::girafe(ggobj = ordinaryCrowdingPlots()[[4]])  
    })
  
  
  output$readingRepeatedGradePlot <- ggiraph::renderGirafe({
    ggiraph::girafe(ggobj = readingRepeatedPlots()[[2]])
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
    
    lapply(seq_len(n), function(ii) {
      local({
        i <- ii
        output[[paste0("p", i)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(outfile,
                 plot = plotList[[i]] + plt_theme,
                 width = 6, height = 6,
                 unit = 'in', device = svglite)
          list(src = outfile, contentType = 'image/svg+xml')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadP", i)]] <- downloadHandler(
          filename = paste0(experiment_names(), fileNames[[i]], i, ".", input$fileType),
          content = function(file) {
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
    })
    
    return(out)
  })
  
  output$histograms <- renderUI({
    out    <- list()
    plots  <- histograms()$plotList
    files  <- histograms()$fileNames
    n      <- length(plots)
    nPerRow <- 6
    
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
          outfile <- tempfile(fileext = ".svg")
          save_plot_with_error_handling(
            plot = plots[[jj]] + hist_theme,
            filename = outfile,
            width = 2.5,
            height = 2.5,
            size = 3,
            unit = "in",
            plotTitle = files[[jj]]
          )
        }, deleteFile = TRUE)
        
        output[[paste0("downloadHist", jj)]] <- downloadHandler(
          filename = paste0(
            experiment_names(),
            files[[jj]],
            jj, ".", input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
              tmp_svg <- tempfile(fileext = ".svg")
              ggsave(tmp_svg,
                     plot   = plots[[jj]] + hist_theme,
                     device = svglite,
                     width  = 2.5,
                     height = 2.5,
                     unit   = "in",
                     limitsize = FALSE)
              rsvg::rsvg_png(tmp_svg, file,
                             width  = 900 * 3/4,
                             height = 900 * 2.5/4)
            } else {
              ggsave(
                file,
                plot   = plots[[jj]] + hist_theme,
                width  = 2.5,
                height = 2.5,
                unit   = "in",
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
          tryCatch({
            outfile <- tempfile(fileext = '.svg')
            ggsave(
              file = outfile,
              plot = histogramsQuality()$plotList[[ii]] + hist_theme,
              device = svglite,
              width = 4,
              # Reduced width
              height = 3.5,
              # Reduced height
              unit = 'in'
            )
            list(src = outfile, contenttype = 'svg')
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
              ggtitle(histogramsQuality()$fileNames[[ii]])
            
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
        
        output[[paste0("downloadQualityHist", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
              histogramsQuality()$fileNames[[ii]],
              ii,
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
          tryCatch({
            outfile <- tempfile(fileext = '.svg')
            ggsave(
              file = outfile,
              plot = timingHistograms()$plotList[[ii]] + hist_theme,
              device = svglite,
              width = 4,
              # Reduced width
              height = 3.5,
              # Reduced height
              unit = 'in'
            )
            list(src = outfile, contenttype = 'svg')
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
              ggtitle(timingHistograms()$fileNames[[ii]])
            
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
        
        output[[paste0("downloadTimingHist", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
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
      ggsave(
        filename = outfile,
        plot = stackedPlots()$rsvp_plot +
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
          ),
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
          experiment_names(),
          "histogram-of-rsvp-reading-stacked-by-grade.",
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$rsvp_plot +
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
              ),
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          ggsave(
            filename = file,
            plot = stackedPlots()$rsvp_plot +
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
              ),
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
          experiment_names(),
          "histogram-of-peripheral-crowding-stacked-by-grade.",
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$crowding_plot +
              plt_theme +
              stacked_theme,
            device = svglite,
            width = 6,
            height = 8,
            unit = "in"
          )
          rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)
        } else {
          ggsave(
            filename = file,
            plot = stackedPlots()$crowding_plot +
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
          experiment_names(),
          "histogram-of-foveal-acuity-stacked-by-grade.",
          input$fileType
        )
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_acuity_plot +
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
            plot = stackedPlots()$foveal_acuity_plot +
              plt_theme + stacked_theme,
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
          experiment_names(),
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
          experiment_names(),
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
            experiment_names(),
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
          outfile <- tempfile(fileext = '.svg')
          save_plot_with_error_handling(
            plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
            filename = outfile,
            width = 6,
            height = 6,
            size = 5,
            unit = 'in',
            colorPalette = colorPalette,
            plotTitle = scatterDiagrams()$fileNames[[ii]]
          )
        }, deleteFile = TRUE)
        output[[paste0("downloadScatter", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
              scatterDiagrams()$fileNames[[ii]],
              ii,
              '.',
              input$fileType
            ),
            content = function(file) {
              if (input$fileType == "png") {
                tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
                ggsave(
                  tmp_svg,
                  plot = scatterDiagrams()$plotList[[ii]] +
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
                  plot = scatterDiagrams()$plotList[[ii]] +
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
          outfile <- tempfile(fileext = '.svg')
          save_plot_with_error_handling(
            plot = scatterQuality()$plotList[[ii]] + plt_theme_scatter,
            filename = outfile,
            width = 6,
            height = 6,
            size = 5,
            unit = 'in',
            colorPalette = colorPalette,
            plotTitle = scatterQuality()$fileNames[[ii]]
          )
        }, deleteFile = TRUE)
        output[[paste0("downloadQualityScatter", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
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
              outfile <- tempfile(fileext = '.svg')
              ggsave(
                file = outfile,
                plot = scatterTimeParticipant()$plotList[[ii]],
                device = svglite,
                height = 12.5,
                unit = 'in'
              )
              list(src = outfile, contenttype = 'svg')
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
                ggtitle(scatterTimeParticipant()$fileNames[[ii]])
              
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
                alt = paste0("Error in ", scatterTimeParticipant()$fileNames[[ii]])
              )
            })
            
          }, deleteFile = TRUE)
        
        
        output[[paste0("downloadScatterTimeParticipant", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
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
              outfile <- tempfile(fileext = '.svg')
              ggsave(
                file = outfile,
                plot = scatterTime()$plotList[[ii]] +
                  plt_theme_scatter,
                unit = 'in',
                limitsize = F,
              )
              list(src = outfile, contenttype = 'svg')
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
                ggtitle(scatterTime()$fileNames[[ii]])
              
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
                alt = paste0("Error in ", scatterTime()$fileNames[[ii]])
              )
            })
            
          }, deleteFile = TRUE)
        
        output[[paste0("downloadScatterTime", ii)]] <-
          downloadHandler(
            filename = paste0(
              experiment_names(),
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
  

  #### test retest ####
  output$readingTestRetest <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = readingTestRetest() + plt_theme,
      device = svg,
      width = 6,
      height = 4
    )
    
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$crowdingTestRetest <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = crowdingTestRetest() + plt_theme,
      device = svg,
      width = 6,
      height = 4
    )
    
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$rsvpReadingTestRetest <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = rsvpReadingTestRetest() + plt_theme,
      device = svg,
      width = 6,
      height = 4
    )
    
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$readingSpeedRetention <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = readingSpeedRetention() + plt_theme,
      device = svg,
      width = 6,
      height = 4
    )
    
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  #### crowding stair plots
  stairPlot <- reactive({
    plotStaircases(files()$stairs,
                   input$thresholdParameter,
                   conditionNames())
  })
  
  output$stairPlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = stairPlot()$plot + plt_theme,
      width = 8,
      height = stairPlot()$height,
      limitsize = F,
      unit = 'in',
      device = svglite
    )
    list(src = outfile,
         contenttype = 'svg')
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
        outfile   <- tempfile(fileext = ".svg")
        ggsave(file = outfile, plot = p)
        removeModal()
        list(src = outfile, contentType = "image/svg+xml", alt = "autocorrelation")
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = sound_level_plot()[[1]],
        width = sound_level_plot()[[2]],
        height = sound_level_plot()[[3]],
        device = svg,
        units = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = record_freq_component_plot()$plot,
        height = record_freq_component_plot()$height,
        width = 8.5,
        units = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "Power Spectral Density")
    }, deleteFile = TRUE)
    
    output$`power variation` <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = recording_variation()$plot,
        height = recording_variation()$height,
        width = 8,
        units = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "Power Spectral Density")
    }, deleteFile = TRUE)
    
    output$`volume power variation` <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = volume_power_variation()$plot,
        height = volume_power_variation()$height,
        width = 8,
        units = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = componentIIRPlots()$ten +
          sound_theme_display,
        height = 6,
        unit = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR 0 to 10 ms")
    }, deleteFile = TRUE)
    
    output$componentIIR0To50 <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = componentIIRPlots()$fifty +
          sound_theme_display,
        height = 6,
        unit = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR 0 to 50 ms")
    }, deleteFile = TRUE)
    
    output$componentIIR0To400 <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = componentIIRPlots()$schroeder +
          sound_theme_display,
        height = 5,
        unit = "in",
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR schroeder")
    }, deleteFile = TRUE)
    
    
    #### IR ####
    
    output$componentIRPSD <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = componentIR_PSD_plot()$plot,
        height =  componentIR_PSD_plot()$height,
        width = 8
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To6 <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = irPlots()[[1]],
        height = 6,
        unit = "in",
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To50 <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = irPlots()[[2]],
        height = 6,
        unit = "in",
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR two")
    }, deleteFile = TRUE)
    
    output$componentIR0To400 <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = irPlots()[[3]],
        height = 5,
        unit = "in",
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
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
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = p$plot,
        height = p$height,
        width = 8,
        unit = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = p$shiftedPlot,
        height = p$height,
        width = 8,
        unit = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
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
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = p$avgPlot,
        height = p$avgHeight,
        width = 8,
        unit = "in",
        limitsize = FALSE
      )
      list(src = outfile,
           contenttype = 'svg',
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
                   if (!is.null(prolific())) {
                     combinedTable <-
                       combineProlific(prolific(), summary_table(), files()$pretest)[[1]]
                   } else{
                     combinedTable <-
                       combineProlific(NULL, summary_table(), files()$pretest)[[1]]
                   }
                   
                   participants <-
                     unique(combinedTable$`Pavlovia session ID`)
                   prolific_id <-
                     unique(combinedTable$`Prolific participant ID`)
                   output$ex1 <- DT::renderDataTable({
                     render_summary_datatable(combinedTable, participants, prolific_id)
                   })
                   
                   #### stats page ####
                   output$ex2 <-
                     renderTable(threshold_and_warnings()[[2]])
                   output$ex3 <-
                     renderTable(if (nrow(threshold_and_warnings()[[1]]) > 0) {
                       threshold_and_warnings()[[1]]
                     } else {
                       tibble()
                     })
                   output$ex4 <-
                     renderTable(threshold_and_warnings()[[3]])
                   
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
  
  
  #### download handlers ####
  
  
  output$downloadAll = downloadHandler(
    filename = 'plots.zip',
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      
      fileNames <- c()
      
      # Save correlation matrices
      savePlot(corrMatrix()$plot, 
               paste0("correlation-matrix.", input$fileType),
               input$fileType, 
               width = corrMatrix()$width, 
               height = corrMatrix()$height)
      
      savePlot(durationCorrMatrix()$plot, 
               paste0("duration-correlation-matrix.", input$fileType),
               input$fileType, 
               width = durationCorrMatrix()$width, 
               height = durationCorrMatrix()$height)
      
      fileNames <- c(
        paste0("correlation-matrix.", input$fileType),
        paste0("duration-correlation-matrix.", input$fileType)
      )
      
      # Save histograms
      if (length(histograms()$plotList) > 0) {
        for (i in seq_along(histograms()$plotList)) {
          plotFileName <- paste0(histograms()$fileNames[[i]], '.', input$fileType)
          savePlot(histograms()$plotList[[i]] + plt_theme, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save scatter diagrams
      if (length(scatterDiagrams()$plotList) > 0) {
        for (i in seq_along(scatterDiagrams()$plotList)) {
          plotFileName <- paste0(scatterDiagrams()$fileNames[[i]], '.', input$fileType)
          savePlot(scatterDiagrams()$plotList[[i]] + plt_theme_scatter, plotFileName, input$fileType)
          fileNames <- c(fileNames, plotFileName)
        }
      }
      
      # Save age plots
      if (length(agePlots()$plotList) > 0) {
        for (i in seq_along(agePlots()$plotList)) {
          plotFileName <- paste0(agePlots()$fileNames[[i]], '.', input$fileType)
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
          print(i)
          print(ggiraph_plots$fileNames[[i]])
          if (!is.null(ggiraph_plots$plotList[[i]])) {
            plotFileName <- paste0(ggiraph_plots$fileNames[[i]], '.', input$fileType)
            savePlot(ggiraph_plots$plotList[[i]], plotFileName, input$fileType, theme = plt_theme_ggiraph)
            fileNames <- c(fileNames, plotFileName)
          }
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
      if (!is.null(prolific())) {
        combinedTable <-
          combineProlific(prolific(), summary_table(), files()$pretest)[[1]]
      } else{
        combinedTable <-
          combineProlific(NULL, summary_table(), files()$pretest)[[1]]
      }
      openxlsx::write.xlsx(combinedTable %>% select(-order), file = filename)
    }
  )
  
  output$report <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "error report.html",
        paste0(experiment_names(), ".html")
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
        paste0(experiment_names(), "-Summary-of-each-condition.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(threshold_and_warnings()[[2]], file = filename)  # Using openxlsx
    }
  )
  
  output$thresholdTwo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Thresholds(brief).xlsx",
        paste0(experiment_names(), "-Thresholds(brief).xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(threshold_and_warnings()[[3]], file = filename)  # Using openxlsx
    }
  )
  
  output$thresholdThree <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Thresholds.xlsx",
        paste0(experiment_names(), "-Thresholds.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(threshold_and_warnings()[[4]], file = filename)  # Using openxlsx
    }
  )
  
  output$`sound_data` <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "sound-calibration.xlsx",
        # Change file extension to .xlsx
        paste0(experiment_names(), "sound-calibration.xlsx")
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
              experiment_names(),
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
                  plot = scatterDiagrams()$plotList[[ii]] +
                    plt_theme_scatter,
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
    
    output$downloadRsvpCrowdingPeripheralGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-grade.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        plot <- rsvpCrowding()$p_grade + plt_theme
        savePlot(
          plot = plot,
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
        paste0('rsvp-vs-peripheral-crowding-by-font', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        plot <- rsvpCrowding()$p_font + plt_theme
        savePlot(
          plot = plot,
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
        plot <- rsvpCrowding()$residual + plt_theme
        savePlot(
          plot = plot,
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
        paste0(experiment_names(),
               'rsvp-vs-fovel-acuity-by-grade.',
               input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpAcuityFoveal()[[2]] +
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
            plot = rsvpAcuityFoveal()[[2]] +
              plt_theme,
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
        paste0(experiment_names(),
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
          paste0(experiment_names(),
                 'rsvp-vs-periphreal-acuity-by-grade.',
                 input$fileType)
        },
        content = function(file) {
          savePlot(plot = rsvpAcuityPeripheral()$grade + plt_theme,
                   fileName = file,
                   fileType = input$fileType,
                   width = 8,
                   height = 6
                   )
        }
      )
    
    output$downloadRsvpPeripheralAcuityFontPlot <-
      downloadHandler(
        filename = function () {
          paste0(experiment_names(),
                 'rsvp-vs-periphreal-acuity-by-font',
                 input$fileType)
        },
        content = function(file) {
          if (input$fileType == "png") {
            ggsave(
              "tmp.svg",
              plot = rsvpAcuityPeripheral()$font +
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
              plot = rsvpAcuityPeripheral()$font +
                plt_theme,
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
      filename = paste(
        app_title$default,
        paste0('reading-vs-peripheral-acuity-by-grade.', input$fileType),
        sep = "-"
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
      filename = paste(
        app_title$default,
        paste0('reading-vs-peripheral-acuity-by-font.', input$fileType),
        sep = "-"
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
            experiment_names(),
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
            experiment_names(),
            "ordinary-reading-vs-foveal-crowding-by-grade.",
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
            ggsave(
              "tmp.svg",
              plot = ordinaryCrowdingPlots()[[4]] + plt_theme,
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
              plot =  ordinaryCrowdingPlots()[[4]] + plt_theme,
              device = svg,
              width = 6,
            )
          }
        }
      )
    
    output$downloadOrdinaryPeripheralCrowdingFontPlot <-
      downloadHandler(
        filename = function() {
          paste0(
            experiment_names(),
            "ordinary-reading-vs-peripheral-crowding-by-font.",
            input$fileType
          )
        },
        content = function(file) {
          if (input$fileType == "png") {
            ggsave(
              "tmp.svg",
              plot = ordinaryCrowdingPlots()[[1]] +
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
              plot =  ordinaryCrowdingPlots()[[1]] +
                plt_theme,
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
            experiment_names(),
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
      filename = paste0('correlation-matrix',
                        '.',
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
      filename = function() paste0("n-matrix.", input$fileType),
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
      filename = paste0('duration-correlation-matrix',
                        '.',
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
    
    output$downloadCrowdingAvgPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('average_crowding_left_vs_right.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = crowdingAvgPlot(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadSloanVsTimesMeanPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('2_fonts_mean.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = two_fonts_plots()[[1]] +
            labs(
              title = experiment_names(),
              subtitle = paste0("Crowding ", two_fonts_plots()$title, ", mean")
            ) +
            plt_theme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    
    #### download sloan vs times ####
    
    output$downloadReadingSpeedRetention <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-speed-vs-retention.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = readingSpeedRetention(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadReadingTestRetest <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-test-retest.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = readingTestRetest(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadCrowdingTestRetest <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('crowding-test-retest.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = crowdingTestRetest(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadRsvpReadingTestRetest <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-test-retest.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = rsvpReadingTestRetest(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
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
