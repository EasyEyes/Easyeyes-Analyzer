#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize = 500 * 1024 ^ 2)
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
library(plotly)
library(grid)
# library(showtext)
# library(systemfonts)
# Enables automatic font loading for showtext

# showtext_auto(F)


source('load_fonts.R')
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

source("./other/getBits.R")
source("./other/sound_plots.R")
source("./other/read_json.R")
source("./other/formSpree.R")
source("./other/utility.R")


#### server code ####

shinyServer(function(input, output, session) {

  #### formSpree ####
  formSpreeTable <- reactive(monitorFormSpree(input$listFontParameters))
  
  output$formSpreeDashboard <- renderDataTable({
    datatable(formSpreeTable(),
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
                columnDefs = list(
                  list(visible = FALSE, targets = c(0, ncol(formSpreeTable())))))
    ) %>% 
      formatStyle(names(formSpreeTable()),
                  'hl',
                  backgroundColor = styleEqual(c(T,F), c('yellow','white')))
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
    t <- read_files(input$file)  
    return(t)
  })
  
  # text output to show the file upload status
  output$fileStatusMessage <- renderText({
    t <- files()  
    paste0(
      'Analyzed ', length(t$data_list), ' sessions and ',
      ifelse(nrow(t$pretest) > 0, 1, 0), ' pretest file'
    )
  })
  
  
  
  # stairPlots <- reactive({
  #   require(input$file)
  #   t <- getStairsPlot(input$file)
  #   return(t)
  # })
  
  prolific <- reactive({
    t <- find_prolific_from_files(input$file)
    return(t)
  })
  data_list <- reactive({
    t <- files()[[1]]
    return(t)
  })
  summary_list <- reactive({
    t <- files()[[2]]
    return(t)
  })
  experiment_names <- reactive({
    return(trimws(files()[[3]]))
  })
  
  readingCorpus <- reactive({
    return(trimws(files()[[4]]))
  })
  
  app_title <- reactiveValues(default = "EasyEyes Analyze")
  output$app_title <- renderText({
    "EasyEyes Analyze"
  })
  
  #### reactive dataframes ####
  
  summary_table <- reactive({
    generate_summary_table(files()$data_list)
  })
  
  threshold_and_warnings <- reactive({
    return(generate_threshold(files()$data_list, summary_list(), files()$pretest, files()$stairs, files()$df))
  })
  
  minNQuestTrials <- reactive({input$NQuestTrials}) %>% debounce(1000)
  df_list <- reactive({
    return(generate_rsvp_reading_crowding_fluency(files()$data_list, summary_list(), files()$pretest, files()$stairs, input$filterInput, minNQuestTrials()))
  })
  
  crowdingBySide <- reactive({
    crowding_by_side(df_list()$crowding)
  })
  
  reading_rsvp_crowding_df <- reactive({
    return(get_mean_median_df(df_list()))
  })
  
  corrMatrix <- reactive({
    getCorrMatrix(df_list(), files()$pretest)
  })
  
  durationCorrMatrix <- reactive({
    get_duration_corr(files()$data_list)
  }) 
  
  #### duration data ####
  
  durationData <- reactive({
    get_duration_data(files()$data_list)
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
  
  record_freq_system_plot <- reactive({
    plot_record_freq_system(sound_data())
  })
  
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
      system =  paste0(
        "Filtered MLS: ",
        format(
          round(
            inputParameters$calibrateSoundAttenuationSpeakerAndMicDb,
            1
          ),
          nsmall = 1
        ),
        " dB, ampl. ",
        round(inputParameters$filteredMLSMaxAbsSystem, 1),
        ", ",
        inputParameters$calibrateSoundMinHz,
        " – ",
        inputParameters$fMaxHzSystem,
        " Hz, ",
        format(
          round(
            inputParameters$attenuatorDBSystem,
            1
          ),
          nsmall = 1
        ),
        " dB atten."
      ),
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
        format(
          round(
            inputParameters$attenuatorDBComponent,
            1
          ),
          nsmall = 1
        ),
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
    crowding_scatter_plot(crowdingBySide())
  })
  
  crowdingAvgPlot <- reactive({
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
    plot_rsvp_crowding(df_list())
  })
  
  rsvpAcuityFoveal <- reactive({
    plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp,'foveal')
  })
  
  rsvpAcuityPeripheral <- reactive({
    plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp,'peripheral')
  })
  
 rsvp_repeated_letter_crowding <- reactive({
    plot_rsvp_repeated_letter_crowding(df_list())
  })
  
  two_fonts_plots <- reactive({
    get_two_fonts_plots(df_list()$crowding)
  })
  
  crowding_hist <- reactive({
    get_crowding_hist(df_list()$crowding)
  })
  
  acuity_hist <- reactive({
    get_acuity_hist(df_list()$acuity)
  })
  
  
  repeated_letter_hist <- reactive({
    get_repeatedLetter_hist(df_list()$repeatedLetters)
  })
  
  foveal_peripheral_diag <- reactive({
    get_foveal_peripheral_diag(df_list()$crowding)
  })
  
  
  foveal_crowding_vs_acuity_diag <- reactive({
    get_foveal_acuity_diag(df_list()$crowding, df_list()$acuity)
  })
  
  regressionFontPlot <- reactive({
    regression_font(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Reading vs crowding")
  })
  
  regressionFontPlotWithLabel <- reactive({
    regression_font_with_label(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Reading vs crowding")
  })
  
  readingTestRetest <- reactive({
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
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    print('inside agePlots')
    i = 1
    l <- list()
    fileNames <- list()
    
    t <- get_foveal_crowding_vs_age(df_list()$crowding)
   
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-age-by-grade'
      i = i + 1
    }
    t <- get_peripheral_crowding_vs_age(df_list()$crowding)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'peripheral-crowding-vs-age-by-grade'
      i = i + 1
    }
    t <- get_repeatedLetter_vs_age(df_list()$repeatedLetters)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'repeated-letter-crowding-vs-age-by-grade'
      i = i + 1
    }
    t <- plot_reading_age(df_list()$reading)
 
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'reading-vs-age-by-grade'
      i = i + 1
    }
    t <- plot_rsvp_age(df_list()$rsvp)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'rsvp-reading-vs-age-by-grade'
      i = i + 1
    }
    t <- get_foveal_acuity_vs_age(df_list()$acuity)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-acuity-vs-age'
      i = i + 1
    }

    t <- get_peripheral_acuity_vs_age(df_list()$acuity)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'peripheral-acuity-vs-age'
      i = i + 1
    }
    # t <- get_crowding_vs_repeatedLetter(df_list()$crowding, 
    #                                     df_list()$repeatedLetters)$age
    # 
    # if (!is.null(t)) {
    #   l[[i]] = t
    #   fileNames[[i]] = 'crowding-vs-repeated-letters-crowding-age'
    #   i = i + 1
    # }
    
    t <- get_crowding_vs_repeatedLetter(df_list()$crowding, 
                                        df_list()$repeatedLetters)$grade

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'crowding-vs-repeated-letters-crowding-grade'
      i = i + 1
    }
    
   
   
    print('done age plots')
    return(list(
      plotList = l,
      fileNames = fileNames
    ))
  })
  
  histograms <- reactive({
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    print('inside histograms')
    i = 1
    l <- list()
    fileNames <- list()

    if (!is.null(acuity_hist()[[1]])) {
      l[[i]] =  acuity_hist()[[1]]
      fileNames[[i]] = 'foveal-acuity-histogram'
      i = i + 1
    }

    if (!is.null(acuity_hist()[[2]])) {
      l[[i]] = acuity_hist()[[2]]
      fileNames[[i]] = 'peripheral-acuity-histogram'
      i = i + 1
    }
    

    if (!is.null(crowding_hist()$foveal)) {
      l[[i]] = crowding_hist()$foveal
      fileNames[[i]] = 'foveal-crowding-histogram'
      i = i + 1
    }

    if (!is.null(crowding_hist()$peripheral)) {
      l[[i]] = crowding_hist()$peripheral
      fileNames[[i]] = 'peripheral-crowding-histogram'
      i = i + 1
    }
    
    t <- get_reading_hist(df_list()$rsvp)
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'rsvp-reading-speed-histogram'
      i = i + 1
    }
    
    t <- get_reading_hist(df_list()$reading)
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'reading-speed-histogram'
      i = i + 1
    }

    if (!is.null(repeated_letter_hist())) {
      l[[i]] = repeated_letter_hist()
      fileNames[[i]] = 'repeated-letter-crowding-histogram'
      i = i + 1
    }
    
    # t <- get_fluency_histogram(df_list()$fluency)
    # if (!is.null(t)) {
    #   l[[i]] = t
    #   fileNames[[i]] = 'english-fluency-histogram'
    #   i = i + 1
    # }
    # 
    # t <- get_reading_retention_histogram(df_list()[[1]])
    # if (!is.null(t)) {
    #   l[[i]] = t
    #   fileNames[[i]] = 'reading-retention-histogram'
    #   i = i + 1
    # }
    print('before appending')
    lists = append_hist_list(data_list(),l,fileNames)
    return(lists)
})

timingHistograms <- reactive({
  if (is.null(input$file)) {
    return(list(
      plotList = list(),
      fileNames = list()
    ))
  }
  
  print('Generating timing histograms')
  l <- list()
  fileNames <- list()
  
  lists <- append_hist_time(data_list(), l, fileNames)
  return(lists)
})


  
  #### stacked histograms #### 
  stackedPlots <- reactive({
    if (is.null(df_list())) {
      return(NULL)
    }
    
    # Generate the stacked plots
    stacked <- generate_histograms_by_grade(df_list())
    
    # Return all plots, including the new ones
    return(list(
      rsvp_plot = stacked$rsvp_reading_plot,
      crowding_plot = stacked$peripheral_crowding_plot,
      foveal_acuity_plot = stacked$foveal_acuity_plot,
      foveal_crowding_plot = stacked$foveal_crowding_plot,
      foveal_repeated_plot = stacked$foveal_repeated_plot,
      peripheral_acuity_plot = stacked$peripheral_acuity_plot
    ))
  })
  
  #### scatterDiagrams ####
  
  scatterDiagrams <- reactive({
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    print('inside scatter plots')
    i = 1
    l <- list()
    fileNames <- list()
    
    crowdingPlots <- plotCrowdingStaircasesVsQuestTrials(df_list(), files()$stairs)
    
    t <- plot_distance(files()$data_list)
    
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'calibrateTrackDistanceMeasuredCm-vs-calibrateTrackDistanceRequestedCm-plot'
      i = i + 1
    }
    
    # Add foveal plot to the list
    if (!is.null(crowdingPlots$fovealPlot)) {
      l[[i]] <- crowdingPlots$fovealPlot + plt_theme
      fileNames[[i]] <- 'foveal-crowding-staircases-threshold-vs-questTrials'
      print(fileNames[[i]])
      i <- i + 1
    }
    
    # Add peripheral plot to the list
    if (!is.null(crowdingPlots$peripheralPlot)) {
      l[[i]] <- crowdingPlots$peripheralPlot + plt_theme
      fileNames[[i]] <- 'peripheral-crowding-staircases-threshold-vs-questTrials'
      print(fileNames[[i]])
      i <- i + 1
    }
    
    t <- foveal_crowding_vs_acuity_diag()$foveal$grade
    
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-foveal-acuity-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <-  foveal_crowding_vs_acuity_diag()$peripheral$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-acuity-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- get_acuity_foveal_peripheral_diag(df_list()$acuity)
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-acuity-vs-peripheral-acuity-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- foveal_peripheral_diag()$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-crowding-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- peripheral_plot(df_list())
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'peripheral-acuity-vs-peripheral-crowding-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- crowdingPlot()
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'peripheral_crowding_left_vs_right'
      print(fileNames[[i]])
      i = i + 1
    }
    
    # t <- get_quest_diag(df_list()$quest)$age
    # if (!is.null(t)) {
    #   l[[i]] = t
    #   fileNames[[i]] = 'quest-sd-vs-mean-age-diagram'
    #   i = i + 1
    # }
    
    t <- get_quest_diag(df_list()$quest)$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'quest-sd-vs-mean-grade-diagram'
      print(fileNames[[i]])
      i = i + 1
    }
    
   t <- get_quest_sd_vs_trials(df_list()$quest)
    if (!is.null(t)) {
      l[[i]] <- t
      fileNames[[i]] <- 'quest-sd-vs-quest-trials-grade-diagram'
      print(fileNames[[i]])
      i <- i + 1
    }
    
    t <- regression_reading_plot(df_list())
    if (!is.null(t$foveal)) {
      l[[i]] = t$foveal
      fileNames[[i]] = 'reading-rsvp-reading-vs-foveal-crowding'
      print(fileNames[[i]])
      i = i + 1
    }
    
    if (!is.null(t$peripheral)) {
      l[[i]] = t$peripheral
      fileNames[[i]] = 'reading-rsvp-reading-vs-peripheral-crowding'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- regression_acuity_plot(df_list())
    
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'ordinary-reading-rsvp-reading-vs-acuity'
      print(fileNames[[i]])
      i = i + 1
    }
    
    t <- plot_reading_rsvp(df_list()$reading, df_list()$rsvp)
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'reading-vs-RSVP-reading-plot'
      print(fileNames[[i]])
      i = i + 1
    }
    lists = append_scatter_list(data_list(), l, fileNames)
    return(lists)
  })
  scatterTime <- reactive({
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    
    print("inside ScatterTime")
    i <- 1
    l <- list()
    fileNames <- list()
    
    # Extract scatter plots using the append_scatter_time function
    scatter_time_plots <- append_scatter_time(data_list(), l, fileNames)
    
    return(scatter_time_plots)
  })

  
  #### plotly plots ####
  rsvpPlotlyPlots <-  reactive({
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    print('inside plotly plots')
    i = 1
    l <- list()
    fileNames <- list()
    
    repeatedLetter_rsvp <- plot_rsvp_repeated_letter_crowding(df_list())
    
    print('plot_rsvp_repeated_letter_crowding')
    if (!is.null(repeatedLetter_rsvp[[1]])) {
      l[[i]] <- repeatedLetter_rsvp[[1]] + plt_theme  
      fileNames[[i]] <- 'rsvp-foveal-repeated-letter-crowding-grade'
      i <- i + 1
    }
    
    if (!is.null(repeatedLetter_rsvp[[2]])) {
      l[[i]] <- repeatedLetter_rsvp[[2]] + plt_theme
      fileNames[[i]] <- 'rsvp-peripheral-repeated-letter-crowding-grade'
      i <- i + 1
    }
    
    t <- plot_rsvp_crowding(df_list())
    
    print('plot_rsvp_crowding')
    if (!is.null(t[[3]])) {
      l[[i]] <- t[[3]] + plt_theme
      fileNames[[i]] <- 'rsvp-vs-peripheral-crowding-by-grade'
      i <- i + 1
    }
    
    if (!is.null(t[[4]])) {
      l[[i]] <- t[[4]] + plt_theme
      fileNames[[i]] <- 'rsvp-vs-foveal-crowding-by-grade'
      i <- i + 1
    }
    
    print('plot_acuity_rsvp')
    t <- plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp,'foveal')
    if (!is.null(t[[2]])) {
      l[[i]] <- t[[2]] + plt_theme
      fileNames[[i]] <- 'rsvp-vs-foveal-acuity-by-grade'
      i <- i + 1
    }
    t <- plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp,'peripheral')
    if (!is.null(t[[2]])) {
      l[[i]] <- t[[2]] + plt_theme
      fileNames[[i]] <- 'rsvp-vs-peripheral-acuity-by-grade'
      i <- i + 1
    }
    
    print('factor_out_age_and_plot')
    factored_age_plot <- factor_out_age_and_plot(df_list())
    if (!is.null(factored_age_plot)) {
      l[[i]] <- factored_age_plot + plt_theme
      fileNames[[i]] <- 'rsvp-vs-crowding-factored-age'
      i <- i + 1
    }
    
    return(list(
      plotList = l,
      fileNames=fileNames
    ))
  })
  
  readingPlotlyPlots <-  reactive({
    if (is.null(input$file)) {
      return(list(
        plotList = list(),
        fileNames = list()
      ))
    }
    print('inside reading plotly plots')
    i = 1
    l <- list()
    fileNames <- list()
    
    repeatedLetter_reading <- plot_reading_repeated_letter_crowding(df_list())
    
    if (!is.null(repeatedLetter_reading[[1]])) {
      l[[i]] <- repeatedLetter_reading[[1]] + plt_theme
      fileNames[[i]] <- 'rsvp-foveal-repeated-letter-crowding-grade'
      i <- i + 1
    }
    
    if (!is.null(repeatedLetter_reading[[2]])) {
      l[[i]] <- repeatedLetter_reading[[2]] + plt_theme
      fileNames[[i]] <- 'rsvp-peripheral-repeated-letter-crowding-grade'
      i <- i + 1
    }
    
    print('plot_acuity_reading')
    t <- plot_acuity_reading(df_list()$acuity, df_list()$reading, 'foveal')[[2]]
    
    if (!is.null(t)) {
      l[[i]] <- t + plt_theme
      fileNames[[i]] <- 'reading-vs-foveal-acuity-grade'
      i <- i + 1
    }
    
    t <- plot_acuity_reading(df_list()$acuity, df_list()$reading, 'peripheral')[[2]]
    
    if (!is.null(t)) {
      l[[i]] <- t + plt_theme
      fileNames[[i]] <- 'reading-vs-peripheral-acuity-grade'
      i <- i + 1
    }
    
    print('plot_reading_crowding')
    t <- plot_reading_crowding(df_list())
    
    if (!is.null(t[[4]])) {
      l[[i]] <- t[[4]] + plt_theme
      fileNames[[i]] <- 'reading-vs-foveal-crowding-by-grade'
      i <- i + 1
    }
    
    if (!is.null(t[[3]])) {
      l[[i]] <- t[[3]] + plt_theme
      fileNames[[i]] <- 'reading-vs-peripheral-crowding-by-grade'
      i <- i + 1
    }
    
    return(list(
      plotList = l,
      fileNames=fileNames
    ))
  })
  
  gradePlots <- reactive({
    plot_rsvp_crowding_acuity(df_list())
  })
  
  output$isPeripheralAcuity <- reactive({
    if ('acuity' %in% names(df_list())) {
      peripheral <- df_list()$acuity %>% filter(targetEccentricityXDeg != 0)
      return(nrow(peripheral) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isRsvp<- reactive({
    if ('rsvp' %in% names(df_list()) ) {
      return(nrow(df_list()$rsvp) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isRepeated <- reactive({
    if ('repeatedLetters' %in% names(df_list()) ) {
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
  
  output$isAcuity <- reactive({
    if ('acuity' %in% names(df_list())) {
      return(nrow(df_list()$acuity) > 0)
    } else {
      return(FALSE)
    }
  })
  
  output$isDuration <- reactive({
    return(nrow(durationData()) > 0)
  })
  
  
  output$fileUploaded <- reactive({
    return(nrow(files()$pretest>0))
  })
  
  output$questData <- reactive({
    if ('quest' %in% names(df_list())) {
      return(nrow(df_list()$quest>0))
    }
    return(FALSE)
  })
  
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'questData', suspendWhenHidden=FALSE)
  outputOptions(output, 'isPeripheralAcuity', suspendWhenHidden=FALSE)
  outputOptions(output, 'isReading', suspendWhenHidden=FALSE)
  outputOptions(output, 'isRsvp', suspendWhenHidden=FALSE)
  outputOptions(output, 'isRepeated', suspendWhenHidden=FALSE)
  outputOptions(output, 'isCrowding', suspendWhenHidden=FALSE)
  outputOptions(output, 'isAcuity', suspendWhenHidden=FALSE)
  outputOptions(output, 'isDuration', suspendWhenHidden=FALSE)

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
  
  
  duration_lateness_hist <- reactive({
    get_histogram_duration_lateness(durationData())
  })
  
  output$durationHist <- renderImage({
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
  }, deleteFile = TRUE)
  
  output$latenessHist <- renderImage({
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
  }, deleteFile = TRUE)
  
  durationPlot <- reactive({
    plot_duraction_sec(durationData())
  })
  
  latenessPlot <- reactive({
    plot_Lateness_sec(durationData())
  })
  
  output$durationByID <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  durationPlot()$participant +
        scale_color_manual(values= colorPalette),
      device = svglite,
      unit = 'in'
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$durationByFont <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  durationPlot()$font + 
        scale_color_manual(values= colorPalette),
      device = svglite,
      unit = 'in'
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$latenessByID <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =latenessPlot()$participant +
        scale_color_manual(values= colorPalette),
      device = svglite,
      unit = 'in'
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  output$latenessByFont <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot = latenessPlot()$font +
        scale_color_manual(values= colorPalette),
      device = svglite,
      unit = 'in'
    )
    list(src = outfile,
         contenttype = 'svg')
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
          labs(title = experiment_names(),
               subtitle = paste0("Crowding ", two_fonts_plots()$title, ", mean")) +
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
        labs(title = experiment_names(),
             subtitle = paste0("Crowding ", two_fonts_plots()$title, ", sd")) +
        plt_theme,
      device = svg,
      width = 7,
      height = 4
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
  #### rsvp plotlys ####
  output$rsvpCrowdingPeripheralAgePlot <- renderPlotly({
    rsvpCrowding()[[1]]
  })
  output$rsvpCrowdingPeripheralGradePlot <- renderPlotly({
    rsvpCrowding()[[3]]
  })
  output$rsvpResidualCrowding <- renderPlotly({
    rsvpCrowding()[[4]]
  })
  output$rsvpCrowdingFovealGradePlot <- renderPlotly({
    rsvpCrowding()[[5]]
  })
  
  output$rsvpFovealAcuityGradePlot <- renderPlotly({
    rsvpAcuityFoveal()[[2]]
  })
  output$factorOutAgePlot <- renderPlotly({
    factor_out_age_and_plot(df_list())
  })
  
  output$rsvpPeripheralAcuityGradePlot <- renderPlotly({
    ggplotly(rsvpAcuityPeripheral()[[2]]) %>% layout()
  })
  output$rsvpRepeatedGradePlot <- renderPlotly({
    rsvp_repeated_letter_crowding()[[2]]
  })
  
  #### reading plotlys ####
  
  ordinaryAcuityFoveal <- reactive({
    plot_acuity_reading(df_list()$acuity, df_list()$reading, 'foveal')
  })
  
  ordinaryAcuityPeripheral <- reactive({
    plot_acuity_reading(df_list()$acuity, df_list()$reading, 'peripheral')
  })
  
  output$ordinaryFovealAcuityGradePlot <- renderPlotly({
    ordinaryAcuityFoveal()[[2]]  # Second plot (e.g., grade-based)
  })
  
  output$ordinaryPeripheralAcuityGradePlot <- renderPlotly({
    ordinaryAcuityPeripheral()[[2]]  # Second plot (e.g., grade-based)
  })
  
  ordinaryCrowdingPlots <- reactive({
    plot_reading_crowding(df_list())
  })
  
  readingRepeatedPlots <- reactive({
    plot_reading_repeated_letter_crowding(df_list())
  })
  
  # Peripheral Crowding Plots
  output$ordinaryPeripheralCrowdingAgePlot <- renderPlotly({
    ordinaryCrowdingPlots()[[1]]  # Age-based peripheral crowding plot
  })
  
  output$ordinaryPeripheralCrowdingGradePlot <- renderPlotly({
    ordinaryCrowdingPlots()[[3]]  # Grade-based peripheral crowding plot
  })
  
  # Foveal Crowding Plots
  output$ordinaryFovealCrowdingAgePlot <- renderPlotly({
    ordinaryCrowdingPlots()[[2]]  # Age-based foveal crowding plot
  })
  
  output$ordinaryFovealCrowdingGradePlot <- renderPlotly({
    ordinaryCrowdingPlots()[[4]]  # Grade-based foveal crowding plot
  })
  
  output$readingRepeatedGradePlot <- renderPlotly({
    readingRepeatedPlots()[[2]]
  })
  
  #### age plots ####
  output$plots <- renderUI({
    out <- list()
    i = 1
    while(i <= length(agePlots()$plotList)-1) {
      out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
                               shinycssloaders::withSpinner(plotOutput(paste0("p", i), width = "100%", height = "100%"), type = 4),
                               shinycssloaders::withSpinner(plotOutput(paste0("p", i+1), width = "100%", height = "100%"), type = 4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadP", i), 'Download'),
                                  downloadButton(paste0("downloadP", i+1), 'Download'))
      i = i + 2
    }
    if (i == length(agePlots()$plotList)){
      out[[i]] <-splitLayout(cellWidths = c("50%", "50%"),
                             shinycssloaders::withSpinner(plotOutput(paste0("p", i), width = "100%", height = "100%"),type = 4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadP", i), 'Download'))
    }
    for (j in 1:length(agePlots()$plotList)) {
      local({
        ii <- j
        output[[paste0("p", ii)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(
            file = outfile,
            plot = agePlots()$plotList[[ii]] + plt_theme,
            width = 6,
            unit = 'in',
            device = svglite
          )
          
          list(src = outfile,
               contenttype = 'svg')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadP", ii)]] <- downloadHandler(
          filename = paste0(
            experiment_names(),
            agePlots()$fileNames[[ii]],
            ii,
            '.',
            input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
              tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
              ggsave(
                tmp_svg,
                plot = agePlots()$plotList[[ii]] + plt_theme,
                limitsize = F,
                width = 6,
                unit = "in",
                device = svglite
              )
              rsvg::rsvg_png(tmp_svg, file,
                             height = 1800,
                             width = 1800)
            } else {
              ggsave(
                file,
                plot = agePlots()$plotList[[ii]] + plt_theme,
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
    return(out)
  })
  
  # output$histograms <- renderUI({
  #   out <- list()
  #   i = 1
  #   
  #   while(i <= length(histograms()$plotList)-1) {
  #     
  #     out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
  #                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i), width = "50%", height = "50%"), type = 4),
  #                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i+1), width = "50%", height = "50%"), type = 4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadHist", i), 'Download'),
  #                                 downloadButton(paste0("downloadHist", i+1), 'Download'))
  #     i = i + 2
  #   }
  #   if (i == length(histograms()$plotList)){
  #     out[[i]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                             shinycssloaders::withSpinner(plotOutput(paste0("hist", i), width = "50%", height = "50%"), type = 4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadHist", i), 'Download'))
  #   }
  #   for (j in 1:length(histograms()$plotList)) {
  #     
  #     local({
  #       ii <- j
  #       
  #       output[[paste0("hist", ii)]] <- renderImage({
  #         outfile <- tempfile(fileext = '.svg')
  #         ggsave(
  #           file = outfile,
  #           plot = histograms()$plotList[[ii]] + plt_theme,
  #           device = svglite,
  #           width = 5,
  #           height=5,
  #           unit = 'in',
  #         )
  #         
  #         list(src = outfile,
  #              contenttype = 'svg')
  #       }, deleteFile = TRUE)
  #       
  #       output[[paste0("downloadHist", ii)]] <- downloadHandler(
  #         filename = paste0(
  #           experiment_names(),
  #           histograms()$fileNames[[ii]],
  #           ii,
  #           '.',
  #           input$fileType
  #         ),
  #         content = function(file) {
  #           if (input$fileType == "png") {
  #             ggsave(
  #               "tmp.svg",
  #               plot = histograms()$plotList[[ii]] + plt_theme,
  #               unit = "in",
  #               width = 6,
  #               limitsize = F,
  #               device = svglite
  #             )
  #             rsvg::rsvg_png("tmp.svg", file,
  #                            height = 1800,
  #                            width = 1800)
  #           } else {
  #             ggsave(
  #               file,
  #               plot = histograms()$plotList[[ii]] + plt_theme,
  #               unit = "in",
  #               limitsize = F,
  #               device = ifelse(
  #                 input$fileType == "svg",
  #                 svglite::svglite,
  #                 input$fileType
  #               )
  #             )
  #           }
  #         }
  #       )
  #     })
  #   }
  #   return(out)
  # })
  output$histograms <- renderUI({
    out <- list()
    i <- 1
    
    while (i <= length(histograms()$plotList) - 3) {
      # Create a row with 4 histograms
      out[[i]] <- splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i + 1), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i + 2), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i + 3), width = "100%", height = "100%"), type = 4))
      # Create a row with 4 download buttons
      out[[i + 1]] <- splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                                  downloadButton(paste0("downloadHist", i), 'Download'),
                                  downloadButton(paste0("downloadHist", i + 1), 'Download'),
                                  downloadButton(paste0("downloadHist", i + 2), 'Download'),
                                  downloadButton(paste0("downloadHist", i + 3), 'Download'))
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(histograms()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(paste0("hist", i + j - 1), width = "100%", height = "100%"), type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(paste0("downloadHist", i + j - 1), 'Download')
      })
      
      # Fill remaining cells with empty space
      emptyCells <- 4 - remaining
      plotOutputs <- c(plotOutputs, rep("", emptyCells))
      downloadButtons <- c(downloadButtons, rep("", emptyCells))
      
      out[[length(out) + 1]] <- do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), plotOutputs))
      out[[length(out) + 1]] <- do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), downloadButtons))
    }
    
    # Render histograms and download handlers
    for (j in seq_along(histograms()$plotList)) {
      local({
        ii <- j
        output[[paste0("hist", ii)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(
            file = outfile,
            plot = histograms()$plotList[[ii]] + hist_theme,
            device = svglite,
            width = 4,  # Reduced width
            height = 3.5,  # Reduced height
            unit = 'in'
          )
          list(src = outfile, contenttype = 'svg')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadHist", ii)]] <- downloadHandler(
          filename = paste0(
            experiment_names(),
            histograms()$fileNames[[ii]],
            ii,
            '.',
            input$fileType
          ),
          content = function(file) {
            if (input$fileType == "png") {
              tmp_svg <- tempfile(tmpdir = tempdir(), fileext = ".svg")
              ggsave(
                tmp_svg,
                plot = histograms()$plotList[[ii]] + hist_theme,
                unit = "in",
                width = 4,  # Reduced width
                height = 3.5,  # Reduced height
                limitsize = F,
                device = svglite
              )
              rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)  # Reduced resolution
            } else {
              ggsave(
                file,
                plot = histograms()$plotList[[ii]] + hist_theme,
                width = 4,  # Reduced width
                height = 3.5,  # Reduced height
                unit = "in",
                limitsize = F,
                device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType)
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
      out[[i]] <- splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                              shinycssloaders::withSpinner(plotOutput(paste0("timingHist", i), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("timingHist", i + 1), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("timingHist", i + 2), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("timingHist", i + 3), width = "100%", height = "100%"), type = 4))
      # Create a row with 4 download buttons
      out[[i + 1]] <- splitLayout(cellWidths = c("25%", "25%", "25%", "25%"),
                                  downloadButton(paste0("downloadTimingHist", i), 'Download'),
                                  downloadButton(paste0("downloadTimingHist", i + 1), 'Download'),
                                  downloadButton(paste0("downloadTimingHist", i + 2), 'Download'),
                                  downloadButton(paste0("downloadTimingHist", i + 3), 'Download'))
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(timingHistograms()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(paste0("timingHist", i + j - 1), width = "100%", height = "100%"), type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(paste0("downloadTimingHist", i + j - 1), 'Download')
      })
      
      # Fill remaining cells with empty space
      emptyCells <- 4 - remaining
      plotOutputs <- c(plotOutputs, rep("", emptyCells))
      downloadButtons <- c(downloadButtons, rep("", emptyCells))
      
      out[[length(out) + 1]] <- do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), plotOutputs))
      out[[length(out) + 1]] <- do.call(splitLayout, c(list(cellWidths = rep("25%", 4)), downloadButtons))
    }
    
    # Render histograms and download handlers
    for (j in seq_along(timingHistograms()$plotList)) {
      local({
        ii <- j
        output[[paste0("timingHist", ii)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(
            file = outfile,
            plot = timingHistograms()$plotList[[ii]] + hist_theme,
            device = svglite,
            width = 4,  # Reduced width
            height = 3.5,  # Reduced height
            unit = 'in'
          )
          list(src = outfile, contenttype = 'svg')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadTimingHist", ii)]] <- downloadHandler(
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
                width = 4,  # Reduced width
                height = 3.5,  # Reduced height
                limitsize = F,
                device = svglite
              )
              rsvg::rsvg_png(tmp_svg, file, height = 900, width = 900)  # Reduced resolution
            } else {
              ggsave(
                file,
                plot = timingHistograms()$plotList[[ii]] + hist_theme,
                width = 4,  # Reduced width
                height = 3.5,  # Reduced height
                unit = "in",
                limitsize = F,
                device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType)
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
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
        paste0(experiment_names(),"histogram-of-rsvp-reading-stacked-by-grade.", input$fileType)
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
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
          theme(
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)),
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
    
    output$downloadStackedCrowdingPlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(), "histogram-of-peripheral-crowding-stacked-by-grade.", input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$crowding_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            plot = stackedPlots()$crowding_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
          plt_theme +
          theme(
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)),
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
    
    output$downloadStackedFovealAcuityPlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(), "histogram-of-foveal-acuity-stacked-by-grade.", input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_acuity_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            plot = stackedPlots()$foveal_acuity_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
          plt_theme +
          theme(
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)),
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
    
    output$downloadStackedFovealCrowdingPlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(), "histogram-of-foveal-crowding-stacked-by-grade.", input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_crowding_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            plot = stackedPlots()$foveal_crowding_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
          plt_theme +
          theme(
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)),
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
    
    output$downloadStackedFovealRepeatedPlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(), "histogram-of-foveal-repeated-letter-crowding-stacked-by-grade.", input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$foveal_repeated_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            plot = stackedPlots()$foveal_repeated_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
          plt_theme +
          theme(
            axis.text.x = element_text(),
            axis.ticks.x = element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)),
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
    
    output$downloadStackedPeripheralAcuityPlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(), "histogram-of-peripheral-acuity-stacked-by-grade.", input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            filename = tmp_svg,
            plot = stackedPlots()$peripheral_acuity_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            plot = stackedPlots()$peripheral_acuity_plot +
              plt_theme +
              theme(
                axis.text.x = element_text(),
                axis.ticks.x = element_line(),
                plot.title = element_text(size = 14, margin = margin(b = 1)),
                plot.margin = margin(t = 2, r = 5, b = 2, l = 5)
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
            device = ifelse(input$fileType == "svg", svglite::svglite, input$fileType),
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
    while(i <= length(scatterDiagrams()$plotList)-1) {
      out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
                               shinycssloaders::withSpinner(plotOutput(paste0("scatter", i), width = "100%", height = "100%"),type=4),
                               shinycssloaders::withSpinner(plotOutput(paste0("scatter", i+1), width = "100%", height = "100%"),type=4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatter", i), 'Download'),
                                  downloadButton(paste0("downloadScatter", i+1), 'Download'))
      i = i + 2
    }
    if (i == length(scatterDiagrams()$plotList)){
      out[[i]] <-splitLayout(cellWidths = c("50%", "50%"),
                             shinycssloaders::withSpinner(plotOutput(paste0("scatter", i), width = "100%", height = "100%"),type=4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatter", i), 'Download'))
    }
    for (j in 1:length(scatterDiagrams()$plotList)) {
      local({
        ii <- j
        output[[paste0("scatter", ii)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(
            file = outfile,
            plot = scatterDiagrams()$plotList[[ii]] + 
              plt_theme_scatter + 
              scale_color_manual(values = colorPalette),
            unit = 'in',
            limitsize = F,
            device = svglite
          )
          
          list(src = outfile,
               contenttype = 'svg')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadScatter", ii)]] <- downloadHandler(
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
  output$scatterTime <- renderUI({
    out <- list()
    i = 1
    
    while (i <= length(scatterTime()$plotList) - 1) {
      out[[i]] <- splitLayout(cellWidths = c("50%", "50%"),
                              shinycssloaders::withSpinner(plotOutput(paste0("scatterTime", i), width = "100%", height = "100%"), type = 4),
                              shinycssloaders::withSpinner(plotOutput(paste0("scatterTime", i + 1), width = "100%", height = "100%"), type = 4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatterTime", i), 'Download'),
                                  downloadButton(paste0("downloadScatterTime", i + 1), 'Download'))
      i = i + 2
    }
    
    # Handle any remaining scatter plot
    if (i == length(scatterTime()$plotList)) {
      out[[i]] <- splitLayout(cellWidths = c("50%", "50%"),
                              shinycssloaders::withSpinner(plotOutput(paste0("scatterTime", i), width = "100%", height = "100%"), type = 4))
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(paste0("downloadScatterTime", i), 'Download'))
    }
    
    # Generate the plots and download handlers
    for (j in 1:length(scatterTime()$plotList)) {
      local({
        ii <- j
        output[[paste0("scatterTime", ii)]] <- renderImage({
          outfile <- tempfile(fileext = '.svg')
          ggsave(
            file = outfile,
            plot = scatterTime()$plotList[[ii]] +
              plt_theme_scatter +
              scale_color_manual(values = colorPalette),
            unit = 'in',
            limitsize = F,
            device = svglite
          )
          
          list(src = outfile, contenttype = 'svg')
        }, deleteFile = TRUE)
        
        output[[paste0("downloadScatterTime", ii)]] <- downloadHandler(
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
                  plt_theme_scatter +
                  scale_color_manual(values = colorPalette),
                unit = "in",
                limitsize = F,
                device = svglite
              )
              rsvg::rsvg_png(tmp_svg, file, width = 1800)
            } else {
              ggsave(
                file,
                plot = scatterTime()$plotList[[ii]] +
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
  
  
  
  # output$rsvpPlotlys <- renderUI({
  #   out <- list()
  #   i = 1
  #   while(i <= length(rsvpPlotlyPlots()$plotList)-1) {
  #     out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
  #                              shinycssloaders::withSpinner(plotlyOutput(paste0("rsvpPlotly", i), width = "100%", height = "100%"),type=4),
  #                              shinycssloaders::withSpinner(plotlyOutput(paste0("rsvpPlotly", i+1), width = "100%", height = "100%"),type=4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadRSVPPlotly", i), 'Download'),
  #                                 downloadButton(paste0("downloadRSVPPlotly", i+1), 'Download'))
  #     i = i + 2
  #   }
  #   if (i == length(rsvpPlotlyPlots()$plotList)){
  #     out[[i]] <-splitLayout(cellWidths = c("50%", "50%"),
  #                            shinycssloaders::withSpinner(plotlyOutput(paste0("rsvpPlotly", i), width = "100%", height = "100%"),type=4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadRSVPPlotly", i), 'Download'))
  #   }
  #   return(out)
  # })
  # for (j in 1:length(rsvpPlotlyPlots()$plotList)) {
  # 
  #   local({
  #     ii <- j
  #     output[[paste0("rsvpPlotly", ii)]] <- renderPlotly({
  #       rsvpPlotlyPlots()$plotList[[ii]]
  #     })
  #     
  #     output[[paste0("downloadRSVPPlotly", ii)]] <- downloadHandler(
  #       filename = paste0(
  #         experiment_names(),
  #         rsvpPlotlyPlots()$fileNames[[ii]],
  #         ii,
  #         '.',
  #         input$fileType
  #       ),
  #       content = function(file) {
  #         if (input$fileType == "png") {
  #           ggsave(
  #             "tmp.svg",
  #             plot = rsvpPlotlyPlots()$plotList[[ii]],
  #             unit = "in",
  #             limitsize = F,
  #             device = svglite
  #           )
  #           rsvg::rsvg_png("tmp.svg", file,
  #                          height = 1800,
  #                          width = 1800)
  #         } else {
  #           ggsave(
  #             file,
  #             plot = rsvpPlotlyPlots()$plotList[[ii]],
  #             unit = "in",
  #             limitsize = F,
  #             device = ifelse(
  #               input$fileType == "svg",
  #               svglite::svglite,
  #               input$fileType
  #             )
  #           )
  #         }
  #       }
  #     )
  #   })
  # }
  # 
  # output$readingPlotlys <- renderUI({
  #   out <- list()
  #   i = 1
  #   while(i <= length(readingPlotlyPlots()$plotList)-1) {
  #     out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
  #                              shinycssloaders::withSpinner(plotlyOutput(paste0("readingplotly", i), width = "100%", height = "100%"),type=4),
  #                              shinycssloaders::withSpinner(plotlyOutput(paste0("readingplotly", i+1), width = "100%", height = "100%"),type=4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadReadingPlotly", i), 'Download'),
  #                                 downloadButton(paste0("downloadReadingPlotly", i+1), 'Download'))
  #     i = i + 2
  #   }
  #   if (i == length(readingPlotlyPlots()$plotList)){
  #     out[[i]] <-splitLayout(cellWidths = c("50%", "50%"),
  #                            shinycssloaders::withSpinner(plotlyOutput(paste0("readingplotly", i), width = "100%", height = "100%"),type=4))
  #     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
  #                                 downloadButton(paste0("downloadReadingPlotly", i), 'Download'))
  #   }
  #   return(out)
  # })
  # 
  # for (j in 1:length(readingPlotlyPlots()$plotList)) {
  # 
  #   local({
  #     ii <- j
  #     output[[paste0("readingplotly", ii)]] <- renderPlotly({
  #       readingPlotlyPlots()$plotList[[ii]]
  #     })
  #     
  #     output[[paste0("downloadReadingPlotly", ii)]] <- downloadHandler(
  #       filename = paste0(
  #         experiment_names(),
  #         readingPlotlyPlots()$fileNames[[ii]],
  #         ii,
  #         '.',
  #         input$fileType
  #       ),
  #       content = function(file) {
  #         if (input$fileType == "png") {
  #           ggsave(
  #             "tmp.svg",
  #             plot = readingPlotlyPlots()$plotList[[ii]],
  #             unit = "in",
  #             limitsize = F,
  #             device = svglite
  #           )
  #           rsvg::rsvg_png("tmp.svg", file,
  #                          height = 1800,
  #                          width = 1800)
  #         } else {
  #           ggsave(
  #             file,
  #             plot = readingPlotlyPlots()$plotList[[ii]],
  #             unit = "in",
  #             limitsize = F,
  #             device = ifelse(
  #               input$fileType == "svg",
  #               svglite::svglite,
  #               input$fileType
  #             )
  #           )
  #         }
  #       }
  #     )
  #   })
  # }
  
  
  
  #### grade plots
  
  output$crowdingAgePlot <-renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =  plot_crowding_vs_age(df_list()$crowding) + plt_theme,
      width = 6,
      unit = 'in',
      device = svglite,
    )

    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)

  # output$rsvpGradePlot <- renderImage({
  #   outfile <- tempfile(fileext = '.svg')
  #   ggsave(
  #     file = outfile,
  #     plot = gradePlots()[[2]] + plt_theme,
  #     width = 6,
  #     unit = 'in',
  #     device = svglite
  #   )
  #   
  #   list(src = outfile,
  #        contenttype = 'svg')
  # }, deleteFile = TRUE)
  # 
  output$acuityAgePlot <- renderImage({
    outfile <- tempfile(fileext = '.svg')
    ggsave(
      file = outfile,
      plot =plot_acuity_vs_age(df_list()) + plt_theme,
      width = 6,
      unit = 'in',
      device = svglite
    )
    list(src = outfile,
         contenttype = 'svg')
  }, deleteFile = TRUE)
  
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
    plotStaircases(files()$stairs, input$thresholdParameter)
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
      rsvg::rsvg_png(tmp_svg, file,
                     height = 225 * stairPlot()$height,
                     width = 1800, )
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
  observeEvent(input$do, {
    output$autocorrelation <- renderImage({
      showModal(modalDialog("Generating plot", footer = NULL))
      file_list <- input$fileJSON$data
      jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
      p <- get_autocorrelation_plot(jsonFile, sound_data())
      outfile <- tempfile(fileext = '.svg')
      ggsave(file = outfile,
             plot = p)
      removeModal()
      list(src = outfile,
           contenttype = 'svg',
           alt = "autocorrelation")
    }, deleteFile = TRUE)
    
  })
  
  sound_level_plot <- reactive({
    plot_sound_level(sound_data())
  })
  
  observeEvent(input$fileJSON, {
    #### sound calibration server side####
    output$`sound table` <- renderTable(sound_data()[[1]],
                                        digits = 1)
    output$`Dynamic Range Compression Model` <-
      renderTable(
        expr = sound_data()[[3]],
        rownames = sound_data()[[4]],
        colnames = F,
        digits = 1
      )
    
    output$`sound level plot` <- renderImage({
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
    
    
    output$`record freq plot system` <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = record_freq_system_plot()$plot,
        height = record_freq_system_plot()$height,
        width = 8.5,
        units = "in"
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "Power Spectral Density")
    }, deleteFile = TRUE)
    
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
    output$IRtmpFour <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = iirPlots()[[1]],
        height = iirPlots()[[3]]
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR one")
    }, deleteFile = TRUE)
    
    output$IRtmpFive <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = iirPlots()[[2]],
        height = iirPlots()[[4]]
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR two")
    }, deleteFile = TRUE)
    
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
    
    output$cumSumPowerPlotSystem <- renderImage({
      outfile <- tempfile(fileext = '.svg')
      ggsave(
        file = outfile,
        plot = cumSumPowerPlot()$p_system +
          add_transducerTable_system(
            sound_data()[[7]],
            c("left", "bottom"),
            subtitle = list(
              c(
                subtitleOne(),
                subtitleTwo()$component,
                subtitleThree()$component
              )
            ),
            leftShift = 0.02
          ) + sound_theme_display,
        height = cumSumPowerPlot()$height_system,
        unit = "in",
      )
      list(src = outfile,
           contenttype = 'svg',
           alt = "IIR two")
    }, deleteFile = TRUE)
    
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
    
    outputOptions(output, 'jsonUploaded', suspendWhenHidden = FALSE)
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
    }, rownames = T, colnames = T
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
    }, rownames = T, colnames = T
    )
    output$downloadProfileTable <- downloadHandler(
      filename = "profiles-table.csv",
      content = function(filename) {
        write.csv(df, file = filename,row.names = FALSE)
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
          rsvg::rsvg_png(tmp_svg, file,
                         height = p$height* 300,
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
          rsvg::rsvg_png(tmp_svg, file,
                         height = p$height* 300,
                         width = 1800, )
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
      filename = paste0(
        'average-of-profiles',
        '.',
        input$fileProfile
      ),
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
          rsvg::rsvg_png(tmp_svg, file,
                         height = p$avgHeight* 300,
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
          ifelse(json$isDefault,
                 paste0("default/", unlist(json$modelNumbers), "/",json$createDates),
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
    }, rownames = T, colnames = T
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
    
    output$profileAverageTitle <- renderText(profile_plot()$title)
  })
  
  
  
  #### Event Handler ####
  
  observeEvent(input$file,
               {
                 if (!is.null(input$file)) {
                   # Extract file names from the uploaded files
                   names <- input$file$name
                   
                   # Filter out the names that end with "zip" in a case-insensitive manner
                   zip_names <- names[grepl("\\.zip$", names, ignore.case = TRUE)]
                   
                   if (length(zip_names) > 0) {
                     # If there are any ZIP files, use the name of the first ZIP file
                     app_title$default <- tools::file_path_sans_ext(basename(zip_names[1]))
                   } else {
                     # If there are no ZIP files but there are CSV files, concatenate their names
                     csv_names <- names[grepl("\\.csv$", names, ignore.case = TRUE)]
                     if (length(csv_names) > 0) {
                       concatenated_names <- tools::file_path_sans_ext(basename(csv_names[1]))
                       app_title$default <- concatenated_names
                     } else {
                       # Fallback if no ZIP or CSV files are uploaded
                       app_title$default <- "EasyEyes Analyze"
                     }
                   }
                 } else {
                   # Fallback title if no files are uploaded at all
                   app_title$default <- "EasyEyes Analyze"
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
                 output$experiment <- renderText(experiment_names())
                 if (!is.null(prolific())) {
                   combinedTable <- combineProlific(prolific(), summary_table())[[1]]
                 } else{
                   combinedTable <- combineProlific(NULL, summary_table())[[1]]
                 }

                 participants <-
                   unique(combinedTable$`Pavlovia session ID`)
                 prolific_id <-
                   unique(combinedTable$`Prolific participant ID`)
                 output$ex1 <- DT::renderDataTable(
                   render_summary_datatable(combinedTable, participants, prolific_id)
                 )
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
                 
                 updateSelectInput(session,
                                   'thresholdParameter',
                                   choices = unique(files()$stairs$thresholdParameter),
                                   selected = unique(files()$stairs$thresholdParameter)[1],
                                   )
                 #### stairPlots ####

                 # output$p1 <- renderImage({
                 #   if (stairPlots()[[4]]) {
                 #     p <- (stairPlots()[[1]][[1]] + plt_theme) / (stairPlots()[[1]][[2]] + plt_theme) / (stairPlots()[[1]][[3]] + plt_theme)
                 #   } else {
                 #     p <- ggplot() + plt_theme
                 #   }
                 #   outfile <- tempfile(fileext = '.svg')
                 #   ggsave(
                 #     file = outfile,
                 #     plot = p,
                 #     device = svg,
                 #     width = 6,
                 #     height = 4
                 #   )
                 # })
                 #
                 # output$p2 <- renderImage({
                 #   outfile <- tempfile(fileext = '.svg')
                 #   ggsave(
                 #     file = outfile,
                 #     plot = stairPlots()[[2]] + plt_theme,
                 #     device = svg,
                 #     width = 6,
                 #     height = 4
                 #   )
                 #
                 # })
                 #
                 # output$p3 <- renderImage({
                 #   outfile <- tempfile(fileext = '.svg')
                 #   ggsave(
                 #     file = outfile,
                 #     plot = stairPlots()[[3]] + plt_theme,
                 #     device = svg,
                 #     width = 6,
                 #     height = 4
                 #   )
                 #  }, deleteFile = T)

               }
               )
  
  
  #### download handlers ####
 
  
  output$downloadAll = downloadHandler(
    filename = 'plots.zip',
    content = function( file){
      
      # Set temporary working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      fileNames = c()
      ggsave(
        paste0("correlation-matrix.", input$fileType),
        plot =  corrMatrix()$plot,
        width = corrMatrix()$width,
        height = corrMatrix()$height,
        unit = "in",
        limitsize = F,
        device = input$fileType
      )
      
      ggsave(
        paste0("duration-correlation-matrix.", input$fileType),
        plot =  durationCorrMatrix()$plot,
        width = durationCorrMatrix()$width,
        height = durationCorrMatrix()$height,
        unit = "in",
        limitsize = F,
        device = input$fileType
      )
      fileNames = c(paste0("correlation-matrix.", input$fileType), paste0("duration-correlation-matrix.", input$fileType))
      
      if (length(histograms()$plotList) > 0) {
        for (i in 1:length(histograms()$plotList)) {
          print(i)
          plotFileName =  paste0(histograms()$fileNames[[i]], '.',input$fileType)
          ggsave(filename = plotFileName, 
                 plot = histograms()$plotList[[i]] + plt_theme,
                 width = 6,
                 unit = 'in',
                 device = input$fileType)
          fileNames = c(fileNames, plotFileName)
        }
        print('done histograms')
      }
      if (length(scatterDiagrams()$plotList) > 0) {
        for (i in 1:length(scatterDiagrams()$plotList)) {
          plotFileName =  paste0(scatterDiagrams()$fileNames[[i]], '.',input$fileType)
          ggsave(filename = plotFileName, 
                 plot = scatterDiagrams()$plotList[[i]] +
                   plt_theme_scatter + 
                 scale_color_manual(values = colorPalette),
                 width = 6,
                 unit = 'in',
                 device = input$fileType)
          fileNames = c(fileNames, plotFileName)
        }
        print('done scatterDiagrams')
      }
      if (length(agePlots()$plotList) > 0) {
       for (i in 1:length(agePlots()$plotList)) {
         plotFileName =  paste0(agePlots()$fileNames[[i]], '.',input$fileType)
         ggsave(filename = plotFileName, 
                plot = agePlots()$plotList[[i]] + plt_theme,
                width = 6,
                unit = 'in',
                device = input$fileType)
         fileNames = c(fileNames, plotFileName)
       }
       print('done agePlots')
     }
      if (length(rsvpPlotlyPlots()$plotList) > 0) {
        for (i in 1:length(rsvpPlotlyPlots()$plotList)) {
          plotFileName =  paste0(rsvpPlotlyPlots()$fileNames[[i]], '.',input$fileType)
          ggsave(filename = plotFileName, 
                 plot = rsvpPlotlyPlots()$plotList[[i]] + plt_theme,
                 width = 6,
                 unit = 'in',
                 device = input$fileType)
          fileNames = c(fileNames, plotFileName)
        }
        print('done rsvpPlotlyPlots')
      }
     
      if (length(readingPlotlyPlots()$plotList) > 0) {
        for (i in 1:length(readingPlotlyPlots()$plotList)) {
          plotFileName =  paste0(readingPlotlyPlots()$fileNames[[i]], '.',input$fileType)
          ggsave(filename = plotFileName, 
                 plot = readingPlotlyPlots()$plotList[[i]] + plt_theme,
                 width = 6,
                 unit = 'in',
                 device = input$fileType)
          fileNames = c(fileNames, plotFileName)
        }
      }
      print('done readingPlotlyPlots')
      print(fileNames)
      # Zip them up
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
      openxlsx::write.xlsx(summary_table() %>% select(-order), file = filename)
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
        params = summary_table() %>% mutate(experiment = experiment_names()),
        envir = new.env(parent = globalenv())
      )
    }
  )
  
  output$thresholdOne <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Summary-of-each-condition.csv",
        paste0(experiment_names(), "-Summary-of-each-condition.csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[2]], file = filename, row.names = FALSE)
    }
  )
  
  output$thresholdTwo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Thresholds(brief).csv",
        paste0(experiment_names(), "-Thresholds(brief).csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[3]], file = filename, row.names = FALSE)
    }
  )
  
  output$thresholdThree <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "Thresholds.csv",
        paste0(experiment_names(), "-Thresholds.csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[4]], file = filename, row.names = FALSE)
    }
  )
  
  output$`sound_data` <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "sound-calibration.csv",
        paste0(experiment_names(), "sound-calibration.csv")
      )
    },
    content = function(filename) {
      write.csv(all_sound_data()[[1]], file = filename, row.names = FALSE)
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
                         height = profile_plot()$height* 300,
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
          rsvg::rsvg_png(tmp_svg, file,
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
      filename = paste0(
        'average-of-profiles',
        '.',
        input$fileProfile
      ),
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
          rsvg::rsvg_png(tmp_svg, file,
                         height = profile_plot()$avgHeight* 300,
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
            height =record_freq_system_plot()$height,
            width = 8.5,
            dpi = 100,
            units = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2000, height = 4000)
        } else {
          ggsave(
            file,
            plot = record_freq_system_plot()$plot,
            height =record_freq_system_plot()$height,
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2000, height = 4000)
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
    
    output$downloadIRtmpFour <- downloadHandler(
      filename = paste0(
        'power-spectral-density-of-system-iir.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = iirPlots()[[1]],
            height = iirPlots()[[3]],
            units = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = iirPlots()[[1]],
            height = iirPlots()[[3]],
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
    output$downloadIRtmpFive <- downloadHandler(
      filename = paste0(
        'power-spectral-density-of-',
        sound_data()[[12]]$transducerTypeF,
        "-iir.",
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = iirPlots()[[2]],
            height = iirPlots()[[4]],
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = iirPlots()[[2]],
            height = iirPlots()[[4]],
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
        } else {
          ggsave(
            file,
            plot =componentIIRPlots()$schroeder +
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
    
    output$downloadCumSumPowerPlotSystem <- downloadHandler(
      filename = paste0(
        'cumulative-power-of-system-corrected-mls.',
        input$fileTypeSound
      ),
      content = function(file) {
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = cumSumPowerPlot()$p_system +
              add_transducerTable_system(
                sound_data()[[7]],
                c("left", "bottom"),
                subtitle = list(
                  c(
                    subtitleOne(),
                    subtitleTwo()$component,
                    subtitleThree()$component
                  )
                ),
                leftShift = 0.02
              ),
            height = cumSumPowerPlot()$height_system,
            units = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800,
                         height = 1800)
        } else {
          ggsave(
            file,
            plot = cumSumPowerPlot()$p_system +
              add_transducerTable_system(
                sound_data()[[7]],
                c("left", "bottom"),
                subtitle = list(
                  c(
                    subtitleOne(),
                    subtitleTwo()$component,
                    subtitleThree()$component
                  )
                ),
                leftShift = 0.02
              ),
            height = cumSumPowerPlot()$height_system,
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite::svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
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
          rsvg::rsvg_png("tmp.svg", file,
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
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
      filename = paste0('autocorrelation.', input$fileTypeSound),
      content = function(file) {
        file_list <- input$fileJSON$data
        jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
        if (input$fileTypeSound == "png") {
          ggsave(
            "tmp.svg",
            plot = get_autocorrelation_plot(jsonFile, sound_data()),
            dpi = 100,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
        } else {
          ggsave(
            file,
            plot = get_autocorrelation_plot(jsonFile, sound_data()),
            device = ifelse(
              input$fileTypeSound == "svg",
              svglite,
              input$fileTypeSound
            )
          )
        }
      }
    )
    
  })
  
  # download plots handlers
  
  observeEvent(toListen(), {
    
    ##### download handler for grade plots #####
    
    output$downloadCrowdingAgePlot <- downloadHandler(
      filename = paste0(
        'crowding-vs-age',
        '.',
        input$fileType
      ),
        content = function(file) {
          if (input$fileType == "png") {
            ggsave(
              "tmp.svg",
              plot = plot_crowding_vs_age(df_list()$crowding ) + plt_theme,
              unit = "in",
              limitsize = F,
              device = svglite
            )
            rsvg::rsvg_png("tmp.svg", file,
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
            plot = durationPlot()$font +
              scale_color_manual(values= colorPalette),
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = durationPlot()$font + 
              scale_color_manual(values= colorPalette),
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
            plot = durationPlot()$participant + 
              scale_color_manual(values= colorPalette),
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = durationPlot()$participant + 
              scale_color_manual(values= colorPalette),
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
            plot = latenessPlot()$font + 
              scale_color_manual(values= colorPalette),
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = latenessPlot()$font + 
              scale_color_manual(values= colorPalette),
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
            plot = latenessPlot()$participant + 
              scale_color_manual(values= colorPalette),
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot = latenessPlot()$participant + 
              scale_color_manual(values= colorPalette),
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
          rsvg::rsvg_png("tmp.svg", file,
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
          rsvg::rsvg_png("tmp.svg", file,
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
      filename = paste0(
        'acuity-vs-age',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = plot_acuity_vs_age(df_list()) + plt_theme,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         height = 1800,
                         width = 1800)
        } else {
          ggsave(
            file,
            plot =plot_acuity_vs_age(df_list()) + plt_theme,
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
              rsvg::rsvg_png("tmp.svg", file,
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
              rsvg::rsvg_png("tmp.svg", file,
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
        
        output[[paste0("downloadScatter", ii)]] <- downloadHandler(
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
                  plt_theme_scatter +
                  scale_color_manual(values = colorPalette),
                height = 6,
                width = 6,
                unit = "in",
                limitsize = F,
                device = svglite
              )
              rsvg::rsvg_png("tmp.svg", file,
                             height = 1800,
                             width = 1800)
            } else {
              ggsave(
                file,
                plot = scatterDiagrams()$plotList[[ii]] + 
                  plt_theme_scatter +
                  scale_color_manual(values = colorPalette),
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
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpCrowding()[[3]] + 
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvpCrowding()[[3]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    output$downloadRsvpResidualCrowding <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('residual-rsvp-vs-residual-peripheral-crowding-by-grade.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpCrowding()[[4]] + 
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvpCrowding()[[4]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    
    output$downloadRsvpCrowdingFovealGradePlot <- downloadHandler(
      filename = function () {
        paste0(experiment_names(), 'rsvp-vs-foveal-crowding-by-grade.', input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpCrowding()[[5]] + 
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvpCrowding()[[5]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6
          )
        }
      }
    )
    
    
    output$downloadRsvpFovealAcuityGradePlot <- downloadHandler(
      filename = function () {
        paste0(experiment_names(), 'rsvp-vs-fovel-acuity-by-grade.', input$fileType)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
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
        paste0(experiment_names(), 'rsvp-vs-crowding-factored-age.', input$fileType)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
        } else {
          ggsave(
            file = file,
            plot =factor_out_age_and_plot(df_list()) + 
              plt_theme,
            device = svg,
            width = 6
          )
        }
      }
    )
    
    
    output$downloadRsvpPeripheralAcuityGradePlot <- downloadHandler(
      filename = function () {
        paste0(experiment_names(), 'rsvp-vs-periphreal-acuity-by-grade.', input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpAcuityPeripheral()[[2]]+ 
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvpAcuityPeripheral()[[2]] + 
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
        paste0(experiment_names(), 'rsvp-vs-repeated-letter-crowding-by-grade.', input$fileType)
      },
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvp_repeated_letter_crowding()[[2]]+ 
              plt_theme,
            width = 8,
            height = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
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
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = ordinaryAcuityPeripheral()[[2]] + 
              plt_theme,
            width = 6,
            device = svglite,
            limitsize = FALSE 
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800)
        } else {
          ggsave(
            file = file,
            plot = ordinaryAcuityPeripheral()[[2]] + 
              plt_theme,
            device = svg,
            width = 6
          )
        }
      }
    )
    
    output$downloadOrdinaryFovealAcuityGradePlot <- downloadHandler(
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
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
    
    output$downloadOrdinaryFovealCrowdingGradePlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(),"ordinary-reading-vs-foveal-crowding-by-grade.", input$fileType)
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
    
    output$downloadOrdinaryPeripheralCrowdingGradePlot <- downloadHandler(
      filename = function() {
        paste0(experiment_names(),"ordinary-reading-vs-peripheral-crowding-by-grade.", input$fileType)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
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
        paste0(experiment_names(),"reading-vs-repeated-letter-crowding-by-grade.", input$fileType)
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
          rsvg::rsvg_png("tmp.svg", file,
                         width = 2400, height = 1800)
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
    
    
    
    
    
    
    # for (j in 1:length(rsvpPlotlyPlots()$plotList)) {
    #   local({
    #     ii <- j
    #     
    #     output[[paste0("downloadRSVPPlotly", ii)]] <- downloadHandler(
    #       filename = paste0(
    #         experiment_names(),
    #         rsvpPlotlyPlots()$fileNames[[ii]],
    #         '.',
    #         input$fileType
    #       ),
    #       content = function(file) {
    #         if (input$fileType == "png") {
    #           ggsave(
    #             "tmp.svg",
    #             plot = rsvpPlotlyPlots()$plotList[[ii]],
    #             height = 6,
    #             width = 6,
    #             unit = "in",
    #             limitsize = F,
    #             device = svglite
    #           )
    #           rsvg::rsvg_png("tmp.svg", file,
    #                          height = 1800,
    #                          width = 1800)
    #         } else {
    #           ggsave(
    #             file,
    #             plot = rsvpPlotlyPlots()$plotList[[ii]],
    #             height = 6,
    #             width = 6,
    #             unit = "in",
    #             limitsize = F,
    #             device = ifelse(
    #               input$fileType == "svg",
    #               svglite::svglite,
    #               input$fileType
    #             )
    #           )
    #         }
    #       }
    #     )
    #   })
    # }
    # 
    # for (j in 1:length(readingPlotlyPlots()$plotList)) {
    #   
    #   local({
    #     ii <- j
    #     output[[paste0("readingplotly", ii)]] <- renderPlotly({
    #       readingPlotlyPlots()$plotList[[ii]]
    #     })
    #     
    #     output[[paste0("downloadReadingPlotly", ii)]] <- downloadHandler(
    #       filename = paste0(
    #         experiment_names(),
    #         readingPlotlyPlots()$fileNames[[ii]],
    #         ii,
    #         '.',
    #         input$fileType
    #       ),
    #       content = function(file) {
    #         if (input$fileType == "png") {
    #           ggsave(
    #             "tmp.svg",
    #             plot = readingPlotlyPlots()$plotList[[ii]],
    #             unit = "in",
    #             limitsize = F,
    #             device = svglite
    #           )
    #           rsvg::rsvg_png("tmp.svg", file,
    #                          height = 1800,
    #                          width = 1800)
    #         } else {
    #           ggsave(
    #             file,
    #             plot = readingPlotlyPlots()$plotList[[ii]],
    #             unit = "in",
    #             limitsize = F,
    #             device = ifelse(
    #               input$fileType == "svg",
    #               svglite::svglite,
    #               input$fileType
    #             )
    #           )
    #         }
    #       }
    #     )
    #   })
    # }
    # 
    
    
    # output$downloadP1 <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('p1.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = (stairPlots()[[1]][[1]] + downloadtheme) / (stairPlots()[[1]][[2]] + downloadtheme) / (stairPlots()[[1]][[3]] + downloadtheme)
    #         ,
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    # output$downloadP2 <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('p2.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot =stairPlots()[[2]] +
    #         downloadtheme,
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType),
    #       dpi = 200
    #     )
    #   }
    # )
    # 
    # output$downloadP3 <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('p3.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = stairPlots()[[3]] +
    #         downloadtheme,
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType),
    #       dpi = 200
    #     )
    #   }
    # )
    
    # output$downloadReadingVsXheightLog <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('reading-vs-x-height-log-scale.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = reading_vs_font_size()[[1]] +
    #         downloadtheme +
    #         labs(title = paste(
    #           c(experiment_names(),
    #             "reading speed vs x height, log scale"),
    #           collapse = "\n"
    #         )),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType),
    #       dpi = 200
    #     )
    #   }
    # )
    # 
    # output$downloadReadingVsXheightLinear <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('reading-vs-x-height-linear.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = reading_vs_font_size()[[2]] +
    #         downloadtheme +
    #         labs(title = paste(
    #           c(
    #             experiment_names(),
    #             "reading speed vs x height, linear scale"
    #           ),
    #           collapse = "\n"
    #         )),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    # 
    # output$downloadReading60cm1.2mmVs1.4mm <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('reading-1.4mm-vs-1.2mm-60cm.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = reading_60cm_1.2mm_vs_1.4mm() +
    #         downloadtheme +
    #         labs(title = paste(
    #           c(experiment_names(),
    #             "reading speed 1.4mm vs 1.2mm, 60cm"),
    #           collapse = "\n"
    #         )),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    
    output$downloadCorrMatrixPlot <- downloadHandler(
      filename = paste0(
        'correlation-matrix',
        '.',
        input$fileType
      ),
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
          rsvg::rsvg_png("tmp.svg", file,
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
    
    output$downloadDurationCorrMatrixPlot <- downloadHandler(
      filename = paste0(
        'duration-correlation-matrix',
        '.',
        input$fileType
      ),
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
          rsvg::rsvg_png("tmp.svg", file,
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
    
    
    
    # output$downloadReading30cm1.2mmVs1.4mm <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('reading-1.4mm-vs-1.2mm-30cm.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = reading_30cm_1.2mm_vs_1.4mm() +
    #         downloadtheme +
    #         labs(title = paste(
    #           c(experiment_names(),
    #             "reading speed 1.4mm vs 1.2mm, 30cm"),
    #           collapse = "\n"
    #         )),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    # 
    # output$downloadReading60cmDiffVsAge <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('reading-speed-difference-vs-age-60cm.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = reading_diff_60cm_vs_age() +
    #         downloadtheme +
    #         labs(title = paste(
    #           c(
    #             experiment_names(),
    #             "reading speed difference vs age, 60cm"
    #           ),
    #           collapse = "\n"
    #         )),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    
    
    
    output$downloadMeanPlot <- downloadHandler(
      filename = paste(app_title$default, paste0('mean.', input$fileType), sep = "-"),
      content = function(file) {
        ggsave(
          file,
          plot = meanPlot() + downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadMedianPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('median.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = medianPlot() + downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    # output$downloadRegressionFontPlot <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('regression.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = regressionFontPlot() + downloadtheme + coord_fixed(ratio = 1),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    # 
    # output$downloadRegressionFontPlotWithLabel <- downloadHandler(
    #   filename = paste(
    #     app_title$default,
    #     paste0('regression.', input$fileType),
    #     sep = "-"
    #   ),
    #   content = function(file) {
    #     ggsave(
    #       file,
    #       plot = regressionFontPlotWithLabel() + downloadtheme + coord_fixed(ratio = 1),
    #       device = ifelse(input$fileType == "svg", svglite, input$fileType)
    #     )
    #   }
    # )
    
    output$downloadCrowdingAvgPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('average_crowding_left_vs_right.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = crowdingAvgPlot() + downloadtheme,
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
            labs(title = experiment_names(),
                 subtitle = paste0("Crowding ", two_fonts_plots()$title, ", mean")) +
            plt_theme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
 
    
    #### download sloan vs times ####
    
    output$downloadSloanVsTimesSDPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('2_fonts_sd.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = two_fonts_plots()[[2]] +
            labs(title = experiment_names(),
                 subtitle = paste0("Crowding ", two_fonts_plots()$title, ", sd")) +
            downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
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
  output$downloadNotebook <- downloadHandler(
    filename <- function() {
      paste("sound_calibration_analysis", "ipynb", sep=".")
    },
    
    content <- function(file) {
      file.copy("notebooks/sound_calibration_analysis.ipynb", file)
    }
  )
  
})





