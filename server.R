#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(shiny.maxRequestSize = 200 * 1024 ^ 2)
library(shiny)
library(dplyr)
library(readr)
library(stringr)
library(emojifont)
library(DT)
library(ggpubr)
library(shinyjs)
library(lubridate)
library(ggpp)
library(svglite)
library(patchwork)
library(plotly)
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

source("./other/getBits.R")
source("./other/sound_plots.R")
source("./other/read_json.R")
source("./other/formSpree.R")


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
                paging = FALSE,
                dom = 'lt',
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
    return(generate_threshold(files()$data_list, summary_list()))
  })
  
  df_list <- reactive({
    return(generate_rsvp_reading_crowding_fluency(files()$data_list, summary_list(), files()$pretest, input$filterInput))
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
    crowding_scatter_plot(crowdingBySide())  +
      labs(title = paste(
        c(experiment_names(),
          "Crowding, left vs. right, by observer"),
        collapse = "\n"
      ))
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
  
  regressionPlot <- reactive({
    regression_reading_plot(df_list()) +
      labs(subtitle = "Reading vs crowding")
  })
  regressionAcuityPlot <- reactive({
    regression_acuity_plot(df_list()) +
      labs(subtitle = "Reading vs acuity")
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
      fileNames[[i]] = 'foveal-crowding-vs-age'
      i = i + 1
    }
    t <- get_peripheral_crowding_vs_age(df_list()$crowding)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-age'
      i = i + 1
    }
    t <- get_repeatedLetter_vs_age(df_list()$repeatedLetters)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'repeated-letter-crowding-vs-age'
      i = i + 1
    }
    t <- plot_reading_age(df_list()$reading)
 
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'reading-vs-age'
      i = i + 1
    }
    t <- plot_rsvp_age(df_list()$rsvp)

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'RSVP-vs-age'
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
    t <- get_crowding_vs_repeatedLetter(df_list()$crowding, 
                                        df_list()$repeatedLetters)$age

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'crowding-vs-repeated-letters-crowding-age'
      i = i + 1
    }
    
    t <- get_crowding_vs_repeatedLetter(df_list()$crowding, 
                                        df_list()$repeatedLetters)$grade

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'crowding-vs-repeated-letters-crowding-grade'
      i = i + 1
    }
    
   t <- plot_reading_rsvp(df_list()$reading, df_list()$rsvp)

   if (!is.null(t)) {
     l[[i]] = t
     fileNames[[i]] = 'reading-vs-RSVP-reading'
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
    print('done histograms')
    return(list(
      plotList = l,
      fileNames = fileNames))
    
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
    print('inside get_foveal_acuity_diag')
    t <- foveal_crowding_vs_acuity_diag()$foveal$age

    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-foveal-acuity-age-diagram'
      i = i + 1
    }
    
    t <- foveal_crowding_vs_acuity_diag()$foveal$grade
    
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-foveal-acuity-grade-diagram'
      i = i + 1
    }

    t <-  foveal_crowding_vs_acuity_diag()$peripheral$age
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-acuity-age-diagram'
      i = i + 1
    }
    
    t <-  foveal_crowding_vs_acuity_diag()$peripheral$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-acuity-grade-diagram'
      i = i + 1
    }
    
    t <- foveal_peripheral_diag()$age
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-crowding-age-diagram'
      i = i + 1
    }
    
    t <- foveal_peripheral_diag()$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'foveal-crowding-vs-peripheral-crowding-grade-diagram'
      i = i + 1
    }
    
    t <- get_quest_diag(df_list()$quest)$age
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'quest-sd-vs-mean-age-diagram'
      i = i + 1
    }
    
    t <- get_quest_diag(df_list()$quest)$grade
    if (!is.null(t)) {
      l[[i]] = t
      fileNames[[i]] = 'quest-sd-vs-mean-grade-diagram'
      i = i + 1
    }
    return(list(
      plotList = l,
      fileNames = fileNames
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
  
  output$fileUploaded <- reactive({
    return(nrow(files()$pretest>0))
  })
  
  output$questData <- reactive({
    if ('quest' %in% names(df_list())) {
      return(nrow(df_list()$quest>0))
    }
    return(FALSE)
  })
  
  outputOptions(output, 'isPeripheralAcuity', suspendWhenHidden=FALSE)
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  outputOptions(output, 'questData', suspendWhenHidden=FALSE)
  #### crowding stair plots
  stairPlot <- reactive({
    plotCrowdingStaircases(files()$stairs, input$thresholdParameter)
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
  filename = paste0('stair-plot.',input$fileType),
  content = function(file) {
    if (input$fileType == "png") {
      ggsave(
        "tmp.svg",
        plot = stairPlot()$plot + plt_theme,
        width = 8,
        height = tairPlot()$height,
        unit = "in",
        limitsize = F,
        device = svglite
      )
      rsvg::rsvg_png("tmp.svg", file,
                     height = 225 * stairPlot()$height,
                     width = 1800, )
    } else {
      ggsave(
        file,
        plot = stairPlot()$plot + plt_theme,
        width = 8,
        height = tairPlot()$height,
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
        write.csv(df, file = filename)
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
        write.csv(df, file = filename)
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
          ggsave(
            "tmp.svg",
            plot = p$plot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
          ggsave(
            "tmp.svg",
            plot = p$shiftedPlot,
            height = p$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
          ggsave(
            "tmp.svg",
            plot = p$avgPlot,
            height = p$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
        write.csv(df, file = filename)
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
                 app_title$default <- experiment_names()
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

                 #### plots ####


                 output$corrMatrixPlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot =  corrMatrix()$plot +
                       plt_theme +
                       theme(legend.position = 'none',
                             axis.text.x = element_text(size = 14,
                                                        angle = 30,
                                                        vjust = 1,
                                                        hjust=1),
                             plot.title = element_text(size=18,
                                                       hjust = 1),
                             plot.subtitle = element_text(size=18,
                                                          hjust = 1)),
                     device = svg,
                     width = corrMatrix()$width,
                     height = corrMatrix()$height
                   )
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)


                 output$meanPlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot =   meanPlot() + plt_theme,
                     device = svg,
                     width = 6,
                     height = 4
                   )
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)

                 output$medianPlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot =   medianPlot() + plt_theme,
                     device = svg,
                     width = 6,
                     height = 4
                   )
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)
                 #### regression plots #####

                 output$regressionPlot <-  renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot =   regressionPlot() + plt_theme,
                     device = svg,
                     width = 6,
                     height = 4
                   )

                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)

                 output$regressionAcuityPlot <-  renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot = regressionAcuityPlot() + plt_theme,
                     device = svg,
                     width = 6,
                     height = 4
                   )

                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)


                 #### crowding ####
                 output$crowdingScatterPlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot = crowdingPlot() + plt_theme + coord_fixed(),
                     device = svg,
                     width = 6,
                     height = 4
                   )

                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)

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


                 output$repeatedLetterCrowdingPlot <-
                   renderImage({
                     outfile <- tempfile(fileext = '.svg')
                     ggsave(
                       file = outfile,
                       plot =  get_crowding_vs_repeatedLetter(df_list()$crowding,
                                                              df_list()$repeatedLetters) +
                         plt_theme,
                       device = svg,
                       width = 7,
                       height = 4
                     )

                     list(src = outfile,
                          contenttype = 'svg')
                   }, deleteFile = TRUE)

                 #### rsvp vs crowding ####

                 output$rsvpCrowdingPeripheralAgePlot <- renderPlotly({
                   rsvpCrowding()[[1]]
                 })

                 output$rsvpCrowdingFovealAgePlot <- renderPlotly({
                   rsvpCrowding()[[2]]
                 })

                 output$rsvpCrowdingPeripheralGradePlot <- renderPlotly({
                   rsvpCrowding()[[3]]
                 })

                 output$rsvpCrowdingFovealGradePlot <- renderPlotly({
                   rsvpCrowding()[[4]]
                 })

                 output$rsvpFovealAcuityAgePlot <- renderPlotly({
                   rsvpAcuityFoveal()[[1]]
                 })

                 output$rsvpFovealAcuityGradePlot <- renderPlotly({
                   rsvpAcuityFoveal()[[2]]
                 })


                 output$rsvpPeripheralAcuityAgePlot <- renderPlotly({
                   rsvpAcuityPeripheral()[[1]]
                 })

                 output$rsvpPeripheralAcuityGradePlot <- renderPlotly({
                   rsvpAcuityPeripheral()[[2]]
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
                   return(out)
                 })
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
                           ggsave(
                             "tmp.svg",
                             plot = agePlots()$plotList[[ii]] + plt_theme,
                             limitsize = F,
                             width = 6,
                             unit = "in",
                             device = svglite
                           )
                           rsvg::rsvg_png("tmp.svg", file,
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
                 
                 output$histograms <- renderUI({
                   out <- list()
                   i = 1
                   
                   while(i <= length(histograms()$plotList)-1) {
                     
                     out[[i]] <-  splitLayout(cellWidths = c("50%", "50%"),
                                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i), width = "100%", height = "100%"), type = 4),
                                              shinycssloaders::withSpinner(plotOutput(paste0("hist", i+1), width = "100%", height = "100%"), type = 4))
                     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                                 downloadButton(paste0("downloadHist", i), 'Download'),
                                                 downloadButton(paste0("downloadHist", i+1), 'Download'))
                     i = i + 2
                   }
                   if (i == length(histograms()$plotList)){
                     out[[i]] <- splitLayout(cellWidths = c("50%", "50%"),
                                             shinycssloaders::withSpinner(plotOutput(paste0("hist", i), width = "100%", height = "100%"), type = 4))
                     out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                                 downloadButton(paste0("downloadHist", i), 'Download'))
                   }
                   return(out)
                 })
                 for (j in 1:length(histograms()$plotList)) {

                   local({
                     ii <- j

                     output[[paste0("hist", ii)]] <- renderImage({
                       outfile <- tempfile(fileext = '.svg')
                       ggsave(
                         file = outfile,
                         plot = histograms()$plotList[[ii]] + plt_theme,
                         device = svglite,
                         width = 6,
                         unit = 'in',
                       )
                       
                       list(src = outfile,
                            contenttype = 'svg')
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
                           ggsave(
                             "tmp.svg",
                             plot = histograms()$plotList[[ii]] + plt_theme,
                             unit = "in",
                             width = 6,
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
                   return(out)
                 })
                 for (j in 1:length(scatterDiagrams()$plotList)) {
                   local({
                     ii <- j
                     output[[paste0("scatter", ii)]] <- renderImage({
                       outfile <- tempfile(fileext = '.svg')
                       ggsave(
                         file = outfile,
                         plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
                         width = 6,
                         unit = 'in',
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
                           ggsave(
                             "tmp.svg",
                             plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
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
                             plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
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
                 #### grade plots
                 
                 output$crowdingGradePlot <-renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot =  gradePlots()[[1]] + plt_theme,
                     width = 6,
                     unit = 'in',
                     device = svglite,
                   )
                   
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)
                 
                 output$rsvpGradePlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot = gradePlots()[[2]] + plt_theme,
                     width = 6,
                     unit = 'in',
                     device = svglite
                   )
                   
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)
                 
                 output$acuityGradePlot <- renderImage({
                   outfile <- tempfile(fileext = '.svg')
                   ggsave(
                     file = outfile,
                     plot = gradePlots()[[3]] + plt_theme,
                     width = 6,
                     unit = 'in',
                     device = svglite
                   )
                   list(src = outfile,
                        contenttype = 'svg')
                 }, deleteFile = TRUE)


                 # output$rsvpCrowdingPeripheralPlot <- renderImage({
                 #   outfile <- tempfile(fileext = '.svg')
                 #   ggsave(
                 #     file = outfile,
                 #     plot = rsvpCrowding()[[1]] + plt_theme,
                 #     device = svg,
                 #     width = 6,
                 #     height = 4
                 #   )
                 #
                 #   list(src = outfile,
                 #        contenttype = 'svg')
                 # }, deleteFile = TRUE)
                 #
                 # output$rsvpCrowdingFovealPlot <-renderImage({
                 #   outfile <- tempfile(fileext = '.svg')
                 #   ggsave(
                 #     file = outfile,
                 #     plot = rsvpCrowding()[[2]] + plt_theme,
                 #     device = svg,
                 #     width = 6,
                 #     height = 4
                 #   )
                 #
                 #   list(src = outfile,
                 #        contenttype = 'svg')
                 # }, deleteFile = TRUE)

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

               }
               )
  
  #### download handlers ####
 
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
        "threshold_all_participant.csv",
        paste0(experiment_names(), "-threshold_all_participant.csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[2]], file = filename)
    }
  )
  
  output$thresholdTwo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "threshold.csv",
        paste0(experiment_names(), "-threshold.csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[3]], file = filename)
    }
  )
  
  output$thresholdThree <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "all-threshold.csv",
        paste0(experiment_names(), "-all-threshold.csv")
      )
    },
    content = function(filename) {
      write.csv(threshold_and_warnings()[[4]], file = filename)
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
      write.csv(all_sound_data()[[1]], file = filename)
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
          ggsave(
            "tmp.svg",
            plot = profile_plot()$plot,
            height =  profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg",
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
          ggsave(
            "tmp.svg",
            plot =  profile_plot()$shiftedPlot,
            height = profile_plot()$height,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
          ggsave(
            "tmp.svg",
            plot = profile_plot()$avgPlot,
            height = profile_plot()$avgHeight,
            width = 8,
            unit = "in",
            limitsize = F,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
          ggsave(
            "tmp.svg",
            plot = sound_level_plot()[[1]],
            width = sound_level_plot()[[2]],
            height = sound_level_plot()[[3]],
            unit = "in",
            device = svglite::svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
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
    
    output$downloadCrowdingGradePlot <- downloadHandler(
      filename = paste0(
        experiment_names(),
        'crowding-vs-grade',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = gradePlots()[[1]] + plt_theme,
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
            plot = gradePlots()[[1]] + plt_theme,
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
    
    output$downloadRsvpGradePlot <- downloadHandler(
      filename = paste0(
        experiment_names(),
        'rsvp-vs-grade',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = gradePlots()[[2]] + plt_theme,
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
            plot = gradePlots()[[2]] + plt_theme,
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
    
    output$downloadAcuityGradePlot <- downloadHandler(
      filename = paste0(
        experiment_names(),
        'acuity-vs-grade',
        '.',
        input$fileType
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = gradePlots()[[3]] + plt_theme,
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
            plot = gradePlots()[[3]] + plt_theme,
            unit = "in",
            limitsize = F,
            device = ifelse(
              input$fileType == "svg",
              svglite::svglite,
              input$fileProfile
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
                plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
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
                plot = scatterDiagrams()$plotList[[ii]] + plt_theme_scatter,
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
            plot =  corrMatrix()$plot +
              plt_theme +
              theme(legend.position = 'none',
                    axis.text.x = element_text(size = 14,
                                               angle = 30,
                                               vjust = 1,
                                               hjust=1),
                    plot.title = element_text(size=18,
                                              hjust = 1),
                    plot.subtitle = element_text(size=18,
                                                 hjust = 1)),
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
            plot =  corrMatrix()$plot +
              plt_theme +
              theme(legend.position = 'none',
                    axis.text.x = element_text(size = 14,
                                               angle = 30,
                                               vjust = 1,
                                               hjust=1),
                    plot.title = element_text(size=18,
                                              hjust = 1),
                    plot.subtitle = element_text(size=18,
                                                 hjust = 1)),
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
    
    output$downloadRegressionPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('regression.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = regressionPlot() + 
              plt_theme +
              coord_fixed(ratio = 1),
            width = 7,
            height = 5,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = regressionPlot() + 
              plt_theme + 
              coord_fixed(ratio = 1),
            device = svg,
            width = 7,
            height = 5
          )
        }
      }

    )
    
    output$downloadRegressionAcuityPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('regression.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = regressionAcuityPlot() + 
              plt_theme +
              coord_fixed(ratio = 1),
            width = 7,
            height = 5,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = regressionAcuityPlot() + 
              plt_theme + 
              coord_fixed(ratio = 1),
            device = svg,
            width = 7,
            height = 5
          )
        }
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
    
    
    
    output$downloadCrowdingScatterPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('crowding_left_vs_right.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = crowdingPlot() + coord_fixed(),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
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
    
    
    #### download rsvp crowding ####
    
    output$downloadRsvpCrowdingPeripheralAgePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-age.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpCrowding()[[1]] + 
              plt_theme,
            width = 7,
            height = 5,
            device = svglite
          )
          rsvg::rsvg_png("tmp.svg", file,
                         width = 1800, height = 1800)
        } else {
          ggsave(
            file = file,
            plot = rsvpCrowding()[[1]] + 
              plt_theme,
            device = svg,
            width = 7,
            height = 5
          )
        }
      }
    )
    
    output$downloadRsvpCrowdingFovealAgePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-foveal-crowding-by-age.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpCrowding()[[2]] + 
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
            plot = rsvpCrowding()[[2]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    
    output$downloadRsvpCrowdingPeripheralGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-crowding-by-age.', input$fileType),
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
    
    output$downloadRsvpCrowdingFovealGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-foveal-crowding-by-grade.', input$fileType),
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
    
    output$downloadRsvpFovealAcuityAgePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-acuity-by-age.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpAcuityFoveal()[[1]] + 
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
            plot = rsvpAcuityFoveal()[[1]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    
    output$downloadRsvpFovealAcuityGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-acuity-by-grade.', input$fileType),
        sep = "-"
      ),
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
    
    output$downloadRsvpPeripheralAcuityAgePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-peripheral-acuity-by-age.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        if (input$fileType == "png") {
          ggsave(
            "tmp.svg",
            plot = rsvpAcuityPeripheral()[[1]]+ 
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
            plot = rsvpAcuityPeripheral()[[1]] + 
              plt_theme,
            device = svg,
            width = 8,
            height = 6,
            dpi = 300
          )
        }
      }
    )
    
    output$downloadRsvpPeripheralAcuityGradePlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('rsvp-vs-acuity-periphreal-by-grade.', input$fileType),
        sep = "-"
      ),
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

