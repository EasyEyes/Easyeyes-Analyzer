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
require(foreach)
require(dplyr)
require(readr)
require(stringr)
require(emojifont)
require(DT)
library(ggpubr)
library(shinyjs)
library(lubridate)
require(ggpp)
library(svglite)
library(magick)
library(patchwork)
# library(showtext)
# library(systemfonts)
# Enables automatic font loading for showtext

# showtext_auto(F)


source('./load_fonts.R')
source('./constant.R')
source("./preprocess.R")

source("threshold_and_warning.R")

source("./error report/random_rgb.R")
source("./error report/summary_table.R")
source("./error report/prolific.R")

source("./plotting/mean_median_plot.R")
source("./plotting/regression_plot.R")
source("./plotting/histogram.R")
source("./plotting/crowding_plot.R")
source("./plotting/test_retest.R")
source("./plotting/scatter_plots.R")
source("./plotting/crowding_sloan_vs_times.R")
source("./plotting/reading_vs_font_size.R")
source("./plotting/customized_inplot_table.R")
source("./plotting/profile_plot.R")
source("./plotting/simulatedRSVP.R")


source("./other/getBits.R")
source("./other/sound_plots.R")
source("./other/read_json.R")
source("./other/csvSplitter.R")
source("./other/formSpree.R")

library(shiny)

#### server code ####

shinyServer(function(input, output, session) {
  #### place holder ####
  
  output$formSpreeDashboard <- renderDataTable({
    t <- monitorFormSpree()
    datatable(t,
              class = list(stripe = FALSE),
              selection = 'none',
              filter = "top",
              options = list(
                autoWidth = TRUE,
                paging = FALSE,
                dom = 't')
    )
  })
  
  output$ex1 <- renderDataTable({
    datatable(tibble())
  })
  
  #### reactive objects ####
  
  
  files <- reactive({
    require(input$file)
    showModal(modalDialog("Reading files", footer = NULL))
    t <- read_files(input$file)
    removeModal()
    return(t)
  })
  
  stairPlots <- reactive({
    require(input$file)
    t <- getStairsPlot(input$file)
    return(t)
  })
  
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
    require(input$file)
    generate_summary_table(data_list())
  })
  threshold_and_warnings <- reactive({
    require(input$file)
    return(generate_threshold(data_list(), summary_list()))
  })
  df_list <- reactive({
    return(generate_rsvp_reading_crowding_fluency(data_list(), summary_list()))
  })
  reading_rsvp_crowding_df <- reactive({
    return(get_mean_median_df(df_list()))
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
  
  # TODO, replace the small multi symbol with \U2A09
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
  
  reading_vs_font_size <- reactive({
    plot_rsvp_vs_x_height(df_list()[[3]])
  })
  
  reading_60cm_1.2mm_vs_1.4mm <- reactive({
    get_60cm_scatter(df_list()[[3]])
  })
  
  reading_30cm_1.2mm_vs_1.4mm <- reactive({
    get_30cm_scatter(df_list()[[3]])
  })
  
  reading_diff_60cm_vs_age <- reactive({
    plot_60cm_speed_diff_vs_age(df_list()[[3]])
  })
  
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
  crowdingBySide <- reactive({
    crowding_by_side(df_list()[[2]])
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
  
  sloan_vs_times <- reactive({
    sloan_vs_times_plots(df_list()[[2]])
  })
  
  sloan_vs_times_means <- reactive({
    sloan_vs_times()[[1]] +
      labs(title = experiment_names(),
           subtitle = "Crowding Sloan vs Times, mean")
  })
  
  sloan_vs_times_sd <- reactive({
    sloan_vs_times()[[2]] +
      labs(title = experiment_names(),
           subtitle = "Crowding Sloan vs Times, sd")
  })
  
  regressionPlot <- reactive({
    regression_plot(df_list()) +
      labs(title = experiment_names(),
           subtitle = "Regression of reading vs crowding") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  regressionAndMeanPlot <- reactive({
    regression_and_mean_plot_byfont(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Regression of reading vs crowding")
  })
  regressionFontPlot <- reactive({
    regression_font(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Regression of reading vs crowding")
  })
  
  regressionFontPlotWithLabel <- reactive({
    regression_font_with_label(df_list(), reading_rsvp_crowding_df()) +
      labs(title = experiment_names(),
           subtitle = "Regression of reading vs crowding")
  })
  
  fluency_histogram <- reactive({
    get_fluency_histogram(df_list()[[4]]) +
      labs(title = experiment_names(),
           subtitle = "English fluency histogram") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
  })
  retention_histogram <- reactive({
    get_reading_retention_histogram(df_list()[[1]]) +
      labs(title = experiment_names(),
           subtitle = "Reading retention histogram") +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "bottom",
        label = paste0("italic('N=')~", length(unique(
          df_list()[[1]]$participant
        )))
      ),
      parse = T)
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
  
  
  # recordFreqFilteredSubtractedNoisePlot <- reactive({
  #   plot_filtered_subtracted_noise(sound_data())
  # })
  #
  # recordFreqSubtractedNoisePlot <- reactive({
  #   plot_record_freq_subtracted_noise(sound_data())
  # })
  
  
  
  
  ##### request from Francesca #####
  
  allMeasures <- reactive({
    get_measures(data_list())
  })
  
  prob_all <- reactive({
    get_prob(get_counts_all(allMeasures())) %>% select(-participant)
  })
  
  prob_each <- reactive({
    get_prob_each_block(get_counts_each_block(allMeasures()))
  })
  
  
  
  
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
                 set.seed(2023)
                 #### summary page ####
                 output$instruction <- renderText(instruction)
                 output$experiment <- renderText(experiment_names())
                 if (!is.null(prolific())) {
                   print(nrow(prolific()))
                   print(nrow(summary_table()))
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
                 #### stairPlots ####
                 output$p1 <- renderPlot({

                   (stairPlots()[[1]][[1]] + plt_theme) / (stairPlots()[[1]][[2]] + plt_theme) / (stairPlots()[[1]][[3]] + plt_theme)
                 }, res = 96)
                 output$p2 <- renderPlot({
                   stairPlots()[[2]] + plt_theme
                 }, res = 96)
                 output$p3 <- renderPlot({
                   stairPlots()[[3]] + plt_theme
                 }, res = 96)
                 
                 #### plots ####
                 output$readingVsXheightLog <- renderPlot({
                   p1 <- reading_vs_font_size()[[1]]
                   p1 +
                     plt_theme +
                     labs(title = paste(
                       c(experiment_names(),
                         "reading speed vs x height"),
                       collapse = "\n"
                     ))
                 }, res = 96)
                 output$readingVsXheightLinear <- renderPlot({
                   p2 <- reading_vs_font_size()[[2]]
                   p2 +
                     plt_theme +
                     labs(title = paste(
                       c(experiment_names(),
                         "reading speed vs x height"),
                       collapse = "\n"
                     ))
                 }, res = 96)
                 
                 output$reading60cm1.2mmVs1.4mm <- renderPlot({
                   p1 <- reading_60cm_1.2mm_vs_1.4mm()
                   p1 +
                     plt_theme +
                     labs(title = paste(
                       c(experiment_names(),
                         "reading speed 1.4mm vs 1.2mm, 60cm"),
                       collapse = "\n"
                     ))
                 }, res = 96)
                 
                 output$reading30cm1.2mmVs1.4mm <- renderPlot({
                   p1 <- reading_30cm_1.2mm_vs_1.4mm()
                   p1 +
                     plt_theme +
                     labs(title = paste(
                       c(experiment_names(),
                         "reading speed 1.4mm vs 1.2mm, 30cm"),
                       collapse = "\n"
                     ))
                 }, res = 96)
                 
                 output$reading60cmDiffVsAge <- renderPlot({
                   p1 <- reading_diff_60cm_vs_age()
                   p1 +
                     plt_theme +
                     labs(title = paste(
                       c(
                         experiment_names(),
                         "reading speed difference vs age, 60cm"
                       ),
                       collapse = "\n"
                     ))
                 }, res = 96)
                 
                 
                 output$meanPlot <- renderPlot({
                   meanPlot() + plt_theme
                 }, res = 96)
                 output$medianPlot <- renderPlot({
                   medianPlot() + plt_theme
                 }, res = 96)
                 #### regression plots #####
                 output$regressionPlot <- renderPlot({
                   regressionPlot() + plt_theme
                 })
                 output$regressionFontPlot <- renderPlot({
                   regressionFontPlot() + plt_theme
                 })
                 output$regressionFontPlotWithLabel <- renderPlot({
                   regressionFontPlotWithLabel() + plt_theme
                 })
                 output$regressionAndMeanPlot <- renderPlot({
                   regressionAndMeanPlot() + plt_theme
                 })
                 #### fluency ####
                 output$fluencyHistogram <- renderPlot({
                   fluency_histogram() + plt_theme
                 })
                 output$retentionHistogram <- renderPlot({
                   retention_histogram() + plt_theme
                 })
                 
                 #### crowding ####
                 output$crowdingScatterPlot <- renderPlot({
                   crowdingPlot()
                 })
                 output$crowdingAvgPlot <- renderPlot({
                   crowdingAvgPlot() + plt_theme
                 })
                 
                 output$SloanVsTimesMeanPlot <- renderPlot({
                   sloan_vs_times_means() + plt_theme
                 })
                 
                 output$SloanVsTimesSDPlot <- renderPlot({
                   sloan_vs_times_sd() + plt_theme
                 })
                 
                 #### test retest ####
                 output$readingTestRetest <- renderPlot({
                   readingTestRetest()
                 })
                 output$crowdingTestRetest <- renderPlot({
                   crowdingTestRetest()
                 })
                 output$rsvpReadingTestRetest <- renderPlot({
                   rsvpReadingTestRetest()
                 })
                 output$readingSpeedRetention <- renderPlot({
                   readingSpeedRetention()
                 })
                 # output$`all participant prob` <- renderTable(prob_all())
                 output$`all participant I` <-
                   renderText(paste0("I(X;Y) = ", round(get_bits(prob_all(
                     
                   )), 2)))
                 # output$`each blcck prob` <- renderTable(prob_each())
                 output$`each block I` <- renderTable(prob_each())
                 
                 
               }
               )
  
  observeEvent(input$fileProlific,{
    prolificData <- read_prolific(input$fileProlific$data)
    if (!is.null(input$file)) {
      app_title$default <- experiment_names()
      output$app_title <- renderText({
        ifelse(app_title$default == "",
               "EasyEyes Analysis",
               app_title$default)
      })
      set.seed(2023)
      output$instruction <- renderText(instruction)
      output$experiment <- renderText(experiment_names())
      combinedTable <- combineProlific(prolificData, summary_table())[[1]]
      participants <- unique(combinedTable$`Pavlovia session ID`)
      prolific_id <- unique(combinedTable$`Prolific participant ID`)
      output$ex1 <- DT::renderDataTable(
        render_summary_datatable(combinedTable, participants, prolific_id)
      )
    } else {
      formSpree <- getFormSpree() %>% filter(`prolificSessionID` %in% unique(prolificData$prolificSessionID))
      combinedTable <- combineProlific(prolificData, formSpree)[[1]]
      participants <- unique(combinedTable$`Pavlovia session ID`)
      prolific_id <- unique(combinedTable$`Prolific participant ID`)
      output$ex1 <- DT::renderDataTable(
        render_summary_datatable(combinedTable, participants, prolific_id)
      )
    }

  })
  
  
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
    output$downloadP1 <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('p1.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = (stairPlots()[[1]][[1]] + downloadtheme) / (stairPlots()[[1]][[2]] + downloadtheme) / (stairPlots()[[1]][[3]] + downloadtheme)
            ,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    output$downloadP2 <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('p2.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot =stairPlots()[[2]] +
            downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType),
          dpi = 200
        )
      }
    )
    output$downloadP3 <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('p3.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = stairPlots()[[3]] +
            downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType),
          dpi = 200
        )
      }
    )
    output$downloadReadingVsXheightLog <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-vs-x-height-log-scale.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = reading_vs_font_size()[[1]] +
            downloadtheme +
            labs(title = paste(
              c(experiment_names(),
                "reading speed vs x height, log scale"),
              collapse = "\n"
            )),
          device = ifelse(input$fileType == "svg", svglite, input$fileType),
          dpi = 200
        )
      }
    )
    
    output$downloadReadingVsXheightLinear <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-vs-x-height-linear.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = reading_vs_font_size()[[2]] +
            downloadtheme +
            labs(title = paste(
              c(
                experiment_names(),
                "reading speed vs x height, linear scale"
              ),
              collapse = "\n"
            )),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadReading60cm1.2mmVs1.4mm <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-1.4mm-vs-1.2mm-60cm.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = reading_60cm_1.2mm_vs_1.4mm() +
            downloadtheme +
            labs(title = paste(
              c(experiment_names(),
                "reading speed 1.4mm vs 1.2mm, 60cm"),
              collapse = "\n"
            )),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadReading30cm1.2mmVs1.4mm <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-1.4mm-vs-1.2mm-30cm.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = reading_30cm_1.2mm_vs_1.4mm() +
            downloadtheme +
            labs(title = paste(
              c(experiment_names(),
                "reading speed 1.4mm vs 1.2mm, 30cm"),
              collapse = "\n"
            )),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadReading60cmDiffVsAge <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('reading-speed-difference-vs-age-60cm.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = reading_diff_60cm_vs_age() +
            downloadtheme +
            labs(title = paste(
              c(
                experiment_names(),
                "reading speed difference vs age, 60cm"
              ),
              collapse = "\n"
            )),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    
    
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
        ggsave(
          file,
          plot = regressionPlot() + downloadtheme + coord_fixed(ratio = 1),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadRegressionAndMeanPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('regression.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = regressionAndMeanPlot() + downloadtheme + coord_fixed(ratio = 1),
          device = ifelse(input$fileType == "svg", svglite, input$fileType),
          width = 8.1,
          height = 8.1,
          dpi = 1200
        )
      }
    )
    output$downloadRegressionFontPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('regression.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = regressionFontPlot() + downloadtheme + coord_fixed(ratio = 1),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadRegressionFontPlotWithLabel <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('regression.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = regressionFontPlotWithLabel() + downloadtheme + coord_fixed(ratio = 1),
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadFluencyHistogram <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('fluency.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = fluency_histogram() + downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadRetentionHistogram <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('retention.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = retention_histogram() + downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadCrowdingScatterPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('crowding_left_vs_right.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = crowdingPlot(),
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
        paste0('sloan_vs_times_mean.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = sloan_vs_times_means() + downloadtheme,
          device = ifelse(input$fileType == "svg", svglite, input$fileType)
        )
      }
    )
    
    output$downloadSloanVsTimesSDPlot <- downloadHandler(
      filename = paste(
        app_title$default,
        paste0('sloan_vs_times_sd.', input$fileType),
        sep = "-"
      ),
      content = function(file) {
        ggsave(
          file,
          plot = sloan_vs_times_sd() + downloadtheme,
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
  
  output$download_splitted <- downloadHandler(
    filename = 'splitted.zip',
    content = function(file){
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      splitted <- splitCSVfunction(input$fileDB$data)
      for (i in 1:length(splitted)) {
        fileName <- unique(splitted[[i]]$fileNames)
        readr::write_csv(splitted[[i]] %>% select(-fileNames), file.path(temp_directory, paste0(fileName, '.csv')), na = '')
      }
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
    })
})