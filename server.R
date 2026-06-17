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
source("./server_modules/anova_server.R")
source("./server_modules/stat_server.R")
source("./server_modules/plots_reading_rsvp_outputs.R")
source("./server_modules/plots_tab_server.R")

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
  
  
  output$instruction <- renderText(instruction)
  
  #### reactive objects ####
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

  output$ex1 <- DT::renderDataTable({
    summary <- summary_table()
    if (is.null(summary) || nrow(summary) == 0) {
      return(empty_summary_datatable())
    }
    participants <- unique(summary$`Pavlovia session ID`)
    prolific_id <- unique(summary$`Prolific participant ID`)
    render_summary_datatable(summary, participants, prolific_id)
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
    app_profile_time(app_profiler, "Plots crowding by side", {
      crowding_by_side(df_list()$crowding)
    })
  })
  
  reading_rsvp_crowding_df <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Plots reading RSVP crowding df", {
      get_mean_median_df(df_list())
    })
  })
  
  corrMatrix <- reactive({
    if (is.null(files())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Plots correlation matrix data", {
      getCorrMatrix(df_list(), files()$pretest)
    })
  })

  minDegPlots <- reactive({
    if (is.null(files()) || is.null(df_list())) {
      return(NULL)
    }
    app_profile_time(app_profiler, "Plots minDeg plots", {
      get_minDeg_plots(files()$data_list,
                       df_list()$acuity,
                       df_list()$crowding,
                       df_list()$quest)
    })
  })
  
  downloadFileType <- reactive(input$fileType)

  distanceTabActive <- reactive({
    isTRUE(input$navbar == "Distance")
  })
  timingTabActive <- reactive({
    isTRUE(input$navbar == "Timing")
  })
  qualityTabActive <- reactive({
    isTRUE(input$navbar == "Quality")
  })
  
  distanceModule <- distanceTabServer(
    "distance",
    files = files,
    df_list = df_list,
    experiment_names = experiment_names,
    minRulerCm = minRulerCm,
    calibrateTrackDistanceCheckLengthSDLogAllowed = calibrateTrackDistanceCheckLengthSDLogAllowed,
    fileType = downloadFileType,
    uploaded_file = reactive(input$file),
    tab_active = distanceTabActive,
    app_profiler = app_profiler
  )

  statTabServer(
    "stats",
    df_list = df_list,
    experiment_names = experiment_names,
    mergedParticipantDistanceTable = distanceModule$mergedParticipantDistanceTable,
    uploaded_file = reactive(input$file)
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
    tab_active = timingTabActive,
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
    tab_active = qualityTabActive,
    app_profiler = app_profiler
  )

  anovaTabServer(
    "anova",
    df_list = df_list,
    experiment_names = experiment_names,
    app_profiler = app_profiler
  )
  
  #### reactive plots #####
  
  #### rsvpCrowding reactive ####
  
  rsvpCrowding <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots RSVP crowding ggiraph", {
      plot_rsvp_crowding(df_list())
    })
  })
  
  rsvpAcuityFoveal <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots RSVP foveal acuity ggiraph", {
      plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp, 'foveal')
    })
  })
  
  rsvpAcuityPeripheral <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots RSVP peripheral acuity ggiraph", {
      plot_acuity_rsvp(df_list()$acuity, df_list()$rsvp, 'peripheral')
    })
  })

  ordinaryAcuityFoveal <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots ordinary foveal acuity ggiraph", {
      plot_acuity_reading(df_list()$acuity, df_list()$reading, 'foveal')
    })
  })

  ordinaryAcuityPeripheral <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots ordinary peripheral acuity ggiraph", {
      plot_acuity_reading(df_list()$acuity, df_list()$reading, 'peripheral')
    })
  })
  
  rsvp_repeated_letter_crowding <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots RSVP repeated letter crowding ggiraph", {
      plot_rsvp_repeated_letter_crowding(df_list())
    })
  })

  ordinaryCrowdingPlots <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots ordinary crowding ggiraph", {
      plot_reading_crowding(df_list())
    })
  })

  readingRepeatedPlots <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots reading repeated letter ggiraph", {
      plot_reading_repeated_letter_crowding(df_list())
    })
  })

  fontAggregatedReadingRsvpCrowding <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots font-aggregated reading RSVP crowding", {
      plot_font_aggregated_reading_rsvp_crowding(df_list())
    })
  })

  fontAggregatedOrdinaryReadingCrowding <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots font-aggregated ordinary reading crowding", {
      plot_font_aggregated_ordinary_reading_crowding(df_list())
    })
  })

  fontAggregatedRsvpCrowding <- reactive({
    req(input$file)
    app_profile_time(app_profiler, "Plots font-aggregated RSVP crowding", {
      plot_font_aggregated_rsvp_crowding(df_list())
    })
  })

  register_plots_reading_rsvp_ggiraph_outputs(
    output = output,
    experiment_names = experiment_names,
    rsvpCrowding = rsvpCrowding,
    rsvpAcuityFoveal = rsvpAcuityFoveal,
    rsvpAcuityPeripheral = rsvpAcuityPeripheral,
    rsvp_repeated_letter_crowding = rsvp_repeated_letter_crowding,
    ordinaryAcuityFoveal = ordinaryAcuityFoveal,
    ordinaryAcuityPeripheral = ordinaryAcuityPeripheral,
    ordinaryCrowdingPlots = ordinaryCrowdingPlots,
    readingRepeatedPlots = readingRepeatedPlots
  )

  plotsTab <- register_plots_tab_server(
    output = output,
    session = session,
    input = input,
    files = files,
    df_list = df_list,
    experiment_names = experiment_names,
    downloadFileType = downloadFileType,
    corrMatrix = corrMatrix,
    minDegPlots = minDegPlots,
    conditionNames = conditionNames,
    minCQAccuracy = minCQAccuracy,
    crowdingBySide = crowdingBySide,
    fontAggregatedReadingRsvpCrowding = fontAggregatedReadingRsvpCrowding,
    fontAggregatedOrdinaryReadingCrowding = fontAggregatedOrdinaryReadingCrowding,
    fontAggregatedRsvpCrowding = fontAggregatedRsvpCrowding,
    app_profiler = app_profiler
  )

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
                  output$experiment <-
                    renderText(experiment_names())
                   
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
      plot_list_download_specs(plotsTab$histograms()$plotList, plotsTab$histograms()$fileNames, theme = hist_theme, width = 3.5, height = 3.5),
      plot_list_download_specs(plotsTab$scatterDiagrams()$plotList, plotsTab$scatterDiagrams()$fileNames, theme = plt_theme_scatter),
      plot_list_download_specs(plotsTab$agePlots()$plotList, plotsTab$agePlots()$fileNames, theme = plt_theme),
      plot_list_download_specs(ggiraph_plots$plotList, ggiraph_plots$fileNames, theme = plt_theme_ggiraph),
      plot_list_download_specs(plotsTab$violinPlots()$plotList, plotsTab$violinPlots()$fileNames, theme = plt_theme, width = 8),
      plot_list_download_specs(plotsTab$fontComparisonPlots()$plotList, plotsTab$fontComparisonPlots()$fileNames, theme = plt_theme, width = 8)
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
