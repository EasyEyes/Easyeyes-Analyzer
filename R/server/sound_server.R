#### Sound server module ####
soundTabServer <- function(id, app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
  ##### SOUND CALIBRATION ####
  
  # OPTIMIZATION: Parse JSON once, reuse across all sound calibration reactives
  # This eliminates 8+ redundant JSON parsing operations
  parsed_json <- reactive({
    if (is.null(input$fileJSON)) return(NULL)
    file_list <- input$fileJSON$data
    app_profile_time(app_profiler, "Sound parse JSON", {
      fromJSON(file_list[1], simplifyDataFrame = F)
    })
  })
  
  irPlots <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound IR plots", {
      get_ir_plots(parsed_json(), sound_data())
    })
  })
  
  sound_data <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound preprocess JSON", {
      preprocessJSON(parsed_json())
    })
  })
  
  iir <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound read IIR JSON", {
      read_iir_JSON(parsed_json())
    })
  })
  
  iirPlots <- reactive({
    app_profile_time(app_profiler, "Sound IIR plots", {
      get_iir_plot(iir(), sound_data())
    })
  })
  
  # record_freq_system_plot <- reactive({
  #   plot_record_freq_system(sound_data())
  # })
  
  record_freq_component_plot <- reactive({
    app_profile_time(app_profiler, "Sound record frequency component plot", {
      plot_record_freq_component(sound_data())
    })
  })
  
  componentIIRPlots <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound component IIR plots", {
      plotComponentIIR(parsed_json(), sound_data())
    })
  })
  
  cumSumPowerPlot <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound cumulative power plot", {
      getCumSumPowerPlot(parsed_json())
    })
  })
  
  componentIR_PSD_plot <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound component IR PSD plot", {
      plotComponentIRPSD(parsed_json(), sound_data())
    })
  })
  
  recording_variation <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound power variation plot", {
      plot_power_variations(parsed_json(), sound_data())
    })
  })
  
  volume_power_variation <- reactive({
    req(parsed_json())
    app_profile_time(app_profiler, "Sound volume power variation plot", {
      plot_volume_power_variations(parsed_json(), sound_data())
    })
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
    return(!is.null(input$fileJSON))
  })
  
  outputOptions(output, 'jsonUploaded', suspendWhenHidden = FALSE)

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
  
  toListenSound <- reactive({
    list(input$fileJSON, input$fileTypeSound)
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
}
