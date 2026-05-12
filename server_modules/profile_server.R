#### Profile server module ####
profileTabServer <- function(id, app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
  observeEvent(input$toShinyOptions, {
    json <- app_profile_time(app_profiler, "Profile parse options JSON", {
      fromJSON(input$toShinyOptions)
    })
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
    
    df <- app_profile_time(app_profiler, "Profile options table", {
      get_profile_table(json, input$transducerType) %>%
        select(-label)
    })
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
    p <- app_profile_time(app_profiler, "Profile filtered plots", {
      getFilteredProfilePlots(
        input$transducerType,
        fromJSON(input$totalData),
        title,
        input$profileSelection
      )
    })
    
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
    
    json <- app_profile_time(app_profiler, "Profile parse filtered JSON", {
      fromJSON(input$toShiny)
    })
    
    df <- app_profile_time(app_profiler, "Profile filtered table", {
      get_profile_table(json, input$transducerType) %>%
        filter(label %in% input$profileSelection) %>%
        select(-label)
    })
    
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
      filename = paste0("profile-plot.", input$fileProfile),
      content = function(file) {
        if (input$fileProfile == "png") {
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
      filename = paste0("profile-plot-shifted.", input$fileProfile),
      content = function(file) {
        if (input$fileProfile == "png") {
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
        if (input$fileProfile == "png") {
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
    app_profile_time(app_profiler, "Profile plots", {
      getProfilePlots(input$transducerType,
                      fromJSON(input$totalData),
                      title)
    })
  })
  
  observeEvent(input$totalData, {
    json <- app_profile_time(app_profiler, "Profile parse total data JSON", {
      fromJSON(input$toShiny)
    })
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
    
    df <- app_profile_time(app_profiler, "Profile total data table", {
      get_profile_table(json, input$transducerType) %>%
        select(-label)
    })
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
  
  toListenProfile <- reactive({
    list(input$totalData, input$fileProfile)
  })
  
  
  observeEvent(toListenProfile(), {
    output$downloadProfilePlot <- downloadHandler(
      filename = paste0("profile-plot.", input$fileProfile),
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
      filename = paste0("profile-plot-shifted.", input$fileProfile),
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
        if (input$fileProfile == "png") {
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

  })
}
