#### Quality server module ####
qualityTabServer <- function(id,
                             files,
                             df_list,
                             conditionNames,
                             minDegPlots,
                             experiment_names,
                             fileType,
                             uploaded_file,
                             app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

  histogramsQuality <- reactive({
    if (is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }

    app_profile_time(app_profiler, "Quality histogram list", {
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
  })
  

  qualityHistRenderCount <- reactiveVal(0)

  observeEvent(histogramsQuality(), { qualityHistRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(files(), { qualityHistRenderCount(0) }, ignoreInit = TRUE)

  observe({
    total <- length(histogramsQuality()$plotList)
    current <- qualityHistRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      qualityHistRenderCount(current + 1)
    }
  })

  scatterQuality <- reactive({
    if (is.null(uploaded_file()) | is.null(files())) {
      return(list(plotList = list(),
                  fileNames = list()))
    }
    app_profile_time(app_profiler, "Quality scatter list", {
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
            ns(paste0("qualityHist", i)),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            ns(paste0("qualityHist", i + 1)),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            ns(paste0("qualityHist", i + 2)),
            width = "100%",
            height = "100%"
          ),
          type = 4),
          shinycssloaders::withSpinner(plotOutput(
            ns(paste0("qualityHist", i + 3)),
            width = "100%",
            height = "100%"
          ),
          type = 4)
        )
      # Create a row with 4 download buttons
      out[[i + 1]] <-
        splitLayout(
          cellWidths = c("25%", "25%", "25%", "25%"),
          downloadButton(ns(paste0("downloadQualityHist", i)), 'Download'),
          downloadButton(ns(paste0("downloadQualityHist", i + 1)), 'Download'),
          downloadButton(ns(paste0("downloadQualityHist", i + 2)), 'Download'),
          downloadButton(ns(paste0("downloadQualityHist", i + 3)), 'Download')
        )
      i <- i + 4
    }
    
    # Handle any remaining histograms (fewer than 4)
    remaining <- length(histogramsQuality()$plotList) - i + 1
    if (remaining > 0) {
      plotOutputs <- lapply(1:remaining, function(j) {
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("qualityHist", i + j - 1)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      })
      downloadButtons <- lapply(1:remaining, function(j) {
        downloadButton(ns(paste0("downloadQualityHist", i + j - 1)), 'Download')
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
            # Don't add hist_theme to placeholder plots
            plot_to_save <- if (is_placeholder_plot(histogramsQuality()$plotList[[ii]])) {
              histogramsQuality()$plotList[[ii]]
            } else {
              histogramsQuality()$plotList[[ii]] + hist_theme
            }
            ggsave(
              file = tmp_svg,
              plot = plot_to_save,
              device = svglite,
              width = 4,
              height = 3.5,
              unit = 'in',
              limitsize = FALSE
            )
            # Fit image to the container width to avoid horizontal scrollbars
            disp_w <- session$clientData[[paste0("output_", ns(paste0("qualityHist", ii)), "_width")]]
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
              fileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(histogramsQuality()$plotList[[ii]])) return(invisible(NULL))
              
              if (fileType() == "png") {
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
                    fileType() == "svg",
                    svglite::svglite,
                    fileType()
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
          ns(paste0("qualityScatter", i)),
          width = "100%",
          height = "100%"
        ),
        type = 4),
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("qualityScatter", i + 1)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        downloadButton(ns(paste0("downloadQualityScatter", i)), 'Download'),
        downloadButton(ns(paste0("downloadQualityScatter", i +
                               1)), 'Download')
      )
      i = i + 2
    }
    if (i == length(scatterQuality()$plotList)) {
      out[[i]] <- splitLayout(
        cellWidths = c("50%", "50%"),
        shinycssloaders::withSpinner(plotOutput(
          ns(paste0("qualityScatter", i)),
          width = "100%",
          height = "100%"
        ),
        type = 4)
      )
      out[[i + 1]] <- splitLayout(cellWidths = c("50%", "50%"),
                                  downloadButton(ns(paste0("downloadQualityScatter", i)), 'Download'))
    }
    for (j in seq_along(scatterQuality()$plotList)) {
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
              fileType()
            ),
            content = function(file) {
              # Skip download if plot is NULL/NA/placeholder
              if (is_placeholder_plot(scatterQuality()$plotList[[ii]])) return(invisible(NULL))
              
              if (fileType() == "png") {
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
                    fileType() == "svg",
                    svglite::svglite,
                    fileType()
                  )
                )
              }
            }
          )
      })
    }
    return(out)
  })
  
  downloadSpecs <- reactive({
    c(
      plot_list_download_specs(histogramsQuality()$plotList, histogramsQuality()$fileNames, theme = hist_theme, width = 4, height = 4),
      plot_list_download_specs(scatterQuality()$plotList, scatterQuality()$fileNames, theme = plt_theme_scatter, width = 7, height = 4, append_index = TRUE)
    )
  })

  list(downloadSpecs = downloadSpecs)

  })
}
