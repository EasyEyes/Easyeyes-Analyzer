#### Staircases server module ####
staircasesTabServer <- function(id, files, df_list, conditionNames, thresholdParameter, fileType, app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
    render_staircases_display_png <- function(plot, width_in, height_in, disp_w) {
      scale <- 2
      png_w <- round(disp_w * scale)
      png_h <- round((height_in / width_in) * png_w)
      outfile <- tempfile(fileext = ".png")
      ggsave(
        file = outfile,
        plot = plot,
        width = width_in,
        height = height_in,
        unit = "in",
        limitsize = FALSE,
        device = ragg::agg_png,
        dpi = png_w / width_in
      )
      list(
        src = outfile,
        contenttype = "image/png",
        width = disp_w,
        height = round(png_h / scale)
      )
    }

    output$questData <- reactive({
      if ('quest' %in% names(df_list())) {
        return(nrow(df_list()$quest > 0))
      }
      return(FALSE)
    })
    outputOptions(output, 'questData', suspendWhenHidden = FALSE)

  #### crowding stair plots
  stairPlot <- reactive({
    app_profile_time(app_profiler, "Staircases plotStaircases", {
      plotStaircases(files()$stairs,
                     thresholdParameter(),
                     conditionNames())
    })
  })
  
  output$stairPlot <- renderImage({
    app_profile_time(app_profiler, "Staircases render image", {
      width_in <- 8
      height_in <- stairPlot()$height
      render_staircases_display_png(
        plot = stairPlot()$plot + plt_theme,
        width_in = width_in,
        height_in = height_in,
        disp_w = 700
      )
    })
  }, deleteFile = TRUE)
  
  output$downloadStairPlot <- downloadHandler(
    filename = paste0(thresholdParameter(), '-staircases.', fileType()),
    content = function(file) {
      if (fileType() == "png") {
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
            fileType()  == "svg",
            svglite::svglite,
            fileType()
          )
        )
      }
    }
  )

  downloadSpecs <- reactive({
    list(plot_download_spec(
      plot = stairPlot()$plot,
      filename = paste0(thresholdParameter(), "-staircases"),
      theme = plt_theme,
      width = 8,
      height = stairPlot()$height
    ))
  })

  list(downloadSpecs = downloadSpecs)

  })
}
