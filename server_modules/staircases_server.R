#### Staircases server module ####
staircasesTabServer <- function(id, files, df_list, conditionNames, thresholdParameter, fileType, app_profiler = NULL) {
  moduleServer(id, function(input, output, session) {
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
