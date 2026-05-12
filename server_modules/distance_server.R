#### Distance server module ####
distanceTabServer <- function(id,
                              files,
                              df_list,
                              experiment_names,
                              minRulerCm,
                              calibrateTrackDistanceCheckLengthSDLogAllowed,
                              fileType,
                              uploaded_file,
                              app_profiler = NULL,
                              maxDistanceDotSlots = 20,
                              maxDistanceScatterSlots = 20) {
  moduleServer(id, function(input, output, session) {

  apply_direct_png_theme <- function(plot) {
    default_text_layer_size <- 3
    stats_text_layer_size <- 4
    text_layer_multiplier <- 2
    lineheight_multiplier <- 0.5

    # Layer objects can be shared across ggplot copies. Deep-copy before mutating
    # layer params so PNG-only text tuning cannot leak into SVG/download renders.
    png_plot <- unserialize(serialize(plot, NULL))
    png_plot <- png_plot +
      ggplot2::theme(
        plot.title = ggplot2::element_text(size = 18, lineheight = lineheight_multiplier),
        plot.subtitle = ggplot2::element_text(size = 36, lineheight = lineheight_multiplier),
        axis.title = ggplot2::element_text(size = 28, lineheight = lineheight_multiplier),
        axis.text = ggplot2::element_text(size = 28, lineheight = lineheight_multiplier),
        legend.title = ggplot2::element_text(size = 28, lineheight = lineheight_multiplier),
        legend.text = ggplot2::element_text(size = 20, lineheight = lineheight_multiplier),
        strip.text = ggplot2::element_text(size = 28, lineheight = lineheight_multiplier),
        plot.caption = ggplot2::element_text(size = 20, lineheight = lineheight_multiplier)
      )

    for (layer_idx in seq_along(png_plot$layers)) {
      geom <- png_plot$layers[[layer_idx]]$geom
      if (inherits(geom, "GeomText") || inherits(geom, "GeomLabel") || inherits(geom, "GeomTextNpc")) {
        layer_label <- as.character(c(
          png_plot$layers[[layer_idx]]$aes_params$label,
          png_plot$layers[[layer_idx]]$geom_params$label
        ))
        is_stats_layer <- any(grepl("N =|Sessions=|Mean =|SD =", layer_label))

        size <- png_plot$layers[[layer_idx]]$aes_params$size
        if (is.null(size)) size <- png_plot$layers[[layer_idx]]$geom_params$size
        base_size <- if (is_stats_layer) stats_text_layer_size else default_text_layer_size
        scaled_size <- if (is.null(size)) {
          base_size * text_layer_multiplier
        } else {
          size * text_layer_multiplier
        }
        png_plot$layers[[layer_idx]]$aes_params$size <- scaled_size
        png_plot$layers[[layer_idx]]$geom_params$size <- scaled_size

        lineheight <- png_plot$layers[[layer_idx]]$aes_params$lineheight
        if (is.null(lineheight)) lineheight <- png_plot$layers[[layer_idx]]$geom_params$lineheight
        scaled_lineheight <- if (is.null(lineheight)) {
          0.6
        } else {
          lineheight * lineheight_multiplier
        }
        png_plot$layers[[layer_idx]]$aes_params$lineheight <- scaled_lineheight
        png_plot$layers[[layer_idx]]$geom_params$lineheight <- scaled_lineheight
      }
    }

    png_plot
  }

  distanceCalibration <- reactive({
    app_profile_time(app_profiler, "Distance calibration", {
      get_distance_calibration(files()$data_list, minRulerCm())
    })
  }) %>% bindCache(uploaded_file()$datapath, minRulerCm())
  
  participantInfoForDistance <- reactive({
    participant_info <- NULL
    if (!is.null(df_list()) && is.list(df_list()) && "participant_info" %in% names(df_list())) {
      participant_info <- df_list()$participant_info
    }
    participant_info
  })

  distanceDotPlotsBundle <- reactive({
    app_profile_time(app_profiler, "Distance dot bundle", {
      plot_distance_dot_bundle(
        distanceCalibration(),
        calibrateTrackDistanceCheckLengthSDLogAllowed(),
        participant_info = participantInfoForDistance()
      )
    })
  })

  distanceScatterPlotsBundle <- reactive({
    app_profile_time(app_profiler, "Distance scatter bundle", {
      plot_distance_scatter_bundle(
        distanceCalibration(),
        calibrateTrackDistanceCheckLengthSDLogAllowed(),
        participant_info = participantInfoForDistance()
      )
    })
  })
  #### dotPlots ####
  dotPlots <- reactive({
    app_profile_time(app_profiler, "Distance dot plot list", {
    if (is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    
    l         <- list()
    fileNames <- list()
    
    # SD histogram and distance-related dot plots go here with larger sizing
    dp <- distanceDotPlotsBundle()
    if (is.null(dp)) {
      return(list(plotList = list(), fileNames = list()))
    }
    bs_vd <- if (!is.null(dp$bs_vd)) dp$bs_vd else list(mean_plot = NULL, sd_plot = NULL)

    # Build static_calls list using a spec table + loop.
    # This is the set of "dot/histogram-style" distance plots shown in dotPlots.
    static_calls <- list()
    get_nested <- function(x, keys) {
      for (k in keys) {
        if (is.null(x) || !is.list(x) || !(k %in% names(x))) return(NULL)
        x <- x[[k]]
      }
      x
    }
    add_from_keys <- function(plot_keys, height_keys, fname) {
      plot <- get_nested(dp, plot_keys)
      if (is.null(plot)) return(invisible(NULL))
      height <- get_nested(dp, height_keys)
      static_calls[[length(static_calls) + 1]] <<- list(plot = plot, height = height, fname = fname)
      invisible(NULL)
    }
    specs <- list(
      # ===== 1. PIXEL DENSITY HISTOGRAMS =====
      list(plot_keys = c("sizeCheck", "sd_hist", "plot"),
           height_keys = c("sizeCheck", "sd_hist", "height"),
           fname = "histogram-of-SD-of-log10-pixel-density"),
      list(plot_keys = c("raw_pxPerCm_hist", "plot"),
           height_keys = c("raw_pxPerCm_hist", "height"),
           fname = "histogram-of-raw-pxPerCm-over-remeasured"),

      # ===== 2. RULER AND OBJECT LENGTH HISTOGRAMS =====
      list(plot_keys = c("sizeCheck", "ruler_hist", "plot"),
           height_keys = c("sizeCheck", "ruler_hist", "height"),
           fname = "histogram-of-ruler-length-cm"),
      list(plot_keys = c("object_length_hist", "plot"),
           height_keys = c("object_length_hist", "height"),
           fname = "histogram-of-object-length-cm"),
      list(plot_keys = c("raw_objectMeasuredCm_hist", "plot"),
           height_keys = c("raw_objectMeasuredCm_hist", "height"),
           fname = "histogram-of-raw-objectMeasuredCm-over-median"),

      # ===== 3. fOverWidth HISTOGRAMS =====
      list(plot_keys = c("calibrated_over_median_hist", "plot"),
           height_keys = c("calibrated_over_median_hist", "height"),
           fname = "histogram-of-fOverWidth-median-calibration-over-median-check"),
      list(plot_keys = c("raw_fOverWidth_hist", "plot"),
           height_keys = c("raw_fOverWidth_hist", "height"),
           fname = "histogram-of-fOverWidth-calibration-over-median-check"),
      list(plot_keys = c("calibration_rejected_proportion_hist", "plot"),
           height_keys = c("calibration_rejected_proportion_hist", "height"),
           fname = "histogram-of-proportion-calibration-snapshots-rejected"),
      list(plot_keys = c("check_rejected_proportion_hist", "plot"),
           height_keys = c("check_rejected_proportion_hist", "height"),
           fname = "histogram-of-proportion-check-snapshots-rejected"),
      list(plot_keys = c("accepted_calib_ratio_hist", "plot"),
           height_keys = c("accepted_calib_ratio_hist", "height"),
           fname = "histogram-of-accepted-calibration-fOverWidth-ratio"),
      list(plot_keys = c("accepted_check_ratio_hist", "plot"),
           height_keys = c("accepted_check_ratio_hist", "height"),
           fname = "histogram-of-accepted-check-fOverWidth-ratio"),
      list(plot_keys = c("rejected_calib_ratio_hist", "plot"),
           height_keys = c("rejected_calib_ratio_hist", "height"),
           fname = "histogram-of-rejected-calibration-fOverWidth-ratio"),
      list(plot_keys = c("rejected_check_ratio_hist", "plot"),
           height_keys = c("rejected_check_ratio_hist", "height"),
           fname = "histogram-of-rejected-check-fOverWidth-ratio"),
      list(plot_keys = c("fOverWidth_hist_check", "plot"),
           height_keys = c("fOverWidth_hist_check", "height"),
           fname = "histogram-of-fOverWidth-check"),
      list(plot_keys = c("fOverWidth_hist_calibration", "plot"),
           height_keys = c("fOverWidth_hist_calibration", "height"),
           fname = "histogram-of-fOverWidth-calibration"),
      list(plot_keys = c("fOverWidth_hist_all_calibration", "plot"),
           height_keys = c("fOverWidth_hist_all_calibration", "height"),
           fname = "histogram-of-fOverWidth-all-calibration"),
      list(plot_keys = c("fOverWidth_hist_all_check", "plot"),
           height_keys = c("fOverWidth_hist_all_check", "height"),
           fname = "histogram-of-fOverWidth-all-check")
    )
    for (s in specs) {
      add_from_keys(s$plot_keys, s$height_keys, s$fname)
    }

    # Blindspot dot-histograms (already built inside plot_distance)
    if (!is.null(bs_vd$mean_plot) && !is.null(bs_vd$mean_plot$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = bs_vd$mean_plot$plot,
        height = bs_vd$mean_plot$height,
        fname = "mean-of-left-and-right-viewing-distances-blindspot"
      )
    }
    if (!is.null(bs_vd$sd_plot) && !is.null(bs_vd$sd_plot$plot)) {
      static_calls[[length(static_calls) + 1]] <- list(
        plot = bs_vd$sd_plot$plot,
        height = bs_vd$sd_plot$height,
        fname = "SD-of-log10-left-and-right-viewing-distances-blindspot"
      )
    }
    
    heights <- list()
    for (call in static_calls) {
      res <- append_plot_list(l, 
                              fileNames,
                              add_experiment_title(call$plot, experiment_names()), 
                              call$fname,
                              height = call$height,
                              heights = heights)
      l <- res$plotList
      fileNames <- res$fileNames
      heights <- res$heights
    }
    
    log_debug("Distance histograms: ", length(l), "/", length(static_calls), " added")
    
    return(list(
      plotList = l,
      fileNames = fileNames,
      heights = heights
    ))
    })
  }) %>% bindCache(uploaded_file()$datapath, minRulerCm(), calibrateTrackDistanceCheckLengthSDLogAllowed())
  
  # Progressive rendering controls for distance dot plots.
  dotRenderCount <- reactiveVal(0)
  dotRenderedCount <- reactiveVal(0)

  observeEvent(dotPlots(), {
    dotRenderCount(0)
    dotRenderedCount(0)
  }, ignoreInit = FALSE)
  observeEvent(files(), {
    dotRenderCount(0)
    dotRenderedCount(0)
  }, ignoreInit = TRUE)

  observe({
    total <- min(length(dotPlots()$plotList), maxDistanceDotSlots)
    current <- dotRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      dotRenderCount(current + 1)
    }
  })

  dotImagesReady <- reactive({
    total <- min(length(dotPlots()$plotList), maxDistanceDotSlots)
    is.null(total) || total <= 0 || dotRenderedCount() >= total
  })

  #### scatterDistance ####
  # NOTE: sizeCheck plots are produced by the split distance bundles and read from their local bundle objects.
  
  scatterDistance <- reactive({
    app_profile_time(app_profiler, "Distance scatter plot list", {
    if (is.null(uploaded_file()) | is.null(files())) {
      return(list(plotList = list(), fileNames = list()))
    }
    l <- list()
    fileNames <- list()

    dotPlots()
    req(dotImagesReady())

    # Distance-specific plots come from the scatter-only distance bundle.
    dp <- distanceScatterPlotsBundle()
    if (is.null(dp)) {
      return(list(plotList = list(), fileNames = list()))
    }
    distance_production_plots <- dp$distance_production

    plot_calls <- list(
      # ===== 1. TWO PIXEL DENSITY PLOTS =====
      list(plot = if(!is.null(dp$sizeCheck)) dp$sizeCheck$density_vs_length$plot else NULL,
           height = if(!is.null(dp$sizeCheck)) dp$sizeCheck$density_vs_length$height else NULL,
           fname = 'pixel-density-vs-requested-length'),
      list(plot = if(!is.null(dp$sizeCheck)) dp$sizeCheck$density_ratio_vs_sd$plot else NULL,
           height = if(!is.null(dp$sizeCheck)) dp$sizeCheck$density_ratio_vs_sd$height else NULL,
           fname = 'credit-card-pixel-density-vs-SD-log-remeasured-pixel-density'),
      
      # ===== 2. FIVE SCATTER DIAGRAMS W FEW POINTS PER SESSION =====
      list(plot = dp$fOverWidth_scatter$plot, height = dp$fOverWidth_scatter$height, fname = 'fOverWidth-vs-max-width'),
      list(plot = dp$calibration_over_check_vs_check$plot, height = dp$calibration_over_check_vs_check$height, fname = 'focal-length-calibration-over-check-vs-check'),
      list(plot = dp$fOverWidth_calibration_vs_check$plot, height = dp$fOverWidth_calibration_vs_check$height, fname = 'fOverWidth-calibration-vs-check'),
      list(plot = dp$fOverWidth_cal_vs_check_scatter$plot, height = dp$fOverWidth_cal_vs_check_scatter$height, fname = 'fOverWidth-median-calibration-vs-median-check'),
      
      # ===== 3. SIX PLOTS W 8 CONNECTED DOTS FROM CHECK PHASE =====
      list(plot = dp$imb_vs_rb$plot, height = dp$imb_vs_rb$height, fname = 'imageBasedEyesToPointCm-vs-rulerBasedEyesToPointCm'),
      list(plot = dp$imb_over_rb$plot, height = dp$imb_over_rb$height, fname = 'imageBasedEyesToPointCm-over-rulerBasedEyesToPointCm'),
      list(plot = if(!is.null(dp$eyeToPoint)) dp$eyeToPoint$plot else NULL, height = if(!is.null(dp$eyeToPoint)) dp$eyeToPoint$height else NULL, fname = 'imageBasedEyesToPointCm-vs-rulerBasedEyesToFootCm'),
      list(plot = if(!is.null(dp$eyesToFoot_estimated)) dp$eyesToFoot_estimated$plot else NULL, height = if(!is.null(dp$eyesToFoot_estimated)) dp$eyesToFoot_estimated$height else NULL, fname = 'imageBasedEyesToFootCm-vs-rulerBasedEyesToFootCm'),
      list(plot = if(!is.null(dp$ipd)) dp$ipd$ipdOverWidth_vs_rulerBasedEyesToFootCm$plot else NULL,
           height = if(!is.null(dp$ipd)) dp$ipd$ipdOverWidth_vs_rulerBasedEyesToFootCm$height else NULL,
           fname = 'ipdOverWidth-vs-rulerBasedEyesToFootCm'),
      list(plot = if(!is.null(dp$ipd)) dp$ipd$ipdOverWidth_times_rulerBasedEyesToFootCm_vs_rulerBasedEyesToFootCm$plot else NULL,
           height = if(!is.null(dp$ipd)) dp$ipd$ipdOverWidth_times_rulerBasedEyesToFootCm_vs_rulerBasedEyesToFootCm$height else NULL,
           fname = 'fOverWidth-vs-rulerBasedEyesToFootCm'),
      list(plot = if(!is.null(dp$ipd)) dp$ipd$fOverWidth_over_median_check_vs_rulerBasedEyesToFootCm$plot else NULL,
           height = if(!is.null(dp$ipd)) dp$ipd$fOverWidth_over_median_check_vs_rulerBasedEyesToFootCm$height else NULL,
           fname = 'fOverWidth-over-median-check-vs-rulerBasedEyesToFootCm'),
      
      # ===== 4. TWO HIDDEN BLINDSPOT PLOTS =====
      list(plot = if(!is.null(distance_production_plots)) distance_production_plots$error_vs_blindspot_diameter$plot else NULL,
           height = if(!is.null(distance_production_plots)) distance_production_plots$error_vs_blindspot_diameter$height else NULL,
           fname = 'check-distance-error-vs-blindspot-diameter'),
      list(plot = dp$calibrated_over_mean_vs_spot$plot, height = dp$calibrated_over_mean_vs_spot$height, fname = 'calibrated-over-mean-factorVpxCm-vs-spot-diameter'),
      
      # ===== 5. THREE XY FOOT LOCATION PLOTS =====
      list(plot = dp$foot_position_calibration$plot, height = dp$foot_position_calibration$height, fname = 'foot-position-during-calibration',
           render_mode = "direct_png"),
      list(plot = dp$eye_feet_position$plot, height = dp$eye_feet_position$height, fname = 'fOverWidth-calibration-over-median-check-vs-foot-position-during-calibration',
           render_mode = "direct_png"),
      list(plot = if(!is.null(dp$eye_feet_check)) dp$eye_feet_check$plot else NULL,
           height = if(!is.null(dp$eye_feet_check)) dp$eye_feet_check$height else NULL,
           fname = 'measured-over-requested-distance-vs-foot-position-during-check',
           render_mode = "direct_png")
    )

    heights <- list()
    renderModes <- list()
    for (call in plot_calls) {
      plot <- call$plot
      if (!is.null(plot)) {
        plot <- add_experiment_title(plot, experiment_names())
      } else {
      }
      res <- append_plot_list(l, fileNames, plot, call$fname, height = call$height, heights = heights)
      l <- res$plotList
      fileNames <- res$fileNames
      heights <- res$heights
      if (length(l) > length(renderModes)) {
        render_mode <- if (!is.null(call$render_mode)) call$render_mode else "svg_png"
        renderModes[[length(renderModes) + 1]] <- render_mode
      }
    }
    
    log_debug("Distance scatters: ", length(l), "/", length(plot_calls), " added")

    return(list(
      plotList = l,
      fileNames = fileNames,
      heights = heights,
      renderModes = renderModes
    ))
    })
  }) %>% bindCache(uploaded_file()$datapath, minRulerCm(), calibrateTrackDistanceCheckLengthSDLogAllowed())

  distanceScatterRenderCount <- reactiveVal(0)

  mergedParticipantDistanceTable <- reactive({
    if (is.null(uploaded_file()) | is.null(files())) {
      return(tibble())
    }
    app_profile_time(app_profiler, "Distance merged participant table", {
      get_merged_participant_distance_info(distanceCalibration(), df_list()$participant_info)
    })
  })
  
  output$IsMergedParticipantDistanceTable <- reactive({
    return(nrow(mergedParticipantDistanceTable()) > 0)
  })
  outputOptions(output, "IsMergedParticipantDistanceTable", suspendWhenHidden = FALSE)

  #### Merged Participant Distance Table ####
  
  output$mergedParticipantDistanceTable <- DT::renderDataTable({
    app_profile_time(app_profiler, "Distance merged participant table render", {
    table_data <- mergedParticipantDistanceTable()
    
    col_names <- names(table_data)
    
  
    named_widths <- c(
      "PavloviaParticipantID" = "100px",
      "Object" = "200px",      
      "Comment" = "500px"    
    )
    

    default_width <- "60px"
    
    # Build columnDefs list
    column_defs <- lapply(seq_along(col_names), function(i) {
      col_name <- col_names[i]
      width <- if (col_name %in% names(named_widths)) named_widths[[col_name]] else default_width
      list(width = width, targets = i - 1L)
    })
    # Compute indices (1-based for nth-child) of Object/Comment columns if present
    object_idx <- if ("Object" %in% names(table_data)) which(names(table_data) == "Object")[1] else NA_integer_
    comment_idx <- if ("Comment" %in% names(table_data)) which(names(table_data) == "Comment")[1] else NA_integer_
    # Build initComplete JS that only aligns existing columns
    js_align <- paste0(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'font-size': '13px', 'padding': '2px 4px'});",
      "$(this.api().table().body()).css({'font-size': '12px', 'padding': '2px 4px'});",
      "$('td').css({'text-align': 'center'});",
      "$('th').css({'text-align': 'center'});",
      if (!is.na(object_idx)) sprintf("$('td:nth-child(%d)').css('text-align','left');$('th:nth-child(%d)').css('text-align','left');", object_idx, object_idx) else "",
      if (!is.na(comment_idx)) sprintf("$('td:nth-child(%d)').css('text-align','left');$('th:nth-child(%d)').css('text-align','left');", comment_idx, comment_idx) else "",
      "}"
    )
    datatable(
      table_data,
              class = list(stripe = FALSE, 'compact'),
              selection = 'none',
              extensions = 'FixedHeader',
              filter = "top",
              escape = FALSE,
              options = list(
                autoWidth = TRUE,
                paging = FALSE,
                scrollX = TRUE,
                fixedHeader = TRUE,
        columnDefs = column_defs,
        initComplete = JS(js_align)
      ),
      rownames = FALSE
    )
    })
  })
  
  output$downloadParticipantDistanceInfo <- downloadHandler(
    filename = function() {
      ifelse(
        experiment_names() == "",
        "ParticipantDistanceInfo.xlsx",
        paste0(get_short_experiment_name(experiment_names()), "ParticipantDistanceInfo.xlsx")
      )
    },
    content = function(filename) {
      openxlsx::write.xlsx(mergedParticipantDistanceTable(), file = filename)
    }
  )
  
  #### fixed distance dot plots ####

  for (i in seq_len(maxDistanceDotSlots)) {
    local({
      ii <- i

      output[[paste0("hasDot", ii)]] <- reactive({
        length(dotPlots()$plotList) >= ii && !is_placeholder_plot(dotPlots()$plotList[[ii]])
      })
      outputOptions(output, paste0("hasDot", ii), suspendWhenHidden = FALSE)

      output[[paste0("dotTitle", ii)]] <- renderText({
        req(length(dotPlots()$fileNames) >= ii)
        dotPlots()$fileNames[[ii]]
      })

      output[[paste0("dot", ii)]] <- renderImage({
        req(ii <= dotRenderCount())
        req(length(dotPlots()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Distance dot image ", ii), {
        tryCatch({
          heights <- dotPlots()$heights
          height_in <- if (!is.null(heights) && length(heights) >= ii && !is.null(heights[[ii]])) heights[[ii]] else 4
          tmp_svg <- tempfile(fileext = ".svg")
          ggsave(
            file = tmp_svg,
            plot = dotPlots()$plotList[[ii]],
            device = svglite,
            width = 6,
            height = height_in,
            unit = "in",
            limitsize = FALSE
          )
          disp_w <- 600
          scale <- 2
          png_w <- disp_w * scale
          png_h <- round((height_in / 6) * png_w)
          outfile <- tempfile(fileext = ".png")
          rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
          if (isolate(dotRenderedCount()) < ii) dotRenderedCount(ii)
          list(src = outfile, contenttype = "image/png", width = disp_w, height = round(png_h / scale))
        }, error = function(e) {
          error_plot <- ggplot() +
            annotate(
              "text",
              x = 0.5,
              y = 0.5,
              label = paste("Error:", e$message),
              hjust = 0.5,
              vjust = 0.5
            ) +
            xlim(0, 1) +
            ylim(0, 1) +
            theme_void()

          outfile <- tempfile(fileext = ".svg")
          ggsave(
            file = outfile,
            plot = error_plot,
            device = svglite,
            width = 6,
            height = 4,
            unit = "in"
          )
          if (isolate(dotRenderedCount()) < ii) dotRenderedCount(ii)
          list(
            src = outfile,
            contenttype = "svg",
            alt = paste0("Error in ", dotPlots()$fileNames[[ii]])
          )
        })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("dot", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadDot", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          dotPlots()$fileNames[[ii]],
          ".",
          fileType()
        ),
        content = function(file) {
          req(length(dotPlots()$plotList) >= ii)
          if (is_placeholder_plot(dotPlots()$plotList[[ii]])) return(invisible(NULL))

          heights <- dotPlots()$heights
          height_in <- if (!is.null(heights) && length(heights) >= ii && !is.null(heights[[ii]])) heights[[ii]] else 4
          if (tolower(fileType()) == "png") {
            tmp_svg <- tempfile(fileext = ".svg")
            ggsave(
              file = tmp_svg,
              plot = dotPlots()$plotList[[ii]],
              device = svglite,
              width = 6,
              height = height_in,
              units = "in",
              limitsize = FALSE
            )
            png_w <- 1200
            png_h <- round((height_in / 6) * png_w)
            rsvg::rsvg_png(tmp_svg, file, width = png_w, height = png_h)
          } else {
            ggsave(
              file,
              plot = dotPlots()$plotList[[ii]],
              width = 6,
              height = height_in,
              units = "in",
              limitsize = FALSE,
              device = if (tolower(fileType()) == "svg") svglite::svglite else fileType()
            )
          }
        }
      )
    })
  }

  #### fixed distance scatter plots ####

  observeEvent({
    req(dotImagesReady())
    scatterDistance()
  }, { distanceScatterRenderCount(0) }, ignoreInit = FALSE)
  observeEvent(files(), { distanceScatterRenderCount(0) }, ignoreInit = TRUE)

  observe({
    req(dotImagesReady())
    total <- min(length(scatterDistance()$plotList), maxDistanceScatterSlots)
    current <- distanceScatterRenderCount()
    if (is.null(total) || total <= 0) return()
    if (current < total) {
      invalidateLater(200, session)
      distanceScatterRenderCount(current + 1)
    }
  })

  for (i in seq_len(maxDistanceScatterSlots)) {
    local({
      ii <- i

      output[[paste0("hasDistanceScatter", ii)]] <- reactive({
        req(dotImagesReady())
        length(scatterDistance()$plotList) >= ii && !is_placeholder_plot(scatterDistance()$plotList[[ii]])
      })
      outputOptions(output, paste0("hasDistanceScatter", ii), suspendWhenHidden = FALSE)

      output[[paste0("distanceScatterTitle", ii)]] <- renderText({
        req(dotImagesReady())
        req(length(scatterDistance()$fileNames) >= ii)
        scatterDistance()$fileNames[[ii]]
      })

      output[[paste0("distanceScatter", ii)]] <- renderImage({
        req(dotImagesReady())
        req(ii <= distanceScatterRenderCount())
        req(length(scatterDistance()$plotList) >= ii)
        app_profile_time(app_profiler, paste0("Distance scatter image ", ii), {
        tryCatch({
          scatter_bundle <- scatterDistance()
          height_in <- scatter_bundle$heights[[ii]]
          plot_to_save <- if (is_placeholder_plot(scatterDistance()$plotList[[ii]])) {
            scatter_bundle$plotList[[ii]]
          } else {
            scatter_bundle$plotList[[ii]] + plt_theme_scatter
          }
          disp_w <- 700
          scale <- 2
          png_w <- disp_w * scale
          png_h <- round((height_in / 7) * png_w)
          outfile <- tempfile(fileext = ".png")
          render_mode <- if (!is.null(scatter_bundle$renderModes) && length(scatter_bundle$renderModes) >= ii) {
            scatter_bundle$renderModes[[ii]]
          } else {
            "svg_png"
          }
          if (identical(render_mode, "direct_png")) {
            plot_to_save <- apply_direct_png_theme(plot_to_save)
            ggsave(
              file = outfile,
              plot = plot_to_save,
              width = 7,
              height = height_in,
              unit = "in",
              limitsize = FALSE,
              device = ragg::agg_png,
              dpi = png_w / 7
            )
          } else {
            tmp_svg <- tempfile(fileext = ".svg")
            ggsave(
              file = tmp_svg,
              plot = plot_to_save,
              width = 7,
              height = height_in,
              unit = "in",
              limitsize = FALSE,
              device = svglite
            )
            rsvg::rsvg_png(tmp_svg, outfile, width = png_w, height = png_h)
          }
          list(src = outfile, contenttype = "image/png", width = disp_w, height = round(png_h / scale))
        }, error = function(e) {
          log_error("Distance scatter render: ", conditionMessage(e))
          handle_plot_error(e, paste0("distanceScatter", ii), experiment_names(), scatterDistance()$fileNames[[ii]])
        })
        })
      }, deleteFile = TRUE)
      outputOptions(output, paste0("distanceScatter", ii), suspendWhenHidden = TRUE)

      output[[paste0("downloadDistanceScatter", ii)]] <- downloadHandler(
        filename = function() paste0(
          get_short_experiment_name(experiment_names()),
          scatterDistance()$fileNames[[ii]],
          ".",
          fileType()
        ),
        content = function(file) {
          req(dotImagesReady())
          req(length(scatterDistance()$plotList) >= ii)
          if (is_placeholder_plot(scatterDistance()$plotList[[ii]])) return(invisible(NULL))

          height_in <- scatterDistance()$heights[[ii]]
          if (tolower(fileType()) == "png") {
            tmp_svg <- tempfile(fileext = ".svg")
            ggsave(
              file = tmp_svg,
              plot = scatterDistance()$plotList[[ii]] + plt_theme_scatter,
              width = 7,
              height = height_in,
              unit = "in",
              limitsize = FALSE,
              device = svglite
            )
            png_w <- 1400
            png_h <- round((height_in / 7) * png_w)
            rsvg::rsvg_png(tmp_svg, file, width = png_w, height = png_h)
          } else {
            ggsave(
              file,
              plot = scatterDistance()$plotList[[ii]] + plt_theme_scatter,
              width = 7,
              height = height_in,
              unit = "in",
              limitsize = FALSE,
              device = ifelse(tolower(fileType()) == "svg", svglite::svglite, fileType())
            )
          }
        }
      )
    })
  }

  downloadSpecs <- reactive({
    dot_bundle <- dotPlots()
    scatter_bundle <- scatterDistance()

    dot_specs <- lapply(seq_along(dot_bundle$plotList), function(i) {
      height_in <- if (!is.null(dot_bundle$heights) && length(dot_bundle$heights) >= i && !is.null(dot_bundle$heights[[i]])) {
        dot_bundle$heights[[i]]
      } else {
        6
      }
      if (grepl("sd-log-density-histogram", dot_bundle$fileNames[[i]])) {
        plot_download_spec(dot_bundle$plotList[[i]], dot_bundle$fileNames[[i]], width = 12, height = height_in)
      } else {
        plot_download_spec(dot_bundle$plotList[[i]], dot_bundle$fileNames[[i]], width = 8, height = height_in)
      }
    })

    c(
      dot_specs,
      plot_list_download_specs(
        scatter_bundle$plotList,
        scatter_bundle$fileNames,
        theme = plt_theme_scatter,
        width = 7,
        heights = scatter_bundle$heights
      )
    )
  })

  list(
    downloadSpecs = downloadSpecs,
    mergedParticipantDistanceTable = mergedParticipantDistanceTable
  )
  })
}
