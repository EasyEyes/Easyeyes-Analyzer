arabic_to_western <- function(x) {
  chartr("٠١٢٣٤٥٦٧٨٩", "0123456789", x)
}


pxToPt <- function(px, pxPerCm) {
  return ((px / pxPerCm) * 72) / 2.54
}

ptToPx <- function(pt, pxPerCm) {
  return ((2.54 * pt) / 72) * pxPerCm
}

# Helper to get first non-NA calibration parameter value
get_first_non_na <- function(values) {
  non_na_values <- values[!is.na(values) & values != ""]
  if (length(non_na_values) > 0) {
    return(non_na_values[1])
  }
  return(NA)
}


# Helper function to add experiment name to plot title
add_experiment_title <- function(plot, experiment_name) {
  short_name <- get_short_experiment_name(experiment_name)
  
  if (is.null(plot) || is.null(short_name) || short_name == "") {
    return(plot)
  }

 
  # Remove trailing underscore for title display
  short_name <- gsub("_$", "", short_name)
  # Get the current title
  original_title <- plot$labels$title
  
  # If there's no original title, use empty string
  if (is.null(original_title)) {
    original_title <- ""
  }
  
  # Create new title with short experiment name and line break
  new_title <- paste0(short_name, "\n", original_title)
  
  # Update the plot title
  plot <- plot + labs(title = new_title)
  
  return(plot)
}

# Consistent way of saving plot using ragg
savePlot <- function(plot, filename, fileType, width = 8, height = 6) {
  if (fileType == "png") {
    ggsave('tmp.svg', plot = plot, width = width, unit = 'in', device = svglite)
    save_png_with_ragg("tmp.svg", filename, width = width*300, height = height*300)
  } else {
    ggsave(
      filename = filename,
      plot = plot,
      width = width,
      height = height,
      unit = 'in',
      device = ifelse(fileType == "svg", svglite::svglite, fileType)
    )
  }
}

save_plot_with_error_handling <- function(plot, filename, 
                                          width = 6, height = 4,
                                          size = 5,
                                          unit = 'in', theme = NULL, 
                                          colorPalette = NULL, plotTitle = "") {
  tryCatch({
    # Apply theme and colorPalette if provided
    if (!is.null(theme)) plot <- plot + theme
    if (!is.null(colorPalette)) plot <- plot + scale_color_manual(values = colorPalette)
    ggsave(
      file = filename,
      plot = plot,
      device = svglite,
      width = width,
      height = height,
      unit = unit,
      limitsize = FALSE
    )
    list(src = filename, contenttype = 'svg')
  }, error = function(e) {
    # Show error in a ggplot-friendly way
    error_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = paste("Error:", e$message),
        color = "red",
        size = size,
        hjust = 0.5,
        vjust = 0.5
      ) +
      theme_void() +
      labs(subtitle=plotTitle)
    # Save the error plot to a temp file
    ggsave(
      file = filename,
      plot = error_plot,
      device = svglite,
      width = width,
      height = height,
      unit = unit
    )
    list(
      src = filename,
      contenttype = 'svg',
      alt = paste0("Error in ", plotTitle)
    )
  })
}



safely_execute <- function(expression, error_message = "Error occurred", return_value = NULL) {
  result <- tryCatch({
    # Evaluate the expression
    eval(expression)
    
  }, error = function(e) {
    # If an error occurs, print the error and return specified value
    message("Error: ", e$message)
    if (!is.null(error_message)) {
      message("Context: ", error_message)
    }
    return_value
  })
  
  return(result)
}

append_plot_list <- function(plotList, fileNames, plot, fname, height = NULL, heights = list(), show_placeholder = TRUE) {
  # Default height in inches
  default_height <- 4
  if (!is.null(plot)) {
    plotList[[length(plotList) + 1]] <- plot
    fileNames[[length(fileNames) + 1]] <- fname
    heights[[length(heights) + 1]] <- ifelse(is.null(height), default_height, height)
  } else if (show_placeholder) {
    # Create a minimal placeholder - just "[X] title" on one line
    placeholder_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.5,
        label = paste0(" x ", fname),
        hjust = 0.5,
        vjust = 0.5,
        size = 4.5,
        color = "#666666"
      ) +
      theme_void() +
      theme(
        # Explicitly blank ALL axis elements to override plt_theme
        axis.title = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank(),
        axis.line.x = element_blank(),
        axis.line.y = element_blank(),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank()
      )
    
    # Mark this as a placeholder plot so it can be skipped during download
    class(placeholder_plot) <- c("placeholder_plot", class(placeholder_plot))
    
    plotList[[length(plotList) + 1]] <- placeholder_plot
    fileNames[[length(fileNames) + 1]] <- fname
    heights[[length(heights) + 1]] <- 4  # Small height for one line
  }
  return(list(plotList = plotList, fileNames = fileNames, heights = heights))
}

# Helper function to check if a plot is a placeholder (empty/no data)
is_placeholder_plot <- function(plot) {
  if (is.null(plot)) return(TRUE)
  if ("placeholder_plot" %in% class(plot)) return(TRUE)
  return(FALSE)
}

# Helper function to get a short experiment name for filenames
# If multiple experiments, pick the alphabetically first one
get_short_experiment_name <- function(experiment_names) {
  if (length(experiment_names) == 0) {
    return(NULL)
  }
  
  return(sort(experiment_names)[1])
}

get_stats_label <- function(data, needSlope, needCorr) {
  N = nrow(data)
  if (N == 0) return("")
  corr = format(
    round(
      cor(data$block_avg_log_WPM, 
          data$log_crowding_distance_deg, 
          method = "pearson"), 
      2), 
    nsmall = 2)
  
  
  slope <- data_for_stat %>%
    mutate(WPM = 10^(block_avg_log_WPM),
           cdd = 10^(log_crowding_distance_deg)) %>%
    do(fit = lm(WPM ~ cdd, data = .)) %>%
    transmute(coef = map(fit, tidy)) %>%
    unnest(coef) %>%
    mutate(slope = round(estimate, 2)) %>%
    filter(term == 'cdd') %>%
    select(-term)
}

get_range_breaks_length <- function(x) {
  # get the range for log log plot
  breaks <- c(0.01,0.03,0.1,0.3,1,3,10,30,100,300,1000,3000)
  maxX <- max(x) * 1.1
  minX <- min(x) * .9
  breaks <- breaks[which.max(breaks > minX): which.min(breaks < maxX)]
  length <- maxX / minX / 10 * 1.5
  return(list(
    range = c(minX, maxX),
    breaks = breaks,
    length = length
  ))
}

get_webGL <- function(data_list) {
  webGL <- tibble()
  for (i in 1:length(data_list)) {
    if ('WebGL_Report' %in% names(data_list[[i]])) {
      json_candidates <- unique(data_list[[i]]$WebGL_Report[!is.na(data_list[[i]]$WebGL_Report)])
      if (length(json_candidates) == 0) next
      json_txt <- as.character(json_candidates[1])
      json_txt <- trimws(json_txt)
      if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
        json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
      }
      if (grepl('""', json_txt, fixed = TRUE)) {
        json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
      }
      t <- tryCatch(jsonlite::fromJSON(json_txt), error = function(e) NULL)
      if (is.null(t)) next
      if ('maxTextureSize' %in% names(t)) {
        df <- data.frame(
          participant = data_list[[i]]$participant[1],
          WebGLVersion = t$WebGL_Version,
          maxTextureSize = t$maxTextureSize,
          maxViewportSize = max(unlist(t$maxViewportSize)),
          WebGLUnmaskedRenderer = t$Unmasked_Renderer)
      } else {
        df <- data.frame(
          participant = data_list[[i]]$participant[1],
          WebGLVersion = ifelse("WebGL_Version" %in% names(t), t$WebGL_Version,""),
          maxTextureSize = ifelse("Max_Texture_Size" %in% names(t), t$Max_Texture_Size,""),
          maxViewportSize = ifelse("Max_Viewport_Dims" %in% names(t), max(unlist(t$Max_Viewport_Dims)),""),
          WebGLUnmaskedRenderer = ifelse("Unmasked_Renderer" %in% names(t), max(unlist(t$Unmasked_Renderer)),""))
      }
      df$date = data_list[[i]]$date[1]
      webGL = rbind(webGL, df)
    }
  }
  if (nrow(webGL) == 0) {
    webGL = tibble(
      participant = '',
      WebGLVersion = NA,
      maxTextureSize = NA,
      maxViewportSize = NA,
      WebGLUnmaskedRenderer = NA,
      date=NA)
  } else {
    webGL = tibble(
      participant = webGL$participant,
      date = webGL$date,
      WebGLVersion = webGL$WebGLVersion,
      maxTextureSize = as.numeric(webGL$maxTextureSize),
      maxViewportSize = as.numeric(webGL$maxViewportSize),
      WebGLUnmaskedRenderer = webGL$WebGLUnmaskedRenderer)
  }
  return(webGL)
}

get_N_text <- function(data) {
  # create a text in this format:
  #   conditionName1: N = xx\nconditionName2 N= xx
  text = c()
  t <- data %>%
    group_by(conditionName) %>%
    summarize(n = n(),
              .groups = "drop") %>%
    mutate(text = paste0(conditionName, ': N=', n))
  return(paste0(unique(t$text), collapse='\n'))
}

# Convert to PNG using ragg without rsvg dependency
save_png_with_ragg <- function(
  svg,
  file,
  width = NULL,
  height = NULL,
  plot = NULL,
  dpi = 300,
  disp_w = 700,
  scale = 2,
  aspect = 4 / 6
) {
  # Keep `svg` arg for backward-compatible call signatures.
  if (is.null(plot)) {
    parent <- parent.frame()
    candidate_names <- c('p', 'plot_with_title', 'base_plot', 'error_plot', 'plot')
    for (nm in candidate_names) {
      if (exists(nm, envir = parent, inherits = TRUE)) {
        obj <- get(nm, envir = parent, inherits = TRUE)
        if (inherits(obj, 'ggplot')) {
          plot <- obj
          break
        }
      }
    }

    # Common loop-based containers in this app.
    if (is.null(plot) &&
        exists('plots', envir = parent, inherits = TRUE) &&
        exists('jj', envir = parent, inherits = TRUE)) {
      candidate_plots <- get('plots', envir = parent, inherits = TRUE)
      idx <- get('jj', envir = parent, inherits = TRUE)
      if (length(candidate_plots) >= idx && inherits(candidate_plots[[idx]], 'ggplot')) {
        plot <- candidate_plots[[idx]]
      }
    }
  }

  if (is.null(plot)) {
    plot <- tryCatch(ggplot2::last_plot(), error = function(e) NULL)
  }

  if (!inherits(plot, 'ggplot')) {
    stop('save_png_with_ragg could not infer a ggplot object; pass plot= explicitly.')
  }

  if (is.null(width)) {
    width <- round(disp_w * scale)
  }

  if (is.null(height)) {
    height <- round(aspect * width)
  }

  ggplot2::ggsave(
    filename = file,
    plot = plot,
    device = ragg::agg_png,
    width = width / dpi,
    height = height / dpi,
    units = 'in',
    dpi = dpi,
    limitsize = FALSE
  )

  invisible(file)
}


# Simple font scaling for PNG rendering parity with SVG look.
scale_plot_fonts <- function(plot, font_scale = 1.0) {
  if (!inherits(plot, "ggplot") || is.null(font_scale) || !is.finite(font_scale) || font_scale == 1) {
    return(plot)
  }
  get_scaled_text_element <- function(name, default_size) {
    el <- NULL
    if (!is.null(plot$theme) && !is.null(plot$theme[[name]])) {
      el <- plot$theme[[name]]
    } else {
      base_theme <- ggplot2::theme_get()
      if (!is.null(base_theme[[name]])) {
        el <- base_theme[[name]]
      }
    }
    if (!inherits(el, "element_text")) {
      el <- ggplot2::element_text(size = default_size)
    }
    if (is.null(el$size) || !is.numeric(el$size)) {
      el$size <- default_size
    }
    el$size <- el$size * font_scale
    if (!is.null(el$lineheight) && is.numeric(el$lineheight)) {
      # Keep multiline spacing from exploding when font sizes are scaled up.
      el$lineheight <- max(0.6, el$lineheight / font_scale)
    }
    el
  }

  plot <- plot + do.call(
    ggplot2::theme,
    list(
      text = get_scaled_text_element("text", 11),
      plot.title = get_scaled_text_element("plot.title", 12),
      plot.subtitle = get_scaled_text_element("plot.subtitle", 11),
      plot.caption = get_scaled_text_element("plot.caption", 10),
      axis.title = get_scaled_text_element("axis.title", 11),
      axis.title.x = get_scaled_text_element("axis.title.x", 11),
      axis.title.y = get_scaled_text_element("axis.title.y", 11),
      axis.text = get_scaled_text_element("axis.text", 10),
      axis.text.x = get_scaled_text_element("axis.text.x", 10),
      axis.text.y = get_scaled_text_element("axis.text.y", 10),
      legend.title = get_scaled_text_element("legend.title", 10),
      legend.text = get_scaled_text_element("legend.text", 9),
      strip.text = get_scaled_text_element("strip.text", 10),
      strip.text.x = get_scaled_text_element("strip.text.x", 10),
      strip.text.y = get_scaled_text_element("strip.text.y", 10)
    )
  )

  # Scale explicit text sizes set in geoms (geom_text, geom_text_npc, annotate("text"), etc.)
  for (i in seq_along(plot$layers)) {
    layer <- plot$layers[[i]]
    geom_name <- class(layer$geom)[1]
    if (!grepl("GeomText|GeomLabel", geom_name)) {
      next
    }

    if (!is.null(layer$aes_params$size) && is.numeric(layer$aes_params$size)) {
      plot$layers[[i]]$aes_params$size <- layer$aes_params$size * font_scale
    }
    if (!is.null(layer$geom_params$size) && is.numeric(layer$geom_params$size)) {
      plot$layers[[i]]$geom_params$size <- layer$geom_params$size * font_scale
    }

    # Keep multiline text blocks compact after scaling.
    if (!is.null(layer$aes_params$lineheight) && is.numeric(layer$aes_params$lineheight)) {
      plot$layers[[i]]$aes_params$lineheight <- max(0.6, layer$aes_params$lineheight / font_scale)
    }
    if (!is.null(layer$geom_params$lineheight) && is.numeric(layer$geom_params$lineheight)) {
      plot$layers[[i]]$geom_params$lineheight <- max(0.6, layer$geom_params$lineheight / font_scale)
    }
  }

  plot
}

# renderImage / downloadHandler: direct ggplot -> PNG with theme + geom text scaling for readability.
# `svg` is unused; kept for call sites that still pass NA_character_ or "tmp.svg" from older code.
render_scaled_png_image <- function(
  svg = NA_character_,
  file = NULL,
  width = NULL,
  height = NULL,
  plot = NULL,
  width_in = NULL,
  height_in = NULL,
  disp_w = 700,
  scale = 2,
  dpi = 144,
  font_scale = getOption("easyeyes.png_font_scale", 1.0)
) {
  if (is.null(plot)) {
    plot <- tryCatch(ggplot2::last_plot(), error = function(e) NULL)
  }
  if (!inherits(plot, "ggplot")) {
    stop("render_scaled_png_image requires a ggplot in `plot`.")
  }
  plot <- scale_plot_fonts(plot, font_scale = font_scale)
  effective_dpi <- as.numeric(dpi)
  png_device <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

  if (!is.null(file)) {
    width_px <- if (is.null(width)) round(disp_w * scale) else width
    height_px <- if (is.null(height)) {
      if (!is.null(width_in) && !is.null(height_in)) round((height_in / width_in) * width_px) else width_px
    } else {
      height
    }
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      device = png_device,
      width = width_px / effective_dpi,
      height = height_px / effective_dpi,
      units = "in",
      dpi = effective_dpi,
      limitsize = FALSE
    )
    return(invisible(file))
  }

  if (is.null(width_in) || is.null(height_in)) {
    stop("In render mode, `width_in` and `height_in` are required.")
  }
  png_w <- round(disp_w * scale)
  png_h <- round((height_in / width_in) * png_w)
  outfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(
    filename = outfile,
    plot = plot,
    device = png_device,
    width = png_w / effective_dpi,
    height = png_h / effective_dpi,
    units = "in",
    dpi = effective_dpi,
    limitsize = FALSE
  )
  list(
    src = outfile,
    contenttype = "image/png",
    width = disp_w,
    height = round(png_h / scale)
  )
}

# Enhanced error logging function for debugging
log_detailed_error <- function(e, plot_id = "Unknown Plot") {
  cat("\n=== DETAILED ERROR INFORMATION ===\n")
  cat("Plot ID:", plot_id, "\n")
  cat("Error Message:", e$message, "\n")
  cat("Error Call:", deparse(e$call), "\n")
  if (!is.null(e$trace)) {
    cat("Stack Trace:\n")
    print(e$trace)
  }
  cat("Full Error Object:\n")
  print(e)
  cat("Session Info at time of error:\n")
  print(sessionInfo())
  cat("=== END ERROR INFORMATION ===\n\n")
}

# Word-wrapping helper that never splits words; wraps by max characters per line
wrap_words <- function(text, max_chars) {
  words <- strsplit(text, "\\s+")[[1]]
  lines <- character()
  current <- ""
  for (w in words) {
    if (nchar(current) == 0) {
      current <- w
    } else if (nchar(current) + 1 + nchar(w) <= max_chars) {
      current <- paste(current, w, sep = " ")
    } else {
      lines <- c(lines, current)
      current <- w
    }
  }
  lines <- c(lines, current)
  paste(lines, collapse = "\n")
}

# Enhanced error handler for plot rendering with detailed console logging
handle_plot_error <- function(e, plot_id, experiment_names = NULL, plot_subtitle = "") {
  # Enhanced error logging for debugging - print detailed error info to console
  # log_detailed_error(e, plot_id)
  
  # Create user-friendly error plot
  error_plot <- ggplot() +
    annotate(
      "text",
      x = 0.5,
      y = 0.6,
      label = paste("Error:", e$message),
      color = "red",
      size = 5,
      hjust = 0.5,
      vjust = 0.5
    ) +
    annotate(
      "text",
      x = 0.5,
      y = 0.4,
      label = paste("Plot ID:", plot_id),
      size = 4,
      fontface = "italic"
    ) +
    plt_theme
  
  # Add title if experiment_names function is provided
  if (!is.null(experiment_names)) {
    tryCatch({
      title_text <- experiment_names
      error_plot <- error_plot + labs(title = title_text, subtitle = plot_subtitle)
    }, error = function(title_error) {
      error_plot <<- error_plot + labs(title = "Error getting experiment names", subtitle = plot_subtitle)
    })
  } else {
    error_plot <- error_plot + labs(subtitle = plot_subtitle)
  }
  
  # Save the error plot to a temp file
  outfile <- tempfile(fileext = '.svg')
  ggsave(
    file = outfile,
    plot = error_plot,
    device = svglite,
    width = 6,
    height = 6
  )
  
  return(list(
    src = outfile,
    contenttype = 'image/svg+xml',
    alt = paste0("Error in ", plot_id, ": ", e$message)
  ))
}
