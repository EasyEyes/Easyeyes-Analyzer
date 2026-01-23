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

# Consistent way of saving plot using rsvg_png
savePlot <- function(plot, filename, fileType, width = 8, height = 6) {
  if (fileType == "png") {
    ggsave('tmp.svg', plot = plot, width = width, unit = 'in', device = svglite)
    rsvg::rsvg_png("tmp.svg", filename, width = width*300, height = height*300)
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
    # Create a placeholder plot showing the missing plot name
    placeholder_plot <- ggplot() +
      annotate(
        "text",
        x = 0.5,
        y = 0.6,
        label = paste0("Plot Missing:\n", fname),
        hjust = 0.5,
        vjust = 0.5,
        size = 5,
        fontface = "bold",
        color = "#666666"
      ) +
      annotate(
        "text",
        x = 0.5,
        y = 0.35,
        label = "(Insufficient data or missing required fields)",
        hjust = 0.5,
        vjust = 0.5,
        size = 3.5,
        color = "#999999"
      ) +
      xlim(0, 1) +
      ylim(0, 1) +
      theme_void() +
      theme(
        panel.border = element_rect(color = "#CCCCCC", fill = NA, linewidth = 1),
        plot.background = element_rect(fill = "#F8F8F8", color = NA)
      )
    
    # Mark this as a placeholder plot so it can be skipped during download
    class(placeholder_plot) <- c("placeholder_plot", class(placeholder_plot))
    
    plotList[[length(plotList) + 1]] <- placeholder_plot
    fileNames[[length(fileNames) + 1]] <- fname
    heights[[length(heights) + 1]] <- 3  # Smaller height for placeholder
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

# Helper: return a PNG image response for renderImage using ragg
render_png_response <- function(plot, width, height, units = "in", dpi = 144, limitsize = FALSE) {
  outfile <- tempfile(fileext = ".png")
  ggplot2::ggsave(
    filename  = outfile,
    plot      = plot,
    device    = ragg::agg_png,
    width     = width,
    height    = height,
    units     = units,
    dpi       = dpi,
    limitsize = limitsize
  )
  list(src = outfile, contenttype = "image/png")
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
    contenttype = 'svg',
    alt = paste0("Error in ", plot_id, ": ", e$message)
  ))
}
