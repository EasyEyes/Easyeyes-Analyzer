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
      ggtitle(plotTitle)
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

append_plot_list <- function(plotList, fileNames, plot, fname) {
  if (!is.null(plot)) {
    plotList[[length(plotList) + 1]] <- plot
    fileNames[[length(fileNames) + 1]] <- fname
  }
  return(list(plotList = plotList, fileNames = fileNames))
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
      t <- fromJSON(data_list[[i]]$WebGL_Report[1])
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
          WebGLVersion = t$WebGL_Version,
          maxTextureSize = t$Max_Texture_Size,
          maxViewportSize = max(unlist(t$Max_Viewport_Dims)),
          WebGLUnmaskedRenderer = t$Unmasked_Renderer)
      }
      webGL = rbind(webGL, df)
    }
  }
  if (nrow(webGL) == 0) {
    webGL = tibble(
      participant = '',
      WebGLVersion = NA,
      maxTextureSize = NA,
      maxViewportSize = NA,
      WebGLUnmaskedRenderer = NA)
  } else {
    webGL = tibble(
      participant = webGL$participant,
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
