# Consistent way of saving plot using rsvg_png
savePlot <- function(plot, filename, fileType, width = 6, height = NULL, theme = NULL) {
  if (fileType == "png") {
    ggsave('tmp.svg', plot = plot + theme, width = width, unit = 'in', device = svglite)
    rsvg::rsvg_png("tmp.svg", filename, height = 1200, width = 1200)
  } else {
    ggsave(
      filename = filename,
      plot = plot + theme,
      width = width,
      unit = 'in',
      device = ifelse(fileType == "svg", svglite::svglite, fileType)
    )
  }
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
    summarise(n = n()) %>%
    mutate(text = paste0(conditionName, ': N=', n))
  return(paste0(unique(t$text), collapse='\n'))
}