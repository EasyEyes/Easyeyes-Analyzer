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

