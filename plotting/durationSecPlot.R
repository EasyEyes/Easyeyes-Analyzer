get_duration_data <- function(data_list) {
  df <- foreach(i=1:length(data_list), .combine = 'rbind') %do% {
    data_list[[i]] %>%
      select(participant, 
             font, 
             conditionName,
             targetMeasuredDurationSec, 
             targetMeasuredLatenessSec, 
             fontNominalSizePx,
             thresholdAllowedDurationRatio, 
             thresholdAllowedLatenessSec,
             fontMaxPx,
             targetDurationSec, 
             deviceSystemFamily) %>% 
      mutate(fontNominalSizePx = as.numeric(fontNominalSizePx),
             targetMeasuredDurationSec = as.numeric(targetMeasuredDurationSec)) %>%
      distinct() %>% 
      filter(!is.na(fontNominalSizePx))
  }
  return(df)
}

get_duration_corr <- function(data_list) {
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             targetMeasuredDurationSec,
             targetMeasuredLatenessSec,
             trialGivenToQuestErrorCheckLabels,
             trialGivenToQuestChecks,
             `heapUsedBeforeDrawing (MB)`,
             `heapTotalBeforeDrawing (MB)`,
             `heapLimitBeforeDrawing (MB)`,
             `heapUsedAfterDrawing (MB)`,
             `heapTotalAfterDrawing (MB)`,
             `heapLimitAfterDrawing (MB)`,
             `heapTotalPostLateness (MB)`,
             `heapTotalPreLateness (MB)`,
             computeRandomMHz,
             cores,
             fontNominalSizePx,
             trialGivenToQuest) %>% 
      rename(hardwareConcurrency = cores) %>% 
      filter(trialGivenToQuestChecks != '', !is.na(trialGivenToQuestChecks)) %>% 
      separate_rows(trialGivenToQuestErrorCheckLabels, trialGivenToQuestChecks, sep = ",") %>%
      mutate(trialGivenToQuestChecks = as.logical(trialGivenToQuestChecks)) %>% 
      mutate(deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
             deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
             deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`))
    
    t <- t %>%
      pivot_wider(names_from = trialGivenToQuestErrorCheckLabels,
                  values_from = trialGivenToQuestChecks)
  }
  params <- params %>%
    select(-c(block, 
              participant, 
              trialGivenToQuest,
              `heapTotalPostLateness (MB)`,
              `heapTotalPreLateness (MB)`))
  params <- params[complete.cases(params),]
  c <- colnames(params)
  t <- data.frame(cor(params))
  colnames(t) <- c
  t <- t %>% mutate(across(everything(), round, 3))
  corplot <- ggcorrplot(t,
                        show.legend = FALSE,
                        show.diag = T,
                        type = "lower",
                        colors= c('white'),
                        lab = T) + 
    theme_bw() +
    labs(x = '', 
         y = '',
         title = 'Correlation table targetMeasuredDurationSec') +
    plt_theme + 
    theme(legend.position = 'none',
          plot.title.position = "plot",
          axis.text.x = element_text(size = 14,
                                     angle = 30,
                                     vjust = 1,
                                     hjust=1),
          plot.title = element_text(size=18,
                                    hjust = 1),
          plot.subtitle = element_text(size=18,
                                       hjust = 1))
  
  return(list(
    plot = corplot,
    width = 10,
    height = 10
  ))
}

plot_duraction_sec <- function(df) {
  print('inside plot_duraction_sec')
  if (nrow(df) == 0) {
    return(list(font = NULL, participant = NULL))
  }
  bounds <- df %>% 
    distinct(thresholdAllowedDurationRatio, targetDurationSec, fontMaxPx) %>% 
    mutate(upper = max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio) * targetDurationSec,
           lower = targetDurationSec / max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio))
  df <- df %>% 
    filter(!is.na(font)) %>% 
    filter(font != '', participant != '') %>%
    mutate(intervention = ifelse(grepl('Post', conditionName), 'Post intervention', 'Before intervention'))
  p1 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx, 
                   y = targetMeasuredDurationSec,
                   color = font))
  for (i in 1:nrow(bounds)) {
    p1 <- p1 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p1 <- p1 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    guides(color=guide_legend(ncol=3, title = '')) + 
    theme_bw() +
    plt_theme_scatter +
    facet_wrap(~intervention) +
    annotation_logticks() + 
    labs(title = 'targetMeasuredDurationSec vs fontNominalSizePx \ncolored by font',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  p2 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx, 
                   y = targetMeasuredDurationSec,
                   color = participant))
  for (i in 1:nrow(bounds)) {
    p2 <- p2 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p2 <- p2 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    guides(color=guide_legend(ncol=2, title = '')) + 
    annotation_logticks() + 
    facet_wrap(~intervention) +
    theme_bw()+
    plt_theme_scatter+
    labs(title = 'targetMeasuredDurationSec vs fontNominalSizePx \n coloredby participant',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  return(list(font = p1,
              participant = p2))
}

plot_Lateness_sec <- function(df) {
  print('inside plot_Lateness_sec')
  if (nrow(df) == 0) {
    return(list(font = NULL, participant = NULL))
  }
  bounds <- df %>% 
    distinct(thresholdAllowedLatenessSec, fontMaxPx)
  
  df <- df %>% 
    filter(!is.na(font)) %>% 
    filter(font != '', participant != '') %>%
    mutate(intervention = ifelse(grepl('Post', conditionName), 'Post intervention', 'Before intervention'))
  p1 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx, 
                   y = targetMeasuredLatenessSec,
                   color = font))
  
  for (i in 1:nrow(bounds)) {
    p1 <- p1 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p1 <- p1 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    facet_wrap(~intervention) + 
    guides(color=guide_legend(ncol=3, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredLatenessSec vs fontNominalSizePx \ncolored by font',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx') + 
    theme_bw()+
    plt_theme_scatter
  
  p2 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx, 
                   y = targetMeasuredLatenessSec,
                   color = participant))
  for (i in 1:nrow(bounds)) {
    p2 <- p2 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p2 <- p2 +
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    facet_wrap(~intervention) + 
    guides(color=guide_legend(ncol=2, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredLatenessSec vs fontNominalSizePx \ncolored by participant',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx') +
    theme_bw()+
    plt_theme_scatter
  return(list(font = p1,
              participant = p2))
}

get_histogram_duration_lateness <- function(duration){
  if (nrow(duration) == 0) {
    return(NULL)
  }
  p1 <- ggplot(data = duration) +  
    geom_histogram(aes(x = targetMeasuredDurationSec),color="black", fill="black") + 
    theme_bw()+
    plt_theme+
    facet_wrap(~deviceSystemFamily)
  
  p2 <- ggplot(data = duration) +  
    geom_histogram(aes(x = targetMeasuredLatenessSec),color="black", fill="black") + 
    theme_bw()+
    plt_theme+
    facet_wrap(~deviceSystemFamily)
  return(list(
    duration = p1,
    lateness = p2
  ))
}

append_hist_list <- function(data_list, plot_list, fileNames){
  print('inside get_dur_param_hist')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             targetMeasuredDurationSec,
             targetMeasuredLatenessSec,
             `heapUsedBeforeDrawing (MB)`,
             `heapTotalBeforeDrawing (MB)`,
             `heapLimitBeforeDrawing (MB)`,
             `heapUsedAfterDrawing (MB)`,
             `heapTotalAfterDrawing (MB)`,
             `heapLimitAfterDrawing (MB)`,
             `heapTotalPostLateness (MB)`,
             `heapTotalPreLateness (MB)`,
             computeRandomMHz,
             cores,
             fontNominalSizePx,
             screenWidthPx,
             trialGivenToQuest) %>% 
      rename(hardwareConcurrency = cores) %>% 
      mutate(deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
             deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
             deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`))
  }
  
  webGL <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    if ('WebGL_Report' %in% names(data_list[[i]])) {
      t <- fromJSON(data_list[[i]]$WebGL_Report[1])
      df <- data.frame(WebGL_Version = t$WebGL_Version,
                       Max_Texture_Size = t$Max_Texture_Size,
                       Max_Viewport_Dims = max(unlist(t$Max_Viewport_Dims)))
      df$participant = data_list[[i]]$participant[1]
    }
    df
  }
  webGL$WebGL_Version = as.factor(webGL$WebGL_Version)
  summary <- params %>%
    group_by(participant,computeRandomMHz, screenWidthPx, hardwareConcurrency) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T))
  
  blockAvg <- params %>% 
    group_by(participant, block) %>% 
    summarize(heapTotalAfterDrawingAvg = mean( `heapTotalAfterDrawing (MB)`, na.rm =T),
              heapUsedAfterDrawingAvg = mean( `heapUsedAfterDrawing (MB)`, na.rm =T))
  
  params_vars <- c("heapUsedAfterDrawing (MB)", "heapTotalAfterDrawing (MB)", 
                   "heapLimitAfterDrawing (MB)", "deltaHeapTotalMB", "deltaHeapUsedMB",
                   "deltaHeapLatenessMB", "computeRandomMHz", "screenWidthPx",  "hardwareConcurrency")
  
  webGL_vars <- c("Max_Texture_Size", "Max_Viewport_Dims")
  
  summary_vars <- c("goodTrials", "badTrials")
  
  blockAvg_vars <- c("heapTotalAfterDrawingAvg", "heapUsedAfterDrawingAvg")
  
  datasets <- list(params = params, webGL = webGL, summary = summary, blockAvg = blockAvg)
  
  j = length(plot_list) + 1
  # Loop through params dataset and generate histograms
  for (var in params_vars) {
    plot_list[[j]] <- ggplot(params, aes(x = .data[[var]])) +
      geom_histogram(color="black", fill="black") + 
      theme_bw() +
      labs(title = paste("Histogram of", var))
    fileNames[[j]] <- paste0(var,'-histogram')
    j = j + 1
  }
  
  plot_list[[j]] <- ggplot(webGL, aes(x = WebGL_Version)) +
    geom_bar(color="black", fill="black") + 
    theme_bw() +
    labs(title = paste("Histogram of", 'WebGL_Version')) +
    theme(axis.text.x = element_text(size = 10,
                                     angle = 10,
                                     vjust = 0.5,
                                     hjust= 0.5))
  fileNames[[j]] <- paste0('WebGL_Version','-histogram')
  j = j + 1
  
  for (var in webGL_vars) {
    plot_list[[j]] <- ggplot(webGL, aes(x = .data[[var]])) +
      geom_histogram(color="black", fill="black") + 
      theme_bw() +
      labs(title = paste("Histogram of", var))
    fileNames[[j]] <- paste0(var,'-histogram')
    j = j + 1
  }
  
  # Loop through summary dataset and generate histograms
  for (var in summary_vars) {
    plot_list[[j]] <- ggplot(summary, aes(x = .data[[var]])) +
      geom_histogram(color="black", fill="black") + 
      theme_bw() +
      labs(title = paste("Histogram of", var))
    fileNames[[j]] <- paste0(var,'-histogram')
    j = j + 1
  }
  
  # Loop through blockAvg dataset and generate histograms
  for (var in blockAvg_vars) {
    plot_list[[j]] <- ggplot(blockAvg, aes(x = .data[[var]])) +
      geom_histogram(color="black", fill="black") + 
      theme_bw() +
      labs(title = paste("Histogram of", var))
    fileNames[[j]] <- paste0(var,'-histogram')
    j = j + 1
  }
  return(list(plotList = plot_list,
              fileNames = fileNames))
}

