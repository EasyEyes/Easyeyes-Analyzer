source('./other/utility.R')
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
             fontPadding,
             targetDurationSec, 
             deviceSystemFamily) %>% 
      mutate(fontNominalSizePx = as.numeric(fontNominalSizePx)) %>%
      distinct() %>% 
      filter(!is.na(fontNominalSizePx))
  }
  if (is.character(df$targetMeasuredDurationSec)) {
    df <- df %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    df$targetMeasuredDurationSec <- as.numeric(df$targetMeasuredDurationSec)
  }
  return(df)
}

get_duration_corr <- function(data_list) {
  print('inside get_duration_corr')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             targetMeasuredDurationSec,
             targetMeasuredLatenessSec,
             trialGivenToQuestErrorCheckLabels,
             trialGivenToQuestChecks,
             fontRenderSec,
             heap100MBAllocSec,
             targetMeasuredPreRenderSec,
             # `heapUsedBeforeDrawing (MB)`,
             # `heapTotalBeforeDrawing (MB)`,
             # `heapLimitBeforeDrawing (MB)`,
             # `heapUsedAfterDrawing (MB)`,
             # `heapTotalAfterDrawing (MB)`,
             # `heapLimitAfterDrawing (MB)`,
             # `heapTotalPostLateness (MB)`,
             # `heapTotalPreLateness (MB)`,
             computeRandomMHz,
             deviceMemoryGB,
             cores,
             fontNominalSizePx,
             trialGivenToQuest) %>% 
      rename(hardwareConcurrency = cores) %>% 
      filter(trialGivenToQuestChecks != '', !is.na(trialGivenToQuestChecks))
    # mutate(deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
    #        deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
    #        deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`))
    # 
    # t <- t %>%
    #   pivot_wider(names_from = trialGivenToQuestErrorCheckLabels,
    #               values_from = trialGivenToQuestChecks)
  }
  webGL <- get_webGL(data_list)
  
  
  summary <- params %>%
    group_by(participant, block) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T))
  
  params <- params %>% 
    select(-trialGivenToQuest) %>% 
    mutate(order = row_number())
  
  trialGivenToQuest <- params %>%
    select(order,trialGivenToQuestErrorCheckLabels,trialGivenToQuestChecks) %>% 
    separate_rows(trialGivenToQuestErrorCheckLabels, trialGivenToQuestChecks, sep = ',') %>% 
    mutate(trialGivenToQuestChecks = as.logical(trialGivenToQuestChecks)) %>% 
    pivot_wider(names_from = trialGivenToQuestErrorCheckLabels,
                values_from = trialGivenToQuestChecks)
  params <- params %>% select(-c(trialGivenToQuestErrorCheckLabels, trialGivenToQuestChecks)) %>% 
    left_join(trialGivenToQuest, by = 'order') %>% 
    select(-order) %>% 
    left_join(webGL, by = "participant") %>% 
    left_join(summary, by = c("participant", "block")) %>% 
    select(-participant, block)
  
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  params <- params %>% select_if(is.numeric) %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  params <- params[complete.cases(params),]

  c <- colnames(params)
  t <- data.frame(cor(params))
  
  colnames(t) <- c
  t <- t %>% mutate(across(everything(), round, 3))
  print(t)
  corplot <- ggcorrplot(t,
                        show.legend = FALSE,
                        show.diag = T,
                        type = "lower",
                        colors= c('white'),
                        lab = T) + 
    theme_bw() +
    labs(x = '', 
         y = '',
         title = 'Timing Correlation Table') +
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
    width = 3 + ncol(t) * 0.4,
    height = 3 + ncol(t) * 0.4
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
    labs(title = 'targetMeasuredDurationSec vs fontNominalSizePx \ncolored by participant',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  p3 <- ggplot() +
    geom_point(data=df, 
               aes(x= fontNominalSizePx*(1+fontPadding),
                   y= targetMeasuredDurationSec, 
                   color = font))
  
  for (i in 1:nrow(bounds)) {
    p3 <- p3 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p3 <- p3 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    guides(color=guide_legend(ncol=2, title = '')) + 
    annotation_logticks() + 
    facet_wrap(~intervention) +
    theme_bw()+
    plt_theme_scatter+
    labs(title = 'targetMeasuredDurationSec vs.\nfontNominalSizePx*(1+fontPadding) colored by font',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  return(list(font = p1,
              participant = p2,
              fontPadding = p3))
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
  
  p3 <- ggplot() +
    geom_point(data=df, aes(x=fontNominalSizePx*(1+fontPadding),
                                y=targetMeasuredLatenessSec, 
                                color = font))
  for (i in 1:nrow(bounds)) {
    p3 <- p3 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p3 <- p3 +
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    facet_wrap(~intervention) + 
    guides(color=guide_legend(ncol=2, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredLatenessSec vs.\nfontNominalSizePx*(1+fontPadding) \ncolored by fontPadding',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx') +
    theme_bw()+
    plt_theme_scatter
  
  return(list(font = p1,
              participant = p2,
              fontPadding = p3))
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
  print("Data list done")
  
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
             deviceMemoryGB,
             cores,
             fontNominalSizePx,
             fontPadding,
             heap100MBAllocSec,
             fontRenderSec,
             targetMeasuredPreRenderSec,
             screenWidthPx,
             screenHeightPx,
             pxPerCm,
             trialGivenToQuest) %>% 
      rename(hardwareConcurrency = cores) %>% 
      mutate(
        deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
        deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
        deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`),
        screenWidthCm = ifelse(is.na(pxPerCm) | pxPerCm <= 0, NA, screenWidthPx / pxPerCm),
        # screenWidthCm = screenWidthPx / pxPerCm,
        #s creenHeightCm = screenHeightPx / pxPerCm,
      )
   
  }
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  
  webGL <- get_webGL(data_list)
  summary <- params %>%
    group_by(participant,computeRandomMHz, screenWidthPx, screenWidthCm, hardwareConcurrency, deviceMemoryGB) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T))
  
  blockAvg <- params %>% 
    group_by(participant, block) %>% 
    summarize(heapTotalAfterDrawingAvg = mean( `heapTotalAfterDrawing (MB)`, na.rm =T),
              heapUsedAfterDrawingAvg = mean( `heapUsedAfterDrawing (MB)`, na.rm =T))
  

  params_vars <- c( "heapLimitAfterDrawing (MB)", "deltaHeapTotalMB", "fontPadding", "heap100MBAllocSec",
                   "fontRenderSec", "targetMeasuredPreRenderSec")

  
  webGL_vars <- c("maxTextureSize", "maxViewportSize")
  
  summary_vars <- c("screenWidthPx", "screenWidthCm", "deviceMemoryGB")
  
  #blockAvg_vars <- c("heapTotalAfterDrawingAvg", "heapUsedAfterDrawingAvg")
  
  j = length(plot_list) + 1
  # Loop through params dataset and generate histograms
  #for (var in params_vars) {
    #if (n_distinct(params[var]) > 1) {
     # plot_list[[j]] <- ggplot(params, aes(x = .data[[var]])) +
      #  geom_histogram(color="black", fill="black") + 
       # theme_bw() +
      #  labs(title = paste("Histogram of", var))
      #fileNames[[j]] <- paste0(var,'-histogram')
      #j = j + 1
    #}
  #}
  if (n_distinct(webGL['WebGLVersion']) > 1) {
    plot_list[[j]] <- ggplot(webGL, aes(x = WebGLVersion)) +
      geom_bar(color="black", fill="black") + 
      theme_bw() +
      labs(title = paste("Histogram of", 'WebGLVersion')) +
      theme(axis.text.x = element_text(size = 10,
                                       angle = 10,
                                       vjust = 0.5,
                                       hjust= 0.5))
    fileNames[[j]] <- paste0('WebGLVersion','-histogram')
    j = j + 1
  }
  
  
  for (var in webGL_vars) {
    if (n_distinct(webGL[var]) > 1) {
      plot_list[[j]] <- ggplot(webGL, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="black") + 
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  # Loop through summary dataset and generate histograms
  for (var in summary_vars) {
    if (n_distinct(summary[var]) > 1) {
      plot_list[[j]] <- ggplot(summary, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="black") + 
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  # Loop through blockAvg dataset and generate histograms
  #for (var in blockAvg_vars) {
   # if (n_distinct(blockAvg[var]) > 1) {
     # plot_list[[j]] <- ggplot(blockAvg, aes(x = .data[[var]])) +
     #   geom_histogram(color="black", fill="black") + 
      #  theme_bw() +
       # labs(title = paste("Histogram of", var))
      #fileNames[[j]] <- paste0(var,'-histogram')
      #j = j + 1
    #}
  #}
  return(list(plotList = plot_list,
              fileNames = fileNames))
}

append_hist_time <- function(data_list, plot_list, fileNames){
  print('inside get_dur_param_hist')
  print("Data list done")
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
             deviceMemoryGB,
             cores,
             fontNominalSizePx,
             screenWidthPx,
             screenHeightPx,
             trialGivenToQuest) %>% 
      rename(hardwareConcurrency = cores) %>% 
      mutate(
        deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
        deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
        deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`),
        # screenWidthCm = screenWidthPx / pxPerCm,
        #s creenHeightCm = screenHeightPx / pxPerCm,
      )
  }
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  webGL <- get_webGL(data_list)
  summary <- params %>%
    group_by(participant,computeRandomMHz, screenWidthPx, hardwareConcurrency, deviceMemoryGB) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T))
  
  blockAvg <- params %>% 
    group_by(participant, block) %>% 
    summarize(heapTotalAfterDrawingAvg = mean( `heapTotalAfterDrawing (MB)`, na.rm =T),
              heapUsedAfterDrawingAvg = mean( `heapUsedAfterDrawing (MB)`, na.rm =T))
  
  params_vars <- c("heapUsedAfterDrawing (MB)", 
                   "heapTotalAfterDrawing (MB)", 
                   "heapLimitAfterDrawing (MB)", 
                   "deltaHeapTotalMB", 
                   "deltaHeapUsedMB")
  
  #webGL_vars <- c("maxTextureSize", "maxViewportSize")
  
  summary_vars <- c("goodTrials", "badTrials","computeRandomMHz",   "hardwareConcurrency", "deviceMemoryGB")
  
  blockAvg_vars <- c("heapTotalAfterDrawingAvg", "heapUsedAfterDrawingAvg")
  
  j = length(plot_list) + 1
  # Loop through params dataset and generate histograms
  for (var in params_vars) {
    if (n_distinct(params[var]) > 1) {
      plot_list[[j]] <- ggplot(params, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="black") + 
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  #if (n_distinct(webGL['WebGLVersion']) > 1) {
   # plot_list[[j]] <- ggplot(webGL, aes(x = WebGLVersion)) +
    #  geom_bar(color="black", fill="black") + 
     # theme_bw() +
      #labs(title = paste("Histogram of", 'WebGLVersion')) +
      #theme(axis.text.x = element_text(size = 10,
        #                               angle = 10,
       #                                vjust = 0.5,
        #                               hjust= 0.5))
    #fileNames[[j]] <- paste0('WebGLVersion','-histogram')
    #j = j + 1
  #}
  
  
  #for (var in webGL_vars) {
    #if (n_distinct(webGL[var]) > 1) {
     # plot_list[[j]] <- ggplot(webGL, aes(x = .data[[var]])) +
      #  geom_histogram(color="black", fill="black") + 
       # theme_bw() +
        #labs(title = paste("Histogram of", var))
      #fileNames[[j]] <- paste0(var,'-histogram')
      #j = j + 1
    #}
  #}
  
  # Loop through summary dataset and generate histograms
  for (var in summary_vars) {
    if (n_distinct(summary[var]) > 1) {
      plot_list[[j]] <- ggplot(summary, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="black") + 
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  # Loop through blockAvg dataset and generate histograms
  for (var in blockAvg_vars) {
    if (n_distinct(blockAvg[var]) > 1) {
      plot_list[[j]] <- ggplot(blockAvg, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="black") + 
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  return(list(plotList = plot_list,
              fileNames = fileNames))
}

append_scatter_list <- function(data_list, plot_list, fileNames) {
  print('inside append_scatter_list')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             targetMeasuredDurationSec,
             targetMeasuredLatenessSec,
             targetDurationSec,
             mustTrackSec,
             thresholdAllowedDurationRatio,
             thresholdAllowedLatenessSec,
             `heapUsedBeforeDrawing (MB)`,
             `heapTotalBeforeDrawing (MB)`,
             `heapLimitBeforeDrawing (MB)`,
             `heapUsedAfterDrawing (MB)`,
             `heapTotalAfterDrawing (MB)`,
             `heapLimitAfterDrawing (MB)`,
             `heapTotalPostLateness (MB)`,
             `heapTotalPreLateness (MB)`,
             computeRandomMHz,
             deviceMemoryGB,
             cores,
             font,
             fontNominalSizePx,
             screenWidthPx,
             fontPadding,
             trialGivenToQuest,
             longTaskDurationSec,
             deviceSystemFamily) %>% 
      rename(hardwareConcurrency = cores) %>% 
      mutate(deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
             deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
             deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`))
  }
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  
  webGL <- get_webGL(data_list)
  params <- params %>%
    filter(!is.na(font)) %>% 
    left_join(webGL, by = 'participant') %>% 
    arrange(hardwareConcurrency) %>% 
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  blockAvg <- params %>% 
    group_by(participant, block, hardwareConcurrency, deviceSystemFamily, font) %>% 
    mutate(upper = max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio) * targetDurationSec,
           lower = targetDurationSec / max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio)) %>% 
    summarize(heapTotalAfterDrawingAvg = mean(`heapTotalAfterDrawing (MB)`, na.rm =T),
           heapUsedAfterDrawingAvg = mean(`heapUsedAfterDrawing (MB)`, na.rm =T),
           badLatenessTrials = sum(targetMeasuredLatenessSec > thresholdAllowedLatenessSec),
           badDurationTrials = sum(targetMeasuredDurationSec > upper | targetMeasuredDurationSec < lower)) %>% 
    arrange(hardwareConcurrency) %>% 
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  j = length(plot_list) + 1
  
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=fontNominalSizePx,
                                              y=targetMeasuredDurationSec, 
                                              color = as.factor(fontPadding))) +
      geom_jitter() + 
      guides(color=guide_legend(ncol=2, title='')) + 
      labs(title = 'targetMeasuredDurationSec vs. fontNominalSizePx \ncolored by fontPadding',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-fontNominalSizePx-by-fontPadding'
    j = j + 1
  }

  if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$mustTrackSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=mustTrackSec,y=targetMeasuredLatenessSec, color = deviceSystemFamily)) +
      geom_jitter() + 
      scale_x_continuous(limits = c(min(params$mustTrackSec) - 2,max(params$mustTrackSec) + 2)) + 
      guides(color=guide_legend(ncol=2, title = 'OS')) + 
      labs(title = 'targetMeasuredLatenessSec vs mustTrackSec \ncolored by OS',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-mustTrackSec-by-OS'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=hardwareConcurrency, y=targetMeasuredLatenessSec, color = font)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = ' targetMeasuredLatenessSec vs hardwareConcurrency \ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=hardwareConcurrency, y=targetMeasuredDurationSec, color = font)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredDurationSec vs hardwareConcurrency \ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=maxTextureSize, y=targetMeasuredLatenessSec, color = participant)) +
      geom_jitter() +
      scale_x_continuous(limits = c(min(params$maxTextureSize)-5, max(params$maxTextureSize) + 5)) + 
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredLatenessSec vs maxTextureSize \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-maxTextureSize-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=maxViewportSize, y=targetMeasuredLatenessSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      scale_x_continuous(limits = c(min(params$maxViewportSize)-5, max(params$maxViewportSize)+5)) + 
      labs(title = 'targetMeasuredLatenessSec vs maxViewportSize \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-maxViewportSize-by-participant'
    j = j + 1
  }
 
  if (n_distinct(blockAvg$badLatenessTrials) > 1) {
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency, y=badLatenessTrials, color = font)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs hardwareConcurrency \ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$badDurationTrials) > 1) {
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency, y=badDurationTrials, color = font)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badDurationTrials vs hardwareConcurrency \ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badDurationTrials-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(params$deltaHeapLatenessMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=deltaHeapLatenessMB,y=targetMeasuredLatenessSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredLatenessSec vs. deltaHeapLatenessMB \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-deltaHeapLatenessMB-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$deltaHeapLatenessMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=deltaHeapLatenessMB,y=targetMeasuredDurationSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredDurationSec vs. deltaHeapLatenessMB \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-deltaHeapLatenessMB-by-participant'
    j = j + 1
  }
  
  # if (n_distinct(params$deltaHeapTotalMB) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=deltaHeapTotalMB,y=targetMeasuredLatenessSec, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'targetMeasuredLatenessSec vs. deltaHeapTotalMB \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-deltaHeapTotalMB-by-participant'
  #   j = j + 1
  # }
  # if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredLatenessSec) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=targetMeasuredLatenessSec, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'targetMeasuredLatenessSec vs. longTaskDurationSec \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-longTaskDurationSec-by-participant'
  #   j = j + 1
  # }
  # if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=targetMeasuredDurationSec, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'targetMeasuredDurationSec vs. longTaskDurationSec \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'longTaskDurationSec-vs-longTaskDurationSec-by-participant'
  #   j = j + 1
  # }
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=fontNominalSizePx,y=deltaHeapUsedMB, color = font)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      scale_x_log10() +
      scale_y_log10() +
      coord_fixed() +
      labs(title = 'deltaHeapUsedMB vs. fontNominalSizePx \ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-fontNominalSizePx-by-font'
    j = j + 1
  }
  
  # if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$longTaskDurationSec) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=fontNominalSizePx,y=longTaskDurationSec, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'longTaskDurationSec vs. fontNominalSizePx \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'longTaskDurationSec-vs-fontNominalSizePx-by-participant'
  #   j = j + 1
  # }
  
  # if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=deltaHeapUsedMB, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'deltaHeapUsedMB vs. longTaskDurationSec \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'deltaHeapUsedMB-vs-longTaskDurationSec-by-participant'
  #   j = j + 1
  # }
  
  if (n_distinct(params$`heapLimitAfterDrawing (MB)`) > 1 & n_distinct(params$deltaHeapTotalMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=`heapLimitAfterDrawing (MB)`,y=deltaHeapTotalMB, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'deltaHeapTotalMB vs. heapLimitAfterDrawing \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapTotalMB-vs-heapLimitAfterDrawing-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$`heapUsedBeforeDrawing (MB)`) > 1 & n_distinct(params$deltaHeapTotalMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=`heapUsedBeforeDrawing (MB)`,y=deltaHeapTotalMB, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'deltaHeapTotalMB vs. heapUsedBeforeDrawing \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapTotalMB-vs-heapUsedBeforeDrawing-by-participant'
    j = j + 1
  }
  
  # if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=targetMeasuredLatenessSec,y=targetMeasuredDurationSec, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'targetMeasuredDurationSec vs. targetMeasuredLatenessSec \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'targetMeasuredDurationSec-vs-targetMeasuredLatenessSec-by-participant'
  #   j = j + 1
  # }
  
  if (n_distinct(params$deltaHeapTotalMB) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=deltaHeapTotalMB,y=deltaHeapUsedMB, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'deltaHeapUsedMB vs. deltaHeapTotalMB \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-deltaHeapTotalMB-by-participant'
    j = j + 1
  }
  
  # if (n_distinct(blockAvg$badLatenessTrials) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
  #   plot_list[[j]] <- ggplot(data=blockAvg, aes(x=badLatenessTrials,y=badDurationTrials, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'badDurationTrials vs. badLatenessTrials \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'badDurationTrials-vs-badLatenessTrials-by-participant'
  #   j = j + 1
  # }
  
  # if (n_distinct(params$`heapLimitAfterDrawing (MB)`) > 1 & n_distinct(params$badLatenessTrials) > 1) {
  #   plot_list[[j]] <- ggplot(data=params, aes(x=`heapLimitAfterDrawing (MB)`,y=badLatenessTrials, color = participant)) +
  #     geom_jitter() +
  #     guides(color=guide_legend(ncol=2, title = '')) + 
  #     labs(title = 'badLatenessTrials vs. heapLimitAfterDrawing \ncolored by participant',
  #          caption = 'Points jittered to avoid occlusion.')
  #   fileNames[[j]] <- 'badLatenessTrials-vs-heapLimitAfterDrawing-by-participant'
  #   j = j + 1
  # }
  
  if (n_distinct(blockAvg$heapUsedAfterDrawingAvg) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=heapUsedAfterDrawingAvg,y=badLatenessTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. heapUsedAfterDrawingAvg \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-heapUsedAfterDrawingAvg-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$heapTotalAfterDrawingAvg) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=heapTotalAfterDrawingAvg,y=badLatenessTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. heapTotalAfterDrawingAvg \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-heapTotalAfterDrawingAvg-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$deviceMemoryGB) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=deviceMemoryGB,y=badLatenessTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. deviceMemoryGB \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-deviceMemoryGB-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency,y=badLatenessTrials, color = participant)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. hardwareConcurrency \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-hardwareConcurrency-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
    
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency,y=badDurationTrials, color = participant)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badDurationTrials vs. hardwareConcurrency \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badDurationTrials-vs-hardwareConcurrency-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency,y=badLatenessTrials, color = deviceSystemFamily)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. hardwareConcurrency \ncolored by deviceSystemFamily',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-hardwareConcurrency-by-deviceSystemFamily'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
    
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=hardwareConcurrency,y=badDurationTrials, color = deviceSystemFamily)) +
      geom_jitter(position=position_jitter(width=0.1, height=0.1)) +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badDurationTrials vs. hardwareConcurrency \ncolored by deviceSystemFamily',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badDurationTrials-vs-hardwareConcurrency-by-deviceSystemFamily'
    j = j + 1
  }
  
  return(list(
    plotList = plot_list,
    fileNames=fileNames
  ))
}
append_scatter_time <- function(data_list, plot_list, fileNames) {
  print('inside append_scatter_time')
  
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             targetMeasuredDurationSec,
             targetMeasuredLatenessSec,
             targetDurationSec,
             mustTrackSec,
             thresholdAllowedDurationRatio,
             thresholdAllowedLatenessSec,
             `heapUsedBeforeDrawing (MB)`,
             `heapTotalBeforeDrawing (MB)`,
             `heapLimitBeforeDrawing (MB)`,
             `heapUsedAfterDrawing (MB)`,
             `heapTotalAfterDrawing (MB)`,
             `heapLimitAfterDrawing (MB)`,
             `heapTotalPostLateness (MB)`,
             `heapTotalPreLateness (MB)`,
             computeRandomMHz,
             deviceMemoryGB,
             cores,
             font,
             fontNominalSizePx,
             screenWidthPx,
             trialGivenToQuest,
             longTaskDurationSec,
             deviceSystemFamily) %>% 
      rename(hardwareConcurrency = cores) %>% 
      mutate(deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
             deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
             deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`))
  }
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  webGL <- get_webGL(data_list)
  params <- params %>%
    filter(!is.na(font)) %>% 
    left_join(webGL, by = 'participant') %>% 
    arrange(hardwareConcurrency) %>% 
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  blockAvg <- params %>% 
    group_by(participant, block, hardwareConcurrency, deviceSystemFamily, font) %>% 
    mutate(upper = max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio) * targetDurationSec,
           lower = targetDurationSec / max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio)) %>% 
    summarize(heapTotalAfterDrawingAvg = mean(`heapTotalAfterDrawing (MB)`, na.rm =T),
              heapUsedAfterDrawingAvg = mean(`heapUsedAfterDrawing (MB)`, na.rm =T),
              badLatenessTrials = sum(targetMeasuredLatenessSec > thresholdAllowedLatenessSec),
              badDurationTrials = sum(targetMeasuredDurationSec > upper | targetMeasuredDurationSec < lower)) %>% 
    arrange(hardwareConcurrency) %>% 
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  j = length(plot_list) + 1
  
  if (n_distinct(params$deltaHeapTotalMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=deltaHeapTotalMB,y=targetMeasuredLatenessSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredLatenessSec vs. deltaHeapTotalMB \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-deltaHeapTotalMB-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredLatenessSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=targetMeasuredLatenessSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredLatenessSec vs. longTaskDurationSec \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=targetMeasuredDurationSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredDurationSec vs. longTaskDurationSec \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'longTaskDurationSec-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$longTaskDurationSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=fontNominalSizePx,y=longTaskDurationSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'longTaskDurationSec vs. fontNominalSizePx \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'longTaskDurationSec-vs-fontNominalSizePx-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=longTaskDurationSec,y=deltaHeapUsedMB, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'deltaHeapUsedMB vs. longTaskDurationSec \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=targetMeasuredLatenessSec,y=targetMeasuredDurationSec, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'targetMeasuredDurationSec vs. targetMeasuredLatenessSec \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-targetMeasuredLatenessSec-by-participant'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$badLatenessTrials) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=badLatenessTrials,y=badDurationTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badDurationTrials vs. badLatenessTrials \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badDurationTrials-vs-badLatenessTrials-by-participant'
    j = j + 1
  }
  
  if (n_distinct(params$`heapLimitAfterDrawing (MB)`) > 1 & n_distinct(params$badLatenessTrials) > 1) {
    plot_list[[j]] <- ggplot(data=params, aes(x=`heapLimitAfterDrawing (MB)`,y=badLatenessTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. heapLimitAfterDrawing \ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-heapLimitAfterDrawing-by-participant'
    j = j + 1
  }
  
 
  
  return(list(
    plotList = plot_list,
    fileNames = fileNames
  ))
}



