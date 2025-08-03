source('./constant.R')
get_duration_data <- function(data_list, conditionNameInput) {
  print('inside get_duration_data')
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
      distinct()
  }
  
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    df <- df %>% filter(conditionName %in% conditionNameInput)
  } 
  if (is.character(df$targetMeasuredDurationSec)) {
    df <- df %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    df$targetMeasuredDurationSec <- as.numeric(df$targetMeasuredDurationSec)
  }
  return(df)
}

get_duration_corr <- function(data_list, conditionNameInput) {
  print('inside get_duration_corr')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             conditionName,
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
    
  }
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
  } 
  
  webGL <- get_webGL(data_list)
  
  
  summary <- params %>%
    group_by(participant, block) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T),
              .groups="drop")
  
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
    select(-c(participant, block))
  
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  params <- params %>% select_if(is.numeric) %>% 
    select_if(~sum(!is.na(.)) > 0)
  
  params <- params[complete.cases(params),]
  N = nrow(params)
  
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
         title = 'Timing Correlation Table') +
    plt_theme + 
    ggpp::geom_text_npc(aes(npcx = 'left',
                            npcy = 'top',
                            label= paste0('N=',N))) + 
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
  n = nrow(df %>% filter(!is.na(fontNominalSizePx), !is.na(fontPadding), !is.na(targetMeasuredDurationSec)))
  p1 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx*(1+fontPadding), 
                   y = targetMeasuredDurationSec,
                   color = font)) + 
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  for (i in 1:nrow(bounds)) {
    p1 <- p1 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p1 <- p1 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color_manual(values = colorPalette) + 
    guides(color=guide_legend(ncol=3, title = '')) + 
    theme_bw() +
    plt_theme_scatter +
    facet_wrap(~intervention) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    labs(title = 'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by font',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  p2 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx*(1+fontPadding), 
                   y = targetMeasuredDurationSec,
                   color = participant)) + 
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  for (i in 1:nrow(bounds)) {
    p2 <- p2 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p2 <- p2 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color(df$participant) +  
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    facet_wrap(~intervention) +
    theme_bw()+
    plt_theme_scatter+
    labs(title = 'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by participant',
         caption = 'Dashed lines are limits set by thresholdAllowedDurationRatio and fontMaxPx')
  
  if(n_distinct(df$participant) <= 20) {
    p2 <- p2 + guides(color=guide_legend(ncol=2, title = ''))
  } else {
    p2 <- p2 + guides(color=F)
  }
  p3 <- ggplot() +
    geom_point(data=df, 
               aes(x= fontNominalSizePx*(1+fontPadding),
                   y= targetMeasuredDurationSec, 
                   color = as.factor(fontPadding))) +
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  
  for (i in 1:nrow(bounds)) {
    p3 <- p3 + 
      geom_hline(yintercept=bounds$lower[i], linetype="dashed") +
      geom_hline(yintercept=bounds$upper[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p3 <- p3 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color(df$fontPadding) + 
    guides(color=guide_legend(ncol=2, title = 'fontPadding')) + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    facet_wrap(~intervention) +
    theme_bw()+
    plt_theme_scatter + 
    labs(title = 'targetMeasuredDurationSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding',
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
  
  dt <- df %>% filter(!is.na(fontPadding),
                      !is.na(fontNominalSizePx),
                      !is.na(targetMeasuredLatenessSec),
                      !is.na(font)) %>% 
    distinct()
  n = nrow(dt)
  p1 <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePx*(1+fontPadding), 
                   y = targetMeasuredLatenessSec,
                   color = font)) +
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  
  for (i in 1:nrow(bounds)) {
    p1 <- p1 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p1 <- p1 + 
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color(df$font) +  
    facet_wrap(~intervention) + 
    guides(color=guide_legend(ncol=3, title = '')) + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) + 
    labs(title = 'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by font',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx') + 
    theme_bw()+
    plt_theme_scatter
  
  dt <- df %>% filter(!is.na(fontPadding),
                      !is.na(fontNominalSizePx),
                      !is.na(targetMeasuredLatenessSec),
                      !is.na(participant)) %>% 
    distinct()
  n = nrow(dt)
  p2 <- ggplot() +
    geom_point(data=dt,
               aes(x=fontNominalSizePx*(1+fontPadding), 
                   y = targetMeasuredLatenessSec,
                   color = participant)) +
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  for (i in 1:nrow(bounds)) {
    p2 <- p2 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p2 <- p2 +
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color(df$participant) +  
    facet_wrap(~intervention) + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    labs(title = 'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by participant',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx') +
    theme_bw()+
    plt_theme_scatter
  
  if (n_distinct(df$participant) <= 20) {
    p2 <- p2 + guides(color=guide_legend(ncol=2, title = ''))
  } else {
    p2 <- p2 + guides(color=F)
  }
 
  dt <- df %>% filter(!is.na(fontPadding),
                      !is.na(fontNominalSizePx),
                      !is.na(targetMeasuredLatenessSec)) %>% 
    distinct()
  n = nrow(dt)
  p3 <- ggplot() +
    geom_point(data=dt, 
               aes(x=fontNominalSizePx*(1+fontPadding),
                   y=targetMeasuredLatenessSec, 
                   color = as.factor(fontPadding))) +
    ggpp::geom_text_npc(aes(npcx = 'left',
                           npcy = 'top',
                           label = paste0('N=', n)))
  for (i in 1:nrow(bounds)) {
    p3 <- p3 + 
      geom_hline(yintercept=bounds$thresholdAllowedLatenessSec[i], linetype="dashed") + 
      geom_vline(xintercept=bounds$fontMaxPx[i], linetype="dashed")
  }
  p3 <- p3 +
    scale_x_log10(expand=c(0,.1)) +
    scale_y_log10(expand=c(0,.1)) + 
    scale_color(df$fontPadding) +  
    facet_wrap(~intervention) + 
    guides(color=guide_legend(ncol=2, title = 'fontPadding')) + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    labs(title = 'targetMeasuredLatenessSec vs\nfontNominalSizePx*(1+fontPadding)\ncolored by fontPadding',
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
  t1 <- duration %>% filter(!is.na(targetMeasuredDurationSec)) %>% 
    group_by(deviceSystemFamily) %>% 
    mutate(N= paste0('N=',n()))
  p1 <- ggplot(data = t1) +  
    geom_histogram(aes(x = targetMeasuredDurationSec),color="black", fill="gray80") + 
    theme_bw()+
    plt_theme+
    facet_wrap(~deviceSystemFamily) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = 'top', label = N)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01),  # Reduce decimal places
      breaks = scales::breaks_pretty(n = 5)  # Reduce number of labels shown
    ) 
  
  t2 <- duration %>% filter(!is.na(targetMeasuredLatenessSec)) %>% 
    group_by(deviceSystemFamily) %>% 
    mutate(N= paste0('N=',n()))
  
  p2 <- ggplot(data = t2) +  
    geom_histogram(aes(x = targetMeasuredLatenessSec), color="black", fill="gray80") + 
    theme_bw() +
    plt_theme +
    facet_wrap(~deviceSystemFamily) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = 'top', label = N)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-axis labels
    scale_x_continuous(
      labels = scales::label_number(accuracy = 0.01),  # Reduce decimal places
      breaks = scales::breaks_pretty(n = 5)  # Reduce number of labels shown
    ) 
  
  # p2 <- ggplot(data = duration) +  
  #   geom_histogram(aes(x = targetMeasuredLatenessSec),color="black", fill="black") + 
  #   theme_bw()+
  #   plt_theme+
  #   facet_wrap(~deviceSystemFamily)
  return(list(
    duration = p1,
    lateness = p2
  ))
}

append_hist_quality <- function(data_list, plot_list, fileNames, conditionNameInput){
  
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             block_condition,
             conditionName,
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
        # screenHeightCm = screenHeightPx / pxPerCm,
      )
    
  }
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
  } 
  
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%  separate_rows(targetMeasuredDurationSec,sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  
  
  
  blockCondition <- params %>% 
    group_by(participant, block_condition) %>% 
    summarize(goodTrials = sum(trialGivenToQuest, na.rm =T),
              badTrials = sum(!trialGivenToQuest, na.rm =T),
              .groups="drop")
  
  blockCondition_vars <- c("goodTrials", 'badTrials')
  
  j = length(plot_list) + 1
  for (var in blockCondition_vars) {
    if (n_distinct(blockCondition[var]) > 1) {
      
      avg <- round(mean(as.numeric(blockCondition[[var]]), na.rm =T),2)
      sd <- round(sd(as.numeric(blockCondition[[var]]), na.rm =T),2)
      n = length(blockCondition[!is.na(blockCondition[var]),])
      
      plot_list[[j]] <- ggplot(blockCondition, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="gray80") + 
        ggpp::geom_text_npc(
          data = NULL,
          aes(npcx = 'right',
              npcy = 'top'),
          label = paste0('N = ', n)
          
        ) +
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  return(list(plotList = plot_list,
              fileNames = fileNames))
}

append_hist_time <- function(data_list, plot_list, fileNames, conditionNameInput){

  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             block_condition,
             conditionName,
             computeRandomMHz,
             `heapUsedBeforeDrawing (MB)`,
             `heapTotalBeforeDrawing (MB)`,
             `heapLimitBeforeDrawing (MB)`,
             `heapUsedAfterDrawing (MB)`,
             `heapTotalAfterDrawing (MB)`,
             `heapLimitAfterDrawing (MB)`,
             `heapTotalPostLateness (MB)`,
             `heapTotalPreLateness (MB)`) %>% 
      mutate(
        deltaHeapUsedMB = as.numeric(`heapUsedAfterDrawing (MB)`) - as.numeric(`heapUsedBeforeDrawing (MB)`),
        deltaHeapTotalMB = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
        deltaHeapLatenessMB = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`),
      )
    
  }
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
  } 
  
  webGL <- get_webGL(data_list)
  
  blockAvg <- params %>% 
    group_by(participant, block) %>% 
    summarize(heapTotalAfterDrawingAvg = mean( `heapTotalAfterDrawing (MB)`, na.rm =T),
              heapUsedAfterDrawingAvg = mean( `heapUsedAfterDrawing (MB)`, na.rm =T),
              .groups="drop")
  
  params_vars <- c("heapUsedAfterDrawing (MB)", 
                   "heapTotalAfterDrawing (MB)", 
                   "heapLimitAfterDrawing (MB)", 
                   "deltaHeapTotalMB", 
                   "deltaHeapUsedMB",               
                   "computeRandomMHz")
  
  webGL_vars <- c("maxTextureSize", "maxViewportSize")
  
  blockAvg_vars <- c("heapTotalAfterDrawingAvg", "heapUsedAfterDrawingAvg")
  
  j = length(plot_list) + 1
  # Loop through params dataset and generate histograms
  
  for (var in params_vars) {
     data <- params %>% select("participant", all_of(var)) %>% 
       filter(!is.na(var)) %>%
       distinct()
     
    if (nrow(data) >0) {
      # avg <- round(mean(as.numeric(data[[var]]), na.rm =T),2)
      # sd <- round(sd(as.numeric(data[[var]]), na.rm =T),2)
      n = nrow(data)
      label = paste0('N = ', n)

      plot_list[[j]] <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="gray80") + 
        ggpp::geom_text_npc(
          aes(npcx = 'right',
              npcy = 'top'),
          label = label
        ) +
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  
  if (n_distinct(webGL['WebGLVersion']) > 1) {
    plot_list[[j]] <- ggplot(webGL, aes(x = WebGLVersion)) +
      geom_bar(color="black", fill="black") + 
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = 'right',
            npcy = 'top'),
        label = paste0('N = ', n_distinct(webGL['WebGLVersion']))
      ) +
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
      
      # avg <- round(mean(as.numeric(webGL[[var]]), na.rm =T),2)
      # sd <- round(sd(as.numeric(webGL[[var]]), na.rm =T),2)
      n = length(webGL[!is.na(webGL[var]),])
      
      label = paste0('N = ', n)
      plot_list[[j]] <- ggplot(webGL, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="gray80") +
        ggpp::geom_text_npc(
          data = NULL,
          aes(npcx = 'right',
              npcy = 'top'),
          label = paste0('N = ', n)
          
        ) +
        theme_bw() +
        labs(title = paste("Histogram of", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  for (var in blockAvg_vars) {
    if (n_distinct(blockAvg[var]) > 1) {
      # avg <- round(mean(blockAvg[[var]], na.rm =T),2)
      # sd <- round(sd(blockAvg[[var]], na.rm =T),2)
      n = length(blockAvg[!is.na(blockAvg[var]),])
      
      plot_list[[j]] <- ggplot(blockAvg, aes(x = .data[[var]])) +
        geom_histogram(color="black", fill="gray80") + 
        ggpp::geom_text_npc(
          data = NULL,
          aes(npcx = 'right',
              npcy = 'top'),
          label = paste0('N = ', n)
          
        ) +
        theme_bw() +
        labs(title = paste("Histogram of\n", var))
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  
  print('done timing hist')
  
  return(list(plotList = plot_list,
              fileNames = fileNames))
}

append_scatter_list <- function(data_list, plot_list, fileNames, conditionNameInput) {
  print('inside append_scatter_list')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             conditionName,
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
  
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
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
              badDurationTrials = sum(targetMeasuredDurationSec > upper | targetMeasuredDurationSec < lower),
              .groups="drop") %>% 
    arrange(hardwareConcurrency) %>% 
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  j = length(plot_list) + 1
  
  
  if (n_distinct(blockAvg$deviceMemoryGB) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    n = nrow(blockAvg %>% filter(!is.na(deviceMemoryGB), !is.na(badLatenessTrials)))
    plot_list[[j]] <- ggplot(data=blockAvg, aes(x=deviceMemoryGB,y=badLatenessTrials, color = participant)) +
      geom_jitter() +
      guides(color=guide_legend(ncol=2, title = '')) + 
      labs(title = 'badLatenessTrials vs. deviceMemoryGB\ncolored by participant',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-deviceMemoryGB-by-participant'
    j = j + 1
  }
  
  
  return(list(
    plotList = plot_list,
    fileNames=fileNames
  ))
}

append_scatter_time_participant <- function(data_list, plot_list, fileNames, conditionNameInput) {
  print('inside append_scatter_time_participant')
  
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>%
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             conditionName,
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
      mutate(
        deltaHeapUsedMB      = as.numeric(`heapUsedAfterDrawing (MB)`)  - as.numeric(`heapUsedBeforeDrawing (MB)`),
        deltaHeapTotalMB     = as.numeric(`heapTotalAfterDrawing (MB)`) - as.numeric(`heapTotalBeforeDrawing (MB)`),
        deltaHeapLatenessMB  = as.numeric(`heapTotalPostLateness (MB)`) - as.numeric(`heapTotalPreLateness (MB)`)
      )
  }
  
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
  } 
  
  # Convert targetMeasuredDurationSec if character with comma-separated values
  if (is.character(params$targetMeasuredDurationSec)) {
    params <- params %>%
      separate_rows(targetMeasuredDurationSec, sep=',')
    params$targetMeasuredDurationSec <- as.numeric(params$targetMeasuredDurationSec)
  }
  
  # Retrieve webGL info and merge
  webGL <- get_webGL(data_list)
  
  params <- params %>%
    filter(!is.na(font)) %>%
    left_join(webGL, by = 'participant') %>%
    arrange(hardwareConcurrency) %>%
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  # Summaries at block level
  blockAvg <- params %>%
    group_by(participant, block, hardwareConcurrency, deviceSystemFamily, font) %>%
    mutate(
      upper = max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio) * targetDurationSec,
      lower = targetDurationSec / max(thresholdAllowedDurationRatio, 1/thresholdAllowedDurationRatio)
    ) %>%
    summarize(
      heapTotalAfterDrawingAvg = mean(`heapTotalAfterDrawing (MB)`, na.rm =T),
      heapUsedAfterDrawingAvg  = mean(`heapUsedAfterDrawing (MB)`,  na.rm =T),
      badLatenessTrials        = sum(targetMeasuredLatenessSec > thresholdAllowedLatenessSec),
      badDurationTrials        = sum(targetMeasuredDurationSec > upper | targetMeasuredDurationSec < lower),
      .groups="drop"
    ) %>%
    arrange(hardwareConcurrency) %>%
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency))
  
  j = length(plot_list) + 1
  
  # 1) targetMeasuredLatenessSec vs deltaHeapTotalMB (y=targetMeasuredLatenessSec)
  if (n_distinct(params$deltaHeapTotalMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    # Filter valid rows
    filtered_params <- params %>%
      filter(!is.na(deltaHeapTotalMB), !is.na(targetMeasuredLatenessSec))
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)

    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=deltaHeapTotalMB, 
                      y=targetMeasuredLatenessSec,
                      color = participant)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      scale_color(params$participant) +  
      plt_theme_scatter +
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'targetMeasuredLatenessSec vs. deltaHeapTotalMB\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-deltaHeapTotalMB-by-participant'
    j = j + 1
  }
  
  # 2) targetMeasuredLatenessSec vs longTaskDurationSec (y=targetMeasuredLatenessSec)
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredLatenessSec) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(longTaskDurationSec), !is.na(targetMeasuredLatenessSec)) %>% distinct()
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params,
                  aes(x=longTaskDurationSec,
                      y=targetMeasuredLatenessSec,
                      color = participant)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'targetMeasuredLatenessSec vs. longTaskDurationSec\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  
  # 3) targetMeasuredDurationSec vs longTaskDurationSec (y=targetMeasuredDurationSec)
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(longTaskDurationSec), !is.na(targetMeasuredDurationSec)) %>% 
      distinct()
    
    y_min <- min(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    n = nrow(filtered_params)

    plot_list[[j]] <- ggplot(data=filtered_params, aes(x=longTaskDurationSec, y=targetMeasuredDurationSec, color = participant)) +
      geom_jitter() +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'targetMeasuredDurationSec vs. longTaskDurationSec\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'longTaskDurationSec-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  
  # 4) longTaskDurationSec vs. fontNominalSizePx*(1+fontPadding) (y=longTaskDurationSec)
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$longTaskDurationSec) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    # For dynamic y-limits, filter out NA in y=longTaskDurationSec and any columns needed for x
    filtered_params <- params %>%
      filter(!is.na(fontNominalSizePx), !is.na(fontPadding), !is.na(longTaskDurationSec)) %>% 
      distinct()
    
    y_min <- min(filtered_params$longTaskDurationSec, na.rm = TRUE)
    y_max <- max(filtered_params$longTaskDurationSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot(data=filtered_params,
                             aes(x=fontNominalSizePx*(1+fontPadding), y=longTaskDurationSec, color = participant)) +
      geom_jitter() +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'longTaskDurationSec vs. fontNominalSizePx*(1+fontPadding)\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'longTaskDurationSec-vs-fontNominalSizePx-by-participant'
    j = j + 1
  }
  
  # 5) deltaHeapUsedMB vs longTaskDurationSec (y=deltaHeapUsedMB)
  if (n_distinct(params$longTaskDurationSec) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(longTaskDurationSec), !is.na(deltaHeapUsedMB)) %>% 
      distinct()
    
    y_min <- min(filtered_params$deltaHeapUsedMB, na.rm = TRUE)
    y_max <- max(filtered_params$deltaHeapUsedMB, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=longTaskDurationSec, 
                      y=deltaHeapUsedMB, 
                      color = participant)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'deltaHeapUsedMB vs. longTaskDurationSec\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-longTaskDurationSec-by-participant'
    j = j + 1
  }
  
  # 6) targetMeasuredDurationSec vs. targetMeasuredLatenessSec (y=targetMeasuredDurationSec) [ALREADY DONE]
  if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    # Dynamically calculate ylim range with padding
    filtered_params <- params %>%
      filter(!is.na(targetMeasuredLatenessSec) & !is.na(targetMeasuredDurationSec))
    
    y_min <- min(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    y_limits <- c(y_min, y_max + y_padding)
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot(filtered_params) +
      geom_jitter(data=params, 
                  aes(x=targetMeasuredLatenessSec, 
                      y=targetMeasuredDurationSec, 
                      color = participant)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))) +
      labs(
        title   = 'targetMeasuredDurationSec vs. targetMeasuredLatenessSec\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-targetMeasuredLatenessSec-by-participant'
    j = j + 1
  }
  
  # 7) badDurationTrials vs. badLatenessTrials (blockAvg) (y=badDurationTrials)
  if (n_distinct(blockAvg$badLatenessTrials) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_block <- blockAvg %>%
      filter(!is.na(badLatenessTrials), !is.na(badDurationTrials))
    
    y_min <- min(filtered_block$badDurationTrials, na.rm = TRUE)
    y_max <- max(filtered_block$badDurationTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block, 
                  aes(x=badLatenessTrials,
                      y=badDurationTrials, 
                      color = participant)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                        npcy = 'top',
      #                        label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(blockAvg$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badDurationTrials vs. badLatenessTrials\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badDurationTrials-vs-badLatenessTrials-by-participant'
    j = j + 1
  }
  
  # 8) badLatenessTrials vs. heapLimitAfterDrawing (MB) (y=badLatenessTrials)
  if (n_distinct(params$`heapLimitAfterDrawing (MB)`) > 1 & n_distinct(params$badLatenessTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(`heapLimitAfterDrawing (MB)`), !is.na(badLatenessTrials))
    
    y_min <- min(filtered_params$badLatenessTrials, na.rm = TRUE)
    y_max <- max(filtered_params$badLatenessTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params,
                  aes(x=`heapLimitAfterDrawing (MB)`,
                      y=badLatenessTrials,
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top'
                             ),
                          label = paste0('N=', n)) + 
      plt_theme_scatter +
      scale_color(blockAvg$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badLatenessTrials vs. heapLimitAfterDrawing\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badLatenessTrials-vs-heapLimitAfterDrawing-by-participant'
    j = j + 1
  }
  
  
  # 9) badLatenessTrials vs. hardwareConcurrency (blockAvg) (y=badLatenessTrials)
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_block <- blockAvg %>%
      filter(!is.na(hardwareConcurrency), !is.na(badLatenessTrials))
    
    y_min <- min(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_max <- max(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block,
                  aes(x=hardwareConcurrency, 
                      y=badLatenessTrials, 
                      color = participant),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top'
                             ),
                          label = paste0('N=', n)) + 
      plt_theme_scatter +
      scale_color(blockAvg$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badLatenessTrials vs. hardwareConcurrency\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badLatenessTrials-vs-hardwareConcurrency-by-participant'
    j = j + 1
  }
  
  # 10) badDurationTrials vs. hardwareConcurrency (blockAvg) (y=badDurationTrials)
  if (n_distinct(blockAvg$hardwareConcurrency) > 1 & n_distinct(blockAvg$badDurationTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_block <- blockAvg %>%
      filter(!is.na(hardwareConcurrency), !is.na(badDurationTrials))
    
    y_min <- min(filtered_block$badDurationTrials, na.rm = TRUE)
    y_max <- max(filtered_block$badDurationTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block,
                  aes(x=hardwareConcurrency, 
                      y=badDurationTrials, 
                      color = participant),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top'),
                          label = paste0('N=', n)) + 
      scale_color(blockAvg$participant) +  
      plt_theme_scatter +
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badDurationTrials vs. hardwareConcurrency\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badDurationTrials-vs-hardwareConcurrency-by-participant'
    j = j + 1
  }
  
  # 11) badLatenessTrials vs. heapUsedAfterDrawingAvg (blockAvg) (y=badLatenessTrials)
  if (n_distinct(blockAvg$heapUsedAfterDrawingAvg) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_block <- blockAvg %>%
      filter(!is.na(heapUsedAfterDrawingAvg), !is.na(badLatenessTrials))
    
    y_min <- min(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_max <- max(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block,
                  aes(x=heapUsedAfterDrawingAvg, 
                      y=badLatenessTrials,
                      color = participant),
                  ) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top'),
                            label = paste0('N=', n),
                          position=position_jitter(width=0.1, height=0.1)) + 
      plt_theme_scatter +
      scale_color(blockAvg$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badLatenessTrials vs. heapUsedAfterDrawingAvg\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badLatenessTrials-vs-heapUsedAfterDrawingAvg-by-participant'
    j = j + 1
  }
  
  # 12) badLatenessTrials vs. heapTotalAfterDrawingAvg (blockAvg) (y=badLatenessTrials)
  if (n_distinct(blockAvg$heapTotalAfterDrawingAvg) > 1 & n_distinct(blockAvg$badLatenessTrials) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_block <- blockAvg %>%
      filter(!is.na(heapTotalAfterDrawingAvg), !is.na(badLatenessTrials))
    
    y_min <- min(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_max <- max(filtered_block$badLatenessTrials, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block,
                  aes(x=heapTotalAfterDrawingAvg, 
                      y=badLatenessTrials, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(blockAvg$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'badLatenessTrials vs. heapTotalAfterDrawingAvg\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'badLatenessTrials-vs-heapTotalAfterDrawingAvg-by-participant'
    j = j + 1
  }
  
  # 13) targetMeasuredDurationSec vs. deltaHeapLatenessMB (y=targetMeasuredDurationSec)
  if (n_distinct(params$deltaHeapLatenessMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(deltaHeapLatenessMB), !is.na(targetMeasuredDurationSec))
    
    y_min <- min(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredDurationSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=deltaHeapLatenessMB, 
                      y=targetMeasuredDurationSec, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'targetMeasuredDurationSec vs. deltaHeapLatenessMB\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-deltaHeapLatenessMB-by-participant'
    j = j + 1
  }
  
  # 14) targetMeasuredLatenessSec vs maxTextureSize (y=targetMeasuredLatenessSec)
  if (n_distinct(params$targetMeasuredLatenessSec) > 1  & n_distinct(params$maxViewportSize) > 1) {
    # We keep your x-limits from min(params$maxTextureSize)-5 to max(params$maxTextureSize)+5
    # Add dynamic y-limits for targetMeasuredLatenessSec
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(maxTextureSize), !is.na(targetMeasuredLatenessSec))
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=params, 
                  aes(x=maxTextureSize, 
                      y=targetMeasuredLatenessSec, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      scale_x_continuous(limits = c(min(params$maxTextureSize)-5, max(params$maxTextureSize) + 5)) +
      labs(
        title   = 'targetMeasuredLatenessSec vs maxTextureSize\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-maxTextureSize-by-participant'
    j = j + 1
  }
  
  # 15) targetMeasuredLatenessSec vs maxViewportSize (y=targetMeasuredLatenessSec)
  if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$maxViewportSize) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(maxViewportSize), !is.na(targetMeasuredLatenessSec))
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=maxViewportSize,
                      y=targetMeasuredLatenessSec, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      scale_x_continuous(limits = c(min(params$maxViewportSize)-5, max(params$maxViewportSize)+5)) +
      labs(
        title   = 'targetMeasuredLatenessSec vs maxViewportSize\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-maxViewportSize-by-participant'
    j = j + 1
  }
  
  # 16) targetMeasuredLatenessSec vs deltaHeapLatenessMB (y=targetMeasuredLatenessSec)
  if (n_distinct(params$deltaHeapLatenessMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(deltaHeapLatenessMB), !is.na(targetMeasuredLatenessSec))
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=deltaHeapLatenessMB, 
                      y=targetMeasuredLatenessSec,
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) +
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'targetMeasuredLatenessSec vs. deltaHeapLatenessMB\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-deltaHeapLatenessMB-by-participant'
    j = j + 1
  }
  
  # 17) deltaHeapTotalMB vs heapUsedBeforeDrawing (MB) (y=deltaHeapTotalMB)
  if (n_distinct(params$`heapUsedBeforeDrawing (MB)`) > 1 & n_distinct(params$deltaHeapTotalMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(`heapUsedBeforeDrawing (MB)`), !is.na(deltaHeapTotalMB))
    
    y_min <- min(filtered_params$deltaHeapTotalMB, na.rm = TRUE)
    y_max <- max(filtered_params$deltaHeapTotalMB, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params,
                  aes(x=`heapUsedBeforeDrawing (MB)`,
                      y=deltaHeapTotalMB, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'deltaHeapTotalMB vs. heapUsedBeforeDrawing\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'deltaHeapTotalMB-vs-heapUsedBeforeDrawing-by-participant'
    j = j + 1
  }
  
  # 18) deltaHeapUsedMB vs deltaHeapTotalMB (y=deltaHeapUsedMB)
  if (n_distinct(params$deltaHeapTotalMB) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(deltaHeapTotalMB), !is.na(deltaHeapUsedMB))
    
    y_min <- min(filtered_params$deltaHeapUsedMB, na.rm = TRUE)
    y_max <- max(filtered_params$deltaHeapUsedMB, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params,
                  aes(x=deltaHeapTotalMB, 
                      y=deltaHeapUsedMB, 
                      color = participant)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             label = paste0('N=', n))) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'deltaHeapUsedMB vs. deltaHeapTotalMB\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-deltaHeapTotalMB-by-participant'
    j = j + 1
  }
  
  # 19) deltaHeapTotalMB vs heapLimitAfterDrawing (MB) (y=deltaHeapTotalMB)
  if (n_distinct(params$`heapLimitAfterDrawing (MB)`) > 1 & n_distinct(params$deltaHeapTotalMB) > 1) {
    num_legend_items <- n_distinct(params$participant)
    
    filtered_params <- params %>%
      filter(!is.na(`heapLimitAfterDrawing (MB)`), !is.na(deltaHeapTotalMB))
    
    y_min <- min(filtered_params$deltaHeapTotalMB, na.rm = TRUE)
    y_max <- max(filtered_params$deltaHeapTotalMB, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=`heapLimitAfterDrawing (MB)`, 
                      y=deltaHeapTotalMB, 
                      color = participant),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                             npcy = 'top',
                             ),
                          label = paste0('N=', n)) + 
      plt_theme_scatter +
      scale_color(params$participant) +  
      guides(color = guide_legend(
        col  = ifelse(num_legend_items > 20, 5, 3),
        title = '',
        override.aes = list(size = ifelse(num_legend_items > 20, 3, 5))
      )) +
      theme(
        legend.text = element_text(size = ifelse(num_legend_items > 20, 9, 14))
      ) +
      labs(
        title   = 'deltaHeapTotalMB vs. heapLimitAfterDrawing\ncolored by participant',
        caption = 'Points jittered to avoid occlusion.'
      ) +
      coord_cartesian(ylim = c(y_min, y_max + y_padding))
    
    fileNames[[j]] <- 'deltaHeapTotalMB-vs-heapLimitAfterDrawing-by-participant'
    j = j + 1
  }
  
  return(list(
    plotList = plot_list,
    fileNames = fileNames
  ))
}

append_scatter_time <- function(data_list, plot_list, fileNames, conditionNameInput) {
  print('inside append_scatter_time')
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>%
      filter(!is.na(staircaseName)) %>%
      select(participant,
             block,
             conditionName,
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
  
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    params <- params %>% filter(conditionName %in% conditionNameInput)
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
    mutate(hardwareConcurrency = as.factor(hardwareConcurrency)) %>% 
    ungroup()
  
  j = length(plot_list) + 1
  
  
  if (n_distinct(blockAvg$badLatenessTrials) > 1) {
    filtered_block <- blockAvg %>% filter(!is.na(hardwareConcurrency), !is.na(badLatenessTrials), !is.na(font))
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block,
                  aes(x=hardwareConcurrency, 
                      y=badLatenessTrials, 
                      color = font),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top'),
                          label = paste0('N=', n)) +
      guides(color=guide_legend(ncol=3, title = '')) +
      labs(title = 'badLatenessTrials vs hardwareConcurrency\ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badLatenessTrials-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(blockAvg$badDurationTrials) > 1) {
    filtered_block <- blockAvg %>% filter(!is.na(hardwareConcurrency), !is.na(badDurationTrials), !is.na(font))
    n = nrow(filtered_block)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_block, 
                  aes(x=hardwareConcurrency, 
                      y=badDurationTrials, 
                      color = font),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top'),
                          label = paste0('N=', n)) +
      guides(color=guide_legend(ncol=3, title = '')) +
      labs(title = 'badDurationTrials vs hardwareConcurrency\ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'badDurationTrials-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    filtered_params <- params %>% filter(!is.na(hardwareConcurrency), !is.na(targetMeasuredLatenessSec), !is.na(font))
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=hardwareConcurrency, 
                      y=targetMeasuredLatenessSec,
                      color = font),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top'),
                          label = paste0('N=', n)) +
      guides(color=guide_legend(ncol=3, title = '')) +
      labs(title = ' targetMeasuredLatenessSec vs hardwareConcurrency\ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1) {
    filtered_params <- params %>% filter(!is.na(hardwareConcurrency), !is.na(targetMeasuredDurationSec), !is.na(font))
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=hardwareConcurrency, 
                      y=targetMeasuredDurationSec, 
                      color = font),
                  position=position_jitter(width=0.1, height=0.1)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                         npcy = 'top'),
      #                     label = paste0('N=', n)) +
      # guides(color=guide_legend(ncol=3, title = '')) +
      labs(title = 'targetMeasuredDurationSec vs hardwareConcurrency\ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-hardwareConcurrency-by-font'
    j = j + 1
  }
  
  if (n_distinct(params$targetMeasuredLatenessSec) > 1 & n_distinct(params$mustTrackSec) > 1) {
    filtered_params <- params %>%
      filter(!is.na(mustTrackSec), !is.na(targetMeasuredLatenessSec))
    
    y_min <- min(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_max <- max(filtered_params$targetMeasuredLatenessSec, na.rm = TRUE)
    y_padding <- (y_max - y_min) * 0.1
    n = nrow(filtered_params)
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=mustTrackSec,
                      y=targetMeasuredLatenessSec, 
                      color = deviceSystemFamily),
                  position=position_jitter(width=0.1, height=0.1)) +
      # ggpp::geom_text_npc(aes(npcx = 'left',
      #                         npcy = 'top'),
      #                     label = paste0('N=', n)) +
      scale_x_continuous(limits = c(min(params$mustTrackSec) - 2,max(params$mustTrackSec) + 2)) +
      scale_y_continuous(limits =  c(y_min, y_max + y_padding)) +
      guides(color=guide_legend(ncol=3, title = 'OS')) +
      labs(title = 'targetMeasuredLatenessSec vs mustTrackSec\ncolored by OS',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredLatenessSec-vs-mustTrackSec-by-OS'
    j = j + 1
  }
  
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$targetMeasuredDurationSec) > 1) {
    n = nrow(params %>% filter(!is.na(fontNominalSizePx), !is.na(fontPadding), !is.na(targetMeasuredDurationSec)))
    plot_list[[j]] <- ggplot() +
      geom_jitter(data=params, aes(x=fontNominalSizePx*(1+fontPadding),
                                   y=targetMeasuredDurationSec,
                                   color = as.factor(fontPadding)),
                  position=position_jitter(width=0.1, height=0.1)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top'),
                          label = paste0('N=', n)) +
      guides(color=guide_legend(ncol=3, title='')) +
      labs(title = 'targetMeasuredDurationSec vs. fontNominalSizePx*(1+fontPadding)\ncolored by fontPadding',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'targetMeasuredDurationSec-vs-fontNominalSizePx-by-fontPadding'
    j = j + 1
  }
  if (n_distinct(params$fontNominalSizePx) > 1 & n_distinct(params$deltaHeapUsedMB) > 1) {
    filtered_params <- params %>%
      filter(!is.na(fontNominalSizePx), !is.na(fontPadding), !is.na(deltaHeapUsedMB))
    
    n = nrow(filtered_params)

    plot_list[[j]] <- ggplot() +
      geom_jitter(data=filtered_params, 
                  aes(x=fontNominalSizePx*(1+fontPadding),
                      y=deltaHeapUsedMB, 
                      color = font)) +
      ggpp::geom_text_npc(aes(npcx = 'left',
                          npcy = 'top'),
                          label = paste0('N=', n)) +
      guides(color=guide_legend(ncol=3, title = '')) +
      scale_x_log10() +
      scale_y_log10() +
      coord_fixed() +
      labs(title = 'deltaHeapUsedMB vs.fontNominalSizePx*(1+fontPadding)\ncolored by font',
           caption = 'Points jittered to avoid occlusion.')
    fileNames[[j]] <- 'deltaHeapUsedMB-vs-fontNominalSizePx-by-font'
    j = j + 1
  }
  
  
  return(list(
    plotList = plot_list,
    fileNames = fileNames
  ))
}







