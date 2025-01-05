get_duration_data <- function(data_list) {
  df <- foreach(i=1:length(data_list), .combine = 'rbind') %do% {
    data_list[[i]] %>%
      select(participant, 
             font, 
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
    t <- data_list[[i]] %>% select(targetMeasuredDurationSec,
                                   targetMeasuredLatenessSec,
                                   trialGivenToQuestErrorCheckLabels,
                                   trialGivenToQuestChecks,
                                   `heapUsedBeforeDrawing (MB)`,
                                   `heapTotalBeforeDrawing (MB)`,
                                   `heapLimitBeforeDrawing (MB)`,
                                   `heapUsedAfterDrawing (MB)`,
                                   `heapTotalAfterDrawing (MB)`,
                                   `heapLimitAfterDrawing (MB)`,
                                   fontNominalSizePx) %>% 
      filter(trialGivenToQuestChecks != '', !is.na(trialGivenToQuestChecks)) %>% 
      separate_rows(trialGivenToQuestErrorCheckLabels, trialGivenToQuestChecks, sep = ",") %>%
      mutate(trialGivenToQuestChecks = as.logical(trialGivenToQuestChecks))
    
    t <- t %>%
      pivot_wider(names_from = trialGivenToQuestErrorCheckLabels,
                  values_from = trialGivenToQuestChecks)
  }

  params <- params %>% select(where(~sum(!is.na(.)) > 0))
  params <- params[complete.cases(params),]
  c <- colnames(params)
  print(params %>% select(durationAcceptable, latenessAcceptable, noBlackout))
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
    width = 7,
    height = 7
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
    filter(font != '', participant != '')
  
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
    annotation_logticks() + 
    labs(title = 'targetMeasuredDurationSec vs fontNominalSizePx by font',
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
    guides(color=guide_legend(ncol=3, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredDurationSec vs fontNominalSizePx by participant',
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
    filter(font != '', participant != '')
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
    guides(color=guide_legend(ncol=3, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredLatenessSec vs fontNominalSizePx by font',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx')
  
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
    guides(color=guide_legend(ncol=3, title = '')) + 
    annotation_logticks() + 
    labs(title = 'targetMeasuredLatenessSec vs fontNominalSizePx by participant',
         caption = 'Dashed lines are limits set by thresholdAllowedLatenessSec and fontMaxPx')
  return(list(font = p1,
              participant = p2))
}

get_histogram_durationSec <- function(duration){
  if (nrow(duration) == 0) {
    return(NULL)
  }
  p <- ggplot(data = duration) +  
    geom_histogram(aes(x = targetMeasuredDurationSec),color="black", fill="black") + 
    facet_wrap(~deviceSystemFamily)
  return(p)
}

