get_minDeg_plots <- function(data_list, acuity, crowding) {

  levels <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(participant, conditionName, block_condition,thresholdParameter, level) %>% 
      filter(!is.na(level))
  }
  minDeg <- levels %>% group_by(participant, block_condition, conditionName, 
                                thresholdParameter) %>%
    summarize(minDeg = 10^min(level))
  
  params <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% distinct(participant, block_condition,targetMinPhysicalPx, pxPerCm,viewingDistanceCm,spacingOverSizeRatio)
  } %>% filter(!is.na(targetMinPhysicalPx))
  
  estimatedDevicePixelRatio <- minDeg %>%
    left_join(params, by = c('participant', 'block_condition')) %>% 
    filter(thresholdParameter == 'targetSizeDeg ' | thresholdParameter == 'spacingDeg') %>% 
    group_by(participant, block_condition, conditionName,thresholdParameter) %>% 
    summarize(estimatedDevicePixelRatio = case_when(thresholdParameter == 'targetSizeDeg' ~ targetMinPhysicalPx/(tan(minDeg * pi / 180) * pxPerCm * viewingDistanceCm),
                                                    thresholdParameter == 'spacingDeg' ~ targetMinPhysicalPx/(tan((minDeg/spacingOverSizeRatio) * pi / 180) * pxPerCm * viewingDistanceCm)
    )) %>% 
    filter(!is.na(estimatedDevicePixelRatio))
  print(estimatedDevicePixelRatio)
  # histogram of estimatedDevicePixelRatio
  p1 <- ggplot(estimatedDevicePixelRatio) + 
    geom_histogram(aes(x = estimatedDevicePixelRatio),color="black", fill="black") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    labs(x = 'estimatedDevicePixelRatio',
         y = 'Count',
         title ='Histogram of estimatedDevicePixelRatio')
  # histogram of sizeMinDeg
  p2 <- ggplot(minDeg %>% filter(thresholdParameter == 'targetSizeDeg')) + 
    geom_histogram(aes(x = minDeg),color="black", fill="black") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    labs(x = 'sizeMinDeg',
         y = 'Count',
         title ='Histogram of sizeMinDeg')
  # histogram of spacingMinDeg
  p3 <- ggplot(minDeg %>% filter(thresholdParameter == 'spacingDeg')) + 
    geom_histogram(aes(x = minDeg),color="black", fill="black") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    labs(x = 'spacingMinDeg',
         y = 'Count',
         title ='Histogram of spacingMinDeg')
  
  # scatter diagram of foveal acuity (deg) vs. sizeMinDeg
  t <- acuity %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition'))
  p4 <- ggplot(t) + 
    geom_point(aes(x = minDeg, y = 10^(questMeanAtEndOfTrialsLoop)),color="black", fill="black") +
    scale_x_continuous(expand = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 1)) + 
    labs(x = 'sizeMinDeg',
         y = 'Acuity (deg)',
         title ='Foveal acuity vs. sizeMinDeg')
  
  # scatter diagram of foveal crowding (deg) vs. spacingMinDeg
  t <- crowding %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition'))
  p5 <- ggplot(t) + 
    geom_point(aes(x = minDeg, y = 10^(log_crowding_distance_deg)),color="black", fill="black") +
    scale_x_continuous(expand = c(0, 1)) + 
    scale_y_continuous(expand = c(0, 1)) + 
    labs(x = 'spacingMinDeg',
         y = 'Crowding distance (deg)',
         title ='Foveal crowding vs. spacingMinDeg')
  list(plotList = list(p1,p2,p3,p4,p5),
       fileNames = list('estimatedDevicePixelRatio-hist', 
                        'sizeMinDeg-hist', 
                        'spacingMinDeg-hist', 
                        'foveal-acuity-vs-sizeMinDeg', 
                        'foveal-crowding-vs-spacingMinDeg'))
}