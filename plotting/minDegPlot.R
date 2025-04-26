

get_minDeg_plots <- function(data_list, acuity, crowding) {

  levels <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(participant, conditionName, block_condition,thresholdParameter, targetEccentricityXDeg, targetEccentricityYDeg, spacingOverSizeRatio, viewingDistanceCm, fontNominalSizePt, level)
  } %>% 
    filter(targetEccentricityXDeg == 0 & targetEccentricityYDeg == 0) %>% 
    filter(!grepl("practice",conditionName, ignore.case = T))
  minDeg <- levels %>% 
    mutate(fontNominalSizeDeg = (180/pi) * atan2(fontNominalSizePt*2.54/72, viewingDistanceCm)) %>% 
    group_by(participant, block_condition, conditionName, 
                 thresholdParameter) %>%
    # minDeg = spacingMinDeg when thresholdParameter = spacingDeg, minDeg = sizeMinDeg when tresholdParameter = targetSizeDeg
    summarize(minDeg = case_when(!is.na(level) ~ 10^min(level),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg'  ~ min(fontNominalSizeDeg),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio
                                 )) %>% 
  filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg') %>% 
  distinct() %>% 
    filter(!is.na(minDeg))

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
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition')) %>% 
    filter(!is.na(minDeg), !is.na(questMeanAtEndOfTrialsLoop)) %>% 
    distinct()

  p4 <- ggplot(t) + 
    geom_point(aes(x = minDeg, y = 10^(questMeanAtEndOfTrialsLoop)),color="black", fill="black") +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(t))
    )) + 
    labs(x = 'sizeMinDeg',
         y = 'Acuity (deg)',
         title ='Foveal acuity vs. sizeMinDeg')
  
  # scatter diagram of foveal crowding (deg) vs. spacingMinDeg
  t <- crowding %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition')) %>% 
    filter(!is.na(minDeg), !is.na(log_crowding_distance_deg)) %>% 
    distinct()

  p5 <- ggplot(t) + 
    geom_point(aes(x = minDeg, y = 10^(log_crowding_distance_deg)),color="black", fill="black") +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(t))
    )) + 
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