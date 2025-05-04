

get_minDeg_plots <- function(data_list, acuity, crowding, quest) {

  levels <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(participant, conditionName, block_condition,thresholdParameter, 
                              targetEccentricityXDeg, targetEccentricityYDeg, spacingOverSizeRatio, 
                              viewingDistanceCm, fontNominalSizePt, level,
                              font)
  } %>% 
    filter(targetEccentricityXDeg == 0 & targetEccentricityYDeg == 0) %>% 
    filter(!grepl("practice",conditionName, ignore.case = T))
  # the conversion formula from fontNominalSizePt to level only true when font is Sloan.woff2
  minDeg <- levels %>% 
    mutate(fontNominalSizeDeg = (180/pi) * atan2(fontNominalSizePt*2.54/72, viewingDistanceCm)) %>% 
    group_by(participant, block_condition, conditionName, 
                 thresholdParameter, font) %>%
    # minDeg = spacingMinDeg when thresholdParameter = spacingDeg, minDeg = sizeMinDeg when tresholdParameter = targetSizeDeg
    summarize(minDeg = case_when(!is.na(level) ~ 10^min(level),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Sloan.woff2' ~ min(fontNominalSizeDeg),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Sloan.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Pelli.woff2' ~ min(fontNominalSizeDeg) / 5,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Pelli.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio / 5
                                 )) %>% 
  filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg') %>% 
  distinct() %>% 
    filter(!is.na(minDeg))


  params <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% distinct(participant, block_condition,targetMinimumPix, targetMinPhysicalPx, pxPerCm,viewingDistanceCm,spacingOverSizeRatio)
  } %>% filter(!is.na(targetMinPhysicalPx))
  
  estimatedDevicePixelRatio <- minDeg %>%
    left_join(params, by = c('participant', 'block_condition')) %>% 
    filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg')
  
  estimatedDevicePixelRatio <- minDeg %>%
    left_join(params, by = c('participant', 'block_condition')) %>% 
    filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg') %>% 
    group_by(participant, block_condition, conditionName, thresholdParameter) %>% 
    summarize(estimatedDevicePixelRatio = case_when(thresholdParameter == 'targetSizeDeg' ~ targetMinPhysicalPx/(tan(minDeg * pi / 180) * pxPerCm * viewingDistanceCm),
                                                    thresholdParameter == 'spacingDeg' ~ targetMinPhysicalPx/(tan((minDeg/spacingOverSizeRatio) * pi / 180) * pxPerCm * viewingDistanceCm)
    )) %>% 
    filter(!is.na(estimatedDevicePixelRatio))

  
  # histogram of estimatedDevicePixelRatio 
  p1 <- ggplot(estimatedDevicePixelRatio) + 
    geom_histogram(aes(x = estimatedDevicePixelRatio),
                   color = "black", fill = "black") +
    scale_x_log10(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x     = 'estimatedDevicePixelRatio',
      y     = 'Count',
      title = 'Histogram of estimatedDevicePixelRatio'
    )
  
  # histogram of sizeMinDeg (log-x, no ticks)
  p2 <- ggplot(minDeg %>% filter(thresholdParameter == 'targetSizeDeg')) + 
    geom_histogram(aes(x = minDeg),
                   color = "black", fill = "black") +
    scale_x_log10(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x     = 'sizeMinDeg',
      y     = 'Count',
      title = 'Histogram of sizeMinDeg'
    )
  
  # histogram of spacingMinDeg (log-x, no ticks)
  p3 <- ggplot(minDeg %>% filter(thresholdParameter == 'spacingDeg')) + 
    geom_histogram(aes(x = minDeg),
                   color = "black", fill = "black") +
    scale_x_log10(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(
      x     = 'spacingMinDeg',
      y     = 'Count',
      title = 'Histogram of spacingMinDeg'
    )
  
  
  # scatter diagram of foveal acuity (deg) vs. sizeMinDeg
  foveal_acuity <- acuity %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition')) %>% 
    filter(!is.na(minDeg), !is.na(questMeanAtEndOfTrialsLoop)) %>% 
    distinct()
  
  x_min <- min(foveal_acuity$minDeg, na.rm = TRUE)
  x_max <- max(foveal_acuity$minDeg, na.rm = TRUE)
  xlim_p4 <- c(0.6 * x_min, 1.2 * x_max)
  
  p4 <- ggplot(foveal_acuity) + 
    # equality line (y = x)
    geom_abline(slope = 1, intercept = 0, linetype = "solid") +
    geom_point(aes(x = minDeg, y = 10^(questMeanAtEndOfTrialsLoop)),
               color = "black", fill = "black") +
    scale_x_log10(
      limits = xlim_p4,
      breaks       = scales::log_breaks(base = 10),
    ) +
    scale_y_log10(
      breaks       = scales::log_breaks(base = 10),
    ) + 
    coord_fixed() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) + 
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(foveal_acuity))
    )) + 
    labs(
      x     = 'sizeMinDeg',
      y     = 'Acuity (deg)',
      title = 'Foveal acuity vs. sizeMinDeg'
    )
  
  # scatter diagram of foveal crowding (deg) vs. spacingMinDeg
  foveal_crowding <- crowding %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,block_condition,minDeg), by = c('participant', 'block_condition')) %>% 
    filter(!is.na(minDeg), !is.na(log_crowding_distance_deg)) %>% 
    distinct()
  
  x_min <- min(foveal_crowding$minDeg, na.rm = TRUE)
  x_max <- max(foveal_crowding$minDeg, na.rm = TRUE)
  xlim_p5 <- c(0.8 * x_min, 1.2 * x_max)
  
  p5 <- ggplot(foveal_crowding) + 
    # equality line (y = x)
    geom_abline(slope = 1, intercept = 0, linetype = "solid") +
    geom_point(aes(x = minDeg, y = 10^(log_crowding_distance_deg)),
               color = "black", fill = "black") +
    scale_x_log10(
      limits = xlim_p5,
      breaks = scales::log_breaks(base = 10),
    ) +
    scale_y_log10(
      breaks = scales::log_breaks(base = 10),
    ) + 
    coord_fixed() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) + 
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(foveal_crowding))
    )) + 
    labs(
      x     = 'spacingMinDeg',
      y     = 'Crowding distance (deg)',
      title = 'Foveal crowding vs. spacingMinDeg'
    )
  
  quest_subset <- quest %>%
    select(participant, block_condition, questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop, questType)
  
  # 4. derive foveal crowding DF and plot p6
  crowding_df <- quest_subset %>%
    filter(questType == "Foveal crowding") %>%
    rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop) %>%
    left_join(minDeg %>% select(participant, block_condition, minDeg),
              by = c("participant", "block_condition")) %>%
    distinct()
  
  p6 <- ggplot(crowding_df, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(log_crowding_distance_deg)
  )) +
    geom_point(color = "black", fill = "black") +
    scale_x_log10(breaks = scales::log_breaks(base = 10)) +
    scale_y_log10(breaks = scales::log_breaks(base = 10)) +
    coord_fixed() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Crowding distance (deg)",
      title = "Foveal crowding vs. Quest SD"
    )
  
  # 5. derive foveal acuity DF and plot p7
  acuity_df <- quest_subset %>%
    filter(questType == "Foveal acuity") %>%
    left_join(minDeg %>% select(participant, block_condition, minDeg),
              by = c("participant", "block_condition")) %>%
    distinct()
  
  p7 <- ggplot(acuity_df, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(questMeanAtEndOfTrialsLoop)
  )) +
    geom_point(color = "black", fill = "black") +
    scale_x_log10(breaks = scales::log_breaks(base = 10)) +
    scale_y_log10(breaks = scales::log_breaks(base = 10)) +
    coord_fixed() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Acuity (deg)",
      title = "foveal acuity vs. Quest SD"
    )
   
  
  return(
    list(
      scatter = list(
        plotList = list(p4, p5, p6, p7),
        fileNames = list(
          'foveal-acuity-vs-sizeMinDeg',
          'foveal-crowding-vs-spacingMinDeg',
          'questSD-vs-crowding',
          'questSD-vs-acuity'
        )
      ),
      hist = list(
        plotList = list(p1,p2,p3),
        fileNames = list('estimatedDevicePixelRatio-hist', 
                       'sizeMinDeg-hist', 
                       'spacingMinDeg-hist')
      )
    )
  )
}