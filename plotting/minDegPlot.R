

get_minDeg_plots <- function(data_list, acuity, crowding, quest) {

  levels <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(participant, conditionName,thresholdParameter, 
                              targetEccentricityXDeg, targetEccentricityYDeg, spacingOverSizeRatio, 
                              viewingDistanceCm, fontNominalSizePt, level, font)
  } %>% 
    filter(targetEccentricityXDeg == 0 & targetEccentricityYDeg == 0) %>% 
    filter(!grepl("practice",conditionName, ignore.case = T)) %>% 
    mutate(spacingOverSizeRatio = as.numeric(spacingOverSizeRatio),
           viewingDistanceCm = as.numeric(viewingDistanceCm),
           fontNominalSizePt = as.numeric(fontNominalSizePt),
           level = as.numeric(level))
  
  # the conversion formula from fontNominalSizePt to level only true when font is Sloan.woff2
  minDeg <- levels %>% 
    mutate(fontNominalSizeDeg = (180/pi) * atan2(fontNominalSizePt*2.54/72, viewingDistanceCm)) %>% 
    group_by(participant, conditionName, 
                 thresholdParameter, font) %>%
    # minDeg = spacingMinDeg when thresholdParameter = spacingDeg, minDeg = sizeMinDeg when tresholdParameter = targetSizeDeg
    summarize(minDeg = case_when(!is.na(level) ~ 10^min(level),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Sloan.woff2' ~ min(fontNominalSizeDeg),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Sloan.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Pelli.woff2' ~ min(fontNominalSizeDeg) / 5,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Pelli.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio / 5
                                 ),
              .groups="drop") %>% 
  filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg') %>% 
  distinct() %>% 
    filter(!is.na(minDeg)) %>% 
    ungroup()


  params <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>%
      distinct(participant, 
               conditionName,
               targetMinimumPix,
               targetMinPhysicalPx,
               pxPerCm,
               viewingDistanceCm,
               spacingOverSizeRatio)
  } %>% filter(!is.na(targetMinPhysicalPx)) %>% 
    mutate(targetMinimumPix = as.numeric(targetMinimumPix), 
           targetMinPhysicalPx = as.numeric(targetMinPhysicalPx),
           pxPerCm = as.numeric(pxPerCm),
           viewingDistanceCm = as.numeric(viewingDistanceCm),
           spacingOverSizeRatio = as.numeric(spacingOverSizeRatio))
  
  # histogram of sizeMinDeg (log-x, no ticks)
  stats <- minDeg %>%
    filter(thresholdParameter == 'targetSizeDeg',
           !is.na(minDeg)) %>%
    summarize(mean = round(mean(minDeg),2),
              sd = round(sd(minDeg),2),
              N = n(),
              .groups="drop")
  p2 <- ggplot(minDeg %>% filter(thresholdParameter == 'targetSizeDeg')) + 
    geom_histogram(aes(x = minDeg),
                   color = NA, fill = "gray80") +
    scale_x_log10(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    ggpp::geom_text_npc(
      aes(npcx = 'right',
          npcy = 'top'),
      label = paste0('mean = ', stats$mean, '\n sd = ', stats$sd, '\n N = ', stats$N)
    ) +
    labs(
      x = 'sizeMinDeg',
      y = 'Count',
      title = 'Histogram of sizeMinDeg'
    )
  
  
  # scatter diagram of foveal acuity (deg) vs. sizeMinDeg
  foveal_acuity <- acuity %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,conditionName,minDeg), by = c('participant', 'conditionName')) %>% 
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
      breaks = scales::log_breaks(base = 10)
    ) +
    scale_y_log10(
      breaks = scales::log_breaks(base = 10)
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
      x = 'sizeMinDeg',
      y= 'Acuity (deg)',
      title = 'Foveal acuity vs. sizeMinDeg'
    )
  
  # scatter diagram of foveal crowding (deg) vs. spacingMinDeg
  foveal_crowding <- crowding %>%
    filter(targetEccentricityXDeg == 0) %>%
    left_join(minDeg %>% select(participant,conditionName,minDeg), by = c('participant', 'conditionName')) %>% 
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
      x = 'spacingMinDeg',
      y = 'Crowding distance (deg)',
      title = 'Foveal crowding vs. spacingMinDeg'
    )
  
  quest_subset <- quest %>%
    select(participant, conditionName, questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop, questType, targetEccentricityXDeg)
  
  # 4. derive foveal crowding DF and plot p6
  foveal_crowding <- quest_subset %>%
    filter(questType == "Foveal crowding") %>%
    rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop) %>%
    distinct()
  
  p6 <- ggplot(foveal_crowding, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(log_crowding_distance_deg)
  )) +
    geom_point(color = "black", fill = "black") +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(foveal_crowding))
    )) + 
    scale_y_log10() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Crowding distance (deg)",
      title = "Foveal crowding vs. Quest SD"
    )
  
  p_crowding <- quest_subset %>%
    filter(questType == "Peripheral crowding") %>%
    rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop) %>%
    distinct()
  

  eccs_pc  <- sort(unique(p_crowding$targetEccentricityXDeg))
  lbl_pc   <- paste0("X ecc = ", paste(as.integer(round(eccs_pc)), collapse=", "), " deg")
  p7 <- ggplot(p_crowding, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(log_crowding_distance_deg)
  )) +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label=paste0(lbl_pc, "\nN=", nrow(p_crowding))
    )) + 
    geom_point(color = "black", fill = "black") +
    scale_y_log10() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Crowding distance (deg)",
      title = "Peripheral crowding vs. Quest SD"
    )
  
  
  # 5. derive foveal acuity DF and plot p7
  foveal_acuity <- quest_subset %>%
    filter(questType == "Foveal acuity") %>%
    distinct()
  
  p8 <- ggplot(foveal_acuity, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(questMeanAtEndOfTrialsLoop)
  )) +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(foveal_acuity))
    )) + 
    geom_point(color = "black", fill = "black") +
    scale_y_log10() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Acuity (deg)",
      title = "Foveal acuity vs. Quest SD"
    )
  
  p_acuity <- quest_subset %>%
    filter(questType == "Peripheral acuity") %>%
    distinct()
  
  print("in get_min_deg_plot")
  eccs_pa <- sort(unique(p_acuity$targetEccentricityXDeg))
  lbl_pa  <- paste0("X ecc = ", paste(as.integer(round(eccs_pa)), collapse=", "), " deg")
  
  p9 <- ggplot(p_acuity, aes(
    x = questSDAtEndOfTrialsLoop,
    y = 10^(questMeanAtEndOfTrialsLoop)
  )) +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0(lbl_pa,'\nN=', nrow(p_acuity))
    )) + 
    geom_point(color = "black", fill = "black") +
    scale_y_log10() +
    annotation_logticks(sides = "bl",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) +
    labs(
      x = "Quest SD",
      y = "Acuity (deg)",
      title = "Peripheral acuity vs. Quest SD"
    )
   
  
  return(
    list(
      scatter = list(
        plotList = list(p5),
        fileNames = list(
          'foveal-crowding-vs-spacingMinDeg'
        )
      ),
      scatter_quality = list(
        plotList = list(p4, p6, p7, p8, p9),
        fileNames = list(
          'foveal-acuity-vs-sizeMinDeg',
          'questSD-vs-foveal-crowding',
          'questSD-vs-peripheral-crowding',
          'questSD-vs-foveal-acuity',
          'questSD-vs-peripheral-acuity'
        )
      ),
      hist_quality = list (
        plotList = list(p2),
        fileNames = list('sizeMinDeg-hist')
      )
    )
  )
}

