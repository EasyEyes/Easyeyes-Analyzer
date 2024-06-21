plot_rsvp_crowding_acuity <- function(allData,df,pretest) {
  if (is.null(allData)) {
    return(list(
      ggplot(),
      ggplot(),
      ggplot()
    ))
  }
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  acuity <- allData$acuity
  t <- pretest %>% left_join(df, by = 'participant')
    # rename('PartcipantCode in experiment csv' =  'PartcipantCode.x',
    #        'PartcipantCode in pretest xlsx' =  'PartcipantCode.x')
  # write.csv(t, '~/Downloads/pretestVsExperimentCSV.csv')
  rsvp_speed <- rsvp_speed %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')
  crowding <- crowding %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')
  acuity <- acuity %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')

  p1 = ggplot(data = crowding) +
    geom_point(aes(x = Grade, y = 10^(log_crowding_distance_deg))) +
    facet_wrap(font~.) + 
    theme_classic() +
    labs(title = 'Crowding vs Grade',
         y = 'crowding (deg)')
  
  p2 = ggplot(data = rsvp_speed) +
    geom_point(aes(x = Grade, y = 10^(block_avg_log_WPM))) +
    theme_classic() + 
    labs(title = 'Rsvp vs Grade',
         y = 'rsvp reading speed (word/min)')
  
  p3 = ggplot(data = acuity) +
    geom_point(aes(x = Grade, y = 10^(questMeanAtEndOfTrialsLoop))) +
    theme_classic() +
    labs(title = 'Acuity vs Grade',
         y = 'acuity (deg)')
  p3 = ggplot(data = acuity) +
  return(list(
    p1,
    p2,
    p3
  ))
}

plot_rsvp_crowding <- function(allData) {
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  foveal <- crowding %>% filter(grepl('foveal', conditionName,ignore.case = T))
  peripheral <- crowding %>% filter(grepl('peripheral', conditionName,ignore.case = T))
  peripheral_rsvp <- peripheral %>%
    select(participant, log_crowding_distance_deg) %>%
    left_join(rsvp_speed)
  
  p1 <- ggplot(data = peripheral_rsvp, aes(x = 10^(log_crowding_distance_deg), y = 10^(block_avg_log_WPM))) +
    geom_point() +
    theme_classic() +
    scale_y_log10() +
    scale_x_log10() + 
    annotation_logticks() +
    labs(x = 'crowding distance degree',
         y = 'rsvp reading (word/min)',
         title='rsvp vs peripheral crowding')
  
  foveal_rsvp <- foveal %>%
    select(participant, log_crowding_distance_deg) %>%
    left_join(rsvp_speed)
  p2 <- ggplot(data = foveal_rsvp, aes(x = 10^(log_crowding_distance_deg), y = 10^(block_avg_log_WPM))) +
    geom_point() + 
    theme_classic() + 
    scale_y_log10() +
    scale_x_log10() + 
    annotation_logticks() + 
    labs(x = 'crowding distance degree', 
         y = 'rsvp reading (word/min)',
         title = 'rsvp vs foveal crowding')
  return(list(p1, p2))
}

get_pretest <- function(pretestCSV){
  file_list <- pretestCSV$data
  t <- readxl::read_xlsx(file_list[1]) %>% 
    rename('participant' = 'PavloviaSessionID')
  return(t)
}

