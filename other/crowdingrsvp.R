library(ggcorrplot) 
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
  acuity <- allData$acuity %>% rename('log_acuity'='questMeanAtEndOfTrialsLoop')
  
  crowdingW <- crowding %>% mutate(type=ifelse(
    grepl('foveal', conditionName, ignore.case = T),
    'foveal',
    'peripheral'
  )) %>% 
    select(participant, log_crowding_distance_deg,type) %>% 
    pivot_wider(names_from=type, values_from = log_crowding_distance_deg)
  
  crowdingW <- crowdingW %>% 
    left_join(acuity %>% select(participant, log_acuity),by = 'participant') %>% 
    left_join(rsvp_speed %>% select(participant, block_avg_log_WPM), by = 'participant')
  
  crowdingW <- crowdingW %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant') %>% 
    select(log_acuity, foveal, peripheral, block_avg_log_WPM, Grade, RAVEN_Perc,
           `RAVEN - tot`, `MT_Reading Time (sec.)`, `MT_Total Text Syll`, 
           `MT_sill/sec`, MT_Errors,`OMT_Corr.Score (Read-Err)`) %>% 
    rename('log rsvp' = 'block_avg_log_WPM')
  crowdingW <- crowdingW %>% mutate_if(is.character, as.numeric)
  
  t <- data.frame(cor(crowdingW[complete.cases(crowdingW),]))
  t <- t %>% mutate(across(everything(), round, 3))

  corplot <- ggcorrplot(t,
             show.legend = FALSE,
             show.diag = T,
             type = "lower",
             colors= c('white'),
             lab = T) + 
    theme_bw() +
    labs(x = '', y = '')
    
 
  # t <- pretest %>% left_join(df, by = 'participant')
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
    scale_y_log10() + 
    annotation_logticks(sides = 'l') + 
    labs(title = 'Crowding vs Grade',
         y = 'crowding (deg)')
  
  p2 = ggplot(data = rsvp_speed) +
    geom_point(aes(x = Grade, y = 10^(block_avg_log_WPM))) +
    theme_classic() + 
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    labs(title = 'Rsvp vs Grade',
         y = 'rsvp reading speed (word/min)')
  
  p3 = ggplot(data = acuity) +
    geom_point(aes(x = Grade, y = 10^(log_acuity))) +
    theme_classic() +
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    labs(title = 'Acuity vs Grade',
         y = 'acuity (deg)')

  return(list(
    p1,
    p2,
    p3,
    corplot
  ))
}

plot_rsvp_crowding <- function(allData) {
  # add R and coefficient
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  foveal <- crowding %>% filter(grepl('foveal', conditionName,ignore.case = T))
  peripheral <- crowding %>% filter(grepl('peripheral', conditionName,ignore.case = T))
  if (nrow(rsvp_speed) == 0) {
    p1 <- ggplot() + 
      labs(x = 'crowding distance (deg)',
                        y = 'rsvp reading (word/min)',
                        title='rsvp vs peripheral crowding')
    p2 <- ggplot() + 
      labs(x = 'crowding distance degree',
           y = 'rsvp reading (word/min)',
           title='rsvp vs foveal crowding')
    return(list(p1,p2))
  }
  peripheral_rsvp <- peripheral %>%
    select(participant, log_crowding_distance_deg) %>%
    left_join(rsvp_speed)
  
  p1 <- ggplot(data = peripheral_rsvp, aes(x = 10^(log_crowding_distance_deg), y = 10^(block_avg_log_WPM))) +
    geom_point() +
    theme_classic() +
    scale_y_log10() +
    scale_x_log10() + 
    geom_smooth(method = 'lm', se = F) + 
    annotation_logticks() +
    coord_fixed(ratio = 1) + 
    labs(x = 'crowding distance (deg)',
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
    geom_smooth(method = 'lm', se = F) + 
    coord_fixed(ratio = 1) + 
    labs(x = 'crowding distance (deg)', 
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

