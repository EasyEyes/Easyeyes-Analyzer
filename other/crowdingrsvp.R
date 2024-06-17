get_info <- function(data_list){
  df <- tibble()
  for (i in 1:length(data_list)) {
    if (!'ParticipantCode' %in% names(data_list[[i]])) {
      data_list[[i]]$ParticipantCode = ''
    }
    if (!'participant' %in% names(data_list[[i]])) {
      data_list[[i]]$participant = ''
    }
    if (!'Birthdate' %in% names(data_list[[i]])) {
      data_list[[i]]$Birthdate = ''
    }
    unique_participantCode = unique(data_list[[i]]$ParticipantCode)
    if (length(unique_participantCode) > 1) {
      data_list[[i]]$ParticipantCode = unique(data_list[[i]]$ParticipantCode[!is.na(data_list[[i]]$ParticipantCode)])
    } else {
      data_list[[i]]$ParticipantCode = ''
    }
    
    unique_Birthdate = unique(data_list[[i]]$Birthdate)
    if (length(unique_Birthdate) > 1) {
      data_list[[i]]$Birthdate = unique(data_list[[i]]$Birthdate[!is.na(data_list[[i]]$Birthdate)])
    } else {
      data_list[[i]]$Birthdate = ''
    }
   
    df <- rbind(df, data_list[[i]] %>% distinct(participant, ParticipantCode, Birthdate))
    
  }
  return(df %>% filter(!ParticipantCode == '',
                       !Birthdate == ''))
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
