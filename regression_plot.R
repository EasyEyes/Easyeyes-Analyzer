regression_plot <- function(df_list){
  reading <- df_list[[1]]
  crowding <- df_list[[2]]
  rsvp_speed <- df_list[[3]]
  crowding_vs_rsvp <- merge(crowding,rsvp_speed, by = c("participant", "font"))
  reading_each <- reading %>% 
    group_by(font, participant, block_condition, thresholdParameter) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)), .groups = "keep") %>% 
    ungroup()
  
  reading_exceed_1500 <- reading_each %>% 
    filter(avg_wordPerMin > 1500) %>% 
    mutate(warning =  paste("Participant:",
                            participant,
                            "reading speeds removed due to excessive max speed",
                            round(avg_wordPerMin,2),
                            "> 1500 word/min",
                            sep = " "))
  reading_valid <- reading_each %>% 
    filter(!participant %in% reading_exceed_1500$participant) %>% 
    mutate(targetKind = "reading")
  
  crowding_summary <- crowding %>% 
    group_by(participant, font) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg))
  
  reading_crowding <- reading_valid %>% 
    select(participant, font, avg_wordPerMin) %>% 
    group_by(participant, font) %>% 
    summarize(avg_log_WPM = mean(log10(avg_wordPerMin))) %>% 
    ungroup() %>% 
    left_join(crowding_summary, by = c("participant", "font")) %>% 
    mutate(targetKind = "reading")
  crowding_vs_rsvp_summary <- crowding_vs_rsvp %>% 
    group_by(participant, font) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg), avg_log_WPM = mean(block_avg_log_WPM)) %>% 
    mutate(targetKind = "RsvpReading")
  
  t <- rbind(crowding_vs_rsvp_summary, reading_crowding)
  corr <- t %>% group_by(targetKind) %>% 
     summarize(correlation = round(cor(log_crowding_distance_deg,avg_log_WPM),2))
   t <- t %>% left_join(corr, by = "targetKind") %>% mutate(targetKind = paste0(targetKind, ", R =  ", correlation))
  
  # plot for the regression
  p <- ggplot(t,aes(x = 10^(log_crowding_distance_deg), y = 10^(avg_log_WPM), color = targetKind)) + 
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_x_log10() + 
    scale_y_log10() +
    labs(x="crowding distance", y = "reading speed (word/min)") +
    theme_bw() + 
    annotation_logticks() +
    guides(color=guide_legend(title="Regression and correlation"))
  return(p)
}