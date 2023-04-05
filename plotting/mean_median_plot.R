library(ggplot2)
get_mean_median_df <- function(df_list){
  reading <- df_list[[1]]
  crowding <- df_list[[2]]
  rsvp_speed <- df_list[[3]]
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
  for (i in 1 : nrow(reading_exceed_1500)) {
    warning(paste("Participant:",
                  reading_exceed_1500$participant[i],
                  "reading speeds removed due to excessive max speed",
                  round(reading_exceed_1500$avg_wordPerMin[i],2),
                  "> 1500 word/min",
                  sep = " "))
  }
  reading_valid <- reading_each %>% 
    filter(!participant %in% reading_exceed_1500$participant) %>% 
    mutate(targetKind = "reading")
  
  reading_byfont <- reading_valid %>% 
    group_by(font) %>% 
    summarise(avg_log_SpeedWPM = mean(log10(avg_wordPerMin), na.rm = T), 
              median_log_SpeedWPM = median(log10(avg_wordPerMin), na.rm = T),
              se = sd(log10(avg_wordPerMin), na.rm = T)/sqrt(n()))
  # average log crowding distance degree by font
  crowding_byfont <- crowding%>% 
    group_by(font) %>% 
    summarize(avg_log_crowding = mean(log_crowding_distance_deg, na.rm = T),
              median_log_crowding = median(log_crowding_distance_deg, na.rm = T),
              se_crowding = sd(log_crowding_distance_deg, na.rm = T)/sqrt(n()))
  # average log rsvp reading wpm by font
  rsvp_byfont <- rsvp_speed%>% 
    group_by(font) %>% 
    summarize(avg_log_SpeedWPM = mean(block_avg_log_WPM),
              median_log_SpeedWPM = median(block_avg_log_WPM),
              se = sd(block_avg_log_WPM)/sqrt(n()))
  reading_vs_crowding <- merge(reading_byfont,crowding_byfont, by = "font") %>% 
    mutate(targetKind = "reading") 
  
  RSVP_vs_crowding <- merge(rsvp_byfont,crowding_byfont, by = "font") %>% mutate(targetKind = "rsvpReading")
  rsvp_vs_ordinary_vs_crowding <- rbind(reading_vs_crowding,RSVP_vs_crowding)
  
  N_reading <- length(unique(reading_valid$participant))
  N_rsvp <- length(unique(RSVP_vs_crowding$participant))
  if (N_reading == N_rsvp) {
    N_text = paste0(" = ", N_reading)
  } else {
    N_text = paste0(" = ", min(N_reading, N_rsvp), " to ", max(N_reading, N_rsvp))
  }
  return(list(rsvp_vs_ordinary_vs_crowding, N_text))
}

mean_plot <- function(reaing_rsvp_crowding_df){
  rsvp_vs_ordinary_vs_crowding <- reaing_rsvp_crowding_df[[1]]
  N_text <- reaing_rsvp_crowding_df[[2]]
  
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, 
         aes(x = 10^(avg_log_crowding), 
             y = 10^(avg_log_SpeedWPM), 
             color = targetKind
         )) +
    geom_point(aes(shape = font), size = 3) + 
    scale_y_log10() +
    scale_x_log10() + 
    geom_errorbar(aes(ymin=10^(avg_log_SpeedWPM-se), ymax=10^(avg_log_SpeedWPM+se)), width=0) +
    geom_errorbar(aes(xmin=10^(avg_log_crowding-se_crowding), xmax=10^(avg_log_crowding+se_crowding)), width=0) +
    theme_bw() + 
    labs(x = "Crowding distance (deg)", y = "Reading speed (word/min)") +
    annotation_logticks(short = unit(0.1, "cm"),                                                                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    scale_shape_manual(values = sample(0:25, length(unique(rsvp_vs_ordinary_vs_crowding$font)), F))
  return(p)
}

median_plot <- function(reaing_rsvp_crowding_df){
  rsvp_vs_ordinary_vs_crowding <- reaing_rsvp_crowding_df[[1]]
  N_text <- reaing_rsvp_crowding_df[[2]]
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, 
         aes(x = 10^(median_log_crowding), 
             y = 10^(median_log_SpeedWPM), 
             color = targetKind)) +
    geom_point(aes(shape = font), size = 3) + 
    scale_y_log10() +
    scale_x_log10() + 
    theme_bw() + 
    labs(x = "Crowding distance (deg)", y = "Reading speed (word/min)") +
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) +
    scale_shape_manual(values = sample(0:25, length(unique(rsvp_vs_ordinary_vs_crowding$font)), F))
  return(p)
}
