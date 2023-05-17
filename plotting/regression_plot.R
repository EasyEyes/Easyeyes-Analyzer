library(dplyr)
prepare_regression_data <- function(df_list){
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
    summarize(bouma_factor = 10^(mean(log10(bouma_factor))))
  
  reading_crowding <- reading_valid %>% 
    select(participant, font, avg_wordPerMin) %>% 
    group_by(participant, font) %>% 
    summarize(avg_log_WPM = mean(log10(avg_wordPerMin))) %>% 
    ungroup() %>% 
    left_join(crowding_summary, by = c("participant", "font")) %>% 
    mutate(targetKind = "reading")
  crowding_vs_rsvp_summary <- crowding_vs_rsvp %>% 
    group_by(participant, font) %>% 
    summarize(bouma_factor = 10^(mean(log10(bouma_factor))), avg_log_WPM = mean(block_avg_log_WPM)) %>% 
    mutate(targetKind = "rsvpReading")
  
  t <- rbind(crowding_vs_rsvp_summary, reading_crowding)
  corr <- t %>% group_by(targetKind) %>% 
    summarize(correlation = round(cor(bouma_factor,avg_log_WPM, use = "pairwise.complete.obs"),2))
  t <- t %>% left_join(corr, by = "targetKind") %>% mutate(targetKind = paste0(targetKind, ", R =  ", correlation))
  return(list(t,corr))
}
regression_plot <- function(df_list){
  t <- prepare_regression_data(df_list)[[1]]
  # plot for the regression
  p <- ggplot(t,aes(x = bouma_factor, y = 10^(avg_log_WPM), color = targetKind)) + 
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_x_log10() + 
    scale_y_log10() +
    coord_fixed(ratio = 1) + 
    labs(x="Bouma factor", y = "Reading (word/min)") +
    theme_bw() + 
    annotation_logticks() +
    # annotate("text", x = 10^(max(t$bouma_factor)), 
    #          y = 10^(min(t$avg_log_WPM)), 
    #          label = paste("italic(n)==", n_distinct(t$participant)), 
    #          parse = TRUE) +
    guides(color=guide_legend(title="targetKind and correlation"))
  return(p)
}

regression_and_mean_plot <- function(df_list, reading_rsvp_crowding_df){
  t <- prepare_regression_data(df_list)[[1]]
  corr <- prepare_regression_data(df_list)[[2]]
  rsvp_vs_ordinary_vs_crowding <- reading_rsvp_crowding_df[[1]]
  rsvp_vs_ordinary_vs_crowding <-  rsvp_vs_ordinary_vs_crowding %>% 
    left_join(corr, by = "targetKind") %>% 
    mutate(targetKind = paste0(targetKind, ", R =  ", correlation))
  
  # regression and font mean
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, aes(x = mean_bouma_factor, 
                                                       y = 10^(avg_log_SpeedWPM), 
                                                       color = targetKind)) + 
    # geom_errorbar(aes(ymin=10^(avg_log_SpeedWPM-se),
    #                   ymax=10^(avg_log_SpeedWPM+se)), width=0) +
    # geom_errorbar(aes(xmin=(mean_bouma_factor-se_bouma_factor),
    #                   xmax=(mean_bouma_factor+se_bouma_factor)), width=0) +
    geom_point(aes(shape = font),size = 5) +
    geom_point(data = t, aes(x = bouma_factor, y = 10^(avg_log_WPM), color = targetKind),alpha = 0.5) +
    geom_smooth(data = t, aes(x = bouma_factor, y = 10^(avg_log_WPM), color = targetKind),method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10() +
    scale_x_log10() + 
    coord_fixed(ratio = 1) +
    labs(x="Bouma factor", y = "Reading speed (word/min)") +
    theme_bw() + 
    annotation_logticks() +
    guides(color=guide_legend(title="targetKind, Correlation"))
  return(p)
}