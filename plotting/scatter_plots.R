#### scatter plots ####
reading_speed_vs_retention <- function(reading){
  #TODO
  t <- reading %>% group_by(participant,
                       block_condition, 
                       conditionName,
                       font,
                       accuracy) %>% 
    summarize(wordPerMin = 10^(mean(log_WPM)), .groups = "drop")
  ggplot(t) + 
    geom_point(aes(x = accuracy, y = wordPerMin)) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    scale_y_log10() + 
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=16),
          plot.subtitle = element_text(size=16)) +
    xlab("Reading retention (proportion correct)") +
    ylab("Reading speed (w/min)")
}

#### New scatter plots for beauty/comfort vs crowding ####

# Scatter plot: Comfort vs Crowding
comfort_vs_crowding_scatter <- function(df_list) {
  # Get comfort data from QA
  comfort_data <- df_list$QA %>%
    filter(grepl('CMFRT', questionAndAnswerNickname)) %>%
    mutate(comfort_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            # Fix early data typos - map old nicknames to correct fonts
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ questionAndAnswerNickname)) %>%
    filter(!is.na(comfort_rating)) %>%
    group_by(participant, font) %>%
    summarize(comfort_rating = mean(comfort_rating), .groups = "drop")
  
  # Get crowding data (average across conditions for each participant-font combination)
  # Standardize font names to match beauty/comfort mapping
  crowding_data <- df_list$crowding %>%
    mutate(
      crowding_distance = 10^log_crowding_distance_deg
    ) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = mean(crowding_distance, na.rm = TRUE), .groups = "drop")
  
  # Debug: Check font availability
  print(paste("Comfort fonts available:", paste(unique(comfort_data$font), collapse = ", ")))
  print(paste("Crowding fonts available:", paste(unique(crowding_data$font), collapse = ", ")))
  
  # Join comfort and crowding data
  combined_data <- comfort_data %>%
    inner_join(crowding_data, by = c("participant", "font")) %>%
    filter(!is.na(comfort_rating), !is.na(crowding_distance))
  
  print(paste("Combined fonts after join:", paste(unique(combined_data$font), collapse = ", ")))
  
  if (nrow(combined_data) == 0) {
    return(ggplot() + 
           labs(subtitle = "Comfort vs crowding", 
                x = "Crowding Distance (deg)", 
                y = "Comfort Rating"))
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$crowding_distance, combined_data$comfort_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create the plot
  ggplot(combined_data, aes(x = crowding_distance, y = comfort_rating, color = font)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_log10() +
    annotation_logticks(sides = "b", 
                        short = unit(2, "pt"), 
                        mid = unit(2, "pt"), 
                        long = unit(7, "pt")) +
    annotate("text", x = min(combined_data$crowding_distance) * 1.1, 
             y = max(combined_data$comfort_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Comfort vs crowding",
         x = "Crowding Distance (deg)",
         y = "Comfort Rating") +
    guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Crowding  
beauty_vs_crowding_scatter <- function(df_list) {
  # Get beauty data from QA
  beauty_data <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(beauty_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ conditionName)) %>%
    filter(!is.na(beauty_rating)) %>%
    group_by(participant, font) %>%
    summarize(beauty_rating = mean(beauty_rating), .groups = "drop")
  
  # Get crowding data (average across conditions for each participant-font combination)
  # Standardize font names to match beauty/comfort mapping
  crowding_data <- df_list$crowding %>%
    mutate(
      crowding_distance = 10^log_crowding_distance_deg
    ) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = mean(crowding_distance, na.rm = TRUE), .groups = "drop")
  
  # Debug: Check font availability
  print(paste("Beauty fonts available:", paste(unique(beauty_data$font), collapse = ", ")))
  print(paste("Crowding fonts available:", paste(unique(crowding_data$font), collapse = ", ")))
  
  # Join beauty and crowding data
  combined_data <- beauty_data %>%
    inner_join(crowding_data, by = c("participant", "font")) %>%
    filter(!is.na(beauty_rating), !is.na(crowding_distance))
  
  print(paste("Combined fonts after join:", paste(unique(combined_data$font), collapse = ", ")))
  
  if (nrow(combined_data) == 0) {
    return(ggplot() + 
           labs(subtitle = "Beauty vs crowding", 
                x = "Crowding Distance (deg)", 
                y = "Beauty Rating"))
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$crowding_distance, combined_data$beauty_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create the plot
  ggplot(combined_data, aes(x = crowding_distance, y = beauty_rating, color = font)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_log10() +
    annotation_logticks(sides = "b", 
                        short = unit(2, "pt"), 
                        mid = unit(2, "pt"), 
                        long = unit(7, "pt")) +
    annotate("text", x = min(combined_data$crowding_distance) * 1.1, 
             y = max(combined_data$beauty_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Beauty vs Crowding",
         x = "Crowding Distance (deg)",
         y = "Beauty Rating") +
    guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Comfort
beauty_vs_comfort_scatter <- function(df_list) {
  # Get beauty data from QA
  beauty_data <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(beauty_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ conditionName)) %>%
    filter(!is.na(beauty_rating)) %>%
    group_by(participant, font) %>%
    summarize(beauty_rating = mean(beauty_rating), .groups = "drop")
  
  # Get comfort data from QA
  comfort_data <- df_list$QA %>%
    filter(grepl('CMFRT', questionAndAnswerNickname)) %>%
    mutate(comfort_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ questionAndAnswerNickname)) %>%
    filter(!is.na(comfort_rating)) %>%
    group_by(participant, font) %>%
    summarize(comfort_rating = mean(comfort_rating), .groups = "drop")
  
  # Join beauty and comfort data (only for common fonts)
  combined_data <- beauty_data %>%
    inner_join(comfort_data, by = c("participant", "font")) %>%
    filter(!is.na(beauty_rating), !is.na(comfort_rating))
  
  if (nrow(combined_data) == 0) {
    return(ggplot() + 
           labs(subtitle = "Beauty vs Comfort", 
                x = "Comfort Rating", 
                y = "Beauty Rating"))
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$comfort_rating, combined_data$beauty_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Create the plot
  ggplot(combined_data, aes(x = comfort_rating, y = beauty_rating, color = font)) +
    geom_point(size = 3, alpha = 0.7) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    annotate("text", x = min(combined_data$comfort_rating) * 1.1, 
             y = max(combined_data$beauty_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Beauty vs Comfort",
         x = "Comfort Rating",
         y = "Beauty Rating") +
    guides(color = guide_legend(title = "Font", ncol = 2))
}