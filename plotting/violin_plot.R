plot_violins <- function(df_list) {
  crowding = df_list$crowding %>% mutate(y = log_crowding_distance_deg)
  rsvp = df_list$rsvp %>% mutate(y = block_avg_log_WPM)
  reading = df_list$reading %>% mutate(y = log_WPM)
  
  # Debug reading data
  print("Reading data for violin plot:")
  print(paste("Number of reading rows:", nrow(reading)))
  print("Reading y values summary:")
  print(summary(reading$y))
  print("Any NA values in reading y:")
  print(sum(is.na(reading$y)))
  acuity = df_list$acuity %>% mutate(y = questMeanAtEndOfTrialsLoop)
  beauty = df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           )) %>% 
    filter(!is.na(y))
  
  cmfrt = df_list$QA %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           ))
  print("inside plot_violins")
  create_plot <- function(data, ylabel, title) {
    p <- NULL
    print(paste("Creating plot for:", title))
    print(paste("Data rows:", nrow(data)))
    
    if (nrow(data) > 0) {
      # Debug: Check if this is reading data and print more details
      if (grepl("Reading", title)) {
        print("Reading plot debug:")
        print("Font counts:")
        print(table(data$font))
        print("Y value summary:")
        print(summary(data$y))
        print("Any infinite values:")
        print(sum(is.infinite(data$y)))
      }
      # Calculate participant count by font
      participant_counts <- data %>%
        group_by(font) %>%
        summarise(n_participants = n_distinct(participant), .groups = "drop")
      
      # Create labels with N counts for each font
      font_labels <- participant_counts %>%
        mutate(label = paste0(font, "\n(N=", n_participants, ")"))
      
      # Update data with new labels and filter out infinite values
      plot_data <- data %>%
        left_join(font_labels, by = "font") %>%
        mutate(font_label = factor(label, levels = font_labels$label)) %>%
        filter(is.finite(y))  # Remove -Inf, Inf, NA values for plotting
      
      # Calculate means by font for mean lines (already filtered for finite values)
      mean_data <- plot_data %>%
        group_by(font_label) %>%
        summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")
      
      # Debug mean calculation for reading plot
      if (grepl("Reading", title)) {
        print("Mean data for reading plot:")
        print(mean_data)
        print("Any NA means:")
        print(sum(is.na(mean_data$mean_y)))
      }
      
      p <- ggplot(plot_data, aes(x = font_label, y = y)) +
        geom_violin(trim = FALSE, alpha = 0.5) +
        geom_jitter(width = 0.15, alpha = 0.7) +
        geom_segment(data = mean_data, 
                     aes(x = as.numeric(font_label) - 0.4, 
                         xend = as.numeric(font_label) + 0.4,
                         y = mean_y, 
                         yend = mean_y),
                     color = "red", size = 1, alpha = 0.8) +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          subtitle = title,
          x = "Font",
          y = ylabel
        )
    }
    return(p)
  }
  
  return(list(
    reading = create_plot(reading, "Log Reading Speed(word/min)", "Log Reading Speed by Font"),
    rsvp = create_plot(rsvp, "Log RSVP Reading Speed(word/min)", "Log RSVP Reading Speed by Font"),
    crowding = create_plot(crowding, "Log Crowding Distance (deg)", "Crowding Threshold by Font"),
    acuity = create_plot(acuity, "Log acuity (deg)", "Acuity Threshold by Font"),
    beauty = create_plot(beauty, "Beauty Rating", "Beauty Rating by Font"),
    cmfrt = create_plot(cmfrt, "Comfort Rating", "Comfort Rating by Font")
  ))
}