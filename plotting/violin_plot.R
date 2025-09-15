# Helper function for logarithmic jitter (unbiased for log scales)
# NOTE: geom_jitter in violin plots uses mainly x-axis jitter, but when
# y-axis uses log scale, any y-jitter should ideally be logarithmic
add_log_jitter <- function(values, jitter_percent = 1, seed = 42) {
  # Apply logarithmic jitter for unbiased results on log scales
  # jitter_percent: percentage jitter (e.g., 1 for ±1%)
  set.seed(seed)
  log_max <- log10(1 + jitter_percent/100)
  log_min <- -log_max
  log_factor <- log_min + runif(length(values)) * (log_max - log_min)
  return(values * 10^log_factor)
}

plot_violins <- function(df_list) {
  crowding = df_list$crowding %>% mutate(y = 10^log_crowding_distance_deg)
  rsvp = df_list$rsvp %>% mutate(y = 10^block_avg_log_WPM)
  reading = df_list$reading %>% mutate(y = 10^log_WPM)
  
  # Debug reading data
  # print("Reading data for violin plot:")
  # print(paste("Number of reading rows:", nrow(reading)))
  # print("Reading y values summary:")
  # print(summary(reading$y))
  # print("Any NA values in reading y:")
  # print(sum(is.na(reading$y)))
  acuity = df_list$acuity %>% mutate(y = questMeanAtEndOfTrialsLoop)
  beauty = df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
           )) %>% 
    filter(!is.na(y))
  
  cmfrt = df_list$QA %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
           ))
  print("inside plot_violins")
  create_plot <- function(data, ylabel, title, xlimits = NULL) {
    p <- NULL
    
    if (nrow(data) > 0) {

      # Define font order
      font_order <- c(
        "Al-Awwal-Regular.ttf",
        "majalla.ttf",
        "Saudi-Regular.ttf",
        "SaudiTextv1-Regular.otf",
        "SaudiTextv2-Regular.otf",
        "SaudiTextv3-Regular.otf"
      )
      
      # Calculate participant count by font
      participant_counts <- data %>%
        group_by(font) %>%
        summarise(n_participants = n_distinct(participant), .groups = "drop")
      
      # Create labels with N counts for each font and apply ordering
      font_labels <- participant_counts %>%
        mutate(label = paste0(font, "\n(N=", n_participants, ")"),
               font_factor = factor(font, levels = font_order)) %>%
        arrange(font_factor)
      
      # Update data with new labels and filter out infinite values
      plot_data <- data %>%
        left_join(font_labels, by = "font") %>%
        mutate(font_label = factor(label, levels = font_labels$label)) %>%
        filter(is.finite(y))  # Remove -Inf, Inf, NA values for plotting
      
      # Apply x-axis limits if specified (filter data to limits)
      if (!is.null(xlimits)) {
        plot_data <- plot_data %>%
          filter(y >= xlimits[1] & y <= xlimits[2])
      }
      
      # Calculate means by font for mean lines (already filtered for finite values and limits)
      mean_data <- plot_data %>%
        group_by(font_label) %>%
        summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")
      
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
        theme(
          plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA)
        ) +
        labs(
          subtitle = title,
          x = "Font",
          y = ylabel
        )
      
      # Apply log scaling to y-axis for specific plot types (becomes x-axis after coord_flip)
      if (grepl("Reading|RSVP|Crowding", title)) {
        print("Applying log scaling to y-axis")
        print(paste("Plot type:", title))
        p <- p + scale_y_log10(breaks = scales::log_breaks()) +
          annotation_logticks(sides = "b",
                      short = unit(2, "pt"),
                      mid   = unit(2, "pt"),
                      long  = unit(7, "pt"))
        
        # Apply x-axis limits if specified (for log scale plots)
        if (!is.null(xlimits)) {
          p <- p + coord_flip(ylim = xlimits)
        }
      } else {
        # For non-log scale plots, add standard tick marks
        p <- p + theme(
          axis.ticks.x = element_line(color = "black", size = 0.5),
          axis.ticks.y = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(4, "pt")
        )
        
        # Apply x-axis limits if specified (for non-log scale plots)
        if (!is.null(xlimits)) {
          p <- p + coord_flip(ylim = xlimits)
        }
      }
    }
    return(p)
  }
  
  return(list(
    reading = create_plot(reading, "Reading Speed (word/min)", "Reading Speed by Font", xlimits = c(25, 1000)),
    rsvp = create_plot(rsvp, "RSVP Reading Speed (word/min)", "RSVP Reading Speed by Font", xlimits = c(100, 4000)),
    crowding = create_plot(crowding, "Crowding Distance (deg)", "Crowding Threshold by Font"),
    acuity = create_plot(acuity, "acuity (deg)", "Acuity Threshold by Font"),
    beauty = create_plot(beauty, "Beauty Rating", "Beauty Rating by Font"),
    cmfrt = create_plot(cmfrt, "Comfort Rating", "Comfort Rating by Font")
  ))
}