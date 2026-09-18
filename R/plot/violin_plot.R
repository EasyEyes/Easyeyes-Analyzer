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
  reading = df_list$reading %>%
    mutate(log_WPM = suppressWarnings(as.numeric(log_WPM))) %>%
    filter(!is.na(log_WPM), is.finite(log_WPM)) %>%
    mutate(y = 10^log_WPM)
  
  # Debug reading data
  # print("Reading data for violin plot:")
  # print(paste("Number of reading rows:", nrow(reading)))
  # print("Reading y values summary:")
  # print(summary(reading$y))
  # print("Any NA values in reading y:")
  # print(sum(is.na(reading$y)))
  # Linear acuity (deg), same transform as font-comparison bar plot
  acuity = df_list$acuity %>% mutate(y = 10^questMeanAtEndOfTrialsLoop)
  beauty = df_list$beauty %>%
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  comfort = df_list$comfort %>% 
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  familiarity_data = df_list$familiarity %>% 
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  create_plot <- function(data, ylabel, title, xlimits = NULL,
                          abbreviate_fonts = FALSE,
                          use_log_scale = FALSE,
                          axis_label_scale = 1) {
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
        mutate(
          font_display = if (abbreviate_fonts) strip_font_filetype(font) else font,
          label = paste0(font_display, "\n(N=", n_participants, ")"),
          font_factor = factor(font, levels = font_order)
        ) %>%
        arrange(font_factor)
      
      # Update data with new labels and filter out infinite values
      plot_data <- data %>%
        left_join(font_labels, by = "font") %>%
        mutate(font_label = factor(label, levels = font_labels$label)) %>%
        filter(is.finite(y))  # Remove -Inf, Inf, NA values for plotting

      if (use_log_scale) {
        plot_data <- plot_data %>% filter(y > 0)
      }
      
      # Apply x-axis limits if specified (filter data to limits)
      if (!is.null(xlimits)) {
        plot_data <- plot_data %>%
          filter(y >= xlimits[1] & y <= xlimits[2])
      }
      
      # Calculate means by font for mean lines (already filtered for finite values and limits)
      mean_data <- plot_data %>%
        group_by(font_label) %>%
        summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

      axis_title_size <- 14 * axis_label_scale
      
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
          panel.background = element_rect(fill = "white", color = NA),
          axis.title.x = element_text(size = axis_title_size),
          axis.title.y = element_text(size = axis_title_size)
        ) +
        labs(
          subtitle = title,
          x = "Font",
          y = ylabel
        )
      
      # Log scale: linear tick labels with log spacing (matches font-comparison bars)
      if (use_log_scale || grepl("Reading|RSVP|Crowding", title)) {
        p <- p + scale_y_log10(breaks = scales::log_breaks()) +
          annotation_logticks(sides = "b",
                      short = unit(2, "pt"),
                      mid   = unit(2, "pt"),
                      long  = unit(7, "pt"))
      } else {
        # For non-log scale plots, add standard tick marks
        p <- p + theme(
          axis.ticks.x = element_line(color = "black", size = 0.5),
          axis.ticks.y = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(4, "pt")
        )
      }
    }
    return(p)
  }
  
  # Calculate dynamic limits based on actual data
  reading_limits <- if (nrow(reading) > 0 && !all(is.na(reading$y))) {
    c(min(reading$y, na.rm = TRUE) * 0.8, max(reading$y, na.rm = TRUE) * 1.2)
  } else {
    NULL  # Let ggplot auto-scale
  }
  
  rsvp_limits <- if (nrow(rsvp) > 0 && !all(is.na(rsvp$y))) {
    c(min(rsvp$y, na.rm = TRUE) * 0.8, max(rsvp$y, na.rm = TRUE) * 1.2)
  } else {
    NULL  # Let ggplot auto-scale
  }

  return(list(
    reading = create_plot(reading, "Reading Speed (word/min)", "Reading Speed by Font"),
    rsvp = create_plot(rsvp, "RSVP Reading Speed (word/min)", "RSVP Reading Speed by Font"),
    crowding = create_plot(crowding, "Crowding Distance (deg)", "Crowding Threshold by Font"),
    acuity = create_plot(acuity, "Acuity (deg)", "Acuity vs. font",
                         abbreviate_fonts = TRUE,
                         use_log_scale = TRUE,
                         axis_label_scale = 1.4),
    beauty = create_plot(beauty, "Beauty Rating", "Beauty Rating by Font"),
    cmfrt = create_plot(comfort, "Comfort Rating", "Comfort Rating by Font"),
    familiarity = create_plot(familiarity_data, "familiarity", "Familiarity by Font")
  ))
}