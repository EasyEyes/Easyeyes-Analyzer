library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Function to calculate geometric mean and standard error
geometric_mean_se <- function(x) {
  x <- x[!is.na(x) & x > 0]  # Remove NA and non-positive values
  if (length(x) == 0) return(c(mean = NA, se_lower = NA, se_upper = NA))
  
  log_x <- log(x)
  log_mean <- mean(log_x)
  log_se <- sd(log_x) / sqrt(length(log_x))
  
  # Calculate geometric mean and asymmetric error bars
  geom_mean <- exp(log_mean)
  se_lower <- geom_mean - exp(log_mean - log_se)
  se_upper <- exp(log_mean + log_se) - geom_mean
  
  return(c(
    mean = geom_mean,
    se_lower = se_lower,
    se_upper = se_upper
  ))
}

# Function to calculate ordinary mean and standard error
ordinary_mean_se <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(mean = NA, se = NA))
  
  return(c(
    mean = mean(x),
    se = sd(x) / sqrt(length(x))
  ))
}

# Main function to create font comparison plots
plot_font_comparison <- function(df_list) {
  # Extract data for each measure
  crowding_data <- df_list$crowding %>%
    mutate(measure = 10^log_crowding_distance_deg) %>%  # Convert from log to linear
    select(participant, font, measure)
  
    rsvp_data <- df_list$rsvp %>%
    mutate(measure = 10^block_avg_log_WPM) %>%  # Convert from log to linear
    select(participant, font, measure)
  
  reading_data <- df_list$reading %>%
    mutate(measure = wordPerMin) %>%  # Already in linear scale
    select(participant, font, measure)
  
  acuity_data <- df_list$acuity %>%
    mutate(measure = 10^questMeanAtEndOfTrialsLoop) %>%  # Convert from log to linear
    select(participant, font, measure)
 
  comfort_data <- df_list$QA %>%
    filter(grepl('CMFRT', questionAndAnswerNickname)) %>%
    mutate(measure = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ questionAndAnswerNickname  # fallback for any unmatched cases
           )) %>%
    filter(!is.na(measure)) %>%
    select(participant, font, measure)
  
  beauty_data <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(measure = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ conditionName  # fallback for any unmatched cases
           )) %>% 
    filter(!is.na(measure)) %>%
    select(participant, font, measure)
  
  # Function to create individual plots
  create_font_plot <- function(data, title, ylabel, use_log_scale = TRUE, use_geometric_mean = TRUE, y_limits = NULL) {
    if (nrow(data) == 0) return(NULL)
    
    # Calculate means and standard errors by font
    if (use_geometric_mean) {
      summary_data <- data %>%
        group_by(font) %>%
        summarise(
          stats = list(geometric_mean_se(measure)),
          .groups = "drop"
        ) %>%
        mutate(
          mean_val = map_dbl(stats, ~ .x["mean"]),
          se_lower = map_dbl(stats, ~ .x["se_lower"]),
          se_upper = map_dbl(stats, ~ .x["se_upper"])
        ) %>%
        select(-stats)
    } else {
      summary_data <- data %>%
        group_by(font) %>%
        summarise(
          stats = list(ordinary_mean_se(measure)),
          .groups = "drop"
        ) %>%
        mutate(
          mean_val = map_dbl(stats, ~ .x["mean"]),
          se_val = map_dbl(stats, ~ .x["se"]),
          se_lower = se_val,
          se_upper = se_val
        ) %>%
        select(-stats, -se_val)
    }
    
    # Filter out invalid data
    summary_data <- summary_data %>%
      filter(!is.na(mean_val), !is.na(se_lower), !is.na(se_upper), mean_val > 0)
    
    if (nrow(summary_data) == 0) return(ggplot())
    
    # Calculate participant count by font
    participant_counts <- data %>%
      group_by(font) %>%
      summarise(n_participants = n_distinct(participant), .groups = "drop")
    
    # Add participant counts to summary data
    summary_data <- summary_data %>%
      left_join(participant_counts, by = "font")
    
    # Define font order (corrected file extensions)
    font_order <- c(
      "Al-Awwal-Regular.ttf",
      "majalla.ttf", 
      "Saudi-Regular.ttf",
      "SaudiTextv1-Regular.otf",  # Fixed: .otf not .ttf
      "SaudiTextv2-Regular.otf",  # Fixed: .otf not .ttf
      "SaudiTextv3-Regular.otf"   # Fixed: .otf not .ttf
    )
    
    # Sort fonts by specified order
    summary_data$font <- factor(summary_data$font, levels = font_order)
    
    # Get consistent font colors
    font_colors <- font_color_palette(unique(summary_data$font))
    

    # Create the plot with colored bars using font color palette
    p <- ggplot(summary_data, aes(x = font, fill = font)) +
      geom_rect(
        aes(
          xmin = as.numeric(factor(font)) - 0.2,  # control bar width
          xmax = as.numeric(factor(font)) + 0.2,
          ymin = y_limits[1],                     # custom baseline
          ymax = mean_val
        ),
        alpha = 0.8
      ) +
      geom_errorbar(
        aes(ymin = pmax(mean_val - se_lower, 0.001), ymax = mean_val + se_upper),
        width = 0.15, size = 0.5, color = "black"
      ) +
      geom_text(
        aes(label = paste0("N=", n_participants), 
            y = y_limits[1] * 1.1), 
        vjust = 0.5, hjust = 0.5, size = 4, color = "black", fontface = "bold"
      ) +
      scale_fill_manual(values = font_colors, guide = "none") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 9, color = "black", angle = 45, hjust = 1, vjust = 1),
        axis.ticks.x = element_line(color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10), color = "black"),
        axis.title.y = element_text(size = 12, margin = margin(r = 10), color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", size = 0.3),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black", size = 0.5)
      ) +
      labs(
        subtitle = title,
        x = "Fonts",
        y = ylabel
      )
    
    # Apply log scale if requested
    if (use_log_scale && use_geometric_mean) {
      p <- p + scale_y_log10(limits = y_limits,expand = c(0,0)) 
      p <- p + annotation_logticks(sides = "l",
                        short = unit(2, "pt"),
                        mid   = unit(2, "pt"),
                        long  = unit(7, "pt")) 
    } else if (!is.null(y_limits)) {
      p <- p +
        scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = y_limits)
    } else {
      p <- p +
        scale_y_continuous(expand = c(0,0)) + coord_cartesian(ylim = c(0, max(summary_data$mean_val) * 1.5))
    }
    
    return(p)
  }
  
  # Create individual plots with appropriate titles and y-axis labels
 
  
  plots <- list(
    rsvp = create_font_plot(rsvp_data, "RSVP", "RSVP Reading Speed (WPM)", use_log_scale = TRUE, use_geometric_mean = TRUE, y_limits = c(500, 1500)),
    crowding = create_font_plot(crowding_data, "Crowding", "Crowding Distance (deg)", use_log_scale = TRUE, use_geometric_mean = TRUE, y_limits = c(0.5,2.5)),
    reading = create_font_plot(reading_data, "Reading", "Ordinary Reading Speed (WPM)", use_log_scale = TRUE, use_geometric_mean = TRUE, y_limits = c(100, 200)),
    acuity = create_font_plot(acuity_data, "Acuity", "Acuity Threshold (deg)", use_log_scale = TRUE, use_geometric_mean = TRUE, y_limits = c(0.5, 2.0)),
    comfort = create_font_plot(comfort_data, "Comfort", "Comfort Rating", use_log_scale = FALSE, use_geometric_mean = FALSE, y_limits = c(3, 6)),
    beauty = create_font_plot(beauty_data, "Beauty", "Beauty Rating", use_log_scale = FALSE, use_geometric_mean = FALSE, y_limits = c(3, 6))
  )
  
 
  
  return(plots)
}


