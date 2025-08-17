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
  print("inside plot_font_comparison")
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
 
  comfort_data <- df_list$QA %>%
    filter(grepl('CMFRT', questionAndAnswerNickname)) %>%
    mutate(measure = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"Amareddine",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"Makdessi",
                            questionAndAnswerNickname=="CMFRTKafa" ~"Kafa",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.ttf",
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
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
                            TRUE ~ conditionName  # fallback for any unmatched cases
           )) %>% 
    filter(!is.na(measure)) %>%
    select(participant, font, measure)
  
  # Function to create individual plots
  create_font_plot <- function(data, title, ylabel, use_log_scale = TRUE, use_geometric_mean = TRUE) {
    if (nrow(data) == 0) return(ggplot())
    
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
    
    # Sort fonts alphabetically for consistent ordering
    summary_data$font <- factor(summary_data$font, levels = sort(unique(summary_data$font)))
    
    # Get consistent font colors
    font_colors <- font_color_palette(unique(summary_data$font))
    
    # Create the plot with colored bars using font color palette
    p <- ggplot(summary_data, aes(x = font, y = mean_val, fill = font)) +
      geom_col(width = 0.4, alpha = 0.8) +  # Made bars smaller (0.4 instead of 0.6)
      geom_errorbar(aes(ymin = pmax(mean_val - se_lower, 0.001), ymax = mean_val + se_upper),
                    width = 0.15, size = 0.5, color = "black") +  # Made error bars smaller too
      scale_fill_manual(values = font_colors, name = "Font", guide = guide_legend(ncol = 3)) +  # Use consistent font colors with 3 columns
      theme_minimal(base_size = 12) +
      theme(
        # Ensure white background and black text for export
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key = element_rect(fill = "white", color = NA),
        
        # Text colors - explicitly set to black
        axis.text.x = element_blank(),  # Remove font names from x-axis
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        axis.text.y = element_text(size = 10, color = "black"),
        legend.position = "top",  # Show legend at top
        legend.title = element_text(size = 10, color = "black"),
        legend.text = element_text(size = 6, color = "black"),
        plot.subtitle = element_text(size = 14, hjust = 0.5, margin = margin(b = 10), color = "black"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10), color = "black"),
        axis.title.y = element_text(size = 12, margin = margin(r = 10), color = "black"),
        
        # Grid and borders
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", size = 0.3),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(color = "black", size = 0.5)
      ) +
      labs(
        subtitle = title,
        x = "Fonts",
        y = ylabel  # Add y-axis label
      )
    
    # Apply log scale if requested
    if (use_log_scale && use_geometric_mean) {
      p <- p + scale_y_log10()
    }
    
    return(p)
  }
  
  # Create individual plots with appropriate titles and y-axis labels
  plots <- list(
    rsvp = create_font_plot(rsvp_data, "RSVP", "RSVP Reading Speed (WPM)", use_log_scale = TRUE, use_geometric_mean = TRUE),
    crowding = create_font_plot(crowding_data, "Crowding", "Crowding Distance (deg)", use_log_scale = TRUE, use_geometric_mean = TRUE),
    reading = create_font_plot(reading_data, "Reading", "Ordinary Reading Speed (WPM)", use_log_scale = TRUE, use_geometric_mean = TRUE),
    comfort = create_font_plot(comfort_data, "Comfort", "Comfort Rating", use_log_scale = FALSE, use_geometric_mean = FALSE),
    beauty = create_font_plot(beauty_data, "Beauty", "Beauty Rating", use_log_scale = FALSE, use_geometric_mean = FALSE)
  )
  
  return(plots)
}
