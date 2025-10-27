# Font-aggregated plot: Reading vs peripheral crowding (both reading and rsvpReading)
plot_font_aggregated_reading_rsvp_crowding <- function(allData) {
  reading <- allData$reading %>% 
    mutate(participant = tolower(participant))
  rsvp <- allData$rsvp %>% 
    mutate(participant = tolower(participant))
  crowding <- allData$crowding %>% 
    mutate(participant = tolower(participant))
  
  if (nrow(reading) == 0 & nrow(rsvp) == 0) {
    return(NULL)
  }
  if (nrow(crowding) == 0) {
    return(NULL)
  }
  
  # Get peripheral crowding data
  peripheral_crowding <- crowding %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = 10^(mean(log_crowding_distance_deg, na.rm = TRUE)),
              .groups = "drop")
  
  # Prepare reading data
  reading_data <- NULL
  if (nrow(reading) > 0) {
    reading_data <- reading %>%
      group_by(participant, font) %>%
      summarize(wordPerMin = mean(wordPerMin, na.rm = TRUE),
                .groups = "drop") %>%
      inner_join(peripheral_crowding, by = c("participant", "font")) %>%
      mutate(targetKind = "reading")
  }
  
  # Prepare RSVP data
  rsvp_data <- NULL
  if (nrow(rsvp) > 0) {
    rsvp_data <- rsvp %>%
      group_by(participant, font) %>%
      summarize(wordPerMin = 10^(mean(block_avg_log_WPM, na.rm = TRUE)),
                .groups = "drop") %>%
      inner_join(peripheral_crowding, by = c("participant", "font")) %>%
      mutate(targetKind = "rsvpReading")
  }
  
  # Combine data
  combined_data <- bind_rows(reading_data, rsvp_data)
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Aggregate by font: geometric mean of linear values
  font_aggregated <- combined_data %>%
    group_by(font, targetKind) %>%
    summarize(
      mean_crowding = exp(mean(log(crowding_distance), na.rm = TRUE)),
      mean_reading_speed = exp(mean(log(wordPerMin), na.rm = TRUE)),
      N = n(),
      .groups = "drop"
    )
  
  # Compute correlation and slope per font
  stats_per_font <- combined_data %>%
    group_by(font) %>%
    summarize(
      N = n(),
      R = cor(log10(crowding_distance), log10(wordPerMin), 
              use = "complete.obs", method = "pearson"),
      .groups = "drop"
    ) %>%
    mutate(
      R = round(R, 2),
      label = paste0(font, ", N=", N, ", R=", sprintf("%.2f", R))
    )
  
  # Join labels
  font_aggregated <- font_aggregated %>%
    left_join(stats_per_font %>% select(font, label), by = "font") %>%
    mutate(font_label = label)
  
  # Compute eccentricity label
  eccs <- sort(unique(abs(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg != 0])))
  ecc_label <- paste0("X ecc = ", paste(as.integer(round(eccs)), collapse = ", "), " deg")
  
  # Total N (participants)
  total_n <- nrow(combined_data)
  
  # Dynamic axis limits
  xMin <- min(font_aggregated$mean_crowding, na.rm = TRUE) / 1.5
  xMax <- max(font_aggregated$mean_crowding, na.rm = TRUE) * 1.5
  yMin <- min(font_aggregated$mean_reading_speed, na.rm = TRUE) / 1.5
  yMax <- max(font_aggregated$mean_reading_speed, na.rm = TRUE) * 1.5
  
  # Generate dynamic breaks for the y-axis
  y_breaks <- scales::log_breaks()(c(yMin, yMax))
  
  # Create plot
  p <- ggplot(font_aggregated, aes(x = mean_crowding, y = mean_reading_speed,
                                    color = font_label, shape = targetKind)) +
    theme_classic() +
    scale_y_log10(
      breaks = y_breaks,
      limits = c(yMin, yMax),
      expand = c(0, 0)
    ) +
    scale_x_log10(
      breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1.0, 3.0, 10.0, 100.0),
      labels = function(x) format(x, nsmall = 1, scientific = FALSE),
      limits = c(xMin, xMax),
      expand = c(0, 0)
    ) +
    geom_point(size = 3) +
    geom_smooth(aes(linetype = targetKind), method = "lm", formula = y ~ x, se = FALSE) +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +
    annotate(
      "text",
      x = xMin * 1.4,
      y = yMin * 1.6,
      label = paste0(ecc_label, "\nN = ", total_n),
      hjust = 0,
      vjust = 0,
      size = 5,
      color = "black"
    ) +
    labs(
      x = "Peripheral crowding (deg)",
      y = "Reading speed (w/min)",
      subtitle = "Reading vs peripheral crowding\nGeometric mean of left and right."
    ) +
    guides(
      color = guide_legend(ncol = 2, title = ""),
      shape = guide_legend(title = "")
    ) +
    plt_theme_ggiraph +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
  
  return(p)
}

# Font-aggregated plot: Ordinary reading vs peripheral crowding
plot_font_aggregated_ordinary_reading_crowding <- function(allData) {
  reading <- allData$reading %>% 
    mutate(participant = tolower(participant))
  crowding <- allData$crowding %>% 
    mutate(participant = tolower(participant))
  
  if (nrow(reading) == 0 || nrow(crowding) == 0) {
    return(NULL)
  }
  
  # Get peripheral crowding data
  peripheral_crowding <- crowding %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = 10^(mean(log_crowding_distance_deg, na.rm = TRUE)),
              .groups = "drop")
  
  # Prepare reading data
  reading_data <- reading %>%
    group_by(participant, font) %>%
    summarize(wordPerMin = mean(wordPerMin, na.rm = TRUE),
              .groups = "drop") %>%
    inner_join(peripheral_crowding, by = c("participant", "font"))
  
  if (nrow(reading_data) == 0) {
    return(NULL)
  }
  
  # Aggregate by font: geometric mean of linear values
  font_aggregated <- reading_data %>%
    group_by(font) %>%
    summarize(
      mean_crowding = exp(mean(log(crowding_distance), na.rm = TRUE)),
      mean_reading_speed = exp(mean(log(wordPerMin), na.rm = TRUE)),
      N = n(),
      .groups = "drop"
    )
  
  # Compute overall correlation and slope
  corr <- cor(log10(reading_data$crowding_distance), 
              log10(reading_data$wordPerMin),
              use = "complete.obs", method = "pearson")
  corr <- round(corr, 2)
  
  # Compute slope
  lm_fit <- lm(log10(wordPerMin) ~ log10(crowding_distance), data = reading_data)
  slope <- round(coef(lm_fit)[2], 2)
  
  # Total N (participants)
  total_n <- nrow(reading_data)
  
  # Compute eccentricity label
  eccs <- sort(unique(abs(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg != 0])))
  ecc_label <- paste0("X ecc = ±", paste(as.integer(round(eccs)), collapse = ", ±"), " deg")
  
  # Dynamic axis limits
  xMin <- min(font_aggregated$mean_crowding, na.rm = TRUE) / 1.5
  xMax <- max(font_aggregated$mean_crowding, na.rm = TRUE) * 1.5
  yMin <- min(font_aggregated$mean_reading_speed, na.rm = TRUE) / 1.5
  yMax <- max(font_aggregated$mean_reading_speed, na.rm = TRUE) * 1.5
  
  # Generate dynamic breaks for the y-axis
  y_breaks <- scales::log_breaks()(c(yMin, yMax))
  
  # Create plot
  p <- ggplot(font_aggregated, aes(x = mean_crowding, y = mean_reading_speed, color = font)) +
    theme_classic() +
    scale_y_log10(
      breaks = y_breaks,
      limits = c(yMin, yMax),
      expand = c(0, 0)
    ) +
    scale_x_log10(
      breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1.0, 3.0, 10.0, 100.0),
      labels = function(x) format(x, nsmall = 1, scientific = FALSE),
      limits = c(xMin, xMax),
      expand = c(0, 0)
    ) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
    scale_color_manual(values = font_color_palette(unique(font_aggregated$font))) +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +
    annotate(
      "text",
      x = xMin * 1.1,
      y = yMin * 1.3,
      label = paste0(
        "N = ", total_n,
        "\nR = ", corr,
        "\nR_factor_out_age = NA",
        "\nslope = ", slope
      ),
      hjust = 0,
      vjust = 0,
      size = 5,
      color = "black"
    ) +
    labs(
      x = "Peripheral crowding (deg)",
      y = "Ordinary reading speed (w/min)",
      subtitle = "Ordinary reading vs peripheral crowding\ncolored by font\nGeometric average of left and right thresholds"
    ) +
    guides(color = guide_legend(title = "font")) +
    plt_theme_ggiraph +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
  
  return(p)
}

# Font-aggregated plot: RSVP vs peripheral crowding
plot_font_aggregated_rsvp_crowding <- function(allData) {
  rsvp <- allData$rsvp %>% 
    mutate(participant = tolower(participant))
  crowding <- allData$crowding %>% 
    mutate(participant = tolower(participant))
  
  if (nrow(rsvp) == 0 || nrow(crowding) == 0) {
    return(NULL)
  }
  
  # Get peripheral crowding data
  peripheral_crowding <- crowding %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = 10^(mean(log_crowding_distance_deg, na.rm = TRUE)),
              .groups = "drop")
  
  # Prepare RSVP data
  rsvp_data <- rsvp %>%
    group_by(participant, font) %>%
    summarize(wordPerMin = 10^(mean(block_avg_log_WPM, na.rm = TRUE)),
              .groups = "drop") %>%
    inner_join(peripheral_crowding, by = c("participant", "font"))
  
  if (nrow(rsvp_data) == 0) {
    return(NULL)
  }
  
  # Aggregate by font: geometric mean of linear values
  font_aggregated <- rsvp_data %>%
    group_by(font) %>%
    summarize(
      mean_crowding = exp(mean(log(crowding_distance), na.rm = TRUE)),
      mean_reading_speed = exp(mean(log(wordPerMin), na.rm = TRUE)),
      N = n(),
      .groups = "drop"
    )
  
  # Compute correlation and slope per font
  stats_per_font <- rsvp_data %>%
    group_by(font) %>%
    summarize(
      N = n(),
      R = cor(log10(crowding_distance), log10(wordPerMin), 
              use = "complete.obs", method = "pearson"),
      slope = {
        fit <- lm(log10(wordPerMin) ~ log10(crowding_distance))
        round(coef(fit)[2], 2)
      },
      .groups = "drop"
    ) %>%
    mutate(R = round(R, 2))
  
  # Join stats to aggregated data
  font_aggregated <- font_aggregated %>%
    left_join(stats_per_font, by = "font")
  
  # Compute overall correlation and slope
  corr <- cor(log10(rsvp_data$crowding_distance), 
              log10(rsvp_data$wordPerMin),
              use = "complete.obs", method = "pearson")
  corr <- round(corr, 2)
  
  # Compute overall slope
  lm_fit <- lm(log10(wordPerMin) ~ log10(crowding_distance), data = rsvp_data)
  overall_slope <- round(coef(lm_fit)[2], 2)
  
  # Compute eccentricity label
  eccs <- sort(unique(abs(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg != 0])))
  ecc_label <- paste0("X ecc = ±", paste(as.integer(round(eccs)), collapse = ", ±"), " deg")
  
  # Dynamic axis limits
  xMin <- min(font_aggregated$mean_crowding, na.rm = TRUE) / 1.5
  xMax <- max(font_aggregated$mean_crowding, na.rm = TRUE) * 1.5
  yMin <- min(font_aggregated$mean_reading_speed, na.rm = TRUE) / 1.5
  yMax <- max(font_aggregated$mean_reading_speed, na.rm = TRUE) * 1.5
  
  # Generate dynamic breaks for the y-axis
  y_breaks <- scales::log_breaks()(c(yMin, yMax))
  
  # Total N (participants)
  total_n <- nrow(rsvp_data)
  
  # Create plot with separate regression lines per font
  p <- ggplot(font_aggregated, aes(x = mean_crowding, y = mean_reading_speed, color = font)) +
    theme_classic() +
    scale_y_log10(
      breaks = y_breaks,
      limits = c(yMin, yMax),
      expand = c(0, 0)
    ) +
    scale_x_log10(
      breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1.0, 3.0, 10.0, 100.0),
      labels = function(x) format(x, nsmall = 1, scientific = FALSE),
      limits = c(xMin, xMax),
      expand = c(0, 0)
    ) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    scale_color_manual(values = font_color_palette(unique(font_aggregated$font))) +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +
    annotate(
      "text",
      x = xMin * 1.4,
      y = yMin * 1.6,
      label = paste0(
        "N = ", total_n,
        "\nR = ", corr,
        "\nslope = ", overall_slope,
        "\n", ecc_label,
        "\nR_factor_out_age = NA"
      ),
      hjust = 0,
      vjust = 0,
      size = 5,
      color = "black"
    ) +
    labs(
      x = "Peripheral crowding (deg)",
      y = "RSVP reading (w/min)",
      subtitle = "RSVP vs peripheral crowding\ncolored by font\nGeometric mean of left and right measurements"
    ) +
    guides(color = guide_legend(title = "font")) +
    plt_theme_ggiraph +
    theme(
      legend.position = "top",
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 9)
    )
  
  return(p)
}
