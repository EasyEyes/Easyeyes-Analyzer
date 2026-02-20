plot_rsvp_repeated_letter_crowding <- function(allData) {
  print('inside plot_reading_repeated_letter_crowding')  
  
  create_plot <- function(data, condition, colorFactor) {
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      inner_join(rsvp, by = c("participant", "font")) %>%
      distinct(participant, 
               block_avg_log_WPM, 
               log_crowding_distance_deg,
               age,
               Grade,
               `Skilled reader?`,
               ParticipantCode) %>%
      filter(!is.na(participant)) %>%
      mutate(
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        X = 10^(log_crowding_distance_deg),
        Y = 10^(block_avg_log_WPM),
        Grade = as.character(Grade)
      )
    
    if (nrow(data_rsvp) == 0) {
      return(NULL)
    }
    
    if (n_distinct(data_rsvp$`Skilled reader?`) > 1) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` != FALSE) %>%
        select(block_avg_log_WPM, log_crowding_distance_deg, X, Y, ageN)
    } else {
      data_for_stat <- data_rsvp %>%
        select(block_avg_log_WPM, log_crowding_distance_deg, X, Y, ageN)
    }
    if (sum(!is.na(data_for_stat$ageN)) == 0) {
      data_for_stat <- data_for_stat %>% select(-ageN)
    }
    
    data_for_stat <- data_for_stat[complete.cases(data_for_stat),]
    
    corr <- data_for_stat %>%
      summarize(
        correlation = cor(block_avg_log_WPM, log_crowding_distance_deg, method = "pearson"),
        N = n(),
        .groups="drop"
      ) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      mutate(
        log_X = log10(X),
        log_Y = log10(Y)
      ) %>%
      do(fit = lm(log_Y ~ log_X, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == "log_X") %>%
      mutate(slope = round(estimate, 2)) %>%
      select(slope)
    
    if ('ageN' %in% names(data_for_stat)) {
      if (nrow(data_for_stat) > 1) {
        corr_without_age <- ppcor::pcor(data_for_stat %>%
                                          select(block_avg_log_WPM, log_crowding_distance_deg, ageN))$estimate[2, 1]
        corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
      } else {
        corr_without_age <- NA
      }
      
    } else {
      corr_without_age <- NA
    }
    
    xMin <- min(data_rsvp$X, na.rm = T) / 1.5
    xMax <- max(data_rsvp$X, na.rm = T) * 1.5
    yMin <- min(data_rsvp$Y, na.rm = T) / 1.5
    yMax <- max(data_rsvp$Y, na.rm = T) * 1.5
    
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    
    unique_colors <- n_distinct(data_rsvp[[colorFactor]])
    p <- ggplot() +
      theme_classic() +
      scale_y_log10(
        breaks = y_breaks,
        limits = c(yMin, yMax),
        expand = c(0, 0)
      ) +
      scale_x_log10(
        breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 10, 100),
        limits = c(xMin, xMax),
        expand = c(0, 0)
      ) +
      geom_smooth(data = data_for_stat,
                  aes(x = X, y = Y),
                  method = 'lm',
                  se = FALSE,
                  color = "black") +  # Black regression line
      annotation_logticks(
        sides = "bl", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +
      annotate(
        "text",
        x = xMin * 1.4,
        y = yMin * 1.6,
        label = paste0(
          "N = ", corr$N,
          "\nR = ", corr$correlation,
          "\nR_factor_out_age = ", corr_without_age,
          "\nslope = ", slope$slope
        ),
        hjust = 0,
        vjust = 0,
        size = 4,
        color = "black"
      ) +
      plt_theme +
      (if (colorFactor == "font") scale_color_manual(values = font_color_palette(unique(data_rsvp$font))) else color_scale(n = unique_colors)) +  # Apply dynamic color scale
      theme(legend.position = ifelse(unique_colors == 1, 'none', 'top')) +
      guides(color = guide_legend(title = colorFactor), shape = F) +
      labs(
        x = paste('Repeated-letter crowding (deg)'),
        y = 'RSVP reading (w/min)',
        subtitle = paste('RSVP vs repeated-letter crowding\ncolored by', tolower(colorFactor))
      ) +
      theme(
        plot.title = element_text(margin = margin(b = 10), size = 17),
        plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
      )
    
    if (n_distinct(data_rsvp$`Skilled reader?`) == 1) {
      p <- p + geom_point(
        data = data_rsvp,
        aes(
          x = X,
          y = Y,
          group = ParticipantCode,
          color = .data[[colorFactor]]
        )
      )
    } else {
      n_reader <- n_distinct(data_rsvp$`Skilled reader?`)
      p <- p + geom_point(
        data = data_rsvp,
        aes(
          x = X,
          y = Y,
          group = ParticipantCode,
          color = .data[[colorFactor]],
          shape = `Skilled reader?`
        )
      ) +
        scale_shape_manual(values = c(4, 19, 1, 2, 0, 5, 6, 8)[seq_len(n_reader)])
    }
    
    return(p)
  }
  
  crowding <- allData$repeatedLetters %>% mutate(participant = tolower(participant))
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
  
  if (nrow(allData$rsvp) == 0 | nrow(allData$repeatedLetters) == 0) {
    return(list(NULL, NULL))
  }
  
  p3 <- create_plot(peripheral, "Peripheral", 'Grade')
  p4 <- create_plot(foveal, "Foveal", 'Grade')
  
  return(list(peripheral = p3, foveal = p4))
}


plot_reading_repeated_letter_crowding <- function(allData) {
  print('inside plot_reading_repeated_letter_crowding')
  
  create_plot <- function(data, condition, colorFactor) {
    data_reading <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      inner_join(reading, by = "participant") %>%
      distinct(
        participant, wordPerMin, log_crowding_distance_deg,
        age, Grade, `Skilled reader?`, ParticipantCode
      ) %>%
      mutate(log_wpm = log10(wordPerMin)) %>%
      filter(!is.na(participant)) %>%
      mutate(
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        X = 10^(log_crowding_distance_deg),
        Y = wordPerMin,
        Grade = as.character(Grade)
      )
    
    if (nrow(data_reading) == 0) {
      return(NULL)
    }
    
    if (n_distinct(data_reading$`Skilled reader?`) > 1) {
      data_for_stat <- data_reading %>%
        filter(`Skilled reader?` != FALSE) %>%
        select(log_wpm, log_crowding_distance_deg, X, Y, ageN)
    } else {
      data_for_stat <- data_reading %>%
        select(log_wpm, log_crowding_distance_deg, X, Y, ageN)
    }
    
    if (sum(!is.na(data_for_stat$ageN)) == 0) {
      data_for_stat <- data_for_stat %>% select(-ageN)
    }
    
    data_for_stat <- data_for_stat[complete.cases(data_for_stat),]
    
    corr <- data_for_stat %>%
      summarize(
        correlation = cor(log_wpm, log_crowding_distance_deg, method = "pearson"),
        N = n(),
        .groups="drop"
      ) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      mutate(
        log_X = log10(X),
        log_Y = log10(Y)
      ) %>%
      # Fit the regression model in log-log space
      do(fit = lm(log_Y ~ log_X, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == "log_X") %>%
      mutate(slope = round(estimate, 2)) %>%
      select(slope)
    
    if ('ageN' %in% names(data_for_stat) & nrow(data_for_stat) > 1) {
      corr_without_age <- ppcor::pcor(data_for_stat %>%
                                        select(log_wpm, log_crowding_distance_deg, ageN))$estimate[2, 1]
      corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    } else {
      corr_without_age <- NA
    }
    
    xMin <- min(data_reading$X, na.rm = T) / 1.5
    xMax <- max(data_reading$X, na.rm = T) * 1.5
    yMin <- min(data_reading$Y, na.rm = T) / 1.5
    yMax <- max(data_reading$Y, na.rm = T) * 1.5
    
    # Generate dynamic breaks for the y-axis
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    
    unique_colors <- n_distinct(data_reading[[colorFactor]])
    p <- ggplot() +
      theme_classic() +
      scale_y_log10(
        breaks = y_breaks,
        limits = c(yMin, yMax),
        expand = c(0, 0)
      ) +
      scale_x_log10(
        breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 10, 100),
        limits = c(xMin, xMax),
        expand = c(0, 0)
      ) +
      geom_smooth(
        data = data_for_stat,
        aes(x = X, y = Y),
        method = 'lm',
        se = FALSE,
        color = "black"
      ) +
      coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +
      annotate(
        "text",
        x = xMin * 1.4,
        y = yMin * 1.4,
        label = paste0(
          "N = ", corr$N,
          "\nR = ", corr$correlation,
          "\nR_factor_out_age = ", corr_without_age,
          "\nslope = ", slope$slope
        ),
        hjust = 0,
        vjust = 0,
        size = 4,
        color = "black"
      ) +
      plt_theme_ggiraph +
      (if (colorFactor == "font") scale_color_manual(values = font_color_palette(unique(data_reading$font))) else color_scale(n = unique_colors)) +  # Apply color scale dynamically
      guides(color = guide_legend(title = colorFactor), shape = F) +
      labs(
        x = paste('Repeated-letter crowding (deg)'),
        y = 'Reading (w/min)',
        subtitle = paste('Reading vs repeated-letter crowding\ncolored by', tolower(colorFactor), '\n')
      ) +
      theme(
        legend.position = ifelse(unique_colors == 1, 'none', 'top'),
        plot.title = element_text(margin = margin(0, 0, 50, 0), size = 17),
        plot.margin = margin(t = 10, r = 10, b = 20, l = 10)
      )
    
    if (n_distinct(data_reading$`Skilled reader?`) == 1) {
      p <- p + geom_point(
        data = data_reading,
        aes(
          x = X,
          y = Y,
          group = ParticipantCode,
          color = .data[[colorFactor]]
        )
      )
    } else {
      n_reader <- n_distinct(data_reading$`Skilled reader?`)
      p <- p + geom_point(
        data = data_reading,
        aes(
          x = X,
          y = Y,
          group = ParticipantCode,
          color = .data[[colorFactor]],
          shape = `Skilled reader?`
        )
      ) +
        scale_shape_manual(values = c(4, 19, 1, 2, 0, 5, 6, 8)[seq_len(n_reader)])
    }
    
    return(p)
  }
  
  crowding <- allData$repeatedLetters %>% mutate(participant = tolower(participant))
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  reading <- allData$reading %>% mutate(participant = tolower(participant))
  
  if (nrow(allData$reading) == 0 | nrow(allData$repeatedLetters) == 0) {
    return(list(NULL, NULL))
  }
  
  # Create plots for peripheral and foveal data
  p3 <- create_plot(peripheral, "Peripheral", 'Grade')
  p4 <- create_plot(foveal, "Foveal", 'Grade')
  
  return(list(peripheral = p3, foveal = p4))
}
