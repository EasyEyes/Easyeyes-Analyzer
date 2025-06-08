source('./constant.R')


get_foveal_acuity_vs_age <- function(acuity) {
  t <- acuity %>% 
    filter(!is.na(age), targetEccentricityXDeg == 0) %>%
    mutate(N = paste0('N = ', n()))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Determine plot aesthetics based on Grade
    unique_grades <- n_distinct(t$Grade)
    if (unique_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(
        x = age,
        y = 10 ^ (questMeanAtEndOfTrialsLoop),
        color = Grade,
        shape = conditionName
      ))
    } else {
      p <- ggplot(t, aes(
        x = age,
        y = 10 ^ (questMeanAtEndOfTrialsLoop),
        shape = conditionName
      ))
    }
    
    # Regression line and statistics
    t <- t %>% mutate(Y = 10^(questMeanAtEndOfTrialsLoop))
    
    regression <- lm(Y ~ age, data = t)
    slope <- coef(regression)[["age"]]
    r_value <- cor(t$age, t$Y, use = "complete.obs")
    N <- nrow(t)
    
    # Plotting
    p <- p +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line in black
      geom_point(size = 3) +  # Add points
      theme_bw() +
      labs(
        title = "Foveal acuity vs age\ncolored by grade",
        x = "Age",
        y = "Foveal acuity (deg)"
      ) +
      scale_y_log10() +
      scale_x_continuous(breaks = floor(min(t$age)): ceiling(max(t$age))) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
     ) +
      annotate(
        "text",
        x = min(t$age, na.rm = TRUE),  # Slightly offset from the minimum x for padding
        y = min(t$Y, na.rm = TRUE),  # Slightly offset from the minimum y for padding
        label = paste0(
          "R = ", round(r_value, 2),
          "\nslope = ", round(slope, 2),
          "\nN = ", N
        ),
        hjust = 0, vjust = 0, size = 4, color = "black"
      ) +
      color_scale(n = unique_grades) +  # Apply color scale
      plt_theme +
      theme(
        legend.position = ifelse(unique_grades == 1, "none", "top")
      ) +
      guides(color = guide_legend(title = 'Grade'),
             shape = guide_legend(title = '', ncol = 1))
    
    # Add shapes for Skilled Reader if necessary
    # if (n_distinct(t$`Skilled reader?`) > 1) {
    #   p <- p + 
    #     geom_point(aes(shape = `Skilled reader?`)) +
    #     scale_shape_manual(values = c(4, 19,1))
    # }
    
    return(p)
  }
}

get_peripheral_acuity_vs_age <- function(acuity) {
  t <- acuity %>% 
    filter(!is.na(age), targetEccentricityXDeg != 0) %>%
    group_by(participant, age, Grade, `Skilled reader?`, conditionName) %>% 
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(N = paste0('N = ', n()))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Determine color aesthetics
    unique_grades <- n_distinct(t$Grade)
    if (unique_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(
        x = age,
        y = 10 ^ (questMeanAtEndOfTrialsLoop),
        color = Grade
      ))
    } else {
      p <- ggplot(t, aes(
        x = age,
        y = 10 ^ (questMeanAtEndOfTrialsLoop)
      ))
    }
    
    # Compute regression statistics
    t <- t %>% mutate(Y = 10 ^ (questMeanAtEndOfTrialsLoop))
    regression <- lm(Y ~ age, data = t)
    slope <- coef(regression)[["age"]]
    r_value <- cor(t$age, t$Y, use = "complete.obs")
    N <- nrow(t)
    
    # Plotting
    p <- p +
      geom_point(aes(shape = conditionName), size = 3) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Black regression line
      scale_y_log10() +
      scale_x_continuous(breaks = floor(min(t$age)): ceiling(max(t$age))) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = "top",
        label = N
      )) +
      theme_bw() +
      labs(
        title = "Peripheral acuity vs age\ncolored by Grade",
        subtitle = "Geometric average of left \nand right thresholds",
        x = "Age",
        y = "Peripheral acuity (deg)"
      ) +
      annotate(
        "text",
        x = min(t$age, na.rm = TRUE),  # Bottom-left placement
        y = min(t$Y, na.rm = TRUE),  # Slightly offset for padding
        label = paste0(
          "N = ", N,
          "\nR = ", round(r_value, 2),
          "\nslope = ", round(slope, 2)
        ),
        hjust = 0,
        vjust = 0,
        size = 4,
        color = "black"
      ) +
      color_scale(n = unique_grades) +  # Apply dynamic color scale
      plt_theme + 
      theme(
        legend.position = ifelse(unique_grades == 1, "none", "top")
      ) + 
        guides(color = guide_legend(title = 'Grade'),
               shape = guide_legend(title = '', ncol = 1))
    
    # Add shapes for Skilled Reader if applicable
    # if (n_distinct(t$`Skilled reader?`) > 1) {
    #   p <- p +
    #     geom_point(aes(shape = `Skilled reader?`)) +
    #     scale_shape_manual(values = c(4, 19,1))
    # }
    
    return(p)
  }
}


plot_acuity_rsvp <- function(acuity, rsvp, type) {
  create_plot <- function(data, type, colorFactor) {
    # Merge data and calculate WPM and acuity
    data_rsvp <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop, conditionName) %>%
      inner_join(rsvp %>% select(-conditionName), by = "participant") %>%
      mutate(
        Y = 10 ^ (block_avg_log_WPM),
        X = 10 ^ (questMeanAtEndOfTrialsLoop),
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        Grade = as.character(Grade)
      )
    
    if (nrow(data_rsvp) == 0) {
      return(NULL)
    }
    
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>%
        filter(`Skilled reader?` != FALSE) %>%
        select(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, X, Y, ageN)
    } else {
      data_for_stat <- data_rsvp %>%
        select(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, X, Y, ageN)
    }
    
    if (n_distinct(data_for_stat$ageN) == 1) {
      data_for_stat <- data_for_stat %>% select(-ageN)
      corr_without_age <- NA
    } else {
      t <- data_for_stat %>%
        select(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, ageN) 
      t <- t[complete.cases(t), ]
      corr_without_age <- ppcor::pcor(t)$estimate[2, 1]
      corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    }
    data_for_stat <- data_for_stat[complete.cases(data_for_stat), ]
    
    
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(
        correlation = cor(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson"),
        N = n()
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
    
    xMin <- min(data_rsvp$X, na.rm = TRUE)
    xMax <- max(data_rsvp$X, na.rm = TRUE)
    yMin <- min(data_rsvp$Y, na.rm = TRUE)
    yMax <- max(data_rsvp$Y, na.rm = TRUE)
    
    # Dynamic breaks for log scales
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    x_breaks <- scales::log_breaks()(c(xMin, xMax))
    
    # Apply dynamic color scale
    unique_colors <- n_distinct(data_rsvp[[colorFactor]])
    p <- ggplot(data_rsvp, aes(x = X, y = Y, color = .data[[colorFactor]])) +
      ggiraph::geom_point_interactive(aes(data_id = ParticipantCode, 
                                          tooltip = ParticipantCode, 
                                          shape = conditionName), size = 3) +
      theme_classic() +
      scale_y_log10(
        breaks = y_breaks,
        limits = c(yMin/1.5, yMax*1.5),
        expand = c(0, 0)
      ) +
      scale_x_log10(
        breaks = x_breaks,
        limits = c(xMin/1.5, xMax*1.5),
        expand = c(0, 0)
      ) +
      geom_smooth(method = 'lm', formula = y ~ x, se = FALSE, color = "black") +  # Black regression line
      annotation_logticks(
        sides = "bl", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      color_scale(n = unique_colors) +  # Dynamic color scale
      guides(color = guide_legend(title = colorFactor), 
             shape = guide_legend(title = '',
                                  ncol = 1)) +
      annotate(
        "text",
        x = xMin,
        y = yMin,
        label = paste0(
          "N = ", corr$N,
          "\nR = ", corr$correlation,
          "\nR_factor_out_age = ", corr_without_age,
          "\nslope = ", slope$slope
        ),
        hjust = 0,
        vjust = 0,
        size = 3,
        color = "black"
      ) +
      labs(
        x = paste0(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), ' acuity (deg)'),
        y = 'RSVP reading speed (w/min)',
        title = paste('RSVP vs', type, 'acuity\ncolored by', tolower(colorFactor), '\n')
      ) +
      plt_theme +
      theme(
        legend.position = ifelse(unique_colors == 1, 'none', 'top')
      )
    
    # if (n_distinct(data_rsvp$`Skilled reader?`) > 1) {
    #   p <- p + geom_point(
    #     aes(shape = `Skilled reader?`, group = ParticipantCode)
    #   ) +
    #     scale_shape_manual(values = c(4, 19,1))
    # } else {
    #   p <- p + geom_point(aes(group = ParticipantCode))
    # }
    # 
    return(p)
  }
  
  acuity <- acuity %>% mutate(participant = tolower(participant))
  rsvp <- rsvp %>% mutate(participant = tolower(participant))
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, conditionName) %>%
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm = T)) %>%
    ungroup()
  
  if (nrow(rsvp) == 0 | nrow(acuity) == 0) {
    return(list(NULL, NULL, NULL, NULL))
  }
  
  if (type == 'foveal') {
    p1 <- create_plot(foveal, type, 'Age')
    p2 <- create_plot(foveal, type, 'Grade')
    return(list(p1, p2))
  } else {
    p3 <- create_plot(peripheral, 'peripheral', 'Age')
    p4 <- create_plot(peripheral, 'peripheral', 'Grade')
    return(list(p3, p4))
  }
}


plot_acuity_reading <- function(acuity, reading, type) {
  create_reading_plot <- function(data, type, colorFactor) {
    # Merge data and calculate WPM and acuity
    data_reading <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop, conditionName) %>%
      inner_join(reading %>% select(-conditionName), by = "participant") %>%
      mutate(
        Y = wordPerMin,  # Linear scale
        log_WPM = log10(wordPerMin),  # Convert wordPerMin to log scale
        X = 10^(questMeanAtEndOfTrialsLoop),
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        Grade = as.character(Grade)
      )

    if (nrow(data_reading) == 0) {
      return(NULL)
    }
    
    # Prepare data for stats
    data_for_stat <- data_reading %>%
      filter(complete.cases(.)) %>%
      select(log_WPM, questMeanAtEndOfTrialsLoop, X, Y, ageN)
    
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(
        correlation = cor(log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson"),
        N = n()
      ) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- lm(log10(Y) ~ log10(X), data = data_for_stat)$coefficients[2]
    slope <- round(slope, 2)
    
    # Partial correlation excluding age
    if ("ageN" %in% names(data_for_stat)) {
      print("acuity reading names done")
      corr_without_age <- ppcor::pcor(data_for_stat %>%
                                        select(log_WPM, questMeanAtEndOfTrialsLoop, ageN))$estimate[2, 1]
      corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    } else {
      corr_without_age <- NA
    }
    
    # Annotation values
    annotation_text <- paste0(
      "N = ", corr$N,
      "\nR = ", corr$correlation,
      "\nR_factor_out_age = ", corr_without_age,
      "\nslope = ", slope
    )
    
    # Apply dynamic color scale
    unique_levels <- unique(data_reading[[colorFactor]])
    
    # Dynamic color scale directly applied in ggplot
    color_scale <- color_scale(n = length(unique_levels))
    
    # Plot
    xMin <- min(data_reading$X, na.rm = TRUE) / 1.5
    xMax <- max(data_reading$X, na.rm = TRUE) * 1.5
    yMin <- min(data_reading$Y, na.rm = TRUE) / 1.5
    yMax <- max(data_reading$Y, na.rm = TRUE) * 1.5
    
    p <- ggplot(data_reading, aes(x = X, y = Y, color = .data[[colorFactor]])) +
      theme_classic() +
      ggiraph::geom_point_interactive(aes(data_id = ParticipantCode, tooltip = ParticipantCode, shape = conditionName), size = 3) +
      scale_x_log10(
        limits = c(xMin, xMax),
        breaks = scales::log_breaks(),
        expand = c(0, 0)
      ) +
      scale_y_log10(
        limits = c(yMin, yMax),
        breaks = scales::log_breaks(),
        expand = c(0, 0)
      ) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Black regression line
      color_scale + 
      annotate(
        "text",
        x = xMin * 1.5,
        y = yMin * 1.8,
        label = annotation_text,
        hjust = 0,
        vjust = 0,
        size = 3,
        color = "black"
      ) +
      labs(
        x = paste0(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), " acuity (deg)"),
        y = "Ordinary reading speed (w/min)",
        title = paste("Ordinary reading vs", type, "acuity\ncolored by", tolower(colorFactor))
      ) +
      plt_theme + 
      guides(color = guide_legend(title = colorFactor), 
             shape = guide_legend(title = '',
                                  ncol = 1))
    
    return(p)
  }
  
  acuity <- acuity %>% mutate(participant = tolower(participant))
  reading <- reading %>% mutate(participant = tolower(participant))
  
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, conditionName) %>%
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm = T), .groups = "keep") %>%
    ungroup()
  
  if (nrow(reading) == 0 | nrow(acuity) == 0) {
    return(list(NULL, NULL, NULL, NULL))
  }
  
  if (type == "foveal") {
    p1 <- create_reading_plot(foveal, type, "Age")
    p2 <- create_reading_plot(foveal, type, "Grade")
    return(list(p1, p2))
  } else {
    p3 <- create_reading_plot(peripheral, "peripheral", "Age")
    p4 <- create_reading_plot(peripheral, "peripheral", "Grade")
    return(list(p3, p4))
  }
}

plot_acuity_vs_age <- function(allData){
  acuity <- allData$acuity %>%
    mutate(ageN = as.numeric(age)) %>% 
    filter(!is.na(ageN))
  if (nrow(acuity) == 0) {
    return(NULL)
  }

  foveal <- acuity %>% filter(questType == 'Foveal acuity')
  peripheral <- acuity %>% filter(questType == "Peripheral acuity")
  if (nrow(foveal) > 0) {
  foveal_stats <- foveal %>% 
    do(fit = lm(questMeanAtEndOfTrialsLoop ~ ageN, data = .)) %>%
    transmute(coef = map(fit, tidy)) %>%
    unnest(coef) %>%
    filter(term == "ageN") %>%  
    mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
    select(slope)
  } else{
    foveal_stats <- NA
  }
  if (nrow(peripheral) > 0) {
    peripheral_stats <- peripheral %>% 
      do(fit = lm(questMeanAtEndOfTrialsLoop ~ ageN, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == "ageN") %>%  
      mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
      select(slope)
  } else{
    peripheral_stats <- NA
  }
  foveal <- foveal %>% filter(!is.na(ageN),!is.na(questMeanAtEndOfTrialsLoop))
  peripheral <- peripheral %>% filter(!is.na(ageN),!is.na(questMeanAtEndOfTrialsLoop))
  label = ''
  foveal_corr <- format(round(cor(foveal$ageN,foveal$questMeanAtEndOfTrialsLoop),2),nsmall=2)
  peripheral_corr <- format(round(cor(peripheral$ageN,peripheral$questMeanAtEndOfTrialsLoop),2),nsmall=2)
  if (nrow(foveal) > 0) {
    label = paste0(label, 'Foveal: slope=', foveal_stats$slope, ', R=', foveal_corr)
  }
  if (nrow(peripheral) > 0) {
    label = paste0(label, 
                   ifelse(length(label) > 0, '\n', ''),
                   'Peripheral: slope=', peripheral_stats$slope, ', R=', peripheral_corr)
  }
  
  p <- ggplot(data = acuity, aes(x = ageN, 
                              y = 10^(questMeanAtEndOfTrialsLoop),
                              color = questType
  )) + 
    annotation_logticks(
      sides = "l", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) + 
    geom_point(aes(shape = conditionName))+ 
    geom_smooth(method = 'lm', se=F) +
    ggpp::geom_text_npc(
      size = 12/.pt,
      aes(npcx = "right",
          npcy = "top",
          label = label)) +
    scale_y_log10() + 
    scale_x_continuous(breaks = floor(min(acuity$ageN)): ceiling(max(acuity$ageN))) + 
    guides(color=guide_legend(title = '')) + 
    labs(x = 'Age',
         y = 'Acuity (deg)',
         title = 'Foveal and peripheral acuity vs age')
  return(p)
}

get_acuity_foveal_peripheral_diag <- function(acuity) {
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, age, Grade, `Skilled reader?`) %>%
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm = T)) %>%
    ungroup()
  
  if (nrow(foveal) == 0 | nrow(peripheral) == 0) {
    return(NULL)
  } else {
    p1 <- NULL
    t <- foveal %>%
      rename('foveal' = 'questMeanAtEndOfTrialsLoop') %>%
      select(foveal, participant, order) %>%
      inner_join(peripheral, by = 'participant') %>%
      rename('peripheral' = 'questMeanAtEndOfTrialsLoop') %>%
      mutate(age = format(age, nsmall = 2),
             N = paste0('N=', n()))
    
    if (n_distinct(t$Grade) > 1) {
      t <- t %>%
        mutate(Grade = as.character(Grade))
      
      n_grades <- n_distinct(t$Grade)  # Determine the number of distinct Grade levels
      
      p1 <- ggplot(t, aes(y = 10^peripheral,
                          x = 10^foveal,
                          color = Grade)) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top", label = N)) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Black regression line
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        plt_theme +
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        labs(y = 'Peripheral acuity (deg)',
             x = 'Foveal acuity (deg)',
             title = 'Peripheral acuity vs foveal acuity\ncolored by grade') +
        coord_fixed() +
        color_scale(n = n_grades)  # Pass the dynamic number of grades
    } 
    
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p1 <- p1 + geom_point()
    } else {
      p1 <- p1 +
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19,1))
    }
    return(p1)
  }
}

peripheral_plot <- function(allData) {
  crowding <- allData$crowding %>% filter(targetEccentricityXDeg != 0)
  acuity <- allData$acuity %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(crowding) == 0 | nrow(acuity) == 0) {
    return(NULL)
  }
  print(crowding)
  print(acuity)
  t <- crowding %>%
    select(participant, log_crowding_distance_deg) %>%
    left_join(acuity, by = "participant") %>%
    mutate(Grade = as.character(Grade))
  
  # Calculate the number of unique grades for the color scale
  n_grades <- n_distinct(t$Grade)
  
  ggplot(data = t, aes(x = 10^(log_crowding_distance_deg),
                       y = 10^questMeanAtEndOfTrialsLoop,
                       color = Grade)) +
    geom_point(size = 3) +  # Adjust point size for better visibility
    scale_x_log10(expand = c(0, 0)) +
    scale_y_log10(expand = c(0, 0)) +
    coord_fixed() +
    labs(
      x = 'Peripheral crowding distance (deg)',
      y = 'Peripheral acuity (deg)',
      title = 'Peripheral acuity vs peripheral crowding\ncolored by grade'
    ) +
    theme_classic() +
    plt_theme +
    theme(
      legend.position = "top"
    ) +
    color_scale(n = n_grades)  # Dynamically apply the gray-to-black color scale
}


