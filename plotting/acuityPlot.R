source('./constant.R')

get_foveal_acuity_vs_age <- function(acuity) {
  t <- acuity %>% filter(!is.na(age),
                         targetEccentricityXDeg == 0) %>%
    mutate(N = paste0('N=', n()))
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    if (n_distinct(t$Grade) > 1) {
      t$Grade = as.character(t$Grade)
      p <-
        ggplot(t, aes(
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
    p <- p +
      scale_y_log10() +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = 'top',
        label = N
      )) +
      theme_bw() +
      labs(title = 'Foveal acuity vs age',
           x = 'Age',
           y = 'Foveal acuity (deg)')
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p +
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19))
    }
    
    return(p)
  }
}


get_peripheral_acuity_vs_age <- function(acuity) {
  t <- acuity %>% filter(!is.na(age),
                         targetEccentricityXDeg != 0) %>%
    mutate(N = paste0('N=', n()))
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    if (n_distinct(t$Grade) > 1) {
      t$Grade = as.character(t$Grade)
      p <-
        ggplot(t, aes(
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
    p <- p +
      geom_point() +
      scale_y_log10() +
      ggpp::geom_text_npc(aes(
        npcx = "left",
        npcy = 'top',
        label = N
      )) +
      theme_bw() +
      labs(title = 'Peripheral acuity vs age',
           x = 'Age',
           y = 'Peripheral acuity (deg)')
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p +
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19))
    }
    return(p)
  }
}



plot_acuity_rsvp <- function(acuity, rsvp, type) {
  create_plot <- function(data, type, colorFactor) {
    # Merge data and calculate WPM and acuity
    data_rsvp <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop) %>%
      inner_join(rsvp, by = "participant") %>%
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
        select(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, X, Y,ageN)
    } else {
      data_for_stat <- data_rsvp %>%
        select(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, X, Y.ageN)
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
      do(fit = lm(Y ~ X, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == 'X') %>%
      select(estimate) %>%
      mutate(slope = round(estimate, 2))
    
    corr_without_age <- ppcor::pcor(data_for_stat %>%
                                      select(block_avg_log_WPM,
                                             questMeanAtEndOfTrialsLoop,
                                             ageN))$estimate[2,1] 
    corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    
    xMin <- min(data_rsvp$X, na.rm = TRUE)
    xMax <- max(data_rsvp$X, na.rm = TRUE)
    yMin <- min(data_rsvp$Y, na.rm = TRUE) / 1.2
    yMax <- max(data_rsvp$Y, na.rm = TRUE) * 1.2
    
    # Dynamic breaks for log scales
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    x_breaks <- scales::log_breaks()(c(xMin, xMax))
    
    # Plot
    p <- ggplot() +
      theme_classic() +
      scale_y_log10(
        breaks = y_breaks,
        limits = c(yMin, yMax),
        expand = c(0, 0)
      ) +
      scale_x_log10(
        breaks = x_breaks,
        limits = c(xMin, xMax),
        expand = c(0, 0)
      ) +
      geom_smooth(data = data_for_stat,
                  aes(x = X, y = Y),
                  method = 'lm',
                  se = FALSE) +
      annotation_logticks() +
      plt_theme +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC) == 1, 'none', 'top')) +
      guides(color = guide_legend(title = colorFactor), shape = 'none') +
      coord_fixed(ratio = 1) +
      annotate(
        "text",
        x = xMin,
        y = yMin * 1.3,
        label = paste0("N = ", corr$N,
                       "\nR = ", corr$correlation,
                       "\nR_factor_out_age = ", corr_without_age,
                       "\nslope = ", slope$slope),
        hjust = 0,        # Left-align text
        vjust = 0,        # Top-align for consistent stacking
        size = 4,
        color = "black"
      ) +
      # ggpp::geom_text_npc(
      #         aes(npcx = "right",
      #             npcy = "top",
      #             label = paste0("italic('R=')~",corr$correlation,
      #                            "~italic(', slope=')~", slope$slope)),
      #         parse = T) +
      labs(
        x = paste0(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), ' acuity (deg)'),
        y = 'RSVP reading speed (w/min)',
        title = paste('RSVP vs', type, '\nacuity colored by', tolower(colorFactor))
      ) +
      theme(
        plot.title = element_text(margin = margin(b = 10), size = 17), # Increased font size
        plot.margin = margin(t = 10, r = 10, b = 20, l = 10) # Extra margin for aestheticsCenter and add bottom margin
      )
    if (n_distinct(data_rsvp$`Skilled reader?`) > 1) {
      p <-  p + geom_point(
        data = data_rsvp,
        aes(
          x = X,
          y = Y,
          color = .data[[colorFactor]],
          shape = `Skilled reader?`,
          group = ParticipantCode
        )
      ) +
        scale_shape_manual(values = c(4, 19))
    } else {
      p <- p + geom_point(data = data_rsvp,
                          aes(
                            x = X,
                            y = Y,
                            color = .data[[colorFactor]],
                            group = ParticipantCode
                          ))
    }
    
    return(p)
  }
  
  acuity <- acuity %>% mutate(participant = tolower(participant))
  rsvp <- rsvp %>% mutate(participant = tolower(participant))
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>% filter(targetEccentricityXDeg != 0)
  
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
      select(participant, questMeanAtEndOfTrialsLoop) %>%
      inner_join(reading, by = "participant") %>%
      mutate(
        Y = log10(wordPerMin),  # Convert wordPerMin to log scale
        X = 10^(questMeanAtEndOfTrialsLoop),
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        Grade = as.character(Grade)
      )
    
    if (nrow(data_reading) == 0) {
      return(NULL)
    }
    
    # Similar filtering and stats as RSVP
    data_for_stat <- data_reading %>%
      filter(complete.cases(.)) %>%
      select(wordPerMin, questMeanAtEndOfTrialsLoop, X, Y, ageN)
    
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(
        correlation = cor(wordPerMin, questMeanAtEndOfTrialsLoop, method = "pearson"),
        N = n()
      ) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      do(fit = lm(Y ~ X, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == 'X') %>%
      select(estimate) %>%
      mutate(slope = round(estimate, 2))
    
    # Partial correlation excluding age
    if ('ageN' %in% names(data_for_stat)) {
      corr_without_age <- ppcor::pcor(data_for_stat %>%
                                        select(wordPerMin, questMeanAtEndOfTrialsLoop, ageN))$estimate[2, 1]
      corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    } else {
      corr_without_age <- NA
    }
    
    # Annotation values
    annotation_text <- paste0(
      "N = ", corr$N,
      "\nR = ", corr$correlation,
      "\nR_factor_out_age = ", corr_without_age,
      "\nslope = ", slope$slope
    )
    
    # Plot
    xMin <- min(data_reading$X, na.rm = TRUE) / 1.5
    xMax <- max(data_reading$X, na.rm = TRUE) * 1.5
    yMin <- min(data_reading$Y, na.rm = TRUE) / 1.5
    yMax <- max(data_reading$Y, na.rm = TRUE) * 1.5
    
    p <- ggplot() +
      theme_classic() +
      scale_y_log10() +
      scale_x_log10() +
      geom_smooth(
        data = data_for_stat,
        aes(x = X, y = Y),
        method = 'lm',
        se = FALSE
      ) +
      annotate(
        "text",
        x = xMin * 1.5,
        y = yMin * 1.5,
        label = annotation_text,
        hjust = 0, # Left-align text
        vjust = 0, # Top-align text
        size = 4,
        color = "black"
      ) +
      labs(
        x = paste0(toupper(substr(type, 1, 1)), substr(type, 2, nchar(type)), ' acuity (deg)'),
        y = 'Ordinary reading speed (w/min)',  # Updated for ordinary reading
        title = paste('Ordinary Reading vs', type, '\nacuity colored by', tolower(colorFactor))
      ) + 
      theme(
        plot.title = element_text(margin = margin(b = 10), size = 17), # Increased font size
        plot.margin = margin(t = 10, r = 10, b = 20, l = 10) # Extra margin for aestheticsCenter and add bottom margin
      )
    
    if (n_distinct(data_reading$`Skilled reader?`) > 1) {
      p <- p + geom_point(
        data = data_reading,
        aes(
          x = X,
          y = Y,
          color = .data[[colorFactor]],
          shape = `Skilled reader?`
        )
      ) +
        scale_shape_manual(values = c(4, 19))
    } else {
      p <- p + geom_point(
        data = data_reading,
        aes(
          x = X,
          y = Y,
          color = .data[[colorFactor]]
        )
      )
    }
    
    return(p)
  }
  
  acuity <- acuity %>% mutate(participant = tolower(participant))
  reading <- reading %>% mutate(participant = tolower(participant))
  
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(reading) == 0 | nrow(acuity) == 0) {
    return(list(NULL, NULL, NULL, NULL))
  }
  
  if (type == 'foveal') {
    p1 <- create_reading_plot(foveal, type, 'Age')
    p2 <- create_reading_plot(foveal, type, 'Grade')
    return(list(p1, p2))
  } else {
    p3 <- create_reading_plot(peripheral, 'peripheral', 'Age')
    p4 <- create_reading_plot(peripheral, 'peripheral', 'Grade')
    return(list(p3, p4))
  }
}


