source('./constant.R')


crowding_by_side <- function(crowding) {
  print('insdie crowding_by_side')
  crowding <- crowding %>% filter(targetEccentricityXDeg != 0, !is.na(conditionName)) %>% 
    mutate(side = ifelse(targetEccentricityXDeg < 0, 'L', 'R'),
           XDeg = abs(targetEccentricityXDeg))
  
  crowding_L <- crowding %>%
    filter(side == "L") %>%
    select(-side)
  crowding_R <- crowding %>% 
    filter(side == "R") %>%
    select(-side)

  crowding_L_R <- crowding_L %>% 
    full_join(crowding_R, by = c("participant","font", 'XDeg','order', 'block')) %>% 
    rename("bouma_factor_Left" = "bouma_factor.x",
           "bouma_factor_Right" = "bouma_factor.y",
           "log_crowding_distance_deg_Left" = "log_crowding_distance_deg.x",
           "log_crowding_distance_deg_Right" = "log_crowding_distance_deg.y") %>%
    filter(!is.na(log_crowding_distance_deg_Left) & !is.na(log_crowding_distance_deg_Right)) %>% 
    mutate(conditionName =  paste0(conditionName.x, ' vs ', conditionName.y)) %>% 
  return(crowding_L_R)
}

crowding_scatter_plot <- function(crowding_L_R){
  if (n_distinct(crowding_L_R$font) < 1) {
    return(NULL)
  }
  if(n_distinct(crowding_L_R$font) == 1) {
    if (is.na(unique(crowding_L_R$log_crowding_distance_deg_Left)[1])) {
      return(NULL)
    }
  }
  crowding_L_R <- crowding_L_R %>% 
    group_by(participant, conditionName) %>% 
    summarize(log_crowding_distance_deg_Left = mean(log_crowding_distance_deg_Left, na.rm=T),
              log_crowding_distance_deg_Right = mean(log_crowding_distance_deg_Right, na.rm=T)) %>% 
    ungroup()
  
  print('inside crowding_scatter_plot')
  N_left = crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Left)) %>% count()
  # this is to work around a bug, need to check later
  N_left = N_left[1]
  N_right= crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Right)) %>% count()
  N_right = N_right[1]
  N_both = crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Right),!is.na(log_crowding_distance_deg_Left)) %>% count()
  N_both = N_both[1]
  summ <- group_by(crowding_L_R) %>% 
    mutate(log_delta = log_crowding_distance_deg_Left - log_crowding_distance_deg_Right,
           log_mean = (log_crowding_distance_deg_Left+log_crowding_distance_deg_Right)/2) %>% 
    summarize(mean_log_delta = mean(log_delta, na.rm = T),
              sd_log_delta = sd(log_delta, na.rm = T),
              mean_left = mean(log_crowding_distance_deg_Left, na.rm = T),
              sd_left = sd(log_crowding_distance_deg_Left, na.rm = T),
              mean_right = mean(log_crowding_distance_deg_Right, na.rm = T),
              sd_right = sd(log_crowding_distance_deg_Right, na.rm = T),
              mean_avg = mean(log_mean, na.rm = T),
              sd_avg = sd(log_mean, na.rm = T)) %>% 
    mutate(sdTestRetest = sd_log_delta / sqrt(2)) %>% 
    mutate(sdIndividual = sqrt(sd_avg^2 - 0.5*sdTestRetest^2))
  
  summ <- summ %>% 
    summarize(label =paste0('N= ', N_both, '\n', 
                         'Left: log(spacingDeg left): n=', N_left,', meanLeft= ', format(round(mean_left,2),nsmall=2), '\n',
                         '      sdLeft= ', format(round(sd_left,2),nsmall=2), '\n',
                         'Right: log(spacingDeg right): n=', N_right,', meanRight= ', format(round(mean_right,2),nsmall=2), '\n',
                         '      sdRight= ', format(round(sd_right,2),nsmall=2), '\n',
                         'Diff: log(spacingDeg left)-log(spacingDeg right): n=',N_both, '\n',
                         '      meanDiff=', format(round(mean_log_delta,2),nsmall=2), ', sdDiff= ', format(round(sd_log_delta,2),nsmall=2), '\n',
                         'Avg: [log(spacingDeg left)+log(spacingDeg right)]/2: n=', N_both, '\n',
                         '      meanAvg=', format(round(mean_avg,2),nsmall=2), ', sdAvg=', format(round(sd_avg,2),nsmall=2), '\n',
                         'TestRetest: sdTestRetest=sdDiff/sqrt(2)=', format(round(sdTestRetest,2),nsmall=2), '\n',
                         'Individual: sdIndividual=\n',
                         '      sqrt(sdAvg^2 - 0.5*(sdTestRetest^2))=', format(round(sdIndividual,2),nsmall=2)
                         ))

  minXY <- min(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right, na.rm = T) * 0.9
  maxXY <- max(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right, na.rm = T) * 1.1
  p <- ggplot(crowding_L_R,aes(y = 10^(log_crowding_distance_deg_Left), 
                               x = 10^(log_crowding_distance_deg_Right),
                               color = conditionName)) + 
    geom_point(size = 1) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10(limits = c(10^(minXY), 
                             10^(maxXY)),
                  breaks = c(0.1, 0.3, 1, 3, 10),
                  expand=c(0,0)) +
    scale_x_log10(limits = c(10^(minXY), 
                             10^(maxXY)),
                  breaks = c(0.1, 0.3, 1, 3, 10),
                  expand=c(0,0)) + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    coord_fixed() + 
    theme_bw() + 
  labs(x = "Right crowding distance (deg)",
       y = "Left crowding distance (deg)",
       title = "Left vs right peripheral crowding",
       caption = summ$label
  ) + 
    theme(plot.caption = element_text(hjust = 0, size = 12)) +
    guides(color = guide_legend(title = '', 
                                ncol = 1))
  return(p)
}

crowding_mean_scatter_plot <- function(crowding_L_R){
  if (n_distinct(crowding_L_R$font) < 1) {
    return(ggplot() + 
             xlab("Left Bouma factor") + 
             ylab("Right Bouma factor") )
  }
  
  t <- crowding_L_R %>% group_by(conditionName) %>% summarize(avg_left_deg = mean(log_crowding_distance_deg_Left),
                                                     avg_right_deg = mean(log_crowding_distance_deg_Right))
  corrrelation <- ifelse(nrow(t) > 1, cor(t$avg_left_deg, t$avg_right_deg), NA)
  
  ggplot(t, aes(x = avg_left_deg, y = avg_right_deg, color = conditionName)) + 
    geom_point(size = 2) +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    stat_cor() +  
    coord_fixed(ratio = 1) + 
    guides(color = guide_legend(title="", ncol = 1)) + 
    xlab("Left crowding distance (deg)") + 
    ylab("Right crowding distance (deg)") + 
    theme_bw() + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "top",
          label = paste0("italic('R=')~", corrrelation)), 
      parse = T) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", dplyr::n_distinct(crowding_L_R$participant))), 
      parse = T)
}

get_two_fonts_plots <- function(crowding) {
  crowding <- crowding %>% 
    filter(bouma_factor != Inf)
  # Calculate mean and standard deviation for each participant and font
  if (n_distinct(crowding$font) < 1) {
    return(ggplot())
  }
  t <- crowding %>% 
    group_by(participant, font, order) %>% 
    summarize(
      MEAN = mean(log10(bouma_factor), na.rm = TRUE),
      SD = sd(log10(bouma_factor), na.rm = TRUE)
    ) %>% 
    mutate(
      MEAN = round(MEAN, 2),
      SD = round(SD, 2)
    ) %>% 
    arrange(participant, font)
  
  # Number of unique participants
  n <- n_distinct(crowding$participant)
  if (n_distinct(t$font) == 2) {
    font1 = unique(t$font)[1]
    font2 = unique(t$font)[2]
  } else {
    return(list(
      ggplot(),
      ggplot(),
      'font A vs font B'
    ))
  }
  # Prepare data for mean plot
  for_plot_means <- t %>% pivot_wider(names_from = font, values_from = MEAN)
  font1_means <- for_plot_means %>% filter(!is.na(!!sym(font1))) %>% select(participant, !!sym(font1))
  font2_means <- for_plot_means %>% filter(!is.na(!!sym(font2))) %>% select(participant, !!sym(font2))
  
  for_plot <- font1_means %>% inner_join(font2_means, by = "participant")
  
  # Mean plot
  mean_plot <- ggplot(for_plot, aes_string(x = paste0("10^(`", font1, "`)"), y = paste0("10^(`", font2, "`)"))) + 
    geom_point() + 
    scale_y_log10() +
    scale_x_log10() + 
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
    theme_bw() + 
    coord_fixed(ratio = 1) + 
    labs(x = paste0(font1, " Bouma Factor"), y = paste0(font2, " Bouma Factor")) +
    ggpp::geom_text_npc(
      aes(npcx = "left", npcy = "bottom", label = paste0("italic('N=')~", n)), 
      parse = TRUE
    ) + 
    stat_cor(aes(label = ..r.label..), r.accuracy = 0.001)
  
  # Prepare data for standard deviation plot
  for_plot_sd <- t %>% pivot_wider(names_from = font, values_from = SD)
  font1_sd <- for_plot_sd %>% filter(!is.na(!!sym(font1))) %>% select(participant, !!sym(font1))
  font2_sd <- for_plot_sd %>% filter(!is.na(!!sym(font2))) %>% select(participant, !!sym(font2))
  for_plot <- font1_sd %>% inner_join(font2_sd, by = "participant")
  
  # Standard deviation plot
  sd_plot <- ggplot(for_plot, aes_string(x = paste0("10^(`", font1, "`)"), y = paste0("10^(`", font2, "`)"))) + 
    geom_point() + 
    scale_y_log10() +
    scale_x_log10() + 
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) + 
    theme_bw() + 
    coord_fixed(ratio = 1) + 
    labs(x = paste0(font1, " Bouma Factor"), y = paste0(font2, " Bouma Factor")) +
    ggpp::geom_text_npc(
      aes(npcx = "left", npcy = "bottom", label = paste0("italic('N=')~", n)), 
      parse = TRUE
    ) + 
    stat_cor(aes(label = ..r.label..), r.accuracy = 0.001)
  
  title = paste(font1, ' vs ', font2)
  
  return(list(mean_plot = mean_plot, sd_plot = sd_plot, title = title))
}

get_foveal_crowding_vs_age <- function(crowding) {
  t <- crowding %>% 
    filter(!is.na(age), targetEccentricityXDeg == 0)
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Compute correlation and regression metrics
    t <- t %>% mutate(X = age, Y = 10^(log_crowding_distance_deg))
    r_value <- cor(t$X, t$Y, method = "pearson", use = "complete.obs")
    lm_fit <- lm(Y ~ X, data = t)
    slope <- coef(lm_fit)[["X"]]
    
    # Calculate the number of unique grades for color scale
    n_grades <- n_distinct(t$Grade)
    
    # Start building the plot
    if (n_grades > 1) {
      t$Grade = as.character(t$Grade)
      p <- ggplot(t, aes(x = X, y = Y, color = Grade, shape = conditionName))
    } else {
      p <- ggplot(t, aes(x = X, y = Y, shape = conditionName))
    }
    
    # Add plot components
    p <- p +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Regression line in black
      annotate(
        "text",
        x = max(t$X, na.rm = TRUE) * 1.3,  # Adjusted for better placement
        y = max(t$Y, na.rm = TRUE) * 0.9,
        label = paste0(
          "slope = ", round(slope, 2),
          "\nR = ", round(r_value, 2),
          "\nN = ", nrow(t)
        ),
        hjust = 1, vjust = 1, size = 4, color = "black"
      ) +
      scale_y_log10() +
      scale_x_continuous(breaks = floor(min(t$age)): ceiling(max(t$age))) + 
      annotation_logticks(
      sides = "l", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
      theme_bw() +
      labs(
        title = 'Foveal crowding vs age\ncolored by grade',
        x = 'Age',
        y = 'Foveal crowding (deg)'
      ) +
      color_scale(n = n_grades) +  # Dynamically apply the color scale
      plt_theme +
      theme(
        legend.position = "top"
      ) +
      guides(shape = guide_legend(title = '', ncol = 1))
    
    # Handle Skilled reader?
    # if (n_distinct(t$`Skilled reader?`) > 1) {
    #   p <- p + 
    #     geom_point(aes(shape = `Skilled reader?`)) +
    #     scale_shape_manual(values = c(4, 19,1))
    # }
    
    return(p)
  }
}

get_peripheral_crowding_vs_age <- function(crowding) {
  t <- crowding %>%
    filter(!is.na(age), targetEccentricityXDeg != 0) %>%
    group_by(participant, age, Grade, `Skilled reader?`, block, conditionName) %>%
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE)) %>%
    ungroup()
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Add regression line, compute slope and R
    t <- t %>% mutate(Y = 10^(log_crowding_distance_deg))
    regression <- lm(Y ~ age, data = t)
    slope <- coef(regression)[["age"]]
    r_value <- cor(t$age, t$Y, use = "complete.obs")
    N <- nrow(t)
    
    # Calculate the number of unique grades for color scale
    n_grades <- n_distinct(t$Grade)
    
    # Define plot aesthetics based on Grade
    if (n_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = Y, color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = Y))
    }
    
    # Plot adjustments
    xMin <- min(t$age, na.rm = TRUE)
    xMax <- max(t$age, na.rm = TRUE)
    yMin <- min(t$Y, na.rm = TRUE)
    yMax <- max(t$Y, na.rm = TRUE)
    
    p <- p +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line in black
      geom_point(aes(shape = conditionName), size = 3) +  # Add points
      theme_bw() +
      color_scale(n = n_grades) +  # Apply the gray-to-black color scale
      labs(
        title = "Peripheral crowding vs age\ncolored by grade",
        x = "Age",
        y = "Peripheral crowding (deg)"
      ) +
      scale_y_log10() +
      scale_x_continuous(breaks = xMin: xMax) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      )+
      annotate(
        "text",
        x = xMax,  # Slightly offset for better visibility
        y = yMax * 0.95,  # Adjust placement to the top-left
        label = paste0(
          "slope = ", round(slope, 2),
          "\nR = ", round(r_value, 2),
          "\nN = ", N
        ),
        hjust = 1, vjust = 1, size = 4, color = "black"
      ) +
      plt_theme +
      theme(
        legend.position = "top"
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

get_repeatedLetter_vs_age <- function(repeatedLetters) {
  t <- repeatedLetters %>% filter(!is.na(age))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Calculate the number of unique grades for color scale
    n_grades <- n_distinct(t$Grade)
    
    # Define the plot aesthetics based on Grade
    if (n_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg)))
    }
    
    # Build the plot
    p <- p + 
      geom_point(size = 3) +  # Add points
      scale_y_log10() + 
      scale_x_continuous(breaks = floor(min(t$age)): ceiling(max(t$age))) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      theme_bw() +
      color_scale(n = n_grades) +  # Apply the gray-to-black color scale
      labs(
        title = 'Repeated-letter crowding vs age\ncolored by grade',
        x = 'Age',
        y = 'Repeated-letter crowding (deg)'
      ) +
      theme(
        plot.title = element_text(size = 16, hjust = 0.5),
        legend.position = "top",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    # # Add shapes for Skilled Reader if applicable
    # if (n_distinct(t$`Skilled reader?`) > 1) {
    #   p <- p + 
    #     geom_point(aes(shape = `Skilled reader?`)) +
    #     scale_shape_manual(values = c(4, 19,1))
    # }
    
    return(p)
  }
}

get_crowding_vs_repeatedLetter <- function(crowding, repeatedLetters) {
  if (nrow(crowding) == 0 | nrow(repeatedLetters) == 0) {
    return(list(NULL, NULL))
  }
  
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% 
    filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant, age, Grade, `Skilled reader?`, block, conditionName) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE)) %>% 
    ungroup()
  
  foveal_vs_repeatedLetters <- repeatedLetters %>%
    rename('repeatedLetters' = 'log_crowding_distance_deg') %>% 
    select(participant, repeatedLetters) %>% 
    inner_join(foveal, by = 'participant') %>% 
    mutate(age = format(age, nsmall = 2))
  
  p <- NULL
  p1 <- NULL
  
  if (nrow(foveal_vs_repeatedLetters) > 0) {
    # Count distinct grades for the color scale
    n_grades <- n_distinct(foveal_vs_repeatedLetters$Grade)
    
    if (n_grades > 1) {
      foveal_vs_repeatedLetters <- foveal_vs_repeatedLetters %>% mutate(Grade = as.character(Grade))
      
      p1 <- ggplot(foveal_vs_repeatedLetters, aes(x = 10^(log_crowding_distance_deg), 
                                                  y = 10^(repeatedLetters), 
                                                  color = Grade,
                                                  shape = conditionName)) +
        geom_point() +
        scale_y_log10() + 
        scale_x_log10() + 
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        theme_bw() +
        coord_fixed() + 
        color_scale(n = n_grades) +  # Apply the color_scale function with n_grades
        labs(title = 'Repeated-letter crowding vs foveal crowding\ncolored by grade',
             x = 'Foveal crowding (deg)',
             y = 'Repeated-letter crowding (deg)')
    }
    
    if (n_distinct(foveal_vs_repeatedLetters$age) > 1) {
      foveal_vs_repeatedLetters <- foveal_vs_repeatedLetters %>% mutate(age = format(age, nsmall = 2))
      
      p <- ggplot(foveal_vs_repeatedLetters, aes(x = 10^(log_crowding_distance_deg), 
                                                 y = 10^(repeatedLetters), 
                                                 color = age)) +
        geom_point() +
        scale_y_log10() + 
        scale_x_log10() + 
        coord_fixed() + 
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        theme_bw() +
        labs(title = 'Repeated-letter crowding vs foveal crowding\ncolored by age',
             x = 'Foveal crowding (deg)',
             y = 'Repeated-letter crowding (deg)')
    } else {
      p <- ggplot(foveal_vs_repeatedLetters, aes(x = 10^(log_crowding_distance_deg), 
                                                 y = 10^(repeatedLetters))) +
        geom_point() +
        scale_y_log10() + 
        scale_x_log10() + 
        coord_fixed() + 
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        theme_bw() +
        labs(title = 'Repeated-letter crowding vs foveal crowding\ncolored by age',
             x = 'Foveal crowding (deg)',
             y = 'Repeated-letter crowding (deg)')
    }
    
    # Add shape for Skilled Reader if applicable
    if (n_distinct(foveal_vs_repeatedLetters$`Skilled reader?`) > 1) {
      p <- p + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19,1))
      p1 <- p1 + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19,1))
    }
  }
  
  return(list(age = p, grade = p1))
}

get_foveal_acuity_diag <- function(crowding, acuity) {

  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  
  if (nrow(foveal) == 0 | nrow(acuity) == 0) {
    return(list(
      periphreal = NULL,
      foveal = NULL
    ))
  } else {

    p <- NULL
    p1 <- NULL
    
    foveal_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>%
      filter(targetEccentricityXDeg == 0) %>%
      select(participant, log_acuity, conditionName) %>%
      inner_join(foveal, by = 'participant') %>%  # Get age from foveal
      mutate(
        conditionName = paste0(conditionName.x, ' vs ', conditionName.y),
        age = as.numeric(age),  # Convert age to numeric
        X = 10^log_crowding_distance_deg,
        Y = 10^log_acuity
      )
    
    
    peripheral_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>%
      filter(targetEccentricityXDeg != 0) %>%
      select(participant, log_acuity, conditionName) %>%
      inner_join(foveal, by = 'participant') %>%  # Get age from foveal
      mutate(
        conditionName = paste0(conditionName.x, ' vs ', conditionName.y),
        age = as.numeric(age),  # Convert age to numeric
        X = 10^log_crowding_distance_deg,
        Y = 10^log_acuity
      )
  
    if (nrow(foveal_acuity) != 0) {
      
      r_value <- cor(foveal_acuity$X, foveal_acuity$Y, method = "pearson", use = "complete.obs")
      lm_fit <- lm(Y ~ X, data = foveal_acuity)
      slope <- coef(lm_fit)[["X"]]
      
      # Compute \( R_{\text{factor out age}} \)
      if ("age" %in% colnames(foveal_acuity) && sum(!is.na(foveal_acuity$age)) > 0) {
        pcor <- ppcor::pcor(foveal_acuity %>% filter(!is.na(age)) %>% select(X, Y, age))
        R_factor_out_age <- round(pcor$estimate[2, 1], 2)
      } else {
        R_factor_out_age <- NA
      }
      
      # Count distinct grades for the color scale
      n_grades <- n_distinct(foveal_acuity$Grade)
      
      # Generate the plot
      p <- ggplot(foveal_acuity, aes(x = X, y = Y)) +
        geom_point(aes(color = Grade, shape = conditionName), size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Regression line in black
        annotate(
          "text",
          x = max(foveal_acuity$X, na.rm = TRUE) * 0.8,  # Adjusted for better placement
          y = max(foveal_acuity$Y, na.rm = TRUE) * 0.8,
          label = paste0(
            "N = ", nrow(foveal_acuity),
            "\nR = ", round(r_value, 2),
            "\nR_factor_out_age = ", R_factor_out_age,
            "\nslope = ", round(slope, 2)
          ),
          hjust = 1, vjust = 1, size = 4, color = "black"
        ) +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        color_scale(n = n_grades) +  # Apply the color scale
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        labs(
          title = 'Foveal acuity vs foveal crowding',
          x = 'Foveal crowding (deg)',
          y = 'Foveal acuity (deg)'
        ) +
        coord_fixed() +
        guides(color = guide_legend(title = 'Grade'), 
               shape = guide_legend(title = '', ncol = 1)) +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = "top",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )
    }
    
    if (nrow(peripheral_acuity) != 0) {
      
      r_value <- cor(peripheral_acuity$X, peripheral_acuity$Y, method = "pearson", use = "complete.obs")
      lm_fit <- lm(Y ~ X, data = peripheral_acuity)
      slope <- coef(lm_fit)[["X"]]
      
      # Compute \( R_{\text{factor out age}} \)
      if ("age" %in% colnames(peripheral_acuity) && sum(!is.na(peripheral_acuity$age)) > 0) {
        pcor <- ppcor::pcor(peripheral_acuity %>% filter(!is.na(age)) %>% select(X, Y, age))
        R_factor_out_age <- round(pcor$estimate[2, 1], 2)
      } else {
        R_factor_out_age <- NA
      }
      
      # Count distinct grades for the color scale
      n_grades <- n_distinct(peripheral_acuity$Grade)
      
      # Generate the plot
      p <- ggplot(peripheral_acuity, aes(x = X, y = Y)) +
        geom_point(aes(color = Grade, shape = conditionName), size = 3) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Regression line in black
        annotate(
          "text",
          x = max(peripheral_acuity$X, na.rm = TRUE) * 0.8,  # Adjusted for better placement
          y = max(peripheral_acuity$Y, na.rm = TRUE) * 0.8,
          label = paste0(
            "N = ", nrow(foveal_acuity),
            "\nR = ", round(r_value, 2),
            "\nR_factor_out_age = ", R_factor_out_age,
            "\nslope = ", round(slope, 2)
          ),
          hjust = 1, vjust = 1, size = 4, color = "black"
        ) +
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        color_scale(n = n_grades) +  # Apply the color scale
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        labs(
          title = 'Peripheral acuity vs foveal crowding',
          x = 'Foveal crowding (deg)',
          y = 'Peripheral acuity (deg)'
        ) +
        coord_fixed() +
        guides(color = guide_legend(title = 'Grade'), 
               shape = guide_legend(title = '', ncol = 1)) +
        theme(
          plot.title = element_text(size = 16, hjust = 0.5),
          legend.position = "top",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )
    }
    
    return(list(
      foveal = p,
      perpheral = p1
    ))
  }
}


get_foveal_peripheral_diag <- function(crowding) {
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, age, Grade, `Skilled reader?`, block, conditionName) %>%
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE)) %>%
    ungroup()
  
  if (nrow(foveal) == 0 | nrow(peripheral) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    p <- NULL
    p1 <- NULL
    
    # Combine foveal and peripheral data
    t <- foveal %>%
      rename(foveal = log_crowding_distance_deg) %>%
      select(foveal, participant, conditionName) %>%
      inner_join(peripheral, by = "participant") %>%
      rename(peripheral = log_crowding_distance_deg) %>%
      mutate(
        conditionName = paste0(conditionName.x, ' vs ', conditionName.y),
        age = ifelse(is.na(age), 0, age),  # Handle missing ages
        N = paste0("N = ", n()),
        foveal_y = 10^foveal,
        peripheral_x = 10^peripheral,
        Grade = as.character(Grade)
      )
    
    regression <- lm(peripheral_x ~ foveal_y, data = t)
    slope <- coef(regression)[["foveal_y"]]
    r_value <- cor(t$foveal_y, t$peripheral_x, use = "complete.obs")
    N <- nrow(t)
    
    if ("age" %in% colnames(t) && any(!is.na(t$age))) {
      pcor <- ppcor::pcor(t %>% select(foveal_y, peripheral_x, age))
      R_factor_out_age <- round(pcor$estimate[2, 1], 2)
    } else {
      R_factor_out_age <- NA
    }
    
    n_grades <- n_distinct(t$Grade)
    
    # Plot for grade
    if (n_distinct(t$Grade) > 1) {
      p1 <- ggplot(t, aes(
        y = peripheral_x,
        x = foveal_y,
        color = Grade,
        shape = conditionName
      )) 
    } else {
      p1 <- ggplot(t, aes(
        y = peripheral_x,
        x = foveal_y,
        shape = conditionName
      )) 
    }
      
    p1 <- p1 +
        geom_point(size =3) + 
        geom_smooth(method = "lm", se = FALSE, color = "black") +  # Black regression line
        scale_x_log10() +
        scale_y_log10() +
        theme_bw() +
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        labs(
          y = "Peripheral crowding (deg)",
          x = "Foveal crowding (deg)",
          title = "Peripheral crowding vs foveal crowding\ncolored by grade"
        ) +
        coord_fixed() +
        color_scale(n = n_grades) +  # Apply color scale
        annotate(
          "text",
          x = max(t$foveal_y, na.rm = TRUE) * 0.1,  # Moved further to the left
          y = max(t$peripheral_x, na.rm = TRUE),    # Keep at the same vertical position
          label = paste0(
            "N = ", N,
            "\nR = ", round(r_value, 2),
            "\nR_factor_out_age = ", R_factor_out_age,
            "\nslope = ", round(slope, 2)
          ),
          hjust = 0, vjust = 1, size = 4, color = "black"
        ) + 
        guides(color = guide_legend(title = 'Grade'),
               shape = guide_legend(title = '',
                                    ncol=1))
    
    
    # Plot for age
    if (n_distinct(t$age) > 1) {
      p <- ggplot(t, aes(
        y = foveal_y,
        x = peripheral_x,
        color = age,
        shape = conditionName
      ))
    } else {
      p <- ggplot(t, aes(
        y = foveal_y,
        x = peripheral_x
      ))
    }
    
    p <- p +
      geom_point(size =3) + 
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Black regression line
      scale_x_log10() +
      scale_y_log10() +
      theme_bw() +
      annotation_logticks(
        sides = "bl", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      labs(
        y = "Foveal crowding (deg)",
        x = "Peripheral crowding (deg)",
        title = "Foveal vs peripheral crowding colored by age"
      ) +
      coord_fixed() +
      annotate(
        "text",
        x = max(t$peripheral_x, na.rm = TRUE) * 0.9,
        y = max(t$foveal_y, na.rm = TRUE) * 0.9,
        label = paste0(
          "N = ", N,
          "\nR = ", round(r_value, 2),
          "\nR_factor_out_age = ", R_factor_out_age,
          "\nslope = ", round(slope, 2)
        ),
        hjust = 1, vjust = 1, size = 4, color = "black"
      ) + 
      guides(color = guide_legend(title = 'Grade'),
             shape = guide_legend(title = '',
                                  ncol = 1))
    
    # Add shapes for Skilled Reader if applicable
    # if (n_distinct(t$`Skilled reader?`) == 1) {
    #   p <- p + geom_point(size = 3)
    #   p1 <- p1 + geom_point(size = 3)
    # } else {
    #   p <- p +
    #     geom_point(aes(shape = `Skilled reader?`), size = 3) +
    #     scale_shape_manual(values = c(4, 19,1))
    #   p1 <- p1 +
    #     geom_point(aes(shape = `Skilled reader?`), size = 3) +
    #     scale_shape_manual(values = c(4, 19,1))
    # }
    
    return(list(age = p, grade = p1))
  }
}

plot_crowding_vs_age <- function(crowding){

  crowding <- crowding %>%
    mutate(ageN = as.numeric(age)) %>% 
    filter(!is.na(ageN))

  if (nrow(crowding) == 0) {
    return(NULL)
  } 
  
  foveal <- crowding %>% 
    filter(questType == 'Foveal crowding')
  # Only right visual field
  peripheral <- crowding %>% 
    filter(targetEccentricityXDeg > 0)
  
  age_values <- seq(4, 10, by = 0.1)  
  log_crowding_distance_deg <- 0.0535 + 1.5294 * (age_values^-1.9438)
  
  foveal_curve <- data.frame(ageN = age_values, log_crowding_distance_deg = log_crowding_distance_deg)

  foveal_stats <- foveal %>% 
    do(fit = lm(log_crowding_distance_deg ~ ageN, data = .)) %>%
    transmute(coef = map(fit, tidy)) %>%
    unnest(coef) %>%
    filter(term == "ageN") %>%  
    mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
    select(slope)
  
  # calculate correlation foveal data
  foveal_corr <- foveal %>% 
    summarize(cor = format(round(cor(ageN,log_crowding_distance_deg,use = "complete.obs"),2),nsmall=2)) %>% 
    select(cor)
  
  # calculate slope peripheral data
  if (nrow(peripheral) > 0) {
    peripheral_stats <- peripheral %>% 
      do(fit = lm(log_crowding_distance_deg ~ ageN, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == "ageN") %>%  
      mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
      select(slope)
    # calculate correlation peripheral data
    peripheral_corr <- peripheral %>% 
      summarize(cor = format(round(cor(ageN,log_crowding_distance_deg,use = "complete.obs"),2),nsmall=2)) %>% 
      select(cor)
  }
 
  ggplot(data = crowding, aes(x = ageN, 
                              y = 10^(log_crowding_distance_deg),
                              color = questType
                              )) + 
    annotation_logticks(
      sides = "l", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    geom_point(aes(shape = conditionName))+ 
    geom_smooth(method='lm', se=F) + 
    ggpp::geom_text_npc(
      size = 12/.pt,
      aes(npcx = "right",
          npcy = "top",
          label = paste0('Foveal: slope=', foveal_stats$slope, ', R=', foveal_corr, '\n',
                         'Peripheral: slope=', peripheral_stats$slope, ', R=', peripheral_corr))) +
    scale_y_log10() + 
    scale_x_continuous(breaks = floor(min(crowding$ageN)) : ceiling(max(crowding$ageN))) + 
    guides(color=guide_legend(title = ''),
           shape=guide_legend(title = '',
                              ncol=1)) + 
    labs(x = 'Age',
         y = 'Crowding distance (deg)',
         title = 'Foveal and peripheral\ncrowding vs age')
}

