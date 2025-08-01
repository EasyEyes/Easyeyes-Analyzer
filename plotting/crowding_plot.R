source('./constant.R')


crowding_by_side <- function(crowding) {
  print('insdie crowding_by_side')
  crowding <- crowding %>% filter(targetEccentricityXDeg != 0, !is.na(conditionName)) %>% 
    mutate(side = ifelse(targetEccentricityXDeg < 0, 'L', 'R'),
           XDeg = abs(targetEccentricityXDeg)) %>% 
    group_by(participant, side, XDeg, font) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm=T),
              bouma_factor = mean(bouma_factor, na.rm=T),
              .groups="drop")
  
  crowding_L <- crowding %>%
    filter(side == "L") %>%
    select(-side)
  crowding_R <- crowding %>% 
    filter(side == "R") %>%
    select(-side)

  crowding_L_R <- crowding_L %>% 
    full_join(crowding_R, by = c("participant","font", 'XDeg')) %>% 
    rename("bouma_factor_Left" = "bouma_factor.x",
           "bouma_factor_Right" = "bouma_factor.y",
           "log_crowding_distance_deg_Left" = "log_crowding_distance_deg.x",
           "log_crowding_distance_deg_Right" = "log_crowding_distance_deg.y") %>%
    filter(!is.na(log_crowding_distance_deg_Left) & !is.na(log_crowding_distance_deg_Right)) %>% 
    mutate(conditionName =  paste0(font, ' -', XDeg, ' vs ', font, ' +', XDeg)) %>% 
  return(crowding_L_R)
}
crowding_scatter_plot <- function(crowding_L_R){
 
  if (n_distinct(crowding_L_R$font) < 1) return(NULL)
  if (n_distinct(crowding_L_R$font) == 1 &&
      is.na(unique(crowding_L_R$log_crowding_distance_deg_Left)[1])) {
    return(NULL)
  }
  
  # eccentricity label
  eccs      <- sort(unique(crowding_L_R$XDeg))
  ecc_label <- paste0("X ecc = ", paste(eccs, collapse = ", "), " deg")
  
  # compute per‐participant averages
  df <- crowding_L_R %>%
    group_by(participant, conditionName) %>%
    summarize(
      log_crowding_distance_deg_Left  = mean(log_crowding_distance_deg_Left,  na.rm = TRUE),
      log_crowding_distance_deg_Right = mean(log_crowding_distance_deg_Right, na.rm = TRUE),
      .groups = "drop"
    )
  
  # compute N & Pearson R per condition
  stats_cond <- df %>%
    filter(!is.na(log_crowding_distance_deg_Left),
           !is.na(log_crowding_distance_deg_Right)) %>%
    group_by(conditionName) %>%
    summarize(
      N = n(),
      R = cor(log_crowding_distance_deg_Left, log_crowding_distance_deg_Right,
              use = "complete.obs"),
      .groups = "drop"
    ) %>%
    mutate(
      # pull out the two numbers (handles both hyphens and unicode dashes)
      eccs = str_extract_all(conditionName, "[-−]?\\d+"),
      left_ecc  = map_chr(eccs, 1),
      right_ecc = map_chr(eccs, 2),
      fontName = sub(" [-−]?[0-9]+.*", "", conditionName),
      short = paste0(
        left_ecc, " vs ", right_ecc, " deg, ",
        fontName, ", N=", N, ", R=", sprintf("%.2f", R)
      )
    ) %>%
    select(conditionName, short)
  
  df <- df %>%
    left_join(stats_cond, by = "conditionName") %>%
    mutate(
      shortConditionName = factor(conditionName,
                                  levels = stats_cond$conditionName,
                                  labels = stats_cond$short)
    )

  
  # build the detailed summary caption
  N_left  <- nrow(df %>% filter(!is.na(log_crowding_distance_deg_Left)))
  N_right <- nrow(df %>% filter(!is.na(log_crowding_distance_deg_Right)))
  N_both  <- nrow(df %>% filter(!is.na(log_crowding_distance_deg_Left),
                                !is.na(log_crowding_distance_deg_Right)))
  
  stats_all <- df %>%
    mutate(
      log_delta = log_crowding_distance_deg_Left - log_crowding_distance_deg_Right,
      log_mean  = (log_crowding_distance_deg_Left + log_crowding_distance_deg_Right) / 2
    ) %>%
    summarize(
      mean_log_delta = mean(log_delta, na.rm = TRUE),
      sd_log_delta   = sd(log_delta,   na.rm = TRUE),
      mean_left      = mean(log_crowding_distance_deg_Left,  na.rm = TRUE),
      sd_left        = sd(log_crowding_distance_deg_Left,    na.rm = TRUE),
      mean_right     = mean(log_crowding_distance_deg_Right, na.rm = TRUE),
      sd_right       = sd(log_crowding_distance_deg_Right,   na.rm = TRUE),
      mean_avg       = mean(log_mean,  na.rm = TRUE),
      sd_avg         = sd(log_mean,    na.rm = TRUE),
      .groups="drop"
    )
  sdTestRetest <- stats_all$sd_log_delta / sqrt(2)
  sdIndividual <- sqrt(stats_all$sd_avg^2 - 0.5 * sdTestRetest^2)
  
  summ_label <- paste0(
    "N= ",    N_both, "\n",
    "Left:  n=", N_left, ", mean=", format(round(stats_all$mean_left,2),nsmall=2),
    ", sd=", format(round(stats_all$sd_left,2),nsmall=2), "\n",
    "Right: n=", N_right, ", mean=", format(round(stats_all$mean_right,2),nsmall=2),
    ", sd=", format(round(stats_all$sd_right,2),nsmall=2), "\n",
    "Diff:  n=", N_both, ", mean=", format(round(stats_all$mean_log_delta,2),nsmall=2),
    ", sd=", format(round(stats_all$sd_log_delta,2),nsmall=2), "\n",
    "Avg:   n=", N_both, ", mean=", format(round(stats_all$mean_avg,2),nsmall=2),
    ", sd=", format(round(stats_all$sd_avg,2),nsmall=2), "\n",
    "TestRetest: sdDiff/sqrt(2)=", format(round(sdTestRetest,2),nsmall=2), "\n",
    "Individual: sqrt(sdAvg^2 - 0.5*sdTestRetest^2)=", format(round(sdIndividual,2),nsmall=2)
  )
  
  # axis limits
  minXY <- min(df$log_crowding_distance_deg_Left,
               df$log_crowding_distance_deg_Right, na.rm = TRUE) * 0.9
  maxXY <- max(df$log_crowding_distance_deg_Left,
               df$log_crowding_distance_deg_Right, na.rm = TRUE) * 1.1
  

  
  # build the plot
  p <- ggplot(df,
              aes(
                x     = 10^(log_crowding_distance_deg_Right),
                y     = 10^(log_crowding_distance_deg_Left),
                color = shortConditionName
              )) +
    geom_point(size = 1) +
    geom_abline(intercept = 0, slope = 1,
                linetype = "dashed", color = "black") +
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
    scale_x_log10(
      name   = "Right crowding distance (deg)",
      limits = c(10^minXY, 10^maxXY),
      breaks = c(0.1, 0.3, 1, 3, 10),
      expand = c(0, 0)
    ) +
    scale_y_log10(
      name   = "Left crowding distance (deg)",
      limits = c(10^minXY, 10^maxXY),
      breaks = c(0.1, 0.3, 1, 3, 10),
      expand = c(0, 0)
    ) +
    annotation_logticks(sides = "bl") +
    coord_fixed() +
    theme_bw() +
    guides(color = guide_legend(title = NULL, ncol = 1)) +
    labs(
      title   = "Left vs right peripheral crowding",
      caption = paste0(ecc_label, "\n", summ_label)
    ) +
    theme(plot.caption = element_text(hjust = 0, size = 12))
  
  return(p)
}




crowding_mean_scatter_plot <- function(crowding_L_R){
  print("Inside crowding_mean_scatter_plot")
  if (n_distinct(crowding_L_R$font) < 1) {
    return(ggplot() + 
             xlab("Left Bouma factor") + 
             ylab("Right Bouma factor") )
  }
  
  t <- crowding_L_R %>% group_by(conditionName) %>% summarize(avg_left_deg = mean(log_crowding_distance_deg_Left),
                                                     avg_right_deg = mean(log_crowding_distance_deg_Right),
                                                     .groups="drop")
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
    # ggpp::geom_text_npc(
    #   aes(npcx = "left",
    #       npcy = "top",
    #       label = paste0("italic('R=')~", corrrelation)), 
    #   parse = T) + 
    ggpp::geom_text_npc(
      # turn off inherited x/y/color
      inherit.aes = FALSE,
      
      # npc positions as arguments
      npcx   = "left",
      npcy   = "top",
      
      # your computed label as a constant
      label  = paste0("italic('R=')~", corrrelation),
      parse  = TRUE
    ) +
  
    # ggpp::geom_text_npc(
    #   aes(npcx = "left",
    #       npcy = "bottom",
    #       label = paste0("italic('N=')~", dplyr::n_distinct(crowding_L_R$participant))), 
    #   parse = T)
    ggpp::geom_text_npc(
      inherit.aes = FALSE,
      npcx   = "left",
      npcy   = "bottom",
      label  = paste0("italic('N=')~",  dplyr::n_distinct(crowding_L_R$participant)),
      parse  = TRUE
    )
  
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
      SD = sd(log10(bouma_factor), na.rm = TRUE),
      .groups="drop"
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
    filter(!is.na(age), targetEccentricityXDeg == 0) %>% 
    filter(font != 'Sloan.woff2')
  
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
      p <- ggplot() + geom_point(data=t, aes(x = X, y = Y, color = Grade, shape = conditionName), size = 3)
    } else {
      p <- ggplot() + geom_point(data=t, aes(x = X, y = Y, shape = conditionName), size = 3)
    }
    p <- p + color_scale(n = n_grades)# Dynamically apply the color scale
    
    age_values <- seq(3, 11, by = 0.05)  # You can adjust the increment if needed
    
    foveal_curve_old <- data.frame(ageN = age_values, 
                                   crowding_distance_deg = 0.0535 + 1.5294 * (age_values^-1.9438))
    foveal_curve_new <- data.frame(ageN = age_values, 
                                   crowding_distance_deg = 0.0535 + 2.4380 * (age_values^-2.2616))
    foveal_curve_denis <- data.frame(ageN = age_values, 
                                     crowding_distance_deg = 10^(-1.27165 + 0.72863 * 10^(-((age_values / 10.77473)^1.62730))))

    p <- p +
      # new_scale_color() +
      # geom_line(data = foveal_curve_old, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "Old fit")) +
      # geom_line(data = foveal_curve_new, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "New fit")) +
      # geom_line(data = foveal_curve_denis, linetype = 'dashed', aes(x = ageN, 
      #                                                               y = crowding_distance_deg,
      #                                                               color = "Denis fit")) +
      # scale_color_manual(values = c("Old fit" = "green", "New fit" = "blue", "Denis fit" = "red")) +
      ggpp::geom_text_npc(
        aes(npcx = "right",
            npcy = "top",
            label = paste0(
          "slope = ", round(slope, 2),
          "\nR = ", round(r_value, 2),
          "\nN = ", nrow(t)
        )),
        size = 4, color = "black"
      ) +
      scale_y_log10() +
      #scale_x_continuous(breaks = 3: ceiling(max(t$age))) + 
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
    uniq <- n_distinct(t$age)
    if (uniq > 1) {
      p <- p +
        scale_x_continuous(
          breaks = pretty_breaks(n = 5),
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        theme(
          axis.text.x = element_text(
            angle = ifelse(uniq > 4, 45, 0),
            hjust  = ifelse(uniq > 4, 1, 0.5)
          )
        )
    } else {
      a <- unique(t$age)
      p <- p +
        scale_x_continuous(
          breaks = a,
          limits = a + c(-1, 1),
          expand = c(0, 0)
        )
    }
    
    return(p)
  }
}

get_peripheral_crowding_vs_age <- function(crowding) {
  t <- crowding %>%
    filter(!is.na(age), targetEccentricityXDeg != 0) %>%
    group_by(participant, age, Grade, `Skilled reader?`,conditionName, targetEccentricityXDeg) %>%
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
              .groups="drop")
  
  if (nrow(t) == 0) {
    return(list(NULL, NULL))
  } else {
    
    eccs     <- sort(unique(t$targetEccentricityXDeg))
    eccs_int <- as.integer(round(eccs))
    ecc_label <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
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
      t$Grade = as.character(t$Grade)
      p1 <- ggplot() + geom_point(data=t, aes(x = age, y = Y, color = Grade, shape = conditionName), size = 3)
    } else {
      p1 <- ggplot() + geom_point(data=t, aes(x = age, y = Y, shape = conditionName), size = 3)
    }
    p1 <- p1 + color_scale(n = n_grades)
    
    # Plot adjustments
    xMin <- min(t$age, na.rm = TRUE)
    xMax <- max(t$age, na.rm = TRUE)
    yMin <- min(t$Y, na.rm = TRUE)
    yMax <- max(t$Y, na.rm = TRUE)
    
    age_values <- seq(3, 11, by = 0.05)  # You can adjust the increment if needed
    
    foveal_curve_old <- data.frame(ageN = age_values, 
                                   crowding_distance_deg = 0.0535 + 1.5294 * (age_values^-1.9438))
    foveal_curve_new <- data.frame(ageN = age_values, 
                                   crowding_distance_deg = 0.0535 + 2.4380 * (age_values^-2.2616))
    foveal_curve_denis <- data.frame(ageN = age_values, 
                                     crowding_distance_deg = 10^(-1.27165 + 0.72863 * 10^(-((age_values / 10.77473)^1.62730))))
    p1 <- p1 +
      # new_scale_color() + 
      # geom_line(data = foveal_curve_old, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "Old fit")) +
      # geom_line(data = foveal_curve_new, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "New fit")) +
      # geom_line(data = foveal_curve_denis, linetype = 'dashed', aes(x = ageN, 
      #                                                               y = crowding_distance_deg,
      #                                                               color = "Denis fit")) +
      # scale_color_manual(values = c("Old fit" = "green", "New fit" = "blue", "Denis fit" = "red")) +
      theme_bw() +
      # color_scale(n = n_grades) +  # Apply the gray-to-black color scale
      labs(
        title = "Peripheral crowding vs age\ncolored by grade",
        x = "Age",
        y = "Peripheral crowding (deg)"
      ) +
      scale_y_log10() +
      #scale_x_continuous(breaks = floor(xMin): ceiling(xMax)) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      )+
      ggpp::geom_text_npc(
        aes(npcx = "right",
            npcy = "top",
            label = paste0(
              ecc_label,
              "\nslope = ", round(slope, 2),
              "\nR = ", round(r_value, 2),
              "\nN = ",N
            )),
        size = 4, color = "black"
      ) +
      plt_theme +
      theme(
        legend.position = "top"
      ) +
      guides(color = guide_legend(title = 'Grade'),
             shape = guide_legend(title = '', ncol = 1))
    
    uniq1 <- n_distinct(t$age)
    if (uniq1 > 1) {
      p1 <- p1 +
        scale_x_continuous(
          breaks = pretty_breaks(n = 5),
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        theme(axis.text.x = element_text(
          angle = ifelse(uniq1 > 4, 45, 0),
          hjust = ifelse(uniq1 > 4, 1, 0.5)
        ))
    } else {
      a1 <- unique(t$age)
      p1 <- p1 +
        scale_x_continuous(
          breaks = a1,
          limits = a1 + c(-1, 1),
          expand = c(0, 0)
        )
    }
    
    t <- t %>% 
      group_by(participant, age, Grade, `Skilled reader?`) %>%
      summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
                .groups="drop") %>% 
      mutate(Y = 10^(log_crowding_distance_deg)) %>% 
      filter(!is.na(Y))
    
    if (n_grades > 1) {
      t$Grade = as.character(t$Grade)
      p2 <- ggplot() + geom_point(data=t, aes(x = age, y = Y, color = Grade), size = 3)
    } else {
      p2 <- ggplot() + geom_point(data=t, aes(x = age, y = Y), size = 3)
    }
    
    p2 <- p2 + color_scale(n = n_grades)
    
    # Plot adjustments
    xMin <- min(t$age, na.rm = TRUE)
    xMax <- max(t$age, na.rm = TRUE)
    yMin <- min(t$Y, na.rm = TRUE)
    yMax <- max(t$Y, na.rm = TRUE)
    
    p2 <- p2 +
      # new_scale_color() + 
      # geom_line(data = foveal_curve_old, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "Old fit")) +
      # geom_line(data = foveal_curve_new, linetype = 'dashed', aes(x = ageN, 
      #                                                             y = crowding_distance_deg,
      #                                                             color = "New fit")) +
      # geom_line(data = foveal_curve_denis, linetype = 'dashed', aes(x = ageN, 
      #                                                               y = crowding_distance_deg,
      #                                                               color = "Denis fit")) +
      # scale_color_manual(values = c("Old fit" = "green", "New fit" = "blue", "Denis fit" = "red")) +
      theme_bw() +
      # color_scale(n = n_grades) +  # Apply the gray-to-black color scale
      labs(
        subtitle = "Geometric average of left and right thresholds",
        title = "Peripheral crowding vs age\ncolored by grade",
        x = "Age",
        y = "Peripheral crowding (deg)"
      ) +
      scale_y_log10() +
      #scale_x_continuous(breaks = floor(xMin): ceiling(xMax)) + 
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      ggpp::geom_text_npc(
        aes(npcx = "right",
            npcy = "top",
            label = paste0(
              ecc_label,
              "\nslope = ", round(slope, 2),
              "\nR = ", round(r_value, 2),
              "\nN = ", nrow(t)
            )),
        size = 4, color = "black"
      ) +
      plt_theme +
      theme(
        legend.position = "top"
      ) +
      guides(color = guide_legend(title = 'Grade'),
             shape = guide_legend(title = '', ncol = 1))
    
    uniq2 <- n_distinct(t$age)
    if (uniq2 > 1) {
      p2 <- p2 +
        scale_x_continuous(
          breaks = pretty_breaks(n = 5),
          expand = expansion(mult = c(0.05, 0.05))
        ) +
        theme(axis.text.x = element_text(
          angle = ifelse(uniq2 > 4, 45, 0),
          hjust = ifelse(uniq2 > 4, 1, 0.5)
        ))
    } else {
      a2 <- unique(t$age)
      p2 <- p2 +
        scale_x_continuous(
          breaks = a2,
          limits = a2 + c(-1, 1),
          expand = c(0, 0)
        )
    }
    
    return(list(p1,p2))
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
    group_by(participant, age, Grade, `Skilled reader?`, conditionName) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
              .groups="drop")
  
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
        Grade = factor(Grade),
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
        Grade = factor(Grade),
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
          title = 'Foveal acuity vs foveal crowding\ncolored by grade',
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
    group_by(participant, age, Grade, `Skilled reader?`) %>%
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
              .groups="drop")
  
  if (nrow(foveal) == 0 | nrow(peripheral) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    
    p <- NULL
    p1 <- NULL
    
    eccs <- sort(unique(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg != 0]))
    eccs_int <- as.integer(round(eccs))
    ecc_label <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
    
    # Combine foveal and peripheral data
    t <- foveal %>%
      rename(foveal = log_crowding_distance_deg) %>%
      select(foveal, participant) %>%
      inner_join(peripheral, by = "participant") %>%
      rename(peripheral = log_crowding_distance_deg) %>%
      mutate(
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
    
    if ("age" %in% colnames(t) & n_distinct(t$age) > 1) {
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
        color = Grade
      )) 
    } else {
      p1 <- ggplot(t, aes(
        y = peripheral_x,
        x = foveal_y
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
          title = "Peripheral crowding vs foveal crowding\ncolored by grade",
          subtitle = "Geometric average of left and right thresholds"
        ) +
        coord_fixed() +
        color_scale(n = n_grades) +  # Apply color scale
        annotate(
          "text",
          x = max(t$foveal_y, na.rm = TRUE) * 0.1,  # Moved further to the left
          y = max(t$peripheral_x, na.rm = TRUE),    # Keep at the same vertical position
          label = paste0(
            "\nR_factor_out_age = ", R_factor_out_age,
            "\n", ecc_label,
            "\nslope = ", round(slope, 2),
            "\nR = ", round(r_value, 2),
            "\nN = ", N
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
        color = age
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
        title = "Foveal vs peripheral crowding colored by age",
        subtitle = "Geometric average of left and right thresholds",
      ) +
      coord_fixed() +
      annotate(
        "text",
        x = max(t$peripheral_x, na.rm = TRUE) * 0.9,
        y = max(t$foveal_y, na.rm = TRUE) * 0.9,
        label = paste0(
          "\nR_factor_out_age = ", R_factor_out_age,
          "\n", ecc_label,
          "\nslope = ", round(slope, 2),
          "\nR = ", round(r_value, 2),
          "\nN = ", N
        ),
        hjust = 1, vjust = 1, size = 4, color = "black"
      ) + 
      guides(color = guide_legend(title = 'Grade'))
    
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
  
  # 1. parse age
  crowding <- crowding %>%
    mutate(ageN = as.numeric(age)) %>% 
    filter(!is.na(ageN))
  
  if (nrow(crowding) == 0) return(NULL)
  
  # 2. build your two summaries
  foveal <- crowding %>% 
    filter(questType == "Foveal crowding") %>% 
    group_by(participant, questType, ageN) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
              .groups = "drop")
  
  peripheral <- crowding %>% 
    # Why only right peripheral?
    filter(targetEccentricityXDeg > 0) %>% 
    group_by(participant, questType, ageN) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
              .groups = "drop")
  
  # 3. slam them together for plotting & stats
  t <- bind_rows(foveal, peripheral)
  
  # 4. compute slopes, R, N, ecc label
  eccs_int  <- round(unique(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg > 0]))
  ecc_label <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
  N_f <- nrow(foveal); N_p <- nrow(peripheral)
  
  label = ""
  if (nrow(foveal) > 0) {
    fit_f <- lm(log_crowding_distance_deg ~ ageN, data = foveal)
    slope_f <- round(coef(fit_f)["ageN"], 2)
    R_f     <- round(cor(foveal$ageN, foveal$log_crowding_distance_deg), 2)
    label =  paste0("Foveal: slope=", slope_f, ", R=", R_f, ", N=", N_f)
  }
 
  if (nrow(peripheral) > 0) {
    fit_p <- lm(log_crowding_distance_deg ~ ageN, data = peripheral)
    slope_p <- round(coef(fit_p)["ageN"], 2)
    R_p     <- round(cor(peripheral$ageN, peripheral$log_crowding_distance_deg), 2)
    label <- paste0(
      ifelse(label=="", "", paste0(label, "\n")),
      "Peripheral: slope=", slope_p, ", R=", R_p, ", N=", N_p, ", ", ecc_label
    )
  }
  
  
  # 5. plot *only* the summary frame t
  p <- ggplot(t, aes(x = ageN, y = 10^(log_crowding_distance_deg), color = questType)) +
    annotation_logticks(sides="l", short=unit(2,"pt"), mid=unit(2,"pt"), long=unit(7,"pt")) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    ggpp::geom_text_npc(aes(npcx="right", npcy="top", label = label), size = 12/.pt) +
    scale_y_log10(expand = expansion(mult = c(0.05, 0.05))) +
    labs(
      title    = "Foveal and peripheral\ncrowding vs age",
      subtitle = "Geometric average of left and right thresholds",
      x        = "Age",
      y        = "Crowding distance (deg)",
      color    = ""
    ) +
    guides(shape = guide_none())
  
  # 6. nice x‑axis breaks & angle
  uniq <- n_distinct(t$ageN)
  if (uniq > 1) {
    p <- p + scale_x_continuous(breaks = scales::pretty_breaks(n=5),
                                expand = expansion(mult = c(0.05,0.05))) +
      theme(axis.text.x = element_text(
        angle = ifelse(uniq>4,45,0),
        hjust  = ifelse(uniq>4,1,0.5)
      ))
  } else {
    a <- unique(t$ageN)
    p <- p + scale_x_continuous(breaks = a, limits = a + c(-1,1), expand = c(0,0))
  }
  
  return(p)
}


# plot_crowding_vs_age <- function(crowding){
# 
#   crowding <- crowding %>%
#     mutate(ageN = as.numeric(age)) %>% 
#     filter(!is.na(ageN))
# 
#   if (nrow(crowding) == 0) {
#     return(NULL)
#   } 
#   
#   eccs_periph <- sort(unique(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg > 0]))
#   eccs_int    <- as.integer(round(eccs_periph))
#   ecc_label   <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
#   
#   foveal <- crowding %>% 
#     filter(questType == 'Foveal crowding') %>% 
#     select(participant, questType, ageN,log_crowding_distance_deg)
#   # Only right visual field
#   peripheral <- crowding %>% 
#     filter(targetEccentricityXDeg > 0) %>% 
#     group_by(participant, questType, ageN) %>% 
#     summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm =T))
#   
#   age_values <- seq(4, 10, by = 0.1)
#   log_crowding_distance_deg <- 0.0535 + 1.5294 * (age_values^-1.9438)
# 
#   foveal_curve <- data.frame(ageN = age_values, log_crowding_distance_deg = log_crowding_distance_deg)
#   if (nrow(foveal) > 0) {
#     foveal_stats <- foveal %>% 
#       do(fit = lm(log_crowding_distance_deg ~ ageN, data = .)) %>%
#       transmute(coef = map(fit, tidy)) %>%
#       unnest(coef) %>%
#       filter(term == "ageN") %>%  
#       mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
#       select(slope)
#     foveal_slope <- foveal_stats$slope[1]
#     # calculate correlation foveal data
#     foveal_corr <- foveal %>% 
#       summarize(cor = format(round(cor(ageN,log_crowding_distance_deg,use = "complete.obs"),2),nsmall=2)) %>% 
#       select(cor)
#     foveal_corr <- foveal_corr$cor[1]
#   } else {
#     foveal_slope = NA
#     foveal_corr = NA
#   }
#  
#   
#   # calculate slope peripheral data
#   if (nrow(peripheral) > 0) {
#     model <- lm(log_crowding_distance_deg ~ ageN, data = peripheral)
#     p_slope <- tidy(model) %>%
#       filter(term == "ageN") %>%
#       mutate(slope = format(round(estimate, 2), nsmall = 2)) %>%
#       select(slope)
#     # calculate correlation peripheral data
#     peripheral_corr <- format(round(cor(peripheral$ageN, peripheral$log_crowding_distance_deg,use = "complete.obs"),2),nsmall=2)
#   } else {
#     p_slope = tibble(slope = NA)
#     peripheral_corr = NA
#   }
#   
#  label = paste0('Foveal: slope=', foveal_slope, ', R=', foveal_corr, ', N=', nrow(foveal), '\n',
#                 'Peripheral: slope=', p_slope$slope[1], ', R=', peripheral_corr,  ', N=', nrow(peripheral), ', ' , ecc_label)
#  t <- rbind(foveal, peripheral)
#   p <- ggplot(data = crowding, aes(x = ageN, 
#                               y = 10^(log_crowding_distance_deg),
#                               color = questType
#                               )) + 
#     annotation_logticks(
#       sides = "l", 
#       short = unit(2, "pt"), 
#       mid   = unit(2, "pt"), 
#       long  = unit(7, "pt")
#     ) +
#     geom_point()+ 
#     geom_smooth(method='lm', se=F) + 
#     ggpp::geom_text_npc(
#       size = 12/.pt,
#       aes(npcx = "right",
#           npcy = "top",
#           label = label)) +
#     scale_y_log10() + 
#     # scale_x_continuous(breaks = c(seq(2,18,2), seq(20 , 50, 10))) + 
#     
#     guides(color=guide_legend(title = ''),
#            shape=guide_legend(title = '',
#                               ncol=1)) + 
#     labs(x = 'Age',
#          y = 'Crowding distance (deg)',
#          subtitle = 'Geometric average of left and right thresholds',
#          title = 'Foveal and peripheral\ncrowding vs age')
#   
#   uniq <- n_distinct(crowding$ageN)
#   if (uniq > 1) {
#     p <- p + scale_x_continuous(
#       breaks = scales::pretty_breaks(n = 5),
#       expand = expansion(mult = c(0.05, 0.05))
#     ) +
#       theme(
#         axis.text.x = element_text(
#           angle = ifelse(uniq > 4, 45, 0),
#           hjust = ifelse(uniq > 4, 1, 0.5)
#         )
#       )
#   } else {
#     a <- unique(crowding$ageN)
#     p <- p + scale_x_continuous(
#       breaks = a,
#       limits = a + c(-1, 1),
#       expand = c(0, 0)
#     )
#   }
#   return(p)
# }

