
# Add a plot of reading speed (word/min) vs. letter size (mm).  
# Use log scaling in X and Y. We’ll have two connected points 
# for 30 cm viewing distance and two connected points for 60 
# cm viewing distance. Add error bars for each point. Shift 
# some points horizontally slightly so they don’t occlude each other.
# Use the SE from “Summary across all participants”. 
# For each condition we have m and se. The mean is at 60/10^m. 
# The error bar goes from 60/10^(m+se/2) to 60/10^(m-se/2)
plot_rsvp_vs_x_height <- function(rsvp_speed){
  rsvp_summary <- rsvp_speed %>%
    group_by(participant, conditionName, viewingDistanceDesiredCm) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_summary <- rsvp_summary %>% 
    group_by(conditionName, viewingDistanceDesiredCm) %>% 
    dplyr::summarize(
      m = mean(pm, na.rm = T),
      se = sd(pm)/sqrt(n()), 
      N = n(),
      parameter = "threshold")
  
  rsvp_summary$x_height <- NA
  rsvp_summary$x_height <- ifelse(str_detect(rsvp_summary$conditionName, "1.2 mm"), 
                                  1.2, rsvp_summary$x_height)
  rsvp_summary$x_height <- ifelse(str_detect(rsvp_summary$conditionName, "1.4 mm"), 
                                  1.4, rsvp_summary$x_height)
  pd <- position_dodge(width = 0.005)
  minN <- min(rsvp_summary$N)
  maxN <- max(rsvp_summary$N)
  rsvp_summary <- rsvp_summary %>% 
    mutate(x_height = ifelse(viewingDistanceDesiredCm == 60, x_height + 0.005, x_height))
  N_text <- ifelse(minN == maxN, minN, paste0(minN, "~to~",maxN))
  rsvp_summary
  p1 <- ggplot(rsvp_summary, aes(x = x_height, y = 60/(10^(m)), color = as.factor(viewingDistanceDesiredCm))) + 
    geom_point() + 
    geom_line() +
    theme_bw() + 
    scale_x_log10() + 
    scale_y_log10(limits = c(250, 1000),breaks = c(250, 500, 750, 1000)) + 
    # this is to dodge error bar
    # geom_errorbar(aes(ymin=60/(10^(m-se/2)),
    #                   ymax=60/(10^(m+se/2))), width=0, position = pd) +
    geom_errorbar(aes(ymin=60/(10^(m-se/2)),
                      ymax=60/(10^(m+se/2))), width=0) +
    xlab("x height (mm)") +
    ylab("Reading speed (w/min) ") +
    guides(color = guide_legend(title = "viewing distance (cm)")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  p2 <- ggplot(rsvp_summary, aes(x = x_height, y = 60/(10^(m)), color = as.factor(viewingDistanceDesiredCm))) + 
    geom_point() + 
    geom_line() +
    theme_bw() + 
    scale_y_continuous(limits = c(250, 1000),breaks = c(250, 500, 750, 1000)) + 
    # this is to dodge error bar
    # geom_errorbar(aes(ymin=60/(10^(m-se/2)),
    #                   ymax=60/(10^(m+se/2))), width=0, position = pd) +
    geom_errorbar(aes(ymin=60/(10^(m-se/2)),
                      ymax=60/(10^(m+se/2))), width=0) +
    xlab("x height (mm)") +
    ylab("Reading speed (w/min)") +
    guides(color = guide_legend(title = "viewing distance (cm)")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  return(list(p1,p2))
}

get_60cm_data <- function(rsvp_speed){
  rsvp_speed <- rsvp_speed %>% 
    group_by(participant, conditionName, viewingDistanceDesiredCm, age) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_speed$x_height <-  NA
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.2 mm"), 
                                1.2, rsvp_speed$x_height)
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.4 mm"), 
                                1.4, rsvp_speed$x_height)
  
  rsvp_speed_60 <- rsvp_speed %>% filter(viewingDistanceDesiredCm == 60)
  rsvp_speed_60_1.2 <- rsvp_speed_60 %>% filter(x_height == 1.2) %>% select(participant, pm, age)
  rsvp_speed_60_1.4 <- rsvp_speed_60 %>% filter(x_height == 1.4) %>% select(participant, pm)
  
  rsvp_speed_60_1.2_vs_1.4 <- rsvp_speed_60_1.2 %>% left_join(rsvp_speed_60_1.4, by = "participant")
  return(rsvp_speed_60_1.2_vs_1.4)
}

get_60cm_scatter <- function(rsvp_speed) {
  rsvp_speed_60_1.2_vs_1.4 <- get_60cm_data(rsvp_speed)
  ggplot(rsvp_speed_60_1.2_vs_1.4, aes(60/10^(pm.x), 60/10^(pm.y))) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks() +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    xlab("reading speed (w/min) 1.2 mm") +
    ylab("reading speed (w/min) 1.4 mm") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_60_1.2_vs_1.4))), 
      parse = T) + 
    geom_abline(intercept = 0, slope = 1)
}

get_30cm_scatter <- function(rsvp_speed) {
  rsvp_speed <- rsvp_speed %>% group_by(participant, conditionName, viewingDistanceDesiredCm) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_speed$x_height <-  NA
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.2 mm"), 
                                1.2, rsvp_speed$x_height)
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.4 mm"), 
                                1.4, rsvp_speed$x_height)
  
  rsvp_speed_30 <- rsvp_speed %>% filter(viewingDistanceDesiredCm == 30)
  rsvp_speed_30_1.2 <- rsvp_speed_30 %>% filter(x_height == 1.2) %>% select(participant, pm)
  rsvp_speed_30_1.4 <- rsvp_speed_30 %>% filter(x_height == 1.4) %>% select(participant, pm)
  
  rsvp_speed_30_1.2_vs_1.4 <- rsvp_speed_30_1.2 %>% left_join(rsvp_speed_30_1.4, by = "participant")
  
  ggplot(rsvp_speed_30_1.2_vs_1.4, aes(60/10^(pm.x), 60/10^(pm.y))) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks() +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    xlab("reading speed (w/min)1.2 mm") +
    ylab("reading speed (w/min) 1.4 mm") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_30_1.2_vs_1.4))), 
      parse = T) + 
    geom_abline(intercept = 0, slope = 1)
}

plot_60cm_speed_diff_vs_age <- function(rsvp_speed){
  rsvp_speed_60_1.2_vs_1.4 <- get_60cm_data(rsvp_speed)
  rsvp_speed_60_1.2_vs_1.4 <- rsvp_speed_60_1.2_vs_1.4 %>% 
    mutate(speed_diff = (60/10^(pm.y) - 60/10^(pm.x)))
  ggplot(rsvp_speed_60_1.2_vs_1.4, aes(x = age, y = speed_diff)) +
    geom_point() + 
    theme_bw() + 
    xlab("age") +
    ylab("Reading speed difference (w/min)") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_60_1.2_vs_1.4))), 
      parse = T)
}

plot_reading_age <- function(reading) {
  t <- reading %>%
    filter(!is.na(age)) %>%
    mutate(N = paste0("N = ", n()))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Choose appropriate aesthetics based on Grade
    unique_grades <- n_distinct(t$Grade)
    if (unique_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(log_WPM), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(log_WPM)))
    }
    
    # Compute correlation, regression slope, and annotate statistics
    t <- t %>%
      mutate(Y = 10^(log_WPM))
    regression <- lm(Y ~ age, data = t)
    slope <- coef(regression)[["age"]]
    r_value <- cor(t$age, t$Y, use = "complete.obs")
    N <- nrow(t)
    
    # Plot with regression line and statistics
    p <- p +
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line in black
      theme_bw() +
      labs(
        title = "Reading vs age\ncolored by grade",
        x = "Age",
        y = "Reading speed (w/min)"
      ) +
      scale_y_log10() +
      annotation_logticks(
        sides = "l",  
        short = unit(0.1, "cm"),
        mid = unit(0.1, "cm"),
        long = unit(0.3, "cm")
      ) +
      annotate(
        "text",
        x = min(t$age, na.rm = TRUE) * 1.1,  # Slightly offset from the minimum x for padding
        y = min(t$Y, na.rm = TRUE) * 1.1,    # Slightly offset from the minimum y for padding
        label = paste0(
          "N = ", N,
          "\nR = ", round(r_value, 2),
          "\nslope = ", round(slope, 2)
        ),
        hjust = 0,  # Left-align text
        vjust = 0,  # Bottom-align text
        size = 4,
        color = "black"
      ) +
      color_scale(n = unique_grades) +  # Apply dynamic color scale
      theme(
        legend.position = ifelse(unique_grades == 1, "none", "top"),  # Hide legend if only one grade
        plot.title = element_text(size = 17, margin = margin(b = 10))  # Adjust title size and spacing
      )
    
    # Add points and shapes for Skilled reader if applicable
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p +
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19, 1))
    }
    
    return(p)
  }
}



plot_rsvp_age <- function(rsvp) {
  t <- rsvp %>%
    filter(!is.na(age)) %>%
    mutate(N = paste0("N = ", n()))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    # Determine plot aesthetics based on Grade
    unique_grades <- n_distinct(t$Grade)
    if (unique_grades > 1) {
      t$Grade <- as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(block_avg_log_WPM), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(block_avg_log_WPM)))
    }
    
    # Compute correlation, slope, and regression model
    t <- t %>%
      mutate(Y = 10^(block_avg_log_WPM))
    regression <- lm(Y ~ age, data = t)
    slope <- coef(regression)[["age"]]
    r_value <- cor(t$age, t$Y, use = "complete.obs")
    N <- nrow(t)
    
    # Plot with regression line and annotated statistics
    p <- p +
     
      geom_smooth(method = "lm", se = FALSE, color = "black") +  # Regression line in black
      theme_bw() +
      labs(
        title = "RSVP reading vs age\ncolored by grade",
        x = "Age",
        y = "RSVP reading speed (w/min)"
      ) +
      scale_y_log10() +
      annotation_logticks(
        sides = "l",  # Log ticks only on the left (y-axis)
        short = unit(0.1, "cm"),
        mid = unit(0.1, "cm"),
        long = unit(0.3, "cm")
      )  +
      annotate(
        "text",
        x = min(t$age, na.rm = TRUE) * 1.1,  # Bottom-left placement, slightly offset for padding
        y = min(t$Y, na.rm = TRUE) * 1.1,
        label = paste0(
          "N = ", N,
          "\nR = ", round(r_value, 2),
          "\nSlope = ", round(slope, 2)
        ),
        hjust = 0,  # Left-align text
        vjust = 0,  # Bottom-align text
        size = 4,
        color = "black"
      ) +
      color_scale(n = unique_grades) +  # Apply color scale dynamically
      theme(
        legend.position = ifelse(unique_grades == 1, "none", "top"),  # Hide legend if only one grade
        plot.title = element_text(size = 17, margin = margin(b = 10))  # Adjust title size and spacing
      )
    
    # Add points and shapes for Skilled reader if applicable
    if (n_distinct(t$`Skilled reader?`) > 1) {
      p <- p +
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4, 19, 1))
    } else {
      p <- p + geom_point()
    }
    
    return(p)
  }
}



plot_reading_rsvp <- function(reading, rsvp) {
  if (nrow(reading) == 0 | nrow(rsvp) == 0) {
    return(NULL)
  }
  
  # Preprocess RSVP data
  rsvp <- rsvp %>% 
    mutate(participant = tolower(participant)) %>% 
    group_by(participant, targetKind, Grade) %>% 
    summarize(
      avg_log_WPM = mean(block_avg_log_WPM, na.rm = TRUE), 
      age = first(age),  # Include age in the summarized data
      .groups = "keep"
    )
  
  # Preprocess Reading data and join with RSVP data
  t <- reading %>%
    mutate(participant = tolower(participant)) %>% 
    group_by(participant, block_condition, targetKind) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = TRUE)), .groups = "keep") %>% 
    ungroup() %>% 
    mutate(log_WPM = log10(avg_wordPerMin)) %>% 
    group_by(participant, targetKind) %>% 
    summarize(avg_log_WPM = mean(log10(avg_wordPerMin), na.rm = TRUE), .groups = "keep") %>% 
    left_join(rsvp, by = 'participant') %>% 
    filter(!is.na(age)) %>%  # Drop rows with NA age
    ungroup() %>% 
    mutate(
      N = paste0('N = ', n()),
      Grade = as.character(Grade),
      age = as.numeric(age)  # Ensure age is numeric
    )
  
  if (nrow(t) == 0) {
    return(NULL)
  }
  
  # Compute correlation and slope
  t <- t %>% mutate(X = 10^(avg_log_WPM.y), Y = 10^(avg_log_WPM.x))
  r_value <- cor(t$X, t$Y, method = "pearson", use = "complete.obs")
  lm_fit <- lm(Y ~ X, data = t)
  slope <- coef(lm_fit)[["X"]]
  
  # Compute R_factor_out_age
  pcor <- ppcor::pcor(t %>% select(X, Y, age))
  R_factor_out_age <- round(pcor$estimate[2, 1], 2)
  
  
  minXY = min(t$X, t$Y, na.rm = T) * 0.95
  maxXY = max(t$X, t$Y, na.rm = T) * 1.05
  # Generate the plot
  p <- ggplot(t, aes(x = X, y = Y, color = Grade)) + 
    geom_point(size = 3) +  # Point size fixed for clarity
    geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +  # Black regression line
    scale_x_log10(limits = c(minXY, maxXY),
                  expand = c(0,0)) +
    scale_y_log10(limits = c(minXY, maxXY),
                  expand = c(0,0)) +
    annotation_logticks(
      sides = "l",  
      short = unit(0.1, "cm"),
      mid = unit(0.1, "cm"),
      long = unit(0.3, "cm")
    ) +
    labs(
      x = "RSVP reading (w/min)",
      y = "Reading (w/min)",
      title = 'Reading vs RSVP reading\ncolored by grade'
    ) +
    coord_fixed() + 
    theme_bw() +
    theme(legend.position = 'top') +
    annotation_logticks() +
    guides(color = guide_legend(title = "Grade")) +
    ggpp::geom_text_npc(aes(npcx = 'left',
                            npcy = 'top',
                            label = paste0(
                              t$N[1],  # Using the first value of N (constant for the group)
                              "\nR = ", round(r_value, 2),
                              "\nR_factor_out_age = ", R_factor_out_age,
                              "\nslope = ", round(slope, 2)
                            ))) + 
    color_scale(n = n_distinct(t$Grade))  # Apply color_scale dynamically
  
  return(p)
}





