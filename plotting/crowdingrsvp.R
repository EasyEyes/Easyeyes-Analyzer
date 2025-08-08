library(ggcorrplot) 
library(tidyr)
source('./constant.R')

plot_rsvp_crowding_acuity <- function(allData) {
  if (is.null(allData)) {

    return(list(
      NULL,
      NULL,
      NULL,
      NULL
    ))
  }
  
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  acuity <- allData$acuity %>% rename('log_acuity'='questMeanAtEndOfTrialsLoop')
    
  rsvp_speed <- rsvp_speed %>% 
    mutate(Age = format(age,nsmall=2),
           Grade = as.character(Grade))

  foveal_crowding <- crowding %>% 
    filter(targetEccentricityXDeg == 0) %>% 
    mutate(Age = format(age,nsmall=2),
           Grade = as.character(Grade))
  
  peripheral_crowding <- crowding %>% 
    filter(targetEccentricityXDeg != 0) %>% 
    mutate(Age = format(age,nsmall=2),
           Grade = as.character(Grade))
  
  foveal_acuity <- acuity %>% 
    filter(targetEccentricityXDeg == 0) %>% 
    mutate(Age = format(age,nsmall=2),
           Grade = as.character(Grade))


  p1 = NULL
  
  if (nrow(foveal_crowding) > 0){
    p1 = ggplot(data = foveal_crowding, 
                aes(x = Grade, 
                y = 10^(log_crowding_distance_deg),
                color = Age,
                shape= conditionName)) +
      facet_wrap(font~.) + 
      theme_classic() +
      scale_y_log10() + 
      annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
     ) +
      labs(title = 'Foveal crowding vs grade colored by font',
           y = 'Foveal crowding (deg)')
  }
  
  p2 = NULL
  
  if (nrow(rsvp_speed) > 0){
      p2 = ggplot(data = rsvp_speed,
                  aes(x = Grade, 
                      y = 10^(block_avg_log_WPM), 
                      color = Age)) +
        theme_classic() + 
        scale_y_log10() +
        annotation_logticks(
          sides = "bl", 
          short = unit(2, "pt"), 
          mid   = unit(2, "pt"), 
          long  = unit(7, "pt")
        ) +
        guides(shape =guide_legend(title = '', ncol = 1),
               color = guide_legend(title = 'Age')) +
        labs(title = 'RSVP vs grade',
             y = 'RSVP reading speed (w/min)')
  }
  
  p3 = NULL
  if (nrow(foveal_acuity) > 0) {
   p3 = ggplot(data = foveal_acuity,
               aes(x = Grade, 
                   y = 10^(log_acuity), 
                   color =  Age,
                   shape = conditionName)) +
     theme_classic() +
     scale_y_log10() +
     annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
     guides(shape =guide_legend(title = '', ncol = 1),
            color = guide_legend(title = 'Age')) +
     labs(title = 'Foveal acuity vs grade',
          y = ' Foveal acuity (deg)')
  }

  # if (n_distinct(foveal_acuity$`Skilled reader?`) == 1) {
  #   p1 <- p1 + geom_point()
  #   p2 <- p2 + geom_point()
  #   p3 <- p3 + geom_point()
  # } else {
  #   p1 <- p1 + 
  #     geom_point(aes(shape = `Skilled reader?`)) +
  #     scale_shape_manual(values = c(4,19, 1))
  #   p2 <- p2 + 
  #     geom_point(aes(shape = `Skilled reader?`)) +
  #     scale_shape_manual(values = c(4,19, 1))
  #   p3 <- p3 + 
  #     geom_point(aes(shape = `Skilled reader?`)) +
  #     scale_shape_manual(values = c(4,19, 1))
  # }
  return(list(
    p1,
    p2,
    p3
  ))
}

plot_rsvp_crowding <- function(allData) {
  
  factor_out_age_and_plot <- function(allData) {
    # Helper function to compute residuals after factoring out age
    eccs     <- sort(unique(allData$crowding$targetEccentricityXDeg[allData$crowding$targetEccentricityXDeg != 0]))
    ecc_label <- paste0("X ecc = ", paste(as.integer(round(eccs)), collapse = ", "), " deg")
    
    compute_residuals <- function(data, x_var, y_var, age_var) {
      regression_y <- lm(paste0(y_var, " ~ ", age_var), data = data)
      residuals_y <- residuals(regression_y)
      regression_x <- lm(paste0(x_var, " ~ ", age_var), data = data)
      residuals_x <- residuals(regression_x)
      return(data.frame(
        residual_y = residuals_y,
        residual_x = residuals_x
      ))
    }
    
    # Data preparation: Merge crowding and RSVP datasets
    crowding <- allData$crowding %>%
      mutate(participant = tolower(participant)) %>% 
      filter(targetEccentricityXDeg != 0) %>%
      group_by(participant,font) %>%
      summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = TRUE),
                .groups="drop")
    
    rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
    
    data <- crowding %>%
      inner_join(rsvp %>% select(-conditionName), by = c("participant", "font"))
    
    # if (setequal(crowding$font, rsvp$font)) {
    #   data <- crowding %>%
    #     inner_join(rsvp %>% select(-conditionName), by = c("participant", "font"))
    #   
    # } else {
    #   data <- crowding %>%
    #     inner_join(rsvp %>% select(-conditionName), by = c("participant")) %>% 
    #     mutate(font = paste0(font.x, " vs ", font.y))
    # }
    
    data <- data %>% 
      distinct(participant, log_crowding_distance_deg, block_avg_log_WPM, age,
               Grade, `Skilled reader?`, ParticipantCode, font) %>%
      filter(!is.na(log_crowding_distance_deg), !is.na(block_avg_log_WPM)) %>%
      mutate(
        log_crowding = log_crowding_distance_deg,
        log_rsvp = block_avg_log_WPM,
        Age = format(age, nsmall = 2),
        ageN = as.numeric(age),
        Grade = as.character(Grade)
      )
    
    # Ensure there is data to process
    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Compute residuals after factoring out age
    if (n_distinct(data$age) > 1) {
      residuals <- compute_residuals(data, "log_crowding", "log_rsvp", "age")
      data <- data %>%
        mutate(
          residual_log_crowding = residuals$residual_x,
          residual_log_rsvp = residuals$residual_y,
          X = 10^residuals$residual_x,
          Y = 10^residuals$residual_y
        )
    } else {
      data <- data %>%
        mutate(
          residual_log_crowding = log_crowding,
          residual_log_rsvp = log_rsvp,
          X = 10^log_crowding,
          Y = 10^log_rsvp
        )
    }
   
    
    # Compute correlation and slope
    correlation <- cor(data$residual_log_crowding, data$residual_log_rsvp, method = "pearson")
    N <- nrow(data)
    slope <- lm(residual_log_rsvp ~ residual_log_crowding, data = data)$coefficients[2]
    
    # Create the plot with log scale
    xMin <- min(data$X, na.rm = TRUE) / 1.5
    xMax <- max(data$X, na.rm = TRUE) * 1.5
    yMin <- min(data$Y, na.rm = TRUE) / 1.5
    yMax <- max(data$Y, na.rm = TRUE) * 1.5
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    
    colorFactor <- "font"  # Set the color factor to 'Grade'
    
    plot <- ggplot(data, aes(x = X, y = Y, color = .data[[colorFactor]])) +
      theme_classic() +
      scale_x_log10(
        breaks = scales::log_breaks(),
        limits = c(xMin, xMax),
        expand = c(0, 0)
      ) +
      scale_y_log10(
        breaks = y_breaks,
        limits = c(yMin, yMax),
        expand = c(0, 0)
      ) +  
      annotation_logticks(
        sides = "bl", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      )+
      geom_smooth(method = "lm", se = FALSE) + 
      coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +  # Set exact limits
      annotate(
        "text",
        x = xMin * 1.4,
        y = yMin * 1.4,
        label = paste0(
          "N = ", N,
          "\nR = ", round(correlation, 2),
          "\nslope = ", round(slope, 2),
          "\n",ecc_label
        ),
        hjust = 0,
        vjust = 0,
        size = 3,
        color = "black"
      ) +
      color_scale(n = n_distinct(data[[colorFactor]])) +
      labs(
        x = "Residual crowding (deg)",
        y = "Residual RSVP reading (w/min)",
        title = paste0("Residual RSVP vs residual peripheral crowding\ncolored by ", colorFactor),
        subtitle = 'Geometric mean of left and right measurements'
      ) + 
      plt_theme +
      theme(
        legend.position = ifelse(n_distinct(data[[colorFactor]]) == 1, "none", "top")
      ) +
      guides(color = guide_legend(title = colorFactor), 
             shape = guide_legend(title = "", ncol = 1)) +
      plt_theme_ggiraph  # Ensure plt_theme is included
    
    # Add points to the plot
    plot <- plot + ggiraph::geom_point_interactive(aes(data_id = ParticipantCode, tooltip = ParticipantCode), size = 3)
    return(plot)
  }
  
  create_plot <- function(data, condition, colorFactor) {
    
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg, font) %>%
      inner_join(rsvp %>% select(-conditionName), by = c("participant", "font"))
    # if (setequal(data$font, rsvp$font)) {
    #   data_rsvp <- data %>%
    #     select(participant, log_crowding_distance_deg, font) %>%
    #     inner_join(rsvp %>% select(-conditionName), by = c("participant", "font"))
    #   
    # } else {
    #   data_rsvp <- data %>%
    #     select(participant, log_crowding_distance_deg, font) %>%
    #     inner_join(rsvp %>% select(-conditionName), by = c("participant")) %>% 
    #     mutate(font = paste0(font.x, " vs ", font.y))
    # }
    
    data_rsvp <- data_rsvp %>%
      distinct(participant,
               block_avg_log_WPM,
               log_crowding_distance_deg,
               font,
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
      data_for_stat <- data_rsvp %>%
        filter(`Skilled reader?` != FALSE) %>%
        select(block_avg_log_WPM, log_crowding_distance_deg, X, Y, ageN)
    } else {
      data_for_stat <- data_rsvp %>%
        select(block_avg_log_WPM, log_crowding_distance_deg, X, Y, ageN)
    }
   
    if (sum(!is.na(data_for_stat$ageN)) == 0) {
      data_for_stat <- data_for_stat %>% select(-ageN)
    }
    
    data_for_stat <- data_for_stat[complete.cases(data_for_stat), ]
    
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
    
    if ("ageN" %in% names(data_for_stat)) {
      corr_without_age <- ppcor::pcor(data_for_stat %>%
                                        select(block_avg_log_WPM, log_crowding_distance_deg, ageN))$estimate[2, 1]
      corr_without_age <- format(round(corr_without_age, 2), nsmall = 2)
    } else {
      corr_without_age <- NA
    }
    
    xMin <- min(data_rsvp$X, na.rm = TRUE) / 1.5
    xMax <- max(data_rsvp$X, na.rm = TRUE) * 1.5
    yMin <- min(data_rsvp$Y, na.rm = TRUE) / 1.5
    yMax <- max(data_rsvp$Y, na.rm = TRUE) * 1.5
    
    # Generate dynamic breaks for the y-axis
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    
    if (condition == "Peripheral") {
      eccs     <- sort(unique(crowding$targetEccentricityXDeg[crowding$targetEccentricityXDeg != 0]))
      ecc_label <- paste0("X ecc = ", paste(as.integer(round(eccs)), collapse = ", "), " deg")
      label_text <- paste0(
        "N = ", corr$N, "\n",
        "R = ", corr$correlation, "\n",
        "slope = ", round(slope,2), "\n",
        ecc_label, "\n",
        "R_factor_out_age = ", corr_without_age
        
      )
    } else {
      label_text <- paste0(
        "N = ", corr$N, "\n",
        "R = ", corr$correlation, "\n",
        "slope = ", round(slope,2),"\n",
        "R_factor_out_age = ", corr_without_age
        
      )
    }
    
    p <- ggplot(data_rsvp, aes(x = X, y = Y, color = .data[[colorFactor]])) +
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
        label = label_text,
        hjust = 0,
        vjust = 0,
        size = 3,
        color = "black"
      ) +
      color_scale(n = n_distinct(data_rsvp[[colorFactor]])) +  # Apply color scale dynamically
      labs(
        x = paste(condition, "crowding (deg)"),
        y = "RSVP reading (w/min)",
        title = paste("RSVP vs", tolower(condition), "crowding\ncolored by", tolower(colorFactor))
      ) + 
      plt_theme_ggiraph +
      theme(
        legend.position = ifelse(n_distinct(data_rsvp[[colorFactor]]) == 1, "none", "top")
      ) +
      guides(color = guide_legend(title = colorFactor), 
             shape = guide_legend(title = '',ncol= 1))
    if (colorFactor == 'Grade') {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") # Black regression line
    } else {
      p <- p + geom_smooth(method = "lm", formula = y ~ x, se = FALSE) 
    }
    
    if (condition == "Peripheral") {
      p <- p + labs(subtitle = 'Geometric mean of left and right measurements')
    }
    
    p <- p + ggiraph::geom_point_interactive(aes(data_id = ParticipantCode, tooltip = ParticipantCode), size = 3)
    
    return(p)
  }
  
  
  crowding <- allData$crowding %>% mutate(participant = tolower(participant))
  foveal <- crowding %>% 
    filter(targetEccentricityXDeg == 0)
  
  peripheral <- crowding %>% 
    filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant, font) %>%
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm=T),
              .groups="drop") 
  rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
  
  if (nrow(allData$rsvp) == 0 | nrow(allData$crowding) == 0) {
    return(list(NULL, NULL, NULL, NULL))
  }
  
  p1 <- create_plot(peripheral, "Peripheral",'Grade')
  p2 <- create_plot(foveal, "Foveal",'Grade')
  p3 <- create_plot(peripheral, "Peripheral",'font')
  p4 <- factor_out_age_and_plot(allData)
  p5 <- create_plot(foveal, "Foveal",'font')
  

  return(list(p_grade = p1, f_grade = p2, p_font = p3, residual =  p4, f_font = p5))
}

getCorrMatrix <- function(allData, pretest) {
  if (nrow(pretest) > 0) {
    pretest_for_corr <- pretest %>%
      mutate(Grade = ifelse(Grade == 'R', -1, Grade)) %>% 
      mutate(Grade = ifelse(is.na(Grade), -1, Grade))
    
    pretest_for_corr <- lapply(pretest_for_corr, as.numeric)
    pretest_for_corr <- tibble(data.frame(pretest_for_corr))
    colnames(pretest_for_corr) <- colnames(pretest)
    
    pretest_for_corr <- pretest_for_corr %>%   
      select_if(~sum(!is.na(.)) > 0)
    pretest_for_corr$participant <- pretest$participant
    pretest_for_corr <- pretest_for_corr %>%
      mutate(participant = tolower(participant))
  } else {
    pretest_for_corr <- tibble()
  }

  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  age <- allData$age %>% select(-Grade)
  acuity <- allData$acuity %>%
    rename('log_acuity'='questMeanAtEndOfTrialsLoop') %>% 
    mutate(type=ifelse(
      targetEccentricityXDeg == 0,
      'log foveal acuity',
      'log peripheral acuity'
    ),
    participant = tolower(participant)
    ) %>% 
    group_by(participant, type) %>% 
    summarize(log_acuity = mean(log_acuity),
              .groups="drop") %>% 
    pivot_wider(names_from=type, values_from = log_acuity)
 
  
  crowding <- crowding %>% mutate(type=ifelse(
    targetEccentricityXDeg == 0,
    'log foveal crowding',
    'log peripheral crowding'
  ),
  participant = tolower(participant)) %>% 
    group_by(participant,type) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg),
              .groups="drop") %>% 
    pivot_wider(names_from=type, values_from = log_crowding_distance_deg)
  
  reading <- allData$reading %>%
    mutate(participant = tolower(participant)) %>%
    group_by(participant, block_condition) %>% 
    summarize(log_WPM = mean(log_WPM,na.rm=T),
              .groups="drop") %>% 
    rename('log reading' = 'log_WPM' ) %>% 
    select(participant, `log reading`)
  
  crowdingW <- crowding %>% 
    full_join(acuity, by = 'participant') %>% 
    mutate(participant = tolower(participant)) %>% 
    full_join(rsvp_speed %>% select(participant, block_avg_log_WPM) %>% mutate(participant = tolower(participant)), by = 'participant') %>% 
    full_join(reading, by = 'participant')
  
  if (nrow(pretest) > 0) {
    crowdingW <- crowdingW %>% 
      full_join(pretest_for_corr, by = 'participant')
  }
  if (!'Age' %in% names(crowdingW)) {
    crowdingW <- crowdingW %>% full_join(age %>% mutate(participant = tolower(participant)), by = 'participant')
  }
  
  crowdingW <- crowdingW %>% 
    rename('log rsvp' = 'block_avg_log_WPM') %>% 
    select_if(is.numeric) %>% 
    select(where(~sum(!is.na(.)) > 0))
  # c <- colnames(crowdingW)

  # t <- data.frame(cor(crowdingW[complete.cases(crowdingW),]))
  # colnames(t) <- c
  # t <- t %>% mutate(across(everything(), round, 3))
  # print(paste('nrow of reading:', nrow(reading)))
  # print(paste('nrow of crowding:', nrow(crowding)))
  # print(paste('nrow of rsvp_speed:', nrow(rsvp_speed)))
  # print(paste('nrow of acuity:', nrow(acuity)))
  
  cor_mat <- cor(crowdingW, use = "pairwise.complete.obs")
  cor_mat <- round(cor_mat, 3)
  
  vars <- colnames(crowdingW)
  n_mat <- matrix(NA_integer_, nrow = length(vars), ncol = length(vars),
                  dimnames = list(vars, vars))

  for (i in seq_along(vars)) {
    for (j in seq_along(vars)) {
      n_mat[i, j] <- sum(
        !is.na(crowdingW[[ vars[i] ]]) &
        !is.na(crowdingW[[ vars[j] ]])
      )
    }
  }

  # corplot <- ggcorrplot(t,
  #                       show.legend = FALSE,
  #                       show.diag = T,
  #                       type = "lower",
  #                       colors= c('white'),
  #                       lab = T) + 
  #   theme_bw() +
  #   labs(x = '', 
  #        y = '',
  #        title = 'Correlation table') + 
  #   ggpp::geom_text_npc(aes(npcx = 'left', npcy = 'top', label = paste0('N=', nrow(crowdingW[complete.cases(crowdingW),])))) +
  #   plt_theme +
  #   theme(legend.position = 'none',
  #         axis.text.x = element_text(size = 14,
  #                                    angle = 30,
  #                                    vjust = 1,
  #                                    hjust=1),
  #         plot.title = element_text(size=18,
  #                                   hjust = 1),
  #         plot.subtitle = element_text(size=18,
  #                                      hjust = 1))
    # 3a) Correlation plot
  corplot <- ggcorrplot(
    cor_mat,
    show.legend = FALSE,
    show.diag   = TRUE,
    type        = "lower",
    lab         = TRUE,
    lab_size    = 4,
    colors      = c("white")
  ) +
    theme_bw() +
    labs(title = "Correlations", x = NULL, y = NULL) +
    ggpp::geom_text_npc(
      aes(npcx = "left", npcy = "top",
          label = paste0("N=", max(n_mat))),
      size = 3
    ) +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 30, vjust = 1, hjust = 1),
      plot.title      = element_text(hjust = 1, size = 16)
    )

  # 3b) N‑counts plot
  nplot <- ggcorrplot(
    n_mat,
    show.legend = FALSE,
    show.diag   = TRUE,
    type        = "lower",
    lab         = TRUE,
    lab_size    = 4,
    colors      = c("white")
  ) +
  scale_fill_gradient2(
    low      = "white",
    mid      = "white",
    high     = "white",
    midpoint = 0,
    na.value = "white"
  ) +
    theme_bw() +
    labs(title = "N (Non-missing Pairs)", x = NULL, y = NULL) +
    theme(
      legend.position = "none",
      axis.text.x     = element_text(angle = 30, vjust = 1, hjust = 1),
      plot.title      = element_text(hjust = 1, size = 16)
    )

  # return(list(
  #   plot = corplot,
  #   width = 2.5 + ncol(t) * 0.38,
  #   height = 2.5 + ncol(t) * 0.38
  # ))
  nc <- ncol(cor_mat)  # number of variables
  w  <- 2.5 + nc * 0.38
  h  <- 2.5 + nc * 0.38

  return(list(
    plot = corplot,
    width    = w,
    height   = h,
    n_plot   = nplot
  ))
}








