library(scales)
source('./constant.R')


crowding_by_side <- function(crowding) {
  
  crowding <- crowding %>% filter(targetEccentricityXDeg != 0) %>% 
    mutate(side = ifelse(targetEccentricityXDeg < 0, 'L', 'R'),
           XDeg = abs(targetEccentricityXDeg))
  
  crowding_L <- crowding %>% filter(side == "L") %>% select(-conditionName, -side)
  crowding_R <- crowding %>% filter(side == "R") %>% select(-conditionName, -side)
  
  crowding_L_R <- crowding_L %>% 
    left_join(crowding_R, by = c("participant","font", 'XDeg','order')) %>% 
    rename("bouma_factor_Left" = "bouma_factor.x",
           "bouma_factor_Right" = "bouma_factor.y",
           "log_crowding_distance_deg_Left" = "log_crowding_distance_deg.x",
           "log_crowding_distance_deg_Right" = "log_crowding_distance_deg.y")
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
  
  summ <- group_by(crowding_L_R, font) %>% 
    mutate(log_delta = log_crowding_distance_deg_Left - log_crowding_distance_deg_Right) %>% 
    summarize(mean_log_delta = round(mean(log_delta),2),
              sd_log_delta = round(sd(log_delta),2),
              mean_left = round(mean(log_crowding_distance_deg_Left),2),
              sd_left = round(sd(log_crowding_distance_deg_Left),2),
              mean_right = round(mean(log_crowding_distance_deg_Right),2),
              sd_right = round(sd(log_crowding_distance_deg_Right),2),
              N = n()) %>% 
    mutate(label =paste0('N= ', N, '\n', 
                         'logDelta: Mean= ', mean_log_delta, ', SD= ', sd_log_delta, '\n',
                         'spacingDeg left: Mean= ', mean_left, ', SD= ', sd_left, '\n',
                         'spacingDeg right: Mean= ', mean_right, ', SD= ', sd_right))

  minX <- min(crowding_L_R$log_crowding_distance_deg_Left) * 0.8
  maxX <- max(crowding_L_R$log_crowding_distance_deg_Left) * 1.2
  minY <- min(crowding_L_R$log_crowding_distance_deg_Right) * 0.8
  maxY <- max(crowding_L_R$log_crowding_distance_deg_Right) * 1.2
  p <- ggplot(crowding_L_R,aes(x = 10^(log_crowding_distance_deg_Left), y = 10^(log_crowding_distance_deg_Right))) + 
    geom_point(size = 1) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10(limits = c(10^(minY), 
                             10^(maxY)),
                  breaks = c(0.1, 0.3, 1)) +
    scale_x_log10(limits = c(10^(minX), 
                             10^(maxX)),
                  breaks = c(0.1, 0.3, 1)) + 
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    theme_bw() + 
    ggpp::geom_text_npc(
      data = summ,
      aes(npcx = "left",
          npcy = "top",
          label = label)) + 
  labs(x = "Left",
       t = "Right",
       title = "Peripheral crowding Left vs Right"
  )
  return(p)
}

crowding_mean_scatter_plot <- function(crowding_L_R){
  if (n_distinct(crowding_L_R$font) < 1) {
    return(ggplot() + 
             xlab("Left Bouma factor") + 
             ylab("Right Bouma factor") )
  }
  
  t <- crowding_L_R %>% group_by(font) %>% summarize(avg_left_deg = mean(log_crowding_distance_deg_Left),
                                                     avg_right_deg = mean(log_crowding_distance_deg_Right))
  corrrelation <- ifelse(nrow(t) > 1, cor(t$avg_left_deg, t$avg_right_deg), NA)
  
  ggplot(t, aes(x = avg_left_deg, y = avg_right_deg, color = font)) + 
    geom_point(size = 2) +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    stat_cor() +  
    coord_fixed(ratio = 1) + 
    guides(color = guide_legend(title="Font")) + 
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
    downloadtheme + 
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
    downloadtheme + 
    ggpp::geom_text_npc(
      aes(npcx = "left", npcy = "bottom", label = paste0("italic('N=')~", n)), 
      parse = TRUE
    ) + 
    stat_cor(aes(label = ..r.label..), r.accuracy = 0.001)
  
  title = paste(font1, ' vs ', font2)
  
  return(list(mean_plot = mean_plot, sd_plot = sd_plot, title = title))
}

get_foveal_crowding_vs_age <- function(crowding) {
  t <- crowding %>% filter(!is.na(age),
                           targetEccentricityXDeg == 0) %>% 
    mutate(N = paste0('N=',n()))
  
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    if (n_distinct(t$Grade) > 1) {
      t$Grade = as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg)))
    }
    p <- p + 
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      scale_y_log10() + 
      theme_bw() +
      labs(title = 'Foveal crowding vs age\ncolored by grade',
           x = 'Age',
           y = 'Foveal crowding (deg)')
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4,19))
    }
    return(p)
  }
}

get_peripheral_crowding_vs_age <- function(crowding) {
  t <- crowding %>% filter(!is.na(age),
                           targetEccentricityXDeg != 0) %>% 
    mutate(N = paste0('N=',n()))
  print('inside get_peripheral_crowding_vs_age')
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    if (n_distinct(t$Grade) > 1) {
      t$Grade = as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg)))
    }
    p <- p + 
      scale_y_log10() + 
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      theme_bw() +
      labs(title = 'Peripheral crowding vs age\n colored by grade',
           x = 'Age',
           y = 'Peripheral crowding (deg)')
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4,19))
    }
    return(p)
  }
}

get_repeatedLetter_vs_age <- function(repeatedLetters) {
  t <- repeatedLetters %>% filter(!is.na(age)) %>% 
    mutate(N = paste0('N=',n()))
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    if (n_distinct(t$Grade) > 1) {
      t$Grade = as.character(t$Grade)
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg), color = Grade))
    } else {
      p <- ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg)))
    }
    p <- p + 
      scale_y_log10() + 
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      theme_bw() +
      labs(title = 'Repeated-letter crowding vs age\ncolored by color',
           x = 'Age',
           y = 'Repeated-letter crowding (deg)')
    
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
    } else {
      p <- p + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4,19))
    }
    return(p)
  }
}

get_crowding_vs_repeatedLetter <- function(crowding, repeatedLetters) {
  if (nrow(crowding) == 0 | nrow(repeatedLetters)) {
    return(list(NULL, NULL))
  }
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  foveal_vs_repeatedLetters <- repeatedLetters %>%
    rename('repeatedLetters' = 'log_crowding_distance_deg') %>% 
    select(participant, repeatedLetters) %>% 
    inner_join(foveal, by = 'participant') %>% 
    mutate(age = format(age, nsmall=2),
           N = paste0('N=',n()))
  
  peripheral_vs_repeatedLetters <- repeatedLetters %>%
    rename('repeatedLetters' = 'log_crowding_distance_deg') %>% 
    select(participant, repeatedLetters) %>% 
    inner_join(peripheral, by = 'participant') %>% 
    mutate(age = format(age, nsmall=2),
           N = paste0('N=',n()))
  p <- NULL
  p1 <- NULL
  if (nrow(foveal_vs_repeatedLetters) == 0) {
    return(list(age = p, grade = p1))
  } else {
    if (n_distinct(foveal_vs_repeatedLetters$Grade) > 1) {
      foveal_vs_repeatedLetters <- foveal_vs_repeatedLetters %>%
        mutate(Grade = as.character(Grade))
      
      p1 <- ggplot(foveal_vs_repeatedLetters, 
                   aes(x = 10^(log_crowding_distance_deg), 
                       y = 10^(repeatedLetters), 
                       color = Grade)) +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        scale_y_log10() + 
        scale_x_log10() + 
        theme_bw() +
        labs(title = 'Repeated-letter crowding vs foveal crowding\ncolored by grade',
             x = 'Foveal crowding (deg)',
             y = 'Repeated-letter crowding (deg)')
    }
    
    if (n_distinct(foveal_vs_repeatedLetters$age) > 1) {
      foveal_vs_repeatedLetters <- foveal_vs_repeatedLetters %>%
        mutate(age = format(age,nsmall=2))
      
      p <- ggplot(foveal_vs_repeatedLetters, 
                  aes(x = 10^(log_crowding_distance_deg), 
                      y = 10^(repeatedLetters), 
                      color = age))
    } else {
      p <-  ggplot(foveal_vs_repeatedLetters, 
                   aes(x = 10^(log_crowding_distance_deg), 
                       y = 10^(repeatedLetters)))
    }
  } 
  p <- p + 
    ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
    scale_y_log10() + 
    scale_x_log10() + 
    theme_bw() +
    labs(title = 'Repeated-letter crowding vs foveal crowding\ncolored by age',
         x = 'Foveal crowding (deg)',
         y = 'Repeated-letter crowding (deg)')
  if (n_distinct(foveal_vs_repeatedLetters$`Skilled reader?`) == 1) {
    p <- p + geom_point()
    p1 <- p1 + geom_point()
  } else {
    p <- p + 
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4,19))
    p1 <- p1 + 
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4,19))
  }
  
  return(list(age = p, grade = p1))
  
}

get_foveal_acuity_diag <- function(crowding, acuity) {
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  
  if (nrow(foveal) == 0 | nrow(acuity) == 0) {
    return(list(
      age = NULL,
      grade = NULL
    ))
  } else {
    
    p <- NULL
    p1 <- NULL
    foveal_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>% 
      filter(targetEccentricityXDeg == 0) %>% 
      select(participant, log_acuity) %>% 
      inner_join(foveal, by = 'participant') %>% 
      mutate(age = format(age,nsmall=2),
             N = paste0('N=',n()))
    
    if (!nrow(foveal_acuity) == 0) {
      if (n_distinct(foveal_acuity$Grade) > 1) {
        foveal_acuity <- foveal_acuity %>% 
          mutate(Grade = as.character(Grade))
        
        p1 <-  ggplot(foveal_acuity, aes(x = 10^log_crowding_distance_deg,
                                         y = 10^(log_acuity),
                                         color = Grade)) + 
          ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
          scale_x_log10() + 
          scale_y_log10() + 
          theme_bw() +
          annotation_logticks(short = unit(0.1, "cm"),                                                
                              mid = unit(0.1, "cm"),
                              long = unit(0.3, "cm")) + 
          labs(title = 'Foveal acuity vs foveal crowding\ncolored by grade',
               x = 'Foveal crowding (deg)',
               y = 'Foveal acuity (deg)') + 
          coord_fixed()
      } 
      if (n_distinct(foveal_acuity$age) > 1) {
        foveal_acuity <- foveal_acuity %>% mutate(age = format(age,nsmall=2))
        p <-  ggplot(foveal_acuity, 
                     aes(x = 10^log_crowding_distance_deg,
                         y = 10^log_acuity,
                         color = age)) +
          labs(title = 'Foveal acuity vs foveal crowding\ncolored by age',
               x = 'Foveal crowding (deg)',
               y = 'Foveal acuity (deg)')
        
      } else {
        p <-  ggplot(foveal_acuity, 
                     aes(x = 10^log_crowding_distance_deg, 
                         y = 10^log_acuity)) +
          labs(title = 'Foveal acuity vs foveal crowding',
               x = 'Foveal crowding (deg)',
               y = 'Foveal acuity (deg)')
        
      }
      p <- p + 
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        coord_fixed()
      
      if (n_distinct(foveal_acuity$`Skilled reader?`) == 1) {
        p <- p + geom_point()
        p1 <- p1 + geom_point()
      } else {
        p <- p + 
          geom_point(aes(shape = `Skilled reader?`)) +
          scale_shape_manual(values = c(4,19))
        p1 <- p1 + 
          geom_point(aes(shape = `Skilled reader?`)) +
          scale_shape_manual(values = c(4,19))
      }
      
      
    }
    
    
    peripheral_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>% 
      filter(targetEccentricityXDeg != 0) %>% 
      select(participant, log_acuity) %>% 
      inner_join(foveal, by = 'participant') %>% 
      mutate(age = format(age,nsmall=2),
             N = paste0('N=',n()))
    
    p2 <- NULL
    p3 <- NULL
    
    if (!nrow(peripheral_acuity) == 0) {
      if (n_distinct(peripheral_acuity$Grade) > 1) {
        p3 <-  ggplot(peripheral_acuity, aes(x = 10^log_crowding_distance_deg,
                                             y = 10^(log_acuity),
                                             shape = `Skilled reader?`,
                                             color = Grade)) + 
          ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
          scale_x_log10() + 
          scale_y_log10() + 
          theme_bw() +
          annotation_logticks(short = unit(0.1, "cm"),                                                
                              mid = unit(0.1, "cm"),
                              long = unit(0.3, "cm")) + 
          labs(title = 'Peripheral acuity vs foveal crowding\ncolored by grade',
               x = 'Foveal crowding (deg)',
               y = 'Peripheral acuity (deg)') + 
          coord_fixed()
      }
      
      if (n_distinct(peripheral_acuity$age) > 1) {
        peripheral_acuity <- peripheral_acuity %>% mutate(age = format(age,nsmall=2))
        p2 <-  ggplot(peripheral_acuity, 
                      aes(x = 10^log_crowding_distance_deg, 
                          y = 10^(log_acuity), 
                          color = age))
      } else {
        p2 <-  ggplot(peripheral_acuity, 
                      aes(x = 10^log_crowding_distance_deg, 
                          y = 10^(log_acuity)))
      }
      
      p2 <- p2 + 
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        labs(title = 'Peripheral acuity vs foveal crowding',
             x = 'Foveal crowding (deg)',
             y = 'Peripheral acuity (deg)') + 
        coord_fixed()
      
      if (n_distinct(foveal_acuity$`Skilled reader?`) == 1) {
        p2 <- p2 + geom_point()
        p3 <- p3 + geom_point()
      } else {
        p2 <- p2 + 
          geom_point(aes(shape = `Skilled reader?`)) + 
          scale_shape_manual(values = c(4,19)) 
        p3 <- p3 + 
          geom_point(aes(shape = `Skilled reader?`)) +
          scale_shape_manual(values = c(4,19))
      }
    }
    
    return(list(
      foveal = list(age = p,grade = p1),
      peripheral = list(age = p2,grade = p3)
    ))
  }
}

get_foveal_peripheral_diag <- function(crowding) {
  
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  if (nrow(foveal) == 0 | nrow(peripheral) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    p <- NULL
    p1 <- NULL
    t <- foveal %>% 
      rename('foveal' = 'log_crowding_distance_deg') %>% 
      select(foveal, participant, order) %>% 
      inner_join(peripheral, by = 'participant') %>% 
      rename('peripheral' = 'log_crowding_distance_deg') %>% 
      mutate(age = format(age,nsmall=2),
             N = paste0('N=',n()))
    if (n_distinct(t$Grade) > 1) {
      t <- t %>% 
        mutate(Grade = as.character(Grade))
      
      p1 <- ggplot(t, aes(y = 10^foveal, 
                          x = 10^(peripheral),
                          color = Grade)) + 
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        labs(y = 'Foveal crowding (deg) ',
             x = 'Peripheral crowding (deg)',
             title = 'Foveal vs peripheral crowding\ncolored by grade') +
        coord_fixed()
    } 
    
    if (n_distinct(t$age) > 1) {
      p <- ggplot(t, aes(y = 10^foveal, 
                         x = 10^(peripheral),
                         color = age))
    } else {
      p <- ggplot(t, aes(y = 10^foveal, 
                         x = 10^(peripheral)))
    }
    
    minor_breaks_x <- unlist(lapply(1:10, function(i) 10^(i-1) * seq(1, 9, by = 1)))
    minor_breaks_y <- unlist(lapply(1:10, function(i) 10^(i-1) * seq(1, 9, by = 1)))
    p <- p + 
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      scale_x_log10(minor_breaks = minor_breaks_x) + 
      scale_y_log10(minor_breaks = minor_breaks_y) + 
      theme_bw() +
      annotation_logticks(short = unit(0.1, "cm"),                                                
                          mid = unit(0.1, "cm"),
                          long = unit(0.3, "cm")) + 
      labs(y = 'Foveal crowding (deg) ',
           x = 'Peripheral crowding (deg)',
           title = 'Foveal vs peripheral crowding colored by age') +
      coord_fixed()
    
    if (n_distinct(t$`Skilled reader?`) == 1) {
      p <- p + geom_point()
      p1 <- p1 + geom_point()
    } else {
      p <- p + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4,19))
      p1 <- p1 + 
        geom_point(aes(shape = `Skilled reader?`)) +
        scale_shape_manual(values = c(4,19))
    }
    print('done foveal_peripheral_diag')
    return(list(age = p,
                grade = p1))
  }
}



