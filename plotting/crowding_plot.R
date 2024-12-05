library(scales)
source('./constant.R')


crowding_by_side <- function(crowding) {
  print('insdie crowding_by_side')
  crowding <- crowding %>% filter(targetEccentricityXDeg != 0) %>% 
    mutate(side = ifelse(targetEccentricityXDeg < 0, 'L', 'R'),
           XDeg = abs(targetEccentricityXDeg))
  
  crowding_L <- crowding %>% filter(side == "L") %>% select(-conditionName, -side)
  crowding_R <- crowding %>% filter(side == "R") %>% select(-conditionName, -side)

  crowding_L_R <- crowding_L %>% 
    full_join(crowding_R, by = c("participant","font", 'XDeg','order', 'block')) %>% 
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
  print('inside crowding_scatter_plot')
  N_left = crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Left)) %>% count()
  N_right= crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Right)) %>% count()
  N_both = crowding_L_R %>% filter(!is.na(log_crowding_distance_deg_Right),!is.na(log_crowding_distance_deg_Left)) %>% count()
  summ <- group_by(crowding_L_R) %>% 
    mutate(log_delta = log_crowding_distance_deg_Left - log_crowding_distance_deg_Right,
           log_mean = (log_crowding_distance_deg_Left+log_crowding_distance_deg_Right)/2) %>% 
    summarize(mean_log_delta = round(mean(log_delta),2),
              sd_log_delta = round(sd(log_delta),2),
              mean_left = round(mean(log_crowding_distance_deg_Left),2),
              sd_left = round(sd(log_crowding_distance_deg_Left),2),
              mean_right = round(mean(log_crowding_distance_deg_Right),2),
              sd_right = round(sd(log_crowding_distance_deg_Right),2),
              mean_avg = round(mean(log_mean),2),
              sd_avg = round(sd(log_mean),2)) %>% 
    mutate(sdTestRetest = round(sd_log_delta / sqrt(2), 2),
           sdIndividual = round(sqrt(sd_avg^2 - 0.5*(sdTestRetest^2)),2)) %>% 
    mutate(label =paste0('N= ', N_both, '\n', 
                         'log(spacingDeg left): n=', format(N_left,nsmall=2),', meanLeft= ', format(mean_left,nsmall=2), ', sdLeft= ', format(sd_left,nsmall=2), '\n',
                         'log(spacingDeg right): n=', format(N_right,nsmall=2),', meanRight= ', format(mean_right,nsmall=2), ', sdRight= ', format(sd_right,nsmall=2), '\n',
                         'log(spacingDeg left)-log(spacingDeg right): n=',format(N_both,nsmall=2), '\n',
                         'meanDiff=', format(mean_log_delta,nsmall=2), ', sdDiff= ', format(sd_log_delta,nsmall=2), '\n',
                         '[log(spacingDeg left)+log(spacingDeg right)]/2: n=', format(N_both,nsmall=2), '\n',
                         'meanAvg=', format(mean_avg,nsmall=2), ', sdAvg=', format(sd_avg,nsmall=2), '\n',
                         'sdTestRetest=', format(sdTestRetest,nsmall=2), '\n',
                         'sdIndividual=', format(sdIndividual,nsmall=2)
                         ))

  minXY <- min(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right) * 0.8
  maxXY <- max(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right) * 1.2
  p <- ggplot(crowding_L_R,aes(y = 10^(log_crowding_distance_deg_Left), x = 10^(log_crowding_distance_deg_Right))) + 
    geom_point(size = 1) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10(limits = c(10^(minXY), 
                             10^(maxXY)),
                  breaks = c(0.1, 0.3, 1),
                  expand=c(0,0)) +
    scale_x_log10(limits = c(10^(minXY), 
                             10^(maxXY)),
                  breaks = c(0.1, 0.3, 1),
                  expand=c(0,0)) + 
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    coord_fixed() + 
    theme_bw() + 
    # ggpp::geom_text_npc(
    #   data = summ,
    #   aes(npcx = "left",
    #       npcy = "top",
    #       label = label)) + 
  labs(x = "Right crowding distance (deg)",
       y = "Left crowding distance (deg)",
       title = "Left vs right peripheral crowding",
       caption = summ$label
  ) + 
    theme(plot.caption = element_text(hjust = 0, size = 12))
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
    group_by(participant,age,Grade,`Skilled reader?`, block) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg,na.rm = T)) %>% 
    ungroup() %>% 
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
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant,age,Grade,`Skilled reader?`, block) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg,na.rm = T)) %>% 
    ungroup()
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
      group_by(participant) %>% 
      summarize(log_acuity = mean(log_acuity, na.rm = T)) %>% 
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
               subtitle = "Geometric average of left \nand right thresholds",
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
  peripheral <- crowding %>%
    filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant,age,Grade,`Skilled reader?`, block) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg,na.rm = T)) %>% 
    ungroup()
  
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

plot_crowding_vs_age <- function(allData){
  crowding <- allData$crowding %>% mutate(ageN = as.numeric(age))
  stats <- crowding %>% 
    do(fit = lm(log_crowding_distance_deg ~ ageN, data = .)) %>%
    transmute(coef = map(fit, tidy)) %>%
    unnest(coef) %>%
    filter(term == "ageN") %>%  
    mutate(slope = format(round(estimate, 2),nsmall=2)) %>%  
    select(slope)
  corr <- format(round(cor(crowding$ageN,crowding$log_crowding_distance_deg,use = "complete.obs"),2),nsmall=2)
  ggplot(data = crowding, aes(x = ageN, 
                              y = 10^(log_crowding_distance_deg),
                              color = questType)) + 
    annotation_logticks(sides = 'l') + 
    geom_smooth(method = 'lm', se=F) +
    geom_point()+ 
    ggpp::geom_text_npc(
      size = 8,
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0('slope=', stats$slope, ', R=', corr))) +
    scale_y_log10() + 
    guides(color=guide_legend(title = '')) + 
    labs(x = 'Age',
         y = 'Log crowding distance (deg)',
         'Foveal and peripheral\ncrowding vs age')
}

