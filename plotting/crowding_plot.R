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
    return(ggplot())
  }
  if(n_distinct(crowding_L_R$font) == 1) {
    if (is.na(unique(crowding_L_R$log_crowding_distance_deg_Left)[1])) {
      return(ggplot())
    }
  }

  summ <- group_by(crowding_L_R, font) %>% 
    summarize(R = cor(log(log_crowding_distance_deg_Left), log(log_crowding_distance_deg_Right)),
              ratio = round(mean(log_crowding_distance_deg_Right) / mean(log_crowding_distance_deg_Left),2),
              N = n()) %>%
    mutate(R = format(R,nsmall=2),
           ratio = format(ratio, nsmall=2)) %>% 
    mutate(label =  paste0("italic('R=')~", 
                           R,
                           "~italic(', right:left=')~", ratio),
           N = paste0("italic('N=')~", N))
  minX <- min(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right)
  maxX <- max(crowding_L_R$log_crowding_distance_deg_Left, crowding_L_R$log_crowding_distance_deg_Right)
  ggplot(crowding_L_R,aes(x = log_crowding_distance_deg_Left, y = log_crowding_distance_deg_Right)) + 
    geom_point(size = 1) + 
    facet_wrap(~font) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10(limits = c(minX, 
                             maxX),
                  breaks = c(0.1, 0.3, 1, 3)) +
    scale_x_log10(limits = c(minX, 
                             maxX),
                  breaks = c(0.1, 0.3, 1, 3)) + 
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    xlab("Left") + 
    ylab("Right") + 
    theme_bw() + 
    ggpp::geom_text_npc(
      data = summ,
      aes(npcx = "left",
          npcy = "bottom",
          label = label), 
      parse = T) + 
    ggpp::geom_text_npc(
      data = summ,
      aes(npcx = "left",
          npcy = "top",
          label = N),
      parse = T)
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
    p <-  ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg))) +
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      geom_point() +
      theme_bw() +
      labs(title = 'Foveal crowding vs age',
           x = 'Age',
           y = 'Crowding distance (deg)')
    return(p)
  }
}

get_peripheral_crowding_vs_age <- function(crowding) {
  t <- crowding %>% filter(!is.na(age),
                           targetEccentricityXDeg != 0) %>% 
    mutate(N = paste0('N=',n()))
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    p <-  ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg))) +
      geom_point() +
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      theme_bw() +
      labs(title = 'Peripheral crowding vs age',
           x = 'Age',
           y = 'Crowding distance (deg)')
    return(p)
  }
}

get_repeatedLetter_vs_age <- function(repeatedLetters) {
  t <- repeatedLetters %>% filter(!is.na(age)) %>% 
    mutate(N = paste0('N=',n()))
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    p <-  ggplot(t, aes(x = age, y = 10^(log_crowding_distance_deg))) +
      scale_y_log10() + 
      geom_point() +
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      theme_bw() +
      labs(title = 'Repeated-letter crowding vs age',
           x = 'Age',
           y = 'Repeated-letter crowding distance (deg)')
    return(p)
  }
}

get_crowding_vs_repeatedLetter <- function(crowding, repeatedLetters, pretest) {
  t <- repeatedLetters %>%
    rename('repeatedLetters' = 'log_crowding_distance_deg') %>% 
    select(participant, order, repeatedLetters) %>% 
    left_join(crowding, by = 'participant', 'order') %>% 
    mutate(age = format(age, nsmall=2),
           N = paste0('N=',n()))
  
  if (nrow(t) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    if (nrow(pretest) > 0) {
      t <- t %>% left_join(pretest, by = 'participant') %>% 
        mutate(Grade = as.character(Grade))
      p <-  ggplot(t, aes(x = 10^(log_crowding_distance_deg), y = 10^(repeatedLetters), color = age)) +
      p1 <-  ggplot(t, aes(x = 10^(log_crowding_distance_deg), y = 10^(repeatedLetters), color = age)) +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        geom_point() +
        scale_y_log10() + 
        scale_x_log10() + 
        theme_bw() +
        labs(title = 'Repeated-letter vs crowding by grade',
             x = 'Crowding distance (deg)',
             y = 'Repeated-letter crowding distance (deg)')
    } else {
      if (n_distinct(t$age) > 1) {
        t <- t %>% mutate(age = format(age,nsmall=2))
        p <-  ggplot(t, aes(x = 10^(log_crowding_distance_deg), y = 10^(repeatedLetters), color = age))
        p1 <- NULL
      } else {
        p <-  ggplot(t, aes(x = 10^(log_crowding_distance_deg), y = 10^(repeatedLetters)))
        p1 <- NULL
      }
     
    }
  
  } 
  p <- p + 
    ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
    geom_point() +
    scale_y_log10() + 
    scale_x_log10() + 
    theme_bw() +
    labs(title = 'Repeated-letter vs crowding by age',
         x = 'Crowding distance (deg)',
         y = 'Repeated-letter crowding distance (deg)')
  return(list(age = p, grade = p1))
}

get_foveal_acuity_diag <- function(crowding, acuity, pretest) {
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  
  if (nrow(foveal) == 0 | nrow(acuity) == 0) {
    return(list(
      age = NULL,
      grade = NULL
    ))
  } else {
    foveal_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>% 
      filter(targetEccentricityXDeg == 0) %>% 
      select(participant, log_acuity) %>% 
      left_join(foveal, by = 'participant') %>% 
      mutate(age = format(age,nsmall=2),
             N = paste0('N=',n()))
    if (nrow(foveal_acuity) == 0 ) {
      p <- NULL
      p1 <- NULL
    } else {
      if (nrow(pretest) > 0) {
        foveal_acuity <- foveal_acuity %>% 
          left_join(pretest, by = 'participant') %>% 
          mutate(Grade = as.character(Grade))
        p <-  ggplot(foveal_acuity, aes(x = 10^log_crowding_distance_deg,
                                        y = 10^(log_acuity),
                                        shape = `Skilled reader?`,
                                        color = age)) +
          labs(title = 'Foveal acuity vs foveal crowding by age',
               x = 'Crowding distance (deg)',
               y = 'Acuity (deg)')
        
        p1 <-  ggplot(foveal_acuity, aes(x = 10^log_crowding_distance_deg,
                                        y = 10^(log_acuity),
                                        shape = `Skilled reader?`,
                                        color = Grade)) + 
          geom_point() +
          ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
          scale_x_log10() + 
          scale_y_log10() + 
          theme_bw() +
          scale_shape_manual(values = c(4,19)) + 
          annotation_logticks(short = unit(0.1, "cm"),                                                
                              mid = unit(0.1, "cm"),
                              long = unit(0.3, "cm")) + 
          labs(title = 'Foveal acuity vs foveal crowding by grade',
               x = 'Crowding distance (deg)',
               y = 'Acuity (deg)') + 
          coord_fixed()
      } else {
        if (n_distinct(foveal_acuity$age) > 1) {
          foveal_acuity <- foveal_acuity %>% mutate(age = format(age,nsmall=2))
          p <-  ggplot(foveal_acuity, aes(x = 10^log_crowding_distance_deg, y = 10^(log_acuity), color = age)) +
          labs(title = 'Foveal acuity vs foveal crowding by age',
               x = 'Crowding distance (deg)',
               y = 'Acuity (deg)')
          p1 <- NULL
        } else {
          p <-  ggplot(foveal_acuity, aes(x = 10^log_crowding_distance_deg, y = 10^(log_acuity))) +
            labs(title = 'Foveal acuity vs foveal crowding',
                 x = 'Crowding distance (deg)',
                 y = 'Acuity (deg)')
          p1 <- NULL
        }
        
      }
      p <- p + 
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        geom_point() +
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        scale_shape_manual(values = c(4,19)) + 
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        coord_fixed()
    }
    
    peripheral_acuity <- acuity %>%
      rename('log_acuity' = 'questMeanAtEndOfTrialsLoop') %>% 
      filter(targetEccentricityXDeg != 0) %>% 
      select(participant, log_acuity) %>% 
      left_join(foveal, by = 'participant') %>% 
      mutate(N = paste0('N=',n()))
    if (nrow(peripheral_acuity) == 0 ) {
      p2 <- NULL
      p3 <- NULL
    } else {
      if (nrow(pretest) > 0) {
        peripheral_acuity <- peripheral_acuity %>% left_join(pretest, by = 'participant')
        p2 <-  ggplot(peripheral_acuity, aes(x = 10^log_crowding_distance_deg,
                                        y = 10^(log_acuity),
                                        shape = `Skilled reader?`,
                                        color = age))
        p3 <-  ggplot(peripheral_acuity, aes(x = 10^log_crowding_distance_deg,
                                             y = 10^(log_acuity),
                                             shape = `Skilled reader?`,
                                             color = Grade)) + 
          geom_point() +
          ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
          scale_x_log10() + 
          scale_y_log10() + 
          theme_bw() +
          scale_shape_manual(values = c(4,19)) + 
          annotation_logticks(short = unit(0.1, "cm"),                                                
                              mid = unit(0.1, "cm"),
                              long = unit(0.3, "cm")) + 
          labs(title = 'Peripheral acuity vs foveal crowding by Grade',
               x = 'Crowding distance (deg)',
               y = 'Acuity (deg)') + 
          coord_fixed()
      } else {
        if (n_distinct(peripheral_acuity$age) > 1) {
          peripheral_acuity <- peripheral_acuity %>% mutate(age = format(age,nsmall=2))
          p2 <-  ggplot(peripheral_acuity, aes(x = 10^log_crowding_distance_deg, y = 10^(log_acuity), color = age))
          p3 <- NULL
        } else {
          p2 <-  ggplot(peripheral_acuity, aes(x = 10^log_crowding_distance_deg, y = 10^(log_acuity)))
          p3 <- NULL
        }
        
      }
      p2 <- p2 + 
        geom_point() +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        scale_shape_manual(values = c(4,19)) + 
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        labs(title = 'Peripheral acuity vs foveal crowding',
             x = 'Crowding distance (deg)',
             y = 'Acuity (deg)') + 
        coord_fixed()
    }
    
   
   
    return(list(
      foveal = list(age = p,grade = p1),
      peripheral = list(age = p2,grade = p3)
    ))
  }
  

   
}

get_foveal_peripheral_diag <- function(crowding, pretest) {
  
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  if (nrow(foveal) == 0 | nrow(peripheral) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    t <- foveal %>% 
      rename('foveal' = 'log_crowding_distance_deg') %>% 
      select(foveal, participant, order) %>% 
      left_join(peripheral,by = 'participant', 'order') %>% 
      rename('peripheral' = 'log_crowding_distance_deg') %>% 
      mutate(age = format(age,nsmall=2),
             N = paste0('N=',n()))
    if (nrow(pretest) > 0) {
      t <- t %>% left_join(pretest, by ='participant') %>% 
        mutate(Grade = as.character(Grade))
      p <- ggplot(t, aes(y = 10^foveal, 
                          x = 10^(peripheral),
                          shape = `Skilled reader?`,
                          color = age))
      p1 <- ggplot(t, aes(y = 10^foveal, 
                          x = 10^(peripheral),
                          shape = `Skilled reader?`,
                          color = Grade)) + 
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        geom_point() +
        scale_x_log10() + 
        scale_y_log10() + 
        theme_bw() +
        scale_shape_manual(values = c(4,19)) + 
        annotation_logticks(short = unit(0.1, "cm"),                                                
                            mid = unit(0.1, "cm"),
                            long = unit(0.3, "cm")) + 
        labs(y = 'Foveal crowding (deg) ',
             x = 'Peripheral crowding (deg)',
             title = 'Foveal vs peripheral crowding by grade') +
        coord_fixed()
    } else {
      
      if (n_distinct(t$age) > 1) {
        p <-  ggplot(t, aes(y = 10^foveal, x = 10^(peripheral), color = age))
        p1 <- NULL
         } else {
           p <-  ggplot(t, aes(y = 10^foveal, x = 10^(peripheral)))
           p1 <- NULL
         }
      }
    p <- p + 
      ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
      geom_point() +
      scale_x_log10() + 
      scale_y_log10() + 
      theme_bw() +
      scale_shape_manual(values = c(4,19)) + 
      annotation_logticks(short = unit(0.1, "cm"),                                                
                          mid = unit(0.1, "cm"),
                          long = unit(0.3, "cm")) + 
      labs(y = 'Foveal crowding (deg) ',
           x = 'Peripheral crowding (deg)',
           title = 'Foveal vs peripheral crowding by age') +
      coord_fixed()
    return(list(age = p,
                grade = p1))
  }
}



