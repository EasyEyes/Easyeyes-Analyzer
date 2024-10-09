library(ggcorrplot) 
library(plotly)
source('./constant.R')
plot_rsvp_crowding_acuity <- function(allData,df,pretest) {
  if (is.null(allData) | nrow(pretest) < 1) {
  # if (T) {
    return(list(
      ggplot(),
      ggplot(),
      ggplot(),
      ggplot()
    ))
  }
  
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  acuity <- allData$acuity %>% rename('log_acuity'='questMeanAtEndOfTrialsLoop')
    
  rsvp_speed <- rsvp_speed %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant') %>% 
    mutate(Age = format(age,nsmall=2))
  crowding <- crowding %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant') %>% 
    mutate(Age = format(age,nsmall=2))
  acuity <- acuity %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant') %>% 
    mutate(Age = format(age,nsmall=2))
  
  

  p1 = ggplot(data = crowding) +
    geom_point(aes(x = Grade, 
                   y = 10^(log_crowding_distance_deg),
                   shape = `Skilled reader?`,
                   color = Age)) +
    facet_wrap(font~.) + 
    theme_classic() +
    scale_y_log10() + 
    annotation_logticks(sides = 'l') + 
    scale_shape_manual(values = c(17,19)) + 
    labs(title = 'Crowding vs Grade',
         y = 'Crowding (deg)')
  
  p2 = ggplot(data = rsvp_speed) +
    geom_point(aes(x = Grade, 
                   y = 10^(block_avg_log_WPM), 
                   shape = `Skilled reader?`,
                   color = Age,
                   )) +
    theme_classic() + 
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    scale_shape_manual(values = c(17,19)) + 
    labs(title = 'RSVP vs Grade',
         y = 'RSVP reading speed (w/min)')
  
  p3 = ggplot(data = acuity) +
    geom_point(aes(x = Grade, 
                   y = 10^(log_acuity), 
                   shape = `Skilled reader?`,
                   color =  Age)) +
    theme_classic() +
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    scale_shape_manual(values = c(17,19)) + 
    labs(title = 'Acuity vs Grade',
         y = 'Acuity (deg)')

  return(list(
    p1,
    p2,
    p3
  ))
}

plot_rsvp_crowding_plotly <- function(allData, df, pretest) {
  # Helper function to compute correlation, slope, and plot
  create_plot <- function(data, condition, colorFactor) {

    rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      left_join(rsvp, by = "participant") %>% 
      distinct(participant, block_avg_log_WPM, log_crowding_distance_deg,age) %>%
      filter(!is.na(participant)) %>% 
      mutate(age = format(age,nsmall=2))
    
    
    if (nrow(pretest) > 0) {

      pretest <- pretest %>% mutate(participant = tolower(participant))
    
      data_rsvp <- data_rsvp %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(17,19)
      
      
      if (colorFactor =='Age') {
        data_rsvp <- data_rsvp %>% mutate(factorC = age)
      } else {
        data_rsvp <- data_rsvp %>% mutate(factorC = as.character(Grade))
      }
      
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      if ('age' %in% names(data_rsvp) & colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% 
          mutate(factorC = age)
      }
      pointshapes <-  c(19)
    }
    
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` == TRUE)
    } else {
      data_for_stat <- data_rsvp
    }
    
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
                                  method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      mutate(WPM = 10^(block_avg_log_WPM),
             cdd = 10^(log_crowding_distance_deg)) %>%
      do(fit = lm(WPM ~ cdd, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      mutate(slope = round(estimate, 2)) %>%
      filter(term == 'cdd') %>%
      select(-term)
    
    data_rsvp <- data_rsvp %>%
      mutate(label = paste0("italic('R=')~",
                            corr$correlation,
                            "~italic(', slope=')~", slope$slope)) %>% 
      # group_by(factorC,label,`Skilled reader?`,ParticipantCode) %>% 
      # summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg),
      #           block_avg_log_WPM = mean(block_avg_log_WPM)) %>% 
      mutate(X = 10^(log_crowding_distance_deg),
             Y = 10^(block_avg_log_WPM))
    
    p <- ggplot() + 
      geom_point(data = data_rsvp, 
                 aes(x = X,
                     y = Y,
                     color = factorC, shape =`Skilled reader?`, group = ParticipantCode)) +
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$log_crowding_distance_deg), 100)) +
      geom_smooth(data = data_for_stat,
                  aes(x = 10^(log_crowding_distance_deg),
                      y = 10^(block_avg_log_WPM)),
                  method = 'lm', se = FALSE) +
      scale_shape_manual(values = pointshapes) + 
      annotation_logticks() +
      coord_fixed(ratio = 1) +
     geom_text(
        aes(x =30,
            y = max(10^(data_rsvp$block_avg_log_WPM))*0.8,
            label = paste0("R = ", 
                           corr$correlation,
                           ",\n slope = ", slope$slope)))+
      plt_theme +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC)==1, 'none','top')) + 
      guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
             shape = guide_legend(title='')) + 
      labs(x = 'Crowding distance (deg)',
           y = 'RSVP reading (w/min)',
           title = paste('RSVP vs', tolower(condition), 'crowding by', tolower(colorFactor)))
      
    pp <- ggplotly(p)
    
    return(pp)
  }
  
  # Extract and filter data
  crowding <- allData$crowding %>%  mutate(participant = tolower(participant))
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(allData$rsvp) == 0 | nrow(allData$crowding) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs peripheral Crowding by Age',
             xaxis = list(title = 'Crowding distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP reading speed (w/min)', type = 'log'))
    p2 <- plot_ly() %>% 
      layout(title = 'RSVP vs foveal crowding by Age',
             xaxis = list(title = 'Crowding distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP reading speed (w/min)', type = 'log'))
    return(list(p1, p2, p1, p2))
  }
  
  # Create plots for peripheral and foveal data
  p1 <- create_plot(peripheral, "Peripheral",'Age')
  p2 <- create_plot(foveal, "Foveal",'Age')
  p3 <- create_plot(peripheral, "Peripheral",'Grade')
  p4 <- create_plot(foveal, "Foveal",'Grade')
  
  return(list(p1, p2, p3,p4))
}

plot_rsvp_crowding <- function(allData, df, pretest) {
  # Helper function to compute correlation, slope, and plot
  create_plot <- function(data, condition, colorFactor) {
    rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      left_join(rsvp, by = "participant") %>% 
      distinct(participant, block_avg_log_WPM, log_crowding_distance_deg,age) %>%
      filter(!is.na(participant)) %>% 
      mutate(age = format(age,nsmall=2))
    
    
    if (nrow(pretest) > 0) {
      pretest <- pretest %>% mutate(participant = tolower(participant))
      
      data_rsvp <- data_rsvp %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(17,19)
      
      
      if (colorFactor =='Age') {
        data_rsvp <- data_rsvp %>% mutate(factorC = age)
      } else {
        data_rsvp <- data_rsvp %>% mutate(factorC = as.character(Grade))
      }
      
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      if ('age' %in% names(data_rsvp) & colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% 
          mutate(factorC = age)
      }
      pointshapes <-  c(19)
    }
    
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` == TRUE)
    } else {
      data_for_stat <- data_rsvp
    }
    
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
                                  method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      mutate(WPM = 10^(block_avg_log_WPM),
             cdd = 10^(log_crowding_distance_deg)) %>%
      do(fit = lm(WPM ~ cdd, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      mutate(slope = round(estimate, 2)) %>%
      filter(term == 'cdd') %>%
      select(-term)
    
    data_rsvp <- data_rsvp %>%
      mutate(label = paste0("italic('R=')~",
                            corr$correlation,
                            "~italic(', slope=')~", slope$slope)) %>% 
      # group_by(factorC,label,`Skilled reader?`,ParticipantCode) %>% 
      # summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg),
      #           block_avg_log_WPM = mean(block_avg_log_WPM)) %>% 
      mutate(X = 10^(log_crowding_distance_deg),
             Y = 10^(block_avg_log_WPM))
    
    p <- ggplot() + 
      geom_point(data = data_rsvp, 
                 aes(x = X,
                     y = Y,
                     color = factorC, shape =`Skilled reader?`, group = ParticipantCode)) +
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$log_crowding_distance_deg), 100)) +
      geom_smooth(data = data_for_stat,
                  aes(x = 10^(log_crowding_distance_deg),
                      y = 10^(block_avg_log_WPM)),
                  method = 'lm', se = FALSE) +
      scale_shape_manual(values = pointshapes) + 
      annotation_logticks() +
      coord_fixed(ratio = 1) +
      geom_text(
        aes(x =30,
            y = max(10^(data_rsvp$block_avg_log_WPM))*0.8,
            label = paste0("R = ", 
                           corr$correlation,
                           ",\n slope = ", slope$slope)))+
      plt_theme +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC)==1, 'none','top')) + 
      guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
             shape = guide_legend(title='')) + 
      labs(x = 'Crowding distance (deg)',
           y = 'RSVP reading (w/min)',
           title = paste('RSVP vs', tolower(condition), 'crowding by', tolower(colorFactor)))
    
    return(p)
  }
  
  crowding <- allData$crowding %>% mutate(participant = tolower(participant))
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(allData$rsvp) == 0 | nrow(allData$crowding) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Peripheral Crowding by Age',
             xaxis = list(title = 'Crowding Distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading (w/min)', type = 'log'))
    p2 <- plot_ly() %>% 
      layout(title = 'RSVP vs Foveal Crowding by Age',
             xaxis = list(title = 'Crowding Distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading (w/min)', type = 'log'))
    return(list(p1, p2, p1, p2))
  }
  
  # Create plots for peripheral and foveal data
  p1 <- create_plot(peripheral, "Peripheral",'Age')
  p2 <- create_plot(foveal, "Foveal",'Age')
  p3 <- create_plot(peripheral, "Peripheral",'Grade')
  p4 <- create_plot(foveal, "Foveal",'Grade')
  
  return(list(p1, p2, p3,p4))
}

getCorrMatrix <- function(allData,df,pretest) {
  if (nrow(pretest) > 0) {
    pretest_for_corr <- pretest
    pretest_for_corr <- lapply(pretest_for_corr, as.numeric)
    pretest_for_corr <- tibble(data.frame(pretest_for_corr))
    colnames(pretest_for_corr) <- colnames(pretest)
    
    pretest_for_corr <- pretest_for_corr %>%   
      select_if(~sum(!is.na(.)) > 0)
    pretest_for_corr$participant <- pretest$participant
    pretest_for_corr <- pretest_for_corr %>%
      mutate(participant = tolower(participant))
  }
  
  
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  acuity <- allData$acuity %>%
    rename('log_acuity'='questMeanAtEndOfTrialsLoop') %>% 
    mutate(type=ifelse(
      targetEccentricityXDeg == 0,
      'log foveal acuity',
      'log peripheral acuity'
    )) %>% 
    group_by(participant, type) %>% 
    summarize(log_acuity = mean(log_acuity)) %>% 
    pivot_wider(names_from=type, values_from = log_acuity)
 
  
  crowdingW <- crowding %>% mutate(type=ifelse(
    targetEccentricityXDeg == 0,
    'log foveal crowding',
    'log peripheral crowding'
  )) %>% 
    group_by(participant,type) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg)) %>% 
    pivot_wider(names_from=type, values_from = log_crowding_distance_deg)
  
  reading <- allData$reading %>%
    mutate(participant = tolower(participant)) %>%
    rename('log reading' = 'log_WPM' ) %>% 
    select(participant, `log reading`)
  
  crowdingW <- crowdingW %>% 
    full_join(acuity, by = 'participant') %>% 
    mutate(participant = tolower(participant)) %>% 
    full_join(rsvp_speed %>% select(participant, block_avg_log_WPM) %>% mutate(participant = tolower(participant)), by = 'participant') %>% 
    full_join(reading, by = 'participant')
  
  
  crowdingW <- crowdingW %>% 
    full_join(df, by = 'participant')
  
  if (nrow(pretest) > 0) {
    crowdingW <- crowdingW %>% 
      full_join(pretest_for_corr, by = 'participant')
  }
  
  crowdingW <- crowdingW %>% 
    rename('log rsvp' = 'block_avg_log_WPM') %>% 
    ungroup() %>% 
    select_if(is.numeric) %>% 
    select(where(~sum(is.na(.)) >0))
 
  c <- colnames(crowdingW)

  t <- data.frame(cor(crowdingW[complete.cases(crowdingW),]))
  colnames(t) <- c
  t <- t %>% mutate(across(everything(), round, 3))
  
  corplot <- ggcorrplot(t,
                        show.legend = FALSE,
                        show.diag = T,
                        type = "lower",
                        colors= c('white'),
                        lab = T) + 
    theme_bw() +
    labs(x = '', y = '')
  return(list(
    plot = corplot,
    width = 2.5 + ncol(t) * 0.38,
    height = 2.5 + ncol(t) * 0.38
  ))
}
# plot_rsvp_crowding <- function(allData, df, pretest) {
#   # Helper function to compute correlation, slope, and plot
#   create_plot <- function(data, condition, colorFactor) {
#     
#     data_rsvp <- data %>%
#       select(participant, log_crowding_distance_deg) %>%
#       left_join(allData$rsvp, by = "participant")
#     
#     if (is.na(sum(data_rsvp$log_duration_s_RSVP))) {
#       
#       if(length(data$participant[1]) == 6) {
#         print(data$participant[1])
#         rsvp <- allData$rsvp %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)),str_sub(participant,5,6)))
#       } else {
#         rsvp <- allData$rsvp
#       }
#       print(rsvp)
#       data_rsvp <- data %>%
#         select(participant, log_crowding_distance_deg) %>%
#         left_join(df %>% distinct(participant, britishID), by = "participant") %>%
#         select(britishID, log_crowding_distance_deg) %>%
#         left_join(rsvp %>% left_join(df %>% distinct(participant, britishID), by = "participant"), by = "britishID") %>%
#         mutate(WPM = 10^(block_avg_log_WPM),
#                acuity = 10^(log_crowding_distance_deg),
#                participant = ifelse(age < 10, paste0(britishID, '0', age), paste0(britishID, age))) %>%
#         distinct(participant, WPM, acuity, block_avg_log_WPM, log_crowding_distance_deg,age) %>%
#         filter(!is.na(participant)) %>% 
#         mutate(Age = as.character(age))
#     }
#     
#     corr <- data_rsvp %>%
#       summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
#                                   method = "pearson")) %>%
#       mutate(correlation = round(correlation, 2))
#     
#     slope <- data_rsvp %>%
#       mutate(WPM = 10^(block_avg_log_WPM),
#              cdd = 10^(log_crowding_distance_deg)) %>%
#       do(fit = lm(WPM ~ cdd, data = .)) %>%
#       transmute(coef = map(fit, tidy)) %>%
#       unnest(coef) %>%
#       mutate(slope = round(estimate, 2)) %>%
#       filter(term == 'cdd') %>%
#       select(-term)
#     
#     data_rsvp <- data_rsvp %>%
#       mutate(label = paste0("italic('R=')~",
#                             corr$correlation,
#                             "~italic(', slope=')~", slope$slope))
#     
#     if (nrow(pretest) > 0) {
#       if (colorFactor =='Age') {
#         pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)))
#       } else {
#         pretest <- pretest %>% mutate(factorC = as.character(Grade))
#       }
#       
#       data_rsvp <- data_rsvp %>% 
#         left_join(pretest, by = 'participant') %>% 
#         mutate(X = 10^(log_crowding_distance_deg),
#                Y = 10^(block_avg_log_WPM))
#       pointshapes <-  c(4,19)
#     } else {
#       data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
#       data_rsvp <- data_rsvp %>% 
#         mutate(X = 10^(log_crowding_distance_deg),
#                Y = 10^(block_avg_log_WPM))
#       pointshapes <-  c(19)
#     }
#     
#     p <- ggplot(data = data_rsvp, 
#                 aes(x = X,
#                     y = Y
#                 )) +
#       geom_point(aes(color = factorC, shape =`Skilled reader?`)) +
#       theme_classic() +
#       scale_y_log10(breaks = c(1, 10, 100, 1000)) +
#       scale_x_log10(breaks = c(0.3, 1, 10, 100),
#                     limits = c(10^min(data_rsvp$log_crowding_distance_deg), 100)) +
#       geom_smooth(method = 'lm', se = FALSE) +
#       scale_shape_manual(values = pointshapes) + 
#       annotation_logticks() +
#       coord_fixed(ratio = 1) +
#       ggpp::geom_text_npc(
#         aes(npcx = "left",
#             npcy = "top",
#             label = label),
#         parse = TRUE) +
#       theme(legend.position = ifelse(n_distinct(data_rsvp$`Skilled reader?`)==1, 'none','top')) + 
#       guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
#              shape = guide_legend(title='')) + 
#       labs(x = 'crowding distance (deg)',
#            y = 'rsvp reading (word/min)',
#            title = paste('rsvp vs', condition, 'crowding by', tolower(colorFactor)))
#     
#     return(p)
#   }
#   
#   # Extract and filter data
#   crowding <- allData$crowding
#   foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
#   peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
#   
#   if (nrow(allData$rsvp) == 0 | nrow(allData$crowding) == 0) {
#     p1 <- ggplot() +
#       labs(x = 'crowding distance (deg)',
#            y = 'rsvp reading (word/min)',
#            title = 'rsvp vs peripheral crowding')
#     p2 <- ggplot() +
#       labs(x = 'crowding distance degree',
#            y = 'rsvp reading (word/min)',
#            title = 'rsvp vs foveal crowding')
#     return(list(p1, p2,p1,p2))
#   }
#   
#   # Create plots for peripheral and foveal data
#   p1 <- create_plot(peripheral, "peripheral", 'Age')
#   p2 <- create_plot(foveal, "foveal", 'Age')
#   p3 <- create_plot(peripheral, "peripheral", 'Grade')
#   p4 <- create_plot(foveal, "foveal", 'Grade')
#   
#   return(list(p1, p2, p3, p4))
# }