library(plotly)
source('./constant.R')
get_acuity_vs_age <- function(acuity) {
  if (nrow(acuity) == 0) {
    return(ggplot() + ggtitle('acuity vs age')) + theme_bw()
  }
  t <- acuity %>% filter(!is.na(age))
  if (nrow(t) == 0) {
    return(ggplot() + theme_bw() + ggtitle('acuity vs age'))
  } else {
    p <-  ggplot(t, aes(x = age, y = questMeanAtEndOfTrialsLoop)) +
      geom_point() +
      theme_bw() +
      labs(title = 'Acuity vs age',
           x = 'Age',
           y = 'Acuity (deg)')
    return(p)
  }
}

plot_acuity_reading <- function(acuity,reading){
  
}


plot_acuity_rsvp_plotly <- function(acuity, rsvp, df, pretest) {
  create_plot <- function(data,colorFactor) {
    
    # Merge data and calculate WPM and acuity
    data_rsvp <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop) %>%
      left_join(rsvp, by = "participant") %>%
      mutate(WPM = 10^(block_avg_log_WPM),
             acuity = 10^(questMeanAtEndOfTrialsLoop))
    
    if (is.na(sum(data_rsvp$log_duration_s_RSVP))) {
      
      if(length(rsvp$participant[1]) == 6){
        rsvp <- rsvp %>%  mutate(participant = paste0(tolower(str_sub(participant,1,4)),str_sub(participant,5,6)))
      }
      data_rsvp <- data %>%
        select(participant, questMeanAtEndOfTrialsLoop) %>%
        left_join(df %>% distinct(participant, britishID), by = "participant") %>%
        select(britishID, questMeanAtEndOfTrialsLoop) %>%
        left_join(rsvp %>% left_join(df %>% distinct(participant, britishID), by = "participant"), by = "britishID") %>%
        mutate(WPM = 10^(block_avg_log_WPM),
               acuity = 10^(questMeanAtEndOfTrialsLoop)) %>%
        distinct(participant, WPM, acuity, block_avg_log_WPM, questMeanAtEndOfTrialsLoop,age) %>%
        filter(!is.na(participant)) %>% 
        mutate(Age = age)
    }
    
    if (nrow(df) > 0 & nrow(pretest == 0)) {
      data_rsvp <- data_rsvp %>% left_join(df, by = 'participant')
    } else {
      data_rsvp <- data_rsvp %>% mutate(ParticipantCode = participant)
    }
    
    
    # Join with additional df if it has rows
    
    
    if (nrow(pretest) > 0) {
      if (colorFactor == 'Age') {
        pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)),
                                      Grade = as.character(Grade))
      } else {
        pretest <- pretest %>% mutate(Age = as.character(format(round(Age,1), nsmall = 1)),
                                      factorC = as.character(Grade))
      } 
      data_rsvp <- data_rsvp %>% 
        select(-ParticipantCode) %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(questMeanAtEndOfTrialsLoop),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(17,19)
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      if ('Age' %in% names(data_rsvp) & colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% 
          mutate(factorC = as.character(Age))
      }
      data_rsvp <- data_rsvp %>% 
        mutate(X = 10^(questMeanAtEndOfTrialsLoop),
               Y = 10^(block_avg_log_WPM))
      pointshapes <- 19
    }
    
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` == TRUE)
    } else {
      data_for_stat <- data_rsvp
    }
    
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      do(fit = lm(block_avg_log_WPM ~ questMeanAtEndOfTrialsLoop, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == 'questMeanAtEndOfTrialsLoop') %>%
      select(estimate) %>%
      mutate(slope = round(estimate, 2))
    
    # Create plotly plot
    # p <- plot_ly(data = data_rsvp) %>%
    #   add_markers(x = ~10^(questMeanAtEndOfTrialsLoop), 
    #               y = ~10^(block_avg_log_WPM), 
    #               color = ~factorC,
    #               symbol=~`Skilled reader?`,
    #               symbols = c('x','circle'),
    #               marker = list(size = 10),
    #               hoverinfo = 'text',
    #               text = ~paste0('X: ', round(10^(questMeanAtEndOfTrialsLoop), 2),
    #                              '<br>Y: ', round(10^(block_avg_log_WPM), 2),
    #                              '<br>ParticipantCode: ', ParticipantCode)) %>%
    #   layout(title = paste('RSVP vs Acuity by ', colorFactor),
    #          xaxis = list(title = 'Acuity (deg)', type = 'log', 
    #                       tickvals = c(0.3, 1, 10, 100)),
    #          yaxis = list(title = 'RSVP Reading (word/min)', type = 'log', 
    #                       tickvals = c(1, 10, 100, 1000),
    #                       scaleanchor = "x", 
    #                       scaleratio = 1),
    #          margin = list(l = 70, r = 30, b = 50, t = 50),
    #          hovermode = 'closest',
    #          hoverlabel = list(bgcolor = 'white', font = list(color = 'black'))) %>%
    #   add_lines(x = ~10^(questMeanAtEndOfTrialsLoop), 
    #             y = ~10^(est),
    #             line = list(color = 'black', dash = 'solid'),
    #             hoverinfo = 'none', 
    #             showlegend = FALSE) %>% 
    #   add_annotations(text = paste("<i>R =</i>", corr$correlation, "<br>Slope =", slope$slope),
    #                   x = 0.05, y = 0.95, xref = 'paper', yref = 'paper',
    #                   showarrow = FALSE, font = list(size = 12))
    p <- ggplot() +
      geom_point(data = data_rsvp, 
                 aes(x = X,
                     y = Y,
                     color = factorC, 
                     shape = `Skilled reader?`, 
                     group = ParticipantCode)) + 
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$questMeanAtEndOfTrialsLoop), 100)) +
      geom_smooth(data = data_for_stat, aes(x=10^(questMeanAtEndOfTrialsLoop), 
                                            y = 10^(block_avg_log_WPM)), 
                  method = 'lm', 
                  se = FALSE) +
      annotation_logticks() +
      scale_shape_manual(values = pointshapes) + 
      plt_theme + 
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC)==1, 'none','top')) + 
      guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
             shape = guide_legend(title='')) + 
      coord_fixed(ratio = 1) +
      geom_text(
        aes(x = 30,
            y = max(10^(data_rsvp$block_avg_log_WPM))*0.8,
            label = paste0("R = ", 
                           corr$correlation,
                           ",\n slope = ", slope$slope))) +
      labs(x = 'Acuity (deg)',
           y = 'RSVP reading speed (w/min)',
           title = paste('RSVP vs', 'acuity by', tolower(colorFactor)))
    pp <- ggplotly(p)
    return(pp)
  }
  
 if (length(acuity$participant[1]) == 6) {
   acuity <- acuity %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
   rsvp <- rsvp %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
 }
  # foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  # peripheral <- acuity %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(rsvp) == 0 | nrow(acuity) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Acuity',
             xaxis = list(title = 'Acuity (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading speed (w/min)', type = 'log'))
    return(list(p1,p1))
  }
  
  p1 <- create_plot(acuity,'Age')
  p2 <- create_plot(acuity,'Grade')
  return(list(p1,p2))
}



plot_acuity_rsvp <- function(acuity, rsvp, df, pretest) {
  create_plot <- function(data,colorFactor) {
    
    # Merge data and calculate WPM and acuity
    data_rsvp <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop) %>%
      left_join(rsvp, by = "participant") %>%
      mutate(WPM = 10^(block_avg_log_WPM),
             acuity = 10^(questMeanAtEndOfTrialsLoop))
    
    if (is.na(sum(data_rsvp$log_duration_s_RSVP))) {
      
      if(length(rsvp$participant[1]) == 6){
        rsvp <- rsvp %>%  mutate(participant = paste0(tolower(str_sub(participant,1,4)),str_sub(participant,5,6)))
      }
      data_rsvp <- data %>%
        select(participant, questMeanAtEndOfTrialsLoop) %>%
        left_join(df %>% distinct(participant, britishID), by = "participant") %>%
        select(britishID, questMeanAtEndOfTrialsLoop) %>%
        left_join(rsvp %>% left_join(df %>% distinct(participant, britishID), by = "participant"), by = "britishID") %>%
        mutate(WPM = 10^(block_avg_log_WPM),
               acuity = 10^(questMeanAtEndOfTrialsLoop)) %>%
        distinct(participant, WPM, acuity, block_avg_log_WPM, questMeanAtEndOfTrialsLoop,age) %>%
        filter(!is.na(participant)) %>% 
        mutate(Age = age)
    }
    
    if (nrow(df) > 0 & nrow(pretest == 0)) {
      data_rsvp <- data_rsvp %>% left_join(df, by = 'participant')
    } else {
      data_rsvp <- data_rsvp %>% mutate(ParticipantCode = participant)
    }
    
    if (nrow(pretest) > 0) {
      if (colorFactor == 'Age') {
        pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)),
                                      Grade = as.character(Grade))
      } else {
        pretest <- pretest %>% mutate(Age = as.character(format(round(Age,1), nsmall = 1)),
                                      factorC = as.character(Grade))
      } 
      data_rsvp <- data_rsvp %>% 
        select(-ParticipantCode) %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(questMeanAtEndOfTrialsLoop),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(17,19)
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      if ('Age' %in% names(data_rsvp) & colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% 
          mutate(factorC = as.character(Age))
      }
      data_rsvp <- data_rsvp %>% 
        mutate(X = 10^(questMeanAtEndOfTrialsLoop),
               Y = 10^(block_avg_log_WPM))
      pointshapes <- 19
    }
    
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` == TRUE)
    } else {
      data_for_stat <- data_rsvp
    }
    
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      do(fit = lm(block_avg_log_WPM ~ questMeanAtEndOfTrialsLoop, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == 'questMeanAtEndOfTrialsLoop') %>%
      select(estimate) %>%
      mutate(slope = round(estimate, 2))
    
    
    p <- ggplot() +
      geom_point(data = data_rsvp, 
                 aes(x = X,
                     y = Y,
                     color = factorC, 
                     shape = `Skilled reader?`, 
                     group = ParticipantCode)) + 
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$questMeanAtEndOfTrialsLoop), 100)) +
      geom_smooth(data = data_for_stat, aes(x=10^(questMeanAtEndOfTrialsLoop), 
                                            y = 10^(block_avg_log_WPM)), 
                  method = 'lm', 
                  se = FALSE) +
      annotation_logticks() +
      scale_shape_manual(values = pointshapes) + 
      plt_theme +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC)==1, 'none','top')) + 
      guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
             shape = guide_legend(title='')) + 
      coord_fixed(ratio = 1) +
            ggpp::geom_text_npc(
              aes(npcx = "right",
                  npcy = "top",
                  label = paste0("italic('R=')~",corr$correlation,
                                 "~italic(', slope=')~", slope$slope)),
              parse = T)
      labs(x = 'Acuity (deg)',
           y = 'RSVP reading speed (w/min)',
           title = paste('RSVP vs', 'acuity by', tolower(colorFactor)))

    return(p)
  }
  
  if (length(acuity$participant[1]) == 6) {
    acuity <- acuity %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
    rsvp <- rsvp %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
  }
  # foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  # peripheral <- acuity %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(rsvp) == 0 | nrow(acuity) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Acuity',
             xaxis = list(title = 'Acuity (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading speed (w/min)', type = 'log'))
    return(list(p1,p1))
  }
  
  p1 <- create_plot(acuity,'Age')
  p2 <- create_plot(acuity,'Grade')
  return(list(p1,p2))
}

# plot_acuity_rsvp <- function(acuity, rsvp, df, pretest) {
#   create_plot <- function(data,colorFactor) {
#     # Merge data and calculate WPM and acuity
#     if(length(rsvp$participant[1]) == 6){
#       rsvp <- rsvp %>%  mutate(participant = paste0(tolower(str_sub(participant,1,4)),str_sub(participant,5,6)))
#     }
#     data_rsvp <- data %>%
#       select(participant, questMeanAtEndOfTrialsLoop) %>%
#       left_join(rsvp, by = "participant") %>%
#       mutate(WPM = 10^(block_avg_log_WPM),
#              acuity = 10^(questMeanAtEndOfTrialsLoop))
#     
#     # Handle NA case for questMeanAtEndOfTrialsLoop
#     if (is.na(sum(data_rsvp$questMeanAtEndOfTrialsLoop))) {
#       data_rsvp <- data %>%
#         select(participant, questMeanAtEndOfTrialsLoop) %>%
#         left_join(df %>% distinct(participant, britishID), by = "participant") %>%
#         select(britishID, questMeanAtEndOfTrialsLoop) %>%
#         left_join(rsvp %>% left_join(df %>% distinct(participant, britishID), by = "participant"), by = "britishID") %>%
#         mutate(WPM = 10^(block_avg_log_WPM),
#                acuity = 10^(questMeanAtEndOfTrialsLoop),
#                participant = ifelse(age < 10, paste0(britishID, '0', age), paste0(britishID, age))) %>%
#         distinct(participant, WPM, acuity, block_avg_log_WPM, questMeanAtEndOfTrialsLoop,age) %>%
#         filter(!is.na(participant))
#     }
#     
#     # Calculate correlation and slope
#     corr <- data_rsvp %>%
#       summarize(correlation = cor(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson")) %>%
#       mutate(correlation = round(correlation, 2))
#     
#     slope <- data_rsvp %>%
#       do(fit = lm(block_avg_log_WPM ~ questMeanAtEndOfTrialsLoop, data = .)) %>%
#       transmute(coef = map(fit, tidy)) %>%
#       unnest(coef) %>%
#       filter(term == 'questMeanAtEndOfTrialsLoop') %>%
#       select(estimate) %>%
#       mutate(slope = round(estimate, 2))
#     
#     # Add regression predictions
#     fit <- lm(block_avg_log_WPM ~ questMeanAtEndOfTrialsLoop, data = data_rsvp)
#     data_rsvp$est <- predict(fit, newdata = data.frame(questMeanAtEndOfTrialsLoop = data_rsvp$questMeanAtEndOfTrialsLoop))
#     
#     # Join with additional df if it has rows
#     if (nrow(df) > 0 & nrow(pretest == 0)) {
#       data_rsvp <- data_rsvp %>% left_join(df, by = 'participant')
#     } else {
#       data_rsvp <- data_rsvp %>% mutate(ParticipantCode = participant)
#     }
#     
#     if (nrow(pretest) > 0) {
#       if (colorFactor == 'Age') {
#         pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)))
#       } else {
#         pretest <- pretest %>% mutate(factorC = as.character(Grade))
#       } 
#       
#       data_rsvp <- data_rsvp %>% 
#         select(-ParticipantCode) %>% 
#         left_join(pretest, by = 'participant') %>% 
#         mutate(X = 10^(questMeanAtEndOfTrialsLoop),
#                Y = 10^(block_avg_log_WPM))
#       pointshapes <-  c(4,19)
#     } else {
#       data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
#       data_rsvp <- data_rsvp %>% 
#         mutate(X = 10^(questMeanAtEndOfTrialsLoop),
#                Y = 10^(block_avg_log_WPM))
#       pointshapes <- 19
#     }
#     
#     
#     # Create plotly plot
#     p <- ggplot(data = data_rsvp, 
#                 aes(x = X,
#                     y = Y
#                 )) +
#       geom_point(aes(color = factorC, shape =`Skilled reader?`,)) +
#       theme_classic() +
#       scale_y_log10(breaks = c(1, 10, 100, 1000)) +
#       scale_x_log10(breaks = c(0.3, 1, 10, 100),
#                     limits = c(10^min(data_rsvp$questMeanAtEndOfTrialsLoop), 100)) +
#       geom_smooth(method = 'lm', se = FALSE) +
#       annotation_logticks() +
#       scale_shape_manual(values = pointshapes) + 
#       theme(legend.position = ifelse(n_distinct(data_rsvp$`Skilled reader?`)==1, 'none','top')) + 
#       coord_fixed(ratio = 1) +
#       ggpp::geom_text_npc(
#         aes(npcx = "left",
#             npcy = "top",
#             label = paste0("R = ", 
#                            corr$correlation,
#                            ", slope = ", slope$slope)),
#         parse = TRUE) +
#       guides(color = guide_legend(title=paste0(colorFactor, ', Skilled reader?')),
#              shape = guide_legend(title='')) + 
#       labs(x = 'acuity (deg)',
#            y = 'rsvp reading (word/min)',
#            title = paste('rsvp vs', 'acuity by', lower(colorFactor)))
#     
#     return(p)
#   }
#   
#   if (length(acuity$participant[1]) == 6) {
#     acuity <- acuity %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
#     rsvp <- rsvp %>% mutate(participant = paste0(tolower(str_sub(participant,1,4)), str_sub(participant,-2,-1)))
#   }
#   
#   if (nrow(rsvp) == 0 | nrow(acuity) == 0) {
#     p1 <- ggplot() +
#       labs(x = 'acuity (deg)',
#            y = 'rsvp reading (word/min)',
#            title = 'rsvp vs acuity by age')
#     p2 <- ggplot() +
#       labs(x = 'acuity degree',
#            y = 'rsvp reading (word/min)',
#            title = 'rsvp vs acuity by grade')
#     return(list(p1,p1))
#   }
#   
#   p1 <- create_plot(acuity,'Age')
#   p2 <- create_plot(acuity,'Grade')
#   return(list(p1,p2))
# }