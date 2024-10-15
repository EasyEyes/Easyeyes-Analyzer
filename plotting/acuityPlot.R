library(plotly)
source('./constant.R')
get_foveal_acuity_vs_age <- function(acuity) {
  t <- acuity %>% filter(!is.na(age),
                         targetEccentricityXDeg == 0)
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    p <-  ggplot(t, aes(x = age, y = questMeanAtEndOfTrialsLoop)) +
      geom_point() +
      theme_bw() +
      labs(title = 'Foveal cuity vs age',
           x = 'Age',
           y = 'Acuity (deg)')
    return(p)
  }
}


get_peripheral_acuity_vs_age <- function(acuity) {
  t <- acuity %>% filter(!is.na(age),
                         targetEccentricityXDeg != 0)
  if (nrow(t) == 0) {
    return(NULL)
  } else {
    p <-  ggplot(t, aes(x = age, y = questMeanAtEndOfTrialsLoop)) +
      geom_point() +
      theme_bw() +
      labs(title = 'Peripheral acuity vs age',
           x = 'Age',
           y = 'Acuity (deg)')
    return(p)
  }
}

plot_acuity_reading <- function(acuity,reading){
  
}


plot_acuity_rsvp <- function(acuity, rsvp, df, pretest, type) {
  create_plot <- function(data,type,colorFactor) {
    
    # Merge data and calculate WPM and acuity
    data_rsvp <- data %>%
      select(participant, questMeanAtEndOfTrialsLoop) %>%
      left_join(rsvp, by = "participant") %>%
      mutate(WPM = 10^(block_avg_log_WPM),
             acuity = 10^(questMeanAtEndOfTrialsLoop))
    
    if (nrow(df) > 0) {
      data_rsvp <- data_rsvp %>% left_join(df, by = 'participant')
    } else {
      data_rsvp <- data_rsvp %>% mutate(ParticipantCode = participant)
    }
    
    if (nrow(pretest) > 0) {
      pretest <- pretest %>% mutate(participant = tolower(participant))
      
      data_rsvp <- data_rsvp %>% 
        select(-ParticipantCode) %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(questMeanAtEndOfTrialsLoop),
               Y = 10^(block_avg_log_WPM))
      if ('age' %in% names(data_rsvp) & n_distinct(data_rsvp$age) > 1) {
        data_rsvp$Age = format(data_rsvp$age, nsmall=2)
      }
      
      if (colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% mutate(factorC = Age,
                                      Grade = as.character(Grade))
      } else {
        data_rsvp <- data_rsvp %>% mutate(Age = Age,
                                      factorC = as.character(Grade))
      } 
       
      pointshapes <-  c(17,19)
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      if ('age' %in% names(data_rsvp) & colorFactor == 'Age') {
        data_rsvp <- data_rsvp %>% 
          mutate(factorC = as.character(age))
      }
      pointshapes <- 19
    }
    if ('Skilled reader?' %in% names(data_rsvp)) {
      data_for_stat <- data_rsvp %>%
        filter(`Skilled reader?` == TRUE) %>%
        select(block_avg_log_WPM,questMeanAtEndOfTrialsLoop)
    } else {
      data_for_stat <- data_rsvp %>%
        select(block_avg_log_WPM,questMeanAtEndOfTrialsLoop)
    }
    data_for_stat <- data_for_stat[complete.cases(data_for_stat),]
    # Calculate correlation and slope
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, questMeanAtEndOfTrialsLoop, method = "pearson"),
                N = n()) %>%
      mutate(correlation = round(correlation, 2))

    
    slope <- data_for_stat %>%
      do(fit = lm(block_avg_log_WPM ~ questMeanAtEndOfTrialsLoop, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      filter(term == 'questMeanAtEndOfTrialsLoop') %>%
      select(estimate) %>%
      mutate(slope = round(estimate, 2))
    
    data_rsvp <- data_rsvp %>% 
      # group_by(factorC,`Skilled reader?`,ParticipantCode) %>% 
      # summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop),
      #           block_avg_log_WPM = mean(block_avg_log_WPM)) %>% 
      mutate(X = 10^(questMeanAtEndOfTrialsLoop),
             Y = 10^(block_avg_log_WPM))
    
    xMin <- 10^min(data_rsvp$questMeanAtEndOfTrialsLoop, na.rm = T)/3
    xMax <- 10^max(data_rsvp$questMeanAtEndOfTrialsLoop, na.rm = T)*3
    yMax <- max(10^(data_rsvp$block_avg_log_WPM), na.rm = T)
    print(paste('xMin:',xMin, 'xMax:', xMax, 'yMax:', yMax))
    
    p <- ggplot() +
      geom_point(data = data_rsvp, 
                 aes(x = X,
                     y = Y,
                     color = factorC, 
                     shape = `Skilled reader?`, 
                     group = ParticipantCode)) + 
      theme_classic() +
      scale_y_log10(breaks = c(1, 3, 10, 30, 100, 300, 1000)) +
      scale_x_log10(breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 10, 100),
                    limits = c(xMin,xMax)) +
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
        aes(x = xMax / 3,
            y = yMax * 0.8,
            label = paste0("N = ",corr$N,"\n R = ", 
                           corr$correlation,
                           "\n slope = ", slope$slope)))+
      # ggpp::geom_text_npc(
      #         aes(npcx = "right",
      #             npcy = "top",
      #             label = paste0("italic('R=')~",corr$correlation,
      #                            "~italic(', slope=')~", slope$slope)),
      #         parse = T) +
      labs(x = 'Acuity (deg)',
           y = 'RSVP reading speed (w/min)',
           title = paste('RSVP vs', type ,'acuity by', tolower(colorFactor)))

    return(p)
  }
  
  acuity <- acuity %>% mutate(participant = tolower(participant))
  rsvp <- rsvp %>% mutate(participant = tolower(participant))
  foveal <- acuity %>% filter(targetEccentricityXDeg == 0)
  peripheral <- acuity %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(rsvp) == 0 | nrow(acuity) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Acuity',
             xaxis = list(title = 'Acuity (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading speed (w/min)', type = 'log'))
    return(list(p1,p1,p1,p1))
  }
  
  if (type == 'foveal') {
    p1 <- create_plot(foveal,type,'Age')
    p2 <- create_plot(foveal,type,'Grade')
    return(list(p1, p2))
  } else {
    p3 <- create_plot(peripheral,'peripheral','Age')
    p4 <- create_plot(peripheral,'peripheral','Grade')
    return(list(
      p3, p4
    ))
  }
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