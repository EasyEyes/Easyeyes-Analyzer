library(ggcorrplot) 
library(plotly)
source('./constant.R')
plot_rsvp_crowding_acuity <- function(allData,df,pretest) {
  if (is.null(allData) | nrow(pretest) < 1) {

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
    mutate(Age = format(age,nsmall=2))

  foveal_crowding <- crowding %>% 
    left_join(df, by = 'participant') %>% 
    filter(targetEccentricityXDeg == 0) %>% 
    mutate(Age = format(age,nsmall=2))
  
  
  peripheral_crowding <- crowding %>% 
    left_join(df, by = 'participant') %>% 
    filter(targetEccentricityXDeg == 0) %>% 
    mutate(Age = format(age,nsmall=2))
  
  
  foveal_acuity <- acuity %>% 
    filter(targetEccentricityXDeg == 0) %>% 
    left_join(df, by = 'participant') %>% 
    mutate(Age = format(age,nsmall=2))

  p1 = NULL
  
  if (nrow(foveal_crowding) > 0){
    p1 = ggplot(data = foveal_crowding, 
                aes(x = Grade, 
                y = 10^(log_crowding_distance_deg),
                color = Age)) +
      facet_wrap(font~.) + 
      theme_classic() +
      scale_y_log10() + 
      annotation_logticks(sides = 'l') + 
      guides(shape = 'none') +
      scale_shape_manual(values = c(4,19)) + 
      labs(title = 'Foveal crowding vs grade by font',
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
        annotation_logticks(sides = 'l') + 
        scale_shape_manual(values = c(4,19)) + 
        guides(shape = 'none') +
        labs(title = 'RSVP vs grade',
             y = 'RSVP reading speed (w/min)')
  }
  
  p3 = NULL
  if (nrow(foveal_acuity) > 0) {
   p3 = ggplot(data = foveal_acuity,
               aes(x = Grade, 
                   y = 10^(log_acuity), 
                   color =  Age)) +
     theme_classic() +
     scale_y_log10() +
     annotation_logticks(sides = 'l') + 
     guides(shape = 'none') +
     scale_shape_manual(values = c(4,19)) + 
     labs(title = 'Foveal acuity vs grade',
          y = ' Foveal acuity (deg)')
  }
  if (n_distinct(foveal_acuity$`Skilled reader?`) == 1) {
    p1 <- p1 + geom_point()
    p2 <- p2 + geom_point()
    p3 <- p3 + geom_point()
  } else {
    p1 <- p1 + 
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4,19))
    p2 <- p2 + 
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4,19))
    p3 <- p3 + 
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4,19))
  }
  return(list(
    p1,
    p2,
    p3
  ))
}

plot_rsvp_crowding <- function(allData) {
  # Helper function to compute correlation, slope, and plot
  create_plot <- function(data, condition, colorFactor) {
    rsvp <- allData$rsvp %>% mutate(participant = tolower(participant))
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      left_join(rsvp, by = "participant") %>% 
      distinct(participant, 
               block_avg_log_WPM, 
               log_crowding_distance_deg,
               age,
               Grade,
               `Skilled reader?`,
               ParticipantCode) %>%
      filter(!is.na(participant)) %>% 
      mutate(Age = format(age,nsmall=2),
             X = 10^(log_crowding_distance_deg),
             Y = 10^(block_avg_log_WPM),
             Grade = as.character(Grade))
      
    if (n_distinct(data_rsvp$`Skilled reader?`) > 1) {
      data_for_stat <- data_rsvp %>% filter(`Skilled reader?` != FALSE) %>% 
        select(block_avg_log_WPM,log_crowding_distance_deg, X, Y)
    } else {
      data_for_stat <- data_rsvp %>% 
        select(block_avg_log_WPM,log_crowding_distance_deg, X, Y)
    }

    data_for_stat <- data_for_stat[complete.cases(data_for_stat),]
    
    corr <- data_for_stat %>%
      summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
                                  method = "pearson"),
                N = n()) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_for_stat %>%
      do(fit = lm(Y ~ X, data = .)) %>%
      transmute(coef = map(fit, tidy)) %>%
      unnest(coef) %>%
      mutate(slope = round(estimate, 2)) %>%
      filter(term == 'X') %>%
      select(-term)
    
    # data_rsvp <- data_rsvp %>%
    #   mutate(label = paste0("italic('R=')~",
    #                         corr$correlation,
    #                         "~italic(', slope=')~", slope$slope))
    
    
    xMin <- min(data_rsvp$X, na.rm = T) /1.5
    xMax <- max(data_rsvp$X, na.rm = T) * 1.5
    yMin <- min(data_rsvp$Y, na.rm = T) / 1.5
    yMax <- max(data_rsvp$Y, na.rm = T) * 1.5
    
    # Generate dynamic breaks for the y-axis
    y_breaks <- scales::log_breaks()(c(yMin, yMax))
    p <- ggplot() + 
      theme_classic() +
      scale_y_log10(breaks = y_breaks,  # Dynamic breaks based on data range
                    limits = c(yMin, yMax), expand = c(0, 0)) +
      scale_x_log10(breaks = c(0.003, 0.01, 0.03, 0.1, 0.3, 1, 10, 100),
                    limits = c(xMin, xMax), expand = c(0, 0)) +
      geom_smooth(data = data_for_stat,
                  aes(x = X,
                      y = Y),
                  method = 'lm', se = FALSE) +
      annotation_logticks() +
      coord_cartesian(xlim = c(xMin, xMax), ylim = c(yMin, yMax)) +  # Set exact limits
      
      annotate(
        "text",
        x = xMin * 1.4,
        y = yMin * 1.3,
        label = paste0("N = ", corr$N, "\nR = ", corr$correlation, "\nslope = ", slope$slope),
        hjust = 0,        # Left-align text
        vjust = 0,        # Top-align for consistent stacking
        size = 4,
        color = "black"
      ) +
      
      
      #geom_text(data = data_rsvp,
       # aes(x = xMax * 0.8,  # Adjust annotation if needed
        #    y = yMax * 0.8,
         #   label = paste0("N = ", corr$N, "\n R = ",
          #                 corr$correlation,
           #                "\n slope = ", slope$slope))) +
      plt_theme +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC) == 1, 'none', 'top')) + 
      guides(color = guide_legend(title = colorFactor),
             shape = 'none') + 
      labs(x = paste(condition,'crowding (deg)'),
           y = 'RSVP reading (w/min)',
           title = paste('RSVP vs', tolower(condition), 'crowding by', tolower(colorFactor)))
    
    if (n_distinct(data_rsvp$`Skilled reader?`) == 1) {
      p <- p + geom_point(data = data_rsvp, 
                          aes(x = X,
                              y = Y,
                              group = ParticipantCode,
                              color = .data[[colorFactor]]))
    } else {
      p <- p + geom_point(data = data_rsvp, 
                          aes(x = X,
                              y = Y,
                              group = ParticipantCode,
                              color = .data[[colorFactor]],
                              shape = `Skilled reader?`)) + 
        scale_shape_manual(values = c(4,19))
    }
    
    return(p)
  }
  
  crowding <- allData$crowding %>% mutate(participant = tolower(participant))
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(allData$rsvp) == 0 | nrow(allData$crowding) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Peripheral Crowding by Age',
             xaxis = list(title = 'Peripheral crowding (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading (w/min)', type = 'log'))
    p2 <- plot_ly() %>% 
      layout(title = 'RSVP vs Foveal Crowding by Age',
             xaxis = list(title = 'Foveal Crowding (deg)', type = 'log'),
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

getCorrMatrix <- function(allData, pretest) {
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
    ),
    participant = tolower(participant)
    ) %>% 
    group_by(participant, type) %>% 
    summarize(log_acuity = mean(log_acuity)) %>% 
    pivot_wider(names_from=type, values_from = log_acuity)
 
  
  crowdingW <- crowding %>% mutate(type=ifelse(
    targetEccentricityXDeg == 0,
    'log foveal crowding',
    'log peripheral crowding'
  ),
  participant = tolower(participant)) %>% 
    group_by(participant,type) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg)) %>% 
    pivot_wider(names_from=type, values_from = log_crowding_distance_deg)
  
  reading <- allData$reading %>%
    mutate(participant = tolower(participant)) %>%
    group_by(participant, block_condition) %>% 
    summarize(log_WPM = mean(log_WPM)) %>% 
    rename('log reading' = 'log_WPM' ) %>% 
    select(participant, `log reading`)
  
  crowdingW <- crowdingW %>% 
    full_join(acuity, by = 'participant') %>% 
    mutate(participant = tolower(participant)) %>% 
    full_join(rsvp_speed %>% select(participant, block_avg_log_WPM) %>% mutate(participant = tolower(participant)), by = 'participant') %>% 
    full_join(reading, by = 'participant')
  
  if (nrow(pretest) > 0) {
    crowdingW <- crowdingW %>% 
      full_join(pretest_for_corr, by = 'participant')
  }
  
  crowdingW <- crowdingW %>% 
    rename('log rsvp' = 'block_avg_log_WPM') %>% 
    ungroup() %>% 
    select_if(is.numeric) %>% 
    select(where(~sum(!is.na(.)) > 0))
 
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
    labs(x = '', 
         y = '',
         title = ifelse(ncol(t) > 6, 'Big correlation table', 'Small correlation table')) + 
    ggpp::geom_text_npc(aes(npcx = 'left', npcy = 'top', label = paste0('N=', nrow(crowdingW[complete.cases(crowdingW),]))))
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