library(ggcorrplot) 
library(plotly)
plot_rsvp_crowding_acuity <- function(allData,df,pretest) {
  print('inside plot_rsvp_crowding_acuity')
  if (is.null(allData) | nrow(pretest) < 1) {
  # if (T) {
    return(list(
      ggplot(),
      ggplot(),
      ggplot(),
      ggplot()
    ))
  }

  pretest_for_corr <- pretest
  pretest_for_corr <- lapply(pretest_for_corr, as.numeric)
  pretest_for_corr <- tibble(data.frame(pretest_for_corr))
  colnames(pretest_for_corr) <- colnames(pretest)
  
  pretest_for_corr <- pretest_for_corr %>%   
    select_if(~sum(!is.na(.)) > 0)
  pretest_for_corr$participant <- pretest$participant
  
  rsvp_speed <- allData$rsvp
  crowding <- allData$crowding
  acuity <- allData$acuity %>% rename('log_acuity'='questMeanAtEndOfTrialsLoop')
  
  crowdingW <- crowding %>% mutate(type=ifelse(
    targetEccentricityXDeg == 0,
    'foveal',
    'peripheral'
  )) %>% 
    select(participant, log_crowding_distance_deg,type) %>% 
    pivot_wider(names_from=type, values_from = log_crowding_distance_deg)
  
  crowdingW <- crowdingW %>% 
    left_join(acuity %>% select(participant, log_acuity),by = 'participant') %>% 
    left_join(rsvp_speed %>% select(participant, block_avg_log_WPM), by = 'participant')
  
  #   RAVEN_Perc,
  # `RAVEN - tot`, 
  crowdingW <- crowdingW %>% 
    select(log_acuity, foveal, peripheral, block_avg_log_WPM, participant) %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest_for_corr, by = 'participant') %>% 
    rename('log rsvp' = 'block_avg_log_WPM') %>% 
    select_if(is.numeric)
  c <- colnames(crowdingW)
  
  crowdingW
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
    
 
  # t <- pretest %>% left_join(df, by = 'participant')
    # rename('PartcipantCode in experiment csv' =  'PartcipantCode.x',
    #        'PartcipantCode in pretest xlsx' =  'PartcipantCode.x')
  # write.csv(t, '~/Downloads/pretestVsExperimentCSV.csv')
  rsvp_speed <- rsvp_speed %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')
  crowding <- crowding %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')
  acuity <- acuity %>% 
    left_join(df, by = 'participant') %>% 
    left_join(pretest, by = 'participant')
  
  

  p1 = ggplot(data = crowding) +
    geom_point(aes(x = Grade, y = 10^(log_crowding_distance_deg))) +
    facet_wrap(font~.) + 
    theme_classic() +
    scale_y_log10() + 
    annotation_logticks(sides = 'l') + 
    labs(title = 'Crowding vs Grade',
         y = 'crowding (deg)')
  
  p2 = ggplot(data = rsvp_speed) +
    geom_point(aes(x = Grade, y = 10^(block_avg_log_WPM))) +
    theme_classic() + 
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    labs(title = 'Rsvp vs Grade',
         y = 'rsvp reading speed (word/min)')
  
  p3 = ggplot(data = acuity) +
    geom_point(aes(x = Grade, y = 10^(log_acuity))) +
    theme_classic() +
    scale_y_log10() +
    annotation_logticks(sides = 'l') + 
    labs(title = 'Acuity vs Grade',
         y = 'acuity (deg)')

  return(list(
    p1,
    p2,
    p3,
    corplot
  ))
}

plot_rsvp_crowding <- function(allData, df, pretest) {
  # Helper function to compute correlation, slope, and plot
  create_plot <- function(data, condition, colorFactor) {
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      left_join(allData$rsvp, by = "participant")

    corr <- data_rsvp %>%
      summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
                                  method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))

    slope <- data_rsvp %>%
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
                            "~italic(', slope=')~", slope$slope))
    
    if (nrow(pretest) > 0) {
      if (colorFactor =='Age') {
        pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)))
      } else {
        pretest <- pretest %>% mutate(factorC = as.character(Grade))
      }
      
      data_rsvp <- data_rsvp %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(4,19)
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      data_rsvp <- data_rsvp %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(19)
    }
   
    p <- ggplot(data = data_rsvp, 
                aes(x = X,
                    y = Y
                    )) +
      geom_point(aes(color = factorC, shape =`Skilled reader?`)) +
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$log_crowding_distance_deg), 100)) +
      geom_smooth(method = 'lm', se = FALSE) +
      scale_shape_manual(values = pointshapes) + 
      annotation_logticks() +
      coord_fixed(ratio = 1) +
      ggpp::geom_text_npc(
        aes(npcx = "left",
            npcy = "top",
            label = label),
        parse = TRUE) +
      theme(legend.position = ifelse(n_distinct(data_rsvp$`Skilled reader?`)==1, 'none','top')) + 
      labs(x = 'crowding distance (deg)',
           y = 'rsvp reading (word/min)',
           title = paste('rsvp vs', condition, 'crowding by', colorFactor))

    return(p)
  }

  # Extract and filter data
  crowding <- allData$crowding
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)

  if (nrow(allData$rsvp) == 0) {
    p1 <- ggplot() +
      labs(x = 'crowding distance (deg)',
           y = 'rsvp reading (word/min)',
           title = 'rsvp vs peripheral crowding')
    p2 <- ggplot() +
      labs(x = 'crowding distance degree',
           y = 'rsvp reading (word/min)',
           title = 'rsvp vs foveal crowding')
    return(list(p1, p2,p1,p2))
  }

  # Create plots for peripheral and foveal data
  p1 <- create_plot(peripheral, "peripheral", 'Age')
  p2 <- create_plot(foveal, "foveal", 'Age')
  p3 <- create_plot(peripheral, "peripheral", 'Grade')
  p4 <- create_plot(foveal, "foveal", 'Grade')

  return(list(p1, p2, p3, p4))
}


plot_rsvp_crowding_plotly <- function(allData, df, pretest) {
  # Helper function to compute correlation, slope, and plot
  create_plot <- function(data, condition, colorFactor) {
    data_rsvp <- data %>%
      select(participant, log_crowding_distance_deg) %>%
      left_join(allData$rsvp, by = "participant")
    
    corr <- data_rsvp %>%
      summarize(correlation = cor(block_avg_log_WPM, log_crowding_distance_deg,
                                  method = "pearson")) %>%
      mutate(correlation = round(correlation, 2))
    
    slope <- data_rsvp %>%
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
                            "~italic(', slope=')~", slope$slope))
    
    if (nrow(pretest) > 0) {
      if (colorFactor =='Age') {
        pretest <- pretest %>% mutate(factorC = as.character(format(round(Age,1), nsmall = 1)))
      } else {
        pretest <- pretest %>% mutate(factorC = as.character(Grade))
      }
      
      data_rsvp <- data_rsvp %>% 
        left_join(pretest, by = 'participant') %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      pointshapes <-  c(4,19)
    } else {
      data_rsvp <- data_rsvp %>% mutate(factorC = 'black', `Skilled reader?` = 'TRUE', ParticipantCode = participant)
      data_rsvp <- data_rsvp %>% 
        mutate(X = 10^(log_crowding_distance_deg),
               Y = 10^(block_avg_log_WPM))
      c <-  c(19)
    }
    
    p <- ggplot(data = data_rsvp, 
                aes(x = X,
                    y = Y
                )) +
      geom_point(aes(color = factorC, shape =`Skilled reader?`, group = ParticipantCode)) +
      theme_classic() +
      scale_y_log10(breaks = c(1, 10, 100, 1000)) +
      scale_x_log10(breaks = c(0.3, 1, 10, 100),
                    limits = c(10^min(data_rsvp$log_crowding_distance_deg), 100)) +
      geom_smooth(method = 'lm', se = FALSE) +
      scale_shape_manual(values = pointshapes) + 
      annotation_logticks() +
      coord_fixed(ratio = 1) +
      ggpp::geom_text_npc(
        aes(npcx = "left",
            npcy = "top",
            label = label),
        parse = TRUE) +
      theme(legend.position = ifelse(n_distinct(data_rsvp$factorC)==1, 'none','top')) + 
      labs(x = 'crowding distance (deg)',
           y = 'rsvp reading (word/min)',
           title = paste('rsvp vs', condition, 'crowding by', colorFactor))
    pp <- ggplotly(p)
    
    return(pp)
  }
  
  # Extract and filter data
  crowding <- allData$crowding
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0,)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  
  if (nrow(allData$rsvp) == 0) {
    p1 <- plot_ly() %>% 
      layout(title = 'RSVP vs Peripheral Crowding by Age',
             xaxis = list(title = 'Crowding Distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading (word/min)', type = 'log'))
    p2 <- plot_ly() %>% 
      layout(title = 'RSVP vs Foveal Crowding by Age',
             xaxis = list(title = 'Crowding Distance (deg)', type = 'log'),
             yaxis = list(title = 'RSVP Reading (word/min)', type = 'log'))
    return(list(p1, p2, p1, p2))
  }
  
  # Create plots for peripheral and foveal data
  p1 <- create_plot(peripheral, "Peripheral",'Age')
  p2 <- create_plot(foveal, "Foveal",'Age')
  p3 <- create_plot(peripheral, "Peripheral",'Grade')
  p4 <- create_plot(foveal, "Foveal",'Grade')
  
  return(list(p1, p2, p3,p4))
}


