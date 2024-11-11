
get_fluency_histogram <- function(fluency){
  
  if(nrow(fluency) == 0) {
    return(NULL)
  }
  
  print('inside fluency')
  fluency$accuracy <- factor(fluency$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))

  ggplot(data = fluency %>% count(accuracy, .drop = F), aes(x = accuracy, y = n)) +
    geom_bar(stat = "identity") + 
    labs(x = "fluency (proportion correct)",
         y = "Count",
         title = "English fluency histogram")

}

get_reading_retention_histogram <- function(reading) {
  if(nrow(reading) == 0) {
    return(NULL)
  }
  counts <- reading %>% count(accuracy, .drop=F)
  ggplot(data = counts) + geom_bar(aes(x = accuracy, y = n), stat = "identity") + 
    labs(x = "Reading retention (proportion correct)",
         y = "Count",
         title = "Reading retention histogram")
}


get_crowding_hist <- function(crowding) {

  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% summarize(mean = round(mean(log_crowding_distance_deg),2), 
                                   sd = round(sd(log_crowding_distance_deg),2),
                                   N = n())
    p1 <- ggplot(foveal) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log foveal crowding (deg)',
           y = 'Count',
           title ='Histogram of foveal crowding distance')
  } else {
    p1 <- NULL
  }
 if (nrow(peripheral) > 0) { 
   if ('Skilled reader?' %in% names(peripheral)) {
     stats2 <- peripheral %>% filter(`Skilled reader?` != FALSE)
   } else {
     stats2 <- peripheral
   }
   stats2 <- stats2 %>% 
     summarize(mean = round(mean(log_crowding_distance_deg),2),
               sd = round(sd(log_crowding_distance_deg),2),
               N = n())

   p2 <- ggplot(peripheral) + 
    geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ggpp::geom_text_npc(
     aes( npcx = 'left',
          npcy = 'top',
          label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
   ) + 
    labs(x = 'Log peripheral crowding (deg)',
         y = 'Count',
         title ='Histogram of peripheral crowding distance')
 } else {
   p2 <- NULL
 }
  return(list(foveal=p1,peripheral=p2))
}

get_acuity_hist <- function(acuity) {

  foveal <- acuity %>% 
    filter(targetEccentricityXDeg == 0) %>%
    distinct(participant, questMeanAtEndOfTrialsLoop)

  peripheral <- acuity %>%
    filter(targetEccentricityXDeg != 0) %>%
    distinct(participant, questMeanAtEndOfTrialsLoop)
  
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` == TRUE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2),
                N = n())
    
    p1 <-  ggplot(foveal) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           title ='Histogram of foveal acuity')
  } else {
    p1 <-  NULL
  }
  
  if (nrow(peripheral) > 0) {
    if ('Skilled reader?' %in% names(peripheral)) {
      stats2 <- peripheral %>% filter(`Skilled reader?` == TRUE)
    } else {
      stats2 <- peripheral
    }
    stats2 <- stats2 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2),
                N = n())
    
    p2 <-  ggplot(peripheral) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           title ='Histogram of peripheral acuity')
  } else {
    p2 <- NULL
  }
  return(list(p1,p2))
}

get_reading_hist <- function(reading) {
  if (nrow(reading) > 0) {
    
    if (reading$targetKind[1] == 'rsvpReading') {
      reading <- reading %>% mutate(log_WPM = block_avg_log_WPM)
    }
    
    if ('Skilled reader?' %in% names(reading)) {
      stats1 <- reading %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- reading
    }
    
    stats1 <- stats1 %>% summarize(mean = round(mean(log_WPM),2), 
                                   sd = round(sd(log_WPM),2),
                                   N = n())
    p1 <- ggplot(reading) + 
      geom_histogram(aes(x = log_WPM),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) 
    if (reading$targetKind[1] == 'rsvpReading') {
      p1 <- p1 + 
        labs(x = 'Log RSVP reading speed (w/min)',
             y = 'Count',
             title ='Histogram of RSVP reading speed')
    } else {
      p1 <- p1 + 
        labs(x = 'Log reading speed (w/min)',
             y = 'Count',
             title ='Histogram of reading speed')
    }
  } else {
    p1 <- NULL
  }
  return(p1)
}

get_repeatedLetter_hist <- function(repeated) {
  if (nrow(repeated) > 0) {
    if ('Skilled reader?' %in% names(repeated)) {
      stats1 <- repeated %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- repeated
    }

    stats1 <- stats1 %>% summarize(mean = round(mean(log_crowding_distance_deg),2), 
                                   sd = round(sd(log_crowding_distance_deg),2),
                                   N = n())
    p1 <- ggplot(repeated) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log repeated-letter crowding (deg)',
           y = 'Count',
           title ='Histogram of repeated-letter crowding')
  } else {
    p1 <- NULL
  }
  return(p1)
}