
get_fluency_histogram <- function(fluency){
  if(nrow(fluency) == 0) return(ggplot() + ggtitle('fluency histogram'))
  fluency$accuracy <- factor(fluency$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))
  ggplot(fluency %>% count(accuracy, .drop = F), aes(x = accuracy, y = n)) +
    geom_bar(stat = "identity") + 
    xlab("fluency (proportion correct)") + 
    ylab("Count")

}

get_reading_retention_histogram <- function(reading) {
  if(nrow(reading) == 0) return(ggplot() + ggtitle('Reading retention histogram'))
  counts <- reading %>% count(accuracy, .drop=F)
  ggplot(counts) + geom_bar(aes(x = accuracy, y = n), stat = "identity") + 
    ylab("Count") +
    xlab("Reading retention (proportion correct)")
}


get_crowding_hist <- function(crowding, pretest) {
  if (nrow(pretest) > 0) {
    crowding <- crowding %>% left_join(pretest, by = 'participant')
  }
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` == TRUE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% 
      summarize(mean = round(mean(log_crowding_distance_deg),2),
                sd = round(sd(log_crowding_distance_deg),2))
    p1 <- ggplot(foveal) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
       aes( npcx = 'left',
            npcy = 'top',
            label = paste0('mean=',stats1$mean,', sd=', stats1$sd))
      ) + 
      labs(x = 'Log crowding distance (deg)',
           title ='Histogram of foveal crowding distance')
  } else {
    p1 <- ggplot()
  }
 if (nrow(peripheral) > 0) { 
   if ('Skilled reader?' %in% names(peripheral)) {
     stats2 <- peripheral %>% filter(`Skilled reader?` == TRUE)
   } else {
     stats2 <- peripheral
   }
   stats2 <- stats2 %>% 
     summarize(mean = round(mean(log_crowding_distance_deg),2),
               sd = round(sd(log_crowding_distance_deg),2))
   p2 <- ggplot(peripheral) + 
    geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ggpp::geom_text_npc(
     aes( npcx = 'left',
          npcy = 'top',
          label = paste0('mean=',stats2$mean,', sd=', stats2$sd))
   ) + 
    labs(x = 'Log crowding distance (deg)',
         title ='Histogram of peripheral crowding distance')
 } else {
   p2 <- ggplot()
 }
  return(list(foveal=p1,peripheral=p2))
}

get_acuity_hist <- function(acuity, pretest) {
  if (nrow(pretest) > 0) {
    acuity <- acuity %>% left_join(pretest, by = 'participant')
  }
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
                sd = round(sd(questMeanAtEndOfTrialsLoop),2))
    
    p1 <-  ggplot(foveal) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,', sd=', stats1$sd))
      ) +
      labs(x = 'Log acuity (deg)',
           title ='Histogram of foveal acuity')
  } else {
    p1 <-  ggplot() +
      labs(x = 'Log acuity (deg)',
           title ='Histogram of foveal acuity')
  }
  
  if (nrow(peripheral) > 0) {
    if ('Skilled reader?' %in% names(peripheral)) {
      stats2 <- peripheral %>% filter(`Skilled reader?` == TRUE)
    } else {
      stats2 <- peripheral
    }
    stats2 <- stats2 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2))
    
    p2 <-  ggplot(peripheral) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'left',
             npcy = 'top',
             label = paste0('mean=',stats2$mean,', sd=', stats2$sd))
      ) +
      labs(x = 'Log acuity (deg)',
           title ='Histogram of peripheral acuity')
  } else {
    p2 <-  ggplot() +
      labs(x = 'Log acuity (deg)',
           title ='Histogram of peripheral acuity')
  }
  return(list(p1,p2))
}