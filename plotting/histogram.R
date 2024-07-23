
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


get_crowding_hist <- function(crowding) {
  foveal <- crowding %>% filter(grepl('foveal', conditionName,ignore.case = T))
  peripheral <- crowding %>% filter(grepl('peripheral', conditionName,ignore.case = T))
  if (nrow(foveal) > 0) {
    p1 <- ggplot(foveal) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="white") +
      labs(x = 'log crowding distance (deg)',
           title ='histogram of foveal crowding distance')
  } else {
    p1 <- ggplot()
  }
 if (nrow(peripheral) > 0) { p2 <- ggplot(peripheral) + 
    geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="white") +
    labs(x = 'log crowding distance (deg)',
         title ='histogram of peripheral crowding distance')
 } else {
   p2 <- ggplot()
 }
  return(list(foveal=p1,peripheral=p2))
}

get_acuity_hist <- function(acuity) {
  if (nrow(acuity) > 0) {
    p <-  ggplot(acuity) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="white") +
      labs(x = 'log acuity (deg)',
           title ='histogram of acuity')
    return(p)
  } else{
    return(ggplot())
  }
}