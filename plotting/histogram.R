
get_fluency_histogram <- function(fluency){
  fluency$accuracy <- factor(fluency$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))
  ggplot(fluency %>% count(accuracy, .drop = F), aes(x = accuracy, y = n)) +geom_bar(stat = "identity") + xlab("fluency") + ylab("count")

}

get_reading_retention_histogram <- function(data_list, reading) {
  nQs <- as.numeric(reading$readingNumberOfQuestions[1])
  reading_accuracy <- tibble()
  for (i in 1:length(data_list)) {
    t <- data_list[[i]]
    if ("readWordIdentifiedBool" %in% colnames(t)) {
      readingQuestions <- t %>% 
        filter(!is.na(readWordIdentifiedBool)) %>% 
        select(participant,readWordIdentifiedBool)
      if(nrow(readingQuestions) > 0) {
        n_blocks = nrow(readingQuestions)/nQs
        r <- reading %>% filter(participant == readingQuestions$participant[1])
        blocks <- unique(r$block_condition)[1:n_blocks]
        readingQuestions <- cbind(readingQuestions,tibble(block_condition = rep(blocks,each = nQs)))
        reading_accuracy <- rbind(reading_accuracy,readingQuestions)
      }
    }
  }
  reading_accuracy <- reading_accuracy %>% 
    group_by(participant, block_condition) %>% 
    summarize(accuracy = mean(readWordIdentifiedBool))
  reading <- reading %>% left_join(reading_accuracy, by = c("participant", "block_condition") )
  
  reading$accuracy = factor(reading$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))
  counts <- reading %>% count(accuracy, .drop=F)
  ggplot(counts) + geom_bar(aes(x = accuracy, y = n), stat = "identity") + ylab("count")
}