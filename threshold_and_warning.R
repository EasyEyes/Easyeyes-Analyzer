library(foreach)
library(dplyr)
generate_rsvp_reading_crowding_fluency <- function(data_list, summary_list) {
  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]]
  }
  crowding <- all_summary %>% 
    filter(thresholdParameter != "size", targetKind == "letter", !grepl("practice",conditionName)) %>% 
    select(participant, conditionName, questMeanAtEndOfTrialsLoop, font) %>%
    dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop)
  ########################### RSVP READING ############################
  
  rsvp_speed <- all_summary %>% 
    filter(targetKind == "rsvpReading") %>% 
    select(block, participant, conditionName, questMeanAtEndOfTrialsLoop, font, targetKind, thresholdParameter) %>%
    dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
    mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP) 
  ################################ READING #######################################
  reading <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(block_condition, participant, conditionName, font, wordPerMin, 
             targetKind, thresholdParameter, readingNumberOfQuestions) %>% 
      mutate(log_WPM = log10(wordPerMin)) %>% 
      filter(targetKind == "reading" & font !="")
  }
  
  fluency <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    if("questionAndAnswerCorrectAnswer" %in% colnames(data_list[[i]])) {
      data_list[[i]] %>% filter(grepl("fluency", conditionName, fixed=TRUE) ) %>% 
        select(block, participant, conditionName, questionAndAnswerResponse, trials.thisN,
               questionAndAnswerNickname, questionAndAnswerQuestion, targetKind, questionAndAnswerCorrectAnswer)
    }
  }
  fluency <- 
    fluency %>% 
    group_by(participant) %>% 
    summarize(accuracy = mean(questionAndAnswerResponse == questionAndAnswerCorrectAnswer))
  
  return(list(reading, crowding, rsvp_speed, fluency))
}
generate_threshold <- function(data_list, summary_list){
  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]]
  }
  crowding <- all_summary %>% 
    filter(thresholdParameter != "size", targetKind == "letter", !grepl("practice",conditionName)) %>% 
    select(participant, conditionName, questMeanAtEndOfTrialsLoop, font) %>%
    dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop)
  ########################### RSVP READING ############################
  
  rsvp_speed <- all_summary %>% 
    filter(targetKind == "rsvpReading") %>% 
    select(block, participant, conditionName, questMeanAtEndOfTrialsLoop, font, targetKind, thresholdParameter) %>%
    dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
    mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP) 
  ################################ READING #######################################
  reading <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(block_condition, participant, conditionName, font, wordPerMin, targetKind, thresholdParameter) %>% 
      mutate(log_WPM = log10(wordPerMin)) %>% 
      filter(targetKind == "reading" & font !="")
  }
  crowding_vs_rsvp <- merge(crowding,rsvp_speed, by = c("participant", "font"))
  
  reading_each <- reading %>% 
    group_by(font, participant, block_condition, thresholdParameter) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)), .groups = "keep") %>% 
    ungroup()
  
  reading_exceed_1500 <- reading_each %>% 
    filter(avg_wordPerMin > 1500) %>% 
    mutate(warning =  paste("Participant:",
                         participant,
                         "reading speeds removed due to excessive max speed",
                         round(avg_wordPerMin,2),
                         "> 1500 word/min",
                         sep = " "))
  for (i in 1 : nrow(reading_exceed_1500)) {
    warning(paste("Participant:",
                  reading_exceed_1500$participant[i],
                  "reading speeds removed due to excessive max speed",
                  round(reading_exceed_1500$avg_wordPerMin[i],2),
                  "> 1500 word/min",
                  sep = " "))
  }
  reading_valid <- reading_each %>% 
    filter(!participant %in% reading_exceed_1500$participant) %>% 
    mutate(targetKind = "reading")
  
  threshold_all <- all_summary %>%
    filter(!grepl("practice",conditionName)) %>% 
    group_by(conditionName, thresholdParameter) %>%
    dplyr::summarize(
      m = mean(questMeanAtEndOfTrialsLoop),
      se = sd(questMeanAtEndOfTrialsLoop)/sqrt(n()), 
      sd = sd (questMeanAtEndOfTrialsLoop),
      N = n(),
      parameter = "threshold")
  
  practice_all <- all_summary %>%
    filter(grepl("practice",conditionName)) %>% 
    group_by(conditionName, participant, thresholdParameter) %>%
    dplyr::summarize(pm = mean(questMeanAtEndOfTrialsLoop)) %>% 
    ungroup() %>% 
    group_by(conditionName) %>% 
    dplyr::summarize(
      m = mean(pm),
      se = sd(pm)/sqrt(n()), 
      sd = sd(pm),
      N = n(),
      parameter = "threshold")
  
  wpm_all <- reading %>% 
    filter(!participant %in% reading_exceed_1500$participant) %>%
    filter(conditionName != "") %>% 
    group_by(conditionName, participant, thresholdParameter) %>%
    dplyr::summarize(
      pm = mean(wordPerMin, na.rm =T), .group = "keep") %>% 
    filter(!is.na(pm)) %>% 
    ungroup() %>% 
    group_by(conditionName, thresholdParameter) %>% 
    dplyr::summarize(
      m = mean(pm),
      se = sd(pm)/sqrt(n()), 
      sd = sd(pm),
      N = n(),
      parameter = "word per minute")
  threshold <- rbind(threshold_all, practice_all, wpm_all) %>% mutate(m = round(m,3),
                                                                      sd = round(sd,3),
                                                                      se = round(se,3))
  return(list(reading_exceed_1500 %>% select(warning), threshold))
}
