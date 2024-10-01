library(foreach)
library(dplyr)
library(stringr)
getwd()
basicExclude <-readxl::read_xlsx('Basic_Exclude.xlsx') %>%
  filter(`Exclude?` == TRUE) %>% 
  transmute(participant = paste0(tolower(str_sub(ID,1,4)),str_sub(ID,5,6)))
generate_rsvp_reading_crowding_fluency <- function(data_list, summary_list) {
  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]] %>% mutate(order = i)
  } %>% 
    filter(!participant %in% basicExclude$participant)
  
  quest <- all_summary %>% select(questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop)

  eccentricityDeg <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    t <- data_list[[i]] %>%
      distinct(participant, conditionName, targetEccentricityXDeg, targetEccentricityYDeg) %>%
      mutate(order = i)
  }
  
  eccentricityDeg <- eccentricityDeg %>% 
    filter(!is.na(targetEccentricityXDeg),
           !is.na(targetEccentricityYDeg))
  
  age <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(participant, age) %>% 
      distinct()
  }
  age <- distinct(age)
  ########################### CROWDING ############################
  crowding <- all_summary %>% 
    filter(thresholdParameter != "targetSizeDeg",
           thresholdParameter != 'size',
           targetKind == "letter",
           !grepl("practice",conditionName, ignore.case = T)) %>% 
    select(participant,
           block_condition,
           conditionName, 
           questMeanAtEndOfTrialsLoop, 
           font,
           experiment,
           order) %>%
    dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop)
  

  
  crowding <- crowding %>% 
    left_join(eccentricityDeg, by = c("participant", "conditionName", "order")) %>% 
    mutate(targetEccentricityXDeg = as.numeric(targetEccentricityXDeg), 
           targetEccentricityYDeg = as.numeric(targetEccentricityYDeg)) %>% 
    mutate(bouma_factor = 10^(log_crowding_distance_deg)/sqrt(targetEccentricityXDeg^2+targetEccentricityYDeg^2)) %>% 
    left_join(age, by = "participant")
  
  ########################### RSVP READING ############################
  
  rsvp_speed <- all_summary %>% 
    filter(targetKind == "rsvpReading",
           !grepl("practice",conditionName, ignore.case = T)) %>% 
    select(participant, block_condition,conditionName, questMeanAtEndOfTrialsLoop, font, targetKind, thresholdParameter) %>%
    dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
    mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP) 
  
  ################################ READING #######################################
  reading <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(block_condition, participant, conditionName, font, wordPerMin, 
             targetKind, thresholdParameter, readingNumberOfQuestions) %>% 
      mutate(log_WPM = log10(wordPerMin)) %>% 
      filter(targetKind == "reading" & font !="")
  } %>% 
    left_join(age, by = "participant")
  
  ################################ REPEAT LETTER #######################################
  repeatedLetters <- all_summary %>% 
    filter(thresholdParameter != "targetSizeDeg",
           thresholdParameter != 'size',
           targetKind == "repeatedLetters",
           !grepl("practice",conditionName, ignore.case = T)) %>% 
    select(participant,
           block_condition,
           conditionName, 
           questMeanAtEndOfTrialsLoop, 
           font,
           experiment,
           order) %>%
    dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop) %>% 
    left_join(eccentricityDeg, by = c("participant", "conditionName", "order")) %>% 
    left_join(age, by = "participant")
    
  
  #### get viewing distance and font size####
  
  viewingdistance <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(block_condition, participant, viewingDistanceDesiredCm) %>% 
      filter(!is.na(viewingDistanceDesiredCm)) %>% 
      distinct()
  }
  

  rsvp_speed <- rsvp_speed %>% 
    left_join(viewingdistance, by = c("block_condition", "participant")) %>% 
    left_join(age, by = "participant")
  
  nQs <- as.numeric(reading$readingNumberOfQuestions[1])
  
  ################################ READING RETENTION #######################################
  
  reading_accuracy <- tibble()
  if (!is.na(nQs) & nQs > 0) { 
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
    }
  if (nrow(reading_accuracy) > 0) {
    reading_accuracy <- reading_accuracy %>% 
      group_by(participant, block_condition) %>% 
      summarize(accuracy = mean(readWordIdentifiedBool))
    reading <- reading %>% left_join(reading_accuracy, by = c("participant", "block_condition") )
    reading$accuracy = factor(reading$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))
  }
  
  fluency <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    if("questionAndAnswerCorrectAnswer" %in% colnames(data_list[[i]])) {
      data_list[[i]] %>% filter(grepl("fluency", conditionName, fixed=TRUE) ) %>% 
        select(block, participant, conditionName, questionAndAnswerResponse, trials.thisN,
               questionAndAnswerNickname, questionAndAnswerQuestion, targetKind, questionAndAnswerCorrectAnswer)
    }
  }
  if(is.null(fluency)) fluency = tibble()
  if (nrow(fluency) > 0) {
    fluency <- fluency %>% 
      group_by(participant) %>% 
      summarize(accuracy = mean(questionAndAnswerResponse == questionAndAnswerCorrectAnswer))
  } else {
    fluency = tibble()
  }
  
  
  #### acuity  
  acuity <- all_summary %>% 
    filter((thresholdParameter == "targetSizeDeg" | thresholdParameter == 'size'),
           targetKind == "letter",
           !grepl("practice",conditionName, ignore.case = T)) %>% 
    left_join(age, by = "participant")
  
  print(paste('nrow of quest:', nrow(quest)))
  print(paste('nrow of reading:', nrow(reading)))
  print(paste('nrow of crowding:', nrow(crowding)))
  print(paste('nrow of rsvp_speed:', nrow(rsvp_speed)))
  print(paste('nrow of acuity:', nrow(acuity)))
  print(paste('nrow of repeatedLetters:', nrow(repeatedLetters)))
  
  return(list(reading = reading, 
              crowding = crowding,
              rsvp = rsvp_speed,
              fluency = fluency,
              acuity = acuity,
              repeatedLetters = repeatedLetters,
              quest = quest))
}

generate_threshold <- function(data_list, summary_list){
  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]]
  }
  crowding <- all_summary %>% 
    filter(thresholdParameter != "targetSizeDeg",
           thresholdParameter != 'size',
           targetKind == "letter", 
           !grepl("practice",conditionName),
           !grepl("Practice",conditionName)) %>% 
    select(participant, conditionName, questMeanAtEndOfTrialsLoop, font) %>%
    dplyr::rename(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop)
  
  ########################### RSVP READING ############################
  
  rsvp_speed <- all_summary %>% 
    filter(targetKind == "rsvpReading") %>% 
    select(block_condition, participant, conditionName, questMeanAtEndOfTrialsLoop, font, targetKind, thresholdParameter) %>%
    dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
    mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP) 
  
  crowding_vs_rsvp <- merge(crowding,rsvp_speed, by = c("participant", "font"))
  
  
  ################################ READING #######################################
  reading <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(block_condition, participant, conditionName, font, wordPerMin, targetKind, thresholdParameter) %>% 
      mutate(log_WPM = log10(wordPerMin)) %>% 
      filter(targetKind == "reading" & font !="")
  }
  
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
    group_by(participant, conditionName) %>%
    dplyr::summarize(
      pm = mean(questMeanAtEndOfTrialsLoop),
      sd = sd(questMeanAtEndOfTrialsLoop)) %>% 
    mutate(parameter = "threshold") %>% 
    ungroup() 
  
  
  threshold_summary <- threshold_all %>% 
    mutate(variance = sd^2) %>% 
    group_by(conditionName) %>% 
    dplyr::summarize(
      m = mean(pm, na.rm = T),
      `se across participants` = sd(pm)/sqrt(n()), 
      `sd across participants` = sd(pm),
      `sd across repetitions` = sqrt(mean(variance, na.rm = T)),
      N = n(),
      parameter = "threshold")
  
  wpm_all <- reading %>% 
    filter(!participant %in% reading_exceed_1500$participant) %>%
    filter(conditionName != "") %>% 
    group_by(conditionName, participant) %>%
    dplyr::summarize(pm = mean(wordPerMin, na.rm =T),
                     sd = sd(wordPerMin, na.rm =T)) %>% 
    filter(!is.na(pm)) %>% 
    mutate(parameter = "word per minute") %>% 
    ungroup()
  
  wpm_summary <- wpm_all %>% 
    mutate(variance = sd^2) %>% 
    group_by(conditionName) %>% 
    dplyr::summarize(
      m = mean(pm),
      `se across participants` = sd(pm)/sqrt(n()), 
      `sd across participants` = sd(pm),
      `sd across repetitions` = sqrt(mean(variance, na.rm = T)),
      N = n(),
      parameter = "word per minute")
  threshold_each <- rbind(threshold_all, wpm_all) %>% 
    mutate(m = round(pm,3),
           sd = round(sd,3)) %>% 
    select(participant, conditionName, m, sd, parameter)
  threshold <- rbind(threshold_summary, wpm_summary) %>% 
    mutate(m = round(m,3),
           `se across participants` = round(`se across participants`,3),
           `sd across participants` = round(`sd across participants`,3),
           `sd across repetitions` = round(`sd across repetitions`,3))
  return(list(reading_exceed_1500 %>% select(warning), threshold, threshold_each, all_summary))
}
