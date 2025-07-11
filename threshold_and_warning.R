library(foreach)
library(dplyr)
library(stringr)

englishChild <- readxl::read_xlsx('Basic_Exclude.xlsx') %>%
  mutate(participant = tolower(ID))

generate_rsvp_reading_crowding_fluency <- 

  function(data_list, summary_list, pretest, stairs, filterInput, skillFilter,minNQuestTrials, 
           minWrongTrials, maxQuestSD, conditionNameInput, maxReadingSpeed) {

  print('inside threshold warning')
  print(paste0('length of data list: ', length(data_list)))
  print(paste0('length of summary list: ', length(summary_list)))
  if (is.null(data_list)) {
    return(list())
  }
  if (length(data_list) == 0) {
    return(list())
  }

  NQuestTrials <- stairs %>%
    group_by(participant, staircaseName, thresholdParameter) %>%
    summarize(questTrials = sum(trialGivenToQuest,na.rm = T)) %>% 
    ungroup() %>% 
    filter((thresholdParameter != 'spacingDeg'  & thresholdParameter != 'spacing') | questTrials >= minNQuestTrials) %>% 
    mutate(block_condition = as.character(staircaseName)) %>% 
    distinct(participant, block_condition, questTrials)

  wrongTrials <- stairs %>%
    group_by(participant, staircaseName) %>%
    summarize(NWrongTrial = sum((!`key_resp.corr`) & trialGivenToQuest, na.rm = T),
              NCorrectTrial = sum((`key_resp.corr`) & trialGivenToQuest, na.rm = T),
              frac = sum((!`key_resp.corr`) & trialGivenToQuest, na.rm = T) / sum((`key_resp.corr`) & trialGivenToQuest, na.rm = T)) %>% 
    ungroup() %>% 
    filter(NWrongTrial >= minWrongTrials) %>% 
    mutate(block_condition = as.character(staircaseName)) %>% 
    distinct(participant, block_condition, NWrongTrial,NCorrectTrial,frac)
  
  if (nrow(pretest) > 0) {
    if (!'Grade' %in% names(pretest)) {
      pretest$Grade = -1
    }
    if (!'Skilled reader?' %in% names(pretest)) {
      pretest$`Skilled reader?` = 'unkown'
    }

    pretest <- pretest %>%
      mutate(lowerCaseParticipant = tolower(participant))
    if ('Exclude?' %in% names(pretest)) {
      basicExclude <- pretest %>% 
        filter(tolower(`Exclude?`) == 'true')
    } else {
      basicExclude <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude?' %in% names(pretest)) {
      basicExclude <- pretest %>% 
        filter(tolower(`Exclude?`) == 'true')
    } else {
      basicExclude <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude-acuity' %in% names(pretest)) {
      excludeAcuity <- pretest %>% 
        filter(tolower(`Exclude-acuity`) == 'true')
    } else {
      excludeAcuity <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude-crowding' %in% names(pretest)) {
      excludeCrowding <- pretest %>% 
        filter(tolower(`Exclude-crowding`) == 'true')
    } else {
      excludeCrowding <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude-peripheral' %in% names(pretest)) {
      excludePeripheral <- pretest %>% 
        filter(tolower(`Exclude-peripheral`) == 'true')
    } else {
      excludePeripheral <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude-repeated' %in% names(pretest)) {
      excludeRepeated <- pretest %>% 
        filter(tolower(`Exclude-repeated`) == 'true')
    } else {
      excludeRepeated <- tibble(lowerCaseParticipant = '')
    }
    
    if ('Exclude-ordinary' %in% names(pretest)) {
      excludeOrdinary <- pretest %>% 
        filter(tolower(`Exclude-ordinary`) == 'true')
    } else {
      excludeOrdinary <- tibble(lowerCaseParticipant = '')
    }
  } else {
    basicExclude <- tibble(lowerCaseParticipant = '')
  }

  reading <- tibble()
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>% 
      select(block_condition, participant, conditionName, font, readingPageWords, readingPageDurationOnsetToOffsetSec,
             targetKind, thresholdParameter, readingNumberOfQuestions) %>% 
      group_by(participant, block_condition, conditionName, font) %>%
      mutate(trial = row_number()) %>% 
      ungroup() %>% 
      mutate(wordPerMin = ifelse(trial < 3 & tolower(participant) %in% englishChild$participant,
             9.5 / as.numeric(readingPageDurationOnsetToOffsetSec) * 60, 
             as.numeric(readingPageWords)/ as.numeric(readingPageDurationOnsetToOffsetSec) * 60)) %>% 
      mutate(log_WPM = log10(wordPerMin)) %>% 
      filter(targetKind == "reading" & font !="")
    
    reading <- rbind(reading, t)
  }
  # For italian data, reading OMT_words read as reading speed
  
  if (nrow(reading) == 0 & 'OMT_words read' %in% names(pretest)) {
    reading <- pretest %>% 
      select(participant, `OMT_words read`) %>% 
      mutate(`OMT_words read` = as.numeric(`OMT_words read`)) %>% 
      filter(!is.na(`OMT_words read`)) %>% 
      mutate(block_condition = '',
             conditionName = '',
             font = '',
             targetKind = 'reading',
             thresholdParameter = '',
             readingNumberOfQuestions = NA,
             trial = 1,
             log_WPM = log10(`OMT_words read`)) %>% 
      rename(wordPerMin = `OMT_words read`)
  }
  
  reading <- reading %>% filter(wordPerMin <= maxReadingSpeed)
  
  if (nrow(pretest) > 0) {
    reading <- reading %>% 
      mutate(lowerCaseParticipant = tolower(participant)) %>% 
      left_join(select(pretest, Grade, `Skilled reader?`, lowerCaseParticipant), by = 'lowerCaseParticipant') %>% 
      mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
      filter(!lowerCaseParticipant %in% excludeOrdinary$lowerCaseParticipant) %>% 
      select(-lowerCaseParticipant) %>% 
      mutate(`Skilled reader?` = ifelse(is.na(`Skilled reader?`), 'unkown', `Skilled reader?`))
    if (!'ParticipantCode' %in% names(reading)) {
      reading$ParticipantCode = reading$participant
    }
  } else {
    reading$ParticipantCode = reading$participant
    reading$Grade = -1
    reading$`Skilled reader?` = 'unkown'
  }
  
  # combine all thresholds

  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]] %>% mutate(order = i)
  } %>% 
    filter(!tolower(participant) %in% basicExclude$lowerCaseParticipant)

  # apply questSD filter
  all_summary <- all_summary %>% 
    left_join(NQuestTrials, by = c('participant', 'block_condition')) %>% 
    filter(questSDAtEndOfTrialsLoop <= maxQuestSD) %>% 
    inner_join(wrongTrials, by = c('participant', 'block_condition'))
  
  
  if (nrow(pretest) > 0) {
    if ('Include' %in% names(pretest)) {
      all_summary <- all_summary %>% left_join(pretest %>% select(participant, Include), by = 'participant') %>% 
        filter(Include == 'yes')
    }

    all_summary <- all_summary %>% 
      mutate(lowerCaseParticipant = tolower(participant)) %>% 
      left_join(select(pretest, Grade, `Skilled reader?`, lowerCaseParticipant), by = 'lowerCaseParticipant') %>% 
      select(-lowerCaseParticipant) %>% 
      mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
      mutate(`Skilled reader?` = ifelse(is.na(`Skilled reader?`), 'unkown', `Skilled reader?`))
    
   
    if (skillFilter == "skilled") {
      all_summary <- all_summary %>% 
        filter(`Skilled reader?` == "TRUE")
    } else if (skillFilter == "unskilled") {
      all_summary <- all_summary %>% 
        filter(`Skilled reader?` == "FALSE")
    }
    print('done skill filter')
    
    if (!'ParticipantCode' %in% names(all_summary)) {
      all_summary$ParticipantCode = all_summary$participant
    }
  } else {
      all_summary <- all_summary %>% 
        mutate(ParticipantCode = participant,
               Grade = -1,
               `Skilled reader?` = 'unkown')
  }

  
  

  
  print('done combine threshlod')
  
  #### calculate cut-off start here ####
  
  reading_avg <- reading %>% group_by(participant) %>% summarize(avg = mean(wordPerMin))

  threshold <- ifelse(nrow(reading_avg) != 0, quantile(reading_avg$avg, 0.25, na.rm = T), 0)
  slowest = tibble()
  print(paste0('threshold:', threshold))
  
  if (!is.na(threshold) & threshold != 0) {
    slowest = reading_avg %>% filter(avg <= threshold) %>% mutate(participant = tolower(participant)) %>% distinct(participant)
  }

  
  print('done calculate filter cut-off threshold')
  print(paste('pretest:', nrow(pretest)))
  
  if (nrow(pretest) > 0 & 'OMT_words read' %in% names(pretest)) {
    pretest <- pretest %>% mutate(wordPerMin = as.numeric(`OMT_words read`))
    threshold <- quantile(pretest$wordPerMin, 0.25, na.rm = T)
    print(paste('threshold:', threshold))
    slowest = pretest %>% filter(wordPerMin <= threshold)
    print('done pretest')
  }
  
  if (filterInput == 'slowest' & nrow(slowest) > 0) {
    print('before filtering')
    print(paste('unique pavloviaSessionID in reading', n_distinct(reading$participant)))
    print(paste('unique pavloviaSessionID in threshold', n_distinct(all_summary$participant)))
    print(paste('number of rows in reading', nrow(reading)))
    print(paste('number of rows all_summary', nrow(all_summary)))
    reading <- reading %>% filter(tolower(participant) %in% tolower(slowest$participant))
    all_summary <- all_summary %>%  filter(tolower(participant) %in% tolower(slowest$participant))
    print('after filtering')
    print(paste('unique pavloviaSessionID in reading', n_distinct(reading$participant)))
    print(paste('unique pavloviaSessionID in threshold', n_distinct(all_summary$participant)))
    print(paste('number of rows in reading', nrow(reading)))
    print(paste('number of rows all_summary', nrow(all_summary)))
  } 
  if (filterInput == 'fastest' & nrow(slowest) > 0) {
    reading <- reading%>% filter(!tolower(participant) %in% tolower(slowest$participant))
    all_summary <- all_summary%>% filter(!tolower(participant) %in% tolower(slowest$participant))
  }
  print('done filter input')
  
  

  eccentricityDeg <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    t <- data_list[[i]] %>%
      distinct(participant, conditionName, targetEccentricityXDeg, targetEccentricityYDeg)
  }

  eccentricityDeg <- eccentricityDeg %>% 
    filter(!is.na(targetEccentricityXDeg),
           !is.na(targetEccentricityYDeg)) %>% 
    mutate(targetEccentricityXDeg = as.numeric(targetEccentricityXDeg),
           targetEccentricityYDeg = as.numeric(targetEccentricityYDeg))
  
  all_summary <- all_summary %>%
    mutate(questType = case_when(
      thresholdParameter != "targetSizeDeg" &
      thresholdParameter != 'size' &
      targetKind == "letter" &
      !grepl("practice",conditionName, ignore.case = T) ~ 'crowding',
      targetKind == "rsvpReading" &
      !grepl("practice",conditionName, ignore.case = T) ~ 'RSVP reading',
      thresholdParameter != "targetSizeDeg" &
      thresholdParameter != 'size' &
      targetKind == "repeatedLetters" &
      !grepl("practice",conditionName, ignore.case = T) ~ 'Repeated letters',
      (thresholdParameter == "targetSizeDeg" | thresholdParameter == 'size') &
      targetKind == "letter" &
      !grepl("practice",conditionName, ignore.case = T) ~ 'acuity',
      grepl("practice",conditionName, ignore.case = T) ~ 'practice',
      .default = 'unknown'
    )) %>% 
    mutate(lowerCaseParticipant = tolower(participant))
  
  if (nrow(pretest) > 0) {
    all_summary <- all_summary %>%
    filter((questType == 'crowding' & !lowerCaseParticipant %in% excludeCrowding$lowerCaseParticipant) |
           (questType == 'acuity' & !lowerCaseParticipant %in% excludeAcuity$lowerCaseParticipant) | 
           (questType == 'Repeated letters' & !lowerCaseParticipant %in% excludeRepeated$lowerCaseParticipant) | 
             questType == 'practice' | 
             questType == 'RSVP reading') %>% 
      select(-lowerCaseParticipant)
  }
  quest <- all_summary %>% 
    select(participant, block_condition, thresholdParameter, conditionName, font, 
           questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop, questType, Grade,
           `Skilled reader?`, ParticipantCode, questTrials, NWrongTrial,NCorrectTrial,frac
           ) %>% 
    left_join(eccentricityDeg, by = c('participant', 'conditionName'))
  
  if (nrow(pretest) > 0) {
    quest <- quest %>% 
      filter((targetEccentricityXDeg == 0) | 
               ( targetEccentricityXDeg != 0 & !tolower(participant) %in% excludePeripheral$lowerCaseParticipant))
  }
  conditionNames = unique(quest$conditionName)
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    quest <- quest %>% filter(conditionName %in% conditionNameInput)
  } 
  
  quest <- quest %>% 
    mutate(questType = case_when(
      (questType == 'crowding' | questType == 'acuity') & targetEccentricityXDeg == 0 ~ paste('Foveal', questType),
      (questType == 'crowding' | questType == 'acuity') & targetEccentricityXDeg != 0 ~ paste('Peripheral', questType),
      .default = questType
    )) %>% 
    select(-thresholdParameter)

  
  age <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(participant, age) %>% 
      filter(!is.na(age)) %>% 
      distinct()
  }
  
  targetDurationSecs <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(participant, conditionName, targetDurationSec) %>% 
      filter(!is.na(targetDurationSec)) %>% 
      distinct()
  }

  if ('Age' %in% names(pretest) & nrow(pretest) > 0) {
    if (all(is.na(age$age))) {
      age <- age %>%
        select(participant) %>%
        mutate(lowerCaseParticipant = tolower(participant)) %>% 
        left_join(pretest, by = 'lowerCaseParticipant') %>% 
        rename('age' = 'Age') %>% 
        select(participant, age)
    } else {
      tmp <- pretest %>% mutate(Age_p = Age) %>% select(lowerCaseParticipant, Age_p)
      age <- age %>%
        select(participant, age) %>%
        mutate(lowerCaseParticipant = tolower(participant)) %>% 
        left_join(tmp, by = 'lowerCaseParticipant') %>% 
        mutate(age = ifelse(is.na(Age_p), age, Age_p)) %>% 
        select(participant, age)
    }
  }
 
  age <- distinct(age)

  quest <- quest %>%
    left_join(age, by = 'participant') %>% 
    left_join(targetDurationSecs, by = c('participant', 'conditionName'))
  
  quest_all_thresholds <- quest
  valid_ids <- unique(quest_all_thresholds$participant)
  age <- age %>% filter(participant %in% valid_ids)
  
  quest <- quest %>% 
    group_by(participant, conditionName, font, questType,age, Grade,
             `Skilled reader?`, targetDurationSec, targetEccentricityXDeg, targetEccentricityYDeg, ParticipantCode) %>% 
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm=T),
              questSDAtEndOfTrialsLoop = mean(questSDAtEndOfTrialsLoop, na.rm=T)) %>% 
    ungroup()
  
  
  ########################### CROWDING ############################
  crowding <- quest %>% 
    filter(questType == 'Foveal crowding' | 
           questType == 'Peripheral crowding') %>% 
    mutate(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop,
      bouma_factor = 10^(questMeanAtEndOfTrialsLoop)/sqrt(targetEccentricityXDeg^2+targetEccentricityYDeg^2)) %>% 
    select(-questMeanAtEndOfTrialsLoop)
  
  ########################### RSVP READING ############################
  
  rsvp_speed <- quest %>% 
    filter(questType == "RSVP reading") %>% 
    select(participant,conditionName, questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop,
           font, Grade, age,`Skilled reader?`,ParticipantCode) %>%
    dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
    mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP,
           targetKind = 'rsvpReading') 
  
  ################################ READING #######################################
  
  if (ncol(reading) > 1) {
    reading <- reading %>% 
      left_join(age, by = "participant") %>% 
      filter(!tolower(participant) %in% basicExclude$lowerCaseParticipant)
    if ('Include' %in% names(pretest)) {
      reading <- reading %>% filter(!participant %in% (pretest %>% filter(Include == 'no') %>% select(participant)))
    }
  }
 
  ################################ REPEAT LETTER #######################################
  repeatedLetters <- quest %>% 
    filter(questType == "Repeated letters") %>% 
    mutate(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop,
           bouma_factor = 10^(questMeanAtEndOfTrialsLoop)/sqrt(targetEccentricityXDeg^2+targetEccentricityYDeg^2)) %>% 
    select(-questMeanAtEndOfTrialsLoop)
    
  
  #### get viewing distance and font size####
  
  viewingdistance <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    data_list[[i]] %>% 
      select(conditionName, participant, viewingDistanceDesiredCm) %>% 
      filter(!is.na(viewingDistanceDesiredCm)) %>% 
      distinct()
  }
  
  rsvp_speed <- rsvp_speed %>% 
    left_join(viewingdistance, by = c("conditionName", "participant"))
  
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
  
  
  #### acuity ####
  acuity <- quest %>% 
    filter(questType == 'Foveal acuity' | questType == 'Peripheral acuity')
  
  if ('Grade' %in% names(pretest)) {
    age <- age %>%
      mutate(lowerCaseParticipant = tolower(participant)) %>% 
      left_join(select(pretest, Grade, lowerCaseParticipant), by = 'lowerCaseParticipant')
  } else {
    age$Grade = NA
  }

  ##### console logs #####
  
  print(paste('nrow of quest:', nrow(quest)))
  print(paste('nrow of reading:', nrow(reading)))
  print(paste('nrow of crowding:', nrow(crowding)))
  print(paste('nrow of rsvp_speed:', nrow(rsvp_speed)))
  print(paste('nrow of acuity:', nrow(acuity)))
  print(paste('nrow of repeatedLetters:', nrow(repeatedLetters)))
  print(paste('nrow of age:', nrow(age)))
  
  return(list(reading = reading, 
              crowding = crowding,
              rsvp = rsvp_speed,
              fluency = fluency,
              acuity = acuity,
              repeatedLetters = repeatedLetters,
              quest = quest, # threshold averaged by participant, conditionName
              quest_all_thresholds = quest_all_thresholds, # include all threshold estimate
              age = age,
              conditionNames = conditionNames))
}

generate_threshold <- function(data_list, summary_list, pretest, stairs, df, 
                               minNQuestTrials, minWrongTrials, maxQuestSD, 
                               conditionNameInput, maxReadingSpeed){
  print('inside generate_threshold')
  if (nrow(pretest) > 0) {
    if (!'Grade' %in% names(pretest)) {
      pretest$Grade = -1
    }
    if (!'Skilled reader?' %in% names(pretest)) {
      pretest$`Skilled reader?` = 'unkown'
    }
    pretest <- pretest %>%
      mutate(lowerCaseParticipant = tolower(participant))
    if ('Exclude?' %in% names(pretest)) {
      basicExclude <-pretest %>% 
        filter(`Exclude?` == TRUE)
    } else {
      basicExclude <- tibble(participant = '')
    }
  } else {
    basicExclude <- tibble(participant = '')
  }
  
  stairs_summary <- stairs %>%
    group_by(participant, thresholdParameter, block_condition, conditionName) %>% 
    summarize(TrialsSentToQuest = sum(trialGivenToQuest),
              BadTrials = sum(!trialGivenToQuest)) %>% 
    filter((thresholdParameter != 'spacingDeg'  & thresholdParameter != 'spacing') | TrialsSentToQuest >= minNQuestTrials)
  
  wrongTrials <- stairs %>%
    group_by(participant, staircaseName) %>%
    summarize(NWrongTrial = sum(!`key_resp.corr`,na.rm = T)) %>% 
    ungroup() %>% 
    filter(NWrongTrial >= minWrongTrials) %>% 
    mutate(block_condition = as.character(staircaseName)) %>% 
    distinct(participant, block_condition)
  
  all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
    summary_list[[i]]
  } %>% 
    filter(!tolower(participant) %in% basicExclude$lowerCaseParticipant) %>% 
    inner_join(stairs_summary %>% select(participant, block_condition)) %>% 
    filter(questSDAtEndOfTrialsLoop <= maxQuestSD) %>% 
    inner_join(wrongTrials, by = c("participant", "block_condition"))
  
  if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
    all_summary <- all_summary %>% filter(conditionName %in% conditionNameInput)
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
  
  
  ################################ READING #######################################
  reading <- tibble()
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>% 
      select(block_condition, experiment, participant, conditionName, font, readingPageWords, readingPageDurationOnsetToOffsetSec,
             targetKind, thresholdParameter, readingNumberOfQuestions) %>% 
      group_by(participant, block_condition) %>%
      mutate(trial = row_number()) %>% 
      ungroup() %>% 
      mutate(wordPerMin = 
               ifelse(trial < 3 & tolower(participant) %in%englishChild$participant,
             9.5 / as.numeric(readingPageDurationOnsetToOffsetSec) * 60, 
             as.numeric(readingPageWords)/ as.numeric(readingPageDurationOnsetToOffsetSec) * 60)) %>% 
  mutate(log_WPM = log10(wordPerMin)) %>% 
  filter(targetKind == "reading" & font !="") %>% 
  ungroup()

reading <- rbind(reading, t)
  }
  if (tolower(reading$participant[1]) %in% englishChild$participant) {
    reading <- reading %>% filter(trial>= 2) %>% 
      mutate(font = as.character(font), 
             participant = as.character(participant), 
             block_condition = as.character(block_condition), 
             thresholdParameter = as.character(thresholdParameter))
  } 
  
  reading <- reading %>% filter(wordPerMin <= maxReadingSpeed)
  
  
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
    mutate(targetKind = "reading")
  
  threshold_all <- all_summary %>%
    group_by(participant, experiment, conditionName) %>%
    dplyr::summarize(
      pm = mean(questMeanAtEndOfTrialsLoop),
      sd = sd(questMeanAtEndOfTrialsLoop)) %>% 
    mutate(parameter = "threshold") %>% 
    ungroup()
  
  print(unique(threshold_all$conditionName))
  threshold_summary <- threshold_all %>% 
    mutate(variance = sd^2) %>% 
    group_by(conditionName, experiment) %>% 
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
    group_by(conditionName, participant, experiment) %>%
    dplyr::summarize(pm = mean(wordPerMin, na.rm =T),
                     sd = sd(wordPerMin, na.rm =T)) %>% 
    filter(!is.na(pm)) %>% 
    mutate(parameter = "word per minute") %>% 
    ungroup()
  
  wpm_summary <- wpm_all %>% 
    mutate(variance = sd^2) %>% 
    group_by(conditionName, experiment) %>% 
    dplyr::summarize(
      m = mean(pm),
      `se across participants` = sd(pm)/sqrt(n()), 
      `sd across participants` = sd(pm),
      `sd across repetitions` = sqrt(mean(variance, na.rm = T)),
      N = n(),
      parameter = "word per minute") %>% 
    mutate(conditionName = as.character(conditionName))
  
  df <- df %>%
    rename(pavloviaSessionID = participant,
           participantID = ParticipantCode) %>% 
    select(-BirthMonthYear)
  if (nrow(pretest) > 0) {
    grade <- pretest %>% 
      select(participant, Grade) %>% 
      rename(pavloviaSessionID = participant)
  } else {
    grade = tibble(pavloviaSessionID = unique(all_summary$participant), 
                   Grade = NA)
  }
 
  
  threshold_each <- rbind(threshold_all, wpm_all) %>% 
    mutate(m = round(pm,3),
           sd = round(sd,3)) %>% 
    rename(pavloviaSessionID = participant) %>% 
    left_join(df, by = 'pavloviaSessionID') %>% 
    left_join(grade, by = 'pavloviaSessionID') %>% 
    mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
    select(experiment, pavloviaSessionID, participantID, age, Grade, conditionName, m, sd, parameter)
  
  threshold <- rbind(threshold_summary, wpm_summary) %>% 
    mutate(m = round(m,3),
           `se across participants` = round(`se across participants`,3),
           `sd across participants` = round(`sd across participants`,3),
           `sd across repetitions` = round(`sd across repetitions`,3)) %>% 
    select(experiment,conditionName,m,`se across participants`,`sd across participants`,`sd across repetitions`, N,parameter)
  all_summary <- all_summary %>% 
    left_join(stairs_summary, by = c('participant', 'block_condition', 'conditionName')) %>% 
    rename(pavloviaSessionID = participant) %>% 
    mutate(condition = str_split(block_condition,'_')[[1]][2]) %>% 
    left_join(df, by = 'pavloviaSessionID') %>% 
    left_join(grade, by = 'pavloviaSessionID') %>% 
    mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
    select(experiment, pavloviaSessionID, participantID, 
           age, Grade, conditionName, block, condition, 
           conditionName, targetKind, font, questMeanAtEndOfTrialsLoop,
           questSDAtEndOfTrialsLoop, TrialsSentToQuest, BadTrials)
  return(list(reading_exceed_1500 %>% select(warning), threshold, threshold_each, all_summary))
}
