library(foreach)
library(dplyr)
library(stringr)

englishChild <- readxl::read_xlsx('Basic_Exclude.xlsx') %>%
  mutate(participant = tolower(ID))

generate_threshold <- 
  function(data_list, summary_list, df, pretest, stairs, prolific, filterInput, skillFilter, minNQuestTrials, 
           minWrongTrials, maxQuestSD, conditionNameInput, maxReadingSpeed, minRulerCm, minCQAccuracy) {
    
    print('inside threshold warning')
    print(paste0('length of data list: ', length(data_list)))
    print(paste0('length of summary list: ', length(summary_list)))
    if (is.null(data_list)) {
      return(list())
    }
    if (length(data_list) == 0) {
      return(list())
    }
    
    #### age ####
    age <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
      data_list[[i]] %>% 
        select(participant, age) %>% 
        filter(!is.na(age)) %>% 
        distinct()
    }

    #### NQuestTrials ####
    NQuestTrials <- stairs %>%
      group_by(participant, staircaseName, thresholdParameter) %>%
      summarize(questTrials = sum(trialGivenToQuest,na.rm = T),
                badTrials = sum(!trialGivenToQuest,na.rm = T),
                .groups="drop") %>% 
      filter((thresholdParameter != 'spacingDeg'  & thresholdParameter != 'spacing') | questTrials >= minNQuestTrials) %>% 
      mutate(block_condition = as.character(staircaseName)) %>% 
      distinct(participant, block_condition, questTrials, badTrials)
    
    #### wrongTrials ####
    wrongTrials <- stairs %>%
      group_by(participant, staircaseName) %>%
      summarize(NWrongTrial = sum((!`key_resp.corr`) & trialGivenToQuest, na.rm = T),
                NCorrectTrial = sum((`key_resp.corr`) & trialGivenToQuest, na.rm = T),
                frac = sum((!`key_resp.corr`) & trialGivenToQuest, na.rm = T) / sum(trialGivenToQuest, na.rm = T),
                .groups="drop") %>% 
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
    
    
    ################################ READING #######################################
    
    reading <- tibble()
    for (i in 1:length(data_list)) {
      t <- data_list[[i]] %>% 
        select(experiment, date, block_condition, participant, conditionName, font, readingPages, readingPageWords, readingPageDurationOnsetToOffsetSec,
               targetKind, thresholdParameter, readingNumberOfQuestions) %>% 
        filter(readingPages > 1) %>% 
        group_by(experiment, date, participant, block_condition, conditionName, font) %>%
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
        mutate(experiment = '',
               date = '',
               block_condition = '',
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
        reading <- reading %>% mutate(ParticipantCode = participant)
      }
    } else {
      # Use mutate instead of direct assignment to handle empty tibbles
      reading <- reading %>% 
        mutate(ParticipantCode = participant,
               Grade = -1,
               `Skilled reader?` = 'unkown')
    }
    
    
    #### combine all thresholds #####
    
    all_summary <- foreach(i = 1 : length(summary_list), .combine = "rbind") %do% {
      summary_list[[i]] %>% mutate(order = i)
    }
    
    # Check if participant column exists before filtering
    if (!"participant" %in% names(all_summary)) {
      # Create empty all_summary with expected structure
      all_summary <- tibble(
        staircaseName = character(), experiment = character(), participant = character(),
        block = integer(), block_condition = character(), conditionName = character(),
        targetKind = character(), font = character(), thresholdParameter = character(),
        questMeanAtEndOfTrialsLoop = numeric(), questSDAtEndOfTrialsLoop = numeric(),
        order = integer()
      )
    } else {
      all_summary <- all_summary %>% 
        filter(!tolower(participant) %in% basicExclude$lowerCaseParticipant) %>% 
        mutate(participant = as.character(participant),
               block_condition = as.character(block_condition))
    }
    
    all_summary <- all_summary %>%
      # apply questSD filter
      left_join(NQuestTrials, by = c('participant', 'block_condition')) %>% 
      filter(questSDAtEndOfTrialsLoop <= maxQuestSD) %>% 
      inner_join(wrongTrials, by = c('participant', 'block_condition'))
    
    
    if (nrow(pretest) > 0) {
      if ('Include' %in% names(pretest)) {
        all_summary <- all_summary %>% 
          left_join(pretest %>% select(participant, Include), by = 'participant') %>% 
          filter(Include == 'yes')
      }
      
      all_summary <- all_summary %>% 
        mutate(lowerCaseParticipant = tolower(participant)) %>% 
        left_join(select(pretest, Grade, `Skilled reader?`, lowerCaseParticipant), by = 'lowerCaseParticipant') %>% 
        select(-lowerCaseParticipant) %>% 
        mutate(Grade = ifelse(is.na(Grade), -1, Grade), block_condition = as.character(block_condition)) %>% 
        mutate(`Skilled reader?` = ifelse(is.na(`Skilled reader?`), 'unkown', `Skilled reader?`))
      
      
      if (skillFilter == "skilled") {
        all_summary <- all_summary %>% 
          filter(`Skilled reader?` == "TRUE")
      } else if (skillFilter == "unskilled") {
        all_summary <- all_summary %>% 
          filter(`Skilled reader?` == "FALSE")
      }
      
      if (!'ParticipantCode' %in% names(all_summary)) {
        all_summary <- all_summary %>% mutate(ParticipantCode = participant)
      }
    } else {
      all_summary <- all_summary %>% 
        mutate(ParticipantCode = participant,
               Grade = -1,
               `Skilled reader?` = 'unkown')
    }
    
    
    
    #### calculate cut-off start here ####
    
    reading_avg <- reading %>%
      group_by(participant) %>%
      summarize(avg = mean(wordPerMin),
                .groups="drop")
    
    threshold <- ifelse(nrow(reading_avg) != 0, quantile(reading_avg$avg, 0.25, na.rm = T), 0)
    slowest = tibble()
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
      all_summary <- all_summary %>%
        filter(tolower(participant) %in% tolower(slowest$participant))
      print('after filtering')
      print(paste('unique pavloviaSessionID in reading', n_distinct(reading$participant)))
      print(paste('unique pavloviaSessionID in threshold', n_distinct(all_summary$participant)))
      print(paste('number of rows in reading', nrow(reading)))
      print(paste('number of rows all_summary', nrow(all_summary)))
    } 
    if (filterInput == 'fastest' & nrow(slowest) > 0) {
      reading <- reading%>% filter(!tolower(participant) %in% tolower(slowest$participant))
      all_summary <- all_summary %>%
        filter(!tolower(participant) %in% tolower(slowest$participant))
    }
    print('done filter input')
    
    
    if (ncol(reading) > 1) {
      reading <- reading %>% 
        left_join(age, 
                  by = 'participant',
                  relationship = "many-to-many") %>% 
        filter(!tolower(participant) %in% basicExclude$lowerCaseParticipant)
      if ('Include' %in% names(pretest)) {
        reading <- reading %>% filter(!participant %in% (pretest %>% filter(Include == 'no') %>% select(participant)))
      }
    }
    # After filter, compute reading each block
    reading_each <- reading %>% 
      group_by(font, participant, block_condition, thresholdParameter) %>%
      dplyr::summarize(avg_wordPerMin = 10^(mean(log_WPM, na.rm = T)),
                       .groups = "drop")
    
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
        thresholdParameter == "targetSoundDBSPL" ~ 'Sound',
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
      select(experiment, participant, block_condition, thresholdParameter, conditionName, font, 
             questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop, questType, Grade,
             `Skilled reader?`, ParticipantCode, questTrials, NWrongTrial,NCorrectTrial,frac
      ) %>% 
      left_join(eccentricityDeg, 
                by = c('participant', 'conditionName'),
                relationship = "many-to-many")
    
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
      left_join(age, 
                by = 'participant',
                relationship = "many-to-many") %>% 
      left_join(targetDurationSecs, by = c('participant', 'conditionName'))
    
    quest_all_thresholds <- quest
    valid_ids <- unique(quest_all_thresholds$participant)
    age <- age %>% filter(participant %in% valid_ids)
    
    quest <- quest %>% 
      group_by(experiment, participant, conditionName, font, questType, age, Grade,
               `Skilled reader?`, targetDurationSec, targetEccentricityXDeg, targetEccentricityYDeg, ParticipantCode) %>% 
      summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop, na.rm=T),
                questSDAtEndOfTrialsLoop = mean(questSDAtEndOfTrialsLoop, na.rm=T),
                .groups="drop")
    
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
      select(experiment, participant,conditionName, questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop,
             font, Grade, age,`Skilled reader?`,ParticipantCode) %>%
      dplyr::rename(log_duration_s_RSVP = questMeanAtEndOfTrialsLoop) %>% 
      mutate(block_avg_log_WPM = log10(60) - log_duration_s_RSVP,
             targetKind = 'rsvpReading') 
    
    
    ################################ REPEAT LETTER #######################################
    repeatedLetters <- quest %>% 
      filter(questType == "Repeated letters") %>% 
      mutate(log_crowding_distance_deg = questMeanAtEndOfTrialsLoop,
             bouma_factor = 10^(questMeanAtEndOfTrialsLoop)/sqrt(targetEccentricityXDeg^2+targetEccentricityYDeg^2)) %>% 
      select(-questMeanAtEndOfTrialsLoop)
    
    #### acuity ####
    acuity <- quest %>% 
      filter(questType == 'Foveal acuity' | questType == 'Peripheral acuity')
    
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
        summarize(accuracy = mean(readWordIdentifiedBool),
                  .groups="drop")
      reading <- reading %>% left_join(reading_accuracy, by = c("participant", "block_condition") )
      # Use mutate to handle empty tibbles safely
      reading <- reading %>% mutate(accuracy = factor(accuracy, levels = c(0,0.2,0.4,0.6,0.8,1)))
    }
    
    fluency <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
      if("questionAndAnswerCorrectAnswer" %in% colnames(data_list[[i]])) {
        data_list[[i]] %>% filter(grepl("fluency", conditionName, fixed=TRUE) ) %>% 
          select(block, participant, conditionName, questionAndAnswerResponse, `trials.thisN`,
                 questionAndAnswerNickname, questionAndAnswerQuestion, targetKind, questionAndAnswerCorrectAnswer)
      }
    }
    if(is.null(fluency)) fluency = tibble()
    if (nrow(fluency) > 0) {
      fluency <- fluency %>% 
        group_by(participant) %>% 
        summarize(accuracy = mean(questionAndAnswerResponse == questionAndAnswerCorrectAnswer),
                  .groups="drop")
    } else {
      fluency = tibble()
    }
    
    if ('Grade' %in% names(pretest)) {
      age <- age %>%
        mutate(lowerCaseParticipant = tolower(participant)) %>% 
        left_join(select(pretest, Grade, lowerCaseParticipant), by = 'lowerCaseParticipant')
    } else {
      # Use mutate instead of direct assignment to handle empty tibbles
      age <- age %>% mutate(Grade = NA_real_)
    }
    
    ##### console logs #####
    
    print(paste('nrow of quest:', nrow(quest)))
    print(paste('nrow of reading:', nrow(reading)))
    print(paste('nrow of crowding:', nrow(crowding)))
    print(paste('nrow of rsvp_speed:', nrow(rsvp_speed)))
    print(paste('nrow of acuity:', nrow(acuity)))
    print(paste('nrow of repeatedLetters:', nrow(repeatedLetters)))
    print(paste('nrow of age:', nrow(age)))
    
    
    
    #### beauty and comfort ####

    QA <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
      data_list[[i]] %>% 
        distinct(experiment,
                 participant,
                 block,
                 block_condition, 
                 conditionName, 
                 blockShuffleGroups2,
                 questionAndAnswerQuestion, 
                 questionAndAnswerNickname, 
                 questionAndAnswerResponse,
                 questionAndAnswerCorrectAnswer) %>%
        filter(!is.na(questionAndAnswerNickname),
               !is.na(questionAndAnswerQuestion),
               questionAndAnswerNickname != "", 
               questionAndAnswerQuestion != ""
        ) %>% 
        # The rename of questionAndAnswerNickname is to fix an error in previous experiment setting
        mutate(correct = (questionAndAnswerResponse == questionAndAnswerCorrectAnswer),
               questionAndAnswerNickname = case_when(questionAndAnswerNickname=="CMFRTAmareddine" ~"CMFRTSaudiTextv1",
                                                     questionAndAnswerNickname=="CMFRTMakdessi" ~"CMFRTSaudiTextv2",
                                                     questionAndAnswerNickname=="CMFRTKafa" ~"CMFRTSaudiTextv3",
                                                     .default = questionAndAnswerNickname))
    } %>% 
      filter(!blockShuffleGroups2=="readin5") %>% 
      arrange(experiment, participant, block, block_condition)
    
    # write.csv(QA %>% filter(questionAndAnswerCorrectAnswer != "",
    #                                   !is.na(questionAndAnswerCorrectAnswer)),
    #           'QA.csv')
    #### participant information table ####
    
    participant_info_list <- list()
    
    for (i in 1:length(data_list)) {
      # Extract data with standardized columns
      # Check if objectName column exists
      has_objectName <- "objectName" %in% names(data_list[[i]])
      
      temp_data <- data_list[[i]] %>% 
        select(participant, pxPerCm,
               rulerLength, rulerUnit, 
               calibrateTrackDistance, distanceObjectCm,
               any_of("objectName")) %>%
        distinct() %>%
        filter(!is.na(participant)) %>%
        mutate(
          rulerLength = as.numeric(rulerLength),
          rulerUnit = as.character(rulerUnit),
          calibrateTrackDistance = as.character(calibrateTrackDistance),
          distanceObjectCm = as.numeric(distanceObjectCm)
        )
      
      # Add objectName column if it doesn't exist
      if (!has_objectName) {
        temp_data <- temp_data %>% mutate(objectName = NA_character_)
      } else {
        temp_data <- temp_data %>% mutate(objectName = as.character(objectName))
      }
      
      temp_data <- temp_data %>%
        select(participant, pxPerCm,
               rulerLength, rulerUnit, 
               calibrateTrackDistance, distanceObjectCm, objectName)
      
      participant_info_list[[i]] <- temp_data
    }
    
    # Combine all datasets with consistent column structure and consolidate duplicates
    participant_info <- do.call(rbind, participant_info_list) %>%
      distinct() %>%
      mutate(
        # Convert ruler length to cm
        rulerCm = case_when(
          !is.na(rulerLength) & rulerUnit == "cm" ~ rulerLength,
          !is.na(rulerLength) & rulerUnit == "inches" ~ rulerLength * 2.54,
          .default = NA_real_
        )
      ) %>%
      group_by(participant) %>%
      summarize(
        rulerCm = first(rulerCm[!is.na(rulerCm)]),
        calibrateTrackDistance = first(calibrateTrackDistance[!is.na(calibrateTrackDistance)]),
        distanceObjectCm = first(distanceObjectCm[!is.na(distanceObjectCm)]),
        pxPerCm =  first(pxPerCm[!is.na(pxPerCm)]),
        objectName = first(objectName[!is.na(objectName) & objectName != ""]),
        .groups = "drop"
      )
    
    # Extract COMMENT and OBJCT responses directly for participant info table
    participant_qa_list <- list()
    
    for (i in 1:length(data_list)) {
      # Extract COMMENT and OBJCT data directly
      temp_qa <- data_list[[i]] %>%
        filter(!is.na(questionAndAnswerNickname),
               questionAndAnswerNickname %in% c("COMMENT", "OBJCT")) %>%
        distinct(participant, questionAndAnswerNickname, questionAndAnswerResponse, questionAndAnswerQuestion)
      
      if (nrow(temp_qa) > 0) {
        participant_qa_list[[i]] <- temp_qa
      }
    }
    
    # Combine all participant QA data
    if (length(participant_qa_list) > 0) {
      participant_qa <- do.call(rbind, participant_qa_list) %>%
        distinct()
    } else {
      participant_qa <- tibble(participant = character(), 
                               questionAndAnswerNickname = character(), 
                               questionAndAnswerResponse = character(),
                               questionAndAnswerQuestion = character())
    }
    
    # Split into comments and objects
    comments_data <- participant_qa %>%
      filter(questionAndAnswerNickname == "COMMENT") %>%
      distinct(participant, questionAndAnswerResponse) %>%
      rename(Comment = questionAndAnswerResponse)
    
    objects_data <- participant_qa %>%
      filter(questionAndAnswerNickname == "OBJCT") %>% 
      distinct(participant, questionAndAnswerResponse) %>%
      rename(Object = questionAndAnswerResponse)
    
    # Extract objectName from distanceCalibrationTJSON.COMMON for "paper" method
    # This provides the object name when _calibrateDistance is "paper"
    object_name_from_json <- tibble(participant = character(), ObjectFromJSON = character())
    for (i in 1:length(data_list)) {
      if ("distanceCalibrationTJSON" %in% names(data_list[[i]])) {
        tryCatch({
          participant_id <- first(na.omit(data_list[[i]]$participant))
          if (is.na(participant_id) || participant_id == "") next
          
          raw_json <- data_list[[i]]$distanceCalibrationTJSON
          raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
          if (length(raw_json) == 0) next
          
          json_txt <- raw_json[1]
          # Sanitize JSON string (same logic as sanitize_json_string in distancePlot.R)
          json_txt <- trimws(as.character(json_txt))
          # Remove outer quotes if present
          if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
            json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
          }
          # Fix CSV escaping: replace doubled quotes with single quotes
          json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
          json_txt <- gsub("\\\\n", " ", json_txt)
          json_txt <- gsub("\\\\t", " ", json_txt)
          json_txt <- gsub("\n", " ", json_txt)
          json_txt <- gsub("\t", " ", json_txt)
          
          parsed_json <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          
          # Extract objectName from COMMON section
          if (!is.null(parsed_json$COMMON) && !is.null(parsed_json$COMMON$objectName)) {
            obj_name <- parsed_json$COMMON$objectName
            if (length(obj_name) > 0 && !is.na(obj_name[1]) && obj_name[1] != "") {
              object_name_from_json <- rbind(object_name_from_json, 
                tibble(participant = participant_id, ObjectFromJSON = obj_name[1]))
            }
          }
        }, error = function(e) {
          # Skip if JSON parsing fails
        })
      }
    }
    
    # Merge Q&A objects with JSON objects (Q&A takes priority)
    if (nrow(object_name_from_json) > 0) {
      objects_data <- objects_data %>%
        full_join(object_name_from_json, by = "participant") %>%
        mutate(Object = ifelse(is.na(Object) | Object == "", ObjectFromJSON, Object)) %>%
        select(participant, Object)
    }
    
    # Extract objectSuggestion from distanceCalibrationJSON
    object_suggestion_from_json <- tibble(participant = character(), objectSuggestion = character())
    for (i in 1:length(data_list)) {
      json_col <- if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {
        "distanceCalibrationJSON"
      } else if ("distanceCalibrationTJSON" %in% names(data_list[[i]])) {
        "distanceCalibrationTJSON"
      } else {
        NULL
      }
      
      if (!is.null(json_col)) {
        tryCatch({
          participant_id <- first(na.omit(data_list[[i]]$participant))
          if (is.na(participant_id) || participant_id == "") next
          
          raw_json <- data_list[[i]][[json_col]]
          raw_json <- raw_json[!is.na(raw_json) & raw_json != ""]
          if (length(raw_json) == 0) next
          
          json_txt <- raw_json[1]
          # Sanitize JSON string
          json_txt <- trimws(as.character(json_txt))
          if (nchar(json_txt) >= 2 && substr(json_txt, 1, 1) == '"' && substr(json_txt, nchar(json_txt), nchar(json_txt)) == '"') {
            json_txt <- substr(json_txt, 2, nchar(json_txt) - 1)
          }
          json_txt <- gsub('""', '"', json_txt, fixed = TRUE)
          json_txt <- gsub("\\\\n", " ", json_txt)
          json_txt <- gsub("\\\\t", " ", json_txt)
          json_txt <- gsub("\n", " ", json_txt)
          json_txt <- gsub("\t", " ", json_txt)
          
          parsed_json <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          
          # Extract objectSuggestion (top-level array in distanceCalibrationJSON)
          if (!is.null(parsed_json$objectSuggestion)) {
            obj_suggestion <- parsed_json$objectSuggestion
            # Get the first non-empty value from the array
            non_empty_suggestions <- obj_suggestion[!is.na(obj_suggestion) & obj_suggestion != ""]
            if (length(non_empty_suggestions) > 0) {
              object_suggestion_from_json <- rbind(object_suggestion_from_json, 
                tibble(participant = participant_id, objectSuggestion = non_empty_suggestions[1]))
            } else {
              # If all values are empty, still record the participant with empty string
              object_suggestion_from_json <- rbind(object_suggestion_from_json, 
                tibble(participant = participant_id, objectSuggestion = ""))
            }
          }
        }, error = function(e) {
          # Skip if JSON parsing fails
        })
      }
    }
    
    sessions_data <- generate_summary_table(data_list, stairs, pretest, prolific)
    
    # Extract the needed columns from sessions data
    # Handle case where Prolific participant ID might not exist
    if ("Prolific participant ID" %in% names(sessions_data)) {
      sessions_columns <- sessions_data %>%
        select(`Pavlovia session ID`, `Prolific participant ID`, `device type`, `Prolific min`, system, browser, ok, screenWidthCm, resolution) %>%
        rename(
          PavloviaParticipantID = `Pavlovia session ID`,
          ProlificParticipantID = `Prolific participant ID`,
          screenResolutionXY = resolution,
        ) %>%
        distinct()
    } else {
      sessions_columns <- sessions_data %>%
        select(`Pavlovia session ID`, `device type`, `Prolific min`, system, browser, ok, screenWidthCm, resolution) %>%
        mutate(ProlificParticipantID = NA_character_) %>%
        rename(
          PavloviaParticipantID = `Pavlovia session ID`,
          screenResolutionXY = resolution,
        ) %>%
        distinct()
    }
    
    # Join all data together
    participant_info <- sessions_columns %>%
      rename(participant = PavloviaParticipantID) %>%
      left_join(comments_data, by = "participant") %>%
      left_join(objects_data, by = "participant") %>%
      left_join(object_suggestion_from_json, by = "participant") %>%
      full_join(participant_info, by = "participant") %>%
      rename(PavloviaParticipantID = participant) %>%
      mutate(
        objectLengthCm = ifelse(!is.na(distanceObjectCm) & is.finite(distanceObjectCm), format(round(distanceObjectCm, 1), nsmall = 1), NA_character_),
        rulerCm = case_when(
          !is.na(rulerCm) ~ format(round(rulerCm), nsmall = 0),
          .default = NA_character_
        ),
        screenWidthCm = format(round(screenWidthCm, 1), nsmall = 1),
        # Format pxPerCm with 1 decimal place
        pxPerCm = ifelse(!is.na(pxPerCm), format(round(as.numeric(pxPerCm), 1), nsmall = 1), NA_character_),
        # Use objectName from CSV as fallback for Object when empty
        Object = ifelse(
          is.na(Object) | Object == "",
          ifelse(!is.na(objectName) & objectName != "", objectName, NA_character_),
          Object
        )
      ) %>%
      {if("ProlificParticipantID" %in% names(.)) rename(., `Prolific Participant ID` = ProlificParticipantID) else mutate(., `Prolific Participant ID` = NA_character_)} %>%
      select(ok, PavloviaParticipantID, `Prolific Participant ID`, `device type`, system, browser, `Prolific min`, 
             screenWidthCm, screenResolutionXY, rulerCm, pxPerCm, objectLengthCm, Object, objectSuggestion, Comment) %>%
      mutate(
        ok_priority = case_when(
          ok == "✅" ~ 1,  # ✅ (white_check_mark) first
          ok == "🚧" ~ 2,  # 🚧 (construction) second  
          ok == "❌" ~ 3,  # ❌ (x) last
          is.na(ok) ~ 4,   # NA last
          .default = 5     # Any other status
        )
      ) %>%
      arrange(ok_priority, PavloviaParticipantID) %>%
      select(-ok_priority)  # Remove the helper column
    

    if ("ok" %in% names(participant_info)) {
      status_counts <- participant_info %>% 
        group_by(ok) %>% 
        summarize(count = n(), .groups = "drop") %>%
        arrange(match(ok, c("✅", "🚧", "❌", NA)))
    }
    
    # Applied filter: 
    # TODO: Move all filter after this point
    # Get ruler length < minRulerCm
    shortRuler <- participant_info %>% 
      filter(as.numeric(rulerCm) < minRulerCm) %>% 
      distinct(PavloviaParticipantID)
    
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
    
    reading <-  reading %>% 
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    crowding <- crowding %>% 
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    rsvp <-rsvp_speed %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    # fluency <- fluency %>%
    #   filter(!participant %in% shortRuler$PavloviaParticipantID)
    acuity <- acuity %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    repeatedLetters <- repeatedLetters %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    quest <- quest %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    quest_all_thresholds <- quest_all_thresholds %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    age <-  age %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    # threshold <- threshold %>%
    #   filter(!participant %in% shortRuler$PavloviaParticipantID)
    # threshold_each <- threshold_each %>%
    #   filter(!participant %in% shortRuler$PavloviaParticipantID)
    all_summary <- all_summary %>% 
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    QA <- QA %>%
      filter(!participant %in% shortRuler$PavloviaParticipantID)
    participant_info <- participant_info %>%
      filter(!PavloviaParticipantID %in% shortRuler$PavloviaParticipantID)
    
    #### Generate ratings summary stat table ####
    
    ratings_raw <- QA %>% 
      select(-c(questionAndAnswerQuestion,questionAndAnswerCorrectAnswer)) %>% 
      mutate(questionAndAnswerResponse = as.numeric(arabic_to_western(questionAndAnswerResponse))) %>% 
      filter(!is.na(questionAndAnswerResponse)) %>% 
      mutate(type =  case_when(substr(questionAndAnswerNickname, 1, 5) == "CMFRT" ~ "CMFRT",
                               grepl('bty', tolower(questionAndAnswerNickname))   ~ "BTY",
                               grepl('familiarity', tolower(questionAndAnswerNickname)) ~ "FAMILIARITY",
                               .default = "")) 

    comfort <- ratings_raw %>% 
      filter(type == "CMFRT") %>% 
      mutate(font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                               questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                               questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                               questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                               questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                               questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                               questionAndAnswerNickname=="CMFRTB-Nazanin" ~ "B-NAZANIN.TTF",
                               questionAndAnswerNickname=="CMFRT-Nazanin" ~ "B-NAZANIN.TTF",
                               questionAndAnswerNickname=="CMFRT-Titr" ~ "Titr.bold.woff2",
                               questionAndAnswerNickname=="CMFRT-Kalameh" ~ "Kalameh-Regular.ttf",
                               questionAndAnswerNickname=="CMFRT-IranNastaliq" ~ "IranNastaliq.ttf",
                               questionAndAnswerNickname=="CMFRT-Moalla" ~ "Moalla.ttf",
                               questionAndAnswerNickname=="CMFRT-MJ_Hoor" ~ "Mj_Hoor_0.ttf",
                               questionAndAnswerNickname=="CMFRT-Mj_Hoor" ~ "Mj_Hoor_0.ttf",
                               questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                               questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                               questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                               TRUE ~ questionAndAnswerNickname  # fallback for any unmatched cases
                               ))
    
    beauty <- ratings_raw %>% 
      filter(type == "BTY") %>% 
      mutate(font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                       conditionName=="beauty-majalla" ~"majalla.ttf",
                       conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                       conditionName=="beauty-Nazanin" ~"B-NAZANIN.TTF",
                       conditionName=="beauty-Titr" ~ "Titr.bold.woff2",
                       conditionName=="beauty-Kalameh" ~ "Kalameh-Regular.ttf",
                       conditionName=="beauty-IranNastaliq" ~ "IranNastaliq.ttf",
                       conditionName=="beauty-Moalla" ~ "Moalla.ttf",
                       conditionName=="beauty-MJ_Hoor" ~ "Mj_Hoor_0.ttf",
                       conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                       conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                       conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
                       TRUE ~ conditionName  # fallback for any unmatched cases
      ))
    
    familiarity <- ratings_raw %>% 
      filter(type == "FAMILIARITY") %>% 
      mutate(font = case_when(
        conditionName=="beauty-Al-Awwal" ~ "Al-Awwal-Regular.ttf",
        conditionName=="beauty-majalla" ~ "majalla.ttf",
        conditionName=="beauty-Saudi" ~ "Saudi-Regular.ttf",
        conditionName=="beauty-Nazanin" ~ "B-NAZANIN.TTF",
        conditionName=="beauty-Titr" ~ "Titr.bold.woff2",
        conditionName=="beauty-Kalameh" ~ "Kalameh-Regular.ttf",
        conditionName=="beauty-IranNastaliq" ~ "IranNastaliq.ttf",
        conditionName=="beauty-Moalla" ~ "Moalla.ttf",
        conditionName=="beauty-MJ_Hoor" ~ "Mj_Hoor_0.ttf",
        conditionName=="beauty-SaudiTextv1" ~ "SaudiTextv1-Regular.otf",
        conditionName=="beauty-SaudiTextv2" ~ "SaudiTextv2-Regular.otf",
        conditionName=="beauty-SaudiTextv3" ~ "SaudiTextv3-Regular.otf",
        TRUE ~ conditionName
      ))

    ratings <- rbind(comfort,beauty,familiarity) %>% 
      group_by(type,font) %>% 
      summarize(N = n(),
                Mean = mean(questionAndAnswerResponse,na.rm = T),
                SD = sd(questionAndAnswerResponse, na.rm = T))
    
    
    
    threshold_all <- all_summary %>%
      group_by(participant, experiment, conditionName, thresholdParameter) %>%
      dplyr::summarize(
        pm = mean(questMeanAtEndOfTrialsLoop, na.rm =T),
        sd = sd(questMeanAtEndOfTrialsLoop, na.rm =T),
        .groups="drop") %>% 
      rename(parameter = thresholdParameter)
    
    threshold_summary <- threshold_all %>% 
      mutate(variance = sd^2) %>% 
      group_by(conditionName, experiment, parameter) %>% 
      dplyr::summarize(
        m = mean(pm, na.rm = T),
        `se across participants` = sd(pm, na.rm =T)/sqrt(n()), 
        `sd across participants` = sd(pm, na.rm =T),
        `sd across repetitions` = sqrt(mean(variance, na.rm = T)),
        N = n(),
        .groups="drop")
    

    # Calculate and apply reading comprehension accuracy
    # And then link to nearest reading block
    # for example block 3 CQ questions should link to block 2 reading
    # And then apply filter
    comprehension_ac <- QA %>%
      group_by(experiment, participant, block) %>% 
      # Calculate accuracy in percentage
      summarize(CQAccuracy = mean(correct*100, na.rm = T), .groups = 'drop',
                Nquestions = sum(!is.na(correct))) %>% 
      mutate(block=as.numeric(block)-1)

    reading_pre <- reading %>% 
      mutate(block = ifelse(length(str_split(block_condition,'_')) == 0,
                            NA,
                            as.numeric(str_split(block_condition,'_')[[1]][1]))) %>% 
      left_join(comprehension_ac, by = c("experiment", "participant","block"))
    
    reading <- reading_pre %>% 
      filter(CQAccuracy >= minCQAccuracy)

    
    # continue to summarize statistics
    wpm_all <- reading %>% 
      filter(conditionName != "") %>% 
      group_by(conditionName, participant, experiment) %>%
      dplyr::summarize(pm = mean(wordPerMin, na.rm =T),
                       sd = sd(wordPerMin, na.rm =T),
                       parameter = "word per minute",
                       .groups="drop") %>% 
      filter(!is.na(pm))
    
    wpm_summary <- wpm_all %>% 
      mutate(variance = sd^2) %>% 
      group_by(conditionName, experiment) %>% 
      dplyr::summarize(
        m = mean(pm),
        `se across participants` = sd(pm)/sqrt(n()), 
        `sd across participants` = sd(pm),
        `sd across repetitions` = sqrt(mean(variance, na.rm = T)),
        N = n(),
        parameter = "word per minute",
        .groups="drop") %>% 
      mutate(conditionName = as.character(conditionName))
    
    
    df <- df %>%
      rename(participantID = ParticipantCode) %>% 
      distinct(participant,participantID )
    
    threshold_each <- rbind(threshold_all, wpm_all) %>% 
      mutate(m = round(pm,3),
             sd = round(sd,3)) %>% 
      left_join(age, 
                by = 'participant',
                relationship = "many-to-many") %>% 
      left_join(df,
                by = 'participant',
                relationship = "many-to-many") %>% 
      mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
      rename(pavloviaSessionID = participant) %>% 
      select(experiment, pavloviaSessionID, participantID, age, Grade, conditionName, m, sd, parameter)
    
    threshold <- rbind(threshold_summary, wpm_summary) %>% 
      mutate(m = round(m,3),
             `se across participants` = round(`se across participants`,3),
             `sd across participants` = round(`sd across participants`,3),
             `sd across repetitions` = round(`sd across repetitions`,3)) %>% 
      select(experiment,conditionName, m,`se across participants`,`sd across participants`,`sd across repetitions`, N,parameter)
    
    all_summary <- all_summary %>% 
      select(-Grade) %>% 
      left_join(df,
                by = 'participant',
                relationship = "many-to-many") %>% 
      left_join(age, 
                by = 'participant',
                relationship = "many-to-many") %>% 
      rename(pavloviaSessionID = participant,
             TrialsSentToQuest = questTrials) %>% 
      mutate(condition = ifelse(length(str_split(block_condition,'_')) == 0,
                                NA,
                                str_split(block_condition,'_')[[1]][2])) %>% 
      select(experiment, pavloviaSessionID, participantID, 
             age, Grade, conditionName, block, condition, 
             conditionName, targetKind, font, questMeanAtEndOfTrialsLoop,
             questSDAtEndOfTrialsLoop, TrialsSentToQuest, badTrials)

    print('done generate_threshold')
    return(list(reading = reading, 
                crowding = crowding,
                rsvp = rsvp_speed,
                fluency = fluency,
                acuity = acuity,
                repeatedLetters = repeatedLetters,
                quest = quest, # threshold averaged by participant, conditionName
                quest_all_thresholds = quest_all_thresholds, # include all threshold estimate
                age = age,
                conditionNames = conditionNames,
                threshold = threshold, 
                threshold_each = threshold_each, 
                all_summary = all_summary,
                ratings = ratings,
                comfort = comfort,
                beauty = beauty,
                familiarity = familiarity,
                QA = QA %>% select(-block),
                participant_info = participant_info,
                reading_pre = reading_pre
    ))
  }
