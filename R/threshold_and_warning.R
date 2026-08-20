library(foreach)
library(dplyr)
library(stringr)

englishChild <- readxl::read_xlsx(file.path("resources", "Basic_Exclude.xlsx")) %>%
  mutate(participant = tolower(ID))

bind_threshold_chunks <- function(chunks, empty = tibble()) {
  chunks <- Filter(function(x) !is.null(x) && is.data.frame(x), chunks)
  if (length(chunks) == 0) {
    return(empty)
  }
  # Keep 0-row frames so bind_rows preserves column schema (foreach rbind did).
  # Harmonize types: dplyr::bind_rows is strict (e.g. character vs double QA).
  dplyr::bind_rows(harmonize_chunks_for_bind_rows(chunks))
}

empty_reading_thresholds <- function() {
  tibble(
    experiment = character(),
    date = character(),
    block_condition = character(),
    participant = character(),
    conditionName = character(),
    font = character(),
    readingPages = numeric(),
    readingPageWords = numeric(),
    readingPageDurationOnsetToOffsetSec = numeric(),
    targetKind = character(),
    thresholdParameter = character(),
    readingNumberOfQuestions = numeric(),
    trial = integer(),
    wordPerMin = numeric(),
    log_WPM = numeric()
  )
}

empty_age <- function() {
  tibble(participant = character(), age = numeric())
}

empty_eccentricity <- function() {
  tibble(
    participant = character(),
    conditionName = character(),
    targetEccentricityXDeg = numeric(),
    targetEccentricityYDeg = numeric()
  )
}

empty_target_duration <- function() {
  tibble(
    participant = character(),
    conditionName = character(),
    targetDurationSec = numeric()
  )
}

empty_viewing_distance <- function() {
  tibble(
    conditionName = character(),
    participant = character(),
    viewingDistanceDesiredCm = numeric()
  )
}

empty_fluency <- function() {
  tibble(
    block = numeric(),
    participant = character(),
    conditionName = character(),
    questionAndAnswerResponse = character(),
    `trials.thisN` = numeric(),
    questionAndAnswerNickname = character(),
    questionAndAnswerQuestion = character(),
    targetKind = character(),
    questionAndAnswerCorrectAnswer = character()
  )
}

empty_qa <- function() {
  tibble(
    experiment = character(),
    participant = character(),
    block = numeric(),
    block_condition = character(),
    conditionName = character(),
    blockShuffleGroups2 = character(),
    questionAndAnswerQuestion = character(),
    questionAndAnswerNickname = character(),
    questionAndAnswerResponse = character(),
    questionAndAnswerCorrectAnswer = character(),
    correct = logical()
  )
}

# dplyr::filter(participant %in% ...) errors on 0-column tibbles (no `participant`).
filter_out_short_ruler <- function(df, short_ruler_ids) {
  if (!is.data.frame(df) || !"participant" %in% names(df)) {
    return(df)
  }
  df %>% filter(!participant %in% short_ruler_ids)
}

# One walk over data_list for all threshold extracts.
# Viewing-distance rows are only taken for i <= length(summary_list) to match
# the historical foreach(i = 1:length(summary_list)) indexing into data_list.
collect_threshold_data_list_inputs <- function(data_list, summary_list_len = length(data_list)) {
  age_chunks <- list()
  reading_chunks <- list()
  eccentricity_chunks <- list()
  duration_chunks <- list()
  viewing_chunks <- list()
  reading_q_chunks <- list()
  fluency_chunks <- list()
  qa_chunks <- list()

  n <- length(data_list)
  if (n == 0) {
    return(list(
      age = empty_age(),
      reading = empty_reading_thresholds(),
      eccentricityDeg = empty_eccentricity(),
      targetDurationSecs = empty_target_duration(),
      viewingdistance = empty_viewing_distance(),
      reading_questions = list(),
      fluency = empty_fluency(),
      QA = empty_qa()
    ))
  }

  for (i in seq_len(n)) {
    df <- data_list[[i]]
    if (is.null(df) || nrow(df) < 1) {
      next
    }

    # age
    if (all(c("participant", "age") %in% names(df))) {
      age_chunks[[length(age_chunks) + 1]] <- df %>%
        select(participant, age) %>%
        filter(!is.na(age)) %>%
        distinct()
    }

    # reading
    needed_reading <- c(
      "experiment", "date", "block_condition", "participant", "conditionName", "font",
      "readingPages", "readingPageWords", "readingPageDurationOnsetToOffsetSec",
      "targetKind", "thresholdParameter", "readingNumberOfQuestions"
    )
    if (all(needed_reading %in% names(df))) {
      reading_chunks[[length(reading_chunks) + 1]] <- df %>%
        select(all_of(needed_reading)) %>%
        filter(readingPages > 1) %>%
        group_by(experiment, date, participant, block_condition, conditionName, font) %>%
        mutate(trial = row_number()) %>%
        ungroup() %>%
        mutate(wordPerMin = as.numeric(ifelse(
          trial < 3 & tolower(participant) %in% englishChild$participant,
          9.5 / as.numeric(readingPageDurationOnsetToOffsetSec) * 60,
          as.numeric(readingPageWords) / as.numeric(readingPageDurationOnsetToOffsetSec) * 60
        ))) %>%
        mutate(log_WPM = as.numeric(log10(wordPerMin))) %>%
        filter(targetKind == "reading" & font != "", !is.na(wordPerMin))
    }

    # eccentricity
    if (all(c("participant", "conditionName", "targetEccentricityXDeg", "targetEccentricityYDeg") %in% names(df))) {
      eccentricity_chunks[[length(eccentricity_chunks) + 1]] <- df %>%
        distinct(participant, conditionName, targetEccentricityXDeg, targetEccentricityYDeg)
    }

    # target duration
    if (all(c("participant", "conditionName", "targetDurationSec") %in% names(df))) {
      duration_chunks[[length(duration_chunks) + 1]] <- df %>%
        select(participant, conditionName, targetDurationSec) %>%
        mutate(targetDurationSec = suppressWarnings(as.numeric(targetDurationSec))) %>%
        filter(!is.na(targetDurationSec), is.finite(targetDurationSec), targetDurationSec > 0) %>%
        distinct()
    }

    # viewing distance (legacy: only first length(summary_list) files)
    if (i <= summary_list_len &&
        all(c("conditionName", "participant", "viewingDistanceDesiredCm") %in% names(df))) {
      viewing_chunks[[length(viewing_chunks) + 1]] <- df %>%
        select(conditionName, participant, viewingDistanceDesiredCm) %>%
        filter(!is.na(viewingDistanceDesiredCm)) %>%
        distinct()
    }

    # reading comprehension questions (block assignment deferred until reading is known)
    if ("readWordIdentifiedBool" %in% names(df)) {
      rq <- df %>%
        filter(!is.na(readWordIdentifiedBool)) %>%
        select(participant, readWordIdentifiedBool)
      if (nrow(rq) > 0) {
        reading_q_chunks[[length(reading_q_chunks) + 1]] <- rq
      }
    }

    # fluency
    if ("questionAndAnswerCorrectAnswer" %in% names(df) && "conditionName" %in% names(df)) {
      fl <- df %>%
        filter(grepl("fluency", conditionName, fixed = TRUE)) %>%
        select(
          block, participant, conditionName, questionAndAnswerResponse, `trials.thisN`,
          questionAndAnswerNickname, questionAndAnswerQuestion, targetKind, questionAndAnswerCorrectAnswer
        )
      if (nrow(fl) > 0) {
        fluency_chunks[[length(fluency_chunks) + 1]] <- fl
      }
    }

    # QA
    qa_cols <- c(
      "experiment", "participant", "block", "block_condition", "conditionName",
      "blockShuffleGroups2", "questionAndAnswerQuestion", "questionAndAnswerNickname",
      "questionAndAnswerResponse", "questionAndAnswerCorrectAnswer"
    )
    if (all(qa_cols %in% names(df))) {
      qa <- df %>%
        distinct(
          experiment, participant, block, block_condition, conditionName, blockShuffleGroups2,
          questionAndAnswerQuestion, questionAndAnswerNickname, questionAndAnswerResponse,
          questionAndAnswerCorrectAnswer
        ) %>%
        filter(
          !is.na(questionAndAnswerNickname),
          !is.na(questionAndAnswerQuestion),
          questionAndAnswerNickname != "",
          questionAndAnswerQuestion != ""
        ) %>%
        mutate(
          correct = (questionAndAnswerResponse == questionAndAnswerCorrectAnswer),
          questionAndAnswerNickname = case_when(
            questionAndAnswerNickname == "CMFRTAmareddine" ~ "CMFRTSaudiTextv1",
            questionAndAnswerNickname == "CMFRTMakdessi" ~ "CMFRTSaudiTextv2",
            questionAndAnswerNickname == "CMFRTKafa" ~ "CMFRTSaudiTextv3",
            .default = questionAndAnswerNickname
          )
        )
      if (nrow(qa) > 0) {
        qa_chunks[[length(qa_chunks) + 1]] <- qa
      }
    }
  }

  list(
    age = bind_threshold_chunks(age_chunks, empty_age()),
    reading = bind_threshold_chunks(reading_chunks, empty_reading_thresholds()),
    eccentricityDeg = bind_threshold_chunks(eccentricity_chunks, empty_eccentricity()),
    targetDurationSecs = bind_threshold_chunks(duration_chunks, empty_target_duration()),
    viewingdistance = bind_threshold_chunks(viewing_chunks, empty_viewing_distance()),
    reading_questions = reading_q_chunks,
    fluency = bind_threshold_chunks(fluency_chunks, empty_fluency()),
    QA = bind_threshold_chunks(qa_chunks, empty_qa())
  )
}

collect_threshold_summary_list_inputs <- function(summary_list) {
  if (length(summary_list) == 0) {
    return(tibble())
  }
  chunks <- vector("list", length(summary_list))
  for (i in seq_along(summary_list)) {
    s <- summary_list[[i]]
    if (is.null(s) || !is.data.frame(s) || nrow(s) < 1) {
      next
    }
    chunks[[i]] <- s %>% mutate(order = i)
  }
  bind_threshold_chunks(chunks)
}

apply_reading_accuracy_from_chunks <- function(reading, reading_q_chunks, nQs) {
  reading_accuracy <- tibble()
  if (is.na(nQs) || nQs <= 0 || length(reading_q_chunks) == 0) {
    return(reading)
  }
  for (readingQuestions in reading_q_chunks) {
    if (nrow(readingQuestions) == 0) {
      next
    }
    n_blocks <- nrow(readingQuestions) / nQs
    r <- reading %>% filter(participant == readingQuestions$participant[1])
    blocks <- unique(r$block_condition)[1:n_blocks]
    readingQuestions <- cbind(readingQuestions, tibble(block_condition = rep(blocks, each = nQs)))
    reading_accuracy <- rbind(reading_accuracy, readingQuestions)
  }
  if (nrow(reading_accuracy) > 0) {
    reading_accuracy <- reading_accuracy %>%
      group_by(participant, block_condition) %>%
      summarize(accuracy = mean(readWordIdentifiedBool),
                .groups = "drop")
    reading <- reading %>% left_join(reading_accuracy, by = c("participant", "block_condition"))
    reading <- reading %>% mutate(accuracy = factor(accuracy, levels = c(0, 0.2, 0.4, 0.6, 0.8, 1)))
  }
  reading
}

generate_threshold <- 
  function(data_list, summary_list, df, pretest, stairs, prolific, filterInput, skillFilter, minNQuestTrials, 
           minWrongTrials, maxQuestSD, conditionNameInput, maxReadingSpeed, minCQAccuracy,
           sessions_summary = NULL, shortRulerParticipantIDs = NULL) {
    
    log_info("generate_threshold: data_list=", length(data_list), " summary_list=", length(summary_list))
    if (is.null(data_list)) {
      return(list())
    }
    if (length(data_list) == 0) {
      return(list())
    }

    extracted <- collect_threshold_data_list_inputs(data_list, length(summary_list))

    #### age ####
    age <- extracted$age

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
    
    reading <- extracted$reading
    if (!"wordPerMin" %in% names(reading)) {
      reading <- empty_reading_thresholds()
    }
    # For italian data, reading OMT_words read as reading speed
    
    if (nrow(reading) == 0 & 'OMT_words read' %in% names(pretest)) {
      omt_raw <- pretest[["OMT_words read"]]
      message(
        "OMT_words read before as.numeric: class=",
        paste(class(omt_raw), collapse = "/"),
        ", n=", length(omt_raw),
        ", non-empty=", sum(!is.na(omt_raw) & nzchar(trimws(as.character(omt_raw))))
      )
      print(omt_raw)
      reading <- pretest %>% 
        select(participant, `OMT_words read`) %>% 
        mutate(`OMT_words read` = as.numeric(`OMT_words read`)) %>% 
        filter(!is.na(`OMT_words read`)) %>% 
        mutate(experiment = '',
               date = '',
               block_condition = '',
               conditionName = 'Reading from pretest.xlsx',
               font = '',
               targetKind = 'reading',
               thresholdParameter = '',
               readingNumberOfQuestions = NA,
               trial = 1,
               log_WPM = log10(`OMT_words read`)) %>% 
        rename(wordPerMin = `OMT_words read`)
    }
    log_debug("reading from pretest.xlsx: ", nrow(reading))
    if ("wordPerMin" %in% names(reading)) {
      if (!"log_WPM" %in% names(reading)) {
        reading$log_WPM <- NA_real_
      }
      reading <- reading %>%
        mutate(
          wordPerMin = suppressWarnings(as.numeric(wordPerMin)),
          log_WPM = suppressWarnings(as.numeric(log_WPM)),
          log_WPM = ifelse(is.na(log_WPM) & !is.na(wordPerMin), log10(wordPerMin), log_WPM)
        ) %>%
        filter(!is.na(wordPerMin), is.finite(wordPerMin), wordPerMin <= maxReadingSpeed)
    }
    
    
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
    
    all_summary <- collect_threshold_summary_list_inputs(summary_list)
    
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
      left_join(NQuestTrials, by = c('participant', 'block_condition'), relationship = 'many-to-many') %>% 
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
    
    
    
    if (nrow(pretest) > 0 & 'OMT_words read' %in% names(pretest)) {
      pretest <- pretest %>% mutate(wordPerMin = as.numeric(`OMT_words read`))
      threshold <- quantile(pretest$wordPerMin, 0.25, na.rm = T)
      slowest = pretest %>% filter(wordPerMin <= threshold)
    }
    
    if (filterInput == 'slowest' & nrow(slowest) > 0) {
      reading <- reading %>% filter(tolower(participant) %in% tolower(slowest$participant))
      all_summary <- all_summary %>%
        filter(tolower(participant) %in% tolower(slowest$participant))
    } 
    if (filterInput == 'fastest' & nrow(slowest) > 0) {
      reading <- reading%>% filter(!tolower(participant) %in% tolower(slowest$participant))
      all_summary <- all_summary %>%
        filter(!tolower(participant) %in% tolower(slowest$participant))
    }
    
    
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
    
    eccentricityDeg <- extracted$eccentricityDeg
    if (nrow(eccentricityDeg) > 0) {
      eccentricityDeg <- eccentricityDeg %>% 
        filter(!is.na(targetEccentricityXDeg),
               !is.na(targetEccentricityYDeg)) %>% 
        mutate(targetEccentricityXDeg = as.numeric(targetEccentricityXDeg),
               targetEccentricityYDeg = as.numeric(targetEccentricityYDeg))
    }
    
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
    
    
    targetDurationSecs <- extracted$targetDurationSecs
    
    if (nrow(age) == 0) {
      age <- tibble(participant = character(), age = numeric())
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
      left_join(targetDurationSecs, by = c('participant', 'conditionName'), relationship = 'many-to-many') %>%
      mutate(targetDurationSec = suppressWarnings(as.numeric(targetDurationSec)))
    
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
      select(experiment, participant, conditionName, questMeanAtEndOfTrialsLoop, questSDAtEndOfTrialsLoop,
             font, Grade, age, `Skilled reader?`, ParticipantCode, targetDurationSec) %>%
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
    
    viewingdistance <- extracted$viewingdistance
    
    rsvp_speed <- rsvp_speed %>% 
      left_join(viewingdistance, by = c("conditionName", "participant"), relationship = 'many-to-many')
    
    nQs <- if ("readingNumberOfQuestions" %in% names(reading) && nrow(reading) > 0) {
      as.numeric(reading$readingNumberOfQuestions[1])
    } else {
      NA_real_
    }
    
    ################################ READING RETENTION #######################################
    
    reading <- apply_reading_accuracy_from_chunks(reading, extracted$reading_questions, nQs)
    
    fluency <- extracted$fluency
    if (nrow(fluency) > 0) {
      fluency <- fluency %>% 
        group_by(participant) %>% 
        summarize(accuracy = mean(questionAndAnswerResponse == questionAndAnswerCorrectAnswer),
                  .groups="drop")
    } else {
      fluency <- tibble(participant = character(), accuracy = numeric())
    }
    
    if ('Grade' %in% names(pretest)) {
      age <- age %>%
        mutate(lowerCaseParticipant = tolower(participant)) %>% 
        left_join(select(pretest, Grade, lowerCaseParticipant), by = 'lowerCaseParticipant')
    } else {
      # Use mutate instead of direct assignment to handle empty tibbles
      age <- age %>% mutate(Grade = NA_real_)
    }
    
    log_info("Threshold data: quest=", nrow(quest), " reading=", nrow(reading),
             " crowding=", nrow(crowding), " rsvp=", nrow(rsvp_speed),
             " acuity=", nrow(acuity), " repeatedLetters=", nrow(repeatedLetters),
             " age=", nrow(age))
    
    
    
    #### beauty and comfort ####

    QA <- extracted$QA
    if (!"participant" %in% names(QA)) {
      QA <- empty_qa()
    }
    if (nrow(QA) > 0) {
      QA <- QA %>%
        filter(!blockShuffleGroups2=="readin5") %>% 
        arrange(experiment, participant, block, block_condition)
    }
    
    # write.csv(QA %>% filter(questionAndAnswerCorrectAnswer != "",
    #                                   !is.na(questionAndAnswerCorrectAnswer)),
    #           'QA.csv')
    #### short ruler filter (IDs computed once upstream from summary_table) ####
    if (is.null(sessions_summary)) {
      sessions_summary <- generate_summary_table(data_list, stairs, pretest, prolific)
    }
    if (is.null(shortRulerParticipantIDs)) {
      shortRulerParticipantIDs <- character()
    }
    shortRuler <- tibble(PavloviaParticipantID = shortRulerParticipantIDs)
    
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
    
    short_ruler_ids <- shortRuler$PavloviaParticipantID
    reading <- filter_out_short_ruler(reading, short_ruler_ids)
    crowding <- filter_out_short_ruler(crowding, short_ruler_ids)
    rsvp <- filter_out_short_ruler(rsvp_speed, short_ruler_ids)
    # fluency <- filter_out_short_ruler(fluency, short_ruler_ids)
    acuity <- filter_out_short_ruler(acuity, short_ruler_ids)
    repeatedLetters <- filter_out_short_ruler(repeatedLetters, short_ruler_ids)
    quest <- filter_out_short_ruler(quest, short_ruler_ids)
    quest_all_thresholds <- filter_out_short_ruler(quest_all_thresholds, short_ruler_ids)
    age <- filter_out_short_ruler(age, short_ruler_ids)
    # threshold <- filter_out_short_ruler(threshold, short_ruler_ids)
    # threshold_each <- filter_out_short_ruler(threshold_each, short_ruler_ids)
    all_summary <- filter_out_short_ruler(all_summary, short_ruler_ids)
    QA <- filter_out_short_ruler(QA, short_ruler_ids)
    
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
    
    # Append ratings rows to all_summary so they flow into threshold_all/threshold_summary
    # Mapping rule:
    # - questionAndAnswerResponse -> questMeanAtEndOfTrialsLoop
    # - targetKind = "Ratings"
    # - thresholdParameter = "Comfort" / "Beauty" / "Familiarity"
    # - if conditionName == "", use questionAndAnswerNickname
    ratings_for_all_summary <- bind_rows(
      comfort %>% mutate(thresholdParameter = "Comfort"),
      beauty %>% mutate(thresholdParameter = "Beauty"),
      familiarity %>% mutate(thresholdParameter = "Familiarity")
    ) %>%
      mutate(
        conditionName = ifelse(is.na(conditionName) | conditionName == "", questionAndAnswerNickname, conditionName),
        questMeanAtEndOfTrialsLoop = as.numeric(questionAndAnswerResponse),
        targetKind = "Ratings",
        questSDAtEndOfTrialsLoop = NA_real_
      ) %>%
      # Keep ratings aligned with currently valid participants in all_summary
      filter(participant %in% all_summary$participant)
    
    # Ensure ratings_for_all_summary has the same schema as all_summary
    missing_cols <- setdiff(names(all_summary), names(ratings_for_all_summary))
    if (length(missing_cols) > 0) {
      for (col_name in missing_cols) {
        ratings_for_all_summary <- ratings_for_all_summary %>% mutate(!!col_name := NA)
      }
    }
    extra_cols <- setdiff(names(ratings_for_all_summary), names(all_summary))
    if (length(extra_cols) > 0) {
      ratings_for_all_summary <- ratings_for_all_summary %>% select(-all_of(extra_cols))
    }
    ratings_for_all_summary <- ratings_for_all_summary %>% select(all_of(names(all_summary)))
    
    if (nrow(ratings_for_all_summary) > 0) {
      all_summary <- bind_rows(all_summary, ratings_for_all_summary)
    }
    
    
    
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
    if (nrow(QA) > 0 && "correct" %in% names(QA)) {
      comprehension_ac <- QA %>%
        group_by(experiment, participant, block) %>%
        summarize(CQAccuracy = mean(correct * 100, na.rm = T), .groups = "drop",
                  Nquestions = sum(!is.na(correct))) %>%
        mutate(block = as.numeric(block) - 1)

      reading_pre <- reading %>%
        mutate(block = ifelse(length(str_split(block_condition, "_")) == 0,
                              NA,
                              as.numeric(str_split(block_condition, "_")[[1]][1]))) %>%
        left_join(comprehension_ac, by = c("experiment", "participant", "block"))

      reading <- reading_pre %>%
        filter(CQAccuracy >= minCQAccuracy)
    } else {
      reading_pre <- reading %>%
        mutate(CQAccuracy = NA_real_, Nquestions = NA_real_)
    }

    
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
    
    all_summary_ratings <- all_summary %>% filter(targetKind == "Ratings")
    all_summary <- all_summary %>% filter(targetKind != "Ratings" | is.na(targetKind))

    if (nrow(all_summary_ratings) > 0) {
      ratings_wide <- all_summary_ratings %>%
        select(participant, font, thresholdParameter, questMeanAtEndOfTrialsLoop) %>%
        tidyr::pivot_wider(
          names_from = thresholdParameter,
          values_from = questMeanAtEndOfTrialsLoop,
          values_fn = mean
        )
      all_summary <- all_summary %>%
        left_join(ratings_wide, by = c("participant", "font"))
    }
    for (col in c("Comfort", "Beauty", "Familiarity")) {
      if (!col %in% names(all_summary)) {
        all_summary <- all_summary %>% mutate(!!col := NA_real_)
      }
    }

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
             questSDAtEndOfTrialsLoop, TrialsSentToQuest, badTrials,
             Comfort, Beauty, Familiarity)

    log_info("generate_threshold complete")
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
                QA = if ("block" %in% names(QA)) QA %>% select(-block) else QA,
                reading_pre = reading_pre
    ))
  }
