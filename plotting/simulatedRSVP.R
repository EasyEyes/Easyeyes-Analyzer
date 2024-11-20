

library(dplyr)
library(stringr)
library(ggplot2)

# ------------------------------------------------------------------------------
#####                               Load Function                           ####
# ------------------------------------------------------------------------------
is.contained = function(vec1, vec2) {
  x = vector(length = length(vec1))
  for (i in 1:length(vec1)) {
    x[i] = vec1[i] %in% vec2
    if (length(which(vec1[i] %in% vec2)) == 0)
      vec2
    else
      vec2 = vec2[-match(vec1[i], vec2)]
  }
  y = all(x == T)
  return(y)
}

extractRSVPStaircases <- function(df) {
  stairdf <- df %>%
    filter(!is.na(staircaseName) &
             (
               str_detect(conditionName, "rsvp") |
                 str_detect(conditionName, "RSVP")
             )) %>%
    select(
      "participant",
      "ProlificParticipantID",
      "conditionName",
      "staircaseName",
      "questMeanBeforeThisTrialResponse",
      "trialGivenToQuest",
      "rsvpReadingResponseCorrectBool",
      "levelProposedByQUEST",
      "rsvpReadingWordDurationSec",
      "simulationThreshold",
      "font"
    )
}

extractRSVP <- function(df) {
  rsvpdf <- df %>%
    filter(!is.na(questMeanAtEndOfTrialsLoop) &
             (
               grepl('rsvp', conditionName, fixed = TRUE) |
                 grepl('RSVP', conditionName, fixed = TRUE)
             )) %>%
    select(
      "participant",
      "ProlificParticipantID",
      "conditionName",
      "questMeanAtEndOfTrialsLoop",
      "questSDAtEndOfTrialsLoop",
      "simulationThreshold"
    ) %>%
    mutate(rsvpReadingSpeed = 10 ^ (log10(60) - questMeanAtEndOfTrialsLoop)) %>%
    rename(rsvpQuestSD = questSDAtEndOfTrialsLoop) %>%
    #separate(conditionName, into = c("meridian","font"), sep = "(?<=Left|Right)") %>%
    mutate(font = str_remove(as.character(conditionName), "rsvp")) %>%
    select(-conditionName)
  return(rsvpdf)
  
}

# getStairsPlot <- function(file) {
#   file_list <- file$data
#   if (length(file_list) == 1 & grepl(".csv", file_list[1])) {
#     print('read csv')
#     try({allData <- readr::read_csv(file_list[1])}, silent = TRUE)
#     } else {
#     shouldRender = F
#     return(list(ggplot(), ggplot(),ggplot(),F))
#
#   } else if (length(file_list) == 1 & grepl(".zip", file_list[1])) {
#     print('read zip')
#     file_names <- unzip(file_list[1], list = TRUE)$Name
#     all_csv <- file_names[grepl(".csv", file_names)]
#     all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
#     if (length(all_csv) <= 2) {
#       try({allData <- readr::read_csv(unzip(file_list[1], all_csv[1]),show_col_types = FALSE)}, silent = TRUE)
#       if ('Submission id' %in% names(allData)) {
#         try({allData <- readr::read_csv(unzip(file_list[1], all_csv[2]),show_col_types = FALSE)}, silent = TRUE)
#       }
#     }
#   }
#
#
#   if (!is.contained(c("participant", "ProlificParticipantID", "conditionName",
#                       "staircaseName", "questMeanBeforeThisTrialResponse", "trialGivenToQuest", "rsvpReadingResponseCorrectBool",
#                       "levelProposedByQUEST", "rsvpReadingWordDurationSec", "simulationThreshold"),
#                     names(allData))){
#     return(list=NULL, NULL, NULL)
#   }
#   if (!is.contained(c("participant","ProlificParticipantID","conditionName",
#                       "questMeanAtEndOfTrialsLoop",
#                       "questSDAtEndOfTrialsLoop", "simulationThreshold"),
#                     names(allData))){
#     shouldRender = F
#     return(list(ggplot(), ggplot(), ggplot(),F))
#   }
#   rsvpstairdf <- extractRSVPStaircases(allData)
#   rsvpdf <- extractRSVP(allData)
#
#
#   rsvpdf$simulationThreshold <- unique(allData$simulationThreshold)[!is.na(unique(allData$simulationThreshold))]
#
#   N = length(unique(allData$participant))
#
#   rsvpStair <- rsvpstairdf %>%
#     filter(!is.na(questMeanBeforeThisTrialResponse)) %>%
#     select(-conditionName) %>%
#     arrange(participant, font) %>%
#     group_by(staircaseName) %>%
#     mutate(trialN = row_number()) %>%
#     separate(rsvpReadingResponseCorrectBool,
#              into = c("w1", "w2", "w3"), sep = ",") %>%
#     mutate(across(w1:w3, as.logical)) %>%
#     mutate(allCorrect = ifelse(w1+w2+w3 == 3, T, F))
#
#   PCdf <- rsvpStair %>%
#     group_by(simulationThreshold) %>%
#     summarize(PC = (sum(w1) + sum(w2) + sum(w3))/150)
#
#   rsvpdf <- rsvpdf %>% left_join(PCdf, by = "simulationThreshold")
#
#
#
#
#   lpq <- rsvpStair %>%
#     #filter(font == ft) %>%
#     ggplot() +
#     geom_point(aes(x = trialN, y = levelProposedByQUEST), size = 0.5,)+
#     geom_line(aes(x = trialN, y = levelProposedByQUEST))+
#     facet_wrap(.~participant)+
#     theme_classic()+
#     labs(title = "levelProposedByQUEST")+
#     #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2,
#     #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1,
#     #             color = "tomato", alpha = 0.3, linetype = "solid")+
#     facet_wrap(.~simulationThreshold)+
#     theme(plot.title = element_text(hjust = 0.5))
#
#   qmbtr <- rsvpStair %>%
#     #filter(font == ft) %>%
#     ggplot() +
#     geom_point(aes(x = trialN, y = questMeanBeforeThisTrialResponse), size = 0.5)+
#     geom_line(aes(x = trialN, y = questMeanBeforeThisTrialResponse))+
#     facet_wrap(.~participant)+
#     labs(title = "questMeanBeforeThisTrialResponse")+
#     theme_classic()+
#     #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2,
#     #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1,
#     #             color = "tomato", alpha = 0.3, linetype = "solid")+
#     facet_wrap(.~simulationThreshold)+
#     theme(plot.title = element_text(hjust = 0.5))
#
#   rsvpdur <- rsvpStair %>%
#     #filter(font == ft) %>%
#     ggplot() +
#     geom_point(aes(x = trialN, y = rsvpReadingWordDurationSec), size = 0.5)+
#     geom_line(aes(x = trialN, y = rsvpReadingWordDurationSec))+
#     facet_wrap(.~participant)+
#     theme_classic()+
#     labs(title = "rsvpReadingWordDurationSec")+
#     #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2,
#     #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1,
#     #             color = "tomato", alpha = 0.3, linetype = "solid")+
#     facet_wrap(.~simulationThreshold)+
#     theme(plot.title = element_text(hjust = 0.5))
#
#   g1 <- list(lpq, qmbtr, rsvpdur)
#
#
#   g2 <- ggplot(rsvpstairdf, aes(x = rsvpReadingWordDurationSec, y = 10^(levelProposedByQUEST)))+
#     geom_point()+
#     theme_classic(base_size = 16)+
#     coord_fixed(ratio = 1)+
#     geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")
#   #scale_x_log10()+
#   #annotation_logticks()
#
#
#   g3 <- ggplot(rsvpdf, aes(x = PC, y = rsvpQuestSD))+
#     geom_point()+
#     theme_classic(base_size = 16)
#   return(list(
#     g1,
#     g2,
#     g3,
#     shouldRender
#   ))
# }


extractCrowdingStaircases <- function(df, info) {
  if (!'levelProposedByQUEST' %in% names(df)) {
    df$levelProposedByQUEST = NA
  }
  
  if (!'trialGivenToQuest' %in% names(df)) {
    df$trialGivenToQuest = NA
  }
  
  stairdf <- df %>%
    filter(!is.na(staircaseName)) %>%
    select(
      "staircaseName",
      "levelProposedByQUEST",
      "trialGivenToQuest"
    ) %>% 
    inner_join(info, by = 'staircaseName') %>% 
    mutate(questType = case_when(
      thresholdParameter != "targetSizeDeg" &
        thresholdParameter != 'size' &
        targetKind == "letter" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Crowding',
      targetKind == "rsvpReading" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'RSVP reading',
      thresholdParameter != "targetSizeDeg" &
        thresholdParameter != 'size' &
        targetKind == "repeatedLetters" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Repeated letters',
      (thresholdParameter == "targetSizeDeg" | thresholdParameter == 'size') &
        targetKind == "letter" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Acuity',
      grepl("practice",conditionName, ignore.case = T) ~ 'practice',
      .default = 'unknown'
    )) %>% 
    filter(questType != 'practice')
  # "questMeanBeforeThisTrialResponse", "", "targetMeasuredLatenessSec",
  # "targetMeasuredDurationSec", "targetDurationSec", "key_resp.corr", "level", "heightPx", "targetDurationSec", "markingOffsetBeforeTargetOnsetSecs")#, "targetSpacingPx")
}

plotStaircases <- function(Staircases, thresholdParameterSelected) {
    stairdf <- Staircases %>%
      drop_na(levelProposedByQUEST)

    if (is.null(thresholdParameterSelected) | nrow(stairdf) == 0) {
      return(NULL)
    }
    
    t <- stairdf %>%
      # Order since trials were randomized
      arrange(participant, staircaseName) %>%
      filter(thresholdParameter == thresholdParameterSelected) %>%
      group_by(participant, staircaseName) %>%
      mutate(trial = row_number(),
             questTrials = paste0(sum(trialGivenToQuest,na.rm = T), ' questTrials'))
    
    height = n_distinct(t %>% select(participant,thresholdParameter, conditionName)) * 1.3 + 2

    p <- ggplot(t, aes(x = trial, y = levelProposedByQUEST)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      # facet_grid(rows = vars(thresholdParameter),
      #            cols = vars(conditionName))
      facet_wrap(~conditionName + participant + thresholdParameter + questTrials, ncol=2, scales = 'free') +
      scale_x_continuous(breaks = scales::breaks_width(5)) +
      theme(
        axis.ticks = element_line(),
        axis.ticks.length = unit(0.3, "line"),
        strip.text.x = element_text(hjust = 1),
        plot.background = element_rect(fill = "white", colour = 'white'),
        strip.background =  element_rect(fill = "white", colour = 'white'),
        panel.background = element_rect(fill = "white", colour = 'white'))
      # scale_y_continuous(limits=c(-2,0))
    return(list(plot = p,
                height = height))
}

plotCrowdingStaircasesVsQuestTrials <- function(crowding, Staircases){
  stairdf <- Staircases %>%
    drop_na(levelProposedByQUEST)
  
  if (nrow(stairdf) == 0) {
    return(NULL)
  }
  crowdingQuest <- Staircases %>%
    filter(questType == 'Crowding') %>% 
    group_by(participant, staircaseName) %>%
    mutate(questTrials = sum(trialGivenToQuest,na.rm = T),
           block_condition = staircaseName) %>% 
    distinct(participant, block_condition, questTrials)

  crowding <- crowding %>% 
    left_join(crowdingQuest, by = c('participant', 'block_condition'))

  ggplot(crowding, aes(x = questTrials, y = 10^(log_crowding_distance_deg))) + 
    geom_point() + 
    scale_y_log10(breaks = c(0.1,0.3,1,3,10)) + 
    annotation_logticks(sides = 'l') + 
    scale_x_continuous()+ 
    labs(y = 'Crowding distance (deg)')
}
