
library(dplyr)
library(stringr)
library(ggplot2)
library(patchwork)
# ------------------------------------------------------------------------------
#####                               Load Function                           ####
# ------------------------------------------------------------------------------
is.contained=function(vec1,vec2){
  x=vector(length = length(vec1))
  for (i in 1:length(vec1)) {
    x[i] = vec1[i] %in% vec2
    if(length(which(vec1[i] %in% vec2)) == 0) vec2 else 
      vec2=vec2[-match(vec1[i], vec2)]
  }
  y=all(x==T)
  return(y)
}

extractRSVPStaircases <- function(df){
  stairdf <- df %>%
    filter(!is.na(staircaseName) & str_detect(conditionName, "rsvp")) %>%
    select("participant", "ProlificParticipantID", "conditionName", 
           "staircaseName", "questMeanBeforeThisTrialResponse", "trialGivenToQuest", "rsvpReadingResponseCorrectBool", 
           "levelProposedByQUEST", "rsvpReadingWordDurationSec", "simulationThreshold")#, "targetSpacingPx")
}

extractRSVP <- function(df){
  rsvpdf <- df %>%
    filter(!is.na(questMeanAtEndOfTrialsLoop) & 
             startsWith(conditionName, "rsvp")) %>%
    select("participant","ProlificParticipantID","conditionName", 
           "questMeanAtEndOfTrialsLoop",
           "questSDAtEndOfTrialsLoop", "simulationThreshold") %>%
    mutate(rsvpReadingSpeed = 10^(log10(60) - questMeanAtEndOfTrialsLoop)) %>% 
    rename(rsvpQuestSD = questSDAtEndOfTrialsLoop) %>%
    #separate(conditionName, into = c("meridian","font"), sep = "(?<=Left|Right)") %>%
    mutate(font = str_remove(as.character(conditionName), "rsvp")) %>%
    select(-conditionName)
  return(rsvpdf)
  
}

# allData <- read.csv("simulatedData.csv")

getStairsPlot <- function(file) {
  file_list <- file$data
  if (length(file_list) == 1 & grepl(".csv", file_list[1])) {
    try({allData <- readr::read_csv(file_list[i])}, silent = TRUE)
  } else if (length(file_list) == 1 & grepl(".zip", file_list[1])) {
    file_names <- unzip(file_list[k], list = TRUE)$Name
    all_csv <- file_names[grepl(".csv", file_names)]
    all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
    if (length(all_csv) <= 2) {
      try({allData <- readr::read_csv(unzip(file_list[1], all_csv[1]),show_col_types = FALSE)}, silent = TRUE)
      if ('Submission id' %in% names(allData)) {
        try({allData <- readr::read_csv(unzip(file_list[1], all_csv[2]),show_col_types = FALSE)}, silent = TRUE)
      }
    }
  } else {
    return(list(ggplot(), ggplot(),ggplot()))
  }
  
  if (!is.contained(c("participant", "ProlificParticipantID", "conditionName", 
                      "staircaseName", "questMeanBeforeThisTrialResponse", "trialGivenToQuest", "rsvpReadingResponseCorrectBool", 
                      "levelProposedByQUEST", "rsvpReadingWordDurationSec", "simulationThreshold"),
                    names(allData))){
    return(list(ggplot(), ggplot(),ggplot()))
  }
  if (!is.contained(c("participant","ProlificParticipantID","conditionName", 
                      "questMeanAtEndOfTrialsLoop",
                      "questSDAtEndOfTrialsLoop", "simulationThreshold"),
                    names(allData))){
    return(list(ggplot(), ggplot(),ggplot()))
  }
  rsvpstairdf <- extractRSVPStaircases(allData)
  rsvpdf <- extractRSVP(allData)
  
  
  rsvpdf$simulationThreshold <- unique(allData$simulationThreshold)[!is.na(unique(allData$simulationThreshold))]
  
  N = length(unique(allData$participant))
  stairLength = 50
  stairPP = 3
  
  rsvpStair <- rsvpstairdf %>%
    filter(!is.na(questMeanBeforeThisTrialResponse)) %>%
    mutate(font = str_remove(conditionName, "rsvp")) %>%
    select(-conditionName) %>%
    arrange(participant, font) %>%
    mutate(trialN = rep(1:stairLength, N*stairPP)) %>%
    separate(rsvpReadingResponseCorrectBool, 
             into = c("w1", "w2", "w3"), sep = ",") %>%
    mutate(across(w1:w3, as.logical)) %>%
    mutate(allCorrect = ifelse(w1+w2+w3 == 3, T, F))
  
  PCdf <- rsvpStair %>%
    group_by(simulationThreshold) %>%
    summarize(PC = (sum(w1) + sum(w2) + sum(w3))/150)
  
  rsvpdf <- rsvpdf %>% left_join(PCdf, by = "simulationThreshold")
  
  
  
  
  lpq <- rsvpStair %>%
    #filter(font == ft) %>%
    ggplot() +
    geom_point(aes(x = trialN, y = levelProposedByQUEST), size = 0.5,)+
    geom_line(aes(x = trialN, y = levelProposedByQUEST))+
    facet_wrap(.~participant)+
    theme_classic()+
    labs(title = "levelProposedByQUEST")+
    #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2, 
    #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1, 
    #             color = "tomato", alpha = 0.3, linetype = "solid")+
    facet_wrap(.~simulationThreshold)+
    theme(plot.title = element_text(hjust = 0.5))
  
  qmbtr <- rsvpStair %>%
    #filter(font == ft) %>%
    ggplot() +
    geom_point(aes(x = trialN, y = questMeanBeforeThisTrialResponse), size = 0.5)+
    geom_line(aes(x = trialN, y = questMeanBeforeThisTrialResponse))+
    facet_wrap(.~participant)+
    theme_classic()+
    labs(title = "questMeanBeforeThisTrialResponse")+
    #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2, 
    #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1, 
    #             color = "tomato", alpha = 0.3, linetype = "solid")+
    facet_wrap(.~simulationThreshold)+
    theme(plot.title = element_text(hjust = 0.5))
  
  rsvpdur <- rsvpStair %>%
    #filter(font == ft) %>%
    ggplot() +
    geom_point(aes(x = trialN, y = rsvpReadingWordDurationSec), size = 0.5)+
    geom_line(aes(x = trialN, y = rsvpReadingWordDurationSec))+
    facet_wrap(.~participant)+
    theme_classic()+
    labs(title = "rsvpReadingWordDurationSec")+
    #geom_segment(aes(x = trialN, xend = trialN, y = as.numeric(!allCorrect) * -2, 
    #                 yend = as.numeric(!allCorrect) * 2), linewidth = 1, 
    #             color = "tomato", alpha = 0.3, linetype = "solid")+
    facet_wrap(.~simulationThreshold)+
    theme(plot.title = element_text(hjust = 0.5))
  
  g1 <- lpq / qmbtr / rsvpdur
  
  
  g2 <- ggplot(rsvpstairdf, aes(x = rsvpReadingWordDurationSec, y = 10^(levelProposedByQUEST)))+
    geom_point()+
    theme_classic(base_size = 12)+
    coord_fixed(ratio = 1)+
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray")
  #scale_x_log10()+
  #annotation_logticks()
  
  
  g3 <- ggplot(rsvpdf, aes(x = PC, y = rsvpQuestSD))+
    geom_point()+
    theme_classic(base_size = 16)
  return(list(
    g1,
    g2,
    g3
  ))
}
