library(dplyr)
library(stringr)
read_files <- function(file_list){
  data_list <- list()
  summary_list <- list()
  n <- length(file_list)
  j = 1
  for (i in 1 : n) {
    t <- tibble()
    try({t <- read.csv(file_list[i], stringsAsFactors=F)}, silent = TRUE)
    if (!('participant' %in% colnames(t))) {
      t <- tibble(participant = "")
      t$error <- "invalid file"
    }
    if (!('ProlificParticipantID' %in% colnames(t))) {
      t$ProlificParticipantID <- ""
    }
    if (!('error' %in% colnames(t))) {
      t$error <- ""
    }
    if (!('warning' %in% colnames(t))) {
      t$warning <- ""
    }
    if ('readingPageWords' %in% colnames(t)) {
      t$wordPerMin <- (t$readingPageWords + t$readingLinesPerPage - 1) / (t$readingPageDurationOnsetToOffsetSec / 60)
    }
    if (!('readingPageWords' %in% colnames(t))) {
      t$wordPerMin <- NA
    }
    if (!('targetFinishSec' %in% colnames(t))) {
      t$targetFinishSec <- NA
    }
    if (!('targetStartSec' %in% colnames(t))) {
      t$targetStartSec <- NA
    }
    if (!('targetMeasuredDurationSec' %in% colnames(t))) {
      t$targetMeasuredDurationSec <- t$targetFinishSec-t$targetStartSec
    }
    if (!('targetMeasuredLatenessSec' %in% colnames(t))) {
      t$targetMeasuredLatenessSec <- NA
    }
    if (!('questMeanAtEndOfTrialsLoop' %in% colnames(t))) {
      t$questMeanAtEndOfTrialsLoop <- NA
    }
    if (!('screenHeightPx' %in% colnames(t))) {
      t$screenHeightPx <- NA
    }
    if (!('screenWidthPx' %in% colnames(t))) {
      t$screenWidthPx <- NA
    }
    if (!('deviceBrowser' %in% colnames(t))) {
      t$deviceBrowser <- ""
    }
    if (!('deviceBrowserVersion' %in% colnames(t))) {
      t$deviceBrowserVersion <- ""
    }
    if (!('deviceType' %in% colnames(t))) {
      t$deviceType <- ""
    }
    if (!('deviceSystemFamily' %in% colnames(t))) {
      t$deviceSystemFamily <- ""
    }
    if (!('deviceLanguage' %in% colnames(t))) {
      t$deviceLanguage <- ""
    }
    if (!('deviceSystem' %in% colnames(t))) {
      t$deviceSystem <- ""
    }
    if (!('hardwareConcurrency' %in% colnames(t))) {
      t$hardwareConcurrency <- NA
    }
    if (!('block' %in% colnames(t))) {
      t$block <- NA
    }
    if (!('conditionName' %in% colnames(t))) {
      t$conditionName <- ""
    }
    if (!('block_condition' %in% colnames(t))) {
      t$block_condition <- ''
    }
    if (!('staircaseName' %in% colnames(t))) {
      t$staircaseName <- NA
    }
    if (!('font' %in% colnames(t))) {
      t$font <- ""
    }
    if (!('targetTask' %in% colnames(t))) {
      t$targetTask <- ""
    }
    if (!('targetKind' %in% colnames(t))) {
      t$targetKind <- ""
    }
    if (!('thresholdParameter' %in% colnames(t))) {
      t$thresholdParameter <- ""
    }
    if (!('psychojsWindowDimensions' %in% colnames(t))) {
      t$psychojsWindowDimensions <- NA
    }
    screenWidth <- ifelse(length(unique(t$screenWidthPx)) > 1,
                          unique(t$screenWidthPx)[!is.na(unique(t$screenWidthPx))] , 
                          NA)
    screenHeight <- ifelse(length(unique(t$screenHeightPx)) > 1,
                           unique(t$screenHeightPx)[!is.na(unique(t$screenHeightPx))] , 
                           NA)
    t <- t %>% mutate(screenWidthPx = screenWidth,
                      screenHeightPx = screenHeight,
                      browser = ifelse(deviceBrowser == "", "", paste0(deviceBrowser, 
                                                                       " ", 
                                                                       str_split(deviceBrowserVersion, "[.]")[[1]][1])),
                      resolution = paste0(screenWidthPx, " x ", screenHeightPx),
                      block_condition = ifelse(block_condition == "",staircaseName, block_condition))
    t$system = str_replace_all(t$deviceSystem, "OS X","macOS")
    psychojsWindowDimensions <- lapply(strsplit(t$psychojsWindowDimensions[1],","), parse_number)[1]
    WindowDimensions <- paste0(psychojsWindowDimensions[1], " x ", psychojsWindowDimensions[2])
    t$resolution = ifelse(t$resolution[1] == "NA x NA", WindowDimensions, t$resolution)
    info <- filter(t, is.na(questMeanAtEndOfTrialsLoop)) %>% select(block_condition, conditionName) %>% distinct(block_condition, conditionName)
    t <- t %>% rename("cores" = "hardwareConcurrency")
    
    info <- t %>% 
      filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
      distinct(participant, block, staircaseName, conditionName, targetKind, font)
    
    summaries <- t %>% 
      filter(!is.na(questMeanAtEndOfTrialsLoop)) %>% 
      select(
        staircaseName, 
        thresholdParameter,
        questMeanAtEndOfTrialsLoop)
    summaries <- merge(info, summaries, by = ("staircaseName"))
    summary_list[[j]] <- summaries
    data_list[[j]] <- t
    j = j + 1
  }
  return(list(data_list, summary_list))
}
