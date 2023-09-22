require(dplyr)
require(readr)
require(ggplot2)
require(tidyr)
require(stringr)
require(gridExtra)
require(ggsignif)
require(DT)
# store name of experiment in experiment object
rm(list = ls())
experiment = "sound calibration"
# locate the folder that you store experiments
setwd("~/Downloads")
# get the folder name for your experiment
folders <- dir(pattern = glob2rx(paste0(experiment, "*")))
folders <- folders[file.info(folders)$isdir]
# set the experiment folder as working directory
setwd(paste0("~/Downloads/", folders))
# get all the files end with .csv in the folder
file_names <- list.files(pattern = "*.csv")
# use the length() function n number of participants in the experiment
n = length(file_names)
data_list = list()
summary_list = list()
j = 1

for (i in 1 : n){
  t <- tibble()
  try({t <- read_csv(file_names[i])}, silent = TRUE)
  if (!('participant' %in% colnames(t))) {
    t <- tibble(participant = str_split(file_names[i], "[_]")[[1]][1],
                ProlificParticipantID = str_split(file_names[i], "[_]")[[1]][2],
                experimentName = str_split(file_names[i], "[_]")[[1]][3])
    t$error <- "Incomplete"
    t$rows <- 0
    t$cols <- 0
  } else {
    t$cols <- ncol(t)
    t$rows <- nrow(t)
  }
  
  if (!('ProlificParticipantID' %in% colnames(t))) {
    t$ProlificParticipantID <- ""
  }
  if (!('error' %in% colnames(t))) {
    t$error <- ""
  }
  if (!('questionAndAnswerResponse' %in% colnames(t))) {
    t$questionAndAnswerResponse <- ""
  }
  if (!('warning' %in% colnames(t))) {
    t$warning <- ""
  }
  if (!('readingCorpus' %in% colnames(t))) {
    t$readingCorpus <- ""
  }
  if ('readingPageWords' %in% colnames(t)) {
    t$wordPerMin <- (as.numeric(t$readingPageWords) + as.numeric(t$readingLinesPerPage) - 1) / (as.numeric(t$readingPageDurationOnsetToOffsetSec) / 60)
  }
  if (!('viewingDistanceDesiredCm' %in% colnames(t))) {
    t$viewingDistanceDesiredCm <- NA
  }
  if (!('readingPageWords' %in% colnames(t))) {
    t$wordPerMin <- NA
  }
  if (!('readingNumberOfQuestions' %in% colnames(t))) {
    t$readingNumberOfQuestions <- NA
  }
  if (!('key_resp.keys' %in% colnames(t))) {
    t$key_resp.keys <- NA
  }
  if (!('correctAns' %in% colnames(t))) {
    t$correctAns <- NA
  }
  if (!('readWordIdentifiedBool' %in% colnames(t))) {
    t$readWordIdentifiedBool <- NA
  }
  if (!('targetEccentricityXDeg' %in% colnames(t))) {
    t$targetEccentricityXDeg <- NA
  }
  if (!('targetEccentricityYDeg' %in% colnames(t))) {
    t$targetEccentricityYDeg <- NA
  }
  if (!('targetFinishSec' %in% colnames(t))) {
    t$targetFinishSec <- NA
  }
  if (!('targetStartSec' %in% colnames(t))) {
    t$targetStartSec <- NA
  }
  if (!('targetDurationSec' %in% colnames(t))) {
    t$targetDurationSec <- NA
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
  if (!('date' %in% colnames(t))) {
    t$date <- ""
  }
  if (!('hardwareConcurrency' %in% colnames(t))) {
    t$hardwareConcurrency <- NA
  }
  if (!('experiment' %in% colnames(t))) {
    t$experiment <- str_split(file$name[i], "[_]")[[1]][3]
  }
  if (!('experimentCompleteBool' %in% colnames(t))) {
    t$experimentCompleteBool <- FALSE
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
    t$psychojsWindowDimensions <- "NA,NA"
  }
  t$age <- t$questionAndAnswerResponse[2]
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
  t$deviceSystemFamily = str_replace_all(t$deviceSystemFamily, "OS X","macOS")
  psychojsWindowDimensions <- lapply(strsplit(t$psychojsWindowDimensions[1],","), parse_number)[1]
  WindowDimensions <- paste0(psychojsWindowDimensions[[1]][1], " x ", psychojsWindowDimensions[[1]][2])
  t$resolution = ifelse(t$resolution[1] == "NA x NA", WindowDimensions, t$resolution)
  t$resolution = ifelse(t$resolution[1] == "NA x NA", "", t$resolution)
  info <- dplyr::filter(t, is.na(questMeanAtEndOfTrialsLoop)) %>% 
    select(block_condition, conditionName) %>% 
    distinct(block_condition, conditionName) %>% 
    dplyr::filter(conditionName != "" & block_condition != "")
  t <- t %>% 
    rename("cores" = "hardwareConcurrency")
  
  info <- t %>% 
    dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
    distinct(participant, block_condition, staircaseName, conditionName, 
             targetKind, font, experiment)
  
  summaries <- t %>% 
    dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>% 
    select(
      block_condition,
      staircaseName, 
      thresholdParameter,
      questMeanAtEndOfTrialsLoop
    )
  if(n_distinct(summaries$staircaseName) < n_distinct(summaries$block_condition)) {
    summaries <- summaries %>% 
      select(-staircaseName) %>% 
      left_join(info, by = "block_condition")
  } else {
    summaries <- summaries %>% 
      select(-block_condition)
    summaries <- merge(info, summaries, by = ("staircaseName"))
  }
  summary_list[[j]] <- summaries
  data_list[[j]] <- t
  j = j + 1
}

generate_threshold(data_list, summary_list)
# test summary table
# summary_df <- generate_summary_table(data_list)
# datatable(summary_df)
rsvp_speed <- generate_rsvp_reading_crowding_fluency(data_list,summary_list)[[3]]
plot_rsvp_vs_x_height(rsvp_speed)[[1]]
plot_rsvp_vs_x_height(rsvp_speed)[[2]]
## test sound calibration
sound_list <- preprocess_sound_data(data_list)
all_sound_data <- get_all_sound_data(sound_list)
micro_cali_result <- all_sound_data[[7]] %>% filter(id == 4)
IR <- all_sound_data[[9]][[1]] %>% filter(id == 1)
plot_IR_response_0to6(IR)
plot_impulse_response(micro_cali_result)


## test RSVP plots