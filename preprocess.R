library(dplyr)
library(stringr)
library(readr)
source('./plotting/simulatedRSVP.R')

read_files <- function(file){
  if(is.null(file)) return(list())
  file_list <- file$data
  file_names <- file$name
  file_list <- file_list[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  file_names <- file_names[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  data_list <- list()
  stair_list <- list()
  summary_list <- list()
  n <- length(file_list)
  experiment <- rep(NA,n)
  readingCorpus <- c()
  j = 1
  pretest <- tibble()
  for (i in 1 : n) {
    t <- tibble()

    if (grepl("pretest.xlsx", file_names[i]) | grepl("pretest.csv", file_names[i])) {
      if (grepl("pretest.xlsx", file_names[i])) {
        pretest <- readxl::read_xlsx(file_list[i], col_types = 'text')
        column_names <- names(pretest)
        date_columns <- grep('date', column_names, ignore.case = TRUE, value = TRUE)
        # If there are any columns with 'date' in the name, reload the file with date columns
        if (length(date_columns) > 0) {
          col_types <- ifelse(column_names %in% date_columns, 'date', 'text')
          pretest <- readxl::read_xlsx(file_list[i], col_types = col_types)
        }
        
      } else {
        pretest <- read_csv(file_list[i])
      }
      
      if ('PavloviaSessionID' %in% names(pretest)) {
        pretest <- pretest %>% 
          rename('participant' = 'PavloviaSessionID') %>% 
          select(where(~sum(!is.na(.)) >0)) %>% 
          mutate(Grade = ifelse(is.na(Grade), -1, Grade))
        if (!'Skilled reader?' %in% names(pretest)) {
          pretest$`Skilled reader?` = 'unknown'
        }
        if (!'ParticipantCode' %in% names(pretest)) {
          pretest$ParticipantCode = pretest$participant
        }
        if ('participantID' %in% names(pretest)) {
          pretest$ParticipantCode = pretest$participantID
        }
        pretest$`Participant ID` = pretest$ParticipantCode
      }
      
      if ('ID' %in% names(pretest)) {
        pretest <- pretest %>% 
          rename('participant' = 'ID') %>% 
          select(where(~sum(!is.na(.)) >0)) %>% 
          mutate(Grade = ifelse(is.na(Grade), -1, Grade))
      }
      if (!'Date of Birth' %in% names(pretest)) {
        pretest$birthDate = NA
      } else {
        pretest <- pretest %>% rename('birthDate' = 'Date of Birth')
      }
      
      if (!'Age' %in% names(pretest)) {
        pretest$Age = NA
      } else {
        pretest$Age <- as.numeric(pretest$Age)
      }
    }
    if (grepl(".csv", file_names[i]) & !grepl("pretest.csv", file_names[i])){ 
      try({t <- readr::read_csv(file_list[i],show_col_types = FALSE)}, silent = TRUE)
      
      
      if (!'Submission id' %in% names(t)){
        inf <- file.info(file_list[i])
        if (!('participant' %in% colnames(t))) {
          t <- tibble(participant = str_split(file$name[i], "[_]")[[1]][1],
                      ProlificParticipantID = '',
                      experiment = '')
          t$error <- "Incomplete"
          t$rows <- 0
          t$cols <- 0
        } else {
          t$cols <- ncol(t)
          t$rows <- ifelse(nrow(t) == 0, 0, nrow(t) + 1)
        }
        t$kb <-round(inf$size/1024)
        
        if (!('ProlificParticipantID' %in% colnames(t))) {
          t$ProlificParticipantID <- ""
        }
        if (!('ProlificSessionID' %in% colnames(t))) {
          t$prolificSessionID <- ''
        } else {
          t$prolificSessionID <-  t$ProlificSessionID
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
        if (!('viewingDistanceDesiredCm' %in% colnames(t))) {
          t$viewingDistanceDesiredCm <- NA
        }
        if (!('readingLinesPerPage' %in% colnames(t))) {
          t$readingLinesPerPage <- NA
        }
        if (!('readingPageDurationOnsetToOffsetSec' %in% colnames(t))) {
          t$readingPageDurationOnsetToOffsetSec <- NA
        }
        if (!('readingPageWords' %in% colnames(t))) {
          t$readingPageWords <- NA
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
        if (!('targetMeasuredLatenessSec' %in% colnames(t))) {
          t$targetMeasuredLatenessSec <- NA
        }
        if (!('questMeanAtEndOfTrialsLoop' %in% colnames(t))) {
          t$questMeanAtEndOfTrialsLoop <- NA
        }
        if (!('questSDAtEndOfTrialsLoop' %in% colnames(t))) {
          t$questSDAtEndOfTrialsLoop <- NA
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
          t$experiment <- ''
        }
        if (!('experimentCompleteBool' %in% colnames(t))) {
          t$experimentCompleteBool <- FALSE
        }
        if (!('block' %in% colnames(t))) {
          t$block <- 0
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
        if (!('_needsUnmet' %in% colnames(t))) {
          t$`_needsUnmet` <- ""
        }
        if (!('Loudspeaker survey' %in% colnames(t))) {
          t$`Loudspeaker survey` <- ""
        }
        if (!('ComputerInfoFrom51Degrees' %in% colnames(t))) {
          t$ComputerInfoFrom51Degrees <- ""
        }
        if (!('Microphone survey' %in% colnames(t))) {
          t$`Microphone survey` <- ""
        }
        if (!('thresholdParameter' %in% colnames(t))) {
          t$thresholdParameter <- ""
        }
        if (!'psychojsWindowDimensions' %in% colnames(t)) {
          t$psychojsWindowDimensions <- "NA,NA"
        }
        if (!('QRConnect' %in% colnames(t))) {
          t$QRConnect <- ''
        }
        if (!('_logFontBool' %in% colnames(t))) {
          t$`_logFontBool` <- FALSE
        }
        if (!('targetMeasuredDurationSec' %in% colnames(t))) {
          t$`targetMeasuredDurationSec` <- NA
        }
        if (!('fontNominalSizePt' %in% colnames(t))) {
          t$`fontNominalSizePt` <- NA
        }
        if (!('fontNominalSizePx' %in% colnames(t))) {
          t$`fontNominalSizePx` <- NA
        }
        if (!('fontMaxPx' %in% colnames(t))) {
          t$`fontMaxPx` <- NA
        }
        if (!('thresholdAllowedDurationRatio' %in% colnames(t))) {
          t$thresholdAllowedDurationRatio <- NaN
        } else {
          t$thresholdAllowedDurationRatio <- as.numeric(t$thresholdAllowedDurationRatio)
        }
        if (!('thresholdAllowedLatenessSec' %in% colnames(t))) {
          t$thresholdAllowedLatenessSec <- NaN
        } else {
          t$thresholdAllowedLatenessSec <- as.numeric(t$thresholdAllowedLatenessSec)
        }
        if (!('targetDurationSec' %in% colnames(t))) {
          t$`targetDurationSec` <- NaN
        } else {
          t$`targetDurationSec` <- as.numeric(t$`targetDurationSec`)
        }
        if (!('trialGivenToQuestChecks' %in% colnames(t))) {
          t$`trialGivenToQuestChecks` <- ''
        }
        if (!('trialGivenToQuest' %in% colnames(t))) {
          t$`trialGivenToQuest` <- NA
        }
        if (!('trialGivenToQuestErrorCheckLabels' %in% colnames(t))) {
          t$`trialGivenToQuestErrorCheckLabels` <- ''
        }
        if (!('heapUsedBeforeDrawing (MB)' %in% colnames(t))) {
          t$`heapUsedBeforeDrawing (MB)` <- ''
        }
        if (!('heapTotalBeforeDrawing (MB)' %in% colnames(t))) {
          t$`heapTotalBeforeDrawing (MB)` <- ''
        }
        if (!('heapLimitBeforeDrawing (MB)' %in% colnames(t))) {
          t$`heapLimitBeforeDrawing (MB)` <- ''
        }
        if (!('heapUsedAfterDrawing (MB)' %in% colnames(t))) {
          t$`heapUsedAfterDrawing (MB)` <- ''
        }
        if (!('heapTotalAfterDrawing (MB)' %in% colnames(t))) {
          t$`heapTotalAfterDrawing (MB)` <- ''
        }
        if (!('heapLimitAfterDrawing (MB)' %in% colnames(t))) {
          t$`heapLimitAfterDrawing (MB)` <- NaN
        } else {
          t$`heapLimitAfterDrawing (MB)` = as.numeric(t$`heapLimitAfterDrawing (MB)`)
        }
        if (!('heapTotalPostLateness (MB)' %in% colnames(t))) {
          t$`heapTotalPostLateness (MB)` <- ''
        }
        if (!('heapTotalPreLateness (MB)' %in% colnames(t))) {
          t$`heapTotalPreLateness (MB)` <- ''
        }
        if (!('calibrateTrackDistanceMeasuredCm' %in% colnames(t))) {
          t$`calibrateTrackDistanceMeasuredCm` <- ''
        }
        if (!('calibrateTrackDistanceRequestedCm' %in% colnames(t))) {
          t$`calibrateTrackDistanceRequestedCm` <- ''
        }
        if (!('computeRandomMHz' %in% colnames(t))) {
          t$`computeRandomMHz` <- NA
        } else {
          t$computeRandomMHz = t$computeRandomMHz[complete.cases(t$computeRandomMHz)]
        }
        if (!('deviceMemoryGB' %in% colnames(t))) {
          t$`deviceMemoryGB` <- NA
        } else {
          t$deviceMemoryGB = t$deviceMemoryGB[1]
        }
        if (!('longTaskDurationSec' %in% colnames(t))) {
          t$`longTaskDurationSec` <- NA
        }
        if (!('targetMeasuredPreRenderSec' %in% colnames(t))) {
          t$`targetMeasuredPreRenderSec` <- NA
        }
        if (!('heap100MBAllocSec' %in% colnames(t))) {
          t$`heap100MBAllocSec` <- NA
        }
        if (!('fontRenderSec' %in% colnames(t))) {
          t$`fontRenderSec` <- NA
        }
        if (!('fontPadding' %in% colnames(t))) {
          t$`fontPadding` <- NaN
        }
        if (!('mustTrackSec' %in% colnames(t))) {
          t$`mustTrackSec` <- NA
        }
        if (!('pxPerCm' %in% colnames(t))) {
          t$pxPerCm <- NA
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
        # fill block column
        first_non_na <- which(!is.na(t$block))[1]
        if (!is.na(first_non_na)) {
          t$block[1:(first_non_na - 1)] <- 0
          for (i in first_non_na:length(t$block)) {
            if (is.na(t$block[i])) {
              t$block[i] <- current_value
            } else {
              current_value <- t$block[i]
            }
          }
        }
        if (is.na(first_non_na) & length(t$block) > 0) {
          t$block <- 0
        }
        t$system = str_replace_all(t$deviceSystem, "OS X","macOS")
        t$deviceSystemFamily = str_replace_all(t$deviceSystemFamily, "OS X","macOS")
        if (is.na(t$psychojsWindowDimensions[1])) {
          t$psychojsWindowDimensions = 'NA,NA'
        }
        psychojsWindowDimensions <- lapply(str_split(t$psychojsWindowDimensions[1],","), parse_number)[[1]]
        
        WindowDimensions <- paste0(psychojsWindowDimensions, collapse = " x ")
        t$resolution = ifelse(t$resolution[1] == "NA x NA", WindowDimensions, t$resolution)
        t$resolution = ifelse(t$resolution[1] == "NA x NA", "", t$resolution)
        t$screenWidthPx = ifelse(is.na(t$screenWidthPx[1]), psychojsWindowDimensions[1], t$screenWidthPx[1])
        info <- dplyr::filter(t, is.na(questMeanAtEndOfTrialsLoop)) %>% 
          select(block_condition, conditionName) %>% 
          distinct(block_condition, conditionName) %>% 
          dplyr::filter(conditionName != "" & block_condition != "")
        t <- t %>% 
          rename("cores" = "hardwareConcurrency")
        
        info <- t %>% 
          dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
          distinct(experiment, participant, block, block_condition, staircaseName, conditionName, 
                   targetKind, font, experiment, thresholdParameter)
        
        summaries <- t %>% 
          dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>% 
          select(
            block_condition,
            staircaseName, 
            questMeanAtEndOfTrialsLoop,
            questSDAtEndOfTrialsLoop
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
        # for stair plots
        stairdf <- extractCrowdingStaircases(t, info)
        
        summary_list[[j]] <- summaries
        data_list[[j]] <- t
        stair_list[[j]] <-  stairdf 
        t$experiment <- trimws(t$experiment[1])
        experiment[j] <- trimws(t$experiment[1])
        j = j + 1
        readingCorpus <- c(readingCorpus,unique(t$readingCorpus))
      } else {
        next
      }
    }
    if (grepl(".zip", file_list[i])) {
      file_names <- unzip(file_list[i], list = TRUE)$Name
      file_names <- file_names[!grepl("^~", basename(file_names))]
      all_csv <- file_names[grepl(".csv", file_names)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
      all_csv <- all_csv[!grepl("cursor", all_csv)]
      all_csv <- all_csv[!grepl("pretest", all_csv)]
      all_pretest <- file_names[grepl("pretest", file_names)]
      all_pretest <- all_pretest[!grepl("__MACOSX", all_pretest)]
      m <- length(all_csv)
      tmp <- tempdir()
      unzip(file_list[i], exdir = tmp)
      for (k in 1 : m) {
        t <- tibble()
        file_path <- file.path(tmp,all_csv[k])
        try({t <- readr::read_csv(file_path,show_col_types = FALSE)}, silent = TRUE)
        if (!'Submission id' %in% names(t)) {
          if (!('participant' %in% colnames(t))) {
            fileName <- all_csv[k]
            t <- tibble(participant = str_split(fileName, "[_]")[[1]][1],
                        ProlificParticipantID = '',
                        experiment = '')
            t$error <- "Incomplete"
            t$rows <- 0
            t$cols <- 0
          } else {
            t$cols <- ncol(t)
            t$rows <- ifelse(nrow(t) == 0, 0, nrow(t) + 1)
          }
          inf <- file.info(file_path)
          t$kb <-round(inf$size/1024)
          if (!('ProlificParticipantID' %in% colnames(t))) {
            t$ProlificParticipantID <- ""
          }
          if (!('ProlificSessionID' %in% colnames(t))) {
            t$prolificSessionID <- ''
          } else {
            t$prolificSessionID <-  t$ProlificSessionID
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
          if (!'readingPageWords' %in% colnames(t)) {
            t$readingPageWords <- NA
          }
          if (!('viewingDistanceDesiredCm' %in% colnames(t))) {
            t$viewingDistanceDesiredCm <- NA
          }
          if (!('readingLinesPerPage' %in% colnames(t))) {
            t$readingLinesPerPage <- NA
          }
          if (!('readingPageDurationOnsetToOffsetSec' %in% colnames(t))) {
            t$readingPageDurationOnsetToOffsetSec <- NA
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
          if (!('targetMeasuredLatenessSec' %in% colnames(t))) {
            t$targetMeasuredLatenessSec <- NA
          }
          if (!('questMeanAtEndOfTrialsLoop' %in% colnames(t))) {
            t$questMeanAtEndOfTrialsLoop <- NA
          }
          if (!('questSDAtEndOfTrialsLoop' %in% colnames(t))) {
            t$questSDAtEndOfTrialsLoop <- NA
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
            fileName <- str_split(all_csv[k], "[/]")[[1]][2]
            t$experiment <-''
          }
          if (!('experimentCompleteBool' %in% colnames(t))) {
            t$experimentCompleteBool <- FALSE
          }
          if (!('block' %in% colnames(t))) {
            t$block <- 0
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
          if (!('_needsUnmet' %in% colnames(t))) {
            t$`_needsUnmet` <- ""
          }
          if (!('Loudspeaker survey' %in% colnames(t))) {
            t$`Loudspeaker survey` <- ""
          }
          if (!('ComputerInfoFrom51Degrees' %in% colnames(t))) {
            t$ComputerInfoFrom51Degrees <- ""
          }
          if (!('Microphone survey' %in% colnames(t))) {
            t$`Microphone survey` <- ""
          }
          if (!('psychojsWindowDimensions' %in% colnames(t))) {
            t$psychojsWindowDimensions <- "NA,NA"
          }
          if (!('QRConnect' %in% colnames(t))) {
            t$QRConnect <- ''
          }
          if (!('_logFontBool' %in% colnames(t))) {
            t$`_logFontBool` <- FALSE
          }
          if (!('targetMeasuredDurationSec' %in% colnames(t))) {
            t$`targetMeasuredDurationSec` <- NA
          }
          if (!('fontNominalSizePt' %in% colnames(t))) {
            t$`fontNominalSizePt` <- NA
          }
          if (!('fontNominalSizePx' %in% colnames(t))) {
            t$`fontNominalSizePx` <- NA
          }
          if (!('fontMaxPx' %in% colnames(t))) {
            t$`fontMaxPx` <- NA
          }
          if (!('thresholdAllowedDurationRatio' %in% colnames(t))) {
            t$`thresholdAllowedDurationRatio` <- NaN
          } else {
            t$`thresholdAllowedDurationRatio` <- as.numeric(t$`thresholdAllowedDurationRatio`)
          }
          if (!('thresholdAllowedLatenessSec' %in% colnames(t))) {
            t$`thresholdAllowedLatenessSec` <- NaN
          } else {
            t$`thresholdAllowedLatenessSec` <- as.numeric(t$`thresholdAllowedLatenessSec`)
          }
          if (!('targetDurationSec' %in% colnames(t))) {
            t$`targetDurationSec` <- NA
          } else {
            t$`targetDurationSec` <- as.numeric(t$`targetDurationSec`)
          }
          if (!('trialGivenToQuestChecks' %in% colnames(t))) {
            t$`trialGivenToQuestChecks` <- ''
          }
          if (!('trialGivenToQuestErrorCheckLabels' %in% colnames(t))) {
            t$`trialGivenToQuestErrorCheckLabels` <- ''
          }
          if (!('trialGivenToQuest' %in% colnames(t))) {
            t$`trialGivenToQuest` <- NA
          }
          if (!('heapUsedBeforeDrawing (MB)' %in% colnames(t))) {
            t$`heapUsedBeforeDrawing (MB)` <- ''
          }
          if (!('heapTotalBeforeDrawing (MB)' %in% colnames(t))) {
            t$`heapTotalBeforeDrawing (MB)` <- ''
          }
          if (!('heapLimitBeforeDrawing (MB)' %in% colnames(t))) {
            t$`heapLimitBeforeDrawing (MB)` <- ''
          }
          if (!('heapUsedAfterDrawing (MB)' %in% colnames(t))) {
            t$`heapUsedAfterDrawing (MB)` <- ''
          }
          if (!('heapTotalAfterDrawing (MB)' %in% colnames(t))) {
            t$`heapTotalAfterDrawing (MB)` <- NaN
          } else {
            t$`heapTotalAfterDrawing (MB)` = as.numeric(t$`heapTotalAfterDrawing (MB)`)
          }
          if (!('heapLimitAfterDrawing (MB)' %in% colnames(t))) {
            t$`heapLimitAfterDrawing (MB)` <- NaN
          } else {
            t$`heapLimitAfterDrawing (MB)` = as.numeric(t$`heapLimitAfterDrawing (MB)`)
          }
          if (!('heapTotalPostLateness (MB)' %in% colnames(t))) {
            t$`heapTotalPostLateness (MB)` <- ''
          }
          if (!('heapTotalPreLateness (MB)' %in% colnames(t))) {
            t$`heapTotalPreLateness (MB)` <- ''
          }
          if (!('calibrateTrackDistanceMeasuredCm' %in% colnames(t))) {
            t$`calibrateTrackDistanceMeasuredCm` <- ''
          }
          if (!('calibrateTrackDistanceRequestedCm' %in% colnames(t))) {
            t$`calibrateTrackDistanceRequestedCm` <- ''
          }
          if (!('computeRandomMHz' %in% colnames(t))) {
            t$`computeRandomMHz` <- NA
          } else {
            t$computeRandomMHz = t$computeRandomMHz[complete.cases(t$computeRandomMHz)]
          }
          if (!('deviceMemoryGB' %in% colnames(t))) {
            t$`deviceMemoryGB` <- NA
          } else {
            t$deviceMemoryGB = t$deviceMemoryGB[1]
          }
          if (!('longTaskDurationSec' %in% colnames(t))) {
            t$`longTaskDurationSec` <- NA
          }
          if (!('targetMeasuredPreRenderSec' %in% colnames(t))) {
            t$`targetMeasuredPreRenderSec` <- NA
          }
          if (!('heap100MBAllocSec' %in% colnames(t))) {
            t$`heap100MBAllocSec` <- NA
          }
          if (!('fontRenderSec' %in% colnames(t))) {
            t$`fontRenderSec` <- NA
          }
          if (!('fontPadding' %in% colnames(t))) {
            t$`fontPadding` <- NaN
          }
          if (!('mustTrackSec' %in% colnames(t))) {
            t$`mustTrackSec` <- NA
          }
          if (!('pxPerCm' %in% colnames(t))) {
            t$pxPerCm <- NA
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
          # fill block column
          first_non_na <- which(!is.na(t$block))[1]
          if (!is.na(first_non_na)) {
            t$block[1:(first_non_na - 1)] <- 0
            for (i in first_non_na:length(t$block)) {
              if (is.na(t$block[i])) {
                t$block[i] <- current_value
              } else {
                current_value <- t$block[i]
              }
            }
          }
          if (is.na(first_non_na) & length(t$block) > 0) {
          t$block <- 0
          }
          t$system = str_replace_all(t$deviceSystem, "OS X","macOS")
          t$deviceSystemFamily = str_replace_all(t$deviceSystemFamily, "OS X","macOS")
          
          if (is.na(t$psychojsWindowDimensions[1])) {
            t$psychojsWindowDimensions = 'NA,NA'
          }
          psychojsWindowDimensions <- lapply(strsplit(t$psychojsWindowDimensions[1],","), parse_number)[[1]]

          WindowDimensions <- paste0(psychojsWindowDimensions, collapse = " x ")

          t$resolution = ifelse(t$resolution[1] == "NA x NA", WindowDimensions, t$resolution)
          t$resolution = ifelse(t$resolution[1] == "NA x NA", "", t$resolution)
          t$screenWidthPx = ifelse(is.na(t$screenWidthPx[1]), psychojsWindowDimensions[1], t$screenWidthPx[1])
          info <- dplyr::filter(t, is.na(questMeanAtEndOfTrialsLoop)) %>% 
            select(block_condition, conditionName) %>% 
            distinct(block_condition, conditionName) %>% 
            dplyr::filter(conditionName != "" & block_condition != "")
          t <- t %>% 
            rename("cores" = "hardwareConcurrency")
          
          info <- t %>% 
            dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
            distinct(experiment,participant, block, block_condition, staircaseName, conditionName, 
                     targetKind, font, experiment, thresholdParameter)
          
          summaries <- t %>% 
            dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>% 
            select(
              block_condition,
              staircaseName, 
              questMeanAtEndOfTrialsLoop,
              questSDAtEndOfTrialsLoop
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
          
          # for stair plots
          stairdf <- extractCrowdingStaircases(t, info)
          
          if (! t$participant[1] == '') {
            summary_list[[j]] <- summaries
            data_list[[j]] <- t
            stair_list[[j]] <- stairdf
            t$experiment <- trimws(t$experiment[1])
            experiment[j] <- trimws(t$experiment[1])
            readingCorpus <- c(readingCorpus,unique(t$readingCorpus))
            j = j + 1
          }
        }
      }
      if (length(all_pretest) > 0) {
        file_path = file.path(tmp,all_pretest[1] )
        if (grepl("pretest.xlsx", all_pretest[1])) {
          pretest <- readxl::read_xlsx(file_path, col_types = 'text')
          column_names <- names(pretest)
          date_columns <- grep('date', column_names, ignore.case = TRUE, value = TRUE)
          # If there are any columns with 'date' in the name, reload the file with date columns
          if (length(date_columns) > 0) {
            col_types <- ifelse(column_names %in% date_columns, 'date', 'text')
            pretest <- readxl::read_xlsx(file_path, col_types = col_types)
          }
        } 
        else {
          pretest <- readr::read_csv(file_path,show_col_types = FALSE)
        }
       
        if ('PavloviaSessionID' %in% names(pretest)) {
          pretest <- pretest %>% 
            rename('participant' = 'PavloviaSessionID') %>% 
            select(where(~sum(!is.na(.)) >0)) %>% 
            mutate(Grade = ifelse(is.na(Grade), -1, Grade))
          if (!'Skilled reader?' %in% names(pretest)) {
            pretest$`Skilled reader?` = 'unknown'
          }
          if (!'ParticipantCode' %in% names(pretest)) {
            pretest$ParticipantCode = pretest$participant
          }
          if ('participantID' %in% names(pretest)) {
            pretest$ParticipantCode = pretest$participantID
          }
          pretest$`Participant ID` = pretest$ParticipantCode
        }
        if ('ID' %in% names(pretest)) {
          pretest <- pretest %>% 
            rename('participant' = 'ID') %>% 
            select(where(~sum(!is.na(.)) >0)) %>% 
            mutate(Grade = ifelse(is.na(Grade), -1, Grade))
        }
        if (!'Date of Birth' %in% names(pretest)) {
          pretest$birthDate = NA
        } else {
          pretest <- pretest %>% rename('birthDate' = 'Date of Birth')
        }
        if (!'Age' %in% names(pretest)) {
          pretest$Age = NA
        } else {
          pretest$Age <- as.numeric(pretest$Age)
        }
      }
      ('done processing zip')
    }
  }
  
  # Use pretest to override age
  if (nrow(pretest) > 0 ) {
    toJoin <- pretest %>% 
      select(participant, Age, birthDate) %>% 
      rename('birthDate_pre' = 'birthDate',
             'Age_pre' = 'Age')
  }
  
  df <- tibble()
  
  for (i in 1:length(data_list)) {
    if (!'ParticipantCode' %in% names(data_list[[i]])) {
      data_list[[i]]$ParticipantCode = ''
    }
    if (!'participant' %in% names(data_list[[i]])) {
      data_list[[i]]$participant = ''
    }
    if (!'Birthdate' %in% names(data_list[[i]])) {
      data_list[[i]]$Birthdate = ''
    }
    
    if (!'BirthMonthYear' %in% names(t)) {
      data_list[[i]]$BirthMonthYear = ''
    }
    unique_participantCode = unique(data_list[[i]]$ParticipantCode)
    if (length(unique_participantCode) > 1) {
      data_list[[i]]$ParticipantCode = unique(data_list[[i]]$ParticipantCode[!is.na(data_list[[i]]$ParticipantCode)])
    } else {
      data_list[[i]]$ParticipantCode = ''
    }
    
    unique_Birthdate = unique(data_list[[i]]$BirthMonthYear)
    if (length(unique_Birthdate) > 1) {
      data_list[[i]]$BirthMonthYear = unique(data_list[[i]]$BirthMonthYear[!is.na(data_list[[i]]$BirthMonthYear)])
      data_list[[i]]$age = round(interval(parse_date_time(data_list[[i]]$BirthMonthYear[1], orders = c('my')),data_list[[i]]$date[1]) / years(1),2)
    } else {
      data_list[[i]]$BirthMonthYear = ''
      data_list[[i]]$age = NA
    }
    #Override
    if (nrow(pretest) > 0) {
      data_list[[i]] <- data_list[[i]] %>%
        left_join(toJoin, by = 'participant', relationship = "many-to-many") %>% 
        mutate(ageByPretestBirthDate =  round(interval(birthDate_pre, date) / years(1),2)) %>% 
        mutate(age = case_when(
          !is.na(ageByPretestBirthDate) ~ ageByPretestBirthDate,
          !is.na(Age_pre) & is.na(ageByPretestBirthDate) ~ Age_pre,
          is.na(birthDate_pre) & is.na(Age_pre) ~ age,
          .default = NA
        ))
    }
    df <- rbind(df, data_list[[i]] %>% distinct(participant, ParticipantCode, BirthMonthYear,age))
  }

  readingCorpus <- readingCorpus[readingCorpus!="" & !is.na(readingCorpus)]
  experiment <- experiment[!is.na(experiment)]
  experiment <- experiment[experiment!=""]
  stairs <- do.call(rbind, stair_list)
  print('done preprocess')
  return(list(data_list = data_list, 
              summary_list = summary_list, 
              experiment = paste(unique(experiment), collapse = "-"),
              readingCorpus = paste(unique(readingCorpus), collapse = "-"),
              df = df,
              pretest = pretest,
              stairs = stairs
  ))
}

