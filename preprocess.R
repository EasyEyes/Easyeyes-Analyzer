library(dplyr)
library(stringr)
library(readr)
source('./plotting/simulatedRSVP.R')


pxToPt <- function(px, pxPerCm) {
  return ((px / pxPerCm) * 72) / 2.54
}

ptToPx <- function(pt, pxPerCm) {
  return ((2.54 * pt) / 72) * pxPerCm
}
impute_column <- function(df, colname, preceding_value) {
  col <- df[[colname]]
  
  if (all(is.na(col))) {
    return(df)
  }
  
  first_non_na <- which(!is.na(col))[1]
  
  if (!is.na(first_non_na) && first_non_na > 1) {
    df[[colname]][1:(first_non_na - 1)] <- preceding_value
  }
  
  current_value <- col[first_non_na]
  if (first_non_na == nrow(df)) return(df)
  for (i in (first_non_na + 1):nrow(df)) {
    if (is.na(df[[colname]][i])) {
      df[[colname]][i] <- current_value[1]
    } else {
      current_value <- df[[colname]][i]
    }
  }
  return(df)
}

check_file_names <- function(file) {
  file_names <- file$name
  valid_endings <- c(".results.zip", ".csv", ".prolific.csv", ".pretest.xlsx")
  is_valid <- sapply(file_names, 
                     function(name) any(sapply(valid_endings, function(ext) grepl(paste0(ext, "$"), name))))
  invalid_files <- file_names[!is_valid]
  
  if (all(is_valid)) {
    return(NULL) 
  } else {
    return(paste0(
      "Sorry. Incompatible filename(s):<br> ", 
      paste(invalid_files, collapse = ", "), "<br><br>",
      "Compatible filenames must have one of these endings:<br>",
      "&nbsp;&nbsp;&nbsp;• .results.zip<br>",
      "&nbsp;&nbsp;&nbsp;• .csv<br>",
      "&nbsp;&nbsp;&nbsp;• .prolific.csv<br>",
      "&nbsp;&nbsp;&nbsp;• .pretest.xlsx"
    ))
  }
}

ensure_columns <- function(t, file_name = NULL) {
  # Helper to add a column if missing
  add_col <- function(df, col, value) {
    if (!col %in% colnames(df)) df[[col]] <- value
    df
  }
  # List of columns and their default values
  required_cols <- list(
    participant = if (!is.null(file_name)) str_split(file_name, "[_]")[[1]][1] else "",
    ProlificParticipantID = "",
    ProlificSessionID = "",
    experiment = "",
    error = "",
    questionAndAnswerResponse = "",
    warning = "",
    readingCorpus = "",
    viewingDistanceDesiredCm = NA,
    readingLinesPerPage = NA,
    readingPageDurationOnsetToOffsetSec = NA,
    readingPageWords = NA,
    readingNumberOfQuestions = NA,
    key_resp.keys = NA,
    correctAns = NA,
    readWordIdentifiedBool = NA,
    targetEccentricityXDeg = NA,
    targetEccentricityYDeg = NA,
    targetFinishSec = NA,
    targetStartSec = NA,
    targetMeasuredLatenessSec = NA,
    questMeanAtEndOfTrialsLoop = NA,
    questSDAtEndOfTrialsLoop = NA,
    screenHeightPx = NA,
    screenWidthPx = NA,
    deviceBrowser = "",
    deviceBrowserVersion = "",
    deviceType = "",
    deviceSystemFamily = "",
    deviceLanguage = "",
    deviceSystem = "",
    date = "",
    hardwareConcurrency = NA,
    experimentCompleteBool = FALSE,
    block = NA,
    conditionName = "",
    block_condition = "",
    staircaseName = NA,
    font = "",
    targetTask = NA,
    targetKind = NA,
    `_needsUnmet` = "",
    `Loudspeaker survey` = "",
    ComputerInfoFrom51Degrees = "",
    `Microphone survey` = "",
    thresholdParameter = NA,
    psychojsWindowDimensions = "NA,NA",
    QRConnect = "",
    `_logFontBool` = FALSE,
    targetMeasuredDurationSec = NA,
    fontNominalSizePt = NA,
    fontNominalSizePx = NA,
    fontMaxPx = NA,
    thresholdAllowedDurationRatio = NaN,
    thresholdAllowedLatenessSec = NaN,
    targetDurationSec = NaN,
    trialGivenToQuestChecks = "",
    trialGivenToQuest = NA,
    trialGivenToQuestErrorCheckLabels = "",
    `heapUsedBeforeDrawing (MB)` = "",
    `heapTotalBeforeDrawing (MB)` = "",
    `heapLimitBeforeDrawing (MB)` = "",
    `heapUsedAfterDrawing (MB)` = "",
    `heapTotalAfterDrawing (MB)` = NaN,
    `heapLimitAfterDrawing (MB)` = NaN,
    `heapTotalPostLateness (MB)` = "",
    `heapTotalPreLateness (MB)` = "",
    calibrateTrackDistanceMeasuredCm = "",
    calibrateTrackDistanceRequestedCm = "",
    computeRandomMHz = NA,
    deviceMemoryGB = NA,
    longTaskDurationSec = NA,
    targetMeasuredPreRenderSec = NA,
    heap100MBAllocSec = NA,
    fontRenderSec = NA,
    fontPadding = NaN,
    mustTrackSec = NA,
    pxPerCm = NA,
    targetMinimumPix = NA,
    devicePixelRatio = NA,
    targetMinPhysicalPx = NA,
    viewingDistanceCm = NA,
    level = NA,
    spacingOverSizeRatio = NA,
    `key_resp.corr` = NA,
    rsvpReadingResponseCorrectBool = NA
  )
  for (col in names(required_cols)) {
    t <- add_col(t, col, required_cols[[col]])
  }
  
  t <- t %>% 
    mutate(system = str_replace_all(deviceSystem, "OS X","macOS"),
           deviceSystemFamily = str_replace_all(deviceSystemFamily, "OS X","macOS"),
           screenWidthCm = ifelse(is.na(pxPerCm) | pxPerCm <= 0, NA, round(screenWidthPx / pxPerCm,2))
           )
  t$rows = nrow(t)
  t$cols = ncol(t)
  t$deviceMemoryGB = sort(t$deviceMemoryGB)[1]
  t$screenWidthCm = sort(t$screenWidthCm)[1]
  t$experimentCompleteBool = sort(t$experimentCompleteBool)[1]

  t <- impute_column(t, 'block',0)
  t <- impute_column(t, 'thresholdParameter', '')
  t <- impute_column(t, 'targetTask', '')
  t <- impute_column(t, 'targetKind', '')

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
  
  if (is.na(t$psychojsWindowDimensions[1])) {
    t$psychojsWindowDimensions = 'NA,NA'
  }
  psychojsWindowDimensions <- lapply(str_split(t$psychojsWindowDimensions[1],","), parse_number)[[1]]
  
  WindowDimensions <- paste0(psychojsWindowDimensions, collapse = " x ")
  
  t$resolution = ifelse(t$resolution[1] == "NA x NA", WindowDimensions, t$resolution)
  t$resolution = ifelse(t$resolution[1] == "NA x NA", "", t$resolution)
  t$screenWidthPx = ifelse(is.na(t$screenWidthPx[1]), psychojsWindowDimensions[1], t$screenWidthPx[1])

  t <- t %>% 
    rename("cores" = "hardwareConcurrency")
  t
}

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
          mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
          mutate(Grade = ifelse(Grade == 'R', '0', Grade))
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
          mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
          mutate(Grade = ifelse(Grade == 'R', '0', Grade))
        
        pretest$`Participant ID` = pretest$participant
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
        t <- ensure_columns(t, file_names[i])
        t$kb <- round(inf$size / 1024)
        
        info <- t %>% 
          dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
          distinct(experiment, participant, block, block_condition, staircaseName, conditionName, 
                   targetKind, font, thresholdParameter)
        
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
        if (nrow(t) > 0)  {
          stairdf <- extractStaircases(t, info)
          summary_list[[j]] <- summaries
          data_list[[j]] <- t
          stair_list[[j]] <-  stairdf 
          t$experiment <- trimws(t$experiment[1])
          experiment[j] <- trimws(t$experiment[1])
          j = j + 1
          readingCorpus <- c(readingCorpus,unique(t$readingCorpus))
        }
      } else {
        next
      }
    }
    if (grepl(".zip", file_list[i])) {
      file_names <- unzip(file_list[i], list = TRUE)$Name
      file_names <- file_names[!grepl("^~", basename(file_names))]
      all_csv <- file_names[grepl(".csv", file_names)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv) & !grepl("cursor", all_csv) & !grepl("pretest.csv", all_csv)]
      all_pretest <- file_names[grepl("pretest.csv", file_names) | grepl("pretest.xlsx", file_names)]
      all_pretest <- all_pretest[!grepl("__MACOSX", all_pretest)]
      m <- length(all_csv)
      tmp <- tempdir()
      unzip(file_list[i], exdir = tmp)
      for (k in 1 : m) {
        t <- tibble()
        file_path <- file.path(tmp,all_csv[k])
        try({t <- readr::read_csv(file_path,show_col_types = FALSE)}, silent = TRUE)
        if (!'Submission id' %in% names(t)) {
          t <- ensure_columns(t, all_csv[k])
          inf <- file.info(file_path)
          t$kb <- round(inf$size / 1024)
         
          info <- t %>% 
            dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
            distinct(experiment,participant, block, block_condition, staircaseName, conditionName, 
                     targetKind, font, thresholdParameter)
          
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
          stairdf <- extractStaircases(t, info)
          if (nrow(t) > 0)  {
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
            mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
            mutate(Grade = ifelse(Grade == 'R', '0', Grade))
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
            mutate(Grade = ifelse(is.na(Grade), -1, Grade)) %>% 
            mutate(Grade = ifelse(Grade == 'R', '0', Grade))
          pretest$`Participant ID` = pretest$participant
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
    
    if (!'BirthMonthYear' %in% names(data_list[[i]])) {
      data_list[[i]]$BirthMonthYear = ''
    }
    
    if (!'BirthYear' %in% names(data_list[[i]])) {
      data_list[[i]]$BirthYear = NA
    }
    
    
    unique_participantCode = unique(data_list[[i]]$ParticipantCode)
    if (length(unique_participantCode) > 1) {
      data_list[[i]]$ParticipantCode = unique(data_list[[i]]$ParticipantCode[!is.na(data_list[[i]]$ParticipantCode)])
    } else {
      data_list[[i]]$ParticipantCode = ''
    }
    
    unique_Birthdate = unique(data_list[[i]]$BirthMonthYear)
    unique_BirthYear = unique(data_list[[i]]$BirthYear)
    if (length(unique_Birthdate) > 1) {
      data_list[[i]]$BirthMonthYear = unique(data_list[[i]]$BirthMonthYear[!is.na(data_list[[i]]$BirthMonthYear) & data_list[[i]]$BirthMonthYear != ""])
      clean_date <- gsub("([0-9]{2})h([0-9]{2})\\.([0-9]{2})\\.([0-9]{3})", "\\1:\\2:\\3.\\4", data_list[[i]]$date[1])
      clean_date <- sub("_", "T", clean_date)
      
      # Parse with parse_date_time
      parsed_time <- parse_date_time(substr(clean_date, 1, 10), orders = "Ymd", tz = "UTC")
      data_list[[i]]$age = round(interval(parse_date_time(data_list[[i]]$BirthMonthYear[1], orders = c('my')), parsed_time) / years(1),2)
    } else {
      data_list[[i]]$BirthMonthYear = ''
      data_list[[i]]$age = NA
      if (length(unique_BirthYear) > 1 & length(unique_Birthdate) == 1 ) {
        data_list[[i]]$BirthYear = unique(data_list[[i]]$BirthYear[!is.na(data_list[[i]]$BirthYear)])
        data_list[[i]]$age = year(data_list[[i]]$date[1]) - data_list[[i]]$BirthYear[1]
      } else {
        data_list[[i]]$BirthYear = ''
        data_list[[i]]$age = NA
      }
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

