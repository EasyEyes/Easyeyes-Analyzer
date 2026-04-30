library(dplyr)
library(stringr)
library(readr)
source('./plotting/simulatedRSVP.R')

# Helper function to normalize column names from new "Distance" format to old "TrackDistance" format
# This provides backward compatibility: new data files use "calibrateDistance..." while 
# the plotting code expects "calibrateTrackDistance..."
normalize_distance_column_names <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  
  col_names <- names(df)
  
  # Mapping from new names (without "Track") to old names (with "Track")
  # Pattern: calibrateDistance* -> calibrateTrackDistance*
  # Pattern: _calibrateDistance* -> _calibrateTrackDistance*
  
  # For columns that start with "_calibrateDistance" but not "_calibrateTrackDistance"
  new_underscore_cols <- grep("^_calibrateDistance", col_names, value = TRUE)
  new_underscore_cols <- new_underscore_cols[!grepl("^_calibrateTrackDistance", new_underscore_cols)]
  
  for (col in new_underscore_cols) {
    old_col <- sub("^_calibrateDistance", "_calibrateTrackDistance", col)
    if (!old_col %in% col_names) {
      df[[old_col]] <- df[[col]]
    }
  }
  
  # For columns that start with "calibrateDistance" but not "calibrateTrackDistance"
  new_cols <- grep("^calibrateDistance", col_names, value = TRUE)
  new_cols <- new_cols[!grepl("^calibrateTrackDistance", new_cols)]
  
  for (col in new_cols) {
    old_col <- sub("^calibrateDistance", "calibrateTrackDistance", col)
    if (!old_col %in% names(df)) {
      df[[old_col]] <- df[[col]]
    }
  }
  
  return(df)
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

# Check whether a single ZIP archive is empty
# - Accepts a character path, or a Shiny fileInput row/list with $datapath
# - Returns: TRUE (empty), FALSE (not empty), or NA (unreadable)
check_empty_archive <- function(file) {
  # Resolve the path
  path <- NULL
  if (is.character(file)) {
    path <- file[1]
  } else if (is.list(file) && !is.null(file$datapath)) {
    path <- file$datapath
  } else if (is.data.frame(file) && "datapath" %in% names(file)) {
    path <- file$datapath[[1]]
  } else {
    stop("Provide a zip path, or a Shiny file object/row with $datapath.")
  }

  if (is.na(path) || !nzchar(path) || !file.exists(path)) {
    stop("Archive path does not exist.")
  }

  # List contents; if this fails, return NA
  info <- tryCatch(zip::zip_list(path), error = function(e) e)
  if (inherits(info, "error")) {
    warning(sprintf("Couldn't read archive '%s': %s", basename(path), conditionMessage(info)))
    return(NA)
  }

  # Drop directories and Mac metadata
  info <- info[!grepl("/$", info$filename), , drop = FALSE]           # remove directory entries
  info <- info[!grepl("^__MACOSX/", info$filename), , drop = FALSE]   # remove __MACOSX

  # Empty if no files remain, or all remaining files are 0 bytes
  if (nrow(info) == 0) return(TRUE)
  has_nonempty_file <- any(!is.na(info$uncompressed_size) & info$uncompressed_size > 0)
  return(!has_nonempty_file)
}

# Helper function to normalize filenames by removing browser download suffixes
normalize_filename <- function(filename) {
  # Remove download suffixes commonly added by browsers when downloading duplicate files
  # Patterns handled:
  # - " (1)", " (2)", etc. - Standard macOS/Windows/Chrome pattern
  # - "_(1)", "_(2)", etc. - Underscore variant (e.g. .results_(1).zip)
  # - " - Copy", " - Copy (1)" - Some Windows patterns  
  # - ".1", ".2" - Alternative numbering pattern
  # - Case insensitive matching
  
  # Remove " (number)" pattern (most common)
  normalized <- gsub("\\s+\\([0-9]+\\)(?=\\.[^.]*$)", "", filename, perl = TRUE)
  
  # Remove "_(number)" pattern (e.g. .results_(1).zip from browser downloads)
  normalized <- gsub("_\\([0-9]+\\)(?=\\.[^.]*$)", "", normalized, perl = TRUE)
  
  # Remove " - Copy" and " - Copy (number)" patterns
  normalized <- gsub("\\s+-\\s+Copy(\\s+\\([0-9]+\\))?(?=\\.[^.]*$)", "", normalized, perl = TRUE)
  
  # Remove ".number" pattern (before the final extension)
  normalized <- gsub("\\.[0-9]+(?=\\.[^.]*$)", "", normalized, perl = TRUE)
  
  return(normalized)
}

check_file_names <- function(file) {
  file_names <- file$name
  file_paths <- file$datapath
  valid_endings <- c(".results.zip", ".csv", ".prolific.csv", ".pretest.xlsx")
  
  # Normalize filenames to handle browser download suffixes
  normalized_names <- sapply(file_names, normalize_filename)
  
  is_valid <- sapply(normalized_names, 
                     function(name) any(sapply(valid_endings, function(ext) grepl(paste0(ext, "$"), name))))
  invalid_files <- file_names[!is_valid]
  
  # Check for empty zip files
  zip_indices <- grep("\\.zip$", file_names, ignore.case = TRUE)
  unreadable_empty_files <- c()
  
  if (length(zip_indices) > 0) {
    for (i in zip_indices) {
      zip_path <- file_paths[i]
      zip_name <- file_names[i]
      
      empty_result <- tryCatch({
        check_empty_archive(zip_path)
      }, error = function(e) {
        return(NA)
      })
      
      if (is.na(empty_result) || empty_result) {
        # Combine both unreadable (NA) and empty (TRUE) files
        unreadable_empty_files <- c(unreadable_empty_files, zip_name)
      }
    }
  }
  
  # Now determine what message to return based on what problems we found
  has_invalid_names <- length(invalid_files) > 0
  has_unreadable_empty_files <- length(unreadable_empty_files) > 0
  
  # Build comprehensive error message showing ALL problems
  error_sections <- c()
  
  if (has_invalid_names) {
    error_sections <- c(error_sections, paste0(
      "<strong>Incompatible filename(s):</strong><br>", 
      paste(invalid_files, collapse = ", ")
    ))
  }
  
  if (has_unreadable_empty_files) {
    error_sections <- c(error_sections, paste0(
      "<strong>Unreadable/empty zip file(s):</strong><br>", 
      paste(unreadable_empty_files, collapse = ", ")
    ))
  }
  
  # If we have any problems, return comprehensive message
  if (length(error_sections) > 0) {
    
    # Build help text based on what problems we found
    help_text <- ""
    
    if (has_invalid_names) {
      help_text <- paste0(help_text, 
        "Compatible filenames must have one of these endings:<br>",
        "&nbsp;&nbsp;&nbsp;• .results.zip<br>",
        "&nbsp;&nbsp;&nbsp;• .csv<br>",
        "&nbsp;&nbsp;&nbsp;• .prolific.csv<br>",
        "&nbsp;&nbsp;&nbsp;• .pretest.xlsx<br>",
        "<em>Note: Browser download suffixes like ' (1)' or '_(1)' are automatically ignored.</em><br><br>"
      )
    }
    
    if (has_unreadable_empty_files) {
      help_text <- paste0(help_text, 
        "Zip files must contain experiment data (.csv files) and be readable.<br><br>"
      )
    }
    
    # Use appropriate title based on number of error types
    title <- if (length(error_sections) > 1) {
      "Sorry. Multiple issues found:<br><br>"
    } else {
      "Sorry. File issue found:<br><br>"
    }
    
    return(paste0(
      title,
      paste(error_sections, collapse = "<br><br>"),
      "<br><br>",
      help_text
    ))
  }
  
  # No problems found
  return(NULL)
}

coalesce_chr_scalar <- function(x, default = "") {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(trimws(x))]
  if (length(x) == 0) default else x[1]
}

to_date_safe <- function(x) {
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Ymd", "ymd", "mdY", "dmY", "Y/m/d", "m/d/Y", "d/m/Y"),
    tz = "UTC"
  ))
  as.Date(parsed)
}

parse_birth_month_year_safe <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x == ""] <- NA_character_
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("my", "Ym", "mY", "bY", "Yb"),
    tz = "UTC"
  ))
  as.Date(parsed)
}

derive_age_years <- function(event_date, birth_date) {
  out <- rep(NA_real_, length(event_date))
  ok <- !is.na(event_date) & !is.na(birth_date)
  out[ok] <- as.numeric(difftime(event_date[ok], birth_date[ok], units = "days")) / 365.2425
  round(out, 2)
}

has_unzip_command <- function() {
  # webR/Shinylive can warn here ("'which' was not found on this platform").
  # Treat warnings/errors as "command unavailable".
  out <- tryCatch(
    suppressWarnings(Sys.which("unzip")),
    warning = function(e) "",
    error = function(e) ""
  )
  nzchar(out)
}

ensure_columns <- function(t, file_name = NULL) {
  # First, normalize new "Distance" column names to old "TrackDistance" format for compatibility
  t <- normalize_distance_column_names(t)
  
  # List of columns and their default values
  breaked_fileName <- if (!is.null(file_name)) str_split(file_name, "[_]")[[1]] else character(0)
  participant_id <- if (length(breaked_fileName) >= 1) breaked_fileName[1] else ""
  prolific_id <- if (length(breaked_fileName) >= 2) breaked_fileName[2] else ""

  required_cols <- list(
    `_calibrateTrackDistance` = "",
    `_calibrateTrackDistancePupil` = "",
    `_logFontBool` = FALSE,
    `_needsUnmet` = "",
    block = NA,
    blockShuffleGroups2 = "",
    block_condition = "",
    calibrateTrackDistance = "",
    `_calibrateTrackDistanceAllowedRatio` = NA_real_,
    `_calibrateTrackDistanceShowLengthBool` = FALSE,
    `_calibrateTrackDistanceTimes` = "",
    `_calibrateDistanceAllowedRatioCm` = NA_real_,
    `_calibrateDistanceAllowedRatioHalfCm` = NA_real_,
    `_calibrateDistanceLocations` = "",
    calibrateScreenSizeAllowedRatio = NA_real_,
    calibrateScreenSizeTimes = "",
    calibrateTrackDistanceIpdVpx = NA,
    `_calibrateDistanceCameraHz` = NA_real_,
    cameraIsTopCenter = "",
    cameraResolutionXY = "",
    ComputerInfoFrom51Degrees = "",
    computeRandomMHz = NA,
    conditionName = "",
    correctAns = NA,
    date = "",
    deviceBrowser = "",
    deviceBrowserVersion = "",
    deviceLanguage = "",
    deviceMemoryGB = NA,
    devicePixelRatio = NA,
    deviceSystem = "",
    deviceSystemFamily = "",
    deviceType = "",
    distanceObjectCm = NA_real_,
    error = "",
    experiment = "",
    experimentCompleteBool = FALSE,
    factorVpxCm = NA,
    font = "",
    fontMaxPx = NA,
    fontNominalSizePt = NA,
    fontNominalSizePx = NA,
    fontPadding = NaN,
    fontRenderMaxPx = NaN,
    fontRenderSec = NA,
    fontSizePx = NaN,
    hardwareConcurrency = NA,
    heap100MBAllocSec = NA,
    `heapLimitAfterDrawing (MB)` = NaN,
    `heapLimitBeforeDrawing (MB)` = "",
    `heapTotalAfterDrawing (MB)` = NaN,
    `heapTotalBeforeDrawing (MB)` = "",
    `heapTotalPostLateness (MB)` = "",
    `heapTotalPreLateness (MB)` = "",
    `heapUsedAfterDrawing (MB)` = "",
    `heapUsedBeforeDrawing (MB)` = "",
    `key_resp.corr` = NA,
    key_resp.keys = NA,
    level = NA,
    longTaskDurationSec = NA,
    `Loudspeaker survey` = "",
    `Microphone survey` = "",
    mustTrackSec = NA,
    OBJCT = "",
    participant = participant_id,
    ProlificParticipantID = prolific_id,
    ProlificSessionID = "",
    psychojsWindowDimensions = "NA,NA",
    pxPerCm = NA,
    QRConnect = "",
    questMeanAtEndOfTrialsLoop = NA,
    questSDAtEndOfTrialsLoop = NA,
    questionAndAnswerCorrectAnswer = "",
    questionAndAnswerNickname = "",
    questionAndAnswerQuestion = "",
    questionAndAnswerResponse = "",
    readingCorpus = "",
    readingLinesPerPage = NA,
    readingNumberOfQuestions = NA,
    readingPageDurationOnsetToOffsetSec = NA,
    readingPages = NA,
    readingPageWords = NA,
    readWordIdentifiedBool = NA,
    rulerLength = NA,
    rulerUnit = "",
    rsvpReadingResponseCorrectBool = NA,
    screenHeightPx = NA,
    screenWidthPx = NA,
    SizeCheckEstimatedPxPerCm = "",
    SizeCheckRequestedCm = "",
    spacingOverSizeRatio = NA,
    staircaseName = NA,
    targetDurationSec = NaN,
    targetEccentricityXDeg = NA,
    targetEccentricityYDeg = NA,
    targetFinishSec = NA,
    targetKind = NA,
    targetMeasuredDurationSec = NA,
    targetMeasuredLatenessSec = NA,
    targetMeasuredPreRenderSec = NA,
    targetMinimumPix = NA,
    targetMinPhysicalPx = NA,
    targetStartSec = NA,
    targetTask = NA,
    thresholdAllowedDurationRatio = NaN,
    thresholdAllowedLatenessSec = NaN,
    thresholdParameter = NA,
    trialGivenToQuest = NA,
    trialGivenToQuestChecks = "",
    trialGivenToQuestErrorCheckLabels = "",
    `trials.thisN` = NA,
    viewingDistanceCm = NA,
    viewingDistanceDesiredCm = NA,
    viewingDistanceWhichEye = "",
    viewingDistanceWhichPoint = "",
    warning = "",
    snapshotsLink = ""
  )
  # Speedup: only materialize truly missing columns, and use data.table::set() to avoid
  # repeated full data frame copies in browser/webR environments.
  missing_cols <- setdiff(names(required_cols), names(t))
  if (length(missing_cols) > 0) {
    n_rows <- nrow(t)
    t_dt <- data.table::as.data.table(t)
    for (col in missing_cols) {
      default_val <- required_cols[[col]]
      fill_val <- rep(default_val, n_rows)
      data.table::set(t_dt, j = col, value = fill_val)
    }
    t <- as.data.frame(t_dt)
  }

  # Nothing else to derive for empty inputs.
  if (nrow(t) == 0) {
    return(t)
  }
  
  t <- t %>% 
    mutate(system = str_replace_all(deviceSystem, "OS X","macOS"),
           deviceSystemFamily = str_replace_all(deviceSystemFamily, "OS X","macOS"),
           screenWidthCm = ifelse(is.na(pxPerCm) | pxPerCm <= 0, NA, round(screenWidthPx / pxPerCm,1))
           )
  t$rows = nrow(t)
  t$cols = ifelse('placeholder' %in% names(t), 1, ncol(t))
  t$date = t$date[t$date != "" & !is.na(t$date)][1]
  t$rulerLength = t$rulerLength[t$rulerLength != "" & !is.na(t$rulerLength)][1]
  t$rulerUnit = t$rulerUnit[t$rulerUnit != "" & !is.na(t$rulerUnit)][1]
  t$deviceMemoryGB = sort(t$deviceMemoryGB)[1]
  t$cameraIsTopCenter =  t$cameraIsTopCenter[t$cameraIsTopCenter != "" & !is.na(t$cameraIsTopCenter)][1]
  t$viewingDistanceWhichEye = t$viewingDistanceWhichEye[t$viewingDistanceWhichEye != "" & !is.na(t$viewingDistanceWhichEye)][1]
  t$viewingDistanceWhichPoint = t$viewingDistanceWhichPoint[t$viewingDistanceWhichPoint != "" & !is.na(t$viewingDistanceWhichPoint)][1]
  t$screenWidthCm = sort(t$screenWidthCm)[1]
  t$distanceObjectCm = sort(t$distanceObjectCm)[1]
  t$experimentCompleteBool = sort(t$experimentCompleteBool)[1]
  # calibrateTrackDistance has been renamed as __calibrateTrackDistance
  t$calibrateTrackDistance = t$calibrateTrackDistance[t$calibrateTrackDistance != "" & !is.na(t$calibrateTrackDistance)][1]
  t$`_calibrateTrackDistance` = t$`_calibrateTrackDistance`[t$`_calibrateTrackDistance` != "" & !is.na(t$`_calibrateTrackDistance`)][1]
  t$`_calibrateTrackDistancePupil` = t$`_calibrateTrackDistancePupil`[t$`_calibrateTrackDistancePupil` != "" & !is.na(t$`_calibrateTrackDistancePupil`)][1]
  t$hardwareConcurrency = ifelse(sum(!is.na(t$hardwareConcurrency)) >= 1,
                                 unique(t$hardwareConcurrency[!is.na(t$hardwareConcurrency)& t$hardwareConcurrency != ""]), 
                                 "")
  
  t$deviceBrowser = ifelse(sum(!is.na(t$deviceBrowser)) >= 1,
                           unique(t$deviceBrowser[!is.na(t$deviceBrowser)& t$deviceBrowser != ""]), 
                           "")
  
  t$deviceBrowserVersion = ifelse(sum(!is.na(t$deviceBrowserVersion)) >= 1,
                                  unique(t$deviceBrowserVersion[!is.na(t$deviceBrowserVersion)& t$deviceBrowserVersion != ""]), 
                                  "")
   
  t$deviceSystemFamily =  ifelse(sum(!is.na(t$deviceSystemFamily)) >= 1,
                                 unique(t$deviceSystemFamily[!is.na(t$deviceSystemFamily)& t$deviceSystemFamily != ""]), 
                                 "")
  
  t$deviceSystem =  ifelse(sum(!is.na(t$deviceSystem)) >= 1,
                                 unique(t$deviceSystem[!is.na(t$deviceSystem) & t$deviceSystem != ""]), 
                                 "")
  
  t$deviceType = ifelse(sum(!is.na(t$deviceType)) >= 1,
                        unique(t$deviceType[!is.na(t$deviceType)& t$deviceType != ""]), 
                        "")
  
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
                    browser = case_when(
                      deviceBrowser == "" | is.na(deviceBrowser) ~ "",
                      !is.na(deviceBrowserVersion) & deviceBrowserVersion != "" ~ paste(deviceBrowser, 
                                                                                        str_split(deviceBrowserVersion, "[.]")[[1]][1]),
                      !is.na(deviceBrowser) & deviceBrowser != "" ~ deviceBrowser,
                      .default = ""
                    ),
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

read_files <- function(file, progress = NULL){
  if(is.null(file)) return(list())
  file_list <- file$data
  file_names <- file$name
  file_list <- file_list[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  file_names <- file_names[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  log_info("read_files: ", length(file_names), " files uploaded")
  # Pre-allocate list capacity to reduce reallocations in browser R runtimes.
  data_list <- vector("list", length(file_list))
  stair_list <- vector("list", length(file_list))
  summary_list <- vector("list", length(file_list))
  n <- length(file_list)
  experiment <- rep(NA,n)
  readingCorpus <- c()
  j = 1
  pretest <- tibble()
  unzip_cmd_available <- has_unzip_command()

  for (i in 1 : n) {
    if (!is.null(progress)) {
      progress(
        value = (i - 1) / n,
        message = sprintf("Reading file %d of %d", i, n),
        detail = basename(file_names[i])
      )
    }
    log_debug("Processing file ", i, "/", n, ": ", file_names[i])
    t <- tibble(placeholder = "")
    
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
        pretest <- data.table::fread(file_list[i], data.table = FALSE, showProgress = FALSE)
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
      try({t <- data.table::fread(file_list[i], data.table = FALSE, showProgress = FALSE)}, silent = TRUE)
      if(!is.data.frame(t) || is.null(nrow(t)) || is.na(nrow(t)) || nrow(t) == 0) {
        t <- tibble(placeholder = "")
      }
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
          # CRITICAL FIX: Ensure participant column is character for summary data
          if ("participant" %in% names(summaries)) {
            summaries <- summaries %>% mutate(participant = as.character(participant))
          }
          summary_list[[j]] <- summaries 
          # CRITICAL FIX: Ensure participant column is character for main data
          if ("participant" %in% names(t)) {
            t <- t %>% mutate(participant = as.character(participant))
          }
          data_list[[j]] <- t
          # CRITICAL FIX: Ensure participant column is character for stair data
          if ("participant" %in% names(stairdf)) {
            stairdf <- stairdf %>% mutate(participant = as.character(participant))
          }
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
    if (grepl(".zip", file_names[i])) {
      log_debug("ZIP detected: ", file_names[i])
      # Check if this zip file is empty and skip it if so
      empty_result <- tryCatch({
        check_empty_archive(file_list[i])
      }, error = function(e) {
        log_warn("Could not read zip file ", file_names[i], ": ", e$message)
        return(NA)
      })
      
      if (is.na(empty_result)) {
        log_warn("Skipping unreadable zip: ", file_names[i])
        next
      } else if (empty_result) {
        log_debug("Skipping empty zip: ", file_names[i])
        next
      }
      
      # Proceed with processing non-empty zip file
      zl <- zip::zip_list(file_list[i])
      zip_file_names <- zl$filename
      zip_file_names <- zip_file_names[!grepl("^~", basename(zip_file_names))]
      all_csv <- zip_file_names[grepl(".csv$", zip_file_names, ignore.case = TRUE)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv) & !grepl("cursor", all_csv) & !grepl("pretest\\.csv$", all_csv, ignore.case = TRUE)]
      all_pretest <- zip_file_names[grepl("pretest\\.csv$", zip_file_names, ignore.case = TRUE) | grepl("pretest\\.xlsx$", zip_file_names, ignore.case = TRUE)]
      all_pretest <- all_pretest[!grepl("__MACOSX", all_pretest)]
      m <- length(all_csv)
      log_debug("ZIP contains ", m, " CSV files")
      tmp <- tempdir()
      extracted_all_csv <- FALSE
      if (m > 0 && !unzip_cmd_available) {
        # In Shinylive/webR, shelling out to `unzip -p` is unavailable/slow; extract once.
        try(unzip(file_list[i], files = all_csv, exdir = tmp), silent = TRUE)
        extracted_all_csv <- TRUE
      }
      for (k in 1 : m) {
        if (!is.null(progress)) {
          progress(
            value = (i - 1) / n + ((k - 1) / m) / n,
            message = sprintf("Reading file %d of %d", i, n),
            detail = sprintf("Session %d of %d: %s", k, m, basename(all_csv[k]))
          )
        }
        # Use `unzip -p` only when available; otherwise read from once-extracted files.
        if (unzip_cmd_available) {
          cmd <- sprintf("unzip -p %s %s", shQuote(file_list[i]), shQuote(all_csv[k]))
          read_ok <- TRUE
          t <- tryCatch(
            data.table::fread(cmd = cmd, data.table = FALSE, showProgress = FALSE),
            error = function(e) { read_ok <<- FALSE; e }
          )
          if (!read_ok || inherits(t, "error")) {
            try(unzip(file_list[i], files = all_csv[k], exdir = tmp), silent = TRUE)
            file_path <- file.path(tmp, all_csv[k])
            try({t <- data.table::fread(file_path, data.table = FALSE, showProgress = FALSE)}, silent = TRUE)
          }
        } else {
          if (!extracted_all_csv) {
            try(unzip(file_list[i], files = all_csv[k], exdir = tmp), silent = TRUE)
          }
          file_path <- file.path(tmp, all_csv[k])
          try({t <- data.table::fread(file_path, data.table = FALSE, showProgress = FALSE)}, silent = TRUE)
        }
        if(!is.data.frame(t) || is.null(nrow(t)) || is.na(nrow(t)) || nrow(t) == 0) {
          t <- tibble(placeholder = "")
        }
        if (!'Submission id' %in% names(t)) {
          t <- ensure_columns(t, all_csv[k])
          # Use uncompressed size from zip listing where available
          size_row <- zl$uncompressed_size[match(all_csv[k], zl$filename)]
          if (!is.na(size_row) && length(size_row) == 1) {
            t$kb <- round(size_row / 1024)
          } else {
            t$kb <- NA
          }
         
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
            # CRITICAL FIX: Ensure participant column is character for summary data
            if ("participant" %in% names(summaries)) {
              summaries <- summaries %>% mutate(participant = as.character(participant))
            }
            summary_list[[j]] <- summaries
            # CRITICAL FIX: Ensure participant column is character for main data
            if ("participant" %in% names(t)) {
              t <- t %>% mutate(participant = as.character(participant))
            }
            data_list[[j]] <- t
            # CRITICAL FIX: Ensure participant column is character for stair data
            if ("participant" %in% names(stairdf)) {
              stairdf <- stairdf %>% mutate(participant = as.character(participant))
            }
            stair_list[[j]] <- stairdf
            t$experiment <- trimws(t$experiment[1])
            experiment[j] <- trimws(t$experiment[1])
            readingCorpus <- c(readingCorpus,unique(t$readingCorpus))
            j = j + 1
          }
        }
      }
      if (length(all_pretest) > 0) {
        if (grepl("pretest.xlsx$", all_pretest[1], ignore.case = TRUE)) {
          # Extract only the xlsx and read it
          try(unzip(file_list[i], files = all_pretest[1], exdir = tmp), silent = TRUE)
          file_path = file.path(tmp, all_pretest[1])
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
          if (unzip_cmd_available) {
            # Stream pretest.csv directly from the zip
            cmd <- sprintf("unzip -p %s %s", shQuote(file_list[i]), shQuote(all_pretest[1]))
            pretest <- data.table::fread(cmd = cmd, data.table = FALSE, showProgress = FALSE)
          } else {
            try(unzip(file_list[i], files = all_pretest[1], exdir = tmp), silent = TRUE)
            pretest <- data.table::fread(file.path(tmp, all_pretest[1]), data.table = FALSE, showProgress = FALSE)
          }
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
    }
  }
  
  if (!is.null(progress)) {
    progress(value = 0.95, message = "Processing data...", detail = "Merging sessions")
  }
  
  # Use pretest to override age
  if (nrow(pretest) > 0 ) {
    toJoin <- pretest %>% 
      select(participant, Age, birthDate) %>% 
      rename('birthDate_pre' = 'birthDate',
             'Age_pre' = 'Age')
  }
  
  df_parts <- vector("list", length(data_list))
  
  # Remove any NULL entries from lists that might have been created by skipped files
  data_list <- data_list[!sapply(data_list, is.null)]
  stair_list <- stair_list[!sapply(stair_list, is.null)]
  summary_list <- summary_list[!sapply(summary_list, is.null)]
  
  # Safety check: if no data was processed, return empty structure
  if (length(data_list) == 0) {
    return(list(
      data_list = list(),
      stair_list = list(),
      summary_list = list(),
      pretest = tibble(),
      experiment = character(0),
      prolific = tibble()
    ))
  }
  
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

      data_list[[i]]$ParticipantCode = get_first_non_na(data_list[[i]]$ParticipantCode)
    } else {
      data_list[[i]]$ParticipantCode = ''
    }

    # Derive baseline age from in-file birth month/year, then year-only fallback.
    event_date <- to_date_safe(data_list[[i]]$date)

    birth_month_year_scalar <- coalesce_chr_scalar(data_list[[i]]$BirthMonthYear, default = "")
    data_list[[i]]$BirthMonthYear <- birth_month_year_scalar
    birth_from_month <- parse_birth_month_year_safe(rep(birth_month_year_scalar, nrow(data_list[[i]])))
    age_from_month <- derive_age_years(event_date, birth_from_month)

    birth_year_num <- suppressWarnings(as.numeric(arabic_to_western(as.character(data_list[[i]]$BirthYear))))
    birth_year_scalar <- if (all(is.na(birth_year_num))) NA_real_ else max(birth_year_num, na.rm = TRUE)
    if (is.na(birth_year_scalar)) {
      data_list[[i]]$BirthYear <- ""
      age_from_year <- rep(NA_real_, nrow(data_list[[i]]))
    } else {
      data_list[[i]]$BirthYear <- birth_year_scalar
      age_from_year <- suppressWarnings(lubridate::year(event_date) - birth_year_scalar)
    }
    data_list[[i]]$age <- ifelse(!is.na(age_from_month), age_from_month, age_from_year)

    # Override with pretest data when available (DOB preferred over provided Age).
    if (nrow(pretest) > 0) {
      data_list[[i]] <- data_list[[i]] %>%
        left_join(toJoin, by = 'participant', relationship = "many-to-many") %>% 
        mutate(
          birthDate_pre = to_date_safe(birthDate_pre),
          event_date = to_date_safe(date),
          ageByPretestBirthDate = derive_age_years(event_date, birthDate_pre)
        ) %>% 
        mutate(age = case_when(
          !is.na(ageByPretestBirthDate) ~ ageByPretestBirthDate,
          !is.na(Age_pre) & is.na(ageByPretestBirthDate) ~ Age_pre,
          is.na(birthDate_pre) & is.na(Age_pre) ~ age,
          .default = NA
        )) %>%
        select(-event_date)
    }
    df_parts[[i]] <- data_list[[i]] %>%
      distinct(participant, ParticipantCode, BirthMonthYear, age) %>%
      mutate(
        participant = as.character(participant),
        ParticipantCode = as.character(ParticipantCode),
        BirthMonthYear = as.character(BirthMonthYear),
        age = suppressWarnings(as.numeric(age))
      )
  }
  df <- data.table::rbindlist(df_parts, use.names = TRUE, fill = TRUE)
  df <- as.data.frame(df)
  
  readingCorpus <- readingCorpus[readingCorpus!="" & !is.na(readingCorpus)]
  experiment <- experiment[!is.na(experiment)]
  experiment <- experiment[experiment!=""]
  # More tolerant than dplyr::bind_rows() when some chunks are 0-row / placeholder
  # and infer logical for otherwise character columns (e.g., block_condition).
  stairs <- data.table::rbindlist(stair_list, use.names = TRUE, fill = TRUE)
  stairs <- as.data.frame(stairs)
  prolific <- find_prolific_from_files(file)

  log_info("Preprocess complete: ", length(data_list), " sessions loaded")
  return(list(data_list = data_list, 
              summary_list = summary_list, 
              experiment = unique(experiment),
              readingCorpus = paste(unique(readingCorpus), collapse = "-"),
              df = df,
              pretest = pretest,
              stairs = stairs,
              prolific = prolific
  ))
}

