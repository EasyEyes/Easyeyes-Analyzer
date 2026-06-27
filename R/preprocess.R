library(dplyr)
library(stringr)
library(readr)
source("R/plot/simulatedRSVP.R")

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

# Italian and newer pretest files use "OMT"; older files use "OMT_words read".
normalize_pretest_omt_column <- function(pretest) {
  tryCatch({
    if (!is.data.frame(pretest) || nrow(pretest) == 0) {
      return(pretest)
    }

    has_omt <- "OMT" %in% names(pretest)
    has_owr <- "OMT_words read" %in% names(pretest)
    if (!has_omt && !has_owr) {
      return(pretest)
    }

    if (has_omt && has_owr) {
      omt_vals <- suppressWarnings(as.numeric(pretest$OMT))
      owr_raw <- pretest[["OMT_words read"]]
      owr_num <- suppressWarnings(as.numeric(owr_raw))
      missing_owr <- is.na(owr_num) | (!is.na(owr_raw) & as.character(owr_raw) == "")
      if (any(missing_owr, na.rm = TRUE) && any(!is.na(omt_vals))) {
        pretest[["OMT_words read"]][missing_owr] <- as.character(omt_vals[missing_owr])
      }
      return(pretest)
    }

    if (has_omt && !has_owr) {
      omt_vals <- suppressWarnings(as.numeric(pretest$OMT))
      # Only alias OMT when most values look numeric (reading speed), not arbitrary codes.
      if (sum(!is.na(omt_vals)) >= max(1, ceiling(0.1 * nrow(pretest)))) {
        pretest <- pretest %>% rename(`OMT_words read` = OMT)
      }
    }
    pretest
  }, error = function(e) {
    log_warn("normalize_pretest_omt_column failed: ", conditionMessage(e))
    pretest
  })
}

pretest_omt_non_empty <- function(pretest) {
  if (!is.data.frame(pretest) || nrow(pretest) == 0) {
    return(0L)
  }
  best <- 0L
  for (col in c("OMT_words read", "OMT")) {
    if (!col %in% names(pretest)) next
    vals <- pretest[[col]]
    best <- max(best, sum(!is.na(vals) & vals != "", na.rm = TRUE))
  }
  best
}

score_pretest_table <- function(pretest) {
  if (!is.data.frame(pretest) || nrow(pretest) == 0) {
    return(-Inf)
  }
  score <- nrow(pretest)
  if (any(c("participant", "PavloviaSessionID", "ID", "participantID") %in% names(pretest))) {
    score <- score + 1e6
  }
  score <- score + pretest_omt_non_empty(pretest) * 10
  if ("Grade" %in% names(pretest)) score <- score + 100
  if ("Exclude?" %in% names(pretest)) score <- score + 50
  score
}

read_pretest_raw <- function(source, entry = NULL, tmp = tempdir()) {
  tryCatch({
    target <- if (!is.null(entry)) entry else source
    is_xlsx <- grepl("pretest\\.xlsx$", target, ignore.case = TRUE)

    if (!is.null(entry)) {
      if (is_xlsx) {
        try(unzip(source, files = entry, exdir = tmp), silent = TRUE)
        file_path <- file.path(tmp, entry)
        if (!file.exists(file_path)) {
          return(NULL)
        }
        pretest <- readxl::read_xlsx(file_path, col_types = "text")
        column_names <- names(pretest)
        date_columns <- grep("date", column_names, ignore.case = TRUE, value = TRUE)
        if (length(date_columns) > 0) {
          col_types <- ifelse(column_names %in% date_columns, "date", "text")
          pretest <- readxl::read_xlsx(file_path, col_types = col_types)
        }
      } else {
        cmd <- sprintf("unzip -p %s %s", shQuote(source), shQuote(entry))
        pretest <- data.table::fread(cmd = cmd, data.table = FALSE, showProgress = FALSE)
      }
    } else if (is_xlsx) {
      pretest <- readxl::read_xlsx(source, col_types = "text")
      column_names <- names(pretest)
      date_columns <- grep("date", column_names, ignore.case = TRUE, value = TRUE)
      if (length(date_columns) > 0) {
        col_types <- ifelse(column_names %in% date_columns, "date", "text")
        pretest <- readxl::read_xlsx(source, col_types = col_types)
      }
    } else {
      pretest <- data.table::fread(source, data.table = FALSE, showProgress = FALSE)
    }

    if (!is.data.frame(pretest) || nrow(pretest) == 0) {
      return(NULL)
    }
    pretest
  }, error = function(e) {
    log_warn("Could not read pretest file: ", conditionMessage(e))
    NULL
  })
}

apply_pretest_post_read_standardization <- function(pretest) {
  if (!is.data.frame(pretest) || nrow(pretest) == 0) {
    return(tibble())
  }

  tryCatch({
    if ("PavloviaSessionID" %in% names(pretest)) {
      pretest <- pretest %>%
        rename(participant = PavloviaSessionID) %>%
        select(where(~sum(!is.na(.)) > 0))
      if (!"Grade" %in% names(pretest)) {
        pretest$Grade <- -1
      }
      pretest <- pretest %>%
        mutate(
          Grade = ifelse(is.na(Grade), -1, Grade),
          Grade = ifelse(Grade == "R", "0", Grade)
        )
      if (!"Skilled reader?" %in% names(pretest)) {
        pretest$`Skilled reader?` <- "unknown"
      }
      if (!"ParticipantCode" %in% names(pretest)) {
        pretest$ParticipantCode <- pretest$participant
      }
      if ("participantID" %in% names(pretest)) {
        pretest$ParticipantCode <- pretest$participantID
      }
      pretest$`Participant ID` <- pretest$ParticipantCode
    }

    if ("ID" %in% names(pretest) && !"participant" %in% names(pretest)) {
      pretest <- pretest %>%
        rename(participant = ID) %>%
        select(where(~sum(!is.na(.)) > 0))
      if (!"Grade" %in% names(pretest)) {
        pretest$Grade <- -1
      }
      pretest <- pretest %>%
        mutate(
          Grade = ifelse(is.na(Grade), -1, Grade),
          Grade = ifelse(Grade == "R", "0", Grade)
        )
      pretest$`Participant ID` <- pretest$participant
    }

    if (!"Date of Birth" %in% names(pretest)) {
      pretest$birthDate <- NA
    } else {
      pretest <- pretest %>% rename(birthDate = `Date of Birth`)
    }

    if (!"Age" %in% names(pretest)) {
      pretest$Age <- NA
    } else {
      pretest$Age <- suppressWarnings(as.numeric(pretest$Age))
    }

    normalize_pretest_omt_column(pretest)
  }, error = function(e) {
    log_warn("Pretest standardization failed: ", conditionMessage(e))
    tibble()
  })
}

# When several pretest files exist in a zip, pick the richest one; otherwise keep legacy order.
pick_pretest_zip_entry <- function(all_pretest, zip_path, tmp = tempdir()) {
  if (length(all_pretest) == 0) {
    return(NA_character_)
  }
  if (length(all_pretest) == 1) {
    return(all_pretest[1])
  }

  scores <- vapply(
    all_pretest,
    function(entry) {
      score_pretest_table(read_pretest_raw(zip_path, entry = entry, tmp = tmp))
    },
    numeric(1)
  )

  if (all(!is.finite(scores))) {
    return(all_pretest[1])
  }

  best_idx <- which.max(scores)
  if (scores[best_idx] <= 0) {
    return(all_pretest[1])
  }

  # Preserve prior behavior unless another candidate is clearly better.
  if (best_idx != 1L && scores[best_idx] > scores[1L]) {
    log_debug(
      "Selected pretest zip entry ", all_pretest[best_idx],
      " (score ", scores[best_idx], " vs ", scores[1L], ")"
    )
    return(all_pretest[best_idx])
  }
  all_pretest[1]
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

ensure_columns <- function(t, file_name = NULL) {
  # First, normalize new "Distance" column names to old "TrackDistance" format for compatibility
  t <- normalize_distance_column_names(t)
  
  # List of columns and their default values
  breaked_fileName = str_split(file_name, "[_]")[[1]]

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
    participant = if (!is.null(file_name)) str_split(file_name, "[_]")[[1]][1] else "",
    ProlificParticipantID = if (!is.null(file_name)) str_split(file_name, "[_]")[[1]][2] else "",
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
  missing_cols <- setdiff(names(required_cols), names(t))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      t[[col]] <- required_cols[[col]]
    }
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
                    block_condition = as.character(ifelse(
                      is.na(block_condition) | block_condition == "",
                      staircaseName,
                      block_condition
                    )))
  
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

#### read_files helpers ####

# True when a zip::zip_list() result contains at least one non-directory,
# non-__MACOSX file with uncompressed size > 0. Used to skip empty archives
# without calling check_empty_archive() again after check_file_names().
zip_listing_has_data <- function(zl) {
  entries <- zl$filename
  entries <- entries[!grepl("/$", entries)]
  entries <- entries[!grepl("^__MACOSX/", entries)]
  if (length(entries) == 0) {
    return(FALSE)
  }
  sizes <- zl$uncompressed_size[match(entries, zl$filename)]
  any(!is.na(sizes) & sizes > 0, na.rm = TRUE)
}

# Per-session QUEST end-state table: quest mean/SD rows joined to condition
# metadata (info). Joins on block_condition or staircaseName depending on
# which key is unique within the file.
build_quest_summaries <- function(t, info) {
  summaries <- t %>%
    dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>%
    select(
      block_condition,
      staircaseName,
      questMeanAtEndOfTrialsLoop,
      questSDAtEndOfTrialsLoop
    )
  if (n_distinct(summaries$staircaseName) < n_distinct(summaries$block_condition)) {
    summaries <- summaries %>%
      select(-staircaseName) %>%
      left_join(info, by = "block_condition", relationship = "many-to-many")
  } else {
    summaries <- summaries %>%
      select(-block_condition)
    summaries <- merge(info, summaries, by = "staircaseName")
  }
  summaries
}

# Participant IDs must be character so rbind/bind_rows across sessions
# does not coerce mixed types.
coerce_participant_char <- function(df) {
  if ("participant" %in% names(df)) {
    df <- df %>% mutate(participant = as.character(participant))
  }
  df
}

# Normalize one results CSV, build stair/summary tables, and append to the
# session lists at index j. Returns updated lists, experiment vector, j, and
# added = TRUE/FALSE. Skips empty frames, fread placeholders, and Prolific
# export files (Submission id column).
append_parsed_session <- function(
    t, file_label, kb,
    data_list, stair_list, summary_list, experiment, j) {
  unchanged <- list(
    data_list = data_list,
    stair_list = stair_list,
    summary_list = summary_list,
    experiment = experiment,
    j = j,
    added = FALSE
  )
  if (
    !is.data.frame(t) || is.null(nrow(t)) || is.na(nrow(t)) || nrow(t) == 0 ||
      "placeholder" %in% names(t) || "Submission id" %in% names(t)
  ) {
    return(unchanged)
  }

  t <- ensure_columns(t, file_label)
  t$kb <- kb

  info <- t %>%
    dplyr::filter(is.na(questMeanAtEndOfTrialsLoop)) %>%
    distinct(
      experiment, participant, block, block_condition, staircaseName,
      conditionName, targetKind, font, thresholdParameter
    )

  summaries <- build_quest_summaries(t, info)
  stairdf <- extractStaircases(t, info)

  data_list[[j]] <- coerce_participant_char(t)
  summary_list[[j]] <- coerce_participant_char(summaries)
  stair_list[[j]] <- coerce_participant_char(stairdf)
  experiment[j] <- trimws(t$experiment[1])

  unchanged$data_list <- data_list
  unchanged$stair_list <- stair_list
  unchanged$summary_list <- summary_list
  unchanged$experiment <- experiment
  unchanged$j <- j + 1
  unchanged$added <- TRUE
  unchanged
}

read_files <- function(file, progress = NULL){
  if(is.null(file)) return(list())
  file_list <- file$data
  file_names <- file$name
  file_list <- file_list[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  file_names <- file_names[!grepl("cursor", basename(file_names)) & !grepl("^~", basename(file_names))]
  log_info("read_files: ", length(file_names), " files uploaded")
  data_list <- list()
  stair_list <- list()
  summary_list <- list()
  n <- length(file_list)
  experiment <- rep(NA, n)
  j <- 1
  pretest <- tibble()
  prolificDT <- tibble()

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
      pretest <- read_pretest_raw(file_list[i])
      pretest <- apply_pretest_post_read_standardization(pretest)
    }

    if (grepl("prolific\\.csv$", file_names[i], ignore.case = TRUE)) {
      prolificDT <- append_prolific_rows(prolificDT, read_prolific(file_list[i]))
      next
    }

    if (grepl(".csv", file_names[i]) & !grepl("pretest.csv", file_names[i])) {
      try({
        t <- data.table::fread(file_list[i], data.table = FALSE, showProgress = FALSE)
      }, silent = TRUE)
      if (!is.data.frame(t) || is.null(nrow(t)) || is.na(nrow(t)) || nrow(t) == 0) {
        t <- tibble(placeholder = "")
      }
      inf <- file.info(file_list[i])
      parsed <- append_parsed_session(
        t, file_names[i], round(inf$size / 1024),
        data_list, stair_list, summary_list, experiment, j
      )
      data_list <- parsed$data_list
      stair_list <- parsed$stair_list
      summary_list <- parsed$summary_list
      experiment <- parsed$experiment
      j <- parsed$j
    }
    if (grepl(".zip", file_names[i])) {
      log_debug("ZIP detected: ", file_names[i])
      zl <- tryCatch(
        zip::zip_list(file_list[i]),
        error = function(e) {
          log_warn("Could not read zip file ", file_names[i], ": ", e$message)
          e
        }
      )
      if (inherits(zl, "error")) {
        next
      }
      if (!zip_listing_has_data(zl)) {
        log_debug("Skipping empty zip: ", file_names[i])
        next
      }

      zip_file_names <- zl$filename
      zip_file_names <- zip_file_names[!grepl("^~", basename(zip_file_names))]
      prolific_csvs <- zip_file_names[
        grepl("prolific\\.csv$", zip_file_names, ignore.case = TRUE) &
          !grepl("__MACOSX", zip_file_names)
      ]
      for (pf in prolific_csvs) {
        prolificDT <- append_prolific_rows(
          prolificDT,
          read_prolific_from_zip(file_list[i], pf)
        )
      }

      all_csv <- zip_file_names[grepl(".csv$", zip_file_names, ignore.case = TRUE)]
      all_csv <- all_csv[
        !grepl("__MACOSX", all_csv) &
          !grepl("cursor", all_csv) &
          !grepl("pretest\\.csv$", all_csv, ignore.case = TRUE) &
          !grepl("prolific\\.csv$", all_csv, ignore.case = TRUE)
      ]
      all_pretest <- zip_file_names[
        grepl("pretest\\.csv$", zip_file_names, ignore.case = TRUE) |
          grepl("pretest\\.xlsx$", zip_file_names, ignore.case = TRUE)
      ]
      all_pretest <- all_pretest[!grepl("__MACOSX", all_pretest)]
      m <- length(all_csv)
      log_debug("ZIP contains ", m, " CSV files")
      tmp <- tempdir()
      for (k in 1 : m) {
        if (!is.null(progress)) {
          progress(
            value = (i - 1) / n + ((k - 1) / m) / n,
            message = sprintf("Reading file %d of %d", i, n),
            detail = sprintf("Session %d of %d: %s", k, m, basename(all_csv[k]))
          )
        }
        # Stream CSV directly from zip without extracting to disk; fallback to extracting just this file
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
        if (!is.data.frame(t) || is.null(nrow(t)) || is.na(nrow(t)) || nrow(t) == 0) {
          t <- tibble(placeholder = "")
        }
        size_row <- zl$uncompressed_size[match(all_csv[k], zl$filename)]
        kb <- if (!is.na(size_row) && length(size_row) == 1) {
          round(size_row / 1024)
        } else {
          NA
        }
        parsed <- append_parsed_session(
          t, all_csv[k], kb,
          data_list, stair_list, summary_list, experiment, j
        )
        data_list <- parsed$data_list
        stair_list <- parsed$stair_list
        summary_list <- parsed$summary_list
        experiment <- parsed$experiment
        j <- parsed$j
      }
      if (length(all_pretest) > 0) {
        pretest_file <- pick_pretest_zip_entry(all_pretest, file_list[i], tmp)
        if (!is.na(pretest_file)) {
          pretest <- read_pretest_raw(file_list[i], entry = pretest_file, tmp = tmp)
          pretest <- apply_pretest_post_read_standardization(pretest)
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
  
  df <- tibble()
  
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
    
    unique_Birthdate = unique(data_list[[i]]$BirthMonthYear)
    unique_BirthYear = unique(data_list[[i]]$BirthYear)
    if (length(unique_Birthdate) > 1) {
      data_list[[i]]$BirthMonthYear = get_first_non_na(data_list[[i]]$BirthMonthYear)
      clean_date <- gsub("([0-9]{2})h([0-9]{2})\\.([0-9]{2})\\.([0-9]{3})", "\\1:\\2:\\3.\\4", get_first_non_na(data_list[[i]]$date))
      clean_date <- sub("_", "T", clean_date)
      
      # Parse with parse_date_time
      parsed_time <- parse_date_time(substr(clean_date, 1, 10), orders = "Ymd", tz = "UTC")
      data_list[[i]]$age = round(interval(parse_date_time(data_list[[i]]$BirthMonthYear[1], orders = c('my')), parsed_time) / years(1),2)
    } else {
      data_list[[i]]$BirthMonthYear = ''
      data_list[[i]]$age = NA
      if (length(unique_BirthYear) > 1 & length(unique_Birthdate) == 1 ) {
        data_list[[i]]$BirthYear = max(as.numeric(arabic_to_western(data_list[[i]]$BirthYear)), na.rm = T)
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
  
  experiment <- experiment[!is.na(experiment)]
  experiment <- experiment[experiment != ""]
  stairs <- do.call(rbind, stair_list)

  log_info("Preprocess complete: ", length(data_list), " sessions loaded")
  return(list(
    data_list = data_list,
    summary_list = summary_list,
    experiment = unique(experiment),
    df = df,
    pretest = pretest,
    stairs = stairs,
    prolific = prolificDT
  ))
}

