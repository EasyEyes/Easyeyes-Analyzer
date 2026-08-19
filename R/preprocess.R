# =============================================================================
# File:        R/preprocess.R
# Project:     Easyeyes-Analyzer
# Description: Ingest uploaded CSV/ZIP experiment files, normalize columns,
#              extract pretest/prolific metadata, and build session data lists
#              for downstream thresholding and plotting.
# Author:      EasyEyes team
# Depends:     dplyr, stringr, readr, data.table, zip, readxl
# Notes:       Sourced by R/load_app.R. Entry point: read_files().
#              Relies on dplyr/stringr/readr from server.R and
#              extractStaircases() from R/plot/simulatedRSVP.R (loaded later).
# =============================================================================

# Helper: normalize new "Distance" column names to old "TrackDistance"
# format. New files use "calibrateDistance..."; plotting expects
# "calibrateTrackDistance...".
normalize_distance_column_names <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) return(df)
  
  col_names <- names(df)
  
  # Mapping from new names (without "Track") to old names (with "Track")
  # Pattern: calibrateDistance* -> calibrateTrackDistance*
  # Pattern: _calibrateDistance* -> _calibrateTrackDistance*
  
  # Columns starting with "_calibrateDistance" (not already Track)
  new_underscore_cols <- grep(
    "^_calibrateDistance", col_names, value = TRUE
  )
  already_track <- grepl(
    "^_calibrateTrackDistance", new_underscore_cols
  )
  new_underscore_cols <- new_underscore_cols[!already_track]
  
  for (col in new_underscore_cols) {
    old_col <- sub("^_calibrateDistance", "_calibrateTrackDistance", col)
    if (!old_col %in% col_names) {
      df[[old_col]] <- df[[col]]
    }
  }
  
  # Columns starting with "calibrateDistance" (not already Track)
  new_cols <- grep("^calibrateDistance", col_names, value = TRUE)
  already_track <- grepl("^calibrateTrackDistance", new_cols)
  new_cols <- new_cols[!already_track]
  
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
    warning(sprintf(
      "Couldn't read archive '%s': %s",
      basename(path),
      conditionMessage(info)
    ))
    return(NA)
  }

  # Drop directories and Mac metadata
  info <- info[!grepl("/$", info$filename), , drop = FALSE]
  info <- info[
    !grepl("^__MACOSX/", info$filename), , drop = FALSE
  ]

  # Empty if no files remain, or all remaining files are 0 bytes
  if (nrow(info) == 0) return(TRUE)
  has_nonempty_file <- any(
    !is.na(info$uncompressed_size) & info$uncompressed_size > 0
  )
  return(!has_nonempty_file)
}

# Helper function to normalize filenames by removing browser download suffixes
normalize_filename <- function(filename) {
  # Remove browser download suffixes on duplicate filenames
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
  normalized <- gsub(
    "\\s+-\\s+Copy(\\s+\\([0-9]+\\))?(?=\\.[^.]*$)",
    "",
    normalized,
    perl = TRUE
  )
  
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
      missing_owr <- is.na(owr_num) |
        (!is.na(owr_raw) & as.character(owr_raw) == "")
      if (any(missing_owr, na.rm = TRUE) && any(!is.na(omt_vals))) {
        pretest[["OMT_words read"]][missing_owr] <- as.character(
          omt_vals[missing_owr]
        )
      }
      return(pretest)
    }

    if (has_omt && !has_owr) {
      omt_vals <- suppressWarnings(as.numeric(pretest$OMT))
      # Alias OMT only when values look numeric (reading speed).
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
  id_cols <- c(
    "participant", "PavloviaSessionID", "ID", "participantID"
  )
  if (any(id_cols %in% names(pretest))) {
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
        date_columns <- grep(
          "date", column_names, ignore.case = TRUE, value = TRUE
        )
        if (length(date_columns) > 0) {
          col_types <- ifelse(
            column_names %in% date_columns, "date", "text"
          )
          pretest <- readxl::read_xlsx(
            file_path, col_types = col_types
          )
        }
      } else {
        cmd <- sprintf(
          "unzip -p %s %s", shQuote(source), shQuote(entry)
        )
        pretest <- data.table::fread(
          cmd = cmd, data.table = FALSE, showProgress = FALSE
        )
      }
    } else if (is_xlsx) {
      pretest <- readxl::read_xlsx(source, col_types = "text")
      column_names <- names(pretest)
      date_columns <- grep(
        "date", column_names, ignore.case = TRUE, value = TRUE
      )
      if (length(date_columns) > 0) {
        col_types <- ifelse(
          column_names %in% date_columns, "date", "text"
        )
        pretest <- readxl::read_xlsx(
          source, col_types = col_types
        )
      }
    } else {
      pretest <- data.table::fread(
        source, data.table = FALSE, showProgress = FALSE
      )
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

# Pick richest pretest in a zip; else keep legacy first-entry order.
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
  
  is_valid <- sapply(normalized_names, function(name) {
    any(sapply(valid_endings, function(ext) {
      grepl(paste0(ext, "$"), name)
    }))
  })
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
        "<em>Note: Browser download suffixes like ' (1)' ",
        "or '_(1)' are automatically ignored.</em><br><br>"
      )
    }
    
    if (has_unreadable_empty_files) {
      help_text <- paste0(help_text, 
        "Zip files must contain experiment data ",
        "(.csv files) and be readable.<br><br>"
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

# First non-missing, non-empty scalar from a vector (session-level metadata).
first_non_empty <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) {
    return(default)
  }
  keep <- !is.na(x)
  if (is.character(x) || is.factor(x)) {
    keep <- keep & as.character(x) != ""
  }
  x <- x[keep]
  if (length(x) == 0) {
    return(default)
  }
  x[[1]]
}

# Smallest non-NA value (matches sort(x)[1] with NAs last).
first_sorted <- function(x, default = NA) {
  x <- x[!is.na(x)]
  if (length(x) == 0) {
    return(default)
  }
  sort(x)[[1]]
}

# Prefer a non-NA unique screen dim when several exist (legacy).
pick_screen_dim <- function(x) {
  u <- unique(x)
  if (length(u) <= 1) {
    return(NA)
  }
  u <- u[!is.na(u)]
  if (length(u) == 0) {
    return(NA)
  }
  u[[1]]
}

ensure_columns <- function(t, file_name = NULL) {
  # Normalize Distance column names to TrackDistance for compatibility
  t <- normalize_distance_column_names(t)

  name_parts <- if (!is.null(file_name)) {
    str_split(file_name, "[_]")[[1]]
  } else {
    NULL
  }
  participant_default <- if (!is.null(name_parts)) {
    name_parts[1]
  } else {
    ""
  }
  prolific_id_default <- if (
    !is.null(name_parts) && length(name_parts) >= 2
  ) {
    name_parts[2]
  } else {
    ""
  }

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
    participant = participant_default,
    ProlificParticipantID = prolific_id_default,
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

  # Session-level scalars: scan each metadata column once, then broadcast.
  date_val <- first_non_empty(t$date, "")
  device_system <- first_non_empty(t$deviceSystem, "")
  device_system_family <- str_replace_all(
    first_non_empty(t$deviceSystemFamily, ""),
    "OS X",
    "macOS"
  )
  device_browser <- first_non_empty(t$deviceBrowser, "")
  device_browser_version <- first_non_empty(t$deviceBrowserVersion, "")
  device_type <- first_non_empty(t$deviceType, "")
  cores_val <- first_non_empty(t$hardwareConcurrency, "")
  px_per_cm <- first_sorted(suppressWarnings(as.numeric(t$pxPerCm)))
  screen_width_px <- pick_screen_dim(t$screenWidthPx)
  screen_height_px <- pick_screen_dim(t$screenHeightPx)
  if (!is.na(px_per_cm) && px_per_cm > 0 && !is.na(screen_width_px)) {
    screen_width_cm <- round(screen_width_px / px_per_cm, 1)
  } else {
    sw_cm <- suppressWarnings(
      as.numeric(t$screenWidthPx) / as.numeric(t$pxPerCm)
    )
    sw_cm[is.na(as.numeric(t$pxPerCm)) | as.numeric(t$pxPerCm) <= 0] <- NA
    screen_width_cm <- first_sorted(round(sw_cm, 1))
  }

  t$date <- date_val
  t$rulerLength <- first_non_empty(t$rulerLength)
  t$rulerUnit <- first_non_empty(t$rulerUnit, "")
  t$deviceMemoryGB <- first_sorted(t$deviceMemoryGB)
  t$cameraIsTopCenter <- first_non_empty(t$cameraIsTopCenter, "")
  t$viewingDistanceWhichEye <- first_non_empty(t$viewingDistanceWhichEye, "")
  t$viewingDistanceWhichPoint <- first_non_empty(
    t$viewingDistanceWhichPoint, ""
  )
  t$distanceObjectCm <- first_sorted(t$distanceObjectCm)
  t$experimentCompleteBool <- first_sorted(t$experimentCompleteBool)
  t$calibrateTrackDistance <- first_non_empty(t$calibrateTrackDistance, "")
  t$`_calibrateTrackDistance` <- first_non_empty(
    t$`_calibrateTrackDistance`, ""
  )
  t$`_calibrateTrackDistancePupil` <- first_non_empty(
    t$`_calibrateTrackDistancePupil`, ""
  )
  t$hardwareConcurrency <- cores_val
  t$deviceBrowser <- device_browser
  t$deviceBrowserVersion <- device_browser_version
  t$deviceSystemFamily <- device_system_family
  t$deviceSystem <- device_system
  t$deviceType <- device_type
  t$system <- str_replace_all(device_system, "OS X", "macOS")
  t$screenWidthCm <- screen_width_cm
  t$screenWidthPx <- screen_width_px
  t$screenHeightPx <- screen_height_px
  t$rows <- nrow(t)
  t$cols <- ifelse("placeholder" %in% names(t), 1, ncol(t))

  t <- impute_column(t, "block", 0)
  t <- impute_column(t, "thresholdParameter", "")
  t <- impute_column(t, "targetTask", "")
  t <- impute_column(t, "targetKind", "")

  has_browser_ver <- !is.na(device_browser_version) &&
    nzchar(as.character(device_browser_version))
  browser_major <- if (has_browser_ver) {
    str_split(
      as.character(device_browser_version), "[.]"
    )[[1]][1]
  } else {
    ""
  }
  browser_val <- if (
    is.na(device_browser) || !nzchar(as.character(device_browser))
  ) {
    ""
  } else if (nzchar(browser_major)) {
    paste(device_browser, browser_major)
  } else {
    device_browser
  }

  t$browser <- browser_val
  t$block_condition <- as.character(ifelse(
    is.na(t$block_condition) | t$block_condition == "",
    t$staircaseName,
    t$block_condition
  ))

  psycho_raw <- t$psychojsWindowDimensions[1]
  if (is.na(psycho_raw) || identical(psycho_raw, "")) {
    psycho_raw <- "NA,NA"
    t$psychojsWindowDimensions <- "NA,NA"
  }
  psychojsWindowDimensions <- lapply(
    str_split(psycho_raw, ","), parse_number
  )[[1]]
  window_dimensions <- paste0(psychojsWindowDimensions, collapse = " x ")

  resolution <- paste0(t$screenWidthPx[1], " x ", t$screenHeightPx[1])
  if (identical(resolution, "NA x NA")) {
    resolution <- window_dimensions
  }
  if (identical(resolution, "NA x NA")) {
    resolution <- ""
  }
  t$resolution <- resolution
  if (is.na(t$screenWidthPx[1])) {
    t$screenWidthPx <- psychojsWindowDimensions[1]
  }

  t <- t %>% rename("cores" = "hardwareConcurrency")
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
  n_stair <- n_distinct(summaries$staircaseName)
  n_block <- n_distinct(summaries$block_condition)
  if (n_stair < n_block) {
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

  if (!"participant" %in% names(t)) {
    log_warn("Session file missing participant column: ", file_label)
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
  keep <- !grepl("cursor", basename(file_names)) &
    !grepl("^~", basename(file_names))
  file_list <- file_list[keep]
  file_names <- file_names[keep]
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
    
    is_pretest <- grepl("pretest.xlsx", file_names[i]) |
      grepl("pretest.csv", file_names[i])
    if (is_pretest) {
      pretest <- read_pretest_raw(file_list[i])
      pretest <- apply_pretest_post_read_standardization(pretest)
    }

    if (grepl("prolific\\.csv$", file_names[i], ignore.case = TRUE)) {
      prolificDT <- append_prolific_rows(
        prolificDT, read_prolific(file_list[i])
      )
      next
    }

    if (grepl(".csv", file_names[i]) & !grepl("pretest.csv", file_names[i])) {
      try({
        t <- data.table::fread(
          file_list[i], data.table = FALSE, showProgress = FALSE
        )
      }, silent = TRUE)
      empty_df <- !is.data.frame(t) ||
        is.null(nrow(t)) ||
        is.na(nrow(t)) ||
        nrow(t) == 0
      if (empty_df) {
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

      all_csv <- zip_file_names[
        grepl(".csv$", zip_file_names, ignore.case = TRUE)
      ]
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
        # Stream CSV from zip; fall back to extracting this file
        cmd <- sprintf(
          "unzip -p %s %s",
          shQuote(file_list[i]),
          shQuote(all_csv[k])
        )
        read_ok <- TRUE
        t <- tryCatch(
          data.table::fread(
            cmd = cmd, data.table = FALSE, showProgress = FALSE
          ),
          error = function(e) {
            read_ok <<- FALSE
            e
          }
        )
        if (!read_ok || inherits(t, "error")) {
          try(
            unzip(
              file_list[i], files = all_csv[k], exdir = tmp
            ),
            silent = TRUE
          )
          file_path <- file.path(tmp, all_csv[k])
          try({
            t <- data.table::fread(
              file_path,
              data.table = FALSE,
              showProgress = FALSE
            )
          }, silent = TRUE)
        }
        empty_df <- !is.data.frame(t) ||
          is.null(nrow(t)) ||
          is.na(nrow(t)) ||
          nrow(t) == 0
        if (empty_df) {
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
          pretest <- read_pretest_raw(
            file_list[i], entry = pretest_file, tmp = tmp
          )
          pretest <- apply_pretest_post_read_standardization(pretest)
        }
      }
    }
  }
  
  if (!is.null(progress)) {
    progress(
      value = 0.95,
      message = "Processing data...",
      detail = "Merging sessions"
    )
  }
  
  # Use pretest to override age on a compact participant table, then map back.
  toJoin <- NULL
  if (nrow(pretest) > 0) {
    toJoin <- pretest %>%
      select(participant, Age, birthDate) %>%
      rename(
        birthDate_pre = birthDate,
        Age_pre = Age
      ) %>%
      distinct(participant, .keep_all = TRUE)
  }

  # Drop NULL entries left by skipped files
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

  df_parts <- vector("list", length(data_list))
  for (i in seq_along(data_list)) {
    if (!"ParticipantCode" %in% names(data_list[[i]])) {
      data_list[[i]]$ParticipantCode <- ""
    }
    if (!"participant" %in% names(data_list[[i]])) {
      data_list[[i]]$participant <- ""
    }
    if (!"Birthdate" %in% names(data_list[[i]])) {
      data_list[[i]]$Birthdate <- ""
    }
    if (!"BirthMonthYear" %in% names(data_list[[i]])) {
      data_list[[i]]$BirthMonthYear <- ""
    }
    if (!"BirthYear" %in% names(data_list[[i]])) {
      data_list[[i]]$BirthYear <- NA
    }

    unique_participantCode <- unique(data_list[[i]]$ParticipantCode)
    if (length(unique_participantCode) > 1) {
      data_list[[i]]$ParticipantCode <- get_first_non_na(
        data_list[[i]]$ParticipantCode
      )
    } else {
      data_list[[i]]$ParticipantCode <- ""
    }

    unique_Birthdate <- unique(data_list[[i]]$BirthMonthYear)
    unique_BirthYear <- unique(data_list[[i]]$BirthYear)
    if (length(unique_Birthdate) > 1) {
      data_list[[i]]$BirthMonthYear <- get_first_non_na(
        data_list[[i]]$BirthMonthYear
      )
      clean_date <- gsub(
        "([0-9]{2})h([0-9]{2})\\.([0-9]{2})\\.([0-9]{3})",
        "\\1:\\2:\\3.\\4",
        get_first_non_na(data_list[[i]]$date)
      )
      clean_date <- sub("_", "T", clean_date)
      parsed_time <- parse_date_time(
        substr(clean_date, 1, 10),
        orders = "Ymd",
        tz = "UTC"
      )
      data_list[[i]]$age <- round(
        interval(
          parse_date_time(data_list[[i]]$BirthMonthYear[1], orders = c("my")),
          parsed_time
        ) / years(1),
        2
      )
    } else {
      data_list[[i]]$BirthMonthYear <- ""
      data_list[[i]]$age <- NA
      if (length(unique_BirthYear) > 1 && length(unique_Birthdate) == 1) {
        data_list[[i]]$BirthYear <- max(
          as.numeric(arabic_to_western(data_list[[i]]$BirthYear)),
          na.rm = TRUE
        )
        data_list[[i]]$age <- (
          year(data_list[[i]]$date[1]) -
            data_list[[i]]$BirthYear[1]
        )
      } else {
        data_list[[i]]$BirthYear <- ""
        data_list[[i]]$age <- NA
      }
    }

    # Pretest age override: join only distinct participants, then broadcast age.
    if (!is.null(toJoin)) {
      session_date <- data_list[[i]]$date[1]
      participant_age <- data_list[[i]] %>%
        distinct(participant, .keep_all = TRUE) %>%
        select(participant, age) %>%
        left_join(toJoin, by = "participant") %>%
        mutate(
          ageByPretestBirthDate = round(
            interval(birthDate_pre, session_date) / years(1),
            2
          ),
          age = case_when(
            !is.na(ageByPretestBirthDate) ~ ageByPretestBirthDate,
            !is.na(Age_pre) & is.na(ageByPretestBirthDate) ~ Age_pre,
            is.na(birthDate_pre) & is.na(Age_pre) ~ age,
            .default = NA
          )
        ) %>%
        select(participant, age)
      data_list[[i]]$age <- participant_age$age[
        match(data_list[[i]]$participant, participant_age$participant)
      ]
    }

    df_parts[[i]] <- data_list[[i]] %>%
      transmute(
        participant = as.character(participant),
        ParticipantCode = as.character(ParticipantCode),
        # Sessions may store BirthMonthYear as char or numeric; unify for bind_rows.
        BirthMonthYear = as.character(BirthMonthYear),
        age = suppressWarnings(as.numeric(age))
      ) %>%
      distinct(participant, ParticipantCode, BirthMonthYear, age)
  }
  df <- bind_rows(df_parts)

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

