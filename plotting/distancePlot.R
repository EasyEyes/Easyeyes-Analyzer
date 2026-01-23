# Helper function for logarithmic jitter (unbiased for log scales)
add_log_jitter <- function(values, jitter_percent = 1, seed = 42) {
  # Apply logarithmic jitter for unbiased results on log scales
  # jitter_percent: percentage jitter (e.g., 1 for ±1%)
  set.seed(seed)
  log_max <- log10(1 + jitter_percent/100)
  log_min <- -log_max
  log_factor <- log_min + runif(length(values)) * (log_max - log_min)
  return(values * 10^log_factor)
}

# Helper to auto-compute plot height based on base height and legend complexity
compute_auto_height <- function(base_height, n_items, per_row, row_increase) {
  if (is.na(n_items) || length(n_items) == 0 || n_items <= 0) {
    return(base_height)
  }
  legend_rows <- ceiling(n_items / per_row)
  height_factor <- 1 + (legend_rows - 1) * row_increase
  return(base_height * height_factor)
}

# Safe helpers for building short, clean, single-string labels
safe_first <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != "" & x != "NA"]
  if (length(x)) x[1] else ""
}

safe_first_num <- function(x, digits = NULL) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (!length(x)) return("")
  if (is.null(digits)) as.character(x[1]) else format(round(x[1], digits), nsmall = digits)
}

coerce_to_logical <- function(x) {
  vapply(x, function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val)) return(NA)
    if (is.logical(val)) return(val[1])
    if (is.numeric(val)) return(!is.na(val[1]) && val[1] != 0)
    val_chr <- tolower(trimws(as.character(val[1])))
    val_chr <- gsub("\\[|\\]|\"|'", "", val_chr)
    val_chr <- strsplit(val_chr, ",")[[1]][1]
    val_chr <- trimws(val_chr)
    if (val_chr %in% c("true", "t", "1", "yes", "y")) return(TRUE)
    if (val_chr %in% c("false", "f", "0", "no", "n")) return(FALSE)
    return(NA)
  }, logical(1))
}

extract_width_px <- function(res_str) {
  vapply(res_str, function(val) {
    if (is.null(val) || length(val) == 0 || is.na(val) || val == "") return(NA_real_)
    clean <- gsub("\\[|\\]|\"|'", "", val[1])
    parts <- unlist(strsplit(clean, "[,xX ]+"))
    parts <- parts[nzchar(parts)]
    if (length(parts) == 0) return(NA_real_)
    nums <- as.numeric(parts)
    nums <- nums[is.finite(nums)]
    if (length(nums) == 0) return(NA_real_)
    if (length(nums) >= 2) {
      return(max(nums[1:2]))
    }
    return(nums[1])
  }, numeric(1))
}
# Helper function to extract parameters from distanceCalibrationJSON (top-level arrays)
# and populate the corresponding columns in the dataframe
extract_common_params_from_JSON <- function(df) {
  if (!"distanceCalibrationJSON" %in% names(df) || !"distanceCalibrationTJSON" %in% names(df)) return(df)
  if ("distanceCalibrationJSON" %in% names(df)) {
    raw_json <- get_first_non_na(df$distanceCalibrationJSON)
  } else {
    raw_json <- get_first_non_na(df$distanceCalibrationTJSON)
  }

  if (is.null(raw_json) || is.na(raw_json) || raw_json == "") return(df)
  
  tryCatch({
    json_txt <- sanitize_json_string(raw_json)
    parsed <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
    
    param_mapping <- list(
      "_calibrateDistance" = "_calibrateTrackDistance",
      "_calibrateDistancePupil" = "_calibrateTrackDistancePupil",
      "_calibrateDistanceAllowedRatio" = "_calibrateTrackDistanceAllowedRatio",
      "_calibrateDistanceShowLengthBool" = "_calibrateTrackDistanceShowLengthBool",
      "_calibrateDistanceTimes" = "_calibrateTrackDistanceTimes",
      "_viewingDistanceWhichEye" = "viewingDistanceWhichEye",
      "_viewingDistanceWhichPoint" = "viewingDistanceWhichPoint",
      "_calibrateScreenSizeAllowedRatio" = "calibrateScreenSizeAllowedRatio",
      "_calibrateScreenSizeTimes" = "calibrateScreenSizeTimes"
    )
    
    for (new_name in names(param_mapping)) {
      old_col <- param_mapping[[new_name]]
      val <- parsed[[new_name]]
      
      # If we have a value from the top-level array and the column is empty/missing, populate it
      if (!is.null(val) && length(val) > 0) {
        val_str <- as.character(val[1])
        if (!is.na(val_str) && val_str != "" && val_str != "NULL" && val_str != "null") {
          # Check if current column value is empty
          current_val <- if (old_col %in% names(df)) safe_first(df[[old_col]]) else ""
          if (current_val == "" || is.na(current_val)) {
            df[[old_col]] <- val_str
          }
        }
      }
    }
  }, error = function(e) {
    # If JSON parsing fails, return dataframe unchanged
  })
  
  return(df)
}

make_statement <- function(df) {
  paste(
    paste0("_calibrateDistance = ",               safe_first(df[["_calibrateTrackDistance"]])),
    paste0("_calibrateDistancePupil = ",          safe_first(df[["_calibrateTrackDistancePupil"]])),
    paste0("_calibrateDistanceAllowedRatio = ",   safe_first(df[["_calibrateTrackDistanceAllowedRatio"]])),
    paste0("_calibrateDistanceShowLengthBool = ", safe_first(df[["_calibrateTrackDistanceShowLengthBool"]])),
    paste0("_calibrateDistanceTimes = ",          safe_first(df[["_calibrateTrackDistanceTimes"]])),
    paste0("calibrateScreenSizeAllowedRatio = ",       safe_first(df[["calibrateScreenSizeAllowedRatio"]])),
    paste0("calibrateScreenSizeTimes = ",              safe_first(df[["calibrateScreenSizeTimes"]])),
    paste0("viewingDistanceWhichEye = ",               safe_first(df[["viewingDistanceWhichEye"]])),
    paste0("viewingDistanceWhichPoint = ",             safe_first(df[["viewingDistanceWhichPoint"]])),
    sep = "\n"
  )
}

# Build a small parameter table for in-plot display
build_param_table <- function(df) {
  safe_first <- function(x) {
    x <- as.character(x)
    x <- x[!is.na(x) & x != "" & x != "NA"]
    if (length(x)) x[1] else ""
  }
  # Display labels use new naming (without "Track"), but internal columns still use old names
  keys <- c(
    "_calibrateDistance",
    "_calibrateDistancePupil",
    "_calibrateDistanceAllowedRatio",
    "_calibrateDistanceShowLengthBool",
    "_calibrateDistanceTimes",
    "calibrateScreenSizeAllowedRatio",
    "calibrateScreenSizeTimes",
    "viewingDistanceWhichEye",
    "viewingDistanceWhichPoint"
  )
  vals <- c(
    safe_first(df[["_calibrateTrackDistance"]]),
    safe_first(df[["_calibrateTrackDistancePupil"]]),
    safe_first(df[["_calibrateTrackDistanceAllowedRatio"]]),
    safe_first(df[["_calibrateTrackDistanceShowLengthBool"]]),
    safe_first(df[["_calibrateTrackDistanceTimes"]]),
    safe_first(df[["calibrateScreenSizeAllowedRatio"]]),
    safe_first(df[["calibrateScreenSizeTimes"]]),
    safe_first(df[["viewingDistanceWhichEye"]]),
    safe_first(df[["viewingDistanceWhichPoint"]])
  )
  lines <- paste0(keys, " = ", vals)
  # return a one-column data.frame; header will be hidden in theme
  out <- data.frame(text = lines, stringsAsFactors = FALSE)
  out
}

sanitize_json_string <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  s <- trimws(as.character(x))
  if (nchar(s) >= 2 && substr(s, 1, 1) == '"' && substr(s, nchar(s), nchar(s)) == '"') {
    s <- substr(s, 2, nchar(s) - 1)
  }
  if (grepl('""', s, fixed = TRUE)) {
    s <- gsub('""', '"', s, fixed = TRUE)
  }
  s
}

get_cameraResolutionXY <- function(data_list) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)

  for (i in 1:length(data_list)) {
    # Skip if no participant data
    if (!"participant" %in% names(data_list[[i]]) ||
        all(is.na(data_list[[i]]$participant)) ||
        all(data_list[[i]]$participant == "")) {
      next
    }

    participant_id <- first(na.omit(data_list[[i]]$participant))
    factorVpxCm <- NA_real_

    # Compute factorVpxCm from distanceCalibrationJSON if available
    if ("distanceCalibrationJSON" %in% names(data_list[[i]]) ||
        "distanceCalibrationTJSON" %in% names(data_list[[i]])) {
      tryCatch({
        if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {
          raw_json <- get_first_non_na(data_list[[i]]$distanceCalibrationJSON)
        } else {
          raw_json <- get_first_non_na(data_list[[i]]$distanceCalibrationTJSON)
        }
        json_txt <- sanitize_json_string(raw_json)
        t_tjson <- fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
        
        # Extract metadata for debugging/logging
        json_experiment  <- if (!is.null(t_tjson$experiment)) t_tjson$experiment else NA_character_
        json_participant <- if (!is.null(t_tjson$participant)) t_tjson$participant else NA_character_
        json_date        <- if (!is.null(t_tjson$date)) t_tjson$date else NA_character_
        json_type        <- if (!is.null(t_tjson$json)) t_tjson$json else NA_character_
        
        message(sprintf("[DEBUG get_cameraResolutionXY] distanceCalibrationJSON metadata: experiment=%s, participant=%s, date=%s, json=%s",
                        json_experiment, json_participant, json_date, json_type))

        tmp <- tibble(
          # Renamed: rightEyeToFootCm -> objectBasedRightEyeToFootCm
          objectBasedRightEyeToFootCm = if (!is.null(t_tjson$objectBasedRightEyeToFootCm)) t_tjson$objectBasedRightEyeToFootCm else t_tjson$rightEyeToFootCm,
          # Renamed: leftEyeToFootCm -> objectBasedLeftEyeToFootCm
          objectBasedLeftEyeToFootCm  = if (!is.null(t_tjson$objectBasedLeftEyeToFootCm)) t_tjson$objectBasedLeftEyeToFootCm else t_tjson$leftEyeToFootCm,
          # Renamed: eyesToFootCm -> objectBasedEyesToFootCm (now provided directly in JSON)
          objectBasedEyesToFootCm     = if (!is.null(t_tjson$objectBasedEyesToFootCm)) t_tjson$objectBasedEyesToFootCm else t_tjson$eyesToFootCm,
          # New parameters: fOverWidth and ipdOverWidth (4 decimal digits)
          fOverWidth       = if (!is.null(t_tjson$fOverWidth)) as.numeric(t_tjson$fOverWidth) else NA_real_,
          ipdOverWidth     = if (!is.null(t_tjson$ipdOverWidth)) as.numeric(t_tjson$ipdOverWidth) else NA_real_,
          ipdCm            = if (!is.null(t_tjson$ipdCm)) as.numeric(t_tjson$ipdCm) else NA_real_
        ) %>%
        mutate(
          # Use objectBasedEyesToFootCm directly if available, otherwise compute from left/right
          eyeToFootCm = ifelse(!is.na(objectBasedEyesToFootCm), 
                               (as.numeric(objectBasedEyesToFootCm)),
                               (as.numeric(objectBasedLeftEyeToFootCm) + as.numeric(objectBasedRightEyeToFootCm)) / 2),
          factorVpxCm = fOverWidth * widthVpx * ipdCm
        )

        factorVpxCmVals <- tmp$factorVpxCm[!is.na(tmp$factorVpxCm)]
        if (length(factorVpxCmVals) >= 2) {
          factorVpxCm <- mean(tail(factorVpxCmVals, 2))
        } else if (length(factorVpxCmVals) == 1) {
          factorVpxCm <- factorVpxCmVals[1]
        }
      }, error = function(e) {})
    }

    # Select and filter data
    t <- data_list[[i]] %>%
      mutate(factorVpxCm = !!factorVpxCm) %>%
      select(participant, 
             `_calibrateTrackDistance`,
              `_calibrateTrackDistancePupil`,
              factorVpxCm,
              cameraResolutionXY) %>%
              rename(PavloviaParticipantID = participant,
                     `_calibrateDistancePupil` = `_calibrateTrackDistancePupil`,
                     `_calibrateDistance` = `_calibrateTrackDistance`) %>%
              distinct() %>%
      filter(!is.na(cameraResolutionXY), cameraResolutionXY != "", !is.na(factorVpxCm)) %>%
      mutate(widthVpx = extract_width_px(cameraResolutionXY), # take largest of first two resolution entries as horizontal width
             widthVpx = as.numeric(widthVpx)
          )
            
            if (nrow(t) > 0) {
              df <- rbind(df, t)
            }
          }

  return(df %>% distinct())
}

# Helper function to extract raw pxPerCm array from calibrateScreenSizeJSON
get_raw_pxPerCm_data <- function(data_list, sizeCheck) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)
  
  for (i in 1:length(data_list)) {
    if ("calibrateScreenSizeJSON" %in% names(data_list[[i]])) {
      # Skip if participant column doesn't exist
      if (!"participant" %in% names(data_list[[i]]) || 
          all(is.na(data_list[[i]]$participant)) || 
          all(data_list[[i]]$participant == "")) {
        next
      }
      
      tryCatch({
        participant_id <- first(na.omit(data_list[[i]]$participant))
        
        raw_json <- get_first_non_na(data_list[[i]]$calibrateScreenSizeJSON)
        json_txt <- sanitize_json_string(raw_json)
        screenSizeJSON <- fromJSON(
          json_txt,
          simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
        )
        
        # Extract pxPerCm or convert from ppi
        pxPerCm_vals <- NULL
        if (!is.null(screenSizeJSON$pxPerCm)) {
          pxPerCm_vals <- as.numeric(screenSizeJSON$pxPerCm)
        } else if (!is.null(screenSizeJSON$ppi)) {
          pxPerCm_vals <- as.numeric(screenSizeJSON$ppi) / 2.54
        }
        
        # Extract requestedCm values (credit card dimensions)
        requestedCm_vals <- NULL
        if (!is.null(screenSizeJSON$requestedCm)) {
          requestedCm_vals <- as.numeric(screenSizeJSON$requestedCm)
        }
        
        if (!is.null(pxPerCm_vals) && length(pxPerCm_vals) > 0) {
          # Get median of SizeCheckEstimatedPxPerCm for this participant
          participant_sizeCheck <- sizeCheck %>%
            filter(participant == participant_id, !is.na(SizeCheckEstimatedPxPerCm), is.finite(SizeCheckEstimatedPxPerCm))
          
          if (nrow(participant_sizeCheck) > 0) {
            medianEstimated <- median(participant_sizeCheck$SizeCheckEstimatedPxPerCm, na.rm = TRUE)
            
            # Create one row per measurement, including requestedCm if available
            if (!is.null(requestedCm_vals) && length(requestedCm_vals) == length(pxPerCm_vals)) {
              t <- tibble(
                participant = participant_id,
                pxPerCm = pxPerCm_vals,
                requestedCm = requestedCm_vals,
                medianEstimated = medianEstimated,
                relative = pxPerCm / medianEstimated
              ) %>%
                filter(is.finite(relative), relative > 0)
            } else {
              # Fallback: use default credit card dimensions (width=8.56, height=5.398)
              default_sizes <- c(8.56, 5.398)
              if (length(pxPerCm_vals) <= 2) {
                requestedCm_vals <- default_sizes[1:length(pxPerCm_vals)]
              } else {
                requestedCm_vals <- rep(8.56, length(pxPerCm_vals))
              }
              t <- tibble(
                participant = participant_id,
                pxPerCm = pxPerCm_vals,
                requestedCm = requestedCm_vals,
                medianEstimated = medianEstimated,
                relative = pxPerCm / medianEstimated
              ) %>%
                filter(is.finite(relative), relative > 0)
            }
            
            if (nrow(t) > 0) {
              df <- rbind(df, t)
            }
          }
        }
      }, error = function(e) {
        # Skip if JSON parsing fails
      })
    }
  }
  return(df)
}

# Helper function to extract raw objectMeasuredCm array from distanceCalibrationJSON (or distanceCalibrationTJSON)
get_raw_objectMeasuredCm_data <- function(data_list) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)
  
  for (i in 1:length(data_list)) {
    if ("distanceCalibrationJSON" %in% names(data_list[[i]]) ||
        "distanceCalibrationTJSON" %in% names(data_list[[i]])) {
      # Skip if participant column doesn't exist
      if (!"participant" %in% names(data_list[[i]]) || 
          all(is.na(data_list[[i]]$participant)) || 
          all(data_list[[i]]$participant == "")) {
        next
      }
      
      tryCatch({
        participant_id <- first(na.omit(data_list[[i]]$participant))
        
        # Try distanceCalibrationJSON first, then distanceCalibrationTJSON as fallback
        raw_json <- if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {
          get_first_non_na(data_list[[i]]$distanceCalibrationJSON)
        } else {
          get_first_non_na(data_list[[i]]$distanceCalibrationTJSON)
        }
        json_txt <- sanitize_json_string(raw_json)
        distanceCalibJSON <- fromJSON(
          json_txt,
          simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
        )
        
        # Extract objectMeasuredCm from COMMON section
        objectMeasuredCm_vals <- NULL
        if (!is.null(distanceCalibJSON$COMMON) && 
            !is.null(distanceCalibJSON$COMMON$objectMeasuredCm)) {
          objectMeasuredCm_vals <- as.numeric(distanceCalibJSON$COMMON$objectMeasuredCm)
        }
        
        objectMeasuredCm_vals <- objectMeasuredCm_vals[!is.na(objectMeasuredCm_vals)]
        
        if (length(objectMeasuredCm_vals) > 0) {
          # Calculate median for this participant
          median_val <- median(objectMeasuredCm_vals, na.rm = TRUE)
          
          # Create one row per measurement
          t <- tibble(
            participant = participant_id,
            objectMeasuredCm = objectMeasuredCm_vals,
            medianObjectCm = median_val,
            relative = objectMeasuredCm / median_val
          ) %>%
            filter(is.finite(relative), relative > 0)
          
          if (nrow(t) > 0) {
            df <- rbind(df, t)
          }
        }
      }, error = function(e) {
        # Skip if JSON parsing fails
      })
    }
  }
  return(df)
}

# helper function to extract camera resolution width SD and count from JSONs
# returns a tibble with PavloviaParticipantID, cameraResolutionXSD, cameraResolutionN
get_camera_resolution_stats <- function(data_list) {
  result <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(result)
  
  for (i in seq_along(data_list)) {
    dl <- data_list[[i]]
    
    # get participant ID
    if (!"participant" %in% names(dl)) next
    participant_id <- get_first_non_na(dl$participant)
    if (is.null(participant_id) || is.na(participant_id) || participant_id == "") next
   
    all_widths <- c()
    
    # extract from distanceCalibrationJSON
    if ("distanceCalibrationJSON" %in% names(dl) ||
        "distanceCalibrationTJSON" %in% names(dl)) {
      tryCatch({
        if ("distanceCalibrationJSON" %in% names(dl)) {
          raw_json <- get_first_non_na(dl$distanceCalibrationJSON)
        } else {
          raw_json <- get_first_non_na(dl$distanceCalibrationTJSON)
        }
        
        if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
          json_txt <- sanitize_json_string(raw_json)
          parsed <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          
          if (!is.null(parsed$cameraResolutionXYVpx)) {
            res_array <- parsed$cameraResolutionXYVpx
            # handle matrix format [[w1,h1],[w2,h2],...] 
            if (is.matrix(res_array)) {
              widths <- res_array[, 1]
              all_widths <- c(all_widths, as.numeric(widths))
            } else if (is.list(res_array)) {
              for (r in res_array) {
                if (length(r) >= 1) {
                  all_widths <- c(all_widths, as.numeric(r[1]))
                }
              }
            }
          }
        }
      }, error = function(e) {})
    }
    
    # Also extract from distanceCheckJSON if available
    if ("distanceCheckJSON" %in% names(dl)) {
      tryCatch({
        raw_json <- get_first_non_na(dl$distanceCheckJSON)
        if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
          json_txt <- sanitize_json_string(raw_json)
          parsed <- jsonlite::fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          
          if (!is.null(parsed$cameraResolutionXYVpx)) {
            res_array <- parsed$cameraResolutionXYVpx
            if (is.matrix(res_array)) {
              widths <- res_array[, 1]
              all_widths <- c(all_widths, as.numeric(widths))
            } else if (is.list(res_array)) {
              for (r in res_array) {
                if (length(r) >= 1) {
                  all_widths <- c(all_widths, as.numeric(r[1]))
                }
              }
            }
          }
        }
      }, error = function(e) {})
    }
    
    # Calculate stats
    all_widths <- all_widths[!is.na(all_widths) & is.finite(all_widths)]
    n_resolutions <- length(all_widths)
    
    if (n_resolutions > 0) {
      # SD is 0 if only one value, otherwise calculate
      sd_width <- if (n_resolutions > 1) sd(all_widths) else 0
      
      result <- rbind(result, tibble(
        PavloviaParticipantID = participant_id,
        cameraResolutionXSD = round(sd_width, 2),
        cameraResolutionN = n_resolutions
      ))
    }
  }
  
  return(result %>% distinct())
}

get_merged_participant_distance_info <- function(data_or_results, participant_info) {
  # Get camera resolution data (support both raw data_list and precomputed results)
  camera_data <- if (is.list(data_or_results) && "camera" %in% names(data_or_results)) {
    data_or_results$camera
  } else {
    get_cameraResolutionXY(data_or_results)
  }
  
  # Get camera resolution SD stats
  camera_res_stats <- if (is.list(data_or_results) && "camera_res_stats" %in% names(data_or_results)) {
    data_or_results$camera_res_stats
  } else if (is.list(data_or_results) && "filtered" %in% names(data_or_results)) {
    get_camera_resolution_stats(data_or_results$filtered)
  } else {
    get_camera_resolution_stats(data_or_results)
  }
  
  # If no participant_info provided, return just camera data
  if (is.null(participant_info) || nrow(participant_info) == 0) {
    return(camera_data)
  }
  
  # If no camera data, return just participant info with renamed ID column
  if (nrow(camera_data) == 0) {
    if ("PavloviaParticipantID" %in% names(participant_info)) {
      return(participant_info)
    } else if ("participant" %in% names(participant_info)) {
      return(participant_info %>% rename(PavloviaParticipantID = participant))
    } else {
      return(participant_info)
    }
  }
  
  # Prepare participant_info for merging
  participant_info_clean <- participant_info
  
  # Perform full outer join to include all participants from both tables
  merged_data <- camera_data %>%
    left_join(participant_info_clean, by = "PavloviaParticipantID")
  
    
     
   # Use fOverWidth directly from TJSON (distanceCalibrationJSON now provides fOverWidth instead of fVpx)
   calib_fOverWidth <- data_or_results$TJSON %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      rename(PavloviaParticipantID = participant) %>%
      group_by(PavloviaParticipantID) %>%
      summarize(fOverWidth_calibration = median(fOverWidth, na.rm = TRUE), .groups = "drop")

    # checkJSON now has fOverWidth directly (computed from ipdOverWidth)
    check_fOverWidth <- data_or_results$checkJSON %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      rename(PavloviaParticipantID = participant) %>%
      group_by(PavloviaParticipantID) %>%
      summarize(fOverWidth_check = median(fOverWidth, na.rm = TRUE), .groups = "drop")
    
      # Join calibration and check data by participant
    plot_data <- calib_fOverWidth %>%
      inner_join(check_fOverWidth, by = "PavloviaParticipantID") %>%
      filter(!is.na(fOverWidth_calibration), !is.na(fOverWidth_check),
             is.finite(fOverWidth_calibration), is.finite(fOverWidth_check))

  merged_data <- merged_data %>%
    left_join(plot_data, by = "PavloviaParticipantID")

  # Join with camera resolution stats (SD and count)
  if (nrow(camera_res_stats) > 0) {
    merged_data <- merged_data %>%
      left_join(camera_res_stats, by = "PavloviaParticipantID")
  } else {
    merged_data <- merged_data %>%
      mutate(cameraResolutionXSD = NA_real_, cameraResolutionN = NA_integer_)
  }

  # Ensure 'ok' column exists to avoid errors in case_when
  if (!"ok" %in% names(merged_data)) {
    merged_data <- merged_data %>% mutate(ok = NA_character_)
  }

  merged_data <- merged_data %>%
    mutate(
      ok_priority = case_when(
        ok == "✅" ~ 1,
        ok == "🚧" ~ 2,
        ok == "❌" ~ 3,
        is.na(ok) ~ 4,
        .default = 5
      )
    ) %>%
    mutate(
      # fOverWidth now comes directly from TJSON (distanceCalibrationJSON)
      `fOverWidth` = round(fOverWidth_calibration, 4),
      # Calculate fOverWidth calibration/check: ratio of calibration to check values
      calibration_check_ratio_tmp = ifelse(!is.na(fOverWidth_calibration) & !is.na(fOverWidth_check) & fOverWidth_check != 0,
                                          fOverWidth_calibration / fOverWidth_check, NA_real_),
      `fOverWidth calibration/check` = ifelse(is.na(calibration_check_ratio_tmp), NA_character_, format(round(calibration_check_ratio_tmp, 3), nsmall = 3))
    ) %>%
    select(-fOverWidth_calibration, -fOverWidth_check, -calibration_check_ratio_tmp, -factorVpxCm) %>%
    arrange(ok_priority, PavloviaParticipantID) %>%
    select(-ok_priority) %>%
    # Move ok, device type, system, browser, Prolific min immediately after PavloviaParticipantID
    {if("ok" %in% names(.)) relocate(., ok, .after = PavloviaParticipantID) else .} %>%
    {if("device type" %in% names(.)) relocate(., `device type`, .after = ok) else .} %>%
    {if("system" %in% names(.)) relocate(., system, .after = `device type`) else .} %>%
    {if("browser" %in% names(.)) relocate(., browser, .after = system) else .} %>%
    {if("Prolific min" %in% names(.)) relocate(., `Prolific min`, .after = browser) else .} %>%
    # Delete cameraResolutionN column
    {if("cameraResolutionN" %in% names(.)) select(., -cameraResolutionN) else .} %>%
    # Move the two fOverWidth columns side by side
    {if("fOverWidth" %in% names(.) && "fOverWidth calibration/check" %in% names(.)) 
       relocate(., `fOverWidth calibration/check`, .after = `fOverWidth`)
     else .} %>%
    # Move screenResolutionXY before cameraResolutionXY if both exist (do this first)
    {if("screenResolutionXY" %in% names(.) && "cameraResolutionXY" %in% names(.)) 
       relocate(., screenResolutionXY, .before = cameraResolutionXY)
     else .} %>%
    # Move pxPerCm before screenResolutionXY
    {if("pxPerCm" %in% names(.) && "screenResolutionXY" %in% names(.)) 
       relocate(., pxPerCm, .before = screenResolutionXY)
     else .} %>%
    # Move screenWidthCm before pxPerCm
    {if("screenWidthCm" %in% names(.) && "pxPerCm" %in% names(.)) 
       relocate(., screenWidthCm, .before = pxPerCm)
     else .} %>%
    # Move cameraResolutionXSD after cameraResolutionXY if it exists
    {if("cameraResolutionXY" %in% names(.) && "cameraResolutionXSD" %in% names(.)) 
       relocate(., cameraResolutionXSD, .after = cameraResolutionXY)
     else .} %>%
    # Move Object before objectLengthCm
    {if("Object" %in% names(.) && "objectLengthCm" %in% names(.)) 
       relocate(., Object, .before = objectLengthCm)
     else .} %>%
    # Move Comment column to the end if it exists
    {if("Comment" %in% names(.)) relocate(., Comment, .after = last_col()) else .}
  
  
  return(merged_data)
}

get_distance_calibration <- function(data_list, minRulerCm) {
  if (length(data_list) == 0) {
    return(list(
      filtered = list(),
      sizeCheck = tibble(),
      distance = tibble(),
      eye_feet = tibble(),
      feet_calib = tibble(),
      feet_check = tibble(),
      check_factor = tibble(),
      camera = tibble(),
      camera_res_stats = tibble(),
      raw_pxPerCm = tibble(),
      raw_objectMeasuredCm = tibble(),
      raw_fVpx = tibble(),
      statement = "",
      TJSON = tibble(),
      checkJSON = tibble()
    ))
  }
  
  # Get participant ruler info to determine who to exclude
  participant_info_list <- list()
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant, rulerLength, rulerUnit) %>%
      distinct() %>%
      filter(!is.na(rulerLength), !is.na(rulerUnit))
    
    if (nrow(t) > 0) {
      participant_info_list[[length(participant_info_list) + 1]] <- t
    }
  }
  
  if (length(participant_info_list) == 0) {
    return(list(
      filtered = list(),
      sizeCheck = tibble(),
      distance = tibble(),
      eye_feet = tibble(),
      feet_calib = tibble(),
      feet_check = tibble(),
      check_factor = tibble(),
      camera = tibble(),
      camera_res_stats = tibble(),
      raw_pxPerCm = tibble(),
      raw_objectMeasuredCm = tibble(),
      raw_fVpx = tibble(),
      statement = "",
      TJSON = tibble(),
      checkJSON = tibble()
    ))
  }
  
  # Combine and process ruler information
  participant_info <- do.call(rbind, participant_info_list) %>%
    distinct() %>%
    mutate(
      # Convert ruler length to cm
      rulerCm = case_when(
        !is.na(rulerLength) & rulerUnit == "cm" ~ rulerLength,
        !is.na(rulerLength) & rulerUnit == "inches" ~ rulerLength * 2.54,
        .default = NA_real_
      )
    ) %>%
    group_by(participant) %>%
    summarize(
      rulerCm = first(rulerCm[!is.na(rulerCm)]),
      .groups = "drop"
    ) %>%
    filter(rulerCm >= minRulerCm)
  
  # Filter the original data_list to only include participants with acceptable ruler lengths
  valid_participants <- participant_info$participant
  
  if (length(valid_participants) == 0) {
    return(list(
      filtered = list(),
      sizeCheck = tibble(),
      distance = tibble(),
      eye_feet = tibble(),
      feet_calib = tibble(),
      feet_check = tibble(),
      check_factor = tibble(),
      camera = tibble(),
      camera_res_stats = tibble(),
      raw_pxPerCm = tibble(),
      raw_objectMeasuredCm = tibble(),
      raw_fVpx = tibble(),
      statement = "",
      TJSON = tibble(),
      checkJSON = tibble()
    ))
  }
  
  filtered_data_list <- list()
  for (i in 1:length(data_list)) {
    filtered_data <- data_list[[i]] %>%
      filter(participant %in% valid_participants)
    
    if (nrow(filtered_data) > 0) {
      # Extract COMMON parameters from distanceCalibrationJSON to populate empty columns
      filtered_data <- extract_common_params_from_JSON(filtered_data)
      filtered_data_list[[length(filtered_data_list) + 1]] <- filtered_data
    }
  }
  
  # Compute all derived datasets once (single pass per session where possible)
  sizeCheck <- tibble()
  distance <- tibble()
  eye_feet <- tibble()
  feet_calib <- tibble()
  feet_check <- tibble()  # Separate tibble for check foot position data
  check_factor <- tibble()
  camera <- get_cameraResolutionXY(filtered_data_list)  # reused elsewhere; keep helper
  blindspot <- tibble()
  TJSON <- tibble()
  checkJSON <- tibble()
  
  for (i in seq_along(filtered_data_list)) {
    dl <- filtered_data_list[[i]]
    
    # -------- sizeCheck (credit card + production) --------
    if (all(c('SizeCheckEstimatedPxPerCm','SizeCheckRequestedCm') %in% names(dl))) {
      t_sc <- dl %>%
        select(`_calibrateTrackDistance`,
               `_calibrateTrackDistancePupil`,
               `_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               viewingDistanceWhichEye,
               viewingDistanceWhichPoint,
               participant,
               SizeCheckEstimatedPxPerCm,
               SizeCheckRequestedCm,
               rulerLength,
               rulerUnit,
               pxPerCm) %>%
        mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
               `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
               `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
               calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
               calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
               `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
               `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
               `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
               `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)) %>%
        distinct() %>%
        filter(!is.na(SizeCheckEstimatedPxPerCm),
               !is.na(SizeCheckRequestedCm),
               SizeCheckEstimatedPxPerCm != '',
               SizeCheckRequestedCm != '')
      if (nrow(t_sc) > 0) {
        t_sc <- t_sc %>%
          mutate(
            SizeCheckEstimatedPxPerCm = gsub("\\[|\\]|\"", "", SizeCheckEstimatedPxPerCm),
            SizeCheckRequestedCm = gsub("\\[|\\]|\"", "", SizeCheckRequestedCm)
          ) %>%
          mutate(measured_list = strsplit(SizeCheckEstimatedPxPerCm, ","), 
                 requested_list = strsplit(SizeCheckRequestedCm, ",")) %>%
          unnest(c(measured_list, requested_list)) %>%
          mutate(
            SizeCheckEstimatedPxPerCm = as.numeric(trimws(measured_list)),
            SizeCheckRequestedCm = as.numeric(trimws(requested_list))
          ) %>%
          select(-measured_list, -requested_list)
        if (nrow(t_sc) > 0) {
          t_sc <- t_sc %>% mutate(
            SizeCheckEstimatedPxPerCm = as.numeric(SizeCheckEstimatedPxPerCm),
            SizeCheckRequestedCm = as.numeric(SizeCheckRequestedCm)
          )
          sizeCheck <- rbind(sizeCheck, t_sc)
        }
      }
    }
    
    # -------- measured distance (credit card) --------
    if ('calibrateTrackDistanceIpdCameraPx' %in% names(dl)) {
      dl$calibrateTrackDistanceIpdVpx <- dl$calibrateTrackDistanceIpdCameraPx
    }
    if (all(c('calibrateTrackDistanceMeasuredCm','calibrateTrackDistanceRequestedCm') %in% names(dl))) {
      t_dist <- dl %>%
        select(`_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               `_calibrateTrackDistance`,
               `_calibrateTrackDistancePupil`,
               viewingDistanceWhichEye,
               viewingDistanceWhichPoint,
               participant,
               calibrateTrackDistanceMeasuredCm,
               calibrateTrackDistanceRequestedCm,
               calibrateTrackDistanceIpdVpx) %>%
        mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
               `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
               `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
               calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
               calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
               `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
               `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
               `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
               `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)) %>%
        filter(!is.na(calibrateTrackDistanceMeasuredCm),
               !is.na(calibrateTrackDistanceRequestedCm),
               calibrateTrackDistanceMeasuredCm != '',
               calibrateTrackDistanceRequestedCm != '') %>%
        distinct()
      if (nrow(t_dist) > 0) {
        t_dist <- t_dist %>%
          mutate(
            calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
            calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm),
            calibrateTrackDistanceIpdVpx = gsub("\\[|\\]|\"", "", calibrateTrackDistanceIpdVpx)
          ) %>%
          mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","),
                 requested_list = strsplit(calibrateTrackDistanceRequestedCm, ","),
                 ipd_list = strsplit(calibrateTrackDistanceIpdVpx, ",")) %>%
          unnest(c(measured_list, requested_list, ipd_list)) %>%
          mutate(
            calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
            calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
            calibrateTrackDistanceIpdVpx = as.numeric(trimws(ipd_list))
          ) %>%
          select(-measured_list, -requested_list, -ipd_list) %>%
          mutate(
            calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
            calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm),
            calibrateTrackDistanceIpdVpx = if ("calibrateTrackDistanceIpdVpx" %in% names(.)) calibrateTrackDistanceIpdVpx else NA,
            order = row_number()
          )
        distance <- rbind(distance, t_dist)
      }
    }
    
    # -------- eye feet position (runtime XYPx field) --------
    if ("calibrateTrackDistanceEyeFeetXYPx" %in% names(dl)) {
      t_raw <- dl %>%
        select(experiment, participant,
               `_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               `_calibrateTrackDistance`,
               `_calibrateTrackDistancePupil`,
               viewingDistanceWhichEye,
               viewingDistanceWhichPoint,
               pxPerCm, screenWidthPx, screenHeightPx,
               calibrateTrackDistanceMeasuredCm,
               calibrateTrackDistanceRequestedCm,
               calibrateTrackDistanceEyeFeetXYPx) %>%
        mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
               `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
               `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
               calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
               calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
               `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
               `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
               `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
               `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)) %>%
        distinct()
      has_eye_feet_col <- "calibrateTrackDistanceEyeFeetXYPx" %in% names(t_raw)
      
      if (nrow(t_raw) > 0 && has_eye_feet_col && !all(is.na(t_raw$calibrateTrackDistanceEyeFeetXYPx))) {
        eye_feet_raw <- t_raw$calibrateTrackDistanceEyeFeetXYPx[1]
        coords_str <- gsub("\\[|\\]", "", eye_feet_raw)
        coords_numbers <- as.numeric(unlist(strsplit(coords_str, ",")))
        coords_numbers <- coords_numbers[!is.na(coords_numbers)]
        if (length(coords_numbers) %% 4 == 0) {
          n_coord_measurements <- length(coords_numbers) / 4
          t <- t_raw %>%
            filter(!is.na(calibrateTrackDistanceMeasuredCm),
                   !is.na(calibrateTrackDistanceRequestedCm),
                   !is.na(calibrateTrackDistanceEyeFeetXYPx),
                   calibrateTrackDistanceMeasuredCm != '',
                   calibrateTrackDistanceRequestedCm != '',
                   calibrateTrackDistanceEyeFeetXYPx != '') %>%
            mutate(
              calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
              calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm)
            ) %>%
            mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","), 
                   requested_list = strsplit(calibrateTrackDistanceRequestedCm, ",")) %>%
            unnest(c(measured_list, requested_list)) %>%
            mutate(
              calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
              calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
              measurement_index = row_number()
            ) %>%
            select(-measured_list, -requested_list)
          coords_df <- tibble(
            measurement_idx = rep(1:n_coord_measurements, each = 4),
            coord_type = rep(c("left_x", "left_y", "right_x", "right_y"), n_coord_measurements),
            coord_value = coords_numbers
          ) %>% pivot_wider(names_from = coord_type, values_from = coord_value)
          if (n_coord_measurements > nrow(t)) {
            coords_df <- coords_df[1:nrow(t), ]
          } else if (n_coord_measurements < nrow(t)) {
            t <- t[1:n_coord_measurements, ]
          }
          t <- cbind(t, coords_df) %>%
            select(-measurement_idx) %>%
            mutate(
              avg_eye_x_px = (left_x + right_x) / 2,
              avg_eye_y_px = (left_y + right_y) / 2,
              distance_ratio = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
            )
          if ("pxPerCm" %in% names(t) && !all(is.na(t$pxPerCm))) {
            t <- t %>% mutate(pxPerCm = as.numeric(pxPerCm),
                              avg_eye_x_cm = avg_eye_x_px / pxPerCm,
                              avg_eye_y_cm = avg_eye_y_px / pxPerCm)
          } else {
            default_pxPerCm <- 37.8
            t <- t %>% mutate(pxPerCm = default_pxPerCm,
                              avg_eye_x_cm = avg_eye_x_px / default_pxPerCm,
                              avg_eye_y_cm = avg_eye_y_px / default_pxPerCm)
          }
          t <- t %>% mutate(data_list_index = i)
          eye_feet <- rbind(eye_feet, t)
        }
      }
    }
    
    # -------- feet position during calibration (from distanceCalibrationJSON) --------
    if ("distanceCalibrationJSON" %in% names(dl) ||
        "distanceCalibrationTJSON" %in% names(dl)) {
      json_col <- if ("distanceCalibrationJSON" %in% names(dl)) "distanceCalibrationJSON" else "distanceCalibrationTJSON"
      t_meta <- dl %>%
        select(participant,
               `_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               `_calibrateTrackDistance`,
               `_calibrateTrackDistancePupil`,
               viewingDistanceWhichEye,
               viewingDistanceWhichPoint,
               pxPerCm, screenWidthPx, screenHeightPx,
               calibrateTrackDistanceMeasuredCm,
               calibrateTrackDistanceRequestedCm,
               all_of(json_col)) %>%
        mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
               `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
               `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
               calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
               calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
               `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
               `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
               `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
               `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)) %>%
        distinct()

      json_col <- if ("distanceCalibrationJSON" %in% names(dl)) "distanceCalibrationJSON" else "distanceCalibrationTJSON"
      tryCatch({
          raw_json <- get_first_non_na(t_meta[[json_col]])
          json_txt <- sanitize_json_string(raw_json)
          distanceCalibration <- fromJSON(
            json_txt,
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
          )
          # feet coords - distanceCalibrationJSON has arrays at top level
          eye_feet_coords <- list()
          spot_degrees <- c()
          
          # Extract leftEyeFootXYPx and rightEyeFootXYPx arrays
          left_eye_foot <- distanceCalibration$leftEyeFootXYPx
          right_eye_foot <- distanceCalibration$rightEyeFootXYPx
          spot_deg_array <- distanceCalibration$spotDeg
          
          if (!is.null(left_eye_foot) && !is.null(right_eye_foot)) {
            # Handle both matrix format [[x1,y1],[x2,y2],...] and list format
            if (is.matrix(left_eye_foot)) {
              n_coords <- nrow(left_eye_foot)
              for (j in 1:n_coords) {
                eye_feet_coords[[j]] <- list(
                  left_x = left_eye_foot[j, 1],
                  left_y = left_eye_foot[j, 2],
                  right_x = right_eye_foot[j, 1],
                  right_y = right_eye_foot[j, 2]
                )
              }
            } else if (is.list(left_eye_foot) && length(left_eye_foot) > 0) {
              n_coords <- length(left_eye_foot)
              for (j in 1:n_coords) {
                eye_feet_coords[[j]] <- list(
                  left_x = left_eye_foot[[j]][1],
                  left_y = left_eye_foot[[j]][2],
                  right_x = right_eye_foot[[j]][1],
                  right_y = right_eye_foot[[j]][2]
                )
              }
            }
          }
          
          if (!is.null(spot_deg_array)) {
            spot_degrees <- as.numeric(spot_deg_array)
          }
          
          if (length(eye_feet_coords) > 0) {
            coords_df <- tibble(
              measurement_idx = 1:length(eye_feet_coords),
              left_x = sapply(eye_feet_coords, function(x) x$left_x),
              left_y = sapply(eye_feet_coords, function(x) x$left_y),
              right_x = sapply(eye_feet_coords, function(x) x$right_x),
              right_y = sapply(eye_feet_coords, function(x) x$right_y)
            )
            t <- t_meta %>%
              filter(!is.na(calibrateTrackDistanceMeasuredCm),
                     !is.na(calibrateTrackDistanceRequestedCm),
                     calibrateTrackDistanceMeasuredCm != '',
                     calibrateTrackDistanceRequestedCm != '') %>%
              mutate(
                calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
                calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm)
              ) %>%
              mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","), 
                     requested_list = strsplit(calibrateTrackDistanceRequestedCm, ",")) %>%
              unnest(c(measured_list, requested_list)) %>%
              mutate(
                calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
                calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
                measurement_index = row_number()
              ) %>%
              select(-measured_list, -requested_list)
            n_measurements <- nrow(coords_df)
            if (n_measurements > nrow(t)) {
              coords_df <- coords_df[1:nrow(t), ]
            } else if (n_measurements < nrow(t)) {
              t <- t[1:n_measurements, ]
            }
            t <- cbind(t, coords_df) %>%
              select(-measurement_idx) %>%
              mutate(
                avg_eye_x_px = (left_x + right_x) / 2,
                avg_eye_y_px = (left_y + right_y) / 2,
                distance_ratio = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
              )
            if ("pxPerCm" %in% names(t) && !all(is.na(t$pxPerCm))) {
              t <- t %>% mutate(pxPerCm = as.numeric(pxPerCm),
                                avg_eye_x_cm = avg_eye_x_px / pxPerCm,
                                avg_eye_y_cm = avg_eye_y_px / pxPerCm)
            } else {
              default_pxPerCm <- 37.8
              t <- t %>% mutate(pxPerCm = default_pxPerCm,
                                avg_eye_x_cm = avg_eye_x_px / default_pxPerCm,
                                avg_eye_y_cm = avg_eye_y_px / default_pxPerCm)
            }
            t <- t %>% mutate(data_list_index = i)
            feet_calib <- rbind(feet_calib, t)
          }
          if (length(spot_degrees) > 0 && !all(is.na(spot_degrees))) {
            t_bs <- t_meta[1,] %>% 
              mutate(`_calibrateTrackDistanceBlindspotDiameterDeg` = spot_degrees[1]) %>%
              select(participant, `_calibrateTrackDistanceBlindspotDiameterDeg`)
            blindspot <- rbind(blindspot, t_bs)
          }
        }, error = function(e) {})
      }
   
    if ("distanceCheckJSON" %in% names(dl)) {
      
      check_bool_val <- NA
      if ("_calibrateTrackDistanceCheckBool" %in% names(dl)) {
        check_bool_val <- coerce_to_logical(get_first_non_na(dl$`_calibrateTrackDistanceCheckBool`))
      }
        raw_json <- get_first_non_na(dl$distanceCheckJSON)
        json_txt <- sanitize_json_string(raw_json)
        distanceCheck <- fromJSON(
          json_txt,
          simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
        )
        
        # Extract metadata from distanceCheckJSON for debugging/logging
        check_json_experiment  <- if (!is.null(distanceCheck$experiment)) distanceCheck$experiment else NA_character_
        check_json_participant <- if (!is.null(distanceCheck$participant)) distanceCheck$participant else NA_character_
        check_json_date        <- if (!is.null(distanceCheck$date)) distanceCheck$date else NA_character_
        check_json_type        <- if (!is.null(distanceCheck$json)) distanceCheck$json else NA_character_
        
        message(sprintf("[DEBUG distanceCheckJSON] metadata: experiment=%s, participant=%s, date=%s, json=%s",
                        check_json_experiment, check_json_participant, check_json_date, check_json_type))
        
      raw_json <- get_first_non_na(t$distanceCalibrationJSON)
      json_txt <- sanitize_json_string(raw_json)
      t_tjson <- fromJSON(
        json_txt,
        simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
      )
      # -------- distance check median factorVpxCm --------
      tryCatch({
        participant_id_debug <- if("participant" %in% names(dl)) first(na.omit(dl$participant)) else "UNKNOWN"
        measured_vals <- NULL
        if (!is.null(distanceCheck$measuredFactorVpxCm)) {
          # Use measuredFactorVpxCm directly from JSON (consistent with how calibration uses factorVpxCm)
          measured_vals <- distanceCheck$measuredFactorVpxCm
        } else if (!is.null(distanceCheck$measuredFactorCameraPxCm)) {
          measured_vals <- distanceCheck$measuredFactorCameraPxCm
        }
        
        if (!is.null(measured_vals)) {
          measuredFactorVpxCmVals <- as.numeric(measured_vals)
          measuredFactorVpxCmVals <- measuredFactorVpxCmVals[!is.na(measuredFactorVpxCmVals)]
          
          if (length(measuredFactorVpxCmVals) > 0) {
            medianFactorVpxCm <- median(measuredFactorVpxCmVals)
            
            t <- dl %>%
              mutate(medianFactorVpxCm = medianFactorVpxCm,
                     calibrateTrackDistanceCheckBool = check_bool_val) %>%
              select(participant, medianFactorVpxCm, calibrateTrackDistanceCheckBool) %>%
              rename(PavloviaParticipantID = participant) %>%
              distinct() %>%
              filter(!is.na(medianFactorVpxCm))
            if (nrow(t) > 0) check_factor <- rbind(check_factor, t)
          }
        }
        
        # -------- ALSO extract foot positions from distanceCheckJSON for feet_calib --------
        # distanceCheckJSON has leftEyeFootXYPx and rightEyeFootXYPx arrays
        if (!is.null(distanceCheck$leftEyeFootXYPx) && !is.null(distanceCheck$rightEyeFootXYPx)) {
          left_foot <- distanceCheck$leftEyeFootXYPx
          right_foot <- distanceCheck$rightEyeFootXYPx
          
          # These are arrays of [x,y] coordinates for each measurement
          # Convert to data frame
          n_measurements <- length(left_foot) / 2  # Each measurement has x,y
          
          if (n_measurements > 0 && length(left_foot) == length(right_foot)) {
            # Reshape arrays - they might be flat [x1,y1,x2,y2,...] or nested [[x1,y1],[x2,y2],...]
            if (is.list(left_foot)) {
              # Nested format
              coords_list <- lapply(1:length(left_foot), function(idx) {
                list(
                  left_x = left_foot[[idx]][1],
                  left_y = left_foot[[idx]][2],
                  right_x = right_foot[[idx]][1],
                  right_y = right_foot[[idx]][2]
                )
              })
            } else {
              # Flat format [x1,y1,x2,y2,...]
              n_pts <- length(left_foot) / 2
              coords_list <- lapply(1:n_pts, function(idx) {
                base_idx <- (idx - 1) * 2 + 1
                list(
                  left_x = left_foot[base_idx],
                  left_y = left_foot[base_idx + 1],
                  right_x = right_foot[base_idx],
                  right_y = right_foot[base_idx + 1]
                )
              })
            }
            
            if (length(coords_list) > 0) {
              check_coords_df <- tibble(
                measurement_idx = 1:length(coords_list),
                left_x = sapply(coords_list, function(x) as.numeric(x$left_x)),
                left_y = sapply(coords_list, function(x) as.numeric(x$left_y)),
                right_x = sapply(coords_list, function(x) as.numeric(x$right_x)),
                right_y = sapply(coords_list, function(x) as.numeric(x$right_y))
              ) %>% filter(!is.na(left_x), !is.na(right_x))
              
              if (nrow(check_coords_df) > 0) {
                # Get pxPerCm from the data
                pxPerCm_val <- if (!is.null(distanceCheck$pxPerCm)) as.numeric(distanceCheck$pxPerCm[1]) else 37.8
                
                # Get screen dimensions from the data list
                screenWidthPx_val <- if ("screenWidthPx" %in% names(dl)) first(na.omit(as.numeric(dl$screenWidthPx))) else NA_real_
                screenHeightPx_val <- if ("screenHeightPx" %in% names(dl)) first(na.omit(as.numeric(dl$screenHeightPx))) else NA_real_
                
                # Extract measuredEyesToPointCm and requestedEyesToPointCm for correct distance ratio
                # Note: eyesToPointCm was deleted, use rulerBasedEyesToPointCm instead
                measured_eyes_vals <- as.numeric(distanceCheck$rulerBasedEyesToPointCm)
                requested_eyes_vals <- as.numeric(distanceCheck$requestedEyesToPointCm)
                
                # Compute distance ratio per measurement
                n_check_measurements <- nrow(check_coords_df)
                if (length(measured_eyes_vals) >= n_check_measurements && length(requested_eyes_vals) >= n_check_measurements) {
                  distance_ratio_vals <- measured_eyes_vals[1:n_check_measurements] / requested_eyes_vals[1:n_check_measurements]
                } else if (length(measured_eyes_vals) > 0 && length(requested_eyes_vals) > 0) {
                  # Use mean ratio if arrays don't match
                  distance_ratio_vals <- rep(mean(measured_eyes_vals, na.rm = TRUE) / mean(requested_eyes_vals, na.rm = TRUE), n_check_measurements)
                } else {
                  distance_ratio_vals <- rep(NA_real_, n_check_measurements)
                }
                
                t_check_feet <- check_coords_df %>%
                  mutate(
                    participant = participant_id_debug,
                    avg_eye_x_px = (left_x + right_x) / 2,
                    avg_eye_y_px = (left_y + right_y) / 2,
                    pxPerCm = pxPerCm_val,
                    avg_eye_x_cm = avg_eye_x_px / pxPerCm,
                    avg_eye_y_cm = avg_eye_y_px / pxPerCm,
                    distance_ratio = distance_ratio_vals,  # Use measuredEyesToPointCm / requestedEyesToPointCm
                    screenWidthPx = screenWidthPx_val,
                    screenHeightPx = screenHeightPx_val,
                    data_list_index = i,
                    measurement_index = measurement_idx
                  ) %>%
                  select(-measurement_idx)
                
                feet_check <- rbind(feet_check, t_check_feet)
              }
            }
          }
        }
        
      }, error = function(e) {
        # Silently skip if JSON parsing fails
      })
    #### TJSON data ####
      #distanceCalibrationTJSON has been renamed to distanceCalibrationJSON
        
        # Extract metadata for debugging/logging
        json_experiment  <- if (!is.null(t_tjson$experiment)) t_tjson$experiment else NA_character_
        json_participant <- if (!is.null(t_tjson$participant)) t_tjson$participant else NA_character_
        json_date        <- if (!is.null(t_tjson$date)) t_tjson$date else NA_character_
        json_type        <- if (!is.null(t_tjson$json)) t_tjson$json else NA_character_
        
        message(sprintf("[DEBUG TJSON] distanceCalibrationJSON metadata: experiment=%s, participant=%s, date=%s, json=%s",
                        json_experiment, json_participant, json_date, json_type))
        
        tmp <- tibble(
          participant      = first(na.omit(dl$participant)),
          # Metadata from JSON for debugging
          json_experiment  = json_experiment,
          json_participant = json_participant,
          json_date        = json_date,
          json_type        = json_type,
          # Renamed: rightEyeToFootCm -> objectBasedRightEyeToFootCm
          objectBasedRightEyeToFootCm = if (!is.null(t_tjson$objectBasedRightEyeToFootCm)) t_tjson$objectBasedRightEyeToFootCm else t_tjson$rightEyeToFootCm,
          # Renamed: leftEyeToFootCm -> objectBasedLeftEyeToFootCm
          objectBasedLeftEyeToFootCm  = if (!is.null(t_tjson$objectBasedLeftEyeToFootCm)) t_tjson$objectBasedLeftEyeToFootCm else t_tjson$leftEyeToFootCm,
          # Renamed: eyesToFootCm -> objectBasedEyesToFootCm (now provided directly in JSON)
          objectBasedEyesToFootCm     = if (!is.null(t_tjson$objectBasedEyesToFootCm)) t_tjson$objectBasedEyesToFootCm else t_tjson$eyesToFootCm,
          # New parameters: fOverWidth and ipdOverWidth (4 decimal digits)
          fOverWidth       = if (!is.null(t_tjson$fOverWidth)) as.numeric(t_tjson$fOverWidth) else NA_real_,
          ipdOverWidth     = if (!is.null(t_tjson$ipdOverWidth)) as.numeric(t_tjson$ipdOverWidth) else NA_real_,
          ipdCm            = if (!is.null(t_tjson$ipdCm)) as.numeric(t_tjson$ipdCm) else NA_real_
        ) %>%
          mutate(
            # Use objectBasedEyesToFootCm directly if available, otherwise compute from left/right
            eyeToFootCm = ifelse(!is.na(objectBasedEyesToFootCm), 
(as.numeric(objectBasedEyesToFootCm)),
((as.numeric(objectBasedLeftEyeToFootCm) + as.numeric(objectBasedRightEyeToFootCm)) / 2))
          )

        TJSON <- rbind(TJSON, tmp)

      
      #### checkJSON data ####
      
      # Parse distanceCheckJSON
        # Extract measuredEyesToPointCm and requestedEyesToPointCm for correct distance ratio
        measuredEyesToPointCm_vals <- as.numeric(distanceCheck$rulerBasedEyesToPointCm)
        requestedEyesToPointCm_vals <- as.numeric(distanceCheck$requestedEyesToPointCm)
        imageBasedEyesToPointCm_vals <- as.numeric(distanceCheck$imageBasedEyesToPointCm)
        rulerBasedEyesToPointCm_vals <- as.numeric(distanceCheck$rulerBasedEyesToPointCm)
        
        # New: extract ipdOverWidth (replaces ipdVpx) and calibrationFOverWidth (replaces calibrationFVpx)
        ipdOverWidth_vals <- as.numeric(distanceCheck$ipdOverWidth)
        calibrationFOverWidth_val <- if (!is.null(distanceCheck$calibrationFOverWidth)) {
(as.numeric(distanceCheck$calibrationFOverWidth))
        } else {
          NA_real_
        }
        
        message(sprintf("[DEBUG checkJSON] Extracted ipdOverWidth (n=%d): %s", 
                        length(ipdOverWidth_vals),
                        paste(head(ipdOverWidth_vals, 3), collapse=", ")))
        message(sprintf("[DEBUG checkJSON] Extracted calibrationFOverWidth: %s", calibrationFOverWidth_val))
        
        tmp <- tibble(
          participant   = first(na.omit(dl$participant)),
          measuredEyesToPointCm = measuredEyesToPointCm_vals,
          requestedEyesToPointCm = requestedEyesToPointCm_vals,
          eyesToPointCm = measuredEyesToPointCm_vals,  # Keep for backward compatibility
          imageBasedEyesToPointCm = imageBasedEyesToPointCm_vals,
          rulerBasedEyesToPointCm = rulerBasedEyesToPointCm_vals,
          footToPointCm = as.numeric(distanceCheck$footToPointCm),
          rulerBasedEyesToFootCm = as.numeric(distanceCheck$rulerBasedEyesToFootCm),
          # New parameters: ipdOverWidth replaces ipdVpx, calibrationFOverWidth replaces calibrationFVpx
          ipdOverWidth  = ipdOverWidth_vals,
          calibrationFOverWidth = calibrationFOverWidth_val,
          ipdCm         = as.numeric(first(na.omit(t_tjson$ipdCm)))
        ) %>%
          mutate(
            # Measured eyesToFootCm (derived from measured eyesToPointCm)
            eyeToFootCm = rulerBasedEyesToFootCm,
            # Requested eyesToFootCm (derived from requested eyesToPointCm)
            requestedEyesToFootCm = ifelse(is.finite(requestedEyesToPointCm) & is.finite(footToPointCm) & requestedEyesToPointCm >= footToPointCm,
                                           sqrt(requestedEyesToPointCm^2 - footToPointCm^2), NA_real_),
            # fOverWidth computed from ipdOverWidth (replaces fVpx computation)
            fOverWidth = ifelse(is.finite(ipdCm) & is.finite(ipdOverWidth) & is.finite(eyeToFootCm) & ipdCm > 0,
                                ipdOverWidth * eyeToFootCm / ipdCm, NA_real_),
            # Compute correct distance ratio: measured / requested eyesToPointCm
            distance_ratio = ifelse(is.finite(measuredEyesToPointCm) & is.finite(requestedEyesToPointCm) & requestedEyesToPointCm > 0,
                                    measuredEyesToPointCm / requestedEyesToPointCm, NA_real_)
          ) 

        checkJSON <- rbind(checkJSON, tmp)
    } 
  }
  
  
  sizeCheck <- sizeCheck %>% as_tibble()
  distance <- distance %>% as_tibble()
  eye_feet <- eye_feet %>% as_tibble()
  feet_calib <- feet_calib %>% as_tibble()
  feet_check <- feet_check %>% as_tibble()
  check_factor <- check_factor %>% distinct()
  blindspot <- blindspot %>% distinct()
  statement <- make_statement(sizeCheck)
  
  # =============================================================================
  # DEBUG: Invariant checks for distance measurement consistency
  # =============================================================================
  # These checks expose semantic inconsistencies between calibration and check
  # 
  # Invariant 1: fOverWidth from JSON vs computed from components should agree
  #   JSON provides: fOverWidth, ipdOverWidth
  #   Computed: ipdOverWidth * eyeToFootCm / ipdCm should equal fOverWidth
  #
  # Invariant 2: ipdOverWidth * Z should be constant for fixed f and ipdCm
  #   This catches Z definition errors before computing f
  # =============================================================================
  
  message("\n========== DISTANCE INVARIANT DEBUG LOG ==========\n")
  
  # --- CALIBRATION (TJSON) invariant checks ---
  if (nrow(TJSON) > 0 && nrow(camera) > 0 && "PavloviaParticipantID" %in% names(camera)) {
    message("--- CALIBRATION (TJSON) DATA ---")
    
    calib_debug <- TJSON %>%
      left_join(camera %>% select(PavloviaParticipantID, widthVpx), 
                by = c("participant" = "PavloviaParticipantID")) %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth), 
             !is.na(widthVpx), widthVpx > 0,
             !is.na(ipdOverWidth), is.finite(ipdOverWidth),
             !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0,
             !is.na(eyeToFootCm), is.finite(eyeToFootCm), eyeToFootCm > 0) %>%
      mutate(
        # fOverWidth from JSON (directly provided)
        fOverWidth_from_json = fOverWidth,
        
        # fOverWidth computed from components (should match JSON)
        fOverWidth_computed = ipdOverWidth * eyeToFootCm / ipdCm,
        
        # Invariant 1: Check agreement between JSON and computed
        fOverWidth_ratio = fOverWidth_from_json / fOverWidth_computed,
        fOverWidth_pct_diff = abs(fOverWidth_from_json - fOverWidth_computed) / fOverWidth_from_json * 100,
        
        # Invariant 2: ipdOverWidth * Z (should be constant for fixed f, ipdCm)
        # This is proportional to focal length: ipdOverWidth * Z / ipdCm = fOverWidth
        ipdOverWidth_times_Z = ipdOverWidth * eyeToFootCm,
        
        # For reference: what Z value was used
        Z_used = eyeToFootCm
      )
    
    if (nrow(calib_debug) > 0) {
      message(sprintf("  Participants: %s", paste(unique(calib_debug$participant), collapse = ", ")))
      message(sprintf("  N observations: %d", nrow(calib_debug)))
      
      # Invariant 1 summary
      message("\n  INVARIANT 1: fOverWidth JSON vs computed (calibration)")
      message(sprintf("    fOverWidth from JSON:     mean=%.6f, sd=%.6f", 
                      mean(calib_debug$fOverWidth_from_json, na.rm = TRUE),
                      sd(calib_debug$fOverWidth_from_json, na.rm = TRUE)))
      message(sprintf("    fOverWidth computed (ipdOverWidth*Z/ipdCm): mean=%.6f, sd=%.6f",
                      mean(calib_debug$fOverWidth_computed, na.rm = TRUE),
                      sd(calib_debug$fOverWidth_computed, na.rm = TRUE)))
      message(sprintf("    Ratio (JSON/computed): mean=%.6f, sd=%.6f, range=[%.6f, %.6f]",
                      mean(calib_debug$fOverWidth_ratio, na.rm = TRUE),
                      sd(calib_debug$fOverWidth_ratio, na.rm = TRUE),
                      min(calib_debug$fOverWidth_ratio, na.rm = TRUE),
                      max(calib_debug$fOverWidth_ratio, na.rm = TRUE)))
      message(sprintf("    Percent difference: mean=%.4f%%, max=%.4f%%",
                      mean(calib_debug$fOverWidth_pct_diff, na.rm = TRUE),
                      max(calib_debug$fOverWidth_pct_diff, na.rm = TRUE)))
      
      # Invariant 2 summary
      message("\n  INVARIANT 2: ipdOverWidth * Z consistency (calibration)")
      message(sprintf("    ipdOverWidth * Z: mean=%.4f, sd=%.4f, CV=%.4f%%",
                      mean(calib_debug$ipdOverWidth_times_Z, na.rm = TRUE),
                      sd(calib_debug$ipdOverWidth_times_Z, na.rm = TRUE),
                      sd(calib_debug$ipdOverWidth_times_Z, na.rm = TRUE) / 
                        mean(calib_debug$ipdOverWidth_times_Z, na.rm = TRUE) * 100))
      
      # Per-participant breakdown
      message("\n  Per-participant details (calibration):")
      calib_by_participant <- calib_debug %>%
        group_by(participant) %>%
        summarize(
          n = n(),
          Z_mean = mean(Z_used, na.rm = TRUE),
          Z_range = paste0("[", round(min(Z_used, na.rm = TRUE), 1), ", ", round(max(Z_used, na.rm = TRUE), 1), "]"),
          ipdCm = first(ipdCm),
          ipdOverWidth_mean = mean(ipdOverWidth, na.rm = TRUE),
          fOverWidth_json_mean = mean(fOverWidth_from_json, na.rm = TRUE),
          fOverWidth_comp_mean = mean(fOverWidth_computed, na.rm = TRUE),
          ratio_mean = mean(fOverWidth_ratio, na.rm = TRUE),
          ipdOverWidth_x_Z_mean = mean(ipdOverWidth_times_Z, na.rm = TRUE),
          ipdOverWidth_x_Z_sd = sd(ipdOverWidth_times_Z, na.rm = TRUE),
          .groups = "drop"
        )
      
      for (i in 1:nrow(calib_by_participant)) {
        row <- calib_by_participant[i, ]
        message(sprintf("    %s: n=%d, Z=%s cm, ipdCm=%.2f, ipdOverWidth=%.5f",
                        row$participant, row$n, row$Z_range, row$ipdCm, row$ipdOverWidth_mean))
        message(sprintf("      fOverWidth: JSON=%.5f, computed=%.5f, ratio=%.6f",
                        row$fOverWidth_json_mean, row$fOverWidth_comp_mean, row$ratio_mean))
        message(sprintf("      ipdOverWidth*Z: mean=%.4f, sd=%.4f",
                        row$ipdOverWidth_x_Z_mean, 
                        ifelse(is.na(row$ipdOverWidth_x_Z_sd), 0, row$ipdOverWidth_x_Z_sd)))
      }
    }
  }
  
  # --- CHECK (checkJSON) invariant checks ---
  # checkJSON now has fOverWidth and ipdOverWidth directly (distanceCheckJSON updated)
  if (nrow(checkJSON) > 0 && nrow(camera) > 0 && "PavloviaParticipantID" %in% names(camera)) {
    message("\n--- CHECK (checkJSON) DATA ---")
    
    check_debug <- checkJSON %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth), 
             !is.na(ipdOverWidth), is.finite(ipdOverWidth),
             !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0,
             !is.na(eyeToFootCm), is.finite(eyeToFootCm), eyeToFootCm > 0) %>%
      mutate(
        # fOverWidth from JSON (directly provided, computed from ipdOverWidth * eyeToFootCm / ipdCm)
        fOverWidth_from_json = fOverWidth,
        
        # fOverWidth computed from components (should match JSON)
        fOverWidth_computed = ipdOverWidth * eyeToFootCm / ipdCm,
        
        # Also compute using requestedEyesToFootCm (the "other" Z)
        fOverWidth_method3 = ifelse(!is.na(requestedEyesToFootCm) & is.finite(requestedEyesToFootCm),
                                    ipdOverWidth * requestedEyesToFootCm / ipdCm, NA_real_),
        
        # Invariant 1: Check agreement between JSON and computed
        fOverWidth_ratio_json_comp = fOverWidth_from_json / fOverWidth_computed,
        fOverWidth_ratio_json_m3 = ifelse(!is.na(fOverWidth_method3), 
                                        fOverWidth_from_json / fOverWidth_method3, NA_real_),
        fOverWidth_pct_diff_json_comp = abs(fOverWidth_from_json - fOverWidth_computed) / fOverWidth_from_json * 100,
        fOverWidth_pct_diff_json_m3 = ifelse(!is.na(fOverWidth_method3),
                                           abs(fOverWidth_from_json - fOverWidth_method3) / fOverWidth_from_json * 100, NA_real_),
        
        # Invariant 2: ipdOverWidth * Z products
        ipdOverWidth_times_measuredZ = ipdOverWidth * eyeToFootCm,
        ipdOverWidth_times_requestedZ = ifelse(!is.na(requestedEyesToFootCm),
                                               ipdOverWidth * requestedEyesToFootCm, NA_real_),
        
        # For reference
        Z_measured = eyeToFootCm,
        Z_requested = requestedEyesToFootCm,
        Z_ratio = ifelse(!is.na(requestedEyesToFootCm) & requestedEyesToFootCm > 0,
                         eyeToFootCm / requestedEyesToFootCm, NA_real_)
      )
    
    if (nrow(check_debug) > 0) {
      message(sprintf("  Participants: %s", paste(unique(check_debug$participant), collapse = ", ")))
      message(sprintf("  N observations: %d", nrow(check_debug)))
      
      # Invariant 1 summary
      message("\n  INVARIANT 1: fOverWidth JSON vs computed (check)")
      message("  JSON: fOverWidth (computed from ipdOverWidth * eyeToFootCm / ipdCm in JSON)")
      message("  Computed: ipdOverWidth * measured_eyeToFootCm / ipdCm")
      message("  Method 3: ipdOverWidth * requested_eyeToFootCm / ipdCm")
      
      message(sprintf("\n    fOverWidth from JSON:   mean=%.6f, sd=%.6f", 
                      mean(check_debug$fOverWidth_from_json, na.rm = TRUE),
                      sd(check_debug$fOverWidth_from_json, na.rm = TRUE)))
      message(sprintf("    fOverWidth computed:    mean=%.6f, sd=%.6f",
                      mean(check_debug$fOverWidth_computed, na.rm = TRUE),
                      sd(check_debug$fOverWidth_computed, na.rm = TRUE)))
      message(sprintf("    fOverWidth method3:     mean=%.6f, sd=%.6f",
                      mean(check_debug$fOverWidth_method3, na.rm = TRUE),
                      sd(check_debug$fOverWidth_method3, na.rm = TRUE)))
      
      message(sprintf("\n    Ratio JSON/computed (should be ~1.0): mean=%.6f, sd=%.6f, range=[%.6f, %.6f]",
                      mean(check_debug$fOverWidth_ratio_json_comp, na.rm = TRUE),
                      sd(check_debug$fOverWidth_ratio_json_comp, na.rm = TRUE),
                      min(check_debug$fOverWidth_ratio_json_comp, na.rm = TRUE),
                      max(check_debug$fOverWidth_ratio_json_comp, na.rm = TRUE)))
      message(sprintf("    Ratio JSON/m3 (reveals Z definition issue): mean=%.6f, sd=%.6f, range=[%.6f, %.6f]",
                      mean(check_debug$fOverWidth_ratio_json_m3, na.rm = TRUE),
                      sd(check_debug$fOverWidth_ratio_json_m3, na.rm = TRUE),
                      min(check_debug$fOverWidth_ratio_json_m3, na.rm = TRUE),
                      max(check_debug$fOverWidth_ratio_json_m3, na.rm = TRUE)))
      
      # Invariant 2 summary
      message("\n  INVARIANT 2: ipdOverWidth * Z consistency (check)")
      message(sprintf("    ipdOverWidth * Z_measured:  mean=%.4f, sd=%.4f, CV=%.4f%%",
                      mean(check_debug$ipdOverWidth_times_measuredZ, na.rm = TRUE),
                      sd(check_debug$ipdOverWidth_times_measuredZ, na.rm = TRUE),
                      sd(check_debug$ipdOverWidth_times_measuredZ, na.rm = TRUE) / 
                        mean(check_debug$ipdOverWidth_times_measuredZ, na.rm = TRUE) * 100))
      message(sprintf("    ipdOverWidth * Z_requested: mean=%.4f, sd=%.4f, CV=%.4f%%",
                      mean(check_debug$ipdOverWidth_times_requestedZ, na.rm = TRUE),
                      sd(check_debug$ipdOverWidth_times_requestedZ, na.rm = TRUE),
                      sd(check_debug$ipdOverWidth_times_requestedZ, na.rm = TRUE) / 
                        mean(check_debug$ipdOverWidth_times_requestedZ, na.rm = TRUE) * 100))
      
      # Z comparison
      message("\n  Z DEFINITION COMPARISON:")
      message(sprintf("    Z_measured (eyeToFootCm): mean=%.2f, sd=%.2f, range=[%.2f, %.2f]",
                      mean(check_debug$Z_measured, na.rm = TRUE),
                      sd(check_debug$Z_measured, na.rm = TRUE),
                      min(check_debug$Z_measured, na.rm = TRUE),
                      max(check_debug$Z_measured, na.rm = TRUE)))
      message(sprintf("    Z_requested (requestedEyesToFootCm): mean=%.2f, sd=%.2f, range=[%.2f, %.2f]",
                      mean(check_debug$Z_requested, na.rm = TRUE),
                      sd(check_debug$Z_requested, na.rm = TRUE),
                      min(check_debug$Z_requested, na.rm = TRUE),
                      max(check_debug$Z_requested, na.rm = TRUE)))
      message(sprintf("    Z_measured / Z_requested: mean=%.4f, sd=%.4f, range=[%.4f, %.4f]",
                      mean(check_debug$Z_ratio, na.rm = TRUE),
                      sd(check_debug$Z_ratio, na.rm = TRUE),
                      min(check_debug$Z_ratio, na.rm = TRUE),
                      max(check_debug$Z_ratio, na.rm = TRUE)))
      
      # Per-participant breakdown
      message("\n  Per-participant details (check):")
      check_by_participant <- check_debug %>%
        group_by(participant) %>%
        summarize(
          n = n(),
          ipdCm = first(ipdCm),
          ipdOverWidth_mean = mean(ipdOverWidth, na.rm = TRUE),
          Z_measured_range = paste0("[", round(min(Z_measured, na.rm = TRUE), 1), ", ", 
                                    round(max(Z_measured, na.rm = TRUE), 1), "]"),
          Z_requested_range = paste0("[", round(min(Z_requested, na.rm = TRUE), 1), ", ", 
                                     round(max(Z_requested, na.rm = TRUE), 1), "]"),
          Z_ratio_mean = mean(Z_ratio, na.rm = TRUE),
          fOverWidth_m1 = mean(fOverWidth_method1, na.rm = TRUE),
          fOverWidth_m2 = mean(fOverWidth_method2, na.rm = TRUE),
          fOverWidth_m3 = mean(fOverWidth_method3, na.rm = TRUE),
          ratio_m1_m2 = mean(fOverWidth_ratio_m1_m2, na.rm = TRUE),
          ratio_m1_m3 = mean(fOverWidth_ratio_m1_m3, na.rm = TRUE),
          ipdOverWidth_x_Z_meas = mean(ipdOverWidth_times_measuredZ, na.rm = TRUE),
          ipdOverWidth_x_Z_req = mean(ipdOverWidth_times_requestedZ, na.rm = TRUE),
          .groups = "drop"
        )
      
      for (i in 1:nrow(check_by_participant)) {
        row <- check_by_participant[i, ]
        message(sprintf("    %s: n=%d, ipdCm=%.2f, ipdOverWidth=%.5f",
                        row$participant, row$n, row$ipdCm, row$ipdOverWidth_mean))
        message(sprintf("      Z_measured=%s, Z_requested=%s, Z_ratio=%.4f",
                        row$Z_measured_range, row$Z_requested_range, row$Z_ratio_mean))
        message(sprintf("      fOverWidth: m1=%.5f, m2=%.5f, m3=%.5f",
                        row$fOverWidth_m1, row$fOverWidth_m2, row$fOverWidth_m3))
        message(sprintf("      ratios: m1/m2=%.4f (should be 1.0), m1/m3=%.4f (reveals Z issue)",
                        row$ratio_m1_m2, row$ratio_m1_m3))
        message(sprintf("      ipdOverWidth*Z: measured=%.4f, requested=%.4f",
                        row$ipdOverWidth_x_Z_meas, row$ipdOverWidth_x_Z_req))
      }
    }
  }
  
  # --- CROSS-CHECK: Compare calibration vs check ---
  if (nrow(TJSON) > 0 && nrow(checkJSON) > 0 && nrow(camera) > 0 && "PavloviaParticipantID" %in% names(camera)) {
    message("\n--- CROSS-CHECK: CALIBRATION vs CHECK ---")
    
    # Get median ipdOverWidth*Z from calibration per participant
    # TJSON now has ipdOverWidth directly (no longer computed from ipdVpx)
    calib_product <- TJSON %>%
      filter(!is.na(ipdOverWidth), !is.na(eyeToFootCm), !is.na(fOverWidth)) %>%
      mutate(ipdOverWidth_x_Z = ipdOverWidth * eyeToFootCm) %>%
      group_by(participant) %>%
      summarize(calib_ipdOverWidth_x_Z = median(ipdOverWidth_x_Z, na.rm = TRUE),
                calib_fOverWidth = median(fOverWidth, na.rm = TRUE),
                .groups = "drop")
    
    # Get median from check per participant (checkJSON now has ipdOverWidth directly)
    check_product <- checkJSON %>%
      filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), !is.na(eyeToFootCm)) %>%
      mutate(ipdOverWidth_x_Z_meas = ipdOverWidth * eyeToFootCm,
             ipdOverWidth_x_Z_req = ipdOverWidth * requestedEyesToFootCm,
             fOverWidth_meas = ipdOverWidth * eyeToFootCm / ipdCm,
             fOverWidth_req = ipdOverWidth * requestedEyesToFootCm / ipdCm) %>%
      group_by(participant) %>%
      summarize(check_ipdOverWidth_x_Z_meas = median(ipdOverWidth_x_Z_meas, na.rm = TRUE),
                check_ipdOverWidth_x_Z_req = median(ipdOverWidth_x_Z_req, na.rm = TRUE),
                check_fOverWidth_meas = median(fOverWidth_meas, na.rm = TRUE),
                check_fOverWidth_req = median(fOverWidth_req, na.rm = TRUE),
                .groups = "drop")
    
    cross_check <- calib_product %>%
      inner_join(check_product, by = "participant") %>%
      mutate(
        # If camera intrinsics are stable, ipdOverWidth*Z should be same for same ipdCm
        ratio_calib_vs_check_meas = calib_ipdOverWidth_x_Z / check_ipdOverWidth_x_Z_meas,
        ratio_calib_vs_check_req = calib_ipdOverWidth_x_Z / check_ipdOverWidth_x_Z_req,
        # fOverWidth should be same between calibration and check (camera intrinsic)
        fOverWidth_ratio_calib_check_meas = calib_fOverWidth / check_fOverWidth_meas,
        fOverWidth_ratio_calib_check_req = calib_fOverWidth / check_fOverWidth_req
      )
    
    if (nrow(cross_check) > 0) {
      message("  Comparing ipdOverWidth*Z between calibration and check:")
      message(sprintf("    calib vs check(measured Z): mean ratio=%.4f, should be ~1.0 if Z definitions match",
                      mean(cross_check$ratio_calib_vs_check_meas, na.rm = TRUE)))
      message(sprintf("    calib vs check(requested Z): mean ratio=%.4f, deviation from 1.0 shows Z definition error",
                      mean(cross_check$ratio_calib_vs_check_req, na.rm = TRUE)))
      
      message("\n  Comparing fOverWidth between calibration and check:")
      message(sprintf("    calib vs check(measured Z): mean ratio=%.4f, should be ~1.0 (same camera)",
                      mean(cross_check$fOverWidth_ratio_calib_check_meas, na.rm = TRUE)))
      message(sprintf("    calib vs check(requested Z): mean ratio=%.4f, deviation reveals absorbed error",
                      mean(cross_check$fOverWidth_ratio_calib_check_req, na.rm = TRUE)))
      
      message("\n  Per-participant cross-check:")
      for (i in 1:nrow(cross_check)) {
        row <- cross_check[i, ]
        message(sprintf("    %s:", row$participant))
        message(sprintf("      ipdOverWidth*Z: calib=%.4f, check_meas=%.4f, check_req=%.4f",
                        row$calib_ipdOverWidth_x_Z, row$check_ipdOverWidth_x_Z_meas, row$check_ipdOverWidth_x_Z_req))
        message(sprintf("      fOverWidth: calib=%.5f, check_meas=%.5f, check_req=%.5f",
                        row$calib_fOverWidth, row$check_fOverWidth_meas, row$check_fOverWidth_req))
        message(sprintf("      Ratios: ipdOverWidth*Z calib/check_meas=%.4f, calib/check_req=%.4f",
                        row$ratio_calib_vs_check_meas, row$ratio_calib_vs_check_req))
      }
    }
  }
  
  message("\n========== END DISTANCE INVARIANT DEBUG LOG ==========\n")
  
  # Extract raw array data for new histograms
  raw_pxPerCm <- get_raw_pxPerCm_data(filtered_data_list, sizeCheck)
  raw_objectMeasuredCm <- get_raw_objectMeasuredCm_data(filtered_data_list)
  
  # Compute median fOverWidth from checkJSON (which now has fOverWidth directly)
  median_fOverWidth_check <- if (nrow(checkJSON) > 0) {
    checkJSON %>% 
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>% 
      summarize(medianfOverWidth = median(fOverWidth, na.rm = TRUE), .groups = "drop")
  } else {
    tibble(participant = character(), medianfOverWidth = numeric())
  }
  
  # TJSON now has fOverWidth directly (no longer computed from fVpx)
  raw_fVpx <- TJSON %>%
    select(participant, fOverWidth) %>%
    filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
    left_join(median_fOverWidth_check, by = "participant") %>%
    mutate(relative = fOverWidth / medianfOverWidth) %>%
    select(participant, fOverWidth, medianfOverWidth, relative) %>%
    filter(is.finite(relative), relative > 0)
  # Compute camera resolution stats (SD and count of width values)
  camera_res_stats <- get_camera_resolution_stats(filtered_data_list)

  print("TJSON")
  print(TJSON, n = Inf, width = Inf)
  print("checkJSON")
  print(checkJSON, n = Inf, width = Inf)
  print("raw_fVpx")
  print(raw_fVpx, n = Inf, width = Inf)
  
  return(list(
    filtered = filtered_data_list,
    sizeCheck = sizeCheck,
    distance = distance,
    eye_feet = eye_feet,
    feet_calib = feet_calib,
    feet_check = feet_check,
    check_factor = check_factor,
    camera = camera,
    camera_res_stats = camera_res_stats,
    blindspot = blindspot,
    raw_pxPerCm = raw_pxPerCm,
    raw_objectMeasuredCm = raw_objectMeasuredCm,
    raw_fVpx = raw_fVpx,
    statement = statement,
    TJSON = TJSON,
    checkJSON = checkJSON
  ))
}

get_sizeCheck_data <- function(data_list) {
  df <- tibble()
  if(length(data_list) == 0) {
    return(df)
  }
  # pxPerCm: pixel density measured with a credit card
  # SizeCheckEstimatedPxPerCm: pixel density measured from length production
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(`_calibrateTrackDistance`,
             `_calibrateTrackDistancePupil`,
             `_calibrateTrackDistanceAllowedRatio`,
             `_calibrateTrackDistanceShowLengthBool`,
             `_calibrateTrackDistanceTimes`,
             calibrateScreenSizeAllowedRatio,
             calibrateScreenSizeTimes,
             viewingDistanceWhichEye,
             viewingDistanceWhichPoint,
             participant,
             SizeCheckEstimatedPxPerCm,
             SizeCheckRequestedCm,
             rulerLength,
             rulerUnit,
             pxPerCm) %>%
      mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
             `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
             `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
             calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
             calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
             `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
             `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
             `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
             `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)
      ) %>%
      distinct() %>%
      filter(!is.na(SizeCheckEstimatedPxPerCm),
             !is.na(SizeCheckRequestedCm),
             SizeCheckEstimatedPxPerCm != '',
             SizeCheckRequestedCm != '')
    if (nrow(t) > 0) {
      # Convert JSON-like lists into strings and remove extra characters
      t <- t %>%
        mutate(
          SizeCheckEstimatedPxPerCm = gsub("\\[|\\]|\"", "", SizeCheckEstimatedPxPerCm),
          SizeCheckRequestedCm = gsub("\\[|\\]|\"", "", SizeCheckRequestedCm)
        ) %>%
        # Separate both columns together while keeping row-wise structure
        mutate(measured_list = strsplit(SizeCheckEstimatedPxPerCm, ","), 
               requested_list = strsplit(SizeCheckRequestedCm, ",")) %>%
        unnest(c(measured_list, requested_list)) %>%  # Expands both columns together
        mutate(
          SizeCheckEstimatedPxPerCm = as.numeric(trimws(measured_list)),
          SizeCheckRequestedCm = as.numeric(trimws(requested_list))
        ) %>%
        select(-measured_list, -requested_list)  # Remove temp lists
      
      df <- rbind(t, df)
    }
  }

  if (nrow(df) > 0) {
    df <- df %>% mutate(
      SizeCheckEstimatedPxPerCm = as.numeric(SizeCheckEstimatedPxPerCm),
      SizeCheckRequestedCm = as.numeric(SizeCheckRequestedCm)
    )
  }
  
  return(df)
}

get_measured_distance_data <- function(data_list) {
  df <- tibble()
  if(length(data_list) == 0) {
    return(df)
  }
  for (i in 1:length(data_list)) {
    
    if ('calibrateTrackDistanceIpdCameraPx' %in% names(data_list[[i]])) {
      data_list[[i]]$calibrateTrackDistanceIpdVpx = data_list[[i]]$calibrateTrackDistanceIpdCameraPx
    }
    
    t <- data_list[[i]] %>%
      select(  `_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               `_calibrateTrackDistance`,
             `_calibrateTrackDistancePupil`,
             viewingDistanceWhichEye,
             viewingDistanceWhichPoint,
             participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm,
             calibrateTrackDistanceIpdVpx) %>%
      mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`),
             `_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`),
             `_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`),
             calibrateScreenSizeAllowedRatio = get_first_non_na(`calibrateScreenSizeAllowedRatio`),
             calibrateScreenSizeTimes= get_first_non_na(calibrateScreenSizeTimes),
             `_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`),
             `_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`),
             `viewingDistanceWhichEye` = get_first_non_na(`viewingDistanceWhichEye`),
             `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)
      ) %>%
      filter(!is.na(calibrateTrackDistanceMeasuredCm),
             !is.na(calibrateTrackDistanceRequestedCm),
             calibrateTrackDistanceMeasuredCm != '',
             calibrateTrackDistanceRequestedCm != '') %>% 
        distinct()

    if (nrow(t) > 0) {
      # Convert JSON-like lists into strings and remove extra characters
      t <- t %>%
        mutate(
          calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
          calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm),
          calibrateTrackDistanceIpdVpx = gsub("\\[|\\]|\"", "", calibrateTrackDistanceIpdVpx)
        ) %>%
        # Separate all columns together while keeping row-wise structure
        mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","),
               requested_list = strsplit(calibrateTrackDistanceRequestedCm, ","),
               ipd_list = strsplit(calibrateTrackDistanceIpdVpx, ",")) %>%
        unnest(c(measured_list, requested_list, ipd_list)) %>%  # Expands all columns together
        mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
          calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
          calibrateTrackDistanceIpdVpx = as.numeric(trimws(ipd_list))
        ) %>%
        select(-measured_list, -requested_list, -ipd_list)  # Remove temp lists
      
      if (nrow(t) > 0) {
        t <- t %>% mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
          calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm)
        ) %>%
          # Ensure IPD column exists even if it wasn't processed
          mutate(calibrateTrackDistanceIpdVpx = if("calibrateTrackDistanceIpdVpx" %in% names(.)) calibrateTrackDistanceIpdVpx else NA) %>%
          # Add row number first to preserve original order
          mutate(order = row_number())
      }
      df <- rbind(t, df)
    }
  }
  if (nrow(t) > 0) {
    if(all(is.na(df$calibrateTrackDistanceIpdVpx))) {
      df <- df %>% select(-calibrateTrackDistanceIpdVpx)
    } else {
      df <- df %>% filter(!is.na(calibrateTrackDistanceIpdVpx))
    }
  }
  
  return(df)
}

plot_eye_feet_position <- function(distanceCalibrationResults) {
  
  # Use eye_feet (calibration data) for "during calibration" plot
  eye_feet_data <- distanceCalibrationResults$eye_feet
  
  # Fallback to feet_calib if eye_feet is empty
  if (nrow(eye_feet_data) == 0) {
    eye_feet_data <- distanceCalibrationResults$feet_calib
  }

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }

  # Filter out rows with invalid coordinates and distance_ratio
  eye_feet_data <- eye_feet_data %>%
    filter(!is.na(avg_eye_x_px), !is.na(avg_eye_y_px),
           !is.na(avg_eye_x_cm), !is.na(avg_eye_y_cm), 
           !is.na(distance_ratio), is.finite(distance_ratio))

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }
  
  # Estimate screen dimensions in cm from the data
  # Origin is at top-left corner (0,0) as specified
  
  # Get actual screen dimensions from the data if available
  typical_pxPerCm <- median(eye_feet_data$pxPerCm, na.rm = TRUE)
  
  # Try to get screen dimensions from screenWidthPx/screenHeightPx columns if available
  if ("screenWidthPx" %in% names(eye_feet_data) && "screenHeightPx" %in% names(eye_feet_data)) {
    screen_width_px <- median(as.numeric(eye_feet_data$screenWidthPx), na.rm = TRUE)
    screen_height_px <- median(as.numeric(eye_feet_data$screenHeightPx), na.rm = TRUE)
    screen_width_cm <- screen_width_px / typical_pxPerCm
    screen_height_cm <- screen_height_px / typical_pxPerCm
    
  } else {
    # Fallback: estimate from eye position data (may underestimate actual screen size)
    max_x_px <- max(eye_feet_data$avg_eye_x_px, na.rm = TRUE)
    max_y_px <- max(eye_feet_data$avg_eye_y_px, na.rm = TRUE)
    screen_width_cm <- max_x_px / typical_pxPerCm
    screen_height_cm <- max_y_px / typical_pxPerCm
    
  }

  # Add 50% margin around screen as specified in Trello
  # For W=30.20, H=19.61, limits should be x:[-15.10, 45.30], y:[-9.81, 29.42]
  x_margin_cm <- 0.5 * screen_width_cm
  y_margin_cm <- 0.5 * screen_height_cm
  x_min <- -x_margin_cm
  x_max <- screen_width_cm + x_margin_cm
  y_min <- -y_margin_cm
  y_max <- screen_height_cm + y_margin_cm
  
  # Add extra padding to Y-axis to prevent point cutoff
  y_padding <- 0.1 * (y_max - y_min)  # 10% extra padding
  y_min_padded <- y_min - y_padding
  y_max_padded <- y_max + y_padding
  
  
  
  # Prepare data for plotting
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Clip points that fall beyond the margin (as requested)
      foot_x_cm_clipped = pmax(x_min, pmin(x_max, avg_eye_x_cm)),
      foot_y_cm_clipped = pmax(y_min, pmin(y_max, avg_eye_y_cm)),
      # Use continuous ratio for color mapping as specified (limited to 0.7-1.4 range)
      ratio_continuous = pmax(0.7, pmin(1.4, distance_ratio)),
      # Create session identifier for shapes - use actual participant names like other plots
      session_id = as.character(participant)
    )
  
  sample_plot_data <- eye_feet_data %>%
    select(participant, foot_x_cm_clipped, foot_y_cm_clipped, ratio_continuous, distance_ratio) %>%
    head(3)
  
  # Handle unlimited number of participants with cycling shapes
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Always use actual participant names - no artificial limits
      session_limited = factor(session_id)
    )

  n_sessions <- n_distinct(eye_feet_data$session_id)

  # Create statement for calibration parameters (safe, short)
  statement <- make_statement(eye_feet_data)
  param_tbl <- build_param_table(eye_feet_data)

  p <- NULL
  # Create the plot directly without test plots
  tryCatch({
    p <- ggplot(eye_feet_data, aes(x = foot_x_cm_clipped, y = foot_y_cm_clipped)) +
      # Add screen boundary rectangle (quarter thickness)
      geom_rect(aes(xmin = 0, xmax = screen_width_cm,
                    ymin = 0, ymax = screen_height_cm),
                fill = NA, color = "black", linewidth = 0.3, alpha = 0.7) +
      # Add points with fill aesthetic to avoid server color conflicts
      geom_point(aes(fill = ratio_continuous, shape = session_limited), 
                 size = 4, alpha = 0.9, color = "black", stroke = 0.2) +
      # Log-scaled continuous fill scale with 0.1 interval breaks (limited to 0.7-1.4 range)
      scale_fill_gradientn(
        colors = c("#3B4CC0", "#89A1F0", "#FDE725", "#F07E26", "#B2182B"),
        values = scales::rescale(log10(c(0.7, 0.85, 1.0, 1.2, 1.4))),
        limits = c(0.7, 1.4),
        trans = "log10",
        oob = scales::squish,
        name = "Measured over requested",
        breaks = seq(0.7, 1.4, by = 0.1),
        labels = c( "0.7", "", "", "1.0", "", "", "", "1.4"),
        guide = guide_colorbar(
          barheight = unit(0.5, "cm"),
          barwidth = unit(5, "cm"), # Increased colorbar width
          title.position = "top",
          ticks.colour = "black",
          ticks.linewidth = 0.75,
          ticks.position = "outward"
        )
      ) +
      # Shape scale for participants (unlimited participants with cycling shapes)
      scale_shape_manual(
        values = {
          # Use only fillable shapes so fill color encodes error consistently
          all_shapes <- c(21, 22, 23, 24, 25)
          n_participants <- nlevels(eye_feet_data$session_limited)
          # Cycle through the 5 fillable shapes across all participants
          rep(all_shapes, length.out = n_participants)
        },
        name = "",
        guide = guide_legend(
          ncol = 4,  # Keep 3 columns to stay within plot width
          byrow = TRUE,
          key.spacing.y = unit(0.07, "cm"),
          key.spacing.x = unit(0.07, "cm")
        )
      ) +
      # Flip Y-axis to make origin truly at top-left (Y increases downward)
      scale_y_reverse(limits = c(y_max_padded, y_min_padded), expand = c(0,0)) +
      # Lock aspect ratio with exact 50% margins
      coord_fixed(xlim = c(x_min, x_max), expand = FALSE) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
        legend.key.height = unit(0.4, "cm"),  # Shrunk for compact rows
        legend.key.width = unit(0.4, "cm"),
        legend.margin = margin(l = 2, r = 2, t = 5, b = 5),
        legend.background = element_rect(fill = NA, colour = NA),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20, "pt"),  # More bottom margin for legend
        # Open plot style - only bottom and left axis lines
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank()
      ) +
      # # Labels - set these LAST to ensure they stick
      labs(
        subtitle = "Measured over requested distance vs.\nfoot position during calibration",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)",
        caption = "Rectangle shows screen boundaries. Points beyond 50% margin are clipped to plot area.\nOrigin (0,0) is at top-left corner of screen."
      ) +
      # # Screen annotation (moved close to screen outline, same size as Y label, not bold)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
      # Calibration parameters (bottom right corner)
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      # Summary statistics (pushed further down to avoid clipping)
      annotate("text", x = x_min+1, y = y_min + 1,
               label = paste0("N = ", nrow(eye_feet_data), "\n",
                             "Sessions= ", n_sessions, "\n",
                             "Mean = ", round(mean(eye_feet_data$distance_ratio, na.rm = TRUE), 3), "\n",
                             "SD = ", round(sd(eye_feet_data$distance_ratio, na.rm = TRUE), 3)),
               hjust = 0, vjust = 1, size = 3, color = "black")
  }, error = function(e) {
    p <<- NULL
  })

  # Calculate height based on legend complexity (in inches)
  n_sessions <- n_distinct(eye_feet_data$session_id)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_sessions, per_row = 3, row_increase = 0.08)

  # Return the plot or NULL if creation failed
  if(!is.null(p)) {
    return(list(plot = p, height = plot_height))
  } else {
    return(NULL)
  }
}

# Plot: Measured over requested distance vs. foot position during check
# Uses check data (feet_check) - separate from calibration data (feet_calib)
plot_eye_feet_position_during_check <- function(distanceCalibrationResults) {
  
  # Use feet_check (check data) for "during check" plot
  eye_feet_data <- distanceCalibrationResults$feet_check
  
  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }

  # Filter out rows with invalid coordinates and distance_ratio
  eye_feet_data <- eye_feet_data %>%
    filter(!is.na(avg_eye_x_px), !is.na(avg_eye_y_px),
           !is.na(avg_eye_x_cm), !is.na(avg_eye_y_cm), 
           !is.na(distance_ratio), is.finite(distance_ratio))

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }
  
  # Get actual screen dimensions from the data if available
  typical_pxPerCm <- median(eye_feet_data$pxPerCm, na.rm = TRUE)
  
  # Try to get screen dimensions from screenWidthPx/screenHeightPx columns if available
  if ("screenWidthPx" %in% names(eye_feet_data) && "screenHeightPx" %in% names(eye_feet_data)) {
    screen_width_px <- median(as.numeric(eye_feet_data$screenWidthPx), na.rm = TRUE)
    screen_height_px <- median(as.numeric(eye_feet_data$screenHeightPx), na.rm = TRUE)
    screen_width_cm <- screen_width_px / typical_pxPerCm
    screen_height_cm <- screen_height_px / typical_pxPerCm
  } else {
    # Fallback: estimate from eye position data
    max_x_px <- max(eye_feet_data$avg_eye_x_px, na.rm = TRUE)
    max_y_px <- max(eye_feet_data$avg_eye_y_px, na.rm = TRUE)
    screen_width_cm <- max_x_px / typical_pxPerCm
    screen_height_cm <- max_y_px / typical_pxPerCm
  }

  # Add 50% margin around screen
  x_margin_cm <- 0.5 * screen_width_cm
  y_margin_cm <- 0.5 * screen_height_cm
  x_min <- -x_margin_cm
  x_max <- screen_width_cm + x_margin_cm
  y_min <- -y_margin_cm
  y_max <- screen_height_cm + y_margin_cm
  
  # Add extra padding to Y-axis to prevent point cutoff
  y_padding <- 0.1 * (y_max - y_min)
  y_min_padded <- y_min - y_padding
  y_max_padded <- y_max + y_padding
  
  # Prepare data for plotting
  eye_feet_data <- eye_feet_data %>%
    mutate(
      foot_x_cm_clipped = pmax(x_min, pmin(x_max, avg_eye_x_cm)),
      foot_y_cm_clipped = pmax(y_min, pmin(y_max, avg_eye_y_cm)),
      ratio_continuous = pmax(0.7, pmin(1.4, distance_ratio)),
      session_id = as.character(participant)
    )
  
  eye_feet_data <- eye_feet_data %>%
    mutate(session_limited = factor(session_id))

  n_sessions <- n_distinct(eye_feet_data$session_id)
  statement <- make_statement(eye_feet_data)

  p <- NULL
  tryCatch({
    p <- ggplot(eye_feet_data, aes(x = foot_x_cm_clipped, y = foot_y_cm_clipped)) +
      geom_rect(aes(xmin = 0, xmax = screen_width_cm,
                    ymin = 0, ymax = screen_height_cm),
                fill = NA, color = "black", linewidth = 0.3, alpha = 0.7) +
      geom_point(aes(fill = ratio_continuous, shape = session_limited), 
                 size = 4, alpha = 0.9, color = "black", stroke = 0.2) +
      scale_fill_gradientn(
        colors = c("#3B4CC0", "#89A1F0", "#FDE725", "#F07E26", "#B2182B"),
        values = scales::rescale(log10(c(0.7, 0.85, 1.0, 1.2, 1.4))),
        limits = c(0.7, 1.4),
        trans = "log10",
        oob = scales::squish,
        name = "Measured over requested",
        breaks = seq(0.7, 1.4, by = 0.1),
        labels = c("0.7", "", "", "1.0", "", "", "", "1.4"),
        guide = guide_colorbar(
          barheight = unit(0.5, "cm"),
          barwidth = unit(5, "cm"),
          title.position = "top",
          ticks.colour = "black",
          ticks.linewidth = 0.75,
          ticks.position = "outward"
        )
      ) +
      scale_shape_manual(
        values = {
          all_shapes <- c(21, 22, 23, 24, 25)
          n_participants <- nlevels(eye_feet_data$session_limited)
          rep(all_shapes, length.out = n_participants)
        },
        name = "",
        guide = guide_legend(
          ncol = 4,
          byrow = TRUE,
          key.spacing.y = unit(0.07, "cm"),
          key.spacing.x = unit(0.07, "cm")
        )
      ) +
      scale_y_reverse(limits = c(y_max_padded, y_min_padded), expand = c(0,0)) +
      coord_fixed(xlim = c(x_min, x_max), expand = FALSE) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.justification = "center",
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
        legend.key.height = unit(0.4, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.margin = margin(l = 2, r = 2, t = 5, b = 5),
        legend.background = element_rect(fill = NA, colour = NA),
        plot.margin = margin(t = 20, r = 20, b = 40, l = 20, "pt"),
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank()
      ) +
      labs(
        subtitle = "Measured over requested distance vs.\nfoot position during check",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)",
        caption = "Rectangle shows screen boundaries. Points beyond 50% margin are clipped to plot area.\nOrigin (0,0) is at top-left corner of screen."
      ) +
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      annotate("text", x = x_min+1, y = y_min + 1,
               label = paste0("N = ", nrow(eye_feet_data), "\n",
                             "Sessions= ", n_sessions, "\n",
                             "Mean = ", round(mean(eye_feet_data$distance_ratio, na.rm = TRUE), 3), "\n",
                             "SD = ", round(sd(eye_feet_data$distance_ratio, na.rm = TRUE), 3)),
               hjust = 0, vjust = 1, size = 3, color = "black")
  }, error = function(e) {
    p <<- NULL
  })

  n_sessions <- n_distinct(eye_feet_data$session_id)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_sessions, per_row = 3, row_increase = 0.08)

  if(!is.null(p)) {
    return(list(plot = p, height = plot_height))
  } else {
    return(NULL)
  }
}

plot_foot_position_during_calibration <- function(distanceCalibrationResults) {
  eye_feet_data <- distanceCalibrationResults$feet_calib

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }

  # Filter out rows with invalid coordinates
  eye_feet_data <- eye_feet_data %>%
    filter(!is.na(left_x), !is.na(left_y), !is.na(right_x), !is.na(right_y),
           !is.na(avg_eye_x_cm), !is.na(avg_eye_y_cm), !is.na(distance_ratio))

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }
  
  # Calculate mean and SD of distance ratio
  mean_ratio <- mean(eye_feet_data$distance_ratio, na.rm = TRUE)
  sd_ratio <- sd(eye_feet_data$distance_ratio, na.rm = TRUE)
  
  # Get actual screen dimensions from the data if available
  typical_pxPerCm <- median(eye_feet_data$pxPerCm, na.rm = TRUE)
  
  # Try to get screen dimensions from screenWidthPx/screenHeightPx columns if available
  if ("screenWidthPx" %in% names(eye_feet_data) && "screenHeightPx" %in% names(eye_feet_data)) {
    screen_width_px <- median(as.numeric(eye_feet_data$screenWidthPx), na.rm = TRUE)
    screen_height_px <- median(as.numeric(eye_feet_data$screenHeightPx), na.rm = TRUE)
    screen_width_cm <- screen_width_px / typical_pxPerCm
    screen_height_cm <- screen_height_px / typical_pxPerCm
  } else {
    # Fallback: estimate from eye position data (may underestimate actual screen size)
    max_x_px <- max(c(eye_feet_data$left_x, eye_feet_data$right_x), na.rm = TRUE)
    max_y_px <- max(c(eye_feet_data$left_y, eye_feet_data$right_y), na.rm = TRUE)
    screen_width_cm <- max_x_px / typical_pxPerCm
    screen_height_cm <- max_y_px / typical_pxPerCm
  }

  # Calculate plot range to fit all data without clipping
  # Add 50% margin around screen OR expand to fit all data, whichever is larger
  x_margin_cm <- 0.5 * screen_width_cm
  y_margin_cm <- 0.5 * screen_height_cm
  
  # Get actual data range
  x_data_min <- min(eye_feet_data$avg_eye_x_cm, na.rm = TRUE)
  x_data_max <- max(eye_feet_data$avg_eye_x_cm, na.rm = TRUE)
  y_data_min <- min(eye_feet_data$avg_eye_y_cm, na.rm = TRUE)
  y_data_max <- max(eye_feet_data$avg_eye_y_cm, na.rm = TRUE)
  
  # Expand limits to include all data
  x_min <- min(-x_margin_cm, x_data_min - 2)  # At least 50% margin or 2cm beyond min data
  x_max <- max(screen_width_cm + x_margin_cm, x_data_max + 2)  # At least 50% margin or 2cm beyond max data
  y_min <- min(-y_margin_cm, y_data_min - 2)
  y_max <- max(screen_height_cm + y_margin_cm, y_data_max + 2)
  
  # Prepare data for plotting
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Create session identifier for color
      session_id = as.character(participant)
    )

  n_sessions <- n_distinct(eye_feet_data$session_id)

  # Create statement for calibration parameters (safe, short)
  statement <- distanceCalibrationResults$statement
  param_tbl <- build_param_table(eye_feet_data)

  # Create the plot
  tryCatch({
    p <- ggplot(eye_feet_data, aes(x = avg_eye_x_cm, y = avg_eye_y_cm)) +
      # Add screen boundary rectangle
      geom_rect(aes(xmin = 0, xmax = screen_width_cm,
                    ymin = 0, ymax = screen_height_cm),
                fill = NA, color = "black", linewidth = 0.3, alpha = 0.7) +
      # Add points colored by participant
      geom_point(aes(color = participant), size = 3, alpha = 0.8) +
      # Flip Y-axis to make origin truly at top-left (Y increases downward)
      scale_y_reverse(limits = c(y_max, y_min), expand = c(0,0)) +
      scale_color_manual(values = colorPalette) +
      guides(color = guide_legend(
        ncol = 3,
        title = "",
        override.aes = list(size = 2),
        keywidth = unit(1.2, "lines"),
        keyheight = unit(0.8, "lines")
      )) +
      # Lock aspect ratio
      coord_fixed(xlim = c(x_min, x_max), expand = FALSE) +
      theme_classic() +
      theme(
        legend.position = "right",
        legend.box = "vertical",
        legend.justification = "left",
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(0.4, "cm"),
        legend.margin = margin(l = 2, r = 2, t = 1, b = 1),
        legend.background = element_rect(fill = NA, colour = NA),
        plot.margin = margin(t = 20, r = 10, b = 20, l = 20, "pt"),  # Add plot margins to prevent text clipping
        # Open plot style - only bottom and left axis lines
        axis.line.x = element_line(color = "black"),
        axis.line.y = element_line(color = "black"),
        axis.line.x.top = element_blank(),
        axis.line.y.right = element_blank()
      ) +
      # Labels - set these LAST to ensure they stick
      labs(
        subtitle = "Foot position during calibration",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)",
        caption = "Rectangle shows screen boundaries. Plot range expanded to show all data.\nOrigin (0,0) is at top-left corner of screen."
      ) +
      # Screen annotation (moved close to screen outline, same size as Y label, not bold)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
      # Calibration parameters (bottom right corner)
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      # Summary statistics (pushed further down to avoid clipping)
      annotate("text", x = x_min+1, y = y_min + 1,
               label = paste0("N = ", nrow(eye_feet_data), "\n",
                             "Sessions= ", n_sessions, "\n",
                             "Mean = ", round(mean_ratio, 3), "\n",
                             "SD = ", round(sd_ratio, 3)),
               hjust = 0, vjust = 1, size = 3, color = "black")
  }, error = function(e) {
    p <<- NULL
  })

  # Calculate height based on number of participants
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_sessions, per_row = 3, row_increase = 0.08)

  # Return the plot WITH height (expected by calling code)
  if(exists("p") && !is.null(p)) {
    return(list(plot = p, height = plot_height))
  } else {
    return(NULL)
  }
}

get_bs_vd <- function(data_list) {
  # Get blindspot viewing distance left and right from data list
  bs_vwing_ds <- tibble()
  
  # Return empty tibble if data_list is empty to avoid subscript out of bounds error
  if (length(data_list) == 0) {
    return(bs_vwing_ds)
  }
  
  for (i in 1:length(data_list)) {
    if ("viewingDistanceByBlindspot1Cm" %in% names(data_list[[i]])) {
      t <- data_list[[i]] %>%
        select(participant,
               viewingDistanceByBlindspot1Cm,
               viewingDistanceByBlindspot2Cm) %>%
        mutate(viewingDistanceByBlindspot1Cm = as.numeric(viewingDistanceByBlindspot1Cm),
               viewingDistanceByBlindspot2Cm = as.numeric(viewingDistanceByBlindspot2Cm)) %>%
        distinct() %>%
        filter(!is.na(viewingDistanceByBlindspot1Cm),
               !is.na(viewingDistanceByBlindspot2Cm))
      bs_vwing_ds <- rbind(bs_vwing_ds, t)
    }
  }
  if (nrow(bs_vwing_ds) > 0) {
    bs_vwing_ds <- bs_vwing_ds %>%
      mutate(m = (viewingDistanceByBlindspot1Cm + viewingDistanceByBlindspot2Cm) / 2) %>%
      mutate(sd = sqrt((log10(viewingDistanceByBlindspot1Cm) - log10(m))^2 + (log10(viewingDistanceByBlindspot2Cm) - log10(m))^2))
  }
  return(bs_vwing_ds)
}

plot_distance <- function(distanceCalibrationResults, calibrateTrackDistanceCheckLengthSDLogAllowed) {

  distance <- distanceCalibrationResults$checkJSON
  statement <- distanceCalibrationResults$statement
  camera <- distanceCalibrationResults$camera
  if (nrow(distance) == 0) {return(NULL)}
  
  
  # Prepare individual measurements for plotting (not averaged)
  distance_individual <- distance %>%
    mutate(
      # Add consistent logarithmic horizontal jitter to x-axis variable (unbiased for log scales)
      checkDistanceRequestedCm = as.numeric(requestedEyesToPointCm),
      checkDistanceMeasuredCm = as.numeric(eyesToPointCm),
      CheckDistanceRequestedCm_jitter = add_log_jitter(checkDistanceRequestedCm, jitter_percent = 2, seed = 42),
      # For "Measured vs. requested distance" plot (p1):
      # x-axis: rulerBasedEyesToPointCm, y-axis: imageBasedEyesToPointCm
      p1_x = as.numeric(rulerBasedEyesToPointCm),
      p1_y = as.numeric(imageBasedEyesToPointCm),
      p1_x_jitter = add_log_jitter(p1_x, jitter_percent = 2, seed = 42),
      # For "Measured over requested distance" plot (p2):
      # x-axis: rulerBasedEyesToPointCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToPointCm
      p2_x = as.numeric(rulerBasedEyesToPointCm),
      p2_y = as.numeric(imageBasedEyesToPointCm) / as.numeric(rulerBasedEyesToPointCm),
      p2_x_jitter = add_log_jitter(p2_x, jitter_percent = 2, seed = 42)
    )

  # Calculate the ratio Y/X for the second plot
  distance_individual <- distance_individual %>%
    mutate(
      credit_card_fraction = checkDistanceMeasuredCm / checkDistanceRequestedCm
    )
  

  # Calculate mean and SD of the ratio for reporting
  mean_fraction <- mean(distance_individual$credit_card_fraction, na.rm = TRUE)
  sd_fraction <- sd(distance_individual$credit_card_fraction, na.rm = TRUE)
  
  # Format for display
  mean_formatted <- format(round(mean_fraction, 3), nsmall = 3)
  sd_formatted <- format(round(sd_fraction, 3), nsmall = 3)

     # Use appropriate scale limits for cleaner axis display - use JITTERED values
  min_val <- 5 * floor(min(c(distance_individual$CheckDistanceRequestedCm_jitter, 
                              distance_individual$checkDistanceMeasuredCm), na.rm = TRUE) / 5) 
  min_val = max(10, min_val)
  max_val <- 5 * ceiling(max(c(distance_individual$CheckDistanceRequestedCm_jitter,
                               distance_individual$checkDistanceMeasuredCm), na.rm = TRUE) / 5)
  
  # Scale limits for p1 (Measured vs. requested distance plot)
  p1_min_val <- 5 * floor(min(c(distance_individual$p1_x_jitter, 
                                 distance_individual$p1_y), na.rm = TRUE) / 5) 
  p1_min_val = max(10, p1_min_val)
  p1_max_val <- 5 * ceiling(max(c(distance_individual$p1_x_jitter,
                                   distance_individual$p1_y), na.rm = TRUE) / 5)
  
  # Calculate dynamic y-axis limits for the fraction plot based on actual data
  minFrac <- max(0.1, min(0.5, floor(distance_individual$credit_card_fraction * 10) / 10))
  maxFrac <- max(1.5, ceiling(distance_individual$credit_card_fraction * 10) / 10)
  
  # Scale limits for p2 (Measured over requested distance plot)
  p2_x_min_val <- 5 * floor(min(distance_individual$p2_x_jitter, na.rm = TRUE) / 5)
  p2_x_min_val <- max(10, p2_x_min_val)
  p2_x_max_val <- 5 * ceiling(max(distance_individual$p2_x_jitter, na.rm = TRUE) / 5)
  p2_y_minFrac <- max(0.1, min(0.5, floor(min(distance_individual$p2_y, na.rm = TRUE) * 10) / 10))
  p2_y_maxFrac <- max(1.5, ceiling(max(distance_individual$p2_y, na.rm = TRUE) * 10) / 10)
  
  # Calculate mean and SD for p2 ratio
  p2_mean_fraction <- mean(distance_individual$p2_y, na.rm = TRUE)
  p2_sd_fraction <- sd(distance_individual$p2_y, na.rm = TRUE)
  p2_mean_formatted <- format(round(p2_mean_fraction, 3), nsmall = 3)
  p2_sd_formatted <- format(round(p2_sd_fraction, 3), nsmall = 3)
  
  # Plot 1: Measured vs. requested distance (INDIVIDUAL MEASUREMENTS)
  # x-axis: rulerBasedEyesToPointCm, y-axis: imageBasedEyesToPointCm
  p1_data <- distance_individual %>%
    arrange(participant, p1_x_jitter) %>%
    filter(!is.na(p1_x_jitter), !is.na(p1_y))
  
  # Plot 2 data: Measured over requested distance
  # x-axis: rulerBasedEyesToPointCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToPointCm
  p2_data <- distance_individual %>%
    arrange(participant, p2_x_jitter) %>%
    filter(!is.na(p2_x_jitter), !is.na(p2_y))
  
  # Keep original filter for other plots
  distance_individual <- distance_individual %>%
    arrange(participant, CheckDistanceRequestedCm_jitter) %>%  # Ensure proper ordering
    filter(!is.na(CheckDistanceRequestedCm_jitter),
           !is.na(checkDistanceMeasuredCm))
  p1 <- NULL
  if (nrow(p1_data) > 0) {
  p1 <- ggplot() + 
      geom_line(data=p1_data,
              aes(x = p1_x_jitter, 
                    y = p1_y,
                  color = participant, 
                  group = participant), alpha = 0.7) +
      geom_point(data=p1_data, 
               aes(x = p1_x_jitter, 
                     y = p1_y,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                          label = paste0('N=', n_distinct(p1_data$participant))) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(p1_min_val, p1_max_val),
        breaks = scales::log_breaks(n = 6),
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_y_log10(limits = c(p1_min_val, p1_max_val), breaks = scales::log_breaks(n=8), labels = scales::label_number(accuracy = 10)) + 
    scale_color_manual(values= colorPalette) + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) + 
    guides(color = guide_legend(
      ncol = 3,  # More columns to fit more participants horizontally
      title = "",
      override.aes = list(size = 2),  # Smaller points in legend
      keywidth = unit(1.2, "lines"),  # Reduce key width
      keyheight = unit(0.8, "lines")  # Reduce key height
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    coord_fixed() +  
    labs(subtitle = 'Measured vs. requested distance',
         x = 'rulerBasedEyesToPointCm (cm)',
         y = 'imageBasedEyesToPointCm (cm)',
           caption = 'Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
  
  }
 
  # Plot 2: Measured over requested distance (INDIVIDUAL MEASUREMENTS)
  # x-axis: rulerBasedEyesToPointCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToPointCm
  p2 <- ggplot() + 
    geom_line(data=p2_data,
              aes(x = p2_x_jitter, 
                  y = p2_y,
                  color = participant, 
                  group = participant), alpha = 0.7) +
    geom_point(data=p2_data, 
               aes(x = p2_x_jitter, 
                   y = p2_y,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(p2_data$participant), '\n',
                                       'Mean=', p2_mean_formatted, '\n',
                                       'SD=', p2_sd_formatted)) + 
    geom_hline(yintercept = 1, linetype = "dashed") + # y=1 line (perfect ratio)
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    scale_x_log10(
      limits = c(p2_x_min_val, p2_x_max_val),
      breaks = scales::log_breaks(n = 6),
                  expand = expansion(mult = c(0.05, 0.05)),
      labels = scales::label_number(accuracy = 1)
    ) + 
    scale_y_log10(limits = c(p2_y_minFrac, p2_y_maxFrac),
                   breaks = seq(0.6, 1.4, by = 0.1)) + 
    scale_color_manual(values= colorPalette) + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) + 
    guides(color = guide_legend(
      ncol = 3,
      title = "",
      override.aes = list(size = 2),
      keywidth = unit(1.2, "lines"),
      keyheight = unit(0.8, "lines")
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    labs(subtitle = 'Measured over requested distance',
         x = 'rulerBasedEyesToPointCm (cm)',
         y = 'imageBasedEyesToPointCm / rulerBasedEyesToPointCm',
         caption = 'Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')

  # Plot 4: Eye feet position vs distance error
  p4_result <- plot_eye_feet_position(distanceCalibrationResults)
  p4 <- if (!is.null(p4_result)) p4_result$plot else NULL
  
  # Plot 4b: Foot position during calibration (colored by participant, no clipping)
  p4b_result <- plot_foot_position_during_calibration(distanceCalibrationResults)
  p4b <- if (!is.null(p4b_result)) p4b_result$plot else NULL
  p4b_height <- if (!is.null(p4b_result)) p4b_result$height else NULL
  
  # Plot 5b: Focal length ratio (calibration/check) vs. check
  p5b <- NULL
  # Use TJSON (calibration) and checkJSON (check) data
  # Both now have fOverWidth directly
  tjson_data <- distanceCalibrationResults$TJSON
  check_json_data <- distanceCalibrationResults$checkJSON
  
  if (nrow(tjson_data) > 0 && nrow(check_json_data) > 0) {
    # Get median fOverWidth per participant from calibration (TJSON has fOverWidth directly)
    calib_fOverWidth <- tjson_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_calibration = median(fOverWidth, na.rm = TRUE), .groups = "drop")
    
    # Get median fOverWidth per participant from check (checkJSON now has fOverWidth directly)
    check_fOverWidth <- check_json_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_check = median(fOverWidth, na.rm = TRUE), .groups = "drop")
    
    # Join calibration and check data by participant
    plot_data_p5b <- calib_fOverWidth %>%
      inner_join(check_fOverWidth, by = "participant") %>%
      filter(!is.na(fOverWidth_calibration), !is.na(fOverWidth_check),
             is.finite(fOverWidth_calibration), is.finite(fOverWidth_check))
    
    if (nrow(plot_data_p5b) > 0) {
      # Calculate the ratio of calibration to check
      ratio_data_p5b <- plot_data_p5b %>%
        mutate(fOverWidth_ratio = fOverWidth_calibration / fOverWidth_check) %>%
        filter(is.finite(fOverWidth_ratio))
    
    if (nrow(ratio_data_p5b) > 0) {
      # Calculate x-axis limits
      x_min <- min(ratio_data_p5b$fOverWidth_check, na.rm = TRUE) * 0.9
      x_max <- max(ratio_data_p5b$fOverWidth_check, na.rm = TRUE) * 1.1
      
      # Calculate y-axis limits (ratio should be close to 1)
      y_min <- min(ratio_data_p5b$fOverWidth_ratio, na.rm = TRUE) * 0.95
      y_max <- max(ratio_data_p5b$fOverWidth_ratio, na.rm = TRUE) * 1.05
      # Ensure y=1 is visible
      y_min <- min(y_min, 0.95)
      y_max <- max(y_max, 1.05)
      
      p5b <- ggplot(ratio_data_p5b, aes(x = fOverWidth_check, y = fOverWidth_ratio)) +
        geom_point(aes(color = participant), size = 3, alpha = 0.8) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(ratio_data_p5b$participant))) +
        scale_x_log10(limits = c(x_min, x_max), 
                      breaks = scales::log_breaks(n = 8)) +
        scale_y_continuous(limits = c(y_min, y_max)) +
        annotation_logticks(sides = "b") +
        scale_color_manual(values = colorPalette) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 1.5),
          keywidth = unit(0.8, "lines"),
          keyheight = unit(0.6, "lines")
        )) +
        theme_classic() +
        theme(
          legend.position = "right",
          legend.box = "vertical",
          legend.justification = "left",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
        labs(subtitle = 'Focal length: calibration/check vs. check',
             x = 'fOverWidth: check',
             y = 'fOverWidth: calibration / check',
             caption = 'Dashed line shows y=1 (perfect agreement)')
      }
    }
  }
  
  # Plot 6: Calibrated over mean factorVpxCm vs. spot diameter
  p6 <- NULL
  if ("calibrateTrackDistanceIpdVpx" %in% names(distance) && nrow(distance) > 0) {
    # Get camera data
    camera_data <- distanceCalibrationResults$camera
    # Get blindspot diameter data
    blindspot_data <- distanceCalibrationResults$blindspot
    
    if (nrow(camera_data) > 0 && nrow(blindspot_data) > 0) {
      # Join distance data with camera data
      factor_data <- distance %>%
        filter(!is.na(calibrateTrackDistanceIpdVpx),
               !is.na(calibrateTrackDistanceMeasuredCm)) %>%
        left_join(camera_data %>% select(PavloviaParticipantID, factorVpxCm),
                  by = c("participant" = "PavloviaParticipantID")) %>%
        filter(!is.na(factorVpxCm))
      
      if (nrow(factor_data) > 0) {
        # Calculate geometric mean per participant (session)
        geo_mean_data <- factor_data %>%
          mutate(
            product = calibrateTrackDistanceMeasuredCm * calibrateTrackDistanceIpdVpx
          ) %>%
          group_by(participant) %>%
          summarize(
            geoMeanFactorVpxCm = 10^mean(log10(product), na.rm = TRUE),
            .groups = "drop"
          )
        
        # Join with blindspot data and calculate ratio
        plot_data <- factor_data %>%
          left_join(geo_mean_data, by = "participant") %>%
          left_join(blindspot_data, by = "participant") %>%
          filter(!is.na(geoMeanFactorVpxCm), 
                 is.finite(geoMeanFactorVpxCm),
                 !is.na(`_calibrateTrackDistanceBlindspotDiameterDeg`)) %>%
          mutate(
            ratio = factorVpxCm / geoMeanFactorVpxCm,
            spotDeg = `_calibrateTrackDistanceBlindspotDiameterDeg`
          ) %>%
          filter(is.finite(ratio))
        
        if (nrow(plot_data) > 0) {
          # Calculate log scale limits
          x_min <- min(plot_data$spotDeg, na.rm = TRUE) * 0.9
          x_max <- max(plot_data$spotDeg, na.rm = TRUE) * 1.1
          y_min <- min(plot_data$ratio, na.rm = TRUE) * 0.9
          y_max <- max(plot_data$ratio, na.rm = TRUE) * 1.1
          
          plot_data <- plot_data %>%
            mutate(spotDeg_jitter = add_log_jitter(spotDeg, jitter_percent = 0.5, seed = 42))

          p6 <- ggplot(plot_data, aes(x = spotDeg_jitter, y = ratio)) +
            geom_point(aes(color = participant), size = 3, alpha = 0.8) +
            geom_hline(yintercept = 1, linetype = "dashed", color = "black", linewidth = 0.8) +
            ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                                label = paste0('N=', n_distinct(plot_data$participant))) +
            scale_x_log10(limits = c(x_min, x_max), 
                          breaks = scales::log_breaks(n = 8)) +
            scale_y_log10(limits = c(y_min, y_max), 
                          breaks = scales::log_breaks(n = 8)) +
            annotation_logticks() +
            scale_color_manual(values = colorPalette) +
            ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
            guides(color = guide_legend(
              ncol = 3,
              title = "",
              override.aes = list(size = 1.5),
              keywidth = unit(0.8, "lines"),
              keyheight = unit(0.6, "lines")
            )) +
            theme_classic() +
            theme(
              legend.position = "right",
              legend.box = "vertical",
              legend.justification = "left",
              legend.text = element_text(size = 6),
              legend.spacing.y = unit(0, "lines"),
              legend.key.size = unit(0.4, "cm"),
              plot.margin = margin(5, 5, 5, 5, "pt")
            ) +
            labs(subtitle = 'Calibrated over mean factorVpxCm vs. spot diameter',
                 x = 'Spot diameter (deg)',
                 y = 'factorVpxCm over geometric mean',
                 caption = 'Dashed line shows y=1 (perfect agreement with session mean)\nGeometric mean = 10^mean(log10(measuredEyeToCameraCm × ipdOverWidth × widthVpx))')
        }
      }
    }
  }
  
  # Plot 7: Histogram of fVpx median(calibration) / median(check)
  # BUG FIX: Use raw_fVpx data and compute MEDIAN ratio per participant
  # This ensures the summary histogram shows the median of individual ratios,
  # consistent with the raw histogram (Plot 10)
  p7 <- NULL
  if (nrow(distance) > 0 &&
      "raw_fVpx" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$raw_fVpx) > 0) {
    
    # Use raw data (same source as Plot 10) and compute MEDIAN ratio per participant
    raw_data <- distanceCalibrationResults$raw_fVpx %>%
      filter(is.finite(relative), relative > 0)
    
    # Compute MEDIAN ratio per participant (fixes bug where last value was shown instead of median)
    ratio_data <- raw_data %>%
      group_by(participant) %>%
      summarize(
        ratio = median(relative, na.rm = TRUE),  # Use MEDIAN of raw ratios
        .groups = "drop"
      ) %>%
      filter(is.finite(ratio), ratio > 0) %>%
      mutate(PavloviaParticipantID = participant)

    if (nrow(ratio_data) > 0) {
      # SD of ratio (linear scale)
      sd_ratio <- sd(ratio_data$ratio, na.rm = TRUE)

      # For summary histogram (one value per participant), use actual values without binning
      # This ensures displayed positions match table values exactly
      # Only use binning for stacking when multiple participants have very similar values
      bin_w_linear <- 0.01
      ratio_data <- ratio_data %>%
        mutate(
          bin_center = round(ratio / bin_w_linear) * bin_w_linear,
          # Store actual value for display
          actual_ratio = ratio
        ) %>%
        arrange(bin_center, participant) %>%
        group_by(bin_center) %>%
        mutate(
          stack_position = row_number(),
          dot_y = stack_position
        ) %>%
        ungroup()
      
      max_count <- max(ratio_data$dot_y)
      x_min <- min(ratio_data$actual_ratio, na.rm = TRUE) - 0.01  # Add margin
      x_max <- max(ratio_data$actual_ratio, na.rm = TRUE) + 0.01 # Add margin  
      
      p7 <- ggplot(ratio_data, aes(x = ratio)) +
        # Dot stacked points - use actual_ratio for precise positioning (matches table values)
        geom_point(aes(x = actual_ratio, y = dot_y, color = participant), size = 6, alpha = 0.85) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                            label = statement, size = 3, family = "sans", fontface = "plain") +
        scale_color_manual(values = colorPalette) +
        scale_y_continuous(
          limits = c(0, max(8, max_count + 1)),
          expand = expansion(mult = c(0, 0.1)),
          breaks = function(x) seq(0, ceiling(max(x)), by = 1)
        ) +
        scale_x_continuous(limits = c(x_min, x_max),
                           n.breaks = 8) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(ratio_data$participant),
                                           '\nSD(x) = ', format(round(sd_ratio, 3), nsmall = 3)),
                            size = 3, family = "sans", fontface = "plain") +
        guides(color = guide_legend(
          ncol = 4,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        labs(
          subtitle = 'histogram of fOverWidth median(calibration)/median(check)',
          x = "median(calibration)/median(check)",
          y = "Count",
          caption = "Each dot = median ratio for one participant"
        ) +
        theme_bw() +
        theme(
          legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14)
        )
    }
  }
  
  # Plot 8: Histogram of raw pxPerCm / remeasured
  p8 <- NULL
  p8_max_count <- 0
  if ("raw_pxPerCm" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$raw_pxPerCm) > 0) {
    
    raw_pxPerCm_data <- distanceCalibrationResults$raw_pxPerCm %>%
      filter(is.finite(relative), relative > 0)
    
    if (nrow(raw_pxPerCm_data) > 0) {
      # SD of log10(relative)
      sd_log10_relative <- sd(log10(raw_pxPerCm_data$relative), na.rm = TRUE)
      
      # Dot-stack histogram in log space
      bin_w_log <- 0.02  # ~4.6% per bin
      raw_pxPerCm_data <- raw_pxPerCm_data %>%
        mutate(
          log_relative = log10(relative),
          bin_center_log = round(log_relative / bin_w_log) * bin_w_log,
          bin_center = 10^bin_center_log
        ) %>%
        arrange(bin_center, participant) %>%
        group_by(bin_center) %>%
        mutate(
          stack_position = row_number(),
          dot_y = stack_position
        ) %>%
        ungroup()
      
      p8_max_count <- max(raw_pxPerCm_data$dot_y)
      x_min <- min(raw_pxPerCm_data$bin_center, na.rm = TRUE) 
      x_max <- max(raw_pxPerCm_data$bin_center, na.rm = TRUE)
      
      # Calculate appropriate breaks for the data range
      log_range <- log10(x_max) - log10(x_min)
      if (log_range < 0.1) {
        # For very narrow ranges, create custom breaks that fit the data
        n_breaks <- 5
        log_breaks_custom <- seq(log10(x_min), log10(x_max), length.out = n_breaks)
        x_breaks <- 10^log_breaks_custom
      } else {
        x_breaks <- scales::log_breaks(n = 8)(c(x_min, x_max))
      }
      
      p8 <- ggplot(raw_pxPerCm_data, aes(x = relative)) +
        # Dot stacked points
        geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                            label = statement, size = 3, family = "sans", fontface = "plain") +
        scale_color_manual(values = colorPalette) +
        scale_y_continuous(
          limits = c(0, max(8, p8_max_count + 1)),
          expand = expansion(mult = c(0, 0.1)),
          breaks = function(x) seq(0, ceiling(max(x)), by = 1)
        ) +
        scale_x_log10(limits = c(x_min, x_max), breaks = x_breaks) +
        annotation_logticks(sides = "b") +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(raw_pxPerCm_data$participant),
                                           '\nSD(log10(x)) = ', format(round(sd_log10_relative, 3), nsmall = 3)),
                            size = 3, family = "sans", fontface = "plain") +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        labs(
          subtitle = 'Histogram of raw pxPerCm / remeasured',
          x = "pxPerCm / remeasured",
          y = "Count"
        ) +
        theme_bw() +
        theme(
          legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14)
        )
    }
  }
  
  # Plot 9: Histogram of raw objectMeasuredCm / median
  p9 <- NULL
  p9_max_count <- 0
  if ("raw_objectMeasuredCm" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$raw_objectMeasuredCm) > 0) {
    
    raw_objectCm_data <- distanceCalibrationResults$raw_objectMeasuredCm %>%
      filter(is.finite(relative), relative > 0)
    
    if (nrow(raw_objectCm_data) > 0) {
      # SD of log10(relative)
      sd_log10_relative <- sd(log10(raw_objectCm_data$relative), na.rm = TRUE)
      
      # Dot-stack histogram in log space
      bin_w_log <- 0.02  # ~4.6% per bin
      raw_objectCm_data <- raw_objectCm_data %>%
        mutate(
          log_relative = log10(relative),
          bin_center_log = round(log_relative / bin_w_log) * bin_w_log,
          bin_center = 10^bin_center_log
        ) %>%
        arrange(bin_center, participant) %>%
        group_by(bin_center) %>%
        mutate(
          stack_position = row_number(),
          dot_y = stack_position
        ) %>%
        ungroup()
      
      p9_max_count <- max(raw_objectCm_data$dot_y)
      x_min <- min(raw_objectCm_data$bin_center, na.rm = TRUE)
      x_max <- max(raw_objectCm_data$bin_center, na.rm = TRUE)
      
      p9 <- ggplot(raw_objectCm_data, aes(x = relative)) +
        # Dot stacked points
        geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                            label = statement, size = 3, family = "sans", fontface = "plain") +
        scale_color_manual(values = colorPalette) +
        scale_y_continuous(
          limits = c(0, max(8, p9_max_count + 1)),
          expand = expansion(mult = c(0, 0.1)),
          breaks = function(x) seq(0, ceiling(max(x)), by = 1)
        ) +
        scale_x_log10(limits = c(x_min, x_max),breaks = scales::log_breaks(n=9)) +
        annotation_logticks(sides = "b") +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(raw_objectCm_data$participant),
                                           '\nSD(log10(x)) = ', format(round(sd_log10_relative, 3), nsmall = 3)),
                            size = 3, family = "sans", fontface = "plain") +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        labs(
          subtitle = 'Histogram of raw objectMeasuredCm / median',
          x = "objectMeasuredCm / median",
          y = "Count"
        ) +
        theme_bw() +
        theme(
          legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14)
        )
    }
  }
  
  # Plot 10: Histogram of fVpx calibration / median(check) (from array data)
  p10 <- NULL
  p10_max_count <- 0
  if ("raw_fVpx" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$raw_fVpx) > 0) {
    
    raw_factor_data <- distanceCalibrationResults$raw_fVpx %>%
      filter(is.finite(relative), relative > 0)
    
    if (nrow(raw_factor_data) > 0) {
      # SD of relative (linear scale)
      sd_relative <- sd(raw_factor_data$relative, na.rm = TRUE)

      # Dot-stack histogram in linear space
      bin_w_linear <- 0.01
      raw_factor_data <- raw_factor_data %>%
        mutate(
          bin_center = round(relative / bin_w_linear) * bin_w_linear
        ) %>%
        arrange(bin_center, participant) %>%
        group_by(bin_center) %>%
        mutate(
          stack_position = row_number(),
          dot_y = stack_position
        ) %>%
        ungroup()
      
      p10_max_count <- max(raw_factor_data$dot_y)
      x_min <- min(raw_factor_data$bin_center, na.rm = TRUE)
      x_max <- max(raw_factor_data$bin_center, na.rm = TRUE)
      
      p10 <- ggplot(raw_factor_data, aes(x = relative)) +
        # Dot stacked points
        geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                            label = statement, size = 3, family = "sans", fontface = "plain") +
        scale_color_manual(values = colorPalette) +
        scale_y_continuous(
          limits = c(0, max(8, p10_max_count + 1)),
          expand = expansion(mult = c(0, 0.1)),
          breaks = function(x) seq(0, ceiling(max(x)), by = 1)
        ) +
        scale_x_continuous(limits = c(x_min, x_max),
                           n.breaks = 8) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(raw_factor_data$participant),
                                           '\nSD(x) = ', format(round(sd_relative, 3), nsmall = 3)),
                            size = 3, family = "sans", fontface = "plain") +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        labs(
          subtitle = 'Histogram of fOverWidth: calibration/median(check)',
          x = "calibration/median(check)",
          y = "Count"
        ) +
        theme_bw() +
        theme(
          legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14)
        )
    }
  }
  
  # Plot 11: Histogram and scatter of fVpx/widthVpx for distance check sessions
  fvpx_over_width_hist <- NULL
  fvpx_over_width_scatter <- NULL
  fvpx_over_width_data <- tibble()
  fvpx_over_width_max_count <- 0
  if ("check_factor" %in% names(distanceCalibrationResults) &&
      "camera" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$check_factor) > 0 &&
      nrow(distanceCalibrationResults$camera) > 0) {
    
    check_data <- distanceCalibrationResults$check_factor
    if (!"calibrateTrackDistanceCheckBool" %in% names(check_data)) {
      check_data <- check_data %>% mutate(calibrateTrackDistanceCheckBool = NA)
    }
    
    check_data <- check_data %>%
      mutate(
        calibrateTrackDistanceCheckBool = coerce_to_logical(calibrateTrackDistanceCheckBool),
        medianFactorVpxCm = as.numeric(medianFactorVpxCm)
      ) %>%
      filter(!is.na(calibrateTrackDistanceCheckBool),
             calibrateTrackDistanceCheckBool,
             !is.na(medianFactorVpxCm),
             is.finite(medianFactorVpxCm))
    
    if (nrow(check_data) > 0) {
      # Use checkJSON data which now has fOverWidth directly
      check_json_data <- distanceCalibrationResults$checkJSON
      
      if (nrow(check_json_data) > 0) {
        # Get median fOverWidth per participant from check data (fOverWidth now directly in JSON)
        check_fOverWidth_median <- check_json_data %>%
          filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
          group_by(participant) %>%
          summarize(median_fOverWidth = median(fOverWidth, na.rm = TRUE), .groups = "drop")
        
      fOverWidth_over_width_data <- check_data %>%
        left_join(distanceCalibrationResults$camera %>%
                    select(PavloviaParticipantID, widthVpx),
                  by = "PavloviaParticipantID") %>%
          left_join(check_fOverWidth_median, by = c("PavloviaParticipantID" = "participant")) %>%
        mutate(
            # Use fOverWidth directly from checkJSON
            fOverWidth = median_fOverWidth
        ) %>%
        filter(!is.na(widthVpx), widthVpx > 0,
               !is.na(fOverWidth), is.finite(fOverWidth)) %>%
        mutate(participant = PavloviaParticipantID)
      } else {
        fOverWidth_over_width_data <- tibble()
      }
      
      if (nrow(fOverWidth_over_width_data) > 0) {
        # Histogram (dot-stacked)
        bin_width <- 0.02
        fOverWidth_over_width_data <- fOverWidth_over_width_data %>%
          mutate(
            bin_center = floor(fOverWidth / bin_width) * bin_width + bin_width/2
          ) %>%
          arrange(bin_center, participant) %>%
          group_by(bin_center) %>%
          mutate(
            stack_position = row_number(),
            dot_y = stack_position
          ) %>%
          ungroup()
        
        fOverWidth_max_count <- max(fOverWidth_over_width_data$dot_y)
        x_min <- min(fOverWidth_over_width_data$bin_center, na.rm = TRUE)
        x_max <- max(fOverWidth_over_width_data$bin_center, na.rm = TRUE)
        
        p11 <- ggplot(fOverWidth_over_width_data, aes(x = fOverWidth)) +
          geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                              label = statement, size = 3, family = "sans", fontface = "plain") +
          scale_color_manual(values = colorPalette) +
          scale_y_continuous(
            limits = c(0.5, fOverWidth_max_count + 0.5),
            expand = expansion(mult = c(0, 0)),
            breaks = function(x) seq(1, ceiling(max(x)), by = 1)
          ) +
          scale_x_continuous(limits = c(x_min, x_max)) +
          ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                              label = paste0('N=', n_distinct(fOverWidth_over_width_data$participant)),
                              size = 4, family = "sans", fontface = "plain") +
          guides(color = guide_legend(
            ncol = 4,
            title = "",
            override.aes = list(size = 2),
            keywidth = unit(0.3, "cm"),
            keyheight = unit(0.3, "cm")
          )) +
          labs(
            subtitle = 'Histogram of fOverWidth',
            x = "fOverWidth",
            y = "Count",
            caption = 'fOverWidth from calibration (TJSON) or computed from check data'
          ) +
          theme_bw() +
          theme(
            legend.key.size = unit(0, "mm"),
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
            legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
            legend.box.spacing = unit(0, "lines"),
            legend.spacing.y = unit(-10, "lines"),
            legend.spacing.x = unit(0, "lines"),
            legend.key.height = unit(0, "lines"),
            legend.key.width = unit(0, "mm"),
            legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
            legend.margin = margin(0, 0, 0, 0),
            legend.position = "top",
            legend.box = "vertical",
            legend.justification = 'left',
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size = 12, angle = 0, hjust=0, vjust=1),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 0)),
            plot.title.position = "plot",
            plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(t = 0)),
            plot.caption = element_text(size = 11),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
            strip.text = element_text(size = 14)
          )
        
        fOverWidth_hist <- list(
          plot = p11,
          height = compute_auto_height(base_height = 2.0, n_items = n_distinct(fOverWidth_over_width_data$participant), per_row = 3, row_increase = 0.08) +
            0.4 * fvpx_over_width_max_count
        )
        
        median_ratio_fw <- median(fOverWidth_over_width_data$fOverWidth, na.rm = TRUE)
        p12 <- ggplot(fOverWidth_over_width_data, aes(x = widthVpx, y = fOverWidth)) +
          geom_point(aes(color = participant), size = 3, alpha = 0.85) +
          geom_hline(yintercept = median_ratio_fw, linetype = "dashed") +
          ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                              label = paste0('N=', n_distinct(fOverWidth_over_width_data$participant),
                                             '\nmedian=', format(round(median_ratio_fw, 3), nsmall = 3)),
                              size = 3) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
          scale_color_manual(values = colorPalette) +
          scale_x_continuous(expand = expansion(mult = c(0.02, 0.05)),
                             breaks = scales::pretty_breaks(n = 6),
                             labels = scales::label_number(accuracy = 1)) +
          scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)),
                             breaks = scales::pretty_breaks(n = 6)) +
          labs(
            subtitle = 'fOverWidth vs. horizontalVpx',
            x = 'horizontalVpx (px)',
            y = 'fOverWidth',
            caption = 'One point per session with distance checking enabled (_calibrateDistanceCheckBool = TRUE)\nfOverWidth from calibration or computed from check data'
          ) +
          theme_classic() +
          theme(
            legend.position = "right",
            legend.box = "vertical",
            legend.justification = "left",
            legend.text = element_text(size = 6),
            legend.spacing.y = unit(0, "lines"),
            legend.key.size = unit(0.4, "cm"),
            plot.margin = margin(5, 5, 5, 5, "pt")
          )
        
        fOverWidth_scatter <- list(
          plot = p12,
          height = compute_auto_height(base_height = 7, n_items = n_distinct(fvpx_over_width_data$participant), per_row = 3, row_increase = 0.06)
        )
      }
    }
  }
  # plot 14: fOverWidth second/first ratio vs first
  p14 <- NULL
  p14_data <- NULL
  if ("raw_fVpx" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$raw_fVpx) > 0) {

    # Prepare data: get first and second fOverWidth for each participant
    fOverWidth_ratio_data <- distanceCalibrationResults$raw_fVpx %>%
      filter(is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      arrange(participant) %>%  # Ensure consistent ordering
      mutate(measurement_order = row_number()) %>%
      filter(measurement_order <= 2) %>%  # Only keep first two measurements
      ungroup()

    # Only proceed if we have participants with at least 2 measurements
    if (nrow(fOverWidth_ratio_data) > 0) {
      # Pivot to get first and second measurements side by side
      fOverWidth_ratio_wide <- fOverWidth_ratio_data %>%
        select(participant, fOverWidth, measurement_order) %>%
        pivot_wider(names_from = measurement_order, values_from = fOverWidth,
                   names_prefix = "fOverWidth_") %>%
        filter(!is.na(fOverWidth_1) & !is.na(fOverWidth_2)) %>%
        mutate(ratio_second_first = fOverWidth_2 / fOverWidth_1)

      if (nrow(fOverWidth_ratio_wide) > 0) {
        p14_data <- fOverWidth_ratio_wide
        
        # Calculate x-axis limits (same as original plot)
        x_min <- min(fOverWidth_ratio_wide$fOverWidth_1, na.rm = TRUE)
        x_max <- max(fOverWidth_ratio_wide$fOverWidth_1, na.rm = TRUE)
        x_range <- x_max - x_min
        x_padding <- x_range * 0.1  # 10% padding
        x_axis_min <- x_min - x_padding
        x_axis_max <- x_max + x_padding
        
        # Calculate y-axis limits for ratio (centered around 1.0)
        y_min <- min(fOverWidth_ratio_wide$ratio_second_first, na.rm = TRUE)
        y_max <- max(fOverWidth_ratio_wide$ratio_second_first, na.rm = TRUE)
        y_range <- max(abs(y_max - 1), abs(y_min - 1))
        y_axis_min <- 1 - y_range * 1.2
        y_axis_max <- 1 + y_range * 1.2
        
        p14 <- ggplot(fOverWidth_ratio_wide, 
                      aes(x = fOverWidth_1, 
                      y = ratio_second_first, 
                      color = participant)) +
          geom_point(size = 4, alpha = 0.8) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 1) +  # Reference line at ratio = 1
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                              label = statement, size = 3, family = "sans", fontface = "plain") +
          scale_color_manual(values = colorPalette) +
          scale_x_continuous(limits = c(x_axis_min, x_axis_max)) +
          scale_y_continuous(limits = c(y_axis_min, y_axis_max)) +
          labs(
            subtitle = 'Focal length calibration: second/first vs. first',
            x = "First fOverWidth calibration",
            y = "fOverWidth calibration: second/first"
          ) +
          guides(color = guide_legend(
            ncol = 4,
            title = "",
            override.aes = list(size = 2),
            keywidth = unit(0.3, "cm"),
            keyheight = unit(0.3, "cm")
          )) +
          theme_bw() +
          theme(
            legend.key.size = unit(0, "mm"),
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
            legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
            legend.box.spacing = unit(0, "lines"),
            legend.spacing.y = unit(-10, "lines"),
            legend.spacing.x = unit(0, "lines"),
            legend.key.height = unit(0, "lines"),
            legend.key.width = unit(0, "mm"),
            legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
            legend.margin = margin(0, 0, 0, 0),
            legend.position = "top",
            legend.box = "vertical",
            legend.justification = 'left',
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size = 10, angle = 0, hjust = 0, vjust = 1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
            plot.title.position = "plot",
            plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
            plot.caption = element_text(size = 10),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
            strip.text = element_text(size = 14)
          )
      }
    }
  }

  # Calculate heights based on legend complexity
  n_participants <- n_distinct(distance_individual$participant)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.06)

  # Eye feet position plot has complex legend too
  eye_feet_height <- if (!is.null(p4_result)) p4_result$height else 4

  return(list(
    credit_card_vs_requested = list(plot = p1, height = plot_height),
    credit_card_fraction = list(plot = p2, height = plot_height),
    eye_feet_position = list(plot = p4, height = eye_feet_height),
    foot_position_calibration = list(plot = p4b, height = if (!is.null(p4b_height)) p4b_height else plot_height),
    calibration_over_check_vs_check = list(plot = p5b, height = if (!is.null(p5b)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    calibrated_over_mean_vs_spot = list(plot = p6, height = if (!is.null(p6)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    calibrated_over_median_hist = list(
      plot = p7,
      height = if (!is.null(p7)) {
        compute_auto_height(base_height = 1.5, n_items = n_distinct(ratio_data$participant), per_row = 3, row_increase = 0.05) +
          0.24 * max(max_count, 8)
      } else NULL
    ),
    raw_pxPerCm_hist = list(
      plot = p8,
      height = if (!is.null(p8)) {
        compute_auto_height(base_height = 1.5, n_items = n_distinct(raw_pxPerCm_data$participant), per_row = 3, row_increase = 0.05) +
          0.24 * max(p8_max_count, 8)
      } else NULL
    ),
    raw_objectMeasuredCm_hist = list(
      plot = p9,
      height = if (!is.null(p9)) {
        compute_auto_height(base_height = 1.5, n_items = n_distinct(raw_objectCm_data$participant), per_row = 3, row_increase = 0.05) +
          0.24 * max(p9_max_count, 8)
      } else NULL
    ),
    raw_fOverWidth_hist = list(
      plot = p10,
      height = if (!is.null(p10)) {
        compute_auto_height(base_height = 1.5, n_items = n_distinct(raw_factor_data$participant), per_row = 3, row_increase = 0.05) +
          0.24 * max(p10_max_count, 8)
      } else NULL
    ),
    fOverWidth_ratio_vs_first = list(
      plot = p14,
      height = if (!is.null(p14) && !is.null(p14_data)) {
        compute_auto_height(base_height = 7, n_items = n_distinct(p14_data$participant), per_row = 3, row_increase = 0.06)
      } else NULL
    ),
    fOverWidth_hist = fvpx_over_width_hist,
    fOverWidth_scatter = fvpx_over_width_scatter
  ))
}

plot_sizeCheck <- function(distanceCalibrationResults, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  sizeCheck <- distanceCalibrationResults$sizeCheck
  statement <- distanceCalibrationResults$statement
  raw_pxPerCm <- distanceCalibrationResults$raw_pxPerCm %>%
   select(participant, pxPerCm, requestedCm)
  # Check if the data is empty
  if (nrow(sizeCheck) == 0) {
    return(NULL)
  }
  
  ruler <-  sizeCheck %>%
    distinct(participant, rulerLength, rulerUnit) %>%
    filter(!is.na(rulerLength)) %>% 
    mutate(lengthCm = ifelse(rulerUnit == 'cm', rulerLength, rulerLength * 2.54))
  
  # Check for NA values after conversion
  if (sum(is.na(sizeCheck$SizeCheckEstimatedPxPerCm)) == nrow(sizeCheck) ||
      sum(is.na(sizeCheck$SizeCheckRequestedCm)) == nrow(sizeCheck)) {
    return(NULL)
  }
  
  if (nrow(sizeCheck) == 0) {return(NULL)}
  
  # Average Estimated PxPerCm per Participant per Requested Size
  sizeCheck_avg <- sizeCheck %>%
    group_by(participant, SizeCheckRequestedCm) %>%
    summarize(
      avg_estimated = 10^median(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add logarithmic horizontal jitter to x-axis variable (unbiased for log scales)
      SizeCheckRequestedCm_jitter = add_log_jitter(SizeCheckRequestedCm, jitter_percent = 5, seed = 42)
    )
  
  # Determine scale limits
  min_val <- min(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated), na.rm = TRUE)
  max_val <- max(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated), na.rm = TRUE)
  
  # Compute sdLogDensity for each participant
  sdLogDensity_data <- sizeCheck %>%
    group_by(participant, pxPerCm) %>%
    summarize(
      avg_estimated=10^median(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      sdLogDensity = sd(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
      filter(!is.na(sdLogDensity)) %>% 
  mutate(ratio = pxPerCm / avg_estimated)
  
  
  if (nrow(sdLogDensity_data) > 0) {
    # Set bin width
    bin_width <- 0.003
    
    # Create bins that don't cross zero
    sdLogDensity_data <- sdLogDensity_data %>%
      mutate(
        # Assign each participant to a bin that doesn't cross zero
        bin_center = ifelse(sdLogDensity >= 0,
                           floor(sdLogDensity / bin_width) * bin_width + bin_width/2,
                           ceiling(sdLogDensity / bin_width) * bin_width - bin_width/2)
      ) %>%
      arrange(bin_center, participant) %>%
      group_by(bin_center) %>%
      mutate(
        stack_position = row_number(),
        dot_y = stack_position
      ) %>%
      ungroup()
    
    data_range <- range(sdLogDensity_data$sdLogDensity)
    
    # Calculate reasonable x-axis maximum (data max + some padding)
    x_max <- max(sdLogDensity_data$bin_center, calibrateTrackDistanceCheckLengthSDLogAllowed) + bin_width
    
    # Create the histogram plot with stacked dots
    # Calculate legend rows and dynamic sizing (now using 3 columns)
    n_participants <- n_distinct(sdLogDensity_data$participant)
    legend_rows <- ceiling(n_participants / 3)  # Changed to 3 columns
    
    # Calculate appropriate y-axis limit (remove empty space above)
    max_count <- max(sdLogDensity_data$dot_y)
    
    # Calculate x-axis limits
    x_min <- 10^(-3)
    
    h1 <- ggplot(sdLogDensity_data, aes(x = sdLogDensity)) +
      # Add transparent light-green bar for allowed range (from x=0/y-axis to threshold)
      annotate("rect", 
               xmin = 0, 
               xmax = calibrateTrackDistanceCheckLengthSDLogAllowed, 
               ymin = 0, 
               ymax = Inf, 
               fill = "lightgreen", 
               alpha = 0.3) +
      # Stacked colored points (histogram style with discrete stacks)
      geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") + 
      scale_color_manual(values= colorPalette) +
      scale_y_continuous(limits = c(0, max(8, max_count + 1)),
                         expand = expansion(mult = c(0, 0.1)), 
                         breaks = function(x) seq(0, ceiling(max(x)), by = 1)) + 
      scale_x_log10(limits = c(x_min, x_max),breaks = scales::log_breaks(n=8)) +
      annotation_logticks(sides = "b") +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(sdLogDensity_data$participant)),
                          size = 3, family = "sans", fontface = "plain") + 
      guides(color = guide_legend(
        ncol = 4,  
        title = "",
        override.aes = list(size = 2),  # Increase legend dot size
        keywidth = unit(0.3, "cm"),     # Add some width for the dots
        keyheight = unit(0.3, "cm")     # Add some height for the dots
      )) +
      labs(
        subtitle = "Histogram of SD of log10 pixel density",
        x = "SD of log10 pixel density",
        y = "Count"
      ) +
      theme_bw() +
      theme(legend.key.size = unit(0, "mm"),
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
            legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
            legend.box.spacing = unit(0, "lines"),
            legend.spacing.y = unit(-10, "lines"),
            legend.spacing.x = unit(0, "lines"),
            legend.key.height = unit(0, "lines"),
            legend.key.width = unit(0, "mm"),
            legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
            legend.margin = margin(0, 0, 0, 0),
            legend.position = "top", 
            legend.box = "vertical", 
            legend.justification = 'left',
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
            axis.text.y = element_text(size = 10),
            plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
            plot.title.position = "plot",
            plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
            plot.caption = element_text(size = 10),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
            strip.text = element_text(size = 14))
    
  } else {
    h1 <- NULL
  }
  if (nrow(ruler) > 0) {
    maxX = 1.05 * max(ruler$lengthCm) 
    minX = 0.95 * min(ruler$lengthCm)
    # Create dot plot style like SD histogram
    bin_width <- (max(ruler$lengthCm) - min(ruler$lengthCm)) / 20  # Adjust bin width based on data range
    
    # Handle case where all rulers have same length (bin_width = 0)
    if (bin_width == 0) {
      bin_width <- 1  # Use a small default bin width
    }
    
    ruler_dotplot <- ruler %>%
      mutate(
        # Create bins for stacking
        bin_center = round(lengthCm / bin_width) * bin_width
      ) %>%
      arrange(bin_center, participant) %>%
      group_by(bin_center) %>%
      mutate(
        stack_position = row_number(),
        dot_y = stack_position
      ) %>%
      ungroup()
    
    max_count <- max(ruler_dotplot$dot_y)
    n_participants <- n_distinct(ruler_dotplot$participant)
    
    h2 <- ggplot(ruler_dotplot, aes(x = lengthCm)) +
      # Stacked colored points (dot plot style)
      geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") + 
      scale_color_manual(values = colorPalette) +
    scale_y_continuous(limits = c(0, max(8, max_count + 1)), expand = expansion(mult = c(0, 0.1)), breaks = function(x) seq(0, ceiling(max(x)), by = 2)) + 
      scale_x_log10(limits=c(minX, maxX),
                    breaks = scales::log_breaks(n=8)) +
      annotation_logticks(sides = "b", 
                          size = 0.3, 
                          alpha = 0.7, 
                          short = unit(0.1, "cm"),
                          mid = unit(0.15, "cm"), 
                          long = unit(0.2, "cm")) +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(ruler_dotplot$participant)),
                          size = 3, family = "sans", fontface = "plain") + 
      guides(color = guide_legend(
        ncol = 4,  
        title = "",
        override.aes = list(size = 2),
        keywidth = unit(0.3, "cm"),
        keyheight = unit(0.3, "cm")
      )) +
      labs(subtitle = 'Histogram of ruler length (cm)',
           x = "Ruler length (cm)",
           y = "Count") +
      theme_bw() +
      theme(legend.key.size = unit(0, "mm"),
            legend.title = element_text(size = 6),
            legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
            legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
            legend.box.spacing = unit(0, "lines"),
            legend.spacing.y = unit(-10, "lines"),
            legend.spacing.x = unit(0, "lines"),
            legend.key.height = unit(0, "lines"),
            legend.key.width = unit(0, "mm"),
            legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
            legend.margin = margin(0, 0, 0, 0),
            legend.position = "top", 
            legend.box = "vertical", 
            legend.justification = 'left',
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
            axis.text.y = element_text(size = 10),
                  plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
            plot.title.position = "plot",
            plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
            plot.caption = element_text(size = 10),
            plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
            strip.text = element_text(size = 14))
  } else {
    h2 = NULL
  }
  # Prepare calibration data with actual requested sizes
  calibration_data <- raw_pxPerCm %>%
    rename(avg_estimated = pxPerCm) %>%
    mutate(
      SizeCheckRequestedCm_jitter = add_log_jitter(requestedCm, jitter_percent = 5, seed = 43),
      type = "calibration"
    ) %>%
    select(participant, avg_estimated, SizeCheckRequestedCm_jitter, type)
  
  # Combine check and calibration data
  pixelDensity_data <- rbind(
    sizeCheck_avg %>% select(participant, avg_estimated, SizeCheckRequestedCm_jitter) %>% mutate(type = "check"),
    calibration_data
  )
  
  ymin = max(5,floor(min(pixelDensity_data$avg_estimated) / 10 - 1) * 10)
  ymax = ceiling(max(pixelDensity_data$avg_estimated) / 10 + 1) * 10

  p1 <- ggplot(data=pixelDensity_data) + 
    # Solid lines for calibration points
    geom_line(data = pixelDensity_data %>% filter(type == "calibration"),
              aes(x = SizeCheckRequestedCm_jitter, 
                  y = avg_estimated,
                  color = participant,
                  group = participant), 
              linetype = "solid",
              alpha = 0.7) +
    # Dashed lines for check points  
    geom_line(data = pixelDensity_data %>% filter(type == "check"),
              aes(x = SizeCheckRequestedCm_jitter, 
                  y = avg_estimated,
                  color = participant,
                  group = participant), 
              linetype = "dashed",
              alpha = 0.7) +
    geom_point(aes(x = SizeCheckRequestedCm_jitter, 
                   y = avg_estimated,
                   color = participant,
                   shape = type), 
               size = 2) + 
     scale_shape_manual(name = "", values = c(calibration = 16, check = 1),
                       labels = c(calibration = "calibration", check = "check")) +
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(sizeCheck_avg$participant)),
                        size = 3, family = "sans", fontface = "plain") + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement,
                        size = 3, family = "sans", fontface = "plain") + 
    scale_x_log10(
      breaks = scales::log_breaks(n = 8),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_y_log10(breaks = scales::log_breaks(n=8),
                  limits = c(ymin,ymax)) +
    annotation_logticks(size = 0.3, 
                        alpha = 0.7, 
                        short = unit(0.1, "cm"), 
                        mid = unit(0.15, "cm"), 
                        long = unit(0.2, "cm")) + 
    scale_color_manual(values= colorPalette) + 
    guides(color = guide_legend(
      ncol = 4,  
      title = "",
      override.aes = list(size = 2),  
      keywidth = unit(1.2, "lines"),  
      keyheight = unit(0.8, "lines")  
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    labs(subtitle = 'Pixel density vs. requested length',
         x = 'Requested length (cm)',
         y = 'Pixel density (px/cm)')
  
  # smallest positive x so the rect shows up on a log scale
  # Compute positive finite bounds for a log–log plot
  # --- pick panel limits explicitly ---
  min_pos_x <- min(sdLogDensity_data$sdLogDensity[sdLogDensity_data$sdLogDensity > 0], na.rm = TRUE)
  # left edge at the decade just below/at the smallest x (e.g., 0.001 if min≈0.00117)
  x_left <- if (is.finite(min_pos_x)) 10^floor(log10(min_pos_x)) else 1e-6
  
  rat_pos <- sdLogDensity_data$ratio[sdLogDensity_data$ratio > 0]
  y_low  <- min(rat_pos, na.rm = TRUE)
  y_high <- max(rat_pos, na.rm = TRUE)
  
  rat_pos <- sdLogDensity_data$ratio[sdLogDensity_data$ratio > 0]
  y_low  <- min(rat_pos, na.rm = TRUE)
  y_high <- max(rat_pos, na.rm = TRUE)
  pad_mult <- 1.08                       # ~8% headroom on both sides
  y_min_panel <- max(y_low / pad_mult, 1e-6)
  y_max_panel <- y_high * pad_mult
  
  # keep your x_left/xmax_band from earlier...
  p2 <- ggplot(sdLogDensity_data) +
    annotate("rect", 
             xmin = x_left, 
             xmax = calibrateTrackDistanceCheckLengthSDLogAllowed, 
             ymin = y_min_panel, 
             ymax = y_max_panel, 
             fill = "lightgreen", 
             alpha = 0.3) +
    geom_point(aes(x = sdLogDensity, y = ratio, color = participant), size = 2) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0("N=", dplyr::n_distinct(sdLogDensity_data$participant)),
                        size = 3, family = "sans", fontface = "plain") +
    ggpp::geom_text_npc(aes(npcx="right", npcy="bottom"), label = statement,
                        size = 3, family = "sans", fontface = "plain") +
    scale_color_manual(values = colorPalette) +
    scale_x_log10(limits = c(x_left, NA),
                  expand = expansion(mult = c(0, 0.05)),
                  breaks = scales::log_breaks(n=8),
                  labels = scales::label_number(scale = 1)) +
    scale_y_log10(limits = c(y_min_panel, y_max_panel),
                  expand = c(0, 0),
                  breaks = scales::log_breaks(n=8)) +
    annotation_logticks() +
    guides(
      color = guide_legend(ncol = 4, title = "",
                           override.aes = list(size = 2),
                           keywidth = grid::unit(1.2, "lines"),
                           keyheight = grid::unit(0.8, "lines")),
      linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))
    ) +
    labs(
      subtitle = "Credit card pixel density re remeasured vs.\nSD log remeasured pixel density",
      x = "SD of log10 pixel density",
      y = "Credit card pixel density re remeasured"
    )
  
  
  
  # Calculate heights based on legend complexity
  n_participants <- if (nrow(sizeCheck_avg) > 0) n_distinct(sizeCheck_avg$participant) else 0
  n_participants_hist <- if (nrow(sdLogDensity_data) > 0) n_distinct(sdLogDensity_data$participant) else 0
  n_participants_ruler <- if (nrow(ruler) > 0) n_distinct(ruler$participant) else 0

  # Base height calculation: more participants = more legend space (in inches)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = max(n_participants, n_participants_hist, n_participants_ruler), per_row = 3, row_increase = 0.06)

  # Histograms have top legends; reduce overall height to half
  hist_base <- 1.5
  # Compute extra height separately for each dotted histogram
  h1_max_stack <- if (nrow(sdLogDensity_data) > 0) max(sdLogDensity_data$dot_y, na.rm = TRUE) else 0
  h2_max_stack <- if (exists("ruler_dotplot")) max(ruler_dotplot$dot_y, na.rm = TRUE) else 0
  h1_stack_for_height <- max(h1_max_stack, 8)
  h2_stack_for_height <- max(h2_max_stack, 8)
  h1_height <- compute_auto_height(base_height = hist_base, n_items = n_participants_hist, per_row = 3, row_increase = 0.05) + 0.24 * h1_stack_for_height
  h2_height <- compute_auto_height(base_height = hist_base, n_items = n_participants_ruler, per_row = 3, row_increase = 0.05) + 0.24 * h2_stack_for_height

  return(list(
    density_vs_length = list(plot = p1, height = plot_height),
    density_ratio_vs_sd = list(plot = p2, height = plot_height),
    sd_hist = list(plot = h1, height = h1_height),
    ruler_hist = list(plot = h2, height = h2_height)
  ))
}

plot_distance_production <- function(distanceCalibrationResults, participant_info, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  distance <- distanceCalibrationResults$checkJSON
  statement <- distanceCalibrationResults$statement
  if (nrow(distance) == 0) {
    print('no distance data')
    return(NULL)
    }
  
  # Average Measured Distance per Participant per Requested Distance
  distance_avg <- distance %>%
    group_by(participant, requestedEyesToPointCm) %>%
    summarize(
      checkDistanceMeasuredCm = mean(measuredEyesToPointCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add logarithmic horizontal jitter to x-axis variable
      checkDistanceRequestedCm_jitter = add_log_jitter(requestedEyesToPointCm, jitter_percent = 2, seed = 42),
      checkDistanceRequestedCm = requestedEyesToPointCm,
      # Calculate production ratio: measured / requested distance
      production_fraction = checkDistanceMeasuredCm / requestedEyesToPointCm
    ) %>%
      filter(is.finite(production_fraction))
 
  # Plot: Error vs. blindspot diameter
  blindspot_data <- distanceCalibrationResults$blindspot
  p6 <- NULL
  if (nrow(distance_avg) > 0 && nrow(blindspot_data) > 0) {
    # Join distance_avg with blindspot_data
    error_vs_blindspot_data <- distance_avg %>%
      left_join(blindspot_data, by = "participant") %>%
      filter(!is.na(`_calibrateTrackDistanceBlindspotDiameterDeg`), 
             !is.na(production_fraction)) %>%
      rename(spotDeg = `_calibrateTrackDistanceBlindspotDiameterDeg`) %>%
      mutate(spotDeg_jitter = add_log_jitter(spotDeg, jitter_percent = 0.5, seed = 42))

    if (nrow(error_vs_blindspot_data) > 0) {
      # Calculate scale limits
      x_min <- max(0.1, min(error_vs_blindspot_data$spotDeg) * 0.8)
      x_max <- max(error_vs_blindspot_data$spotDeg) * 1.2
      y_min <- max(0.1, min(error_vs_blindspot_data$production_fraction) * 0.8)
      y_max <- min(2.0, max(error_vs_blindspot_data$production_fraction) * 1.2)

      p6 <- ggplot(error_vs_blindspot_data, aes(x = spotDeg_jitter, y = production_fraction)) +
        geom_point(aes(color = participant), size = 3, alpha = 0.8) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
        ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                            label = paste0('N=', n_distinct(error_vs_blindspot_data$participant))) +
        scale_x_log10(limits = c(x_min, x_max), breaks = scales::log_breaks(n=8)) +
        scale_y_log10(limits = c(y_min, y_max), breaks = seq(0.5, 2.0, by = 0.1)) +
        annotation_logticks() +
        scale_color_manual(values = colorPalette) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
        guides(color = guide_legend(ncol = 4, title = "",
                                    override.aes = list(size = 2),
                                    keywidth = unit(1.2, "lines"),
                                    keyheight = unit(0.8, "lines"))) +
        labs(subtitle = 'Check distance error vs. blindspot diameter',
             x = 'Blindspot diameter (deg)',
             y = 'Check measured / requested distance',
             caption = 'Red dashed line shows perfect accuracy (ratio = 1.0)')
    }
  }

  # Calculate heights based on legend complexity
  n_participants <- if (nrow(distance_avg) > 0) n_distinct(distance_avg$participant) else 0
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.06)

  return(list(
    error_vs_blindspot_diameter = list(
      plot = p6,
      height = if (!is.null(p6)) compute_auto_height(base_height = 7,
                                                     n_items = n_distinct(error_vs_blindspot_data$participant),
                                                     per_row = 3, row_increase = 0.06) else NULL
    )
  ))
}

objectCm_hist <- function(participant_info, distanceCalibrationResults) {
  dt <- participant_info %>%
    mutate(
      # Clean the objectLengthCm strings before converting to numeric
      objectLengthCm_clean = trimws(gsub('["\']', '', objectLengthCm)),
      objectLengthCm = as.numeric(objectLengthCm_clean)
    ) %>%
    filter(!is.na(objectLengthCm)) 
  if (nrow(dt) == 0) return(NULL)
  
  # Build statement consistent with other plots (reuse global if available)
  statement <- distanceCalibrationResults$statement
  
  # Create dot plot style like SD histogram
  bin_width <- (max(dt$objectLengthCm) - min(dt$objectLengthCm)) / 20  # Adjust bin width based on data range
  
  # Handle case where all objects have same length (bin_width = 0)
  if (bin_width == 0) {
    bin_width <- 1  # Use a small default bin width
  }
  
  object_dotplot <- dt %>%
    mutate(
      # Create bins for stacking
      bin_center = round(objectLengthCm / bin_width) * bin_width
    ) %>%
    arrange(bin_center, PavloviaParticipantID) %>%
    group_by(bin_center) %>%
    mutate(
      stack_position = row_number(),
      dot_y = stack_position
    ) %>%
    ungroup()
  
  max_count <- max(object_dotplot$dot_y)
  n_participants <- n_distinct(object_dotplot$PavloviaParticipantID)
  
  p <- ggplot(object_dotplot, aes(x = objectLengthCm)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = PavloviaParticipantID), size = 6, alpha = 0.85) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") + 
    scale_color_manual(values = colorPalette) +
    scale_y_continuous(limits = c(0, max(8, max_count + 1)), 
                       expand = expansion(mult = c(0, 0.1)), 
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) + 
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                        label = paste0('N=', n_distinct(object_dotplot$PavloviaParticipantID))) + 
    guides(color = guide_legend(
      ncol = 4,  
      title = "",
      override.aes = list(size = 2),
      keywidth = unit(0.3, "cm"),
      keyheight = unit(0.3, "cm")
    )) +
    labs(subtitle = 'Histogram of object length (cm)',
         x = "Object length (cm)",
         y = "Count") +
    theme_bw() +
    theme(legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top", 
          legend.box = "vertical", 
          legend.justification = 'left',
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14))

  # Calculate height based on legend complexity
  n_participants <- n_distinct(object_dotplot$PavloviaParticipantID)
  # Use inches for height (consistent with other plots)
  base_height <- 1.5
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.05) + 0.24 * max(max_count, 8)

  return(list(plot = p, height = plot_height))
}

bs_vd_hist <- function(data_list) {
  # get blindspot viewing distance data
  dt <- get_bs_vd(data_list)
  if (nrow(dt) == 0) return(list(mean_plot = NULL, sd_plot = NULL))

  # Plot 1: MEAN of left and right viewing distances measured in blindspot-based calibration
  bin_width_mean <- (max(dt$m) - min(dt$m)) / 20  # Adjust bin width based on data range

  # Handle case where all values are the same (bin_width = 0)
  if (bin_width_mean == 0) {
    bin_width_mean <- max(dt$m) * 0.01  # Use 1% of value as bin width
  }

  mean_dotplot <- dt %>%
    mutate(
      # Create bins for stacking
      bin_center = round(m / bin_width_mean) * bin_width_mean
    ) %>%
    arrange(bin_center, participant) %>%
    group_by(bin_center) %>%
    mutate(
      stack_position = row_number(),
      dot_y = stack_position
    ) %>%
    ungroup()

  max_count_mean <- max(mean_dotplot$dot_y)
  n_participants_mean <- n_distinct(mean_dotplot$participant)
  p1 <- ggplot(mean_dotplot, aes(x = m)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
  scale_y_continuous(limits = c(0, max(8, max_count_mean + 1)),
                       expand = expansion(mult = c(0, 0.1)),
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0('N=', n_participants_mean)) +
    guides(color = guide_legend(
      ncol = 4,
      title = "",
      override.aes = list(size = 2),
      keywidth = unit(0.3, "cm"),
      keyheight = unit(0.3, "cm")
    )) +
    labs(subtitle = 'Mean of left and right viewing distances measured in \nblindspot-based calibration',
         x = "Mean viewing distance (cm)",
         y = "Count") +
    theme_bw() +
    theme(legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14))

  # Plot 2: SD of log10 of left and right viewing distances measured in blindspot-based calibration
  bin_width_sd <- (max(dt$sd) - min(dt$sd)) / 20  # Adjust bin width based on data range

  # Handle case where all values are the same (bin_width = 0)
  if (bin_width_sd == 0) {
    bin_width_sd <- max(dt$sd) * 0.01  # Use 1% of value as bin width
  }

  sd_dotplot <- dt %>%
    mutate(
      # Create bins for stacking
      bin_center = round(sd / bin_width_sd) * bin_width_sd
    ) %>%
    arrange(bin_center, participant) %>%
    group_by(bin_center) %>%
    mutate(
      stack_position = row_number(),
      dot_y = stack_position
    ) %>%
    ungroup()

  max_count_sd <- max(sd_dotplot$dot_y)
  n_participants_sd <- n_distinct(sd_dotplot$participant)

  p2 <- ggplot(sd_dotplot, aes(x = sd)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
  scale_y_continuous(limits = c(0, max(8, max_count_sd + 1)),
                       expand = expansion(mult = c(0, 0.1)),
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0('N=', n_participants_sd)) +
    guides(color = guide_legend(
      ncol = 4,
      title = "",
      override.aes = list(size = 2),
      keywidth = unit(0.3, "cm"),
      keyheight = unit(0.3, "cm")
    )) +
    labs(subtitle = 'SD of log10 of left and right viewing distances measured in \nblindspot-based calibration',
         x = "SD of log10 viewing distance",
         y = "Count") +
    theme_bw() +
    theme(legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(l = -0.6, r = 0, t = 0, b = 0, "cm"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(-10, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0, "lines"),
          legend.key.width = unit(0, "mm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size  = 10, angle = 0, hjust=0, vjust=1),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 0.1, r = 0.1, b = 0.1, l = 0.1, "inch"),
          strip.text = element_text(size = 14))

  # Calculate heights based on legend complexity
  n_participants_mean <- if (nrow(mean_dotplot) > 0) n_distinct(mean_dotplot$participant) else 0
  n_participants_sd <- if (nrow(sd_dotplot) > 0) n_distinct(sd_dotplot$participant) else 0

  base_height <- 1.5
  plot_height <- compute_auto_height(base_height = base_height, n_items = max(n_participants_mean, n_participants_sd), per_row = 3, row_increase = 0.05) + 
    0.24 * max(max_count_mean, max_count_sd, 8)

  return(list(
    mean_plot = list(plot = p1, height = plot_height),
    sd_plot = list(plot = p2, height = plot_height)
  ))
}

plot_ipd_vs_eyeToFootCm <- function(distanceCalibrationResults) {
 
  # Use "calibration" instead of "TJSON" for legend
  # For calibration data, requestedEyesToFootCm = eyeToFootCm (calibration establishes the baseline)
  camera <- distanceCalibrationResults$camera
  
  # Early return if camera doesn't have required columns
  if (nrow(camera) == 0 || !"PavloviaParticipantID" %in% names(camera) || !"widthVpx" %in% names(camera)) {
    return(list(ipd_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL),
                ipdVpx_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL)))
  }
  
  # Early return if TJSON is empty or missing required columns
  tjson <- distanceCalibrationResults$TJSON
  if (nrow(tjson) == 0 || !"ipdOverWidth" %in% names(tjson)) {
    return(list(ipd_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL),
                ipdVpx_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL)))
  }
  
  # TJSON now has ipdOverWidth directly (no longer computed from ipdVpx)
  ipd_TJSON <- tjson %>%
    left_join(camera %>% select(PavloviaParticipantID, widthVpx), by = c("participant" = "PavloviaParticipantID")) %>%
    filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0) %>%
    mutate(requestedEyesToFootCm = eyeToFootCm,  # During calibration, requested == measured
           ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = ipdOverWidth * requestedEyesToFootCm / ipdCm,
           # Compute factorVpxCm from fOverWidth if not available
           factorVpxCm = ifelse(is.na(factorVpxCm) & !is.na(fOverWidth) & !is.na(widthVpx) & !is.na(ipdCm),
                                fOverWidth * widthVpx * ipdCm, factorVpxCm),
           type = 'calibration') %>%
    select(participant, requestedEyesToFootCm, ipdOverWidth, ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm, factorVpxCm, type)
  
  # For check data, use the actual requestedEyesToFootCm (derived from requestedEyesToPointCm)
  # checkJSON now has ipdOverWidth directly
  checkjson <- distanceCalibrationResults$checkJSON
  ipd_checkJSON <- if (nrow(checkjson) > 0 && "ipdOverWidth" %in% names(checkjson)) {
    checkjson %>%
      filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0) %>%
      mutate(ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = ipdOverWidth * requestedEyesToFootCm / ipdCm,
             # Compute factorVpxCm for compatibility (fOverWidth * widthVpx * ipdCm) - need widthVpx
             type = 'check') %>%
      left_join(camera %>% select(PavloviaParticipantID, widthVpx), by = c("participant" = "PavloviaParticipantID")) %>%
      mutate(factorVpxCm = fOverWidth * widthVpx * ipdCm) %>%
      select(participant, requestedEyesToFootCm, ipdOverWidth, ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm, factorVpxCm, type)
  } else {
    tibble(participant = character(), requestedEyesToFootCm = numeric(), ipdOverWidth = numeric(),
           ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = numeric(), factorVpxCm = numeric(), type = character())
  }
  
  ipd_data <- rbind(ipd_TJSON, ipd_checkJSON)
  
  if (nrow(ipd_TJSON) == 0) {
    return(list(ipd_vs_requestedEyesToFootCm = list(plot = NULL, 
                                          height = NULL),
                ipdVpx_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, 
                                                               height = NULL)))
  }
  
  # Get calibration focal length (factorVpxCm) and ipdCm per participant for the focal length line
  # Use median factorVpxCm and ipdCm from calibration data per participant
  # Need to get ipdCm from original TJSON data (before selection)
  ipdCm_data <- distanceCalibrationResults$TJSON %>%
    group_by(participant) %>%
    summarize(median_ipdCm = median(ipdCm, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(median_ipdCm), is.finite(median_ipdCm), median_ipdCm > 0)
  
  focal_length_data <- ipd_TJSON %>%
    group_by(participant) %>%
    summarize(focal_length = median(factorVpxCm, na.rm = TRUE), .groups = "drop") %>%
    filter(!is.na(focal_length), is.finite(focal_length)) %>%
    left_join(camera %>% select(PavloviaParticipantID, widthVpx), by = c("participant" = "PavloviaParticipantID")) %>%
    left_join(ipdCm_data, by = "participant") %>%
    mutate(focal_length_over_width = focal_length / widthVpx)
  
  # Create focal length curve data for Plot 2 (ipdVpx vs requestedEyesToFootCm)
  # ipdVpx = factorVpxCm / requestedEyesToFootCm (thin lens formula)
  # Use fewer points (20 instead of 100) to reduce memory - curves are smooth anyway
  x_range <- range(ipd_data$requestedEyesToFootCm, na.rm = TRUE)
  focal_curve_data <- focal_length_data %>%
    crossing(requestedEyesToFootCm = seq(x_range[1] * 0.9, x_range[2] * 1.1, length.out = 20)) %>%
    mutate(ipdOverWidth_focal = focal_length_over_width / requestedEyesToFootCm)
  
  # Create focal length horizontal line data for Plot 1 (ipdVpx*requestedEyesToFootCm/ipdCm vs requestedEyesToFootCm)
  # ipdVpx * requestedEyesToFootCm / ipdCm = factorVpxCm / ipdCm (constant)
  focal_hline_data <- focal_length_data %>%
    filter(!is.na(median_ipdCm), is.finite(median_ipdCm), median_ipdCm > 0) %>%
    crossing(requestedEyesToFootCm = x_range) %>%
    mutate(product_focal_over_width = focal_length_over_width / median_ipdCm)
  
  # Plot 2: ipdVpx vs. requestedEyesToFootCm
  p1 <- ggplot() +
    # Data lines: solid for both calibration and check
    geom_line(data = ipd_data %>% arrange(participant, type, requestedEyesToFootCm),
                  aes(x = requestedEyesToFootCm,
                      y = ipdOverWidth,
                  color = participant,
                  linetype = type,
                  group = interaction(participant, type)),
              linewidth = 0.75, alpha = 0.8) +
    # Focal length curve (dotted) - one per participant
    geom_line(data = focal_curve_data,
              aes(x = requestedEyesToFootCm,
                  y = ipdOverWidth_focal,
                      color = participant,
                      group = participant),
              linewidth = 0.75, linetype = "dotted", alpha = 0.8) +
    # Points with shapes for calibration vs check
    geom_point(data = ipd_data,
               aes(x = requestedEyesToFootCm,
                      y = ipdOverWidth,
                   color = participant,
                   shape = type),
               size = 2) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                        label = paste0('N=', n_distinct(ipd_data$participant))) +
    scale_x_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 10)
    ) +
    scale_y_log10(breaks = scales::log_breaks(n = 8)) +
    # Shape: filled circle for calibration, open circle for check
    scale_shape_manual(name = "", values = c(calibration = 16, check = 1),
                       labels = c(calibration = "calibration", check = "check")) +
    # Linetype: solid for both calibration and check
    scale_linetype_manual(name = "", values = c(calibration = "solid", check = "solid"),
                          labels = c(calibration = "calibration", check = "check")) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5),
                           keywidth = unit(0.8, "lines"), keyheight = unit(0.6, "lines")),
      shape = guide_legend(title = "", override.aes = list(size = 2)),
      linetype = guide_legend(title = "", override.aes = list(linewidth = 0.75))
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(subtitle = 'ipdOverWidth vs. requestedEyesToFootCm',
         x = 'requestedEyesToFootCm',
         y = 'ipdOverWidth',
         caption = 'Dotted lines: focal length from calibration (ipdOverWidth = factorVpxCm / (widthVpx × requestedEyesToFootCm))')
  
  # Plot 1: (ipdVpx*requestedEyesToFootCm/ipdCm) vs. requestedEyesToFootCm
  p2 <- ggplot() +
    # Data lines: solid for both calibration and check
    geom_line(data = ipd_data %>% arrange(participant, type, requestedEyesToFootCm),
              aes(x = requestedEyesToFootCm,
                  y = ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm,
                  color = participant,
                  linetype = type,
                  group = interaction(participant, type)),
              linewidth = 0.75, alpha = 0.8) +
    # Focal length horizontal line (dotted) - one per participant at y = factorVpxCm
    geom_line(data = focal_hline_data,
              aes(x = requestedEyesToFootCm,
                  y = product_focal_over_width,
                      color = participant,
                      group = participant),
              linewidth = 0.75, linetype = "dotted", alpha = 0.8) +
    # Points with shapes for calibration vs check
    geom_point(data = ipd_data,
                   aes(x = requestedEyesToFootCm,
                   y = ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm,
                   color = participant,
                   shape = type),
                   size = 2) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(ipd_data$participant))) +
        scale_x_log10(
          breaks = scales::log_breaks(n = 6),
          labels = scales::label_number(accuracy = 1)
        ) +
    scale_y_log10(breaks = scales::log_breaks(n = 8)) +
    # Shape: filled circle for calibration, open circle for check
    scale_shape_manual(name = "", values = c(calibration = 16, check = 1),
                       labels = c(calibration = "calibration", check = "check")) +
    # Linetype: solid for both calibration and check
    scale_linetype_manual(name = "", values = c(calibration = "solid", check = "solid"),
                          labels = c(calibration = "calibration", check = "check")) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5),
                           keywidth = unit(0.8, "lines"), keyheight = unit(0.6, "lines")),
      shape = guide_legend(title = "", override.aes = list(size = 2)),
      linetype = guide_legend(title = "", override.aes = list(linewidth = 0.75))
    ) +
        theme_classic() +
        theme(
          legend.position = "top",
          legend.box = "vertical",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
    labs(subtitle = 'fOverWidth vs. requestedEyesToFootCm',
         x = 'requestedEyesToFootCm',
         y = 'fOverWidth',
         caption = 'Dotted lines: focal length from calibration (ipdOverWidth * requestedEyesToFootCm / ipdCm = factorVpxCm/ipdCm)')
  
  p_height <- compute_auto_height(base_height = 7, n_items = n_distinct(ipd_data$participant), per_row = 3, row_increase = 0.06)
  return(list(ipdOverWidth_vs_requestedEyesToFootCm = list(plot = p1, 
                                        height = p_height),
              ipdOverWidth_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = p2, 
                                                             height = p_height)))
}


# =============================================================================
# NEW PLOTS: Distance geometry analysis
# =============================================================================

# Plot 2: imageBasedEyesToPointCm vs. rulerBasedEyesToFootCm  
# Shows measured line-of-sight distance as function of ruler-based horizontal distance
plot_eyeToPointCm_vs_requestedEyesToFootCm <- function(distanceCalibrationResults) {
  
  check_data <- distanceCalibrationResults$checkJSON
  
  if (nrow(check_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Prepare data - use imageBasedEyesToPointCm (vertical) and rulerBasedEyesToFootCm (horizontal)
  plot_data <- check_data %>%
    mutate(
      type = 'check',
      plot_x = as.numeric(rulerBasedEyesToFootCm),
      plot_y = as.numeric(imageBasedEyesToPointCm)
    ) %>%
    select(participant, plot_x, plot_y, rulerBasedEyesToFootCm, imageBasedEyesToPointCm, ipdOverWidth, type) %>%
    filter(is.finite(plot_x), is.finite(plot_y))
  
  
  if (nrow(plot_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = plot_x, y = plot_y)) +
    geom_point(aes(color = participant), size = 2.5, alpha = 0.8) +
    geom_line(aes(color = participant, group = participant),
              linewidth = 0.5, alpha = 0.6) +
    # Add identity line (if imageBasedEyesToPointCm == rulerBasedEyesToFootCm, perfect horizontal viewing)
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                        label = paste0('N=', n_distinct(plot_data$participant))) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5))
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(
      subtitle = 'imageBasedEyesToPointCm vs. rulerBasedEyesToFootCm',
      x = 'rulerBasedEyesToFootCm (cm)',
      y = 'imageBasedEyesToPointCm (cm)',
      caption = 'Measured line-of-sight distance vs. ruler-based horizontal distance.\nDashed line: y = x (horizontal viewing)'
    )
  
  p_height <- compute_auto_height(base_height = 7, n_items = n_distinct(plot_data$participant), per_row = 3, row_increase = 0.06)
  
  return(list(plot = p, height = p_height))
}

# Plot 3: eyesToFootCm (estimated via ipdOverWidth) vs. requestedEyesToFootCm
# This is the key plot showing: "Is the participant at the requested distance?"
# eyesToFootCm = fOverWidth * widthVpx * ipdCm / ipdVpx
# where fOverWidth is saved from calibration and ipdCm = 6.3 cm (standard IPD)
plot_eyesToFootCm_estimated_vs_requested <- function(distanceCalibrationResults) {
  
  # Get calibration data to extract fOverWidth per participant
  # TJSON now has fOverWidth directly (no longer computed from fVpx)
  tjson_data <- distanceCalibrationResults$TJSON
  check_data <- distanceCalibrationResults$checkJSON
  camera <- distanceCalibrationResults$camera
  
  # Standard interpupillary distance
  ipdCm_standard <- 6.3
  
  # Get median fOverWidth per participant from calibration
  if (nrow(tjson_data) > 0) {
    fOverWidth_per_participant <- tjson_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_calibration = median(fOverWidth, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(fOverWidth_calibration))
  } else {
    fOverWidth_per_participant <- tibble(participant = character(), fOverWidth_calibration = numeric())
  }
  
  if (nrow(check_data) == 0 || nrow(fOverWidth_per_participant) == 0 || nrow(camera) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Join check data with calibration fOverWidth and compute estimated eyesToFootCm
  # checkJSON now has ipdOverWidth directly
  plot_data <- check_data %>%
    left_join(fOverWidth_per_participant, by = "participant") %>%
    filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), ipdOverWidth > 0) %>%
    mutate(
      # Estimated horizontal distance from camera measurements
      # eyesToFootCm = fOverWidth * ipdCm / ipdOverWidth
      # (since ipdOverWidth = ipdVpx / widthVpx, and fOverWidth = fVpx / widthVpx)
      eyesToFootCm_estimated = fOverWidth_calibration * ipdCm_standard / ipdOverWidth
    ) %>%
    select(participant, requestedEyesToFootCm, eyesToFootCm_estimated, ipdOverWidth, fOverWidth_calibration) %>%
    filter(is.finite(requestedEyesToFootCm), is.finite(eyesToFootCm_estimated))
  
  if (nrow(plot_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Create the plot
  p <- ggplot(plot_data, aes(x = requestedEyesToFootCm, y = eyesToFootCm_estimated)) +
    # Identity line (perfect agreement)
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    # Data points
    geom_point(aes(color = participant), size = 2.5, alpha = 0.8) +
    geom_line(aes(color = participant, group = participant),
              linewidth = 0.5, alpha = 0.6) +
    scale_x_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_y_log10(
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                        label = paste0('N=', n_distinct(plot_data$participant))) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5))
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(
      subtitle = 'imageBasedEyesToFootCm vs. rulerBasedEyesToFootCm',
      x = 'rulerBasedEyesToFootCm',
      y = 'imageBasedEyesToFootCm',
      caption = paste0('eyesToFootCm = fOverWidth * ipdCm / ipdOverWidth, where ipdCm = ', ipdCm_standard, ' cm\n',
                       'Dashed line: perfect agreement (estimated = requested)')
    )
  
  p_height <- compute_auto_height(base_height = 7, n_items = n_distinct(plot_data$participant), per_row = 3, row_increase = 0.06)
  
  return(list(plot = p, height = p_height))
}
