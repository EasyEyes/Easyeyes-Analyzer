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
# Helper function to extract foot XY coordinates from JSON arrays
# Handles matrix (Nx2), list of pairs, and flat vector formats correctly
# Returns a tibble with columns: left_x, left_y, right_x, right_y
make_xy_df <- function(left_foot, right_foot) {
  if (is.null(left_foot) || is.null(right_foot)) return(NULL)
  
  coords <- list()
  
  # Case 1: Matrix format (most common from fromJSON) - e.g., 8x2 matrix
  if (is.matrix(left_foot)) {
    n_pts <- nrow(left_foot)
    for (i in 1:n_pts) {
      coords[[i]] <- list(
        left_x = left_foot[i, 1],
        left_y = left_foot[i, 2],
        right_x = right_foot[i, 1],
        right_y = right_foot[i, 2]
      )
    }
  }
  # Case 2: List of pairs - e.g., list(c(575,-34), c(589,163))
  else if (is.list(left_foot) && length(left_foot) > 0) {
    n_pts <- length(left_foot)
    for (i in 1:n_pts) {
      coords[[i]] <- list(
        left_x = left_foot[[i]][1],
        left_y = left_foot[[i]][2],
        right_x = right_foot[[i]][1],
        right_y = right_foot[[i]][2]
      )
    }
  }
  # Case 3: Flat numeric vector - e.g., c(575, -34, 589, 163, ...)
  else if (is.numeric(left_foot) && length(left_foot) >= 2) {
    n_pts <- length(left_foot) / 2
    for (i in 1:n_pts) {
      base_idx <- (i - 1) * 2 + 1
      coords[[i]] <- list(
        left_x = left_foot[base_idx],
        left_y = left_foot[base_idx + 1],
        right_x = right_foot[base_idx],
        right_y = right_foot[base_idx + 1]
      )
    }
  }
  
  if (length(coords) == 0) return(NULL)
  
  tibble(
    measurement_idx = 1:length(coords),
    left_x = sapply(coords, function(x) x$left_x),
    left_y = sapply(coords, function(x) x$left_y),
    right_x = sapply(coords, function(x) x$right_x),
    right_y = sapply(coords, function(x) x$right_y)
  )
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

        # Store fOverWidth and ipdCm for later factorVpxCm computation (after widthVpx is available)
        fOverWidth_vals <- if (!is.null(t_tjson$fOverWidth)) as.numeric(t_tjson$fOverWidth) else NA_real_
        ipdCm_vals <- if (!is.null(t_tjson$ipdCm)) as.numeric(t_tjson$ipdCm) else NA_real_
        
        # Get median fOverWidth and ipdCm for this participant
        fOverWidth_median <- median(fOverWidth_vals[!is.na(fOverWidth_vals)], na.rm = TRUE)
        ipdCm_median <- median(ipdCm_vals[!is.na(ipdCm_vals)], na.rm = TRUE)
        
      }, error = function(e) {
        fOverWidth_median <<- NA_real_
        ipdCm_median <<- NA_real_
      })
    }
    
    # Initialize fOverWidth_median and ipdCm_median if not set
    if (!exists("fOverWidth_median") || !is.finite(fOverWidth_median)) fOverWidth_median <- NA_real_
    if (!exists("ipdCm_median") || !is.finite(ipdCm_median)) ipdCm_median <- NA_real_

    # Select and filter data - get cameraResolutionXY first to extract widthVpx
    t <- data_list[[i]] %>%
      select(participant, 
             any_of(c("_calibrateTrackDistance", "_calibrateTrackDistancePupil", "cameraResolutionXY", "screenResolutionXY"))) %>%
      rename(PavloviaParticipantID = participant) %>%
      distinct() %>%
      filter(!is.na(cameraResolutionXY), cameraResolutionXY != "") %>%
      mutate(
        widthVpx = as.numeric(extract_width_px(cameraResolutionXY)),
        fOverWidth = fOverWidth_median,
        ipdCm = ipdCm_median,
        # Now compute factorVpxCm with widthVpx available
        factorVpxCm = fOverWidth * widthVpx * ipdCm
      ) %>%
      # Rename cameraResolutionXY -> cameraResolutionXYVpx for display
      rename(cameraResolutionXYVpx = cameraResolutionXY) %>%
      # Rename screenResolutionXY -> screenResolutionXYPx for display (if it exists)
      {if("screenResolutionXY" %in% names(.)) rename(., screenResolutionXYPx = screenResolutionXY) else .}
    
    # Rename columns if they exist
    if ("_calibrateTrackDistancePupil" %in% names(t)) {
      t <- t %>% rename(`_calibrateDistancePupil` = `_calibrateTrackDistancePupil`)
    }
    if ("_calibrateTrackDistance" %in% names(t)) {
      t <- t %>% rename(`_calibrateDistance` = `_calibrateTrackDistance`)
    }
    
  
            
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
  
  # Helper function to parse comma-separated string or array to numeric vector
  parse_csv_or_array <- function(val) {
    if (is.null(val)) return(NULL)
    if (is.numeric(val)) return(val)  # Already numeric array
    if (is.character(val) && length(val) == 1 && grepl(",", val)) {
      # Comma-separated string like "7.6,  12.7,  22.9"
      return(as.numeric(trimws(unlist(strsplit(val, ",")))))
    }
    return(as.numeric(val))  # Try direct conversion
  }
  
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
        
        # Extract pxPerCm or convert from ppi (handle comma-separated strings)
        pxPerCm_vals <- NULL
        if (!is.null(screenSizeJSON$pxPerCm)) {
          pxPerCm_vals <- parse_csv_or_array(screenSizeJSON$pxPerCm)
        } else if (!is.null(screenSizeJSON$ppi)) {
          pxPerCm_vals <- parse_csv_or_array(screenSizeJSON$ppi) / 2.54
        }
        
        # Extract requestedCm values (handle comma-separated strings)
        requestedCm_vals <- NULL
        if (!is.null(screenSizeJSON$SizeCheckRequestedCm)) {
          requestedCm_vals <- parse_csv_or_array(screenSizeJSON$SizeCheckRequestedCm)
        } else if (!is.null(screenSizeJSON$requestedCm)) {
          requestedCm_vals <- parse_csv_or_array(screenSizeJSON$requestedCm)
        }
        
        # Remove NA values
        if (!is.null(pxPerCm_vals)) pxPerCm_vals <- pxPerCm_vals[!is.na(pxPerCm_vals)]
        if (!is.null(requestedCm_vals)) requestedCm_vals <- requestedCm_vals[!is.na(requestedCm_vals)]
        
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
              requestedCm_vals <- rep(8.56, length(pxPerCm_vals))
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
        # Silently skip if JSON parsing fails
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
  
  # Deduplicate camera_data by PavloviaParticipantID to ensure one row per participant
  # Keep the first occurrence when there are multiple rows (e.g., from different conditions)
  camera_data <- camera_data %>%
    distinct(PavloviaParticipantID, .keep_all = TRUE)
  
  # Perform full outer join to include all participants from both tables
  merged_data <- camera_data %>%
    full_join(participant_info_clean, by = "PavloviaParticipantID")
  
    
     
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
      # Format both columns to exactly 4 decimal places (including trailing zeros)
      `fOverWidth calibration` = ifelse(is.na(fOverWidth_calibration), NA_character_, format(round(fOverWidth_calibration, 4), nsmall = 4)),
      `fOverWidth check` = ifelse(is.na(fOverWidth_check), NA_character_, format(round(fOverWidth_check, 4), nsmall = 4)),
      # Calculate fOverWidth calibration/check: ratio of calibration to check values
      calibration_check_ratio_tmp = ifelse(!is.na(fOverWidth_calibration) & !is.na(fOverWidth_check) & fOverWidth_check != 0,
                                          fOverWidth_calibration / fOverWidth_check, NA_real_),
      `fOverWidth calibration/check` = ifelse(is.na(calibration_check_ratio_tmp), NA_character_, format(round(calibration_check_ratio_tmp, 3), nsmall = 3))
    ) %>%
    select(-fOverWidth_calibration, -fOverWidth_check, -calibration_check_ratio_tmp, -factorVpxCm) %>%
    arrange(ok_priority, PavloviaParticipantID) %>%
    select(-ok_priority) %>%
    # Delete cameraResolutionN column
    {if("cameraResolutionN" %in% names(.)) select(., -cameraResolutionN) else .} %>%
    # Remove objectName if it exists (redundant with Object)
    {if("objectName" %in% names(.)) select(., -objectName) else .} %>%
    # Rename columns with spaces for readability
    rename(
      `Pavlovia Participant ID` = PavloviaParticipantID,
      `object Length Cm` = objectLengthCm
    ) %>%
    # Rename Prolific participant ID to Prolific Participant ID (capitalize P)
    {if("Prolific participant ID" %in% names(.)) rename(., `Prolific Participant ID` = `Prolific participant ID`) else .} %>%
    # Rename objectSuggestion to object Suggestion
    {if("objectSuggestion" %in% names(.)) rename(., `object Suggestion` = objectSuggestion) else .} %>%
    # Rename cameraResolutionXSD to "SD of widthVpx"
    {if("cameraResolutionXSD" %in% names(.)) rename(., `SD of widthVpx` = cameraResolutionXSD) else .} %>%
    # Rename screenResolutionXY to screenResolutionXYPx if it exists
    {if("screenResolutionXY" %in% names(.) && !"screenResolutionXYPx" %in% names(.)) 
       rename(., screenResolutionXYPx = screenResolutionXY) 
     else .} %>%
    # Define the desired column order
    {desired_order <- c(
      "ok",
      "Pavlovia Participant ID",
      "Prolific Participant ID",
      "device type",
      "system",
      "browser",
      "Prolific min",
      "_calibrateDistance",
      "_calibrateDistancePupil",
      "ipdCm",
      "screenWidthCm",
      "pxPerCm",
      "screenResolutionXYPx",
      "cameraResolutionXYVpx",
      "widthVpx",
      "SD of widthVpx",
      "fOverWidth calibration",
      "fOverWidth check",
      "fOverWidth calibration/check",
      "rulerCm",
      "object Length Cm",
      "Object",
      "object Suggestion",
      "Comment"
    )
    # Get columns that exist in the data
    existing_cols <- names(.)
    # Filter desired_order to only include columns that exist
    ordered_cols <- c(desired_order[desired_order %in% existing_cols],
                      existing_cols[!existing_cols %in% desired_order])
    # Reorder columns
    select(., all_of(ordered_cols))
    } %>%
    # Ensure one row per participant (final deduplication)
    distinct(`Pavlovia Participant ID`, .keep_all = TRUE)
  
  
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
    # NOTE: Now extracting SizeCheckEstimatedPxPerCm and SizeCheckRequestedCm from calibrateScreenSizeJSON
    # JSON format: "SizeCheckRequestedCm":"7.6,  12.7,  22.9", "SizeCheckEstimatedPxPerCm":"49.2,  49.4,  50.0"
    has_screenSizeJSON <- "calibrateScreenSizeJSON" %in% names(dl)
    message("[DEBUG sizeCheck] Session ", i, ": has calibrateScreenSizeJSON column = ", has_screenSizeJSON)
    if (has_screenSizeJSON) {
      tryCatch({
        raw_json <- get_first_non_na(dl$calibrateScreenSizeJSON)
        message("[DEBUG sizeCheck] Session ", i, ": raw_json is.null=", is.null(raw_json), 
                ", is.na=", if(!is.null(raw_json)) is.na(raw_json) else "NULL",
                ", nchar=", if(!is.null(raw_json) && !is.na(raw_json)) nchar(raw_json) else 0)
        message("[DEBUG sizeCheck] Session ", i, ": JSON content: ", substr(raw_json, 1, 200))
        if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
          json_txt <- sanitize_json_string(raw_json)
          screenSizeJSON <- fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          message("[DEBUG sizeCheck] Session ", i, ": JSON fields: ", paste(names(screenSizeJSON), collapse=", "))
          
          # Helper function to parse comma-separated string or array to numeric vector
          parse_csv_or_array <- function(val) {
            if (is.null(val)) return(NULL)
            if (is.numeric(val)) return(val)  # Already numeric array
            if (is.character(val) && length(val) == 1 && grepl(",", val)) {
              # Comma-separated string like "7.6,  12.7,  22.9"
              return(as.numeric(trimws(unlist(strsplit(val, ",")))))
            }
            return(as.numeric(val))  # Try direct conversion
          }
          
          # Extract SizeCheckEstimatedPxPerCm from JSON (try multiple possible field names)
          estimated_vals <- NULL
          if (!is.null(screenSizeJSON$SizeCheckEstimatedPxPerCm)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$SizeCheckEstimatedPxPerCm)
            message("[DEBUG sizeCheck] Session ", i, ": Found SizeCheckEstimatedPxPerCm: ", paste(estimated_vals, collapse=", "))
          } else if (!is.null(screenSizeJSON$pxPerCm)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$pxPerCm)
            message("[DEBUG sizeCheck] Session ", i, ": Found pxPerCm (fallback): ", paste(estimated_vals, collapse=", "))
          } else if (!is.null(screenSizeJSON$ppi)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$ppi) / 2.54
            message("[DEBUG sizeCheck] Session ", i, ": Found ppi (converted): ", paste(estimated_vals, collapse=", "))
          } else {
            message("[DEBUG sizeCheck] Session ", i, ": No estimated values found")
          }
          
          # Extract SizeCheckRequestedCm from JSON (try multiple possible field names)
          requested_vals <- NULL
          if (!is.null(screenSizeJSON$SizeCheckRequestedCm)) {
            requested_vals <- parse_csv_or_array(screenSizeJSON$SizeCheckRequestedCm)
            message("[DEBUG sizeCheck] Session ", i, ": Found SizeCheckRequestedCm: ", paste(requested_vals, collapse=", "))
          } else if (!is.null(screenSizeJSON$requestedCm)) {
            requested_vals <- parse_csv_or_array(screenSizeJSON$requestedCm)
            message("[DEBUG sizeCheck] Session ", i, ": Found requestedCm (fallback): ", paste(requested_vals, collapse=", "))
          } else {
            message("[DEBUG sizeCheck] Session ", i, ": No requested values found - THIS IS WHY sizeCheck is empty!")
          }
          
          # Remove NA values
          if (!is.null(estimated_vals)) estimated_vals <- estimated_vals[!is.na(estimated_vals)]
          if (!is.null(requested_vals)) requested_vals <- requested_vals[!is.na(requested_vals)]
          
          # If we have valid values from JSON, create rows
          if (!is.null(estimated_vals) && !is.null(requested_vals) && 
              length(estimated_vals) > 0 && length(requested_vals) > 0) {
            
            # Make sure arrays are same length (take min length if different)
            n_vals <- min(length(estimated_vals), length(requested_vals))
            estimated_vals <- estimated_vals[1:n_vals]
            requested_vals <- requested_vals[1:n_vals]
            
            # Extract metadata columns that exist
            base_cols <- c("_calibrateTrackDistance", "_calibrateTrackDistancePupil",
                           "_calibrateTrackDistanceAllowedRatio", "_calibrateTrackDistanceShowLengthBool",
                           "_calibrateTrackDistanceTimes", "calibrateScreenSizeAllowedRatio",
                           "calibrateScreenSizeTimes", "viewingDistanceWhichEye",
                           "viewingDistanceWhichPoint", "participant", "rulerLength", "rulerUnit", "pxPerCm")
            existing_cols <- intersect(base_cols, names(dl))
            
            # Build base tibble with metadata
            t_base <- dl %>% select(all_of(existing_cols)) %>%
              mutate(
                `_calibrateTrackDistanceAllowedRatio` = if ("_calibrateTrackDistanceAllowedRatio" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceAllowedRatio`) else NA,
                `_calibrateTrackDistanceShowLengthBool` = if ("_calibrateTrackDistanceShowLengthBool" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceShowLengthBool`) else NA,
                `_calibrateTrackDistanceTimes` = if ("_calibrateTrackDistanceTimes" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceTimes`) else NA,
                calibrateScreenSizeAllowedRatio = if ("calibrateScreenSizeAllowedRatio" %in% names(.)) get_first_non_na(`calibrateScreenSizeAllowedRatio`) else NA,
                calibrateScreenSizeTimes = if ("calibrateScreenSizeTimes" %in% names(.)) get_first_non_na(calibrateScreenSizeTimes) else NA,
                `_calibrateTrackDistance` = if ("_calibrateTrackDistance" %in% names(.)) get_first_non_na(`_calibrateTrackDistance`) else NA,
                `_calibrateTrackDistancePupil` = if ("_calibrateTrackDistancePupil" %in% names(.)) get_first_non_na(`_calibrateTrackDistancePupil`) else NA,
                `viewingDistanceWhichEye` = if ("viewingDistanceWhichEye" %in% names(.)) get_first_non_na(`viewingDistanceWhichEye`) else NA,
                `viewingDistanceWhichPoint` = if ("viewingDistanceWhichPoint" %in% names(.)) get_first_non_na(`viewingDistanceWhichPoint`) else NA
              ) %>%
              distinct() %>%
              slice(1)
            
            # Create one row per measurement
            for (j in 1:n_vals) {
              t_row <- t_base %>%
                mutate(
                  SizeCheckEstimatedPxPerCm = estimated_vals[j],
                  SizeCheckRequestedCm = requested_vals[j]
                )
              sizeCheck <- rbind(sizeCheck, t_row)
            }
            message("[DEBUG sizeCheck] Session ", i, ": Added ", n_vals, " rows to sizeCheck. Total rows now: ", nrow(sizeCheck))
          } else {
            message("[DEBUG sizeCheck] Session ", i, ": No valid estimated/requested values found in JSON")
          }
        }
      }, error = function(e) {
        message("[DEBUG sizeCheck ERROR] Session ", i, ": ", e$message)
      })
    }
    
    # -------- Ipd camera px alias --------
    if ('calibrateTrackDistanceIpdCameraPx' %in% names(dl)) {
      dl$calibrateTrackDistanceIpdVpx <- dl$calibrateTrackDistanceIpdCameraPx
    }
    
    # NOTE: Legacy CSV columns calibrateTrackDistanceMeasuredCm and calibrateTrackDistanceRequestedCm
    # have been DELETED. Measurement data is now extracted from JSON only:
    # - distanceCalibrationJSON (for calibration)
    # - distanceCheckJSON (for check)
    # See the feet_calib and feet_check extraction sections below.
    
    # -------- feet position during calibration (from distanceCalibrationJSON) --------
    has_distCalibJSON <- "distanceCalibrationJSON" %in% names(dl)
    has_distCalibTJSON <- "distanceCalibrationTJSON" %in% names(dl)
    if (has_distCalibJSON || has_distCalibTJSON) {
      json_col <- if (has_distCalibJSON) "distanceCalibrationJSON" else "distanceCalibrationTJSON"
      
      # Use any_of() for columns that may not exist in the CSV (measurement data is in JSON only)
      # NOTE: calibrateTrackDistanceMeasuredCm and calibrateTrackDistanceRequestedCm are DELETED from CSV
      # These values are now extracted from distanceCalibrationJSON/distanceCheckJSON only
      optional_cols <- c("_calibrateTrackDistanceAllowedRatio", "_calibrateTrackDistanceShowLengthBool",
                         "_calibrateTrackDistanceTimes", "calibrateScreenSizeAllowedRatio", 
                         "calibrateScreenSizeTimes", "_calibrateTrackDistance", "_calibrateTrackDistancePupil",
                         "viewingDistanceWhichEye", "viewingDistanceWhichPoint",
                         "pxPerCm", "screenWidthPx", "screenHeightPx")
      
      t_meta <- dl %>%
        select(participant, any_of(optional_cols), all_of(json_col)) %>%
        distinct()
      
      # Apply get_first_non_na to columns that exist
      if ("_calibrateTrackDistanceAllowedRatio" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(`_calibrateTrackDistanceAllowedRatio` = get_first_non_na(`_calibrateTrackDistanceAllowedRatio`))
      }
      if ("_calibrateTrackDistanceShowLengthBool" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(`_calibrateTrackDistanceShowLengthBool` = get_first_non_na(`_calibrateTrackDistanceShowLengthBool`))
      }
      if ("_calibrateTrackDistanceTimes" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(`_calibrateTrackDistanceTimes` = get_first_non_na(`_calibrateTrackDistanceTimes`))
      }
      if ("calibrateScreenSizeAllowedRatio" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(calibrateScreenSizeAllowedRatio = get_first_non_na(calibrateScreenSizeAllowedRatio))
      }
      if ("calibrateScreenSizeTimes" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(calibrateScreenSizeTimes = get_first_non_na(calibrateScreenSizeTimes))
      }
      if ("_calibrateTrackDistance" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(`_calibrateTrackDistance` = get_first_non_na(`_calibrateTrackDistance`))
      }
      if ("_calibrateTrackDistancePupil" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(`_calibrateTrackDistancePupil` = get_first_non_na(`_calibrateTrackDistancePupil`))
      }
      if ("viewingDistanceWhichEye" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(viewingDistanceWhichEye = get_first_non_na(viewingDistanceWhichEye))
      }
      if ("viewingDistanceWhichPoint" %in% names(t_meta)) {
        t_meta <- t_meta %>% mutate(viewingDistanceWhichPoint = get_first_non_na(viewingDistanceWhichPoint))
      }
      
     

      json_col <- if ("distanceCalibrationJSON" %in% names(dl)) "distanceCalibrationJSON" else "distanceCalibrationTJSON"
      tryCatch({
          raw_json <- get_first_non_na(t_meta[[json_col]])
          message("[DEBUG FEET JSON] Session ", i, ": raw_json is.null=", is.null(raw_json), 
                  ", is.na=", if(!is.null(raw_json)) is.na(raw_json) else "NULL",
                  ", nchar=", if(!is.null(raw_json) && !is.na(raw_json)) nchar(raw_json) else 0)
          json_txt <- sanitize_json_string(raw_json)
          distanceCalibration <- fromJSON(
            json_txt,
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
          )
          message("[DEBUG FEET JSON PARSED] Session ", i, ": parsed OK, class=", class(distanceCalibration),
                  ", fields=", paste(names(distanceCalibration), collapse=", "))
          
          # Extract foot coordinates using make_xy_df() helper (handles matrix/list/vector formats)
          left_eye_foot <- distanceCalibration$leftEyeFootXYPx
          right_eye_foot <- distanceCalibration$rightEyeFootXYPx
          spot_deg_array <- distanceCalibration$spotDeg
          spot_degrees <- c()
          
          message("[DEBUG FEET distanceCalibrationJSON] Session ", i, 
                  ": has leftEyeFootXYPx=", !is.null(left_eye_foot),
                  ", has rightEyeFootXYPx=", !is.null(right_eye_foot),
                  ", left class=", if(!is.null(left_eye_foot)) class(left_eye_foot) else "NULL",
                  ", left dim=", if(!is.null(left_eye_foot) && is.matrix(left_eye_foot)) paste(dim(left_eye_foot), collapse="x") else "N/A")
          
          if (!is.null(spot_deg_array)) {
            spot_degrees <- as.numeric(spot_deg_array)
          }
          
          # Use make_xy_df() to correctly parse foot coordinates
          coords_df <- make_xy_df(left_eye_foot, right_eye_foot)
          
          if (!is.null(coords_df) && nrow(coords_df) > 0) {
            
            
            # NOTE: calibrateTrackDistanceMeasuredCm and calibrateTrackDistanceRequestedCm are DELETED from CSV
            # Always extract measurements from JSON (distanceCalibrationJSON)
            if (!is.null(distanceCalibration$imageBasedEyesToPointCm)) {
              
              measured_vals <- as.numeric(distanceCalibration$imageBasedEyesToPointCm)
              requested_vals <- as.numeric(distanceCalibration$rulerBasedEyesToPointCm)
              n_measurements <- length(measured_vals)
              
              # Create t from t_meta with JSON measurements
              t <- t_meta[rep(1, n_measurements), ] %>%
                mutate(
                  calibrateTrackDistanceMeasuredCm = measured_vals,
                  calibrateTrackDistanceRequestedCm = requested_vals,
                  measurement_index = row_number()
                )
              
            } else {
              
              t <- tibble()
            }
            
            # Common processing for both branches
            n_coords <- nrow(coords_df)
            if (nrow(t) > 0 && n_coords > 0) {
              if (n_coords > nrow(t)) {
                coords_df <- coords_df[1:nrow(t), ]
              } else if (n_coords < nrow(t)) {
                t <- t[1:n_coords, ]
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
              # Also add to distance tibble (same data but used by different plots)
              distance <- rbind(distance, t)
              
            } else {
              
            }
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
      
      # Wrap distanceCheckJSON parsing in tryCatch to handle malformed JSON gracefully
      distanceCheck <- NULL
      tryCatch({
        raw_json <- get_first_non_na(dl$distanceCheckJSON)
        if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
          json_txt <- sanitize_json_string(raw_json)
          distanceCheck <- fromJSON(
            json_txt,
            simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
          )
        }
      }, error = function(e) {
        message("[DEBUG distanceCheckJSON PARSE ERROR] Session ", i, ": ", e$message)
      })
      
      # Skip if parsing failed
      if (is.null(distanceCheck)) {
        next
      }
        
      # Extract metadata from distanceCheckJSON for debugging/logging
      check_json_experiment  <- if (!is.null(distanceCheck$experiment)) distanceCheck$experiment else NA_character_
      check_json_participant <- if (!is.null(distanceCheck$participant)) distanceCheck$participant else NA_character_
      check_json_date        <- if (!is.null(distanceCheck$date)) distanceCheck$date else NA_character_
      check_json_type        <- if (!is.null(distanceCheck$json)) distanceCheck$json else NA_character_

      # Parse distanceCalibrationJSON to get t_tjson (for TJSON extraction later)
      # NOTE: Fixed bug - was using 't$' but 't' was not defined in this scope, should use 'dl$'
      t_tjson <- NULL
      tjson_col <- if ("distanceCalibrationJSON" %in% names(dl)) "distanceCalibrationJSON" else "distanceCalibrationTJSON"
      
      if (tjson_col %in% names(dl)) {
        tryCatch({
          raw_json_tjson <- get_first_non_na(dl[[tjson_col]])
          if (!is.null(raw_json_tjson) && !is.na(raw_json_tjson) && raw_json_tjson != "") {
            json_txt_tjson <- sanitize_json_string(raw_json_tjson)
            t_tjson <- fromJSON(
              json_txt_tjson,
              simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE
            )
          }
        }, error = function(e) {
          # Silently skip if JSON parsing fails
        })
      }
      
      # -------- distance check median factorVpxCm --------
      tryCatch({
        participant_id_debug <- if("participant" %in% names(dl)) first(na.omit(dl$participant)) else "UNKNOWN"
        
        # Try multiple methods to get factorVpxCm values
        measured_vals <- NULL
        
        # Method 1: Use measuredFactorVpxCm directly (old schema)
        if (!is.null(distanceCheck$measuredFactorVpxCm)) {
          measured_vals <- distanceCheck$measuredFactorVpxCm
        } 
        # Method 2: Use measuredFactorCameraPxCm (old schema alternative)
        else if (!is.null(distanceCheck$measuredFactorCameraPxCm)) {
          measured_vals <- distanceCheck$measuredFactorCameraPxCm
        }
        # Method 3: Compute from fVpx - factorVpxCm = fVpx * ipdCm
        # Note: distanceCheckJSON has fVpx (focal length in pixels), not fOverWidth
        else if (!is.null(distanceCheck$fVpx) && !is.null(distanceCheck$ipdCm)) {
          fVpx_vals <- as.numeric(distanceCheck$fVpx)
          ipdCm_val <- as.numeric(first(distanceCheck$ipdCm))
          measured_vals <- fVpx_vals * ipdCm_val
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
        
        # -------- ALSO extract foot positions from distanceCheckJSON for feet_check --------
        # distanceCheckJSON has leftEyeFootXYPx and rightEyeFootXYPx arrays
        # Use make_xy_df() helper to correctly handle matrix/list/vector formats
        if (!is.null(distanceCheck$leftEyeFootXYPx) && !is.null(distanceCheck$rightEyeFootXYPx)) {
          check_coords_df <- make_xy_df(distanceCheck$leftEyeFootXYPx, distanceCheck$rightEyeFootXYPx)
          
          if (!is.null(check_coords_df) && nrow(check_coords_df) > 0) {
            check_coords_df <- check_coords_df %>% filter(!is.na(left_x), !is.na(right_x))
            
            if (nrow(check_coords_df) > 0) {
              # Get pxPerCm from the JSON
              pxPerCm_val <- if (!is.null(distanceCheck$pxPerCm)) as.numeric(distanceCheck$pxPerCm[1]) else 37.8
              
              # Get screen dimensions from the data list
              screenWidthPx_val <- if ("screenWidthPx" %in% names(dl)) first(na.omit(as.numeric(dl$screenWidthPx))) else NA_real_
              screenHeightPx_val <- if ("screenHeightPx" %in% names(dl)) first(na.omit(as.numeric(dl$screenHeightPx))) else NA_real_
              
              # Extract imageBasedEyesToPointCm and requestedEyesToPointCm for distance ratio
              # imageBasedEyesToPointCm is the measured value, requestedEyesToPointCm is the target
              measured_eyes_vals <- as.numeric(distanceCheck$imageBasedEyesToPointCm)
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
                  distance_ratio = distance_ratio_vals,
                  screenWidthPx = screenWidthPx_val,
                  screenHeightPx = screenHeightPx_val,
                  data_list_index = i,
                  measurement_index = measurement_idx
                ) %>%
                select(-measurement_idx)
              
              feet_check <- rbind(feet_check, t_check_feet)
              message("[DEBUG feet_check] Session ", i, ": Added ", nrow(t_check_feet), " rows to feet_check")
            }
          }
        }
        
      }, error = function(e) {
        message("[DEBUG distanceCheckJSON ERROR] Session ", i, ": ", e$message)
      })
    #### TJSON data ####
      #distanceCalibrationTJSON has been renamed to distanceCalibrationJSON
      
      # Only process TJSON if we successfully parsed t_tjson
      if (!is.null(t_tjson)) {
        # Extract metadata for debugging/logging
        json_experiment  <- if (!is.null(t_tjson$experiment)) t_tjson$experiment else NA_character_
        json_participant <- if (!is.null(t_tjson$participant)) t_tjson$participant else NA_character_
        json_date        <- if (!is.null(t_tjson$date)) t_tjson$date else NA_character_
        json_type        <- if (!is.null(t_tjson$json)) t_tjson$json else NA_character_

        tmp <- tibble(
          participant      = first(na.omit(dl$participant)),
          # Metadata from JSON for debugging
          json_experiment  = json_experiment,
          json_participant = json_participant,
          json_date        = json_date,
          json_type        = json_type,
          # rightEyeToFootCm renamed to rulerBasedRightEyeToFootCm (uses object of known length as ruler)
          rulerBasedRightEyeToFootCm = if (!is.null(t_tjson$rulerBasedRightEyeToFootCm)) t_tjson$rulerBasedRightEyeToFootCm else t_tjson$rightEyeToFootCm,
          # leftEyeToFootCm renamed to rulerBasedLeftEyeToFootCm (uses object of known length as ruler)
          rulerBasedLeftEyeToFootCm  = if (!is.null(t_tjson$rulerBasedLeftEyeToFootCm)) t_tjson$rulerBasedLeftEyeToFootCm else t_tjson$leftEyeToFootCm,
          # eyesToFootCm renamed to rulerBasedEyesToFootCm (uses object of known length as ruler)
          rulerBasedEyesToFootCm     = if (!is.null(t_tjson$rulerBasedEyesToFootCm)) as.numeric(t_tjson$rulerBasedEyesToFootCm) else as.numeric(t_tjson$eyesToFootCm),
          imageBasedEyesToFootCm     = if (!is.null(t_tjson$imageBasedEyesToFootCm)) as.numeric(t_tjson$imageBasedEyesToFootCm) else NA_real_,
          rulerBasedEyesToPointCm    = if (!is.null(t_tjson$rulerBasedEyesToPointCm)) as.numeric(t_tjson$rulerBasedEyesToPointCm) else NA_real_,
          imageBasedEyesToPointCm    = if (!is.null(t_tjson$imageBasedEyesToPointCm)) as.numeric(t_tjson$imageBasedEyesToPointCm) else NA_real_,
          # New parameters: fOverWidth and ipdOverWidth (4 decimal digits)
          fOverWidth       = if (!is.null(t_tjson$fOverWidth)) as.numeric(t_tjson$fOverWidth) else NA_real_,
          ipdOverWidth     = if (!is.null(t_tjson$ipdOverWidth)) as.numeric(t_tjson$ipdOverWidth) else NA_real_,
          ipdCm            = if (!is.null(t_tjson$ipdCm)) as.numeric(t_tjson$ipdCm) else NA_real_
        ) %>%
          mutate(
            # Use rulerBasedEyesToFootCm if available, otherwise compute from left/right eye values
            eyeToFootCm = ifelse(!is.na(rulerBasedEyesToFootCm), 
(as.numeric(rulerBasedEyesToFootCm)),
((as.numeric(rulerBasedLeftEyeToFootCm) + as.numeric(rulerBasedRightEyeToFootCm)) / 2))
          )

        TJSON <- rbind(TJSON, tmp)
      }

      
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
        
        # Get ipdCm - try t_tjson first, then distanceCheck
        ipdCm_val <- NA_real_
        if (!is.null(t_tjson) && !is.null(t_tjson$ipdCm)) {
          ipdCm_val <- as.numeric(first(na.omit(t_tjson$ipdCm)))
        } else if (!is.null(distanceCheck$ipdCm)) {
          ipdCm_val <- as.numeric(first(na.omit(distanceCheck$ipdCm)))
        }

        tmp <- tibble(
          participant   = first(na.omit(dl$participant)),
          measuredEyesToPointCm = measuredEyesToPointCm_vals,
          requestedEyesToPointCm = requestedEyesToPointCm_vals,
          eyesToPointCm = measuredEyesToPointCm_vals,  # Keep for backward compatibility
          imageBasedEyesToPointCm = imageBasedEyesToPointCm_vals,
          rulerBasedEyesToPointCm = rulerBasedEyesToPointCm_vals,
          footToPointCm = as.numeric(distanceCheck$footToPointCm),
          rulerBasedEyesToFootCm = if (!is.null(distanceCheck$rulerBasedEyesToFootCm)) as.numeric(distanceCheck$rulerBasedEyesToFootCm) else NA_real_,
          imageBasedEyesToFootCm = sqrt(imageBasedEyesToPointCm^2 - footToPointCm^2),
          ipdOverWidth  = ipdOverWidth_vals,
          calibrationFOverWidth = calibrationFOverWidth_val,
          ipdCm         = ipdCm_val,
          json_type     = check_json_type  # Store json_type to enable filtering jitter data
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
  
  # Add explicit measurement order to preserve the order data was received
  # This is critical for detecting order effects in FaceMesh measurements
  if (nrow(checkJSON) > 0) {
    checkJSON <- checkJSON %>%
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup() %>%
      mutate(measurement_order_global = row_number())
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
  # DEBUG: Comprehensive summary of data extracted from each JSON source
  # =============================================================================
  message("\n", paste(rep("=", 80), collapse=""))
  message("DEBUG: DATA EXTRACTED FROM JSON SOURCES")
  message(paste(rep("=", 80), collapse=""))
  
  # --- From calibrateScreenSizeJSON ---
  message("\n--- From calibrateScreenSizeJSON ---")
  message("  sizeCheck tibble: ", nrow(sizeCheck), " rows")
  if (nrow(sizeCheck) > 0) {
    message("    Columns: ", paste(names(sizeCheck), collapse=", "))
    message("    Participants: ", paste(unique(sizeCheck$participant), collapse=", "))
    if ("SizeCheckEstimatedPxPerCm" %in% names(sizeCheck)) {
      message("    SizeCheckEstimatedPxPerCm: ", paste(head(sizeCheck$SizeCheckEstimatedPxPerCm, 5), collapse=", "), 
              if(nrow(sizeCheck) > 5) "..." else "")
    }
    if ("SizeCheckRequestedCm" %in% names(sizeCheck)) {
      message("    SizeCheckRequestedCm: ", paste(head(sizeCheck$SizeCheckRequestedCm, 5), collapse=", "),
              if(nrow(sizeCheck) > 5) "..." else "")
    }
  }
  message("  raw_pxPerCm tibble: (computed later from sizeCheck)")
  
  # --- From distanceCalibrationJSON ---
  message("\n--- From distanceCalibrationJSON ---")
  message("  distance tibble: ", nrow(distance), " rows")
  if (nrow(distance) > 0) {
    message("    Columns: ", paste(names(distance), collapse=", "))
    message("    Participants: ", paste(unique(distance$participant), collapse=", "))
    if ("calibrateTrackDistanceMeasuredCm" %in% names(distance)) {
      message("    calibrateTrackDistanceMeasuredCm: ", paste(head(distance$calibrateTrackDistanceMeasuredCm, 5), collapse=", "))
    }
    if ("calibrateTrackDistanceRequestedCm" %in% names(distance)) {
      message("    calibrateTrackDistanceRequestedCm: ", paste(head(distance$calibrateTrackDistanceRequestedCm, 5), collapse=", "))
    }
  }
  message("  feet_calib tibble: ", nrow(feet_calib), " rows")
  if (nrow(feet_calib) > 0) {
    message("    Columns: ", paste(names(feet_calib), collapse=", "))
    message("    Participants: ", paste(unique(feet_calib$participant), collapse=", "))
    if ("left_x" %in% names(feet_calib)) {
      message("    left_x: ", paste(head(feet_calib$left_x, 5), collapse=", "))
      message("    left_y: ", paste(head(feet_calib$left_y, 5), collapse=", "))
      message("    right_x: ", paste(head(feet_calib$right_x, 5), collapse=", "))
      message("    right_y: ", paste(head(feet_calib$right_y, 5), collapse=", "))
    }
    if ("avg_eye_x_cm" %in% names(feet_calib)) {
      message("    avg_eye_x_cm: ", paste(head(feet_calib$avg_eye_x_cm, 5), collapse=", "))
      message("    avg_eye_y_cm: ", paste(head(feet_calib$avg_eye_y_cm, 5), collapse=", "))
    }
    if ("distance_ratio" %in% names(feet_calib)) {
      message("    distance_ratio: ", paste(head(feet_calib$distance_ratio, 5), collapse=", "))
    }
  }
  message("  eye_feet tibble: ", nrow(eye_feet), " rows")
  if (nrow(eye_feet) > 0) {
    message("    Columns: ", paste(names(eye_feet), collapse=", "))
  }
  message("  TJSON tibble: ", nrow(TJSON), " rows")
  if (nrow(TJSON) > 0) {
    message("    Columns: ", paste(names(TJSON), collapse=", "))
    if ("rulerBasedEyesToFootCm" %in% names(TJSON)) {
      message("    rulerBasedEyesToFootCm: ", paste(head(TJSON$rulerBasedEyesToFootCm, 5), collapse=", "))
    }
    if ("imageBasedEyesToFootCm" %in% names(TJSON)) {
      message("    imageBasedEyesToFootCm: ", paste(head(TJSON$imageBasedEyesToFootCm, 5), collapse=", "))
    }
    if ("fOverWidth" %in% names(TJSON)) {
      message("    fOverWidth: ", paste(head(TJSON$fOverWidth, 5), collapse=", "))
    }
  }
  message("  blindspot tibble: ", nrow(blindspot), " rows")
  
  # --- From distanceCheckJSON ---
  message("\n--- From distanceCheckJSON ---")
  message("  checkJSON tibble: ", nrow(checkJSON), " rows")
  if (nrow(checkJSON) > 0) {
    message("    Columns: ", paste(names(checkJSON), collapse=", "))
    message("    Participants: ", paste(unique(checkJSON$participant), collapse=", "))
    if ("requestedEyesToPointCm" %in% names(checkJSON)) {
      message("    requestedEyesToPointCm: ", paste(head(checkJSON$requestedEyesToPointCm, 5), collapse=", "))
    }
    if ("eyesToPointCm" %in% names(checkJSON)) {
      message("    eyesToPointCm: ", paste(head(checkJSON$eyesToPointCm, 5), collapse=", "))
    }
    if ("imageBasedEyesToPointCm" %in% names(checkJSON)) {
      message("    imageBasedEyesToPointCm: ", paste(head(checkJSON$imageBasedEyesToPointCm, 5), collapse=", "))
    }
    if ("ipdOverWidth" %in% names(checkJSON)) {
      message("    ipdOverWidth: ", paste(head(checkJSON$ipdOverWidth, 5), collapse=", "))
    }
    if ("fOverWidth" %in% names(checkJSON)) {
      message("    fOverWidth: ", paste(head(checkJSON$fOverWidth, 5), collapse=", "))
    }
    if ("ipdCm" %in% names(checkJSON)) {
      message("    ipdCm: ", paste(head(checkJSON$ipdCm, 5), collapse=", "))
    }
  }
  message("  feet_check tibble: ", nrow(feet_check), " rows")
  if (nrow(feet_check) > 0) {
    message("    Columns: ", paste(names(feet_check), collapse=", "))
    if ("left_x" %in% names(feet_check)) {
      message("    left_x: ", paste(head(feet_check$left_x, 5), collapse=", "))
      message("    left_y: ", paste(head(feet_check$left_y, 5), collapse=", "))
    }
  }
  message("  check_factor tibble: ", nrow(check_factor), " rows")
  if (nrow(check_factor) > 0) {
    message("    Columns: ", paste(names(check_factor), collapse=", "))
  }
  
  message("\n", paste(rep("=", 80), collapse=""))
  message("END DEBUG: DATA EXTRACTED FROM JSON SOURCES")
  message(paste(rep("=", 80), collapse=""), "\n")
  
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
  
  # Extract raw array data for new histograms
  raw_pxPerCm <- get_raw_pxPerCm_data(filtered_data_list, sizeCheck)
  raw_objectMeasuredCm <- get_raw_objectMeasuredCm_data(filtered_data_list)
  
  # Debug: raw_pxPerCm summary (now that it's computed)
  message("\n--- raw_pxPerCm (computed from calibrateScreenSizeJSON) ---")
  message("  raw_pxPerCm tibble: ", nrow(raw_pxPerCm), " rows")
  if (nrow(raw_pxPerCm) > 0) {
    message("    Columns: ", paste(names(raw_pxPerCm), collapse=", "))
    message("    Participants: ", paste(unique(raw_pxPerCm$participant), collapse=", "))
    if ("pxPerCm" %in% names(raw_pxPerCm)) {
      message("    pxPerCm: ", paste(head(raw_pxPerCm$pxPerCm, 10), collapse=", "),
              if(nrow(raw_pxPerCm) > 10) "..." else "")
    }
    if ("requestedCm" %in% names(raw_pxPerCm)) {
      message("    requestedCm: ", paste(head(raw_pxPerCm$requestedCm, 10), collapse=", "),
              if(nrow(raw_pxPerCm) > 10) "..." else "")
    }
  }
  message("  raw_objectMeasuredCm tibble: ", nrow(raw_objectMeasuredCm), " rows")
  
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
  # NOTE: Now extracting from calibrateScreenSizeJSON instead of obsolete direct columns
  # JSON format: "SizeCheckRequestedCm":"7.6,  12.7,  22.9", "SizeCheckEstimatedPxPerCm":"49.2,  49.4,  50.0"
  
  # Helper function to parse comma-separated string or array to numeric vector
  parse_csv_or_array <- function(val) {
    if (is.null(val)) return(NULL)
    if (is.numeric(val)) return(val)  # Already numeric array
    if (is.character(val) && length(val) == 1 && grepl(",", val)) {
      # Comma-separated string like "7.6,  12.7,  22.9"
      return(as.numeric(trimws(unlist(strsplit(val, ",")))))
    }
    return(as.numeric(val))  # Try direct conversion
  }
  
  for (i in 1:length(data_list)) {
    dl <- data_list[[i]]
    
    # Extract metadata columns that exist
    base_cols <- c("_calibrateTrackDistance", "_calibrateTrackDistancePupil",
                   "_calibrateTrackDistanceAllowedRatio", "_calibrateTrackDistanceShowLengthBool",
                   "_calibrateTrackDistanceTimes", "calibrateScreenSizeAllowedRatio",
                   "calibrateScreenSizeTimes", "viewingDistanceWhichEye",
                   "viewingDistanceWhichPoint", "participant", "rulerLength", "rulerUnit", "pxPerCm")
    existing_cols <- intersect(base_cols, names(dl))
    
    # Try to extract SizeCheckEstimatedPxPerCm and SizeCheckRequestedCm from calibrateScreenSizeJSON
    if ("calibrateScreenSizeJSON" %in% names(dl)) {
      tryCatch({
        raw_json <- get_first_non_na(dl$calibrateScreenSizeJSON)
        if (!is.null(raw_json) && !is.na(raw_json) && raw_json != "") {
          json_txt <- sanitize_json_string(raw_json)
          screenSizeJSON <- fromJSON(json_txt, simplifyVector = TRUE, simplifyDataFrame = TRUE, flatten = TRUE)
          
          # Extract SizeCheckEstimatedPxPerCm from JSON (try multiple possible field names)
          estimated_vals <- NULL
          if (!is.null(screenSizeJSON$SizeCheckEstimatedPxPerCm)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$SizeCheckEstimatedPxPerCm)
          } else if (!is.null(screenSizeJSON$pxPerCm)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$pxPerCm)
          } else if (!is.null(screenSizeJSON$ppi)) {
            estimated_vals <- parse_csv_or_array(screenSizeJSON$ppi) / 2.54
          }
          
          # Extract SizeCheckRequestedCm from JSON (try multiple possible field names)
          requested_vals <- NULL
          if (!is.null(screenSizeJSON$SizeCheckRequestedCm)) {
            requested_vals <- parse_csv_or_array(screenSizeJSON$SizeCheckRequestedCm)
          } else if (!is.null(screenSizeJSON$requestedCm)) {
            requested_vals <- parse_csv_or_array(screenSizeJSON$requestedCm)
          }
          
          # Remove NA values
          if (!is.null(estimated_vals)) estimated_vals <- estimated_vals[!is.na(estimated_vals)]
          if (!is.null(requested_vals)) requested_vals <- requested_vals[!is.na(requested_vals)]
          
          # If we have valid values from JSON, create rows
          if (!is.null(estimated_vals) && !is.null(requested_vals) && 
              length(estimated_vals) > 0 && length(requested_vals) > 0) {
            
            # Make sure arrays are same length (take min length if different)
            n_vals <- min(length(estimated_vals), length(requested_vals))
            estimated_vals <- estimated_vals[1:n_vals]
            requested_vals <- requested_vals[1:n_vals]
            
            # Build base tibble with metadata
            t_base <- dl %>% select(all_of(existing_cols)) %>%
              mutate(
                `_calibrateTrackDistanceAllowedRatio` = if ("_calibrateTrackDistanceAllowedRatio" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceAllowedRatio`) else NA,
                `_calibrateTrackDistanceShowLengthBool` = if ("_calibrateTrackDistanceShowLengthBool" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceShowLengthBool`) else NA,
                `_calibrateTrackDistanceTimes` = if ("_calibrateTrackDistanceTimes" %in% names(.)) get_first_non_na(`_calibrateTrackDistanceTimes`) else NA,
                calibrateScreenSizeAllowedRatio = if ("calibrateScreenSizeAllowedRatio" %in% names(.)) get_first_non_na(`calibrateScreenSizeAllowedRatio`) else NA,
                calibrateScreenSizeTimes = if ("calibrateScreenSizeTimes" %in% names(.)) get_first_non_na(calibrateScreenSizeTimes) else NA,
                `_calibrateTrackDistance` = if ("_calibrateTrackDistance" %in% names(.)) get_first_non_na(`_calibrateTrackDistance`) else NA,
                `_calibrateTrackDistancePupil` = if ("_calibrateTrackDistancePupil" %in% names(.)) get_first_non_na(`_calibrateTrackDistancePupil`) else NA,
                `viewingDistanceWhichEye` = if ("viewingDistanceWhichEye" %in% names(.)) get_first_non_na(`viewingDistanceWhichEye`) else NA,
                `viewingDistanceWhichPoint` = if ("viewingDistanceWhichPoint" %in% names(.)) get_first_non_na(`viewingDistanceWhichPoint`) else NA
              ) %>%
              distinct() %>%
              slice(1)
            
            # Create one row per measurement
            for (j in 1:n_vals) {
              t_row <- t_base %>%
                mutate(
                  SizeCheckEstimatedPxPerCm = estimated_vals[j],
                  SizeCheckRequestedCm = requested_vals[j]
                )
              df <- rbind(df, t_row)
            }
          }
        }
      }, error = function(e) {
        # Silently skip if JSON parsing fails
      })
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

# NOTE: get_measured_distance_data function was REMOVED because it depended on 
# deleted CSV columns: calibrateTrackDistanceMeasuredCm and calibrateTrackDistanceRequestedCm
# Distance measurement data is now extracted from JSON only (distanceCalibrationJSON/distanceCheckJSON)

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

  # Use the pre-computed statement from distanceCalibrationResults (has the parameter values)
  statement <- distanceCalibrationResults$statement
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
      ggpp::geom_text_npc(npcx = 0.5, npcy = 0.98, 
                          label = "Rectangle: screen. Points beyond 50% margin clipped. Origin (0,0): top-left", 
                          size = 2.5, hjust = 0.5, vjust = 1) +
      labs(
        subtitle = "Measured over requested distance vs. foot position during calibration",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)"
      ) +
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      # # Screen annotation (moved close to screen outline, same size as Y label, not bold)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
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
  
  # Use the pre-computed statement from distanceCalibrationResults (has the parameter values)
  statement <- distanceCalibrationResults$statement

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
      ggpp::geom_text_npc(npcx = 0.5, npcy = 0.98, 
                          label = "Rectangle: screen. Points beyond 50% margin clipped. Origin (0,0): top-left", 
                          size = 2.5, hjust = 0.5, vjust = 1) +
      labs(
        subtitle = "Measured over requested distance vs. foot position during check",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)"
      ) +
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
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
    message("Debug: No eye feet data found")
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
      ggpp::geom_text_npc(npcx = 0.5, npcy = 0.98, 
                          label = "Rectangle: screen. Plot range expanded. Origin (0,0): top-left", 
                          size = 2.5, hjust = 0.5, vjust = 1) +
      labs(
        subtitle = "Foot position during calibration",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)"
      ) +
      ggpp::geom_text_npc(
        data = NULL,
        aes(npcx = "right", npcy = "bottom"),
        label = statement
      ) +
      # Screen annotation (moved close to screen outline, same size as Y label, not bold)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
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

  check_data <- distanceCalibrationResults$checkJSON
  calib_data <- distanceCalibrationResults$TJSON  # Calibration data
  statement <- distanceCalibrationResults$statement
  camera <- distanceCalibrationResults$camera
  if (nrow(check_data) == 0 && nrow(calib_data) == 0) {return(NULL)}
  
  # ===== PREPARE CHECK DATA =====
  # Filter out jitter data (exclude rows where json_type indicates jitter)
  check_individual <- tibble()
  if (nrow(check_data) > 0) {
    check_individual <- check_data %>%
      # Filter out jitter: exclude rows where json_type contains "jitter" (case-insensitive)
      # Handle case where json_type column might not exist
      {if("json_type" %in% names(.)) filter(., is.na(json_type) | !grepl("jitter", json_type, ignore.case = TRUE)) else .} %>%
      mutate(
        source = "check",
        # For "Measured vs. requested distance" plot (p1):
        # x-axis: rulerBasedEyesToFootCm (ruler-based), y-axis: imageBasedEyesToPointCm (image-based)
        p1_x = as.numeric(rulerBasedEyesToFootCm),
        p1_y = as.numeric(imageBasedEyesToPointCm),
        # For "Measured over requested distance" plot (p2):
        # x-axis: rulerBasedEyesToFootCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToFootCm
        p2_x = as.numeric(rulerBasedEyesToFootCm),
        p2_y = as.numeric(imageBasedEyesToPointCm) / as.numeric(rulerBasedEyesToFootCm)
      ) %>% 
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup()
  }
  
  # ===== PREPARE CALIBRATION DATA =====
  calib_individual <- tibble()
  if (nrow(calib_data) > 0) {
    calib_individual <- calib_data %>%
      mutate(
        source = "calibration",
        # For "Measured vs. requested distance" plot (p1):
        # x-axis: rulerBasedEyesToFootCm (ruler-based/requested), y-axis: imageBasedEyesToPointCm (image-based/measured)
        p1_x = as.numeric(rulerBasedEyesToFootCm),
        p1_y = as.numeric(imageBasedEyesToPointCm),
        # For "Measured over requested distance" plot (p2):
        # x-axis: rulerBasedEyesToFootCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToFootCm
        p2_x = as.numeric(rulerBasedEyesToFootCm),
        p2_y = as.numeric(imageBasedEyesToPointCm) / as.numeric(rulerBasedEyesToFootCm)
      ) %>% 
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup()
  }
  
  # ===== COMBINE CALIBRATION AND CHECK DATA =====
  # Combine both calibration and check data for p1 (excluding jitter)
  distance_individual <- bind_rows(calib_individual, check_individual) %>%
    arrange(participant, source, measurement_order_within_participant)
  # Keep reference to calibration data for use in later plots (p6, p7, etc.)
  distance <- calib_data
  
  # ===== DEBUG: Show data sources for p1/p2 plots =====
  message("\n[DEBUG p1/p2 DATA SOURCES]")
  message("  check_data (checkJSON) input rows: ", nrow(check_data))
  message("  calib_data (TJSON) input rows: ", nrow(calib_data))
  message("  check_individual rows after filtering: ", nrow(check_individual))
  message("  calib_individual rows after filtering: ", nrow(calib_individual))
  message("  distance_individual combined rows: ", nrow(distance_individual))
  
  if (nrow(check_individual) > 0) {
    message("  CHECK data - p1_x (rulerBasedEyesToFootCm) non-NA: ", sum(!is.na(check_individual$p1_x)))
    message("  CHECK data - p1_y (imageBasedEyesToPointCm) non-NA: ", sum(!is.na(check_individual$p1_y)))
    message("  CHECK data columns: ", paste(names(check_individual), collapse = ", "))
  }
  
  if (nrow(calib_individual) > 0) {
    message("  CALIB data - p1_x (rulerBasedEyesToFootCm) non-NA: ", sum(!is.na(calib_individual$p1_x)))
    message("  CALIB data - p1_y (imageBasedEyesToPointCm) non-NA: ", sum(!is.na(calib_individual$p1_y)))
    message("  CALIB data columns: ", paste(names(calib_individual), collapse = ", "))
  } else {
    message("  CALIB data is EMPTY - checking calib_data columns: ", paste(names(calib_data), collapse = ", "))
    if ("rulerBasedEyesToFootCm" %in% names(calib_data)) {
      message("    calib_data rulerBasedEyesToFootCm non-NA: ", sum(!is.na(calib_data$rulerBasedEyesToFootCm)))
    } else {
      message("    calib_data MISSING rulerBasedEyesToFootCm column!")
    }
    if ("imageBasedEyesToPointCm" %in% names(calib_data)) {
      message("    calib_data imageBasedEyesToPointCm non-NA: ", sum(!is.na(calib_data$imageBasedEyesToPointCm)))
    } else {
      message("    calib_data MISSING imageBasedEyesToPointCm column!")
    }
  }
  
  if (nrow(distance_individual) > 0 && "source" %in% names(distance_individual)) {
    source_counts <- table(distance_individual$source)
    message("  distance_individual by source: ", paste(names(source_counts), source_counts, sep = "=", collapse = ", "))
  }
  message("[END DEBUG p1/p2 DATA SOURCES]\n")

  # Scale limits for p1 (Measured vs. requested distance plot)
  p1_min_val <- 5 * floor(min(c(distance_individual$p1_x, 
                                 distance_individual$p1_y), na.rm = TRUE) / 5) 
  p1_min_val = max(10, p1_min_val)
  p1_max_val <- 5 * ceiling(max(c(distance_individual$p1_x,
                                   distance_individual$p1_y), na.rm = TRUE) / 5)
  

  # Scale limits for p2 (Measured over requested distance plot)
  p2_x_min_val <- 5 * floor(min(distance_individual$p2_x, na.rm = TRUE) / 5)
  p2_x_min_val <- max(10, p2_x_min_val)
  p2_x_max_val <- 5 * ceiling(max(distance_individual$p2_x, na.rm = TRUE) / 5)
  p2_y_minFrac <- max(0.1, min(0.5, floor(min(distance_individual$p2_y, na.rm = TRUE) * 10) / 10))
  p2_y_maxFrac <- max(1.5, ceiling(max(distance_individual$p2_y, na.rm = TRUE) * 10) / 10)
  
  # Calculate mean and SD for p2 ratio
  p2_mean_fraction <- mean(distance_individual$p2_y, na.rm = TRUE)
  p2_sd_fraction <- sd(distance_individual$p2_y, na.rm = TRUE)
  p2_mean_formatted <- format(round(p2_mean_fraction, 3), nsmall = 3)
  p2_sd_formatted <- format(round(p2_sd_fraction, 3), nsmall = 3)
  
  # Plot 1: Measured vs. requested distance (CALIBRATION AND CHECK DATA, EXCLUDING JITTER)
  # x-axis: rulerBasedEyesToFootCm (ruler-based), y-axis: imageBasedEyesToPointCm (image-based)
  # IMPORTANT: Arrange by measurement_order_within_participant to preserve measurement order for line connections
  p1_data <- distance_individual %>%
    filter(!is.na(p1_x), !is.na(p1_y)) %>%
    arrange(participant, source, measurement_order_within_participant)
  
  # Plot 2 data: Measured over requested distance (CHECK DATA ONLY)
  # x-axis: rulerBasedEyesToFootCm, y-axis: imageBasedEyesToPointCm / rulerBasedEyesToFootCm
  # IMPORTANT: Arrange by measurement_order_within_participant to preserve measurement order for line connections
  p2_data <- distance_individual %>%
    filter(!is.na(p2_x), !is.na(p2_y)) %>%
    arrange(participant, source, measurement_order_within_participant)
  
  
  # Debug: show data breakdown for p1
  message("[DEBUG p1] p1_data rows: ", nrow(p1_data))
  if (nrow(p1_data) > 0 && "source" %in% names(p1_data)) {
    p1_source_counts <- table(p1_data$source)
    message("[DEBUG p1] p1_data by source: ", paste(names(p1_source_counts), p1_source_counts, sep = "=", collapse = ", "))
  }
  
  p1 <- NULL
  if (nrow(p1_data) > 0) {
    # Count calibration and check data points
    n_calib <- sum(p1_data$source == "calibration", na.rm = TRUE)
    n_check <- sum(p1_data$source == "check", na.rm = TRUE)
    
    # Text legend to appear below graphical legends
    text_legend_p1 <- "Lines connect successive measurements. Dashed line: y = x (perfect agreement)"
    
    p1 <- ggplot(p1_data, aes(x = p1_x, y = p1_y, color = participant, shape = source)) +
      geom_path(aes(group = interaction(participant, source)), linetype = "solid", alpha = 0.7) +
      geom_point(size = 2.5, stroke = 1) +
      # Shape scale: closed circle (16) for calibration, open circle (1) for check
      scale_shape_manual(name = "", values = c("calibration" = 16, "check" = 1),
                         labels = c("calibration" = "Calibration", "check" = "Check")) +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                          label = paste0('N=', n_distinct(p1_data$participant))) + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      scale_x_log10(
        limits = c(p1_min_val, p1_max_val),
        breaks = scales::log_breaks(n = 6),
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_y_log10(limits = c(p1_min_val, p1_max_val), breaks = scales::log_breaks(n=8), labels = scales::label_number(accuracy = 10)) + 
      scale_color_manual(values= colorPalette) + 
      guides(
        color = guide_legend(
          ncol = 3,
          title = "",
          order = 1,
          override.aes = list(size = 2),
          keywidth = unit(1.2, "lines"),
          keyheight = unit(0.8, "lines")
        ),
        shape = guide_legend(
          order = 2,
          title = text_legend_p1,
          title.position = "bottom",
          title.theme = element_text(size = 10, hjust = 0)
        )
      ) +
      coord_fixed() +  
      theme(
        # When multiple guides exist (color + shape), keep their left edges aligned
        legend.justification = "left",
        legend.box.just = "left"
      ) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
      labs(subtitle = 'imageBasedEyesToPointCm vs. rulerBasedEyesToPointCm',
           x = 'rulerBasedEyesToPointCm',
           y = 'imageBasedEyesToPointCm')
  }
 
  # Plot 2: Measured over requested distance (CALIBRATION AND CHECK DATA, EXCLUDING JITTER)
  # x-axis: requested distance, y-axis: measured / requested ratio
  # Debug: show data breakdown for p2
  message("[DEBUG p2] p2_data rows: ", nrow(p2_data))
  if (nrow(p2_data) > 0 && "source" %in% names(p2_data)) {
    p2_source_counts <- table(p2_data$source)
    message("[DEBUG p2] p2_data by source: ", paste(names(p2_source_counts), p2_source_counts, sep = "=", collapse = ", "))
  }
  
  p2 <- NULL
  if (nrow(p2_data) > 0) {
    # Count calibration and check data points
    n_calib_p2 <- sum(p2_data$source == "calibration", na.rm = TRUE)
    n_check_p2 <- sum(p2_data$source == "check", na.rm = TRUE)
    
    # Text legend to appear below graphical legends
    text_legend_p2 <- "Lines connect successive measurements. Dashed line: ratio = 1 (perfect agreement)"
    
    p2 <- ggplot(p2_data, aes(x = p2_x, y = p2_y, color = participant, shape = source)) +
      geom_path(aes(group = interaction(participant, source)), linetype = "solid", alpha = 0.7) +
      geom_point(size = 2.5, stroke = 1) +
      # Shape scale: closed circle (16) for calibration, open circle (1) for check
      scale_shape_manual(name = "", values = c("calibration" = 16, "check" = 1),
                         labels = c("calibration" = "Calibration", "check" = "Check")) +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                          label = paste0('N=', n_distinct(p2_data$participant), '\n',
                                         'Mean=', p2_mean_formatted, '\n',
                                         'SD=', p2_sd_formatted)) + 
      geom_hline(yintercept = 1, linetype = "dashed") +
      scale_x_log10(
        limits = c(p2_x_min_val, p2_x_max_val),
        breaks = scales::log_breaks(n = 6),
        expand = expansion(mult = c(0.05, 0.05)),
        labels = scales::label_number(accuracy = 1)
      ) + 
      scale_y_log10(limits = c(p2_y_minFrac, p2_y_maxFrac),
                    breaks = seq(0.6, 1.4, by = 0.1)) + 
      scale_color_manual(values= colorPalette) + 
      guides(
        color = guide_legend(
          ncol = 3,
          title = "",
          order = 1,
          override.aes = list(size = 2),
          keywidth = unit(1.2, "lines"),
          keyheight = unit(0.8, "lines")
        ),
        shape = guide_legend(
          order = 2,
          title = text_legend_p2,
          title.position = "bottom",
          title.theme = element_text(size = 10, hjust = 0)
        )
      ) +
      theme(
        # When multiple guides exist (color + shape), keep their left edges aligned
        legend.justification = "left",
        legend.box.just = "left"
      ) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
      labs(subtitle = 'imageBasedEyesToPointCm over rulerBasedEyesToPointCm',
           x = 'rulerBasedEyesToPointCm',
           y = 'imageBasedEyesToPointCm / rulerBasedEyesToPointCm')
  }

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
  
  message("[DEBUG p5b] TJSON rows: ", nrow(tjson_data), ", checkJSON rows: ", nrow(check_json_data))
  
  if (nrow(tjson_data) > 0 && nrow(check_json_data) > 0) {
    # Get median fOverWidth per participant from calibration (TJSON has fOverWidth directly)
    calib_fOverWidth <- tjson_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_calibration = median(fOverWidth, na.rm = TRUE), .groups = "drop")
    
    message("[DEBUG p5b] calib_fOverWidth: ", nrow(calib_fOverWidth), " participants")
    
    # Get median fOverWidth per participant from check (checkJSON now has fOverWidth directly)
    check_fOverWidth <- check_json_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_check = median(fOverWidth, na.rm = TRUE), .groups = "drop")
    
    message("[DEBUG p5b] check_fOverWidth: ", nrow(check_fOverWidth), " participants")
    
    # Join calibration and check data by participant
    plot_data_p5b <- calib_fOverWidth %>%
      inner_join(check_fOverWidth, by = "participant") %>%
      filter(!is.na(fOverWidth_calibration), !is.na(fOverWidth_check),
             is.finite(fOverWidth_calibration), is.finite(fOverWidth_check))
    
    message("[DEBUG p5b] plot_data_p5b after join: ", nrow(plot_data_p5b), " rows")
    
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
        guides(color = guide_legend(
          ncol = 3,
          title = "Dashed line: y = 1 (perfect agreement)",
          title.position = "bottom",
          title.theme = element_text(size = 10, hjust = 0),
          override.aes = list(size = 1.5),
          keywidth = unit(0.8, "lines"),
          keyheight = unit(0.6, "lines")
        )) +
        theme_classic() +
        theme(
          legend.position = "right",
          legend.box = "vertical",
          legend.justification = "left",
          legend.box.just = "left",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
        labs(subtitle = 'Focal length: calibration/check vs. check',
             x = 'fOverWidth: check',
             y = 'fOverWidth: calibration / check')
      }
    }
  }
  
  
  # Plot 6: Calibrated over mean factorVpxCm vs. spot diameter
  p6 <- NULL
  message("[DEBUG p6] distance columns: ", paste(names(distance), collapse=", "))
  message("[DEBUG p6] calibrateTrackDistanceIpdVpx in distance: ", "calibrateTrackDistanceIpdVpx" %in% names(distance))
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
            guides(color = guide_legend(
              ncol = 3,
              title = "Dashed line: y = 1 (perfect agreement). X-axis has 0.5% jitter",
              title.position = "bottom",
              title.theme = element_text(size = 10, hjust = 0),
              override.aes = list(size = 1.5),
              keywidth = unit(0.8, "lines"),
              keyheight = unit(0.6, "lines")
            )) +
            theme_classic() +
            theme(
              legend.position = "right",
              legend.box = "vertical",
              legend.justification = "left",
              legend.box.just = "left",
              legend.text = element_text(size = 6),
              legend.spacing.y = unit(0, "lines"),
              legend.key.size = unit(0.4, "cm"),
              plot.margin = margin(5, 5, 5, 5, "pt")
            ) +
            ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
            labs(subtitle = 'Calibrated over mean factorVpxCm vs. spot diameter',
                 x = 'Spot diameter (deg)',
                 y = 'factorVpxCm over geometric mean')
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
      # Use smaller bin width (0.005) to ensure nearby values stack instead of overlapping
      # With 0.01, R's round-to-even can put 0.985 and 0.9851 in different bins
      bin_w_linear <- 0.005
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
          title = "Each dot = median ratio for one participant",
          title.position = "bottom",
          title.theme = element_text(size = 10, hjust = 0),
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
        labs(
          subtitle = 'histogram of fOverWidth median(calibration)/median(check)',
          x = "median(calibration)/median(check)",
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
          legend.box.just = "left",
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
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
  
  # Plot 11: Histogram and scatter of fOverWidth for distance check sessions
  fOverWidth_hist <- NULL
  fOverWidth_scatter <- NULL
  fOverWidth_max_count <- 0
  
  # Debug: Check prerequisites for fOverWidth histogram/scatter
  has_check_factor <- "check_factor" %in% names(distanceCalibrationResults)
  has_camera <- "camera" %in% names(distanceCalibrationResults)
  check_factor_rows <- if (has_check_factor) nrow(distanceCalibrationResults$check_factor) else 0
  camera_rows <- if (has_camera) nrow(distanceCalibrationResults$camera) else 0
  
  if (!has_check_factor || !has_camera || check_factor_rows == 0 || camera_rows == 0) {
    message("[DEBUG fOverWidth_hist/scatter] Skipped: check_factor exists=", has_check_factor, 
            ", camera exists=", has_camera,
            ", check_factor rows=", check_factor_rows,
            ", camera rows=", camera_rows)
  }
  
  if (has_check_factor && has_camera && check_factor_rows > 0 && camera_rows > 0) {
    
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
    
    message("[DEBUG fOverWidth_hist/scatter] After filtering check_data: ", nrow(check_data), " rows with calibrateTrackDistanceCheckBool=TRUE")
    
    if (nrow(check_data) > 0) {
      # Use checkJSON data which now has fOverWidth directly
      check_json_data <- distanceCalibrationResults$checkJSON
      
      message("[DEBUG fOverWidth_hist/scatter] checkJSON has ", nrow(check_json_data), " rows, fOverWidth column exists: ", "fOverWidth" %in% names(check_json_data))
      
      if (nrow(check_json_data) > 0) {
        # Get median fOverWidth per participant from check data
        check_fOverWidth_median <- check_json_data %>%
          filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
          group_by(participant) %>%
          summarize(fOverWidth_check = median(fOverWidth, na.rm = TRUE), .groups = "drop")
        
        # Get calibration fOverWidth from camera table
        calib_fOverWidth <- distanceCalibrationResults$camera %>%
          select(PavloviaParticipantID, widthVpx, fOverWidth) %>%
          rename(fOverWidth_calib = fOverWidth)
        
        message("[DEBUG fOverWidth_hist/scatter] check_fOverWidth_median: ", nrow(check_fOverWidth_median), " rows")
        message("[DEBUG fOverWidth_hist/scatter] camera table: ", nrow(calib_fOverWidth), " rows")
        

        # Check data (open circles)
        check_plot_data <- check_data %>%
          left_join(calib_fOverWidth %>% select(PavloviaParticipantID, widthVpx), by = "PavloviaParticipantID") %>%
          left_join(check_fOverWidth_median, by = c("PavloviaParticipantID" = "participant")) %>%
          filter(!is.na(widthVpx), widthVpx > 0, !is.na(fOverWidth_check), is.finite(fOverWidth_check)) %>%
          mutate(
            participant = PavloviaParticipantID,
            fOverWidth = fOverWidth_check,
            source = "check"
          ) %>%
          select(participant, widthVpx, fOverWidth, source)
        
       
      } else {
        check_plot_data <- tibble()
      }
      
      if (nrow(check_plot_data) > 0) {
        # Histogram (dot-stacked) - use smaller bin width to properly stack nearby values
        bin_width <- 0.005
        check_plot_data <- check_plot_data %>%
          mutate(
            bin_center = round(fOverWidth / bin_width) * bin_width
          ) %>%
          arrange(bin_center, source, participant) %>%
          group_by(bin_center) %>%
          mutate(
            stack_position = row_number(),
            dot_y = stack_position
          ) %>%
          ungroup()
        
        fOverWidth_max_count <- max(check_plot_data$dot_y)
        # Ensure x-axis limits include all tick marks (0.4 to 1.6)
        x_min <- min(0.4, min(check_plot_data$fOverWidth, na.rm = TRUE) - 0.02)
        x_max <- max(1.6, max(check_plot_data$fOverWidth, na.rm = TRUE) + 0.02)
        
        p11 <- ggplot(check_plot_data, aes(x = fOverWidth)) +
          # Filled circles for calibration, open circles for check
          geom_point(aes(x = fOverWidth, y = dot_y, color = participant, shape = source, fill = after_scale(ifelse(shape == 16, color, NA))), 
                     size = 6, alpha = 0.85, stroke = 1.5) +
          # scale_shape_manual(values = c("calibration" = 16, "check" = 21),  # 16=filled, 21=open
          #                    labels = c("calibration" = "calibration", "check" = "check")) +
          scale_color_manual(values = colorPalette) +
          scale_y_continuous(
            limits = c(0, max(8, fOverWidth_max_count + 1)),
            expand = expansion(mult = c(0, 0.1)),
            breaks = function(x) seq(0, ceiling(max(x)), by = 1)
          ) +
          scale_x_continuous(limits = c(x_min, x_max),
                             breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6)) +
          ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                              label = paste0('N=', n_distinct(check_plot_data$participant)),
                              size = 4, family = "sans", fontface = "plain") +
          guides(
            color = "none",
            shape = "none"
          ) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
          labs(
            subtitle = 'Histogram of fOverWidth check',
            x = "fOverWidth",
            y = "Count"
          ) +
          theme_bw() +
          theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title = element_text(size = 14),
            axis.text = element_text(size = 12),
            axis.line = element_line(colour = "black"),
            axis.text.x = element_text(size = 12, angle = 0, hjust = 0.5, vjust = 1),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 12, hjust = 0, margin = margin(b = 5)),
            plot.title.position = "plot",
            plot.subtitle = element_text(size = 14, hjust = 0, margin = margin(t = 0, b = 5)),
            plot.caption = element_text(size = 11),
            plot.margin = margin(t = 0.3, r = 0.1, b = 0.1, l = 0.1, "inch"),
            strip.text = element_text(size = 14)
          )
        
        fOverWidth_hist <- list(
          plot = p11,
          height = 4.5  # Slightly taller to accommodate legend spacing
        )
        
        # Create calibration plot data from camera table
        calib_plot_data <- calib_fOverWidth %>%
          filter(!is.na(fOverWidth_calib), is.finite(fOverWidth_calib),
                 !is.na(widthVpx), widthVpx > 0) %>%
          mutate(
            participant = PavloviaParticipantID,
            fOverWidth = fOverWidth_calib,
            source = "calibration"
          ) %>%
          select(participant, widthVpx, fOverWidth, source)
        
        # Combine calibration and check data for scatter plot
        scatter_plot_data <- bind_rows(
          calib_plot_data,
          check_plot_data %>% select(participant, widthVpx, fOverWidth, source)
        )
        
        message("[DEBUG fOverWidth scatter] calib_plot_data rows: ", nrow(calib_plot_data), 
                ", check_plot_data rows: ", nrow(check_plot_data),
                ", combined rows: ", nrow(scatter_plot_data))
        
        median_ratio_fw <- median(scatter_plot_data$fOverWidth, na.rm = TRUE)
        p12 <- ggplot(scatter_plot_data, aes(x = as.numeric(widthVpx), y = fOverWidth)) +
          # Filled circles for calibration (16), open circles for check (1)
          geom_point(aes(color = participant, shape = source), size = 3, alpha = 0.85, stroke = 1) +
          geom_hline(yintercept = median_ratio_fw, linetype = "dashed") +
          ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                              label = paste0('N=', n_distinct(scatter_plot_data$participant),
                                             '\nmedian=', format(round(median_ratio_fw, 3), nsmall = 3)),
                              size = 3) +
          scale_color_manual(values = colorPalette) +
          scale_shape_manual(name = "", values = c("calibration" = 16, "check" = 1),
                             labels = c("calibration" = "Calibration", "check" = "Check")) +
          scale_x_continuous(expand = expansion(mult = c(0.02, 0.05)),
                             breaks = scales::pretty_breaks(n = 6),
                             labels = scales::label_number(accuracy = 1, big.mark = "")) +
          scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)),
                             breaks = scales::pretty_breaks(n = 6)) +
          guides(
            color = guide_legend(
              ncol = 3,
              title = "",
              order = 1,
              override.aes = list(size = 2)
            ),
            shape = guide_legend(
              order = 2,
              title = "Dashed line: median fOverWidth",
              title.position = "bottom",
              title.theme = element_text(size = 10, hjust = 0),
              override.aes = list(size = 3, color = "black", stroke = 1.5)
            )
          ) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
          labs(
            subtitle = 'fOverWidth vs max width',
            x = 'max width (px)',
            y = 'fOverWidth'
          ) +
          theme_classic() +
          theme(
            legend.position = "top",
            legend.box = "vertical",
            legend.justification = "left",
            legend.box.just = "left",
            legend.text = element_text(size = 6),
            legend.title = element_text(size = 8),
            legend.spacing.y = unit(0, "lines"),
            legend.key.size = unit(0.4, "cm"),
            plot.margin = margin(5, 5, 5, 5, "pt")
          )
        
        fOverWidth_scatter <- list(
          plot = p12,
          height = compute_auto_height(base_height = 7, n_items = n_distinct(scatter_plot_data$participant), per_row = 3, row_increase = 0.06)
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
          scale_color_manual(values = colorPalette) +
          scale_x_continuous(limits = c(x_axis_min, x_axis_max)) +
          scale_y_continuous(limits = c(y_axis_min, y_axis_max)) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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

  # NEW PLOT: Focal length: calibration 1 & 2 over check
  # Two points per participant/session, normalized by median(check)
  p15 <- NULL
  p15_data <- NULL
  check_data <- distanceCalibrationResults$checkJSON
  if (nrow(calib_data) > 0 && nrow(check_data) > 0 &&
      "fOverWidth" %in% names(calib_data) && "fOverWidth" %in% names(check_data)) {
    message("[DEBUG p15] Starting: calib rows=", nrow(calib_data),
            ", check rows=", nrow(check_data),
            ", calib fOverWidth finite rows=", sum(is.finite(calib_data$fOverWidth), na.rm = TRUE),
            ", check fOverWidth finite rows=", sum(is.finite(check_data$fOverWidth), na.rm = TRUE),
            ", calib participants=", n_distinct(calib_data$participant),
            ", check participants=", n_distinct(check_data$participant))

    check_median <- check_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      summarize(fOverWidth_check_median = median(fOverWidth, na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(fOverWidth_check_median), fOverWidth_check_median > 0)
    
    message("[DEBUG p15] check_median rows=", nrow(check_median),
            ", participants=", n_distinct(check_median$participant))
    if (nrow(check_median) == 0) {
      message("[DEBUG p15] check_median is empty (no finite fOverWidth in check data?)")
    }

    calib_first_two <- calib_data %>%
      filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>%
      group_by(participant) %>%
      mutate(calibration_order = row_number()) %>%
      filter(calibration_order %in% c(1, 2)) %>%
      ungroup() %>%
      inner_join(check_median, by = "participant") %>%
      mutate(
        ratio_over_check = fOverWidth / fOverWidth_check_median,
        calibration_order = factor(calibration_order, levels = c(1, 2),
                                   labels = c("Calibration 1", "Calibration 2"))
      ) %>%
      filter(is.finite(ratio_over_check))
    
    message("[DEBUG p15] calib_first_two rows=", nrow(calib_first_two),
            ", participants=", n_distinct(calib_first_two$participant),
            ", calib1 count=", sum(calib_first_two$calibration_order == "Calibration 1", na.rm = TRUE),
            ", calib2 count=", sum(calib_first_two$calibration_order == "Calibration 2", na.rm = TRUE))
    if (nrow(calib_first_two) == 0) {
      # Diagnose where rows were lost
      calib_finite <- calib_data %>% filter(!is.na(fOverWidth), is.finite(fOverWidth)) %>% distinct(participant)
      check_part <- check_median %>% distinct(participant)
      dropped <- calib_finite %>% anti_join(check_part, by = "participant")
      message("[DEBUG p15] Participants with finite calibration fOverWidth but no check median: ",
              nrow(dropped), " (showing up to 5): ",
              paste(head(dropped$participant, 5), collapse = ", "))
    } else {
      rng <- range(calib_first_two$ratio_over_check, na.rm = TRUE)
      message("[DEBUG p15] ratio_over_check range: [", signif(rng[1], 4), ", ", signif(rng[2], 4), "]")
    }

    if (nrow(calib_first_two) > 0) {
      # Set seed for reproducible jitter
      set.seed(42)
      p15_data <- calib_first_two %>%
        mutate(
          # Convert factor to numeric for jitter: "Calibration 1" = 1, "Calibration 2" = 2
          x_numeric = as.numeric(calibration_order),
          # Add horizontal jitter (0.15 units on each side)
          x_jitter = x_numeric + runif(n(), -0.15, 0.15)
        )

      p15 <- ggplot(p15_data, aes(x = x_jitter, y = ratio_over_check, color = participant, shape = calibration_order)) +
        geom_hline(yintercept = 1, linetype = "dashed", color = "gray50", linewidth = 1) +
        # Connect calibration 1 and 2 dots for each participant
        geom_line(aes(group = participant), alpha = 0.5, linewidth = 0.5) +
        geom_point(size = 4, alpha = 0.85, stroke = 1.2) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(p15_data$participant))) +
        scale_color_manual(values = colorPalette) +
        scale_shape_manual(values = c("Calibration 1" = 16, "Calibration 2" = 21)) +
        scale_x_continuous(
          breaks = c(1, 2),
          labels = c("Calibration 1", "Calibration 2"),
          limits = c(0.5, 2.5)
        ) +
        guides(
          color = guide_legend(
            ncol = 4,
            title = "",
            override.aes = list(size = 2),
            keywidth = unit(0.3, "cm"),
            keyheight = unit(0.3, "cm")
          ),
          shape = guide_legend(
            title = "Dashed line: y = 1 (perfect agreement). X-axis has horizontal jitter applied",
            title.position = "bottom",
            title.theme = element_text(size = 10, hjust = 0),
            override.aes = list(size = 3, color = "black")
          )
        ) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
        labs(
          subtitle = 'Focal length: calibration 1 & 2 over check',
          x = "",
          y = "fOverWidth calibration / median(check)"
        ) +
        theme_bw() +
        theme(
          legend.key.size  = unit(0.35, "cm"),
          legend.title = element_text(size = 6),
          legend.text = element_text(size = 7, margin = margin(t = 0, b = 0)),
          legend.box.margin = margin(0, 0, 6, 0, "pt"),
          legend.box.spacing = unit(0, "lines"),
          legend.spacing.y = unit(0.3, "lines"),
          legend.spacing.x = unit(0, "lines"),
          legend.key.height = unit(0.35, "cm"),
          legend.key.width  = unit(0.35, "cm"),
          legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
          legend.margin = margin(0, 0, 0, 0),
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = 'left',
          legend.box.just = "left",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          plot.title = element_text(size = 7, hjust = 0, margin = margin(b = 0)),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 12, hjust = 0, margin = margin(t = 0, b = 15)),
          plot.caption = element_text(size = 10),
          plot.margin = margin(t = 12, r = 8, b = 12, l = 8, unit = "pt"),
          strip.text = element_text(size = 14)
        ) 
    }
  } else {
    message("[DEBUG p15] Skipped: calib rows=", nrow(calib_data),
            ", check rows=", nrow(check_data),
            ", calib has fOverWidth=", "fOverWidth" %in% names(calib_data),
            ", check has fOverWidth=", "fOverWidth" %in% names(check_data))
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
    calibration_over_check_vs_check = list(plot = p5b, height = if (!is.null(p5b)) compute_auto_height(base_height = 7, n_items = n_distinct(distance_individual$participant), per_row = 3, row_increase = 0.06) else NULL),
    calibrated_over_mean_vs_spot = list(plot = p6, height = if (!is.null(p6)) compute_auto_height(base_height = 7, n_items = n_distinct(distance_individual$participant), per_row = 3, row_increase = 0.06) else NULL),
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
    fOverWidth_calibration12_over_check = list(
      plot = p15,
      height = if (!is.null(p15) && !is.null(p15_data)) {
        compute_auto_height(base_height = 7, n_items = n_distinct(p15_data$participant), per_row = 3, row_increase = 0.06)
      } else NULL
    ),
    fOverWidth_hist = fOverWidth_hist,
    fOverWidth_scatter = fOverWidth_scatter
  ))
}

plot_sizeCheck <- function(distanceCalibrationResults, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  message("[DEBUG plot_sizeCheck] distanceCalibrationResults is a list with elements: ", paste(names(distanceCalibrationResults), collapse=", "))
  message("[DEBUG plot_sizeCheck] sizeCheck rows: ", nrow(distanceCalibrationResults$sizeCheck))
  sizeCheck <- distanceCalibrationResults$sizeCheck
  statement <- distanceCalibrationResults$statement
  
  # Check if the data is empty FIRST before trying to select columns
  if (nrow(sizeCheck) == 0) {
    message("[DEBUG plot_sizeCheck] sizeCheck is empty - returning NULL")
    return(NULL)
  }
  
  # Only select from raw_pxPerCm if it has rows and expected columns
  raw_pxPerCm <- distanceCalibrationResults$raw_pxPerCm
  if (nrow(raw_pxPerCm) > 0 && all(c("participant", "pxPerCm", "requestedCm") %in% names(raw_pxPerCm))) {
    raw_pxPerCm <- raw_pxPerCm %>% select(participant, pxPerCm, requestedCm)
  } else {
    # Create empty tibble with expected structure
    raw_pxPerCm <- tibble(participant = character(), pxPerCm = numeric(), requestedCm = numeric())
  }
  
  ruler <-  sizeCheck %>%
    distinct(participant, rulerLength, rulerUnit) %>%
    filter(!is.na(rulerLength)) %>% 
    mutate(lengthCm = ifelse(rulerUnit == 'cm', rulerLength, rulerLength * 2.54))
  
  # Check for NA values after conversion
  if (sum(is.na(sizeCheck$SizeCheckEstimatedPxPerCm)) == nrow(sizeCheck) ||
      sum(is.na(sizeCheck$SizeCheckRequestedCm)) == nrow(sizeCheck)) {
    message("[DEBUG plot_sizeCheck] All values are NA - returning NULL")
    return(NULL)
  }
  
  # Average Estimated PxPerCm per Participant per Requested Size
  sizeCheck_avg <- sizeCheck %>%
    group_by(participant, SizeCheckRequestedCm) %>%
    summarize(
      avg_estimated = 10^median(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(requestedCm = SizeCheckRequestedCm)
  
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
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
    mutate(type = "calibration") %>%
    select(participant, avg_estimated, requestedCm, type)
  
  # Combine check and calibration data
  pixelDensity_data <- rbind(
    sizeCheck_avg %>% select(participant, avg_estimated, requestedCm) %>% mutate(type = "check"),
    calibration_data
  )
  
  ymin = max(5,floor(min(pixelDensity_data$avg_estimated) / 10 - 1) * 10)
  ymax = ceiling(max(pixelDensity_data$avg_estimated) / 10 + 1) * 10

  p1 <- ggplot(data=pixelDensity_data) + 
    # Solid lines for calibration points
    geom_line(data = pixelDensity_data %>% filter(type == "calibration"),
              aes(x = requestedCm, 
                  y = avg_estimated,
                  color = participant,
                  group = participant), 
              linetype = "solid",
              alpha = 0.7) +
    # Dashed lines for check points  
    geom_line(data = pixelDensity_data %>% filter(type == "check"),
              aes(x = requestedCm, 
                  y = avg_estimated,
                  color = participant,
                  group = participant), 
              linetype = "dashed",
              alpha = 0.7) +
    geom_point(aes(x = requestedCm, 
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
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
        guides(color = guide_legend(
          ncol = 4,
          title = "Dashed line: ratio = 1.0 (perfect accuracy). X-axis has 0.5% jitter",
          title.position = "bottom",
          title.theme = element_text(size = 10, hjust = 0),
          override.aes = list(size = 2),
          keywidth = unit(1.2, "lines"),
          keyheight = unit(0.8, "lines")
        )) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
        labs(subtitle = 'Check distance error vs. blindspot diameter',
             x = 'Blindspot diameter (deg)',
             y = 'Check measured / requested distance')
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
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 3, family = "sans", fontface = "plain") +
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
    message("[DEBUG plot_ipd_vs_eyeToFootCm] Early return: camera missing required columns (PavloviaParticipantID or widthVpx)")
    return(list(ipdOverWidth_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL),
                ipdOverWidth_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL)))
  }
  
  # Early return if TJSON is empty or missing required columns
  tjson <- distanceCalibrationResults$TJSON
  if (nrow(tjson) == 0 || !"ipdOverWidth" %in% names(tjson)) {
    message("[DEBUG plot_ipd_vs_eyeToFootCm] Early return: TJSON empty or missing ipdOverWidth column. TJSON columns: ", paste(names(tjson), collapse=", "))
    return(list(ipdOverWidth_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL),
                ipdOverWidth_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, height = NULL)))
  }
  
  # TJSON now has ipdOverWidth directly (no longer computed from ipdVpx)
  # Add measurement_order_within_participant for consistency with check data (calibration has only one point per participant typically)
  ipd_TJSON <- tjson %>%
    left_join(camera %>% select(PavloviaParticipantID, widthVpx), by = c("participant" = "PavloviaParticipantID")) %>%
    filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0) %>%
    group_by(participant) %>%
    mutate(measurement_order_within_participant = row_number()) %>%
    ungroup() %>%
    mutate(requestedEyesToFootCm = eyeToFootCm,  # During calibration, requested == measured
           ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = ipdOverWidth * requestedEyesToFootCm / ipdCm,
           # Compute factorVpxCm from fOverWidth (fOverWidth = f/widthVpx, so f = fOverWidth * widthVpx)
           factorVpxCm = fOverWidth * widthVpx * ipdCm,
           type = 'calibration') %>%
    select(participant, requestedEyesToFootCm, ipdOverWidth, ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm, factorVpxCm, type, measurement_order_within_participant)
  
  # For check data, use the actual requestedEyesToFootCm (derived from requestedEyesToPointCm)
  # checkJSON now has ipdOverWidth directly
  # IMPORTANT: Include measurement_order_within_participant and arrange by it to preserve measurement order
  checkjson <- distanceCalibrationResults$checkJSON
  ipd_checkJSON <- if (nrow(checkjson) > 0 && "ipdOverWidth" %in% names(checkjson)) {
    checkjson %>%
      filter(!is.na(ipdOverWidth), is.finite(ipdOverWidth), !is.na(ipdCm), is.finite(ipdCm), ipdCm > 0) %>%
      mutate(ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = ipdOverWidth * requestedEyesToFootCm / ipdCm,
             # Compute factorVpxCm for compatibility (fOverWidth * widthVpx * ipdCm) - need widthVpx
             type = 'check') %>%
      left_join(camera %>% select(PavloviaParticipantID, widthVpx), by = c("participant" = "PavloviaParticipantID")) %>%
      mutate(factorVpxCm = fOverWidth * widthVpx * ipdCm) %>%
      select(participant, requestedEyesToFootCm, ipdOverWidth, ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm, factorVpxCm, type, measurement_order_within_participant) %>%
      arrange(participant, measurement_order_within_participant)
  } else {
    tibble(participant = character(), requestedEyesToFootCm = numeric(), ipdOverWidth = numeric(),
           ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm = numeric(), factorVpxCm = numeric(), type = character(), measurement_order_within_participant = integer())
  }
  
  ipd_data <- rbind(ipd_TJSON, ipd_checkJSON)
  
  if (nrow(ipd_TJSON) == 0) {
    return(list(ipdOverWidth_vs_requestedEyesToFootCm = list(plot = NULL, 
                                          height = NULL),
                ipdOverWidth_times_requestedEyesToFootCm_vs_requestedEyesToFootCm = list(plot = NULL, 
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
  # IMPORTANT: Do NOT sort by distance values - preserve measurement order for line connections
  
  # Add linetype column to focal_curve_data for legend
  focal_curve_data <- focal_curve_data %>%
    mutate(line_type = "ipdCm × calibrationFOverWidth / requestedEyesToFootCm")
  
  p1 <- ggplot() +
    # Data lines: use geom_path to connect points in measurement order, not x-value order
    geom_path(data = ipd_data,
                  aes(x = requestedEyesToFootCm,
                      y = ipdOverWidth,
                  color = participant,
                  group = interaction(participant, type)),
              linewidth = 0.75, alpha = 0.8) +
    # Focal length curve (dotted) - one per participant, with linetype for legend
    geom_line(data = focal_curve_data,
              aes(x = requestedEyesToFootCm,
                  y = ipdOverWidth_focal,
                      color = participant,
                      group = participant,
                      linetype = line_type),
              linewidth = 0.75, alpha = 0.8) +
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
    # Linetype for focal length curve legend
    scale_linetype_manual(name = "", values = c("ipdCm × calibrationFOverWidth / requestedEyesToFootCm" = "dotted")) +
    scale_color_manual(values = colorPalette) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5),
                           keywidth = unit(0.8, "lines"), keyheight = unit(0.6, "lines")),
      shape = guide_legend(title = "", override.aes = list(size = 2)),
      linetype = guide_legend(title = "", keywidth = unit(2.5, "cm"), 
                              override.aes = list(linewidth = 1))
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.justification = "left",
      legend.box.just = "left",
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(subtitle = 'ipdOverWidth vs. requestedEyesToFootCm',
         x = 'requestedEyesToFootCm',
         y = 'ipdOverWidth')
  
  # Plot 1: (ipdVpx*requestedEyesToFootCm/ipdCm) vs. requestedEyesToFootCm
  # IMPORTANT: Do NOT sort by distance values - preserve measurement order for line connections
  
  # Add linetype column to focal_hline_data for legend
  focal_hline_data <- focal_hline_data %>%
    mutate(line_type = "calibrationFOverWidth")
  
  p2 <- ggplot() +
    # Data lines: use geom_path to connect points in measurement order, not x-value order
    geom_path(data = ipd_data,
              aes(x = requestedEyesToFootCm,
                  y = ipdOverWidth_times_requestedEyesToFootCm_over_ipdCm,
                  color = participant,
                  group = interaction(participant, type)),
              linewidth = 0.75, alpha = 0.8) +
    # Focal length horizontal line (dotted) - one per participant at y = calibrationFOverWidth
    geom_line(data = focal_hline_data,
              aes(x = requestedEyesToFootCm,
                  y = product_focal_over_width,
                      color = participant,
                      group = participant,
                      linetype = line_type),
              linewidth = 0.75, alpha = 0.8) +
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
    # Linetype for focal length line legend
    scale_linetype_manual(name = "", values = c("calibrationFOverWidth" = "dotted")) +
    scale_color_manual(values = colorPalette) +
    guides(
      color = guide_legend(ncol = 3, title = "", override.aes = list(size = 1.5),
                           keywidth = unit(0.8, "lines"), keyheight = unit(0.6, "lines")),
      shape = guide_legend(title = "", override.aes = list(size = 2)),
      linetype = guide_legend(title = "", keywidth = unit(2.5, "cm"), 
                              override.aes = list(linewidth = 1))
    ) +
        theme_classic() +
        theme(
          legend.position = "top",
          legend.box = "vertical",
          legend.justification = "left",
          legend.box.just = "left",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
    labs(subtitle = 'fOverWidth vs. requestedEyesToFootCm',
         x = 'requestedEyesToFootCm',
         y = 'fOverWidth')
  
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
# INCLUDES BOTH CALIBRATION (closed circles) AND CHECK DATA (open circles), EXCLUDING JITTER
plot_eyeToPointCm_vs_requestedEyesToFootCm <- function(distanceCalibrationResults) {
  
  check_data <- distanceCalibrationResults$checkJSON
  calib_data <- distanceCalibrationResults$TJSON
  
  if (nrow(check_data) == 0 && nrow(calib_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Prepare CHECK data - filter out jitter
  check_plot_data <- tibble()
  if (nrow(check_data) > 0) {
    check_plot_data <- check_data %>%
      # Filter out jitter: exclude rows where json_type contains "jitter" (case-insensitive)
      {if("json_type" %in% names(.)) filter(., is.na(json_type) | !grepl("jitter", json_type, ignore.case = TRUE)) else .} %>%
      mutate(
        plot_x = as.numeric(rulerBasedEyesToFootCm),
        plot_y = as.numeric(imageBasedEyesToPointCm),
        source = "check"
      ) %>%
      filter(is.finite(plot_x), is.finite(plot_y)) %>%
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup()
  }
  
  # Prepare CALIBRATION data
  calib_plot_data <- tibble()
  if (nrow(calib_data) > 0) {
    calib_plot_data <- calib_data %>%
      mutate(
        plot_x = as.numeric(rulerBasedEyesToFootCm),
        plot_y = as.numeric(imageBasedEyesToPointCm),
        source = "calibration"
      ) %>%
      filter(is.finite(plot_x), is.finite(plot_y)) %>%
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup()
  }
  
  # Combine calibration and check data
  plot_data <- bind_rows(calib_plot_data, check_plot_data) %>%
    arrange(participant, source, measurement_order_within_participant)
  
  # Debug output
  message("[DEBUG plot_eyeToPointCm_vs_requestedEyesToFootCm] check_plot_data rows: ", nrow(check_plot_data), 
          ", calib_plot_data rows: ", nrow(calib_plot_data), ", combined rows: ", nrow(plot_data))
  
  if (nrow(plot_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Count calibration and check data points
  n_calib <- sum(plot_data$source == "calibration", na.rm = TRUE)
  n_check <- sum(plot_data$source == "check", na.rm = TRUE)
  
  # Use the same x/y range so y=x is a true 45° line
  xy_vals <- c(plot_data$plot_x, plot_data$plot_y)
  xy_vals <- xy_vals[is.finite(xy_vals) & xy_vals > 0]
  shared_limits <- if (length(xy_vals) > 0) range(xy_vals, na.rm = TRUE) else NULL

  # Create the plot - CALIBRATION (closed circles) AND CHECK DATA (open circles)
  # Use geom_path instead of geom_line to connect points in measurement order, not x-value order
  p <- ggplot(plot_data, aes(x = plot_x, y = plot_y, color = participant, shape = source)) +
    geom_path(aes(group = interaction(participant, source)), linewidth = 0.5, alpha = 0.6) +
    geom_point(size = 2.5, alpha = 0.8, stroke = 1) +
    # Shape scale: closed circle (16) for calibration, open circle (1) for check
    scale_shape_manual(name = "", values = c("calibration" = 16, "check" = 1),
                       labels = c("calibration" = "Calibration", "check" = "Check")) +
    # Add identity line (if imageBasedEyesToPointCm == rulerBasedEyesToFootCm, perfect horizontal viewing)
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
    scale_x_log10(
      limits = shared_limits,
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    coord_fixed() +
    scale_y_log10(
      limits = shared_limits,
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                        label = paste0('N=', n_distinct(plot_data$participant))) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), 
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(
        ncol = 3,
        title = "",
        order = 1,
        override.aes = list(size = 1.5)
      ),
      shape = guide_legend(
        order = 2,
        title = "Dashed line: y = x (horizontal viewing)",
        title.position = "bottom",
        title.theme = element_text(size = 10, hjust = 0)
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.justification = "left",
      legend.box.just = "left",
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(
      subtitle = 'imageBasedEyesToPointCm vs. rulerBasedEyesToFootCm',
      x = 'rulerBasedEyesToFootCm (cm)',
      y = 'imageBasedEyesToPointCm (cm)'
    )
  
  p_height <- compute_auto_height(base_height = 7, n_items = n_distinct(plot_data$participant), per_row = 3, row_increase = 0.06)
  
  return(list(plot = p, height = p_height))
}

# Plot 3: eyesToFootCm (estimated via ipdOverWidth) vs. requestedEyesToFootCm
# This is the key plot showing: "Is the participant at the requested distance?"
# eyesToFootCm = fOverWidth * widthVpx * ipdCm / ipdVpx
# where fOverWidth is saved from calibration and ipdCm = 6.3 cm (standard IPD)
# INCLUDES BOTH CALIBRATION (closed circles) AND CHECK DATA (open circles), EXCLUDING JITTER
plot_eyesToFootCm_estimated_vs_requested <- function(distanceCalibrationResults) {
  
  # Get calibration data to extract fOverWidth per participant
  # TJSON now has fOverWidth directly (no longer computed from fVpx)
  tjson_data <- distanceCalibrationResults$TJSON
  check_data <- distanceCalibrationResults$checkJSON
  
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
  
  if (nrow(fOverWidth_per_participant) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # ===== PREPARE CHECK DATA =====
  check_plot_data <- tibble()
  if (nrow(check_data) > 0) {
    # Ensure we have an order column so paths connect in measurement order
    if (!"measurement_order_within_participant" %in% names(check_data)) {
      check_data <- check_data %>%
        group_by(participant) %>%
        mutate(measurement_order_within_participant = row_number()) %>%
        ungroup()
    }
    
    # Filter out jitter and join with calibration fOverWidth
    check_plot_data <- check_data %>%
      # Filter out jitter: exclude rows where json_type contains "jitter" (case-insensitive)
      {if("json_type" %in% names(.)) filter(., is.na(json_type) | !grepl("jitter", json_type, ignore.case = TRUE)) else .} %>%
      left_join(fOverWidth_per_participant, by = "participant") %>%
      filter(
        is.finite(requestedEyesToFootCm), requestedEyesToFootCm > 0,
        is.finite(ipdOverWidth), ipdOverWidth > 0,
        is.finite(fOverWidth_calibration)
      ) %>%
      mutate(
        eyesToFootCm_estimated = fOverWidth_calibration * ipdCm_standard / ipdOverWidth,
        source = "check"
      ) %>%
      filter(is.finite(eyesToFootCm_estimated), eyesToFootCm_estimated > 0)
  }
  
  # ===== PREPARE CALIBRATION DATA =====
  # For calibration, requestedEyesToFootCm = eyeToFootCm (the calibration establishes the baseline)
  calib_plot_data <- tibble()
  if (nrow(tjson_data) > 0) {
    calib_plot_data <- tjson_data %>%
      left_join(fOverWidth_per_participant, by = "participant") %>%
      filter(
        is.finite(eyeToFootCm), eyeToFootCm > 0,
        is.finite(ipdOverWidth), ipdOverWidth > 0,
        is.finite(fOverWidth_calibration)
      ) %>%
      mutate(
        requestedEyesToFootCm = eyeToFootCm,  # During calibration, requested == measured
        eyesToFootCm_estimated = fOverWidth_calibration * ipdCm_standard / ipdOverWidth,
        source = "calibration"
      ) %>%
      filter(is.finite(eyesToFootCm_estimated), eyesToFootCm_estimated > 0) %>%
      group_by(participant) %>%
      mutate(measurement_order_within_participant = row_number()) %>%
      ungroup()
  }
  
  # Combine calibration and check data
  plot_data <- bind_rows(calib_plot_data, check_plot_data) %>%
    arrange(participant, source, measurement_order_within_participant)
  
  # Debug output
  message("[DEBUG plot_eyesToFootCm_estimated_vs_requested] check_plot_data rows: ", nrow(check_plot_data), 
          ", calib_plot_data rows: ", nrow(calib_plot_data), ", combined rows: ", nrow(plot_data))

  if (nrow(plot_data) == 0) {
    return(list(plot = NULL, height = NULL))
  }
  
  # Count calibration and check data points
  n_calib <- sum(plot_data$source == "calibration", na.rm = TRUE)
  n_check <- sum(plot_data$source == "check", na.rm = TRUE)

  # Use the same x/y range so y=x is a true 45° line
  shared_limits <- range(c(plot_data$requestedEyesToFootCm, plot_data$eyesToFootCm_estimated), na.rm = TRUE)

  # Formula text for caption
  formula_text <- paste0("eyesToFootCm = fOverWidth_calib * ipdCm / ipdOverWidth (ipdCm=", ipdCm_standard, "cm)")
  
  # Create the plot - CALIBRATION (closed circles) AND CHECK DATA (open circles)
  p <- ggplot(plot_data, aes(x = requestedEyesToFootCm, y = eyesToFootCm_estimated, shape = source)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
    geom_point(aes(color = participant), size = 2.5, alpha = 0.8, stroke = 1) +
    geom_path(aes(color = participant, group = interaction(participant, source)), linewidth = 0.5, alpha = 0.6) +
    # Shape scale: closed circle (16) for calibration, open circle (1) for check
    scale_shape_manual(name = "", values = c("calibration" = 16, "check" = 1),
                       labels = c("calibration" = "Calibration", "check" = "Check")) +
    scale_x_log10(
      limits = shared_limits,
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    coord_fixed() +
    scale_y_log10(
      limits = shared_limits,
      breaks = scales::log_breaks(n = 6),
      labels = scales::label_number(accuracy = 1)
    ) +
    scale_color_manual(values = colorPalette) +
    ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                        label = paste0('N=', n_distinct(plot_data$participant))) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                        label = distanceCalibrationResults$statement) +
    guides(
      color = guide_legend(
        ncol = 3,
        title = "",
        order = 1,
        override.aes = list(size = 1.5)
      ),
      shape = guide_legend(
        order = 2,
        title = paste0("Dashed line: y = x (perfect agreement). ", formula_text),
        title.position = "bottom",
        title.theme = element_text(size = 10, hjust = 0)
      )
    ) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "vertical",
      legend.text = element_text(size = 6),
      legend.justification = "left",
      legend.box.just = "left",
      legend.spacing.y = unit(0, "lines"),
      legend.key.size = unit(0.4, "cm"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    ) +
    labs(
      subtitle = 'imageBasedEyesToFootCm vs. rulerBasedEyesToFootCm',
      x = 'rulerBasedEyesToFootCm',
      y = 'imageBasedEyesToFootCm'
    )

  p_height <- compute_auto_height(base_height = 7, n_items = n_distinct(plot_data$participant), per_row = 3, row_increase = 0.06)
  
  return(list(plot = p, height = p_height))
}
