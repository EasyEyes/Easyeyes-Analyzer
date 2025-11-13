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

# Helper to compute log-scale axis breaks that grow by ~growth and snap to 10s
compute_log10_breaks <- function(min_val, max_val, growth = 1.1, skip = c(60, 80, 100, 110)) {
  lo_break <- max(10, floor(min_val / 10) * 10)
  hi_break <- ceiling(max_val / 10) * 10
  breaks_seq <- c()
  cur_break <- lo_break
  while (is.finite(cur_break) && cur_break <= hi_break) {
    breaks_seq <- c(breaks_seq, cur_break)
    next_break <- ceiling((cur_break * growth) / 10) * 10
    if (!is.finite(next_break) || next_break <= cur_break) {
      next_break <- cur_break + 10
    }
    cur_break <- next_break
  }
  breaks_seq[!(breaks_seq %in% skip)]
}

# Safe helpers for building short, clean, single-string labels
safe_first <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != "" & x != "NA"]
  if (length(x)) x[1] else ""
}

safe_first_num <- function(x, digits = NULL) {
  x <- suppressWarnings(as.numeric(x))
  x <- x[is.finite(x)]
  if (!length(x)) return("")
  if (is.null(digits)) as.character(x[1]) else format(round(x[1], digits), nsmall = digits)
}

make_statement <- function(df) {
  paste(
    paste0("_calibrateTrackDistance = ",               safe_first(df[["_calibrateTrackDistance"]])),
    paste0("_calibrateTrackDistancePupil = ",          safe_first(df[["_calibrateTrackDistancePupil"]])),
    paste0("_calibrateTrackDistanceAllowedRatio = ",   safe_first(df[["_calibrateTrackDistanceAllowedRatio"]])),
    paste0("_calibrateTrackDistanceShowLengthBool = ", safe_first(df[["_calibrateTrackDistanceShowLengthBool"]])),
    paste0("_calibrateTrackDistanceTimes = ",          safe_first(df[["_calibrateTrackDistanceTimes"]])),
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
  keys <- c(
    "_calibrateTrackDistance",
    "_calibrateTrackDistancePupil",
    "_calibrateTrackDistanceAllowedRatio",
    "_calibrateTrackDistanceShowLengthBool",
    "_calibrateTrackDistanceTimes",
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

# Helper to get first non-NA calibration parameter value
get_first_non_na <- function(values) {
  non_na_values <- values[!is.na(values)]
  if (length(non_na_values) > 0) {
    return(non_na_values[1])
  }
  return(NA)
}

get_cameraResolutionXY <- function(data_list) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)
  for (i in 1:length(data_list)) {
    factorVpxCmList = c()
    factorVpxCm = ""
    if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {

      distanceCalibration <- fromJSON(sort(data_list[[i]]$distanceCalibrationJSON)[1])

     for (j in 1:length(distanceCalibration)) {
       # Accept both new and legacy field names; prefer new if present
       val <- NULL
       if (!is.null(distanceCalibration[[j]]$factorVpxCm)) {
         val <- distanceCalibration[[j]]$factorVpxCm
       } else if (!is.null(distanceCalibration[[j]]$factorCameraPxCm)) {
         val <- distanceCalibration[[j]]$factorCameraPxCm
       }
       if (!is.null(val)) {
         factorVpxCmList = c(factorVpxCmList, val)
       }
     }
      # Take the average of the last two factorVpxCm values (fallback to single if only one)
      factorVpxCmVals <- suppressWarnings(as.numeric(factorVpxCmList))
      factorVpxCmVals <- factorVpxCmVals[!is.na(factorVpxCmVals)]
      if (length(factorVpxCmVals) >= 2) {
        factorVpxCm <- mean(tail(factorVpxCmVals, 2))
      } else if (length(factorVpxCmVals) == 1) {
        factorVpxCm <- factorVpxCmVals[1]
      } else {
        factorVpxCm <- NA_real_
      }
    }

    t <- data_list[[i]] %>%
      mutate(factorVpxCm = !!factorVpxCm)

    # Skip iteration if participant column doesn't exist or has no data
    if (!"participant" %in% names(t) || all(is.na(t$participant)) || all(t$participant == "")) {
      next
    }

    t <- t %>%
      select(participant, `_calibrateTrackDistance`,
             `_calibrateTrackDistancePupil`, factorVpxCm,
             cameraResolutionXY) %>%
      rename(PavloviaParticipantID = participant) %>%
      distinct() %>%
      filter(!is.na(cameraResolutionXY), cameraResolutionXY != "",
             !is.na(factorVpxCm))
    if (nrow(t) > 0) {
      df <- rbind(df, t)
    }
  }

  return(df %>% distinct())
}

get_distanceCheck_factorVpxCm <- function(data_list) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)
  
  for (i in 1:length(data_list)) {
    if ("distanceCheckJSON" %in% names(data_list[[i]])) {
      # Skip iteration if participant column doesn't exist or has no data
      if (!"participant" %in% names(data_list[[i]]) || 
          all(is.na(data_list[[i]]$participant)) || 
          all(data_list[[i]]$participant == "")) {
        next
      }
      
      tryCatch({
        distanceCheck <- fromJSON(sort(data_list[[i]]$distanceCheckJSON)[1])
        
        # Extract measuredFactorVpxCm (new) or measuredFactorCameraPxCm (legacy) from distanceCheckJSON
        measured_vals <- NULL
        if (!is.null(distanceCheck$measuredFactorVpxCm)) {
          measured_vals <- distanceCheck$measuredFactorVpxCm
        } else if (!is.null(distanceCheck$measuredFactorCameraPxCm)) {
          measured_vals <- distanceCheck$measuredFactorCameraPxCm
        }
        if (!is.null(measured_vals)) {
          measuredFactorVpxCm <- measured_vals
          
          # Calculate median of the checking measurements
          measuredFactorVpxCmVals <- suppressWarnings(as.numeric(measuredFactorVpxCm))
          measuredFactorVpxCmVals <- measuredFactorVpxCmVals[!is.na(measuredFactorVpxCmVals)]
          
          if (length(measuredFactorVpxCmVals) > 0) {
            medianFactorVpxCm <- median(measuredFactorVpxCmVals)
            
            t <- data_list[[i]] %>%
              mutate(medianFactorVpxCm = medianFactorVpxCm) %>%
              select(participant, medianFactorVpxCm) %>%
              rename(PavloviaParticipantID = participant) %>%
              distinct() %>%
              filter(!is.na(medianFactorVpxCm))
            
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
  return(df %>% distinct())
}

get_merged_participant_distance_info <- function(data_or_results, participant_info) {
  # Get camera resolution data (support both raw data_list and precomputed results)
  camera_data <- if (is.list(data_or_results) && "camera" %in% names(data_or_results)) {
    data_or_results$camera
  } else {
    get_cameraResolutionXY(data_or_results)
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
      # Parse camera height px from cameraResolutionXY string; take the first numeric entry
      cameraResolution_clean = ifelse(is.na(cameraResolutionXY) | cameraResolutionXY == "", NA_character_, gsub("\\[|\\]|\"|'", "", cameraResolutionXY)),
      cameraResolution_split = ifelse(is.na(cameraResolution_clean), NA, cameraResolution_clean),
      cameraResolution_split = ifelse(is.na(cameraResolution_split), list(NA_character_), strsplit(cameraResolution_clean, "[,xX ]+")),
      cameraHeightPx = suppressWarnings(as.numeric(sapply(cameraResolution_split, function(v) if (length(v) >= 1) trimws(v[1]) else NA_character_))),
      # Coerce to numeric to avoid non-numeric errors during division
      factorVpxCm = suppressWarnings(as.numeric(factorVpxCm)),
      cameraHeightPx = suppressWarnings(as.numeric(cameraHeightPx)),
      ratio_tmp = ifelse(!is.na(factorVpxCm) & !is.na(cameraHeightPx) & cameraHeightPx != 0,
                         factorVpxCm / cameraHeightPx, NA_real_),
      ratio_round = round(ratio_tmp, 1),
      `factorVpxCm/cameraHeightPx` = ifelse(is.na(ratio_round), NA_character_, format(ratio_round, nsmall = 1))
    ) %>%
    select(-cameraResolution_clean, -cameraResolution_split, -ratio_tmp, -ratio_round, -cameraHeightPx) %>%
    arrange(ok_priority, PavloviaParticipantID) %>%
    select(-ok_priority)
  
  
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
      check_factor = tibble(),
      camera = tibble(),
      statement = ""
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
    return(data_list)
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
      check_factor = tibble(),
      camera = tibble(),
      statement = ""
    ))
  }
  
  filtered_data_list <- list()
  for (i in 1:length(data_list)) {
    filtered_data <- data_list[[i]] %>%
      filter(participant %in% valid_participants)
    
    if (nrow(filtered_data) > 0) {
      filtered_data_list[[length(filtered_data_list) + 1]] <- filtered_data
    }
  }
  
  # Compute all derived datasets once
  sizeCheck <- get_sizeCheck_data(filtered_data_list)
  distance <- get_measured_distance_data(filtered_data_list)
  eye_feet <- get_eye_feet_position_data(filtered_data_list)
  feet_calib <- get_foot_position_during_calibration_data(filtered_data_list)
  check_factor <- get_distanceCheck_factorVpxCm(filtered_data_list)
  camera <- get_cameraResolutionXY(filtered_data_list)
  blindspot <- get_calibrateTrackDistanceBlindspotDiameterDeg(filtered_data_list)
  statement <- make_statement(sizeCheck)
  
  return(list(
    filtered = filtered_data_list,
    sizeCheck = sizeCheck,
    distance = distance,
    eye_feet = eye_feet,
    feet_calib = feet_calib,
    check_factor = check_factor,
    camera = camera,
    blindspot = blindspot,
    statement = statement
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

get_eye_feet_position_data <- function(data_list) {
  
  df <- tibble()
  if(length(data_list) == 0) {
    return(df)
  }
  
  for (i in 1:length(data_list)) {
    available_cols <- names(data_list[[i]])

    # Check if we have the required columns
    if (!("calibrateTrackDistanceEyeFeetXYPx" %in% available_cols)) {
      next
    }
    
    
    # First, try to parse calibrateTrackDistanceEyeFeetXYPx to see if we have valid coordinate data
    tryCatch({
      
      # Get raw data first
      t_raw <- data_list[[i]] %>%
        select(experiment,
               participant,
               `_calibrateTrackDistanceAllowedRatio`,
               `_calibrateTrackDistanceShowLengthBool`,
               `_calibrateTrackDistanceTimes`,
               calibrateScreenSizeAllowedRatio,
               calibrateScreenSizeTimes,
               `_calibrateTrackDistance`,
               `_calibrateTrackDistancePupil`,
               viewingDistanceWhichEye,
               viewingDistanceWhichPoint,
               pxPerCm,
               screenWidthPx,
               screenHeightPx,
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
               `viewingDistanceWhichPoint` = get_first_non_na(`viewingDistanceWhichPoint`)
                ) %>% 
        distinct()
      
      # Check if we have the required data
      if (nrow(t_raw) == 0 || 
          all(is.na(t_raw$calibrateTrackDistanceMeasuredCm)) ||
          all(is.na(t_raw$calibrateTrackDistanceRequestedCm)) ||
          all(is.na(t_raw$calibrateTrackDistanceEyeFeetXYPx))) {
        return(tibble())
      }
      
      
      # Parse the eye feet coordinates first
      eye_feet_raw <- t_raw$calibrateTrackDistanceEyeFeetXYPx[1]

      # Manual parsing approach - extract all numbers
      coords_str <- gsub("\\[|\\]", "", eye_feet_raw)

      coords_numbers <- as.numeric(unlist(strsplit(coords_str, ",")))
      coords_numbers <- coords_numbers[!is.na(coords_numbers)]
      
      # Check if we have the right number of coordinates (4 per measurement: left_x, left_y, right_x, right_y)
      n_coord_measurements <- length(coords_numbers) / 4

      if (length(coords_numbers) %% 4 != 0) {
        return(tibble())  # Return empty if parsing fails
      }

      # Now filter the data based on successful coordinate parsing
      t <- t_raw %>%
        filter(!is.na(calibrateTrackDistanceMeasuredCm),
               !is.na(calibrateTrackDistanceRequestedCm),
               !is.na(calibrateTrackDistanceEyeFeetXYPx),
               calibrateTrackDistanceMeasuredCm != '',
               calibrateTrackDistanceRequestedCm != '',
               calibrateTrackDistanceEyeFeetXYPx != '')
      
      
      if (nrow(t) > 0) {
        # Process the measured and requested distances (these are JSON arrays)
        t <- t %>%
          mutate(
            calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
            calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm)
          ) %>%
          # Parse the measured and requested distances
          mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","), 
                 requested_list = strsplit(calibrateTrackDistanceRequestedCm, ",")) %>%
          unnest(c(measured_list, requested_list)) %>%
          mutate(
            calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
            calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
            measurement_index = row_number()
          ) %>%
          select(-measured_list, -requested_list)
        

        # Create coordinate data frame
        coords_df <- tibble(
          measurement_idx = rep(1:n_coord_measurements, each = 4),
          coord_type = rep(c("left_x", "left_y", "right_x", "right_y"), n_coord_measurements),
          coord_value = coords_numbers
        ) %>%
          pivot_wider(names_from = coord_type, values_from = coord_value)
        
        
        # Match measurements - if we have more coordinates than distance measurements, trim
        if (n_coord_measurements > nrow(t)) {
          coords_df <- coords_df[1:nrow(t), ]
        } else if (n_coord_measurements < nrow(t)) {
          # Pad with NAs or truncate distance data
          t <- t[1:n_coord_measurements, ]
        }
        
        # Combine data
        t <- cbind(t, coords_df) %>%
          select(-measurement_idx) %>%
          mutate(
            # Calculate average eye position (as requested)
            avg_eye_x_px = (left_x + right_x) / 2,
            avg_eye_y_px = (left_y + right_y) / 2,
            # Calculate measured over requested ratio
            distance_ratio = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
          )
        
        sample_data <- t %>% 
          select(measurement_index, left_x, left_y, right_x, right_y, 
                 avg_eye_x_px, avg_eye_y_px, distance_ratio) %>%
          head(3)
      }
      
    }, error = function(e) {
      return(tibble())
    })

    # Convert pixels to cm if pxPerCm is available (only if we have data)
    if (exists("t") && nrow(t) > 0) {
      if ("pxPerCm" %in% names(t) && !all(is.na(t$pxPerCm))) {
        t <- t %>%
          mutate(
            pxPerCm = as.numeric(pxPerCm),
            avg_eye_x_cm = avg_eye_x_px / pxPerCm,
            avg_eye_y_cm = avg_eye_y_px / pxPerCm
          )
      } else {
        # If no pxPerCm, assume a typical value (e.g., 37.8 px/cm for 96 DPI)
        default_pxPerCm <- 37.8
        t <- t %>%
          mutate(
            pxPerCm = default_pxPerCm,
            avg_eye_x_cm = avg_eye_x_px / default_pxPerCm,
            avg_eye_y_cm = avg_eye_y_px / default_pxPerCm
          )
      }
      
      final_sample <- t %>%
        select(measurement_index, avg_eye_x_px, avg_eye_y_px, avg_eye_x_cm, avg_eye_y_cm, 
               distance_ratio, calibrateTrackDistanceMeasuredCm, calibrateTrackDistanceRequestedCm) %>%
        head(3)
      
      # Add a data_list_index to help distinguish sessions
      t <- t %>%
        mutate(data_list_index = i)
      
      df <- rbind(df, t)
    }
  }
  
  # Add session detection based on data_list_index for cases where experiment names are the same
  if (nrow(df) > 0) {
    df <- df %>%
      mutate(
        session_from_data_index = paste0("Session_", data_list_index)
    )
  }
  
  
  return(df)
}

get_foot_position_during_calibration_data <- function(data_list) {
  df <- tibble()
  if(length(data_list) == 0) {
    return(df)
  }

  for (i in 1:length(data_list)) {
    available_cols <- names(data_list[[i]])

    # Check if we have the required columns - using distanceCalibrationJSON
    if (!("distanceCalibrationJSON" %in% available_cols)) {
      next
    }

    t <- data_list[[i]] %>%
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
             pxPerCm, 
             screenWidthPx, 
             screenHeightPx,
             calibrateTrackDistanceMeasuredCm, 
             calibrateTrackDistanceRequestedCm,
             distanceCalibrationJSON) %>%
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

    # Parse distanceCalibrationJSON first before filtering
    tryCatch({
      distanceCalibration <- fromJSON(sort(t$distanceCalibrationJSON)[1])

      # Extract eye foot coordinates from each calibration
      eye_feet_coords <- list()
      for (j in 1:length(distanceCalibration)) {
        cal <- distanceCalibration[[j]]
        if (!is.null(cal$leftEyeFootXYPx) && !is.null(cal$rightEyeFootXYPx)) {
          eye_feet_coords[[length(eye_feet_coords) + 1]] <- list(
            left_x = cal$leftEyeFootXYPx[1],
            left_y = cal$leftEyeFootXYPx[2],
            right_x = cal$rightEyeFootXYPx[1],
            right_y = cal$rightEyeFootXYPx[2]
          )
        }
      }

      if (length(eye_feet_coords) == 0) {
        next  # Skip this iteration if no eye foot coordinates found
      }

      # Create coordinate data frame
      coords_df <- tibble(
        measurement_idx = 1:length(eye_feet_coords),
        left_x = sapply(eye_feet_coords, function(x) x$left_x),
        left_y = sapply(eye_feet_coords, function(x) x$left_y),
        right_x = sapply(eye_feet_coords, function(x) x$right_x),
        right_y = sapply(eye_feet_coords, function(x) x$right_y)
      )

    }, error = function(e) {
      next  # Skip this iteration if JSON parsing fails
    })

    # Now filter after JSON parsing
    t <- t %>%
      filter(!is.na(calibrateTrackDistanceMeasuredCm),
             !is.na(calibrateTrackDistanceRequestedCm),
             calibrateTrackDistanceMeasuredCm != '',
             calibrateTrackDistanceRequestedCm != '')

    if (nrow(t) > 0) {
      # Process the measured and requested distances (these are JSON arrays)
      t <- t %>%
        mutate(
          calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
          calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm)
        ) %>%
        # Parse the measured and requested distances
        mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","), 
               requested_list = strsplit(calibrateTrackDistanceRequestedCm, ",")) %>%
        unnest(c(measured_list, requested_list)) %>%
        mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
          calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
          measurement_index = row_number()
        ) %>%
        select(-measured_list, -requested_list)

      # Match measurements - if we have more coordinates than distance measurements, trim
      n_measurements <- nrow(coords_df)
      if (n_measurements > nrow(t)) {
        coords_df <- coords_df[1:nrow(t), ]
      } else if (n_measurements < nrow(t)) {
        # Pad with NAs or truncate distance data
        t <- t[1:n_measurements, ]
      }

      # Combine data
      t <- cbind(t, coords_df) %>%
        select(-measurement_idx) %>%
        mutate(
          # Calculate average eye position (as requested)
          avg_eye_x_px = (left_x + right_x) / 2,
          avg_eye_y_px = (left_y + right_y) / 2,
          # Calculate measured over requested ratio
          distance_ratio = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
        )

      # Convert pixels to cm if pxPerCm is available
      if ("pxPerCm" %in% names(t) && !all(is.na(t$pxPerCm))) {
        t <- t %>%
          mutate(
            pxPerCm = as.numeric(pxPerCm),
            avg_eye_x_cm = avg_eye_x_px / pxPerCm,
            avg_eye_y_cm = avg_eye_y_px / pxPerCm
          )
      } else {
        # If no pxPerCm, assume a typical value (e.g., 37.8 px/cm for 96 DPI)
        default_pxPerCm <- 37.8
        t <- t %>%
          mutate(
            pxPerCm = default_pxPerCm,
            avg_eye_x_cm = avg_eye_x_px / default_pxPerCm,
            avg_eye_y_cm = avg_eye_y_px / default_pxPerCm
          )
      }

      # Add a data_list_index to help distinguish sessions
      t <- t %>%
        mutate(data_list_index = i)

      df <- rbind(df, t)
    }
  }

  # Add session detection based on data_list_index for cases where experiment names are the same
  if (nrow(df) > 0) {
    df <- df %>%
      mutate(
        session_from_data_index = paste0("Session_", data_list_index)
    )
  }
  
  return(df)
}

plot_eye_feet_position <- function(distanceCalibrationResults) {
  
  eye_feet_data <- distanceCalibrationResults$eye_feet

  if (nrow(eye_feet_data) == 0) {
    return(NULL)
  }

  # Filter out rows with invalid coordinates
  eye_feet_data <- eye_feet_data %>%
    filter(!is.na(avg_eye_x_px), !is.na(avg_eye_y_px),
           !is.na(avg_eye_x_cm), !is.na(avg_eye_y_cm), !is.na(distance_ratio))
  

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
          ncol = 3,  # Keep 3 columns to stay within plot width
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
        subtitle = "Measured over requested distance vs. foot position",
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
    return(NULL)
  })

  # Return the plot or NULL if creation failed
  if(exists("p") && !is.null(p)) {
    return(p)
  } else {
    return(NULL)
  }
}

get_calibrateTrackDistanceBlindspotDiameterDeg <- function(data_list) {
  # Get _calibrateTrackDistanceBlindspotDiameterDeg from distanceCalibrationJSON
  blindspot_diameter_data <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(blindspot_diameter_data)

  for (i in 1:length(data_list)) {
    if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {
      t <- data_list[[i]] %>%
        select(participant, distanceCalibrationJSON) %>%
        filter(!is.na(distanceCalibrationJSON), distanceCalibrationJSON != "")

      if (nrow(t) > 0) {
        tryCatch({
          # Parse distanceCalibrationJSON to extract spotDeg
          distanceCalibration <- fromJSON(sort(t$distanceCalibrationJSON)[1])

          # Extract spotDeg from each calibration
          spot_degrees <- c()
          for (j in 1:length(distanceCalibration)) {
            cal <- distanceCalibration[[j]]
            if (!is.null(cal$spotDeg)) {
              spot_degrees <- c(spot_degrees, as.numeric(cal$spotDeg))
            }
          }

          if (length(spot_degrees) > 0) {
            # Use the first (or average) spotDeg value
            spot_deg <- spot_degrees[1]  # or mean(spot_degrees) if you want average

            blindspot_entry <- t[1,] %>%  # Take first row for participant info
              mutate(`_calibrateTrackDistanceBlindspotDiameterDeg` = spot_deg) %>%
              select(participant, `_calibrateTrackDistanceBlindspotDiameterDeg`)

            blindspot_diameter_data <- rbind(blindspot_diameter_data, blindspot_entry)
          }
        }, error = function(e) {
          # Skip if JSON parsing fails
        })
      }
    }
  }

  return(blindspot_diameter_data %>% distinct())
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
  distance <- distanceCalibrationResults$distance
  sizeCheck <- distanceCalibrationResults$sizeCheck
  statement <- distanceCalibrationResults$statement
  if (nrow(distance) == 0) {return(NULL)}
  
  if (nrow(sizeCheck) > 0) {
    sdLogDensity_data <- sizeCheck %>%
      group_by(participant) %>%
      summarize(
        sdLogDensity = sd(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(sdLogDensity)) %>% 
      mutate(reliableBool = (sdLogDensity <= calibrateTrackDistanceCheckLengthSDLogAllowed))
  } else {
    sdLogDensity_data <- tibble()
  }
  
  # Prepare individual measurements for plotting (not averaged)
  distance_individual <- distance %>%
    mutate(
      # Add consistent logarithmic horizontal jitter to x-axis variable (unbiased for log scales)
      calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
    )

  if (nrow(sdLogDensity_data) > 0) {
    distance_individual <- distance_individual %>% 
      left_join(sdLogDensity_data, by = "participant")
  } else {
    distance_individual <- distance_individual %>% mutate(reliableBool = TRUE)
  }
  
  # Calculate the ratio Y/X for the second plot
  distance_individual <- distance_individual %>%
    mutate(
      credit_card_fraction = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
    )

  # Calculate mean and SD of the ratio for reporting
  mean_fraction <- mean(distance_individual$credit_card_fraction, na.rm = TRUE)
  sd_fraction <- sd(distance_individual$credit_card_fraction, na.rm = TRUE)
  
  # Format for display
  mean_formatted <- format(round(mean_fraction, 3), nsmall = 3)
  sd_formatted <- format(round(sd_fraction, 3), nsmall = 3)

     # Use appropriate scale limits for cleaner axis display - use JITTERED values
  min_val <- 5 * floor(min(c(distance_individual$calibrateTrackDistanceRequestedCm_jitter, 
                              distance_individual$calibrateTrackDistanceMeasuredCm), na.rm = TRUE) / 5) 
  min_val = max(10, min_val)
  max_val <- 5 * ceiling(max(c(distance_individual$calibrateTrackDistanceRequestedCm_jitter,
                               distance_individual$calibrateTrackDistanceMeasuredCm), na.rm = TRUE) / 5)
  
  # Precompute log-scale-aware breaks using helper
  breaks_seq_10 <- compute_log10_breaks(min_val, max_val, growth = 1.1)
  
  # Calculate dynamic y-axis limits for the fraction plot based on actual data
  minFrac <- max(0.1, min(0.5, floor(distance_individual$credit_card_fraction * 10) / 10))
  maxFrac <- max(1.5, ceiling(distance_individual$credit_card_fraction * 10) / 10)
  
  # Plot 1: Credit-card-measured vs requested distance (INDIVIDUAL MEASUREMENTS)
  distance_individual <- distance_individual %>%
    arrange(participant, order) %>%  # Ensure proper ordering
    filter(!is.na(calibrateTrackDistanceRequestedCm_jitter),
           !is.na(calibrateTrackDistanceMeasuredCm))
  p1 <- NULL
  if (nrow(distance_individual) > 0) {
  p1 <- ggplot() + 
      geom_line(data=distance_individual,
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                    y = calibrateTrackDistanceMeasuredCm,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
      geom_point(data=distance_individual, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                     y = calibrateTrackDistanceMeasuredCm,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                          label = paste0('N=', n_distinct(distance_individual$participant))) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(min_val, max_val),
        breaks = breaks_seq_10,
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_y_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8), labels = scales::label_number(accuracy = 10)) + 
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
    labs(subtitle = 'Credit-card-measured vs. requested distance',
         x = 'Requested distance (cm)',
         y = 'Credit-card-measured distance (cm)',
           caption = 'Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
  
  }
 
  # Plot 2: Credit-card-measured as fraction of requested distance (INDIVIDUAL MEASUREMENTS)
  p2 <- ggplot() + 
    geom_line(data=distance_individual,
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                  y = credit_card_fraction,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
    geom_point(data=distance_individual, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                   y = credit_card_fraction,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(distance_individual$participant), '\n',
                                       'Mean=', mean_formatted, '\n',
                                       'SD=', sd_formatted)) + 
    geom_hline(yintercept = 1, linetype = "dashed") + # y=1 line (perfect ratio)
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    scale_x_log10(
      limits = c(min_val, max_val),
      breaks = breaks_seq_10,
      expand = expansion(mult = c(0.05, 0.05)),
      labels = scales::label_number(accuracy = 1)
    ) + 
    scale_y_log10(limits = c(minFrac, maxFrac),
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
    labs(subtitle = 'Credit-card-measured over requested distance',
         x = 'Requested distance (cm)',
         y = 'Credit-card-measured over requested distance',
         caption = 'Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')

  # Plot 3: IPD (camera px) vs requested distance (individual data)
  if ("calibrateTrackDistanceIpdVpx" %in% names(distance)) {
    ipd_data <- distance %>%
      filter(!is.na(calibrateTrackDistanceIpdVpx),
             !is.na(calibrateTrackDistanceRequestedCm)) %>%
      arrange(participant, order) %>%  # Ensure proper ordering
      mutate(
        # Add logarithmic jitter to requested distance for better visualization (unbiased for log scales)
        calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
      )

    # Get camera data for factorVpxCm
    camera_data <- distanceCalibrationResults$camera

    if (nrow(ipd_data) > 0 && nrow(camera_data) > 0) {
      # Join with camera data to get factorVpxCm
      ipd_data <- ipd_data %>%
        left_join(camera_data %>% select(PavloviaParticipantID, factorVpxCm),
                  by = c("participant" = "PavloviaParticipantID"))

      # Create prediction data for each participant
      prediction_data <- ipd_data %>%
        filter(!is.na(factorVpxCm)) %>%  # factorVpxCm is already numeric
        group_by(participant) %>%
        summarize(
          factorVpxCm = first(factorVpxCm),
          .groups = "drop"
        ) %>%
        # Create prediction line across the x-range
        crossing(calibrateTrackDistanceRequestedCm = seq(min_val, max_val, length.out = 100)) %>%
        mutate(predictedIpdVpx = factorVpxCm / calibrateTrackDistanceRequestedCm)
      
      minX = max(5, min_val - 5)
      maxX = max_val + 10
      p3 <- ggplot() +
        # Add predicted lines first (so they appear behind the data)
        geom_line(data = prediction_data,
                  aes(x = calibrateTrackDistanceRequestedCm,
                      y = predictedIpdVpx,
                      color = participant,
                      group = participant),
                  linewidth = 0.75, alpha = 0.8, linetype = "solid") +
        # Measured data lines (made thinner)
        geom_line(data=ipd_data,
                  aes(x = calibrateTrackDistanceRequestedCm_jitter,
                      y = calibrateTrackDistanceIpdVpx,
                      color = participant,
                      group = participant),
                  linewidth = 0.5, alpha = 0.7) +
        # Measured data points
        geom_point(data=ipd_data,
                   aes(x = calibrateTrackDistanceRequestedCm_jitter,
                       y = calibrateTrackDistanceIpdVpx,
                       color = participant),
                   size = 2) +
        ggpp::geom_text_npc(aes(npcx="left",
                                npcy="top"),
                            label = paste0('N=', n_distinct(ipd_data$participant))) +
        scale_x_log10(
          limits = c(minX, maxX),
          breaks = c(10, 20, 30, 40, 50, 70, 90, 120),
          labels = scales::label_number(accuracy = 1)
        ) +
        scale_y_log10(breaks = scales::log_breaks(n=8)) +
        coord_fixed(ratio = 1) +  # Equal log scale spacing on both axes
        scale_color_manual(values= colorPalette) +
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
          legend.position = "top",
          legend.box = "vertical",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
        labs(subtitle = 'IPD (camera px) vs. requested distance',
             x = 'Requested distance (cm)',
             y = 'IPD (camera px)',
             caption = 'Thick solid lines: predicted IPD = factorVpxCm/requestedDistance\nThin solid lines: measured data\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
    } else {
      p3 <- NULL
    }
  } else {
    p3 <- NULL
  }

  # Plot 3b: IPD * requested distance vs. requested distance (should equal factorVpxCm)
  p3b <- NULL
  if ("calibrateTrackDistanceIpdVpx" %in% names(distance)) {
    ipd_product_data <- distance %>%
      filter(!is.na(calibrateTrackDistanceIpdVpx),
             !is.na(calibrateTrackDistanceRequestedCm)) %>%
      arrange(participant, order) %>%
      mutate(
        # Calculate IPD * distance product
        ipd_distance_product = calibrateTrackDistanceIpdVpx * calibrateTrackDistanceRequestedCm,
        # Add logarithmic jitter to requested distance for better visualization
        calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
      )
    
    # Get camera data for factorVpxCm (horizontal lines)
    camera_data <- distanceCalibrationResults$camera
    
    if (nrow(ipd_product_data) > 0 && nrow(camera_data) > 0) {
      # Join with camera data to get factorVpxCm for horizontal lines
      ipd_product_data <- ipd_product_data %>%
        left_join(camera_data %>% select(PavloviaParticipantID, factorVpxCm),
                  by = c("participant" = "PavloviaParticipantID"))
      
      # Filter to data with valid factorVpxCm
      ipd_product_data_valid <- ipd_product_data %>%
        filter(!is.na(factorVpxCm))
      
      if (nrow(ipd_product_data_valid) > 0) {
        # Create horizontal line data (one per participant with their factorVpxCm)
        horizontal_lines <- ipd_product_data_valid %>%
          group_by(participant, factorVpxCm) %>%
          summarize(
            min_distance = min(calibrateTrackDistanceRequestedCm, na.rm = TRUE),
            max_distance = max(calibrateTrackDistanceRequestedCm, na.rm = TRUE),
            .groups = "drop"
          )
        
        p3b <- ggplot() +
          # Horizontal lines for calibrated factorVpxCm (behind data)
          geom_segment(data = horizontal_lines,
                       aes(x = min_distance, xend = max_distance,
                           y = factorVpxCm, yend = factorVpxCm,
                           color = participant),
                       linewidth = 1.2, alpha = 0.5, linetype = "solid") +
          # Connected lines for measured IPD * distance
          geom_line(data = ipd_product_data_valid,
                    aes(x = calibrateTrackDistanceRequestedCm_jitter,
                        y = ipd_distance_product,
                        color = participant,
                        group = participant),
                    linewidth = 0.6, alpha = 0.8) +
          # Points for measured IPD * distance
          geom_point(data = ipd_product_data_valid,
                     aes(x = calibrateTrackDistanceRequestedCm_jitter,
                         y = ipd_distance_product,
                         color = participant),
                     size = 2.5, alpha = 0.8) +
          ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                              label = paste0('N=', n_distinct(ipd_product_data_valid$participant))) +
          scale_x_log10(
            limits = c(min_val, max_val), 
            breaks = c(10, 20, 30, 40, 50, 70, 90, 120),
            labels = scales::label_number(accuracy = 1)
          ) +
          scale_y_log10(breaks = scales::log_breaks(n = 8)) +
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
          labs(subtitle = 'IPD × requested distance vs. requested distance',
               x = 'Requested distance (cm)',
               y = 'IPD × requested distance (px·cm)',
               caption = 'Thick faint lines: calibrated factorVpxCm (constant per session)\nThin lines with points: measured IPD × distance\nLogarithmic horizontal jitter added to reduce overlap')
      }
    }
  }

  # Plot 4: Eye feet position vs distance error
  p4_result <- plot_eye_feet_position(distanceCalibrationResults)
  p4 <- if (!is.null(p4_result)) p4_result$plot else NULL
  
  # Plot 4b: Foot position during calibration (colored by participant, no clipping)
  p4b <- plot_foot_position_during_calibration(distanceCalibrationResults)
  
  # Plot 5: Calibrated vs. median factorVpxCm
  p5 <- NULL
  if ("calibrateTrackDistanceIpdVpx" %in% names(distance) && nrow(distance) > 0) {
    # Get calibration data for factorVpxCm (Y-axis)
    camera_data <- distanceCalibrationResults$camera
    # Get distance check data for median factorVpxCm (X-axis)
    check_data <- distanceCalibrationResults$check_factor
    
    if (nrow(camera_data) > 0 && nrow(check_data) > 0) {
      # Join calibration and check data by participant
      plot_data <- camera_data %>%
        select(PavloviaParticipantID, factorVpxCm) %>%
        inner_join(check_data %>% select(PavloviaParticipantID, medianFactorVpxCm),
                  by = "PavloviaParticipantID") %>%
        rename(participant = PavloviaParticipantID) %>%
        filter(!is.na(factorVpxCm), !is.na(medianFactorVpxCm),
               is.finite(factorVpxCm), is.finite(medianFactorVpxCm))
        
      if (nrow(plot_data) > 0) {
        # Calculate symmetric scale limits to ensure equal axes (slope 1 equality line)
        min_val_all <- min(c(plot_data$medianFactorVpxCm, plot_data$factorVpxCm), na.rm = TRUE) * 0.9
        max_val_all <- max(c(plot_data$medianFactorVpxCm, plot_data$factorVpxCm), na.rm = TRUE) * 1.1
        
        p5 <- ggplot(plot_data, aes(x = medianFactorVpxCm, y = factorVpxCm)) +
          geom_point(aes(color = participant), size = 3, alpha = 0.8) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
          ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                              label = paste0('N=', n_distinct(plot_data$participant))) +
          scale_x_log10(limits = c(min_val_all, max_val_all), 
                        breaks = scales::log_breaks(n = 8)) +
          scale_y_log10(limits = c(min_val_all, max_val_all), 
                        breaks = scales::log_breaks(n = 8)) +
          coord_fixed(ratio = 1) +  # Force 1:1 aspect ratio for symmetric axes
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
          labs(subtitle = 'Calibrated vs. median factorVpxCm',
               x = 'Median factorVpxCm from distance checking (per session)',
               y = 'FactorVpxCm from calibration',
               caption = 'Dashed line shows y=x (perfect agreement)\nMedian calculated from measuredFactorVpxCm in distanceCheckJSON')
      }
    }
  }
  
  # Plot 6: Calibrated over mean factorVpxCm vs. spot diameter
  p6 <- NULL
  if ("calibrateTrackDistanceIpdVpx" %in% names(distance) && nrow(distance) > 0) {
    # Get camera data for factorVpxCm
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
                 y = 'FactorVpxCm over geometric mean',
                 caption = 'Dashed line shows y=1 (perfect agreement with session mean)\nGeometric mean = 10^mean(log10(measuredEyeToCameraCm × ipdVpx))')
        }
      }
    }
  }
  
  # Plot 7: Histogram of factorVpxCm / medianFactorVpxCm (colored by participant)
  p7 <- NULL
  if (nrow(distance) > 0 &&
      "camera" %in% names(distanceCalibrationResults) &&
      "check_factor" %in% names(distanceCalibrationResults) &&
      nrow(distanceCalibrationResults$camera) > 0 &&
      nrow(distanceCalibrationResults$check_factor) > 0) {
    
    camera_data <- distanceCalibrationResults$camera %>%
      select(PavloviaParticipantID, factorVpxCm) %>%
      filter(!is.na(factorVpxCm), is.finite(factorVpxCm))
    check_data <- distanceCalibrationResults$check_factor %>%
      select(PavloviaParticipantID, medianFactorVpxCm) %>%
      filter(!is.na(medianFactorVpxCm), is.finite(medianFactorVpxCm))
    
    ratio_data <- camera_data %>%
      inner_join(check_data, by = "PavloviaParticipantID") %>%
      mutate(
        participant = PavloviaParticipantID,
        ratio = factorVpxCm / medianFactorVpxCm
      ) %>%
      filter(is.finite(ratio), ratio > 0)
    
    if (nrow(ratio_data) > 0) {
      # SD of log10(ratio)
      sd_log10_ratio <- sd(log10(ratio_data$ratio), na.rm = TRUE)
      
      # Dot-stack histogram in log space
      bin_w_log <- 0.02  # ~4.6% per bin
      ratio_data <- ratio_data %>%
        mutate(
          log_ratio = log10(ratio),
          bin_center_log = round(log_ratio / bin_w_log) * bin_w_log,
          bin_center = 10^bin_center_log
        ) %>%
        arrange(bin_center, participant) %>%
        group_by(bin_center) %>%
        mutate(
          stack_position = row_number(),
          dot_y = stack_position
        ) %>%
        ungroup()
      
      max_count <- max(ratio_data$dot_y)
      x_min <- min(ratio_data$bin_center, na.rm = TRUE) * 0.95
      x_max <- max(ratio_data$bin_center, na.rm = TRUE) * 1.05
      
      p7 <- ggplot(ratio_data, aes(x = ratio)) +
        # Dot stacked points
        geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                            label = statement, size = 3, family = "sans", fontface = "plain") +
        scale_color_manual(values = colorPalette) +
        scale_y_continuous(
          limits = c(0, max(6, max_count + 1)),
          expand = expansion(mult = c(0, 0.1)),
          breaks = function(x) seq(0, ceiling(max(x)), by = 1)
        ) +
        scale_x_log10(
          limits = c(x_min, x_max),
          labels = scales::label_number(accuracy = 0.01)
        ) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0('N=', n_distinct(ratio_data$participant),
                                           '\nSD(log10) = ', format(round(sd_log10_ratio, 4), nsmall = 4)),
                            size = 3, family = "sans", fontface = "plain") +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(0.3, "cm"),
          keyheight = unit(0.3, "cm")
        )) +
        labs(
          subtitle = 'Histogram of factorVpxCm / median(factorVpxCm)',
          x = "factorVpxCm / median(factorVpxCm)",
          y = "Count"
        ) +
        theme_bw() +
        theme(
          legend.key.size = unit(0, "mm"),
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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
  
  # Calculate heights based on legend complexity
  n_participants <- n_distinct(distance_individual$participant)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.06)

  # Eye feet position plot has complex legend too
  eye_feet_height <- if (!is.null(p4_result)) p4_result$height else 4

  return(list(
    credit_card_vs_requested = list(plot = p1, height = plot_height),
    credit_card_fraction = list(plot = p2, height = plot_height),
    ipd_vs_requested = list(plot = p3, height = if (!is.null(p3)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    ipd_product_vs_requested = list(plot = p3b, height = if (!is.null(p3b)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    eye_feet_position = list(plot = p4, height = eye_feet_height),
    foot_position_calibration = list(plot = p4b, height = plot_height),
    calibrated_vs_mean = list(plot = p5, height = if (!is.null(p5)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    calibrated_over_mean_vs_spot = list(plot = p6, height = if (!is.null(p6)) compute_auto_height(base_height = 7, n_items = n_distinct(distance$participant), per_row = 3, row_increase = 0.06) else NULL),
    calibrated_over_median_hist = list(
      plot = p7,
      height = if (!is.null(p7)) {
        compute_auto_height(base_height = 1.6, n_items = n_distinct(ratio_data$participant), per_row = 3, row_increase = 0.05) +
          0.28 * max(max_count, 6)
      } else NULL
    )
  ))
}

plot_sizeCheck <- function(distanceCalibrationResults, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  sizeCheck <- distanceCalibrationResults$sizeCheck
  statement <- distanceCalibrationResults$statement
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
      scale_y_continuous(limits = c(0, max(6, max_count + 1)),
                         expand = expansion(mult = c(0, 0.1)), 
                         breaks = function(x) seq(0, ceiling(max(x)), by = 1)) + 
      scale_x_log10(limits = c(x_min, x_max),breaks = scales::log_breaks(n=8)) +
      annotation_logticks(sides = "b") +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(sdLogDensity_data$participant)),
                          size = 3, family = "sans", fontface = "plain") + 
      guides(color = guide_legend(
        ncol = 3,  
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
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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
    scale_y_continuous(limits = c(0, max(6, max_count + 1)), expand = expansion(mult = c(0, 0.1)), breaks = function(x) seq(0, ceiling(max(x)), by = 2)) + 
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
        ncol = 3,  
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
            legend.title = element_text(size = 7),
            legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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
  
  sizeCheck_avg <- sizeCheck_avg %>% 
    left_join(sdLogDensity_data %>% select(-avg_estimated), by = "participant") %>% 
    mutate(reliableBool = (sdLogDensity <= calibrateTrackDistanceCheckLengthSDLogAllowed))
  ymin = max(5,floor(min(sizeCheck_avg$avg_estimated) / 10 - 1) * 10)
  ymax = ceiling(max(sizeCheck_avg$avg_estimated) / 10 + 1) * 10

  p1 <- ggplot(data=sizeCheck_avg) + 
    geom_line(aes(x = SizeCheckRequestedCm_jitter, 
                  y = avg_estimated,
                  color = participant,
                  group = participant,
                  linetype = reliableBool), 
              alpha = 0.7) +
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    geom_point(aes(x = SizeCheckRequestedCm_jitter, 
                   y = avg_estimated,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(sizeCheck_avg$participant)),
                        size = 3, family = "sans", fontface = "plain") + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement,
                        size = 3, family = "sans", fontface = "plain") + 
    scale_x_log10(
      breaks = function(lims) {
        lo <- floor(min(sizeCheck_avg$SizeCheckRequestedCm, na.rm = TRUE) / 10) * 10
        hi <- ceiling(max(sizeCheck_avg$SizeCheckRequestedCm, na.rm = TRUE) / 10) * 10
        if (is.na(lo) || is.na(hi)) return(NULL)
        lo <- max(10, lo)
        br <- seq(lo, hi, by = 10)
        br[!(br %in% c(60, 80, 100, 110))]
      },
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
      ncol = 3,  
      title = "",
      override.aes = list(size = 2),  
      keywidth = unit(1.2, "lines"),  
      keyheight = unit(0.8, "lines")  
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    labs(subtitle = 'Measured pixel density vs requested length',
         x = 'Requested length (cm)',
         y = 'Measured pixel density (px/cm)')
  
  # smallest positive x so the rect shows up on a log scale
  # Compute positive finite bounds for a log–log plot
  # --- pick panel limits explicitly ---
  min_pos_x <- suppressWarnings(min(sdLogDensity_data$sdLogDensity[sdLogDensity_data$sdLogDensity > 0], na.rm = TRUE))
  # left edge at the decade just below/at the smallest x (e.g., 0.001 if min≈0.00117)
  x_left <- if (is.finite(min_pos_x)) 10^floor(log10(min_pos_x)) else 1e-6
  
  rat_pos <- sdLogDensity_data$ratio[sdLogDensity_data$ratio > 0]
  y_low  <- suppressWarnings(min(rat_pos, na.rm = TRUE))
  y_high <- suppressWarnings(max(rat_pos, na.rm = TRUE))
  
  rat_pos <- sdLogDensity_data$ratio[sdLogDensity_data$ratio > 0]
  y_low  <- suppressWarnings(min(rat_pos, na.rm = TRUE))
  y_high <- suppressWarnings(max(rat_pos, na.rm = TRUE))
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
      color = guide_legend(ncol = 3, title = "",
                           override.aes = list(size = 2),
                           keywidth = grid::unit(1.2, "lines"),
                           keyheight = grid::unit(0.8, "lines")),
      linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))
    ) +
    labs(
      subtitle = "Credit card pixel density re mean production vs.\nSD of log produced pixel density",
      x = "SD of log10 pixel density",
      y = "Credit card pixel density"
    )
  
  
  
  # Calculate heights based on legend complexity
  n_participants <- if (nrow(sizeCheck_avg) > 0) n_distinct(sizeCheck_avg$participant) else 0
  n_participants_hist <- if (nrow(sdLogDensity_data) > 0) n_distinct(sdLogDensity_data$participant) else 0
  n_participants_ruler <- if (nrow(ruler) > 0) n_distinct(ruler$participant) else 0

  # Base height calculation: more participants = more legend space (in inches)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = max(n_participants, n_participants_hist, n_participants_ruler), per_row = 3, row_increase = 0.06)

  # Histograms have top legends; reduce overall height to half
  hist_base <- 1.7
  # Compute extra height separately for each dotted histogram
  h1_max_stack <- if (nrow(sdLogDensity_data) > 0) max(sdLogDensity_data$dot_y, na.rm = TRUE) else 0
  h2_max_stack <- if (exists("ruler_dotplot")) max(ruler_dotplot$dot_y, na.rm = TRUE) else 0
  h1_stack_for_height <- max(h1_max_stack, 6)
  h2_stack_for_height <- max(h2_max_stack, 6)
  h1_height <- compute_auto_height(base_height = hist_base, n_items = n_participants_hist, per_row = 3, row_increase = 0.05) + 0.28 * h1_stack_for_height
  h2_height <- compute_auto_height(base_height = hist_base, n_items = n_participants_ruler, per_row = 3, row_increase = 0.05) + 0.28 * h2_stack_for_height

  return(list(
    density_vs_length = list(plot = p1, height = plot_height),
    density_ratio_vs_sd = list(plot = p2, height = plot_height),
    sd_hist = list(plot = h1, height = h1_height),
    ruler_hist = list(plot = h2, height = h2_height)
  ))
}

plot_distance_production <- function(distanceCalibrationResults, participant_info, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  distance <- distanceCalibrationResults$distance
  sizeCheck <- distanceCalibrationResults$sizeCheck
  statement <- distanceCalibrationResults$statement
        
  
  if (nrow(distance) == 0) {return(NULL)}
  
  # Calculate density ratio data (same as credit card plot's sdLogDensity_data calculation)
  if (nrow(sizeCheck) > 0) {
    densityRatio_data <- sizeCheck %>%
      group_by(participant, pxPerCm) %>%
      summarize(
        avg_estimated = 10^median(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
        sdLogDensity = sd(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(!is.na(sdLogDensity)) %>%
      mutate(
        densityRatio = pxPerCm / avg_estimated,
        reliableBool = (sdLogDensity <= calibrateTrackDistanceCheckLengthSDLogAllowed)
      )
    
    sdLogDensity_data <- densityRatio_data %>%
      select(participant, sdLogDensity, reliableBool)
    
  } else {
    densityRatio_data <- tibble()
    sdLogDensity_data <- tibble()
  }
  


  
  # Average Measured Distance per Participant per Requested Distance (SAME AS CREDIT CARD)
  distance_avg <- distance %>%
    group_by(participant, calibrateTrackDistanceRequestedCm) %>%
    summarize(
      creditCardMeasuredCm = mean(calibrateTrackDistanceMeasuredCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add consistent logarithmic horizontal jitter to x-axis variable (SAME AS CREDIT CARD)
      calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
    )
  
  # Join with density ratio data
  if (nrow(densityRatio_data) > 0) {
    distance_avg <- distance_avg %>% 
      left_join(densityRatio_data %>% select(participant, densityRatio), by = "participant") %>%
      left_join(sdLogDensity_data, by = "participant")
  } else {
    distance_avg <- distance_avg %>% 
      mutate(densityRatio = 1, reliableBool = TRUE)
  }
  
  # Apply production correction and calculate ratio
  distance_avg <- distance_avg %>%
    mutate(
      avg_measured = ifelse(!is.na(densityRatio), densityRatio * creditCardMeasuredCm, creditCardMeasuredCm),
      # Calculate the ratio Y/X for the second plot
      production_fraction = avg_measured / calibrateTrackDistanceRequestedCm
    ) %>% 
    filter(is.finite(avg_measured))

  # Calculate mean and SD of the ratio for reporting
  mean_fraction <- mean(distance_avg$production_fraction, na.rm = TRUE)
  sd_fraction <- sd(distance_avg$production_fraction, na.rm = TRUE)
  
  # Format for display
  mean_formatted <- format(round(mean_fraction, 3), nsmall = 3)
  sd_formatted <- format(round(sd_fraction, 3), nsmall = 3)
  
  # Calculate scale limits using INDIVIDUAL data that will actually be plotted
  # This ensures all individual points are visible

  # Get individual data for limit calculation (same as what gets plotted)
  individual_data_for_limits <- distance %>%
    arrange(participant, order) %>%
    left_join(densityRatio_data %>% select(participant, densityRatio, pxPerCm), by = "participant") %>%
    left_join(sdLogDensity_data %>% select(participant, reliableBool), by = "participant") %>%
    mutate(
      product_measured = ifelse(!is.na(densityRatio),
                               calibrateTrackDistanceMeasuredCm * densityRatio,
                               calibrateTrackDistanceMeasuredCm),
      calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
    ) %>%
    filter(is.finite(product_measured))

  raw_min <- min(c(individual_data_for_limits$calibrateTrackDistanceRequestedCm_jitter,
                   individual_data_for_limits$product_measured), na.rm = TRUE)
  raw_max <- max(c(individual_data_for_limits$calibrateTrackDistanceRequestedCm_jitter,
                   individual_data_for_limits$product_measured), na.rm = TRUE)

  min_val <- 5 * floor(raw_min / 5)
  min_val = max(10, min_val)
  max_val <- 5 * ceiling(raw_max / 5)

  p1 <- NULL
  p2 <- NULL 
  if (nrow(distance_avg) > 0) {
  # Plot 1: Production-measured vs requested distance
  p1 <- ggplot() + 
    geom_line(data=distance_avg, 
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                  y = avg_measured,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
    geom_point(data=distance_avg, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                   y = avg_measured,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(distance_avg$participant))) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(min_val, max_val),
        breaks = function(lims) {
          lo <- floor(min(lims, na.rm = TRUE) / 10) * 10
          hi <- ceiling(max(lims, na.rm = TRUE) / 10) * 10
          if (is.na(lo) || is.na(hi)) return(NULL)
          lo <- max(10, lo)
          br <- seq(lo, hi, by = 10)
          br[!(br %in% c(60, 80, 100, 110))]
        },
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_y_log10(limits = c(min_val, max_val),
                    breaks = scales::log_breaks(n=8)) +
      annotation_logticks() + 
    scale_color_manual(values= colorPalette) + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) + 
    guides(color = guide_legend(
      ncol = 3,  # SAME AS CREDIT CARD: More columns to fit more participants horizontally
      title = "",
      override.aes = list(size = 2),  # SAME AS CREDIT CARD: Smaller points in legend
      keywidth = unit(1.2, "lines"),  # SAME AS CREDIT CARD: Reduce key width
      keyheight = unit(0.8, "lines")  # SAME AS CREDIT CARD: Reduce key height
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    coord_fixed() +  # SAME AS CREDIT CARD
      labs(subtitle = 'Averge Production-measured vs.\nrequested distance',  # ONLY LABEL DIFFERENCE
         x = 'Requested distance (cm)',                              # SAME AS CREDIT CARD
           y = 'production-measured distance (cm)',
           caption = "Logarithmic horizontal jitter added to reduce overlap (unbiased for log scales)")
  
  # Plot 2: Production-measured as fraction of requested distance
    minFrac <- max(0.1, min(0.5, floor(distance_avg$production_fraction * 10) / 10))
    maxFrac <- min(1.5, ceiling(distance_avg$production_fraction * 10) / 10)
  p2 <- ggplot() + 
    geom_line(data=distance_avg, 
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                  y = production_fraction,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
    geom_point(data=distance_avg, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                   y = production_fraction,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(distance_avg$participant), '\n',
                                       'Mean=', mean_formatted, '\n',
                                       'SD=', sd_formatted)) + 
    geom_hline(yintercept = 1, linetype = "dashed") + # y=1 line (perfect ratio)
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(min_val, max_val),
        breaks = function(lims) {
          lo <- floor(min(lims, na.rm = TRUE) / 10) * 10
          hi <- ceiling(max(lims, na.rm = TRUE) / 10) * 10
          if (is.na(lo) || is.na(hi)) return(NULL)
          lo <- max(10, lo)
          br <- seq(lo, hi, by = 10)
          br[!(br %in% c(60, 80, 100, 110))]
        },
        labels = scales::label_number(accuracy = 1),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_y_log10(limits = c(minFrac,maxFrac),
                    breaks = c(seq(0.5, 1.5, by = 0.1),1.7,2.0)) +
      annotation_logticks() +
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
      labs(subtitle = 'Production-measured over requested distance',
         x = 'Requested distance (cm)',
           y = 'Production-measured over requested distance',
           caption = 'Logarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
    
  }
   
  # Plot 3: Individual production measurements vs requested distance (non-averaged)
  if (nrow(distance) > 0) {
    # Use individual distance measurements, not aggregated data
    individual_data <- distance %>%
      arrange(participant, order) %>%  # Ensure proper ordering
      left_join(densityRatio_data %>% select(participant, densityRatio, pxPerCm), by = "participant") %>%
      left_join(sdLogDensity_data %>% select(participant, reliableBool), by = "participant") %>%
      mutate(
        product_measured = ifelse(!is.na(densityRatio),
                                 calibrateTrackDistanceMeasuredCm * densityRatio,
                                 calibrateTrackDistanceMeasuredCm),
        calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
      ) %>%
      filter(is.finite(product_measured))


    p3 <- ggplot() +
      # Connect points within the same participant in order
      geom_line(data=individual_data,
                aes(x = calibrateTrackDistanceRequestedCm_jitter,
                    y = product_measured,
                    color = participant,
                    lty = reliableBool,
                    group = participant), alpha = 0.7) +
      geom_point(data=individual_data,
                 aes(x = calibrateTrackDistanceRequestedCm_jitter,
                     y = product_measured,
                     color = participant),
                 size = 2) +
      ggpp::geom_text_npc(aes(npcx="left",
                              npcy="top"),
                          label = paste0('N=', n_distinct(individual_data$participant))) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
      scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                            labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(min_val, max_val),
        breaks = function(lims) {
          lo <- floor(min(lims, na.rm = TRUE) / 10) * 10
          hi <- ceiling(max(lims, na.rm = TRUE) / 10) * 10
          if (is.na(lo) || is.na(hi)) return(NULL)
          lo <- max(10, lo)
          br <- seq(lo, hi, by = 10)
          br[!(br %in% c(60, 80, 100, 110))]
        },
        labels = scales::label_number(accuracy = 1)
      ) +
      scale_y_log10(limits = c(min_val, max_val),breaks = scales::log_breaks(n=10), labels = scales::label_number(accuracy = 10)) +
      annotation_logticks() + 
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
      coord_fixed() +
      labs(subtitle = 'Production-measured vs. \nrequested distance',
           x = 'Requested distance (cm)',
           y = 'Production-measured distance (cm)',
           caption = "Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)")
  } else {
    p3 <- NULL
  }

  # Plot 4: Individual production measurements as fraction of requested distance
  if (nrow(distance) > 0) {
    # Use the same individual_data as p3 (already arranged by participant, order)
    individual_fraction_data <- individual_data %>%
      mutate(
        # Calculate fraction for each individual measurement
        production_fraction = product_measured / calibrateTrackDistanceRequestedCm_jitter
      ) %>%
      filter(is.finite(production_fraction))
    

    # Calculate mean and SD of individual fractions for reporting
    mean_individual_fraction <- mean(individual_fraction_data$production_fraction, na.rm = TRUE)
    sd_individual_fraction <- sd(individual_fraction_data$production_fraction, na.rm = TRUE)

    # Format for display
    mean_individual_formatted <- format(round(mean_individual_fraction, 3), nsmall = 3)
    sd_individual_formatted <- format(round(sd_individual_fraction, 3), nsmall = 3)

    minFrac <- max(0.1, min(0.5, floor(individual_fraction_data$production_fraction * 10) / 10))
    maxFrac <- max(1.5, ceiling(individual_fraction_data$production_fraction * 10) / 10)
    p4 <- ggplot() +
      # Connect points within the same participant in order
      geom_line(data=individual_fraction_data,
                aes(x = calibrateTrackDistanceRequestedCm_jitter,
                    y = production_fraction,
                    color = participant,
                    lty = reliableBool,
                    group = participant), alpha = 0.7) +
      geom_point(data=individual_fraction_data,
                 aes(x = calibrateTrackDistanceRequestedCm_jitter,
                     y = production_fraction,
                     color = participant),
                 size = 2) +
      ggpp::geom_text_npc(aes(npcx="left",
                              npcy="top"),
                          label = paste0('N=', n_distinct(individual_fraction_data$participant), '\n',
                                         'Mean=', mean_individual_formatted, '\n',
                                         'SD=', sd_individual_formatted)) +
      geom_hline(yintercept = 1, linetype = "dashed") + # y=1 line (perfect ratio)
      scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                            labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
      scale_x_log10(
        limits = c(min_val, max_val),
        breaks = function(lims) {
          lo <- floor(min(lims, na.rm = TRUE) / 10) * 10
          hi <- ceiling(max(lims, na.rm = TRUE) / 10) * 10
          if (is.na(lo) || is.na(hi)) return(NULL)
          lo <- max(10, lo)
          br <- seq(lo, hi, by = 10)
          br[!(br %in% c(60, 80, 100, 110))]
        },
        labels = scales::label_number(accuracy = 1),
        expand = expansion(mult = c(0.05, 0.05))
      ) +
      scale_y_log10(limits = c(minFrac,maxFrac),
                    breaks = seq(0.6, 1.4, by = 0.1)) +
      annotation_logticks() + 
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
      labs(subtitle = 'Production-measured over\n requested distance',
           x = 'Requested distance (cm)',
           y = 'Production-measured over requested distance',
           caption = 'Lines connect measurements from the same session.\nHorizontal jitter added to reduce overlap')
  } else {
    p4 <- NULL
  }

  # Plot 5: Error vs. object size (Production-measured over requested distance vs objectLengthCm)
  # Only show this plot when _calibrateTrackDistance=object (i.e., when objectLengthCm has valid values)
  p5 <- NULL
  if (nrow(distance_avg) > 0 && !is.null(participant_info) && nrow(participant_info) > 0) {
    # Check if objectLengthCm column has any valid (non-NA) values
    has_valid_object_lengths <- any(!is.na(participant_info$objectLengthCm) & 
                                   participant_info$objectLengthCm != "" & 
                                   participant_info$objectLengthCm != "NA")
    
    if (has_valid_object_lengths) {
      # Join with participant_info to get objectLengthCm
      error_vs_object_data <- distance_avg %>%
        left_join(participant_info %>% select(participant = PavloviaParticipantID, objectLengthCm),
                  by = "participant") %>%
        filter(!is.na(objectLengthCm), !is.na(production_fraction), 
               objectLengthCm != "", objectLengthCm != "NA") %>% 
        mutate(objectLengthCm = as.numeric(objectLengthCm))

      if (nrow(error_vs_object_data) > 0) {
        # Calculate scale limits
        x_min <- max(1, min(error_vs_object_data$objectLengthCm) * 0.8 - 10)
        x_max <- max(error_vs_object_data$objectLengthCm) * 1.2 + 10
        y_min <- max(0.1, min(error_vs_object_data$production_fraction) * 0.8)
        y_max <- min(2.0, max(error_vs_object_data$production_fraction) * 1.2)

        p5 <- ggplot(error_vs_object_data, aes(x = objectLengthCm, y = production_fraction)) +
          geom_point(aes(color = participant), size = 3, alpha = 0.8) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
          ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                              label = paste0('N=', n_distinct(error_vs_object_data$participant))) +
          scale_x_log10(
            limits = c(x_min, x_max),
            breaks = c(10, 20, 30, 40, 50, 70, 90, 120),
            labels = scales::label_number(accuracy = 1)
          ) +
          scale_y_log10(limits = c(y_min, y_max), breaks = seq(0.5, 2.0, by = 0.1)) +
          annotation_logticks() +
          scale_color_manual(values = colorPalette) +
          ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
          guides(color = guide_legend(
            ncol = 3,
            title = "",
            override.aes = list(size = 2),
            keywidth = unit(1.2, "lines"),
            keyheight = unit(0.8, "lines")
          )) +
          coord_fixed() +
          labs(subtitle = 'Error vs. object size',
               x = 'Object length (cm)',
               y = 'Production-measured over requested distance',
               caption = 'Red dashed line shows perfect accuracy (ratio = 1.0)')
      }
    }
  }

  # Plot 6: Error vs. blindspot diameter (spotDeg)
  blindspot_data <- distanceCalibrationResults$blindspot
  p6 <- NULL
  if (nrow(distance_avg) > 0 && nrow(blindspot_data) > 0) {
    # Join distance_avg with blindspot_data
    error_vs_blindspot_data <- distance_avg %>%
      left_join(blindspot_data, by = "participant") %>%
      filter(!is.na(`_calibrateTrackDistanceBlindspotDiameterDeg`), 
             !is.na(production_fraction)) %>%
      rename(spotDeg = `_calibrateTrackDistanceBlindspotDiameterDeg`)

    if (nrow(error_vs_blindspot_data) > 0) {
      # Calculate scale limits
      x_min <- max(0.1, min(error_vs_blindspot_data$spotDeg) * 0.8)
      x_max <- max(error_vs_blindspot_data$spotDeg) * 1.2
      y_min <- max(0.1, min(error_vs_blindspot_data$production_fraction) * 0.8)
      y_max <- min(2.0, max(error_vs_blindspot_data$production_fraction) * 1.2)

      error_vs_blindspot_data <- error_vs_blindspot_data %>%
        mutate(spotDeg_jitter = add_log_jitter(spotDeg, jitter_percent = 0.5, seed = 42))

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
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(1.2, "lines"),
          keyheight = unit(0.8, "lines")
        )) +
        labs(subtitle = 'Error vs. spot diameter',
             x = 'Spot diameter (deg)',
             y = 'Production-measured over requested distance',
             caption = 'Red dashed line shows perfect accuracy (ratio = 1.0)')
    }
  }
   
  
  
  # Calculate heights based on legend complexity
  n_participants <- if (nrow(distance_avg) > 0) n_distinct(distance_avg$participant) else 0

  # Base height calculation: more participants = more legend space (in inches)
  base_height <- 7
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.06)

  return(list(
    production_vs_requested = list(plot = p1, height = plot_height),
    production_fraction = list(plot = p2, height = plot_height),
    raw_production_vs_requested = list(plot = p3, height = plot_height),
    individual_production_fraction = list(plot = p4, height = plot_height),
    error_vs_object_size = list(plot = p5, height = if (!is.null(p5)) compute_auto_height(base_height = 7, n_items = n_distinct(error_vs_object_data$participant), per_row = 3, row_increase = 0.06) else NULL),
    error_vs_blindspot_diameter = list(plot = p6, height = if (!is.null(p6)) compute_auto_height(base_height = 7, n_items = n_distinct(error_vs_blindspot_data$participant), per_row = 3, row_increase = 0.06) else NULL)
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
    scale_y_continuous(limits = c(0, max_count + 1), 
                       expand = expansion(mult = c(0, 0.1)), 
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) + 
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                        label = paste0('N=', n_distinct(object_dotplot$PavloviaParticipantID))) + 
    guides(color = guide_legend(
      ncol = 3,  
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
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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
  base_height <- 1.6
  plot_height <- compute_auto_height(base_height = base_height, n_items = n_participants, per_row = 3, row_increase = 0.05) + 0.28 * max(max_count, 6)

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
  print(paste("mean_dotplot rows:", nrow(mean_dotplot), ", max_count_mean:", max_count_mean, ", n_participants_mean:", n_participants_mean))

  p1 <- ggplot(mean_dotplot, aes(x = m)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
  scale_y_continuous(limits = c(0, max(6, max_count_mean + 1)),
                       expand = expansion(mult = c(0, 0.1)),
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0('N=', n_participants_mean)) +
    guides(color = guide_legend(
      ncol = 3,
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
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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
  print(paste("sd_dotplot rows:", nrow(sd_dotplot), ", max_count_sd:", max_count_sd, ", n_participants_sd:", n_participants_sd))

  p2 <- ggplot(sd_dotplot, aes(x = sd)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 6, alpha = 0.85) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
  scale_y_continuous(limits = c(0, max(6, max_count_sd + 1)),
                       expand = expansion(mult = c(0, 0.1)),
                       breaks = function(x) seq(0, ceiling(max(x)), by = 2)) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0('N=', n_participants_sd)) +
    guides(color = guide_legend(
      ncol = 3,
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
          legend.title = element_text(size = 7),
          legend.text = element_text(size = 8, margin = margin(t = 0, b = 0)),
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

  base_height <- 1.6
  plot_height <- compute_auto_height(base_height = base_height, n_items = max(n_participants_mean, n_participants_sd), per_row = 3, row_increase = 0.05) + 
    0.28 * max(max_count_mean, max_count_sd, 6)
  print(paste("returning bs_vd_hist with height:", plot_height))

  return(list(
    mean_plot = list(plot = p1, height = plot_height),
    sd_plot = list(plot = p2, height = plot_height)
  ))
}

