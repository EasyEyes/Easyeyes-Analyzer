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

get_cameraResolutionXY <- function(data_list) {
  df <- tibble()
  if (is.null(data_list) || length(data_list) == 0) return(df)
  for (i in 1:length(data_list)) {
    factorCameraPxCmList = c()
    factorCameraPxCm = ""
    if ("distanceCalibrationJSON" %in% names(data_list[[i]])) {

      distanceCalibration <- fromJSON(sort(data_list[[i]]$distanceCalibrationJSON)[1])

     for (j in 1:length(distanceCalibration)) {
       factorCameraPxCmList = c(factorCameraPxCmList,distanceCalibration[[j]]$factorCameraPxCm)
     }
      # Take the first factorCameraPxCm value and ensure it's numeric
      factorCameraPxCm <- as.numeric(factorCameraPxCmList[1])
    }

    t <- data_list[[i]] %>%
      mutate(factorCameraPxCm = !!factorCameraPxCm)

    # Skip iteration if participant column doesn't exist or has no data
    if (!"participant" %in% names(t) || all(is.na(t$participant)) || all(t$participant == "")) {
      next
    }

    t <- t %>%
      select(participant, `_calibrateTrackDistance`,
             `_calibrateTrackDistancePupil`, factorCameraPxCm,
             cameraResolutionXY) %>%
      rename(pavloviaParticipantID = participant) %>%
      distinct() %>%
      filter(!is.na(cameraResolutionXY), cameraResolutionXY != "",
             !is.na(factorCameraPxCm))
    if (nrow(t) > 0) {
      df <- rbind(df, t)
    }
  }
  return(df %>% distinct())
}

get_merged_participant_distance_info <- function(data_list, participant_info) {
  # Get camera resolution data
  camera_data <- get_cameraResolutionXY(data_list)
  
  # If no participant_info provided, return just camera data
  if (is.null(participant_info) || nrow(participant_info) == 0) {
    return(camera_data)
  }
  
  # If no camera data, return just participant info with renamed ID column
  if (nrow(camera_data) == 0) {
    if ("PavloviaParticipantID" %in% names(participant_info)) {
      return(participant_info)
    } else if ("participant" %in% names(participant_info)) {
      return(participant_info %>% rename(pavloviaParticipantID = participant))
    } else {
      return(participant_info)
    }
  }
  
  # Prepare participant_info for merging
  participant_info_clean <- participant_info
  
  # Ensure consistent ID column naming
  if ("PavloviaParticipantID" %in% names(participant_info_clean)) {
    participant_info_clean <- participant_info_clean %>%
      rename(pavloviaParticipantID = PavloviaParticipantID)
  } else if ("participant" %in% names(participant_info_clean)) {
    participant_info_clean <- participant_info_clean %>%
      rename(pavloviaParticipantID = participant)
  }
  
  # Perform full outer join to include all participants from both tables
  merged_data <- camera_data %>%
    full_join(participant_info_clean, by = "pavloviaParticipantID") %>%
    mutate(
      ok_priority = case_when(
        ok == "✅" ~ 1,  # ✅ (white_check_mark) first
        ok == "🚧" ~ 2,  # 🚧 (construction) second  
        ok == "❌" ~ 3,  # ❌ (x) last
        is.na(ok) ~ 4,   # NA last
        .default = 5     # Any other status
      )
    ) %>%
    arrange(ok_priority, pavloviaParticipantID) %>%
    select(-ok_priority) 
  
  return(merged_data)
}

get_distance_calibration <- function(data_list, minRulerCm) {
  if (length(data_list) == 0) {
    return(list())
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
    return(list())
  }
  
  filtered_data_list <- list()
  for (i in 1:length(data_list)) {
    filtered_data <- data_list[[i]] %>%
      filter(participant %in% valid_participants)
    
    if (nrow(filtered_data) > 0) {
      filtered_data_list[[length(filtered_data_list) + 1]] <- filtered_data
    }
  }
  return(filtered_data_list)
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
             viewingDistanceWhichEye,
             viewingDistanceWhichPoint,
             participant,
             SizeCheckEstimatedPxPerCm,
             SizeCheckRequestedCm,
             rulerLength,
             rulerUnit,
             pxPerCm) %>%
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
    t <- data_list[[i]] %>%
      select(`_calibrateTrackDistance`,
             `_calibrateTrackDistancePupil`,
             viewingDistanceWhichEye,
             viewingDistanceWhichPoint,
             participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm,
             calibrateTrackDistanceIpdCameraPx) %>%
      distinct() %>%
      filter(!is.na(calibrateTrackDistanceMeasuredCm),
             !is.na(calibrateTrackDistanceRequestedCm),
             calibrateTrackDistanceMeasuredCm != '',
             calibrateTrackDistanceRequestedCm != '')

    if (nrow(t) > 0) {
      # Convert JSON-like lists into strings and remove extra characters
      t <- t %>%
        mutate(
          calibrateTrackDistanceMeasuredCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm),
          calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm),
          calibrateTrackDistanceIpdCameraPx = gsub("\\[|\\]|\"", "", calibrateTrackDistanceIpdCameraPx)
        ) %>%
        # Separate all columns together while keeping row-wise structure
        mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","),
               requested_list = strsplit(calibrateTrackDistanceRequestedCm, ","),
               ipd_list = strsplit(calibrateTrackDistanceIpdCameraPx, ",")) %>%
        unnest(c(measured_list, requested_list, ipd_list)) %>%  # Expands all columns together
        mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
          calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list)),
          calibrateTrackDistanceIpdCameraPx = as.numeric(trimws(ipd_list))
        ) %>%
        select(-measured_list, -requested_list, -ipd_list)  # Remove temp lists
      
      if (nrow(t) > 0) {
        t <- t %>% mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
          calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm)
        ) %>%
          # Ensure IPD column exists even if it wasn't processed
          mutate(calibrateTrackDistanceIpdCameraPx = if("calibrateTrackDistanceIpdCameraPx" %in% names(.)) calibrateTrackDistanceIpdCameraPx else NA) %>%
          # Add row number first to preserve original order
          mutate(order = row_number())
      }
      df <- rbind(t, df)
    }
  }
  if (nrow(t) > 0) {
    if(all(is.na(df$calibrateTrackDistanceIpdCameraPx))) {
      df <- df %>% select(-calibrateTrackDistanceIpdCameraPx)
    } else {
      df <- df %>% filter(!is.na(calibrateTrackDistanceIpdCameraPx))
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
    if (!("distanceCalibrationJSON" %in% available_cols)) {
      next
    }

    t <- data_list[[i]] %>%
      select(experiment, participant, `_calibrateTrackDistance`, `_calibrateTrackDistancePupil`,
             viewingDistanceWhichEye, viewingDistanceWhichPoint, pxPerCm, screenWidthPx, screenHeightPx,
             calibrateTrackDistanceMeasuredCm, calibrateTrackDistanceRequestedCm, distanceCalibrationJSON) %>%
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

plot_eye_feet_position <- function(data_list) {
  eye_feet_data <- get_eye_feet_position_data(data_list)

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
    max_x_px <- max(c(eye_feet_data$left_x, eye_feet_data$right_x), na.rm = TRUE)
    max_y_px <- max(c(eye_feet_data$left_y, eye_feet_data$right_y), na.rm = TRUE)
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
  
  # Handle unlimited number of participants with cycling shapes
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Always use actual participant names - no artificial limits
      session_limited = factor(session_id)
    )

  n_sessions <- n_distinct(eye_feet_data$session_id)

  # Create statement for calibration parameters
  statement <- paste0('_calibrateTrackDistance = ', eye_feet_data$`_calibrateTrackDistance`[1], '\n',
                      '_calibrateTrackDistancePupil = ', eye_feet_data$`_calibrateTrackDistancePupil`[1], '\n',
                      'viewingDistanceWhichEye = ', eye_feet_data$viewingDistanceWhichEye[1], '\n',
                      'viewingDistanceWhichPoint = ', eye_feet_data$viewingDistanceWhichPoint[1])

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
          # Create a large pool of distinct shapes (both fillable and non-fillable)
          all_shapes <- c(21, 22, 23, 24, 25,    # fillable: circle, square, diamond, triangle up, triangle down
                         16, 17, 15, 18, 4, 8,    # solid: circle, triangle, square, diamond, plus, asterisk
                         0, 1, 2, 3, 5, 6, 7, 9, 10, 11, 12, 13, 14)  # additional shapes

          n_participants <- nlevels(eye_feet_data$session_limited)
          # Cycle through shapes if we have more participants than shapes
          rep(all_shapes, length.out = n_participants)
        },
        name = "",
        guide = guide_legend(
          ncol = 3,
          byrow = TRUE,
          key.spacing.y = unit(-0.05, "cm"),
          key.spacing.x = unit(0, "cm")
        )
      ) +
      # Flip Y-axis to make origin truly at top-left (Y increases downward)
      scale_y_reverse(limits = c(y_max, y_min), expand = c(0,0)) +
      # Lock aspect ratio with exact 50% margins
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
        legend.key.height = unit(0.8, "cm"),  # Shrunk for compact rows
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
        subtitle = "Measured over requested distance vs. foot position",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)",
        caption = "Rectangle shows screen boundaries. Points beyond 50% margin are clipped to plot area.\nOrigin (0,0) is at top-left corner of screen."
      ) +
      # Screen annotation (moved close to screen outline, same size as Y label, not bold)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.1,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, color = "black") +
      # Calibration parameters statement (bottom right corner)
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                          label = statement, hjust = 1, vjust = 0, size = 3, color = "black") +
      # Summary statistics (pushed further down to avoid clipping)
      annotate("text", x = x_min+1, y = y_min + 1,
               label = paste0("N = ", nrow(eye_feet_data), "\n",
                             "Sessions= ", n_sessions, "\n",
                             "Mean = ", round(mean(eye_feet_data$distance_ratio, na.rm = TRUE), 3), "\n",
                             "SD = ", round(sd(eye_feet_data$distance_ratio, na.rm = TRUE), 3)),
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

plot_foot_position_during_calibration <- function(data_list) {
  eye_feet_data <- get_eye_feet_position_data(data_list)

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

  # Create statement for calibration parameters
  statement <- paste0('_calibrateTrackDistance = ', eye_feet_data$`_calibrateTrackDistance`[1], '\n',
                      '_calibrateTrackDistancePupil = ', eye_feet_data$`_calibrateTrackDistancePupil`[1], '\n',
                      'viewingDistanceWhichEye = ', eye_feet_data$viewingDistanceWhichEye[1], '\n',
                      'viewingDistanceWhichPoint = ', eye_feet_data$viewingDistanceWhichPoint[1])

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
      # Calibration parameters statement (bottom right corner)
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"),
                          label = statement, hjust = 1, vjust = 0, size = 3, color = "black") +
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

plot_distance <- function(data_list,calibrateTrackDistanceCheckLengthSDLogAllowed) {
  distance <- get_measured_distance_data(data_list)
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('_calibrateTrackDistance = ', distance$`_calibrateTrackDistance`[1], '\n',
                      '_calibrateTrackDistancePupil = ', distance$`_calibrateTrackDistancePupil`[1], '\n',
                      'viewingDistanceWhichEye = ', distance$viewingDistanceWhichEye[1], '\n',
                      'viewingDistanceWhichPoint = ', distance$viewingDistanceWhichPoint[1])
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
      scale_x_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8), labels=as.integer) +
      scale_y_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8), labels=as.integer) + 
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
    scale_x_log10(limits = c(min_val, max_val),
                  breaks = scales::log_breaks(n=8),
                  expand = expansion(mult = c(0.05, 0.05)),
                  labels=as.integer) + 
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
  if ("calibrateTrackDistanceIpdCameraPx" %in% names(distance)) {
    ipd_data <- distance %>%
      filter(!is.na(calibrateTrackDistanceIpdCameraPx),
             !is.na(calibrateTrackDistanceRequestedCm)) %>%
      arrange(participant, order) %>%  # Ensure proper ordering
      mutate(
        # Add logarithmic jitter to requested distance for better visualization (unbiased for log scales)
        calibrateTrackDistanceRequestedCm_jitter = add_log_jitter(calibrateTrackDistanceRequestedCm, jitter_percent = 2, seed = 42)
      )

    # Get camera data for factorCameraPxCm
    camera_data <- get_cameraResolutionXY(data_list)

    if (nrow(ipd_data) > 0 && nrow(camera_data) > 0) {
      # Join with camera data to get factorCameraPxCm
      ipd_data <- ipd_data %>%
        left_join(camera_data %>% select(pavloviaParticipantID, factorCameraPxCm),
                  by = c("participant" = "pavloviaParticipantID"))

      # Create prediction data for each participant
      prediction_data <- ipd_data %>%
        filter(!is.na(factorCameraPxCm)) %>%  # factorCameraPxCm is already numeric
        group_by(participant) %>%
        summarize(
          factorCameraPxCm = first(factorCameraPxCm),
          .groups = "drop"
        ) %>%
        # Create prediction line across the x-range
        crossing(calibrateTrackDistanceRequestedCm = seq(min_val, max_val, length.out = 100)) %>%
        mutate(predictedIpdCameraPx = factorCameraPxCm / calibrateTrackDistanceRequestedCm)

      p3 <- ggplot() +
        # Add predicted lines first (so they appear behind the data)
        geom_line(data = prediction_data,
                  aes(x = calibrateTrackDistanceRequestedCm,
                      y = predictedIpdCameraPx,
                      color = participant,
                      group = participant),
                  linewidth = 1.5, alpha = 0.8, linetype = "solid") +
        # Measured data lines (made thinner)
        geom_line(data=ipd_data,
                  aes(x = calibrateTrackDistanceRequestedCm_jitter,
                      y = calibrateTrackDistanceIpdCameraPx,
                      color = participant,
                      group = participant),
                  linewidth = 0.5, alpha = 0.7) +
        # Measured data points
        geom_point(data=ipd_data,
                   aes(x = calibrateTrackDistanceRequestedCm_jitter,
                       y = calibrateTrackDistanceIpdCameraPx,
                       color = participant),
                   size = 2) +
        ggpp::geom_text_npc(aes(npcx="left",
                                npcy="top"),
                            label = paste0('N=', n_distinct(ipd_data$participant))) +
        scale_x_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8), labels=as.integer) +
        scale_y_log10(breaks = scales::log_breaks(n=8)) +
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
          legend.position = "right",
          legend.box = "vertical",
          legend.justification = "left",
          legend.text = element_text(size = 6),
          legend.spacing.y = unit(0, "lines"),
          legend.key.size = unit(0.4, "cm"),
          plot.margin = margin(5, 5, 5, 5, "pt")
        ) +
        labs(subtitle = 'IPD (camera px) vs. requested distance',
             x = 'Requested distance (cm)',
             y = 'IPD (camera px)',
             caption = 'Thick solid lines: predicted IPD = factorCameraPxCm/requestedDistance\nThin solid lines: measured data\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
    } else {
      p3 <- NULL
    }
  } else {
    p3 <- NULL
  }

  # Plot 4: Eye feet position vs distance error
  p4 <- plot_eye_feet_position(data_list)
  
  # Plot 4b: Foot position during calibration (colored by participant, no clipping)
  p4b <- plot_foot_position_during_calibration(data_list)
  
  # Plot 5: Calibrated vs. mean factorCameraPxCm
  p5 <- NULL
  if ("calibrateTrackDistanceIpdCameraPx" %in% names(distance) && nrow(distance) > 0) {
    # Get camera data for factorCameraPxCm
    camera_data <- get_cameraResolutionXY(data_list)
    
    if (nrow(camera_data) > 0) {
      # Join distance data with camera data
      factor_data <- distance %>%
        filter(!is.na(calibrateTrackDistanceIpdCameraPx),
               !is.na(calibrateTrackDistanceMeasuredCm)) %>%
        left_join(camera_data %>% select(pavloviaParticipantID, factorCameraPxCm),
                  by = c("participant" = "pavloviaParticipantID")) %>%
        filter(!is.na(factorCameraPxCm))
      
      if (nrow(factor_data) > 0) {
        # Calculate geometric mean per participant (session)
        # geoMeanFactorCameraPxCm = 10^mean(log10(measuredEyeToCameraCm * ipdCameraPx))
        geo_mean_data <- factor_data %>%
          mutate(
            product = calibrateTrackDistanceMeasuredCm * calibrateTrackDistanceIpdCameraPx
          ) %>%
          group_by(participant) %>%
          summarize(
            geoMeanFactorCameraPxCm = 10^mean(log10(product), na.rm = TRUE),
            .groups = "drop"
          )
        
        # Join back to get individual measurements with their session's geometric mean
        plot_data <- factor_data %>%
          left_join(geo_mean_data, by = "participant") %>%
          filter(!is.na(geoMeanFactorCameraPxCm), is.finite(geoMeanFactorCameraPxCm))
        
        if (nrow(plot_data) > 0) {
          # Calculate identical log scale limits
          all_values <- c(plot_data$factorCameraPxCm, plot_data$geoMeanFactorCameraPxCm)
          min_val_factor <- min(all_values, na.rm = TRUE) * 0.9
          max_val_factor <- max(all_values, na.rm = TRUE) * 1.1
          
          p5 <- ggplot(plot_data, aes(x = geoMeanFactorCameraPxCm, y = factorCameraPxCm)) +
            geom_point(aes(color = participant), size = 3, alpha = 0.8) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
            ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                                label = paste0('N=', n_distinct(plot_data$participant))) +
            scale_x_log10(limits = c(min_val_factor, max_val_factor), 
                          breaks = scales::log_breaks(n = 8)) +
            scale_y_log10(limits = c(min_val_factor, max_val_factor), 
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
            labs(subtitle = 'Calibrated vs. mean factorCameraPxCm',
                 x = 'Geometric mean factorCameraPxCm (per session)',
                 y = 'Factor Camera PxCm',
                 caption = 'Dashed line shows y=x (perfect agreement)\nGeometric mean = 10^mean(log10(measuredEyeToCameraCm × ipdCameraPx))')
        }
      }
    }
  }
  
  # Plot 6: Calibrated over mean factorCameraPxCm vs. spot diameter
  p6 <- NULL
  if ("calibrateTrackDistanceIpdCameraPx" %in% names(distance) && nrow(distance) > 0) {
    # Get camera data for factorCameraPxCm
    camera_data <- get_cameraResolutionXY(data_list)
    # Get blindspot diameter data
    blindspot_data <- get_calibrateTrackDistanceBlindspotDiameterDeg(data_list)
    
    if (nrow(camera_data) > 0 && nrow(blindspot_data) > 0) {
      # Join distance data with camera data
      factor_data <- distance %>%
        filter(!is.na(calibrateTrackDistanceIpdCameraPx),
               !is.na(calibrateTrackDistanceMeasuredCm)) %>%
        left_join(camera_data %>% select(pavloviaParticipantID, factorCameraPxCm),
                  by = c("participant" = "pavloviaParticipantID")) %>%
        filter(!is.na(factorCameraPxCm))
      
      if (nrow(factor_data) > 0) {
        # Calculate geometric mean per participant (session)
        geo_mean_data <- factor_data %>%
          mutate(
            product = calibrateTrackDistanceMeasuredCm * calibrateTrackDistanceIpdCameraPx
          ) %>%
          group_by(participant) %>%
          summarize(
            geoMeanFactorCameraPxCm = 10^mean(log10(product), na.rm = TRUE),
            .groups = "drop"
          )
        
        # Join with blindspot data and calculate ratio
        plot_data <- factor_data %>%
          left_join(geo_mean_data, by = "participant") %>%
          left_join(blindspot_data, by = "participant") %>%
          filter(!is.na(geoMeanFactorCameraPxCm), 
                 is.finite(geoMeanFactorCameraPxCm),
                 !is.na(`_calibrateTrackDistanceBlindspotDiameterDeg`)) %>%
          mutate(
            ratio = factorCameraPxCm / geoMeanFactorCameraPxCm,
            spotDeg = `_calibrateTrackDistanceBlindspotDiameterDeg`
          ) %>%
          filter(is.finite(ratio))
        
        if (nrow(plot_data) > 0) {
          # Calculate log scale limits
          x_min <- min(plot_data$spotDeg, na.rm = TRUE) * 0.9
          x_max <- max(plot_data$spotDeg, na.rm = TRUE) * 1.1
          y_min <- min(plot_data$ratio, na.rm = TRUE) * 0.9
          y_max <- max(plot_data$ratio, na.rm = TRUE) * 1.1
          
          p6 <- ggplot(plot_data, aes(x = spotDeg, y = ratio)) +
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
            labs(subtitle = 'Calibrated over mean factorCameraPxCm vs. spot diameter',
                 x = 'Spot diameter (deg)',
                 y = 'Factor Camera PxCm over geometric mean',
                 caption = 'Dashed line shows y=1 (perfect agreement with session mean)\nGeometric mean = 10^mean(log10(measuredEyeToCameraCm × ipdCameraPx))')
        }
      }
    }
  }
  
  return(list(
    credit_card_vs_requested = p1,
    credit_card_fraction = p2,
    ipd_vs_requested = p3,
    eye_feet_position = p4,
    foot_position_calibration = p4b,
    calibrated_vs_mean = p5,
    calibrated_over_mean_vs_spot = p6
  ))
}

plot_sizeCheck <- function(data_list, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  sizeCheck <- get_sizeCheck_data(data_list)
  
  statement <- paste0('_calibrateTrackDistance = ', sizeCheck$`_calibrateTrackDistance`[1], '\n',
                      '_calibrateTrackDistancePupil = ', sizeCheck$`_calibrateTrackDistancePupil`[1], '\n',
                      'viewingDistanceWhichEye = ', sizeCheck$viewingDistanceWhichEye[1], '\n',
                      'viewingDistanceWhichPoint = ', sizeCheck$viewingDistanceWhichPoint[1])
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
      avg_estimated = 10^mean(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
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
      avg_estimated=10^mean(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
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
      geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) + 
      scale_color_manual(values= colorPalette) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1)), 
                         breaks = function(x) seq(0, ceiling(max(x)), by = 1)) + 
      scale_x_log10(limits = c(x_min, x_max),breaks = scales::log_breaks(n=8)) +
      annotation_logticks(sides = "b") +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(sdLogDensity_data$participant))) + 
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
      geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement, size = 2) + 
      scale_color_manual(values = colorPalette) +
      scale_y_continuous(limits = c(0, max_count + 1), expand = expansion(mult = c(0, 0.1)), breaks = function(x) seq(0, ceiling(max(x)), by = 2)) + 
      scale_x_log10(limits=c(minX, maxX),
                    breaks = scales::log_breaks(n=8)) +
      annotation_logticks(sides = "b", 
                          size = 0.3, 
                          alpha = 0.7, 
                          short = unit(0.1, "cm"),
                          mid = unit(0.15, "cm"), 
                          long = unit(0.2, "cm")) +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(ruler_dotplot$participant))) + 
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
                        label = paste0('N=', n_distinct(sizeCheck_avg$participant))) + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) + 
    scale_x_log10(breaks = scales::log_breaks(n=8)) +
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
                        label = paste0("N=", dplyr::n_distinct(sdLogDensity_data$participant))) +
    ggpp::geom_text_npc(aes(npcx="right", npcy="bottom"), label = statement) +
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
  
  
  
  return(list(
    density_vs_length = p1,
    density_ratio_vs_sd = p2,
    sd_hist = h1,
    ruler_hist = h2
  ))
}

plot_distance_production <- function(data_list, participant_info,calibrateTrackDistanceCheckLengthSDLogAllowed) {
  distance <- get_measured_distance_data(data_list)
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('_calibrateTrackDistance = ', distance$`_calibrateTrackDistance`[1], '\n',
                      '_calibrateTrackDistancePupil = ', distance$`_calibrateTrackDistancePupil`[1], '\n',
                      'viewingDistanceWhichEye = ', distance$viewingDistanceWhichEye[1], '\n',
                      'viewingDistanceWhichPoint = ', distance$viewingDistanceWhichPoint[1])
        
  
  if (nrow(distance) == 0) {return(NULL)}
  
  # Calculate density ratio data (same as credit card plot's sdLogDensity_data calculation)
  if (nrow(sizeCheck) > 0) {
    densityRatio_data <- sizeCheck %>%
      group_by(participant, pxPerCm) %>%
      summarize(
        avg_estimated = 10^mean(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
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
      scale_x_log10(limits = c(min_val, max_val),
                    breaks = scales::log_breaks(n=8)) +
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
    maxFrac <- max(1.5, ceiling(distance_avg$production_fraction * 10) / 10)
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
      scale_x_log10(limits = c(min_val, max_val),
                    breaks = scales::pretty_breaks(n=8),
                    expand = expansion(mult = c(0.05, 0.05))) +
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
      scale_x_log10(limits = c(min_val, max_val),breaks = scales::log_breaks(n=8), labels=as.integer) +
      scale_y_log10(limits = c(min_val, max_val),breaks = scales::log_breaks(n=8), labels=as.integer) +
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
      scale_x_log10(limits = c(min_val, max_val),
                    breaks = scales::log_breaks(n=8),
                    expand = expansion(mult = c(0.05, 0.05)),
                    labels=as.integer) +
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
        x_min <- max(1, min(error_vs_object_data$objectLengthCm) * 0.8)
        x_max <- max(error_vs_object_data$objectLengthCm) * 1.2
        y_min <- max(0.1, min(error_vs_object_data$production_fraction) * 0.8)
        y_max <- min(2.0, max(error_vs_object_data$production_fraction) * 1.2)

        p5 <- ggplot(error_vs_object_data, aes(x = objectLengthCm, y = production_fraction)) +
          geom_point(aes(color = participant), size = 3, alpha = 0.8) +
          geom_hline(yintercept = 1, linetype = "dashed", color = "red", alpha = 0.7) +
          ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                              label = paste0('N=', n_distinct(error_vs_object_data$participant))) +
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
          coord_fixed() +
          labs(subtitle = 'Error vs. object size',
               x = 'Object length (cm)',
               y = 'Production-measured over requested distance',
               caption = 'Red dashed line shows perfect accuracy (ratio = 1.0)')
      }
    }
  }

  # Plot 6: Error vs. blindspot diameter (spotDeg)
  blindspot_data <- get_calibrateTrackDistanceBlindspotDiameterDeg(data_list)
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

      p6 <- ggplot(error_vs_blindspot_data, aes(x = spotDeg, y = production_fraction)) +
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
   
  
  
  return(list(
    production_vs_requested = p1,
    production_fraction = p2,
    raw_production_vs_requested = p3,
    individual_production_fraction = p4,
    error_vs_object_size = p5,
    error_vs_blindspot_diameter = p6
  ))
}

objectCm_hist <- function(participant_info) {
  dt <- participant_info %>%
    mutate(
      # Clean the objectLengthCm strings before converting to numeric
      objectLengthCm_clean = trimws(gsub('["\']', '', objectLengthCm)),
      objectLengthCm = as.numeric(objectLengthCm_clean)
    ) %>%
    filter(!is.na(objectLengthCm)) 
  if (nrow(dt) == 0) return(NULL)
  
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
    geom_point(aes(x = bin_center, y = dot_y, color = PavloviaParticipantID), size = 3, alpha = 0.8) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = object', size = 2) + 
    scale_color_manual(values = colorPalette) +
    scale_y_continuous(limits = c(0, max_count + 1), 
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
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
    scale_y_continuous(limits = c(0, max_count_mean + 1),
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

  p2 <- ggplot(sd_dotplot, aes(x = sd)) +
    # Stacked colored points (dot plot style)
    geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = blindspot', size = 2) +
    scale_color_manual(values = colorPalette) +
    scale_x_continuous(breaks = scales::pretty_breaks(n = 8),
                       labels = scales::label_number()) +
    scale_y_continuous(limits = c(0, max_count_sd + 1),
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

  return(list(
    mean_plot = p1,
    sd_plot = p2
  ))
}

