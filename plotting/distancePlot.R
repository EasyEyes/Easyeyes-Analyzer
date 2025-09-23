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
    t <- data_list[[i]] %>%
      select(cameraResolutionXY, participant) %>%
      rename(Session = participant) %>% 
      distinct() %>%
      filter(!is.na(cameraResolutionXY), cameraResolutionXY != "")
    if (nrow(t) > 0) {
      df <- rbind(df, t)
    }
  }
  return(df %>% distinct)
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
      select(participant,
             calibrateTrackDistance,
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
  print('inside get_measured_distance_data')
  df <- tibble()
  if(length(data_list) == 0) {
    return(df)
  }
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm,
             calibrateTrackDistance,
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
  print('=== Inside get_eye_feet_position_data ===')
  df <- tibble()
  if(length(data_list) == 0) {
    print("No data_list provided")
    return(df)
  }
  
  for (i in 1:length(data_list)) {
    available_cols <- names(data_list[[i]])

    # Check if we have the required columns
    if (!("calibrateTrackDistanceEyeFeetXYPx" %in% available_cols)) {
      print("calibrateTrackDistanceEyeFeetXYPx column not found, skipping")
      next
    }
    
    cols_to_select <- c("experiment",
                        "participant",
                        "pxPerCm",
                        "screenWidthPx",
                        "screenHeightPx",
                       "calibrateTrackDistanceMeasuredCm", 
                       "calibrateTrackDistanceRequestedCm",
                       "calibrateTrackDistanceEyeFeetXYPx")
    
    t <- data_list[[i]] %>%
      select(all_of(cols_to_select)) %>%
      distinct() %>%
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

      tryCatch({
        print("=== PARSING EYE FEET DATA ===")
        
        # Try to parse as JSON first
        eye_feet_raw <- t$calibrateTrackDistanceEyeFeetXYPx[1]  # Should be same for all rows in this group
        print(paste("Raw eye feet string:", eye_feet_raw))
        
        # Manual parsing approach - extract all numbers
        coords_str <- gsub("\\[|\\]", "", eye_feet_raw)
  
        coords_numbers <- as.numeric(unlist(strsplit(coords_str, ",")))
        coords_numbers <- coords_numbers[!is.na(coords_numbers)]
        print(paste("Extracted", length(coords_numbers), "coordinate values"))
        print(paste("First 8 coordinates:", paste(coords_numbers[1:min(8, length(coords_numbers))], collapse=", ")))
        
        # Check if we have the right number of coordinates (4 per measurement: left_x, left_y, right_x, right_y)
        n_measurements <- length(coords_numbers) / 4

        if (length(coords_numbers) %% 4 != 0) {
          print(paste("WARNING: Expected multiple of 4 coordinates, got", length(coords_numbers)))
          return(tibble())  # Return empty if parsing fails
        }
        
        # Create coordinate data frame
        coords_df <- tibble(
          measurement_idx = rep(1:n_measurements, each = 4),
          coord_type = rep(c("left_x", "left_y", "right_x", "right_y"), n_measurements),
          coord_value = coords_numbers
        ) %>%
          pivot_wider(names_from = coord_type, values_from = coord_value)
        
        print("Coordinate data frame created:")
        print(head(coords_df))
        
        # Match measurements - if we have more coordinates than distance measurements, trim
        if (n_measurements > nrow(t)) {
          print(paste("Trimming coordinates to match", nrow(t), "distance measurements"))
          coords_df <- coords_df[1:nrow(t), ]
        } else if (n_measurements < nrow(t)) {
          print(paste("WARNING: Fewer coordinates than distance measurements"))
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
            # Calculate measured/requested ratio
            distance_ratio = calibrateTrackDistanceMeasuredCm / calibrateTrackDistanceRequestedCm
          )
        
      }, error = function(e) {
        print(paste("ERROR parsing eye feet data:", e$message))
        print("Returning empty data frame")
        return(tibble())
      })
      
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
        print(paste("Using default pxPerCm:", default_pxPerCm))
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
  
  print(paste("=== FINAL RESULTS ==="))
  print(paste("Total rows returned:", nrow(df)))
  if (nrow(df) > 0) {
    print("Summary statistics:")
    print(paste("X range (px):", min(df$avg_eye_x_px, na.rm=T), "to", max(df$avg_eye_x_px, na.rm=T)))
    print(paste("Y range (px):", min(df$avg_eye_y_px, na.rm=T), "to", max(df$avg_eye_y_px, na.rm=T)))
    print(paste("X range (cm):", round(min(df$avg_eye_x_cm, na.rm=T), 2), "to", round(max(df$avg_eye_x_cm, na.rm=T), 2)))
    print(paste("Y range (cm):", round(min(df$avg_eye_y_cm, na.rm=T), 2), "to", round(max(df$avg_eye_y_cm, na.rm=T), 2)))
    print(paste("Ratio range:", round(min(df$distance_ratio, na.rm=T), 3), "to", round(max(df$distance_ratio, na.rm=T), 3)))
  }
  
  return(df)
}

plot_eye_feet_position <- function(data_list) {
  print('=== Inside plot_eye_feet_position ===')
  eye_feet_data <- get_eye_feet_position_data(data_list)
  
  if (nrow(eye_feet_data) == 0) {
    print("No eye feet position data available - returning NULL")
    return(NULL)
  }
  
  # Filter out rows with invalid coordinates
  eye_feet_data <- eye_feet_data %>%
    filter(!is.na(left_x), !is.na(left_y), !is.na(right_x), !is.na(right_y),
           !is.na(avg_eye_x_cm), !is.na(avg_eye_y_cm), !is.na(distance_ratio))
  
  if (nrow(eye_feet_data) == 0) {
    print("No valid eye feet coordinate data available after filtering")
    return(NULL)
  }
  
  print(paste("Plotting", nrow(eye_feet_data), "eye feet measurements"))
  
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
    print(paste("Using actual screen dimensions from data:", round(screen_width_cm, 1), "x", round(screen_height_cm, 1), "cm"))
  } else {
    # Fallback: estimate from eye position data (may underestimate actual screen size)
    max_x_px <- max(c(eye_feet_data$left_x, eye_feet_data$right_x), na.rm = TRUE)
    max_y_px <- max(c(eye_feet_data$left_y, eye_feet_data$right_y), na.rm = TRUE)
    screen_width_cm <- max_x_px / typical_pxPerCm
    screen_height_cm <- max_y_px / typical_pxPerCm
    print(paste("Estimated screen dimensions from eye positions:", round(screen_width_cm, 1), "x", round(screen_height_cm, 1), "cm"))
    print("WARNING: This may underestimate actual screen size")
  }
  
  print(paste("Estimated screen size:", round(screen_width_cm, 1), "x", round(screen_height_cm, 1), "cm"))
  print(paste("Using pxPerCm:", round(typical_pxPerCm, 1)))
  
  # Calculate pixel ranges for debugging
  if (nrow(eye_feet_data) > 0) {
    min_x_px <- min(c(eye_feet_data$left_x, eye_feet_data$right_x), na.rm = TRUE)
    max_x_px <- max(c(eye_feet_data$left_x, eye_feet_data$right_x), na.rm = TRUE)
    min_y_px <- min(c(eye_feet_data$left_y, eye_feet_data$right_y), na.rm = TRUE)
    max_y_px <- max(c(eye_feet_data$left_y, eye_feet_data$right_y), na.rm = TRUE)
    print(paste("Eye position ranges: X", min_x_px, "to", max_x_px, "px, Y", min_y_px, "to", max_y_px, "px"))
  }
  
  # Add 50% margin around screen as specified in Trello
  # For W=30.20, H=19.61, limits should be x:[-15.10, 45.30], y:[-9.81, 29.42]
  x_margin_cm <- 0.5 * screen_width_cm
  y_margin_cm <- 0.5 * screen_height_cm
  x_min <- -x_margin_cm
  x_max <- screen_width_cm + x_margin_cm  
  y_min <- -y_margin_cm
  y_max <- screen_height_cm + y_margin_cm
  
  print(paste("Plot limits: X", round(x_min, 1), "to", round(x_max, 1), "cm, Y", round(y_min, 1), "to", round(y_max, 1), "cm"))
  
  # Prepare data for plotting
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Clip points that fall beyond the margin (as requested)
      foot_x_cm_clipped = pmax(x_min, pmin(x_max, avg_eye_x_cm)),
      foot_y_cm_clipped = pmax(y_min, pmin(y_max, avg_eye_y_cm)),
      # Use continuous ratio for color mapping as specified
      ratio_continuous = pmax(0.5, pmin(2.0, distance_ratio)),
      # Create session identifier for shapes - use actual participant names like other plots
      session_id = as.character(participant)
    )
  
  # Debug session data
  print("=== DEBUG SESSION DATA ===")
  print(paste("Session column exists:", "session_id" %in% names(eye_feet_data)))
  print(paste("Session_id class:", class(eye_feet_data$session_id)))
  print(paste("Session_id values:", paste(unique(eye_feet_data$session_id), collapse = ", ")))
  print(paste("Number of unique sessions:", n_distinct(eye_feet_data$session_id)))
  print(paste("Any NA in session_id:", any(is.na(eye_feet_data$session_id))))
  
  # Handle unlimited number of participants with cycling shapes
  eye_feet_data <- eye_feet_data %>%
    mutate(
      # Always use actual participant names - no artificial limits
      session_limited = factor(session_id)
    )
  
  # Debug session_limited data
  print("=== DEBUG SESSION_LIMITED DATA ===")
  print(paste("Session_limited class:", class(eye_feet_data$session_limited)))
  print(paste("Session_limited levels:", paste(levels(eye_feet_data$session_limited), collapse = ", ")))
  print(paste("Session_limited values:", paste(unique(eye_feet_data$session_limited), collapse = ", ")))
  print(paste("Number of levels in session_limited:", nlevels(eye_feet_data$session_limited)))
  print(paste("Any NA in session_limited:", any(is.na(eye_feet_data$session_limited))))
  
  # Debug other aesthetic variables
  print("=== DEBUG OTHER AESTHETICS ===")
  print(paste("foot_x_cm_clipped range:", round(min(eye_feet_data$foot_x_cm_clipped, na.rm=T), 2), "to", round(max(eye_feet_data$foot_x_cm_clipped, na.rm=T), 2)))
  print(paste("foot_y_cm_clipped range:", round(min(eye_feet_data$foot_y_cm_clipped, na.rm=T), 2), "to", round(max(eye_feet_data$foot_y_cm_clipped, na.rm=T), 2)))
  print(paste("ratio_continuous class:", class(eye_feet_data$ratio_continuous)))
  print(paste("ratio_continuous range:", round(min(eye_feet_data$ratio_continuous, na.rm=T), 3), "to", round(max(eye_feet_data$ratio_continuous, na.rm=T), 3)))
  print(paste("Any NA in ratio_continuous:", any(is.na(eye_feet_data$ratio_continuous))))
  
  n_sessions <- n_distinct(eye_feet_data$session_id)
  print(paste("Final number of sessions:", n_sessions))
  
  # Create the plot directly without test plots
  print("=== CREATING FINAL PLOT ===")
  
  tryCatch({
    print("Creating eye feet position plot...")
    
    p <- ggplot(eye_feet_data, aes(x = foot_x_cm_clipped, y = foot_y_cm_clipped)) +
      # Add screen boundary rectangle
      geom_rect(aes(xmin = 0, xmax = screen_width_cm, 
                    ymin = 0, ymax = screen_height_cm),
                fill = NA, color = "black", linewidth = 1.2, alpha = 0.7) +
      # Add points with fill aesthetic to avoid server color conflicts
      geom_point(aes(fill = ratio_continuous, shape = session_limited), 
                 size = 4, alpha = 0.9, color = "black", stroke = 0.2) +
      # Log-scaled continuous fill scale
      scale_fill_gradientn(
        colors = c("#3B4CC0", "#89A1F0", "#FDE725", "#F07E26", "#B2182B"),
        values = scales::rescale(log10(c(0.5, 0.85, 1.0, 1.3, 2.0))),
        limits = c(0.5, 2.0),
        trans = "log10",
        oob = scales::squish,
        name = "Measured /\nRequested"
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
        name = ""
      ) +
      # Flip Y-axis to make origin truly at top-left (Y increases downward)
      scale_y_reverse(limits = c(y_max, y_min), expand = c(0,0)) +
      # Lock aspect ratio with exact 50% margins
      coord_fixed(xlim = c(x_min, x_max), expand = FALSE) +
      theme_bw() +
      theme(
        legend.position = "right",
        legend.box = "vertical",
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9),
        legend.key.height = unit(0.8, "cm"),
        legend.key.width = unit(0.5, "cm"),
        legend.margin = margin(l = 10, r = 10, t = 5, b = 5),
        plot.margin = margin(t = 20, r = 10, b = 20, l = 20, "pt")  # Add plot margins to prevent text clipping
      ) +
      # Labels - set these LAST to ensure they stick
      labs(
        subtitle = "Measured:requested distance vs. foot position",
        x = "Eye foot X (cm)",
        y = "Eye foot Y (cm)",
        caption = "Rectangle shows screen boundaries. Points beyond 50% margin are clipped to plot area.\nOrigin (0,0) is at top-left corner of screen."
      ) +
      # Screen annotation (adjusted for flipped Y-axis with more margin)
      annotate("text", x = screen_width_cm/2, y = -y_margin_cm*0.7,
               label = "Screen", hjust = 0.5, vjust = 0.5, size = 4, fontface = "bold", color = "black") +
      # Summary statistics (pushed further down to avoid clipping)
      annotate("text", x = x_min + x_margin_cm*0.15, y = y_min + y_margin_cm*0.8,
               label = paste0("N = ", nrow(eye_feet_data), "\n",
                             "Sessions= ", n_sessions, "\n",
                             "Mean = ", round(mean(eye_feet_data$distance_ratio, na.rm = TRUE), 3)),
               hjust = 0, vjust = 0, size = 4)
    
    print("Plot created successfully")
    
  }, error = function(e) {
    print(paste("ERROR in plot creation:", e$message))
    print("Full error:")
    print(e)
    print("Traceback:")
    print(traceback())
    return(NULL)
  })
  
  # Return the plot or NULL if creation failed
  if(exists("p") && !is.null(p)) {
    return(p)
  } else {
    print("Plot creation failed, returning NULL")
    return(NULL)
  }
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
  print('inside plot_distance')
  distance <- get_measured_distance_data(data_list)
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('calibrateTrackDistance = ', distance$calibrateTrackDistance[1])
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
      scale_x_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8)) +
      scale_y_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8)) + 
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
                  expand = expansion(mult = c(0.05, 0.05))) + 
    scale_y_log10(limits = c(minFrac, maxFrac),
                   breaks = scales::log_breaks(n=8)) + 
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

    if (nrow(ipd_data) > 0) {
      p3 <- ggplot() +
        geom_line(data=ipd_data,
                  aes(x = calibrateTrackDistanceRequestedCm_jitter,
                      y = calibrateTrackDistanceIpdCameraPx,
                      color = participant,
                      group = participant), alpha = 0.7) +
        geom_point(data=ipd_data,
                   aes(x = calibrateTrackDistanceRequestedCm_jitter,
                       y = calibrateTrackDistanceIpdCameraPx,
                       color = participant),
                   size = 2) +
        ggpp::geom_text_npc(aes(npcx="left",
                                npcy="top"),
                            label = paste0('N=', n_distinct(ipd_data$participant))) +
        scale_x_log10(limits = c(min_val, max_val), breaks = scales::log_breaks(n=8)) +
        scale_y_log10(breaks = scales::log_breaks(n=8)) +
        scale_color_manual(values= colorPalette) +
        ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement) +
        guides(color = guide_legend(
          ncol = 3,
          title = "",
          override.aes = list(size = 2),
          keywidth = unit(1.2, "lines"),
          keyheight = unit(0.8, "lines")
        )) +
        labs(subtitle = 'IPD (camera px) vs. requested distance',
             x = 'Requested distance (cm)',
             y = 'IPD (camera px)',
             caption = 'Lines connect measurements from the same session.\nLogarithmic horizontal jitter added to reduce overlap (unbiased for log scales)')
    } else {
      p3 <- NULL
    }
  } else {
    p3 <- NULL
  }

  # Plot 4: Eye feet position vs distance error
  print("=== CREATING EYE FEET POSITION PLOT ===")
  p4 <- plot_eye_feet_position(data_list)

  return(list(
    credit_card_vs_requested = p1,
    credit_card_fraction = p2,
    ipd_vs_requested = p3,
    eye_feet_position = p4
  ))
}

plot_sizeCheck <- function(data_list, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  sizeCheck <- get_sizeCheck_data(data_list)
  
  statement <- paste0('calibrateTrackDistance = ', sizeCheck$calibrateTrackDistance[1])
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
            legend.text = element_text(size = 8, margin = margin(t = -10, b = -10)),
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
    print('=====get ruler length histogram======')
    print(ruler)
    maxX = 1.05 * max(ruler$lengthCm) 
    minX = 0.95 * min(ruler$lengthCm)
    # Create dot plot style like SD histogram
    bin_width <- (max(ruler$lengthCm) - min(ruler$lengthCm)) / 20  # Adjust bin width based on data range
    
    # Debug ruler histogram
    print(paste("Ruler bin_width:", bin_width))
    print(paste("Ruler data range:", min(ruler$lengthCm), "to", max(ruler$lengthCm)))
    
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
    
    # Debug ruler dotplot data
    print("Ruler dotplot data:")
    print(ruler_dotplot)
    
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
            legend.text = element_text(size = 8, margin = margin(t = -10, b = -10)),
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

plot_distance_production <- function(data_list, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  distance <- get_measured_distance_data(data_list)
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('calibrateTrackDistance = ', distance$calibrateTrackDistance[1])
  
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
  
  # IDENTICAL scale limits as credit card plot - use JITTERED values
  # Use appropriate scale limits for cleaner axis display
  min_val <- 5 * floor(min(c(distance_avg$calibrateTrackDistanceRequestedCm_jitter, 
                              distance_avg$avg_measured), na.rm = TRUE) / 5)
  min_val = max(10, min_val)
  max_val <- 5 * ceiling(max(c(distance_avg$calibrateTrackDistanceRequestedCm_jitter, 
                                distance_avg$avg_measured), na.rm = TRUE) / 5)

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
                    breaks = scales::log_breaks()) +
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
      scale_x_log10(limits = c(min_val, max_val),breaks = scales::log_breaks(n=8)) +
      scale_y_log10(limits = c(min_val, max_val),breaks = scales::log_breaks(n=8)) +
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
           y = 'production-measured distance (cm)',
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
                    expand = expansion(mult = c(0.05, 0.05))) +
      scale_y_log10(limits = c(minFrac,maxFrac),
                    breaks = scales::log_breaks(n=8)) +
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
           y = 'Individual production-measured over requested distance',
           caption = 'Lines connect measurements from the same session.\nHorizontal jitter added to reduce overlap')
  } else {
    p4 <- NULL
  }

 
  
  return(list(
    production_vs_requested = p1,
    production_fraction = p2,
    raw_production_vs_requested = p3,
    individual_production_fraction = p4
  ))
}

objectCm_hist <- function(participant_info) {
  dt <- participant_info %>% filter(!is.na(objectLengthCm)) %>% mutate(objectLengthCm = as.numeric(objectLengthCm))
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
          legend.text = element_text(size = 8, margin = margin(t = -10, b = -10)),
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
          legend.text = element_text(size = 8, margin = margin(t = -10, b = -10)),
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
          legend.text = element_text(size = 8, margin = margin(t = -10, b = -10)),
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

