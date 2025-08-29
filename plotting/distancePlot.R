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
  df <- tibble()
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm,
             calibrateTrackDistance) %>%
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
          calibrateTrackDistanceRequestedCm = gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm)
        ) %>%
        # Separate both columns together while keeping row-wise structure
        mutate(measured_list = strsplit(calibrateTrackDistanceMeasuredCm, ","), 
               requested_list = strsplit(calibrateTrackDistanceRequestedCm, ",")) %>%
        unnest(c(measured_list, requested_list)) %>%  # Expands both columns together
        mutate(
          calibrateTrackDistanceMeasuredCm = as.numeric(trimws(measured_list)),
          calibrateTrackDistanceRequestedCm = as.numeric(trimws(requested_list))
        ) %>%
        select(-measured_list, -requested_list)  # Remove temp lists
      
      df <- rbind(t, df)
    }
  }
  if (nrow(df) > 0) {
    df <- df %>% mutate(
      calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
      calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm)
    )
  }
  
  return(df)
}

plot_distance <- function(data_list,calibrateTrackDistanceCheckLengthSDLogAllowed) {
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
  
  
  # Check for NA values after conversion
  if (sum(is.na(distance$calibrateTrackDistanceMeasuredCm)) == nrow(distance) ||
      sum(is.na(distance$calibrateTrackDistanceRequestedCm)) == nrow(distance)) {
    return(NULL)
  }
  
  # Ensure we have at least one valid row
  distance <- na.omit(distance)  # Remove rows with NA values
  if (nrow(distance) == 0) {
    return(NULL)
  }
  
  # Average Measured Distance per Participant per Requested Distance
  distance_avg <- distance %>%
    group_by(participant, calibrateTrackDistanceRequestedCm) %>%
    summarize(
      avg_measured = mean(calibrateTrackDistanceMeasuredCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add consistent random horizontal jitter to x-axis variable
      calibrateTrackDistanceRequestedCm_jitter = {
        set.seed(42) # Consistent seed for reproducible jitter across both plots
        calibrateTrackDistanceRequestedCm * runif(n(), min = 0.95, max = 1.05)
      }
    ) 
  if (nrow(sdLogDensity_data) > 0) {
    distance_avg <- distance_avg %>% 
      left_join(sdLogDensity_data, by = "participant")
  } else {
    distance_avg <- distance_avg %>% mutate(reliableBool = TRUE)
  }
  
  # Calculate the ratio Y/X for the second plot
  distance_avg <- distance_avg %>%
    mutate(
      credit_card_fraction = avg_measured / calibrateTrackDistanceRequestedCm
    )

  # Calculate mean and SD of the ratio for reporting
  mean_fraction <- mean(distance_avg$credit_card_fraction, na.rm = TRUE)
  sd_fraction <- sd(distance_avg$credit_card_fraction, na.rm = TRUE)
  
  # Format for display
  mean_formatted <- format(round(mean_fraction, 3), nsmall = 3)
  sd_formatted <- format(round(sd_fraction, 3), nsmall = 3)

     # Use appropriate scale limits for cleaner axis display
   min_val <- 8   # Round down from ~9
   max_val <- 70  # Round up from ~57 for better visual spacing
   
  # Plot 1: Credit-card-measured vs requested distance
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
    scale_x_log10(limits = c(min_val, max_val), breaks = c(10, 20, 30, 50, 100)) + 
    scale_y_log10(limits = c(min_val, max_val), breaks = c(10, 20, 30, 50, 100)) + 
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
         y = 'Credit-card-measured distance (cm)') +
    theme(  # Smaller legend text (was default ~10-12), left-aligned # Smaller legend title
      axis.title = element_text(size = 10),        # Smaller axis titles (was default ~12-14)
      axis.text = element_text(size = 9),          # Smaller axis text (was default ~10-12)
      plot.title = element_text(size = 12),        # Smaller plot title (was default ~14-16)
      legend.position = "bottom",                   # Move legend to bottom to prevent right cutoff
      legend.box = "horizontal"                     # Ensure horizontal layout
    )
  
  # Plot 2: Credit-card-measured as fraction of requested distance
  p2 <- ggplot() + 
    geom_line(data=distance_avg, 
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                  y = credit_card_fraction,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
    geom_point(data=distance_avg, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                   y = credit_card_fraction,
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
    scale_x_log10(breaks = c(10, 20, 30, 50, 100)) + 
    scale_y_continuous(breaks = seq(0.5, 2.0, by = 0.1)) + 
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
    labs(subtitle = 'Credit-card-measured as fraction of requested distance',
         x = 'Requested distance (cm)',
         y = 'Credit-card-measured as fraction of requested distance') +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      plot.title = element_text(size = 12),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  return(list(
    credit_card_vs_requested = p1,
    credit_card_fraction = p2
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
  
  # Ensure we have at least one valid row
  sizeCheck <- na.omit(sizeCheck)  # Remove rows with NA values
  if (nrow(sizeCheck) == 0) {return(NULL)}
  
  # Average Estimated PxPerCm per Participant per Requested Size
  sizeCheck_avg <- sizeCheck %>%
    group_by(participant, SizeCheckRequestedCm) %>%
    summarize(
      avg_estimated = 10^mean(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add random horizontal jitter to x-axis variable
      SizeCheckRequestedCm_jitter = SizeCheckRequestedCm * runif(n(), min = 0.95, max = 1.05)
    )
  
  # Determine scale limits
  min_val <- min(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated))
  max_val <- max(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated))
  
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
      scale_y_continuous(expand = expansion(mult = c(0, 0.5))) + 
      scale_x_log10(limits = c(x_min, x_max)) +
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
      theme( legend.key.size = unit(0.3, "cm"),        # Increase key size for dots
             legend.title = element_text(size=6),
             legend.text = element_text(size=8, margin = margin(l=0.1, r=0, t = -1, b = -1)),
             legend.box.margin = margin(l=-6,r=0,t=2,b=1,"mm"),
             legend.box.spacing = unit(0.2, "cm"),          # Add some spacing between legend elements
             legend.spacing.y = unit(0.1, "cm"),            # Add vertical spacing
             legend.spacing.x = unit(0.2, "cm"),            # Add horizontal spacing
             legend.key.height = unit(0.3, "cm"),           # Match keyheight from guide_legend
             legend.key.width = unit(0.3, "cm"),            # Match keywidth from guide_legend
             legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
             legend.margin = margin(l=0, r=0, t=0, b=0, unit = 'mm'),
             legend.position = "top", 
             legend.box = "vertical", 
             legend.justification='left',
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             panel.background = element_blank(), 
             axis.title = element_text(size = 12),        # Reduced from 12
             axis.text = element_text(size = 12),         # Reduced from 12  
             axis.line = element_line(colour = "black"),
             axis.text.y = element_text(size = 10),       # Reduced from 10
             plot.title = element_text(size=7,
                                       hjust = 0,
                                       margin = margin(b = 0)),
             plot.title.position = "plot",
             plot.subtitle = element_text(size=14,       # Increased for dot plots
                                          hjust = 0,
                                          margin = margin(t = 0)),
             plot.caption = element_text(size=10),        # Reduced from 10
             plot.margin = margin(
               t = 0.2,                                  # Increased top margin for legend
               r = 0.1,
               b = 0,
               l = 0.1,
               "inch"
             ),
             strip.text = element_text(size = 12))
    
  } else {
    h1 <- NULL
  }
  if (nrow(ruler) > 0) {
    h2 <- ggplot(ruler, aes(x = lengthCm)) +
      geom_histogram(fill="gray80") + 
      ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = statement,size = 2) + 
      scale_x_log10(breaks = c(10, 30, 100, 300)) +
      annotation_logticks(sides = "b", size = 0.3, alpha = 0.7, short = unit(0.1, "cm"), mid = unit(0.15, "cm"), long = unit(0.2, "cm")) +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(ruler$participant))
                          ) + 
      labs(subtitle = 'Histogram of ruler \nlength (cm)',
           x = "Ruler length (cm)",
           y = "Count") +
      theme(axis.text.x = element_text(angle = 0, hjust = 0.5))
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
    scale_x_log10(breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 30, 40, 50)) +
    scale_y_log10(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500),
                  limits = c(ymin,ymax)) +
    annotation_logticks(size = 0.3, alpha = 0.7, short = unit(0.1, "cm"), mid = unit(0.15, "cm"), long = unit(0.2, "cm")) + 
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
  
  xmax_allowed <- as.numeric(calibrateTrackDistanceCheckLengthSDLogAllowed)
  
  # guard: keep the band within the panel
  xmax_band <- max(min(xmax_allowed, max(sdLogDensity_data$sdLogDensity, na.rm = TRUE)), x_left)
  
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
             xmax = xmax_band, 
             ymin = y_min_panel, 
             ymax = y_max_panel, 
             fill = "lightgreen", 
             alpha = 0.3) +
    geom_point(aes(x = sdLogDensity, y = ratio, color = participant), size = 2) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"),
                        label = paste0("N=", dplyr::n_distinct(sdLogDensity_data$participant))) +
    ggpp::geom_text_npc(aes(npcx="right", npcy="bottom"), label = statement) +
    scale_color_manual(values = colorPalette) +
    scale_x_log10(limits = c(x_left, NA), expand = c(0, 0)) +   # left edge flush
    scale_y_log10(limits = c(y_min_panel, y_max_panel), expand = c(0, 0)) +
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
  
  # Ensure we have at least one valid row
  distance <- na.omit(distance)
  if (nrow(distance) == 0) {return(NULL)}
  
  # Average Measured Distance per Participant per Requested Distance (SAME AS CREDIT CARD)
  distance_avg <- distance %>%
    group_by(participant, calibrateTrackDistanceRequestedCm) %>%
    summarize(
      creditCardMeasuredCm = mean(calibrateTrackDistanceMeasuredCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add consistent random horizontal jitter to x-axis variable (SAME AS CREDIT CARD)
      calibrateTrackDistanceRequestedCm_jitter = {
        set.seed(42) # IDENTICAL seed as credit card plot for identical jitter
        calibrateTrackDistanceRequestedCm * runif(n(), min = 0.95, max = 1.05)
      }
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
    )

  # Calculate mean and SD of the ratio for reporting
  mean_fraction <- mean(distance_avg$production_fraction, na.rm = TRUE)
  sd_fraction <- sd(distance_avg$production_fraction, na.rm = TRUE)
  
  # Format for display
  mean_formatted <- format(round(mean_fraction, 3), nsmall = 3)
  sd_formatted <- format(round(sd_fraction, 3), nsmall = 3)
  
  # IDENTICAL scale limits as credit card plot
  # Use appropriate scale limits for cleaner axis display
  min_val <- 8   # Round down from ~9
  max_val <- 70  # Round up from ~57 for better visual spacing
  
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
    scale_x_log10(limits = c(min_val, max_val), breaks = c(10, 20, 30, 50, 100)) + 
    scale_y_log10(limits = c(min_val, max_val), breaks = c(10, 20, 30, 50, 100)) + 
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
    labs(subtitle = 'Production-measured vs. requested distance',  # ONLY LABEL DIFFERENCE
         x = 'Requested distance (cm)',                              # SAME AS CREDIT CARD
         y = 'Production-measured distance (cm)') +
    theme(  # IDENTICAL THEME AS CREDIT CARD
      axis.title = element_text(size = 10),        # Same as credit card
      axis.text = element_text(size = 9),          # Same as credit card
      plot.title = element_text(size = 12),        # Same as credit card
      legend.position = "bottom",                   # Same as credit card
      legend.box = "horizontal"                     # Same as credit card
    )
  
  # Plot 2: Production-measured as fraction of requested distance
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
    scale_x_log10(breaks = c(10, 20, 30, 50, 100)) + 
    scale_y_continuous(breaks = seq(0.5, 2.0, by = 0.1)) + 
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
    labs(subtitle = 'Production-measured as fraction of requested distance',
         x = 'Requested distance (cm)',
         y = 'Production-measured as fraction of requested distance') +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 9),
      plot.title = element_text(size = 12),
      legend.position = "bottom",
      legend.box = "horizontal"
    )
  
  return(list(
    production_vs_requested = p1,
    production_fraction = p2
  ))
}

objectCm_hist <- function(participant_info) {
  dt <- participant_info %>% filter(!is.na(objectLengthCm)) %>% mutate(objectLengthCm = as.numeric(objectLengthCm))
  if (nrow(dt) == 0) return(NULL)
  p <- ggplot(dt, aes(x = objectLengthCm)) +
    geom_histogram(fill="gray80") + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "right", npcy = "bottom"), label = 'calibrateTrackDistance = object', size = 2) + 
    # scale_x_log10(breaks = c(10, 30, 100, 300)) +
    # annotation_logticks(sides = "b", size = 0.3, alpha = 0.7, 
    #                     short = unit(0.1, "cm"), 
    #                     mid = unit(0.15, "cm"), 
    #                     long = unit(0.2, "cm")) +
    ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                        label = paste0('N=', n_distinct(dt$PavloviaParticipantID))) + 
    labs(subtitle = 'Histogram of object \nlength (cm)',
         x = "Object length (cm)",
         y = "Count")
}

