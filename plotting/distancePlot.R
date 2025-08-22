# get_measured_distance_data <- function(data_list) {
#   print('inside get_measured_distance_data')
#   df <- tibble()
#   for (i in 1:length(data_list)) {
#     t <- data_list[[i]] %>%
#       select(participant,
#              calibrateTrackDistanceMeasuredCm,
#              calibrateTrackDistanceRequestedCm) %>% 
#       distinct() %>% 
#       filter(!is.na(calibrateTrackDistanceMeasuredCm),
#              !is.na(calibrateTrackDistanceRequestedCm),
#              calibrateTrackDistanceMeasuredCm != '',
#              calibrateTrackDistanceRequestedCm != '')
#     if (nrow(t) > 0) {
#       t <- t %>%
#         separate_rows(calibrateTrackDistanceMeasuredCm,sep=',') %>% 
#         separate_rows(calibrateTrackDistanceRequestedCm,sep=',')
#       df <- rbind(t, df)
#     }
#   }
#   
#   print('done get_measured_distance_data')
#   return(df)
# }

# get_measured_distance_data <- function(data_list) {
#   print('inside get_measured_distance_data')
#   print(data_list)
#   df <- tibble()
#   
#   for (i in 1:length(data_list)) {
#     t <- data_list[[i]] %>%
#       select(participant,
#              calibrateTrackDistanceMeasuredCm,
#              calibrateTrackDistanceRequestedCm) %>% 
#       distinct() %>%
#       filter(!is.na(calibrateTrackDistanceMeasuredCm),
#              !is.na(calibrateTrackDistanceRequestedCm),
#              calibrateTrackDistanceMeasuredCm != '',
#              calibrateTrackDistanceRequestedCm != '')
# 
#     
#     if (nrow(t) > 0) {
#       # Convert lists or JSON-like arrays into character vectors
#       t <- t %>%
#         mutate(
#           calibrateTrackDistanceMeasuredCm = ifelse(
#             grepl("\\[", calibrateTrackDistanceMeasuredCm),  # Detect JSON-like format
#             gsub("\\[|\\]|\"", "", calibrateTrackDistanceMeasuredCm), # Remove brackets and quotes
#             calibrateTrackDistanceMeasuredCm
#           ),
#           calibrateTrackDistanceRequestedCm = ifelse(
#             grepl("\\[", calibrateTrackDistanceRequestedCm),
#             gsub("\\[|\\]|\"", "", calibrateTrackDistanceRequestedCm),
#             calibrateTrackDistanceRequestedCm
#           )
#         ) %>%
#         separate_rows(calibrateTrackDistanceMeasuredCm, sep=",") %>% 
#         separate_rows(calibrateTrackDistanceRequestedCm, sep=",") %>%
#         mutate(
#           calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
#           calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm)
#         )
#       
#       df <- rbind(t, df)
#     }
#   }
#   
#   print('done get_measured_distance_data')
#   return(df)
# }
get_sizeCheck_data <- function(data_list) {
  print('inside get_sizeCheck_data')
  df <- tibble()
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant,
             calibrateTrackDistance,
             SizeCheckEstimatedPxPerCm,
             SizeCheckRequestedCm,
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
  

  print('done get_sizeCheck_data')
  return(df)
}

get_measured_distance_data <- function(data_list) {
  print('inside get_measured_distance_data')
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
 
  print('done get_measured_distance_data')
  return(df)
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
 
  
  # Check for NA values after conversion
  if (sum(is.na(distance$calibrateTrackDistanceMeasuredCm)) == nrow(distance) ||
      sum(is.na(distance$calibrateTrackDistanceRequestedCm)) == nrow(distance)) {
    print("Error: All values in one or both columns are NA after conversion.")
    return(NULL)
  }
  
  # Ensure we have at least one valid row
  distance <- na.omit(distance)  # Remove rows with NA values
  if (nrow(distance) == 0) {
    print("Error: No valid numeric data available after NA removal.")
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
      # Add random horizontal jitter to x-axis variable
      calibrateTrackDistanceRequestedCm_jitter = calibrateTrackDistanceRequestedCm * runif(n(), min = 0.95, max = 1.05)
    ) 
  if (nrow(sdLogDensity_data) > 0) {
    distance_avg <- distance_avg %>% 
      left_join(sdLogDensity_data, by = "participant")
  } else {
    distance_avg <- distance_avg %>% mutate(reliableBool = TRUE)
  }

  fit <- lm(log10(avg_measured) ~ log10(calibrateTrackDistanceRequestedCm), data = distance_avg)

  slope <- coef(fit)
  slope <- format(round(slope[['log10(calibrateTrackDistanceRequestedCm)']], 2), nsmall=2)
  corr <- cor(log10(distance_avg$calibrateTrackDistanceRequestedCm), log10(distance_avg$avg_measured))
  corr <- format(round(corr,2), nsmall=2)
  
  min_val <- min(c(distance_avg$calibrateTrackDistanceRequestedCm, distance_avg$avg_measured))
  max_val <- max(c(distance_avg$calibrateTrackDistanceRequestedCm, distance_avg$avg_measured))

    p <- ggplot() + 
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
                        label = paste0('N=', n_distinct(distance_avg$participant), '\n',
                                       'R=', corr, '\n',
                                       'slope=', slope)) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    scale_x_log10(limits = c(min_val, max_val)) + 
    scale_y_log10(limits = c(min_val, max_val)) + 
    scale_color_manual(values= colorPalette) + 
    ggpp::geom_text_npc(data = NULL, aes(npcx = "left", npcy = "bottom"), label = statement) + 
    guides(color = guide_legend(
      ncol = 3,  # More columns to fit more participants horizontally
      title = "",
      override.aes = list(size = 2),  # Smaller points in legend
      keywidth = unit(1.2, "lines"),  # Reduce key width
      keyheight = unit(0.8, "lines")  # Reduce key height
    ),
    linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
    coord_fixed() +  
         labs(subtitle = 'Measured vs requested distance',
          x = 'Requested distance (cm)',
          y = 'Measured distance (cm)') +
    theme(  # Smaller legend text (was default ~10-12), left-aligned # Smaller legend title
      axis.title = element_text(size = 10),        # Smaller axis titles (was default ~12-14)
      axis.text = element_text(size = 9),          # Smaller axis text (was default ~10-12)
      plot.title = element_text(size = 12),        # Smaller plot title (was default ~14-16)
      legend.position = "bottom",                   # Move legend to bottom to prevent right cutoff
      legend.box = "horizontal"                     # Ensure horizontal layout
    )
  
  return(p)
}

plot_sizeCheck <- function(data_list, calibrateTrackDistanceCheckLengthSDLogAllowed) {
  print('inside plot_sizeCheck')
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('calibrateTrackDistance = ', sizeCheck$calibrateTrackDistance[1])
  # Check if the data is empty
  if (nrow(sizeCheck) == 0) {
    print("Error: Empty dataset returned from get_sizeCheck_data()")
    return(NULL)
  }
  
  
  # Check for NA values after conversion
  if (sum(is.na(sizeCheck$SizeCheckEstimatedPxPerCm)) == nrow(sizeCheck) ||
      sum(is.na(sizeCheck$SizeCheckRequestedCm)) == nrow(sizeCheck)) {
    print("Error: All values in one or both columns are NA after conversion.")
    return(NULL)
  }
  
  # Ensure we have at least one valid row
  sizeCheck <- na.omit(sizeCheck)  # Remove rows with NA values
  if (nrow(sizeCheck) == 0) {return(NULL)}
  
  # Average Estimated PxPerCm per Participant per Requested Size
  sizeCheck_avg <- sizeCheck %>%
    group_by(participant, SizeCheckRequestedCm) %>%
    summarize(
      avg_estimated = mean(SizeCheckEstimatedPxPerCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add random horizontal jitter to x-axis variable
      SizeCheckRequestedCm_jitter = SizeCheckRequestedCm * runif(n(), min = 0.95, max = 1.05)
    )
  
  # Perform regression on averaged data
  fit <- lm(log10(avg_estimated) ~ log10(SizeCheckRequestedCm), data = sizeCheck_avg)

  # Calculate slope and correlation
  slope <- coef(fit)
  slope <- format(round(slope[['log10(SizeCheckRequestedCm)']], 2), nsmall=2)
  corr <- cor(log10(sizeCheck_avg$SizeCheckRequestedCm), log10(sizeCheck_avg$avg_estimated))
  corr <- format(round(corr,2), nsmall=2)
  
  # Determine scale limits
  min_val <- min(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated))
  max_val <- max(c(sizeCheck_avg$SizeCheckRequestedCm, sizeCheck_avg$avg_estimated))
  
  # Compute sdLogDensity for each participant
  sdLogDensity_data <- sizeCheck %>%
    group_by(participant) %>%
    summarize(
      sdLogDensity = sd(log10(SizeCheckEstimatedPxPerCm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(!is.na(sdLogDensity))

  
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
    
    positive_breaks <- seq(0, ceiling(data_range[2] / bin_width) * bin_width + bin_width, by = bin_width)
    negative_breaks <- seq(-bin_width, floor(data_range[1] / bin_width) * bin_width - bin_width, by = -bin_width)
    custom_breaks <- sort(c(negative_breaks, positive_breaks))
    
    # Create the histogram plot with stacked dots
    histogram_plot <- ggplot(sdLogDensity_data, aes(x = sdLogDensity)) +
      # Add transparent light-green bar for allowed range
      annotate("rect", 
               xmin = -Inf, 
               xmax = calibrateTrackDistanceCheckLengthSDLogAllowed, 
               ymin = 0, 
               ymax = Inf, 
               fill = "lightgreen", 
               alpha = 0.3) +
      # geom_histogram(breaks = custom_breaks, alpha = 0, color = "black") +
      # Stacked colored points on top of bars
      geom_point(aes(x = bin_center, y = dot_y, color = participant), size = 3, alpha = 0.8) +
      scale_color_manual(values= colorPalette) +
      scale_y_continuous(expand=expansion(mult=c(0,0.5))) + 
      scale_x_continuous(limits = c(0, x_max)) +
      guides(color = guide_legend(
        ncol = 2,  
        title = "",
        override.aes = list(size = 1.5),  
        keywidth = unit(0.8, "lines"),  
        keyheight = unit(0.3, "lines")
      )) +
      labs(
        subtitle = "Histogram of SD of\nlog10 pixel density",
        x = "sdLogDensity",
        y = "Count"
      ) +
      theme_bw() 
  } else {
    histogram_plot <- ggplot() + 
      annotate("text", x = 0.5, y = 0.5, label = "No data available for sdLogDensity calculation") +
      theme_void()
  }
  
  sizeCheck_avg <- sizeCheck_avg %>% 
    left_join(sdLogDensity_data, by = "participant") %>% 
    mutate(reliableBool = (sdLogDensity <= calibrateTrackDistanceCheckLengthSDLogAllowed))
    scatter_plot <- ggplot(data=sizeCheck_avg) + 
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
                        label = paste0('N=', n_distinct(sizeCheck_avg$participant), '\n',
                                       'R=', corr, '\n',
                                       'slope=', slope)) + 
      ggpp::geom_text_npc(data = NULL, aes(npcx = "left", npcy = "bottom"), label = statement) + 
    scale_x_log10() +
    scale_y_log10() +
    scale_color_manual(values= colorPalette) + 
    guides(color = guide_legend(
      ncol = 3,  
      title = "",
      override.aes = list(size = 2),  
      keywidth = unit(1.2, "lines"),  
      keyheight = unit(0.8, "lines")  
         ),
      linetype = guide_legend(title = "", override.aes = list(color = "transparent", size = 0))) +
     labs(subtitle = 'Estimated pixel density vs Requested length',
          x = 'Requested length (cm)',
          y = 'Measured pixel density (px/cm)')
  
  # Return both plots as a list
  return(list(
    scatter = scatter_plot,
    histogram = histogram_plot
  ))
}
