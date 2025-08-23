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
                        label = paste0('N=', n_distinct(distance_avg$participant))) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    scale_x_log10(limits = c(min_val, max_val), breaks = seq(5, 100, by = 5)) + 
    scale_y_log10(limits = c(min_val, max_val), breaks = seq(5, 100, by = 5)) + 
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
  
  return(p)
}

plot_distance_production <- function(data_list,calibrateTrackDistanceCheckLengthSDLogAllowed) {
  print('inside plot_distance_production')
  distance <- get_measured_distance_data(data_list)
  sizeCheck <- get_sizeCheck_data(data_list)
  statement <- paste0('calibrateTrackDistance = ', distance$calibrateTrackDistance[1])
  if (nrow(distance) == 0) {return(NULL)}
  
  # Calculate density ratio from sizeCheck data
  densityRatio_data <- tibble()
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
      creditCardMeasuredCm = mean(calibrateTrackDistanceMeasuredCm, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      # Add random horizontal jitter to x-axis variable
      calibrateTrackDistanceRequestedCm_jitter = calibrateTrackDistanceRequestedCm * runif(n(), min = 0.95, max = 1.05)
    )
  
  # Join with density ratio data and calculate production-measured distance
  if (nrow(densityRatio_data) > 0) {
    distance_avg <- distance_avg %>% 
      left_join(densityRatio_data %>% select(participant, densityRatio, reliableBool), by = "participant") %>%
      mutate(
        # Apply density ratio correction: productionMeasuredCm = densityRatio × creditCardMeasuredCm
        productionMeasuredCm = ifelse(!is.na(densityRatio), densityRatio * creditCardMeasuredCm, creditCardMeasuredCm),
        reliableBool = ifelse(is.na(reliableBool), TRUE, reliableBool)
      )
  } else {
    distance_avg <- distance_avg %>% 
      mutate(
        productionMeasuredCm = creditCardMeasuredCm,
        reliableBool = TRUE
      )
  }

  fit <- lm(log10(productionMeasuredCm) ~ log10(calibrateTrackDistanceRequestedCm), data = distance_avg)

  slope <- coef(fit)
  slope <- format(round(slope[['log10(calibrateTrackDistanceRequestedCm)']], 2), nsmall=2)
  corr <- cor(log10(distance_avg$calibrateTrackDistanceRequestedCm), log10(distance_avg$productionMeasuredCm))
  corr <- format(round(corr,2), nsmall=2)
  
  min_val <- min(c(distance_avg$calibrateTrackDistanceRequestedCm, distance_avg$productionMeasuredCm))
  max_val <- max(c(distance_avg$calibrateTrackDistanceRequestedCm, distance_avg$productionMeasuredCm))

    p <- ggplot() + 
    geom_line(data=distance_avg, 
              aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                  y = productionMeasuredCm,
                  color = participant, 
                  lty = reliableBool,
                  group = participant), alpha = 0.7) +
    geom_point(data=distance_avg, 
               aes(x = calibrateTrackDistanceRequestedCm_jitter, 
                   y = productionMeasuredCm,
                   color = participant), 
               size = 2) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(distance_avg$participant))) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # y=x line
    scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dotted"),
                          labels = c("TRUE" = "", "FALSE" = "Dotting of line indicates unreliable length production.")) +
    scale_x_log10(limits = c(min_val, max_val), breaks = seq(5, 100, by = 5)) + 
    scale_y_log10(limits = c(min_val, max_val), breaks = seq(5, 100, by = 5)) + 
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
         labs(subtitle = 'Production-measured vs. requested distance',
          x = 'Requested distance (cm)',
          y = 'Production-measured distance (cm)') +
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
  
  ruler <-  sizeCheck %>%
    distinct(participant, rulerLength, rulerUnit) %>%
    filter(!is.na(rulerLength)) %>% 
    mutate(lengthCm = ifelse(rulerUnit == 'cm', rulerLength, rulerLength * 2.54))
  
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
    
    positive_breaks <- seq(0, ceiling(data_range[2] / bin_width) * bin_width + bin_width, by = bin_width)
    negative_breaks <- seq(-bin_width, floor(data_range[1] / bin_width) * bin_width - bin_width, by = -bin_width)
    custom_breaks <- sort(c(negative_breaks, positive_breaks))
    
    # Create the histogram plot with stacked dots
    # Calculate legend rows and dynamic sizing
    n_participants <- n_distinct(sdLogDensity_data$participant)
    legend_rows <- ceiling(n_participants / 2)
    
    # Dynamic sizing based on number of rows
    legend_text_size <-8 * (5/6)^(legend_rows - 1)
    
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
      scale_color_manual(values= colorPalette) +
      scale_x_log10(limits = c(x_min, x_max)) +
      annotation_logticks(sides = "b") +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(sdLogDensity_data$participant))) + 
      guides(color = guide_legend(
        nrow = 4,  
        title = "",
        override.aes = list(size = 0),  
        keywidth = unit(0, "pt"),
        keyheight = unit(0, "pt")
      )) +
      labs(
        subtitle = "Histogram of SD of\nlog10 pixel density",
        x = "SD of log10 pixel density",
        y = "Count"
      ) +
      theme_bw() + 
      theme( legend.key.size = unit(0, "pt"),
             legend.title = element_text(size=6),
             legend.text = element_text(size=legend_text_size * 0.8, margin = margin(l=0.1, r=0, t = -1, b = -1)),
             legend.box.margin = margin(l=-6,r=0,t=2,b=1,"mm"),
             legend.box.spacing = unit(0, "pt"),
             legend.spacing.y = unit(0, "pt"),
             legend.spacing.x = unit(0.1, "pt"),
             legend.key.height = unit(0, "pt"),
             legend.key.width = unit(0.1, "pt"),
             legend.key = element_rect(fill = "transparent", colour = "transparent", size = 0),
             legend.margin = margin(l=0, r=0, t=0, b=0, unit = 'mm'),
             legend.position = "top", 
             legend.box = "vertical", 
             legend.justification='left',
             panel.grid.major = element_blank(), 
             panel.grid.minor = element_blank(),
             panel.background = element_blank(), 
             axis.title = element_text(size = 9),        # Reduced from 12
             axis.text = element_text(size = 8),         # Reduced from 12  
             axis.line = element_line(colour = "black"),
             axis.text.y = element_text(size = 8),       # Reduced from 10
             plot.title = element_text(size=7,
                                       hjust = 0,
                                       margin = margin(b = 0)),
             plot.title.position = "plot",
             plot.subtitle = element_text(size=10,       # Reduced from 12
                                          hjust = 0,
                                          margin = margin(t = 0)),
             plot.caption = element_text(size=8),        # Reduced from 10
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
      geom_histogram(color="black", fill="gray80") + 
      scale_x_log10(breaks = c(1, 2, 3, 5, 10, 15, 20, 30, 50, 100, 150, 200, 300, 500)) +
      annotation_logticks(sides = "b") +
      ggpp::geom_text_npc(aes(npcx="left", npcy="top"), 
                          label = paste0('N=', n_distinct(ruler$participant))) + 
      labs(subtitle = 'Histogram of ruler \nlength (cm)',
           x = "Ruler length (cm)",
           y = "Count")
  } else {
    h2 = NULL
  }
  
  sizeCheck_avg <- sizeCheck_avg %>% 
    left_join(sdLogDensity_data %>% select(-avg_estimated), by = "participant") %>% 
    mutate(reliableBool = (sdLogDensity <= calibrateTrackDistanceCheckLengthSDLogAllowed))
  
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
    scale_y_log10(breaks = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500)) +
    annotation_logticks() + 
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
  geom_rect(
    aes(xmin = x_left, xmax = xmax_band, ymin = y_min_panel, ymax = y_max_panel),
    inherit.aes = FALSE,
    fill = "#e8f5e9",    # very light green
    alpha = 0.18
  ) +
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
