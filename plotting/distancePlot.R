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

get_measured_distance_data <- function(data_list) {
  print('inside get_measured_distance_data')
  df <- tibble()
  
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm) %>%
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
  
  print('done get_measured_distance_data')
  return(df)
}





# plot_distance <- function(data_list) {
#   print('inside plot_distance')
#   print(data_list)
#   distance <- get_measured_distance_data(data_list)
#   if (nrow(distance) == 0) {
#     return(NULL)
#   }
#   distance <- distance %>% mutate(calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
#                       calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm))
#   fit <- lm(calibrateTrackDistanceMeasuredCm~calibrateTrackDistanceRequestedCm, data = distance)
#   print(fit)
#   slope <- coef(fit)
#   slope <- format(round(slope[['calibrateTrackDistanceRequestedCm']],2),nsmall=2)
#   corr <- cor(distance$calibrateTrackDistanceRequestedCm, distance$calibrateTrackDistanceMeasuredCm)
#   corr <- format(round(corr,2),nsmall=2)
#   p <- ggplot(data=distance, aes(x = calibrateTrackDistanceRequestedCm, y = calibrateTrackDistanceMeasuredCm)) + 
#     geom_point() + 
#     ggpp::geom_text_npc(aes(npcx="left",
#                             npcy="top"),
#                         label = paste0('N=',nrow(distance),'\n',
#                                        'R=',corr,'\n',
#                                        'slope=',slope)) + 
#     geom_smooth(method='lm',se=F) + 
#     labs(title = 'measured distance (cm) vs requested\ndistance (cm)')
#   return(p)
# }
plot_distance <- function(data_list) {
  print('inside plot_distance')
  distance <- get_measured_distance_data(data_list)

  # Check if the data is empty
  if (nrow(distance) == 0) {
    print("Error: Empty dataset returned from get_measured_distance_data()")
    return(NULL)
  }
  
  # Convert to numeric
  distance <- distance %>% mutate(
    calibrateTrackDistanceMeasuredCm = as.numeric(calibrateTrackDistanceMeasuredCm),
    calibrateTrackDistanceRequestedCm = as.numeric(calibrateTrackDistanceRequestedCm)
  )
  
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
  
  # Perform regression
  fit <- lm(log10(calibrateTrackDistanceMeasuredCm) ~ log10(calibrateTrackDistanceRequestedCm), data = distance)

  # Calculate slope and correlation
  slope <- coef(fit)
  slope <- format(round(slope[['log10(calibrateTrackDistanceRequestedCm)']], 2), nsmall=2)
  corr <- cor(log10(distance$calibrateTrackDistanceRequestedCm), log10(distance$calibrateTrackDistanceMeasuredCm))
  corr <- format(round(corr,2), nsmall=2)
  
  # Determine identical scale limits for both axes
  min_val <- min(c(distance$calibrateTrackDistanceRequestedCm, distance$calibrateTrackDistanceMeasuredCm))
  max_val <- max(c(distance$calibrateTrackDistanceRequestedCm, distance$calibrateTrackDistanceMeasuredCm))
  
  # Logarithmic plot with identical scales
  p <- ggplot(data=distance, 
              aes(x = calibrateTrackDistanceRequestedCm, 
                  y = calibrateTrackDistanceMeasuredCm)) + 
    geom_point(aes(color =participant)) + 
    ggpp::geom_text_npc(aes(npcx="left",
                            npcy="top"),
                        label = paste0('N=', n_distinct(distance$participant), '\n',
                                       'R=', corr, '\n',
                                       'slope=', slope)) + 
    geom_smooth(method='lm', se=F, formula=y~x) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") + # y=x line
    scale_x_log10(limits = c(min_val, max_val)) +  # Log scale for X-axis
    scale_y_log10(limits = c(min_val, max_val)) +  # Log scale for Y-axis (identical to X)
    scale_color_manual(values= colorPalette) + 
    guides(color = guide_legend(
      nrow = 3,
      title = ""
    )) +
    coord_fixed() +  
    labs(title = 'Measured vs requested Distance',
         x = 'Requested Distance (cm)',
         y = 'Measured Distance (cm)')
  
  return(p)
}



