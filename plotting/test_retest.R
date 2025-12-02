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

get_test_retest <- function(df_list){
  pCrowding <- df_list$crowding %>% 
    filter(targetEccentricityXDeg != 0 ) %>% 
    group_by(experiment, participant, font, conditionName) %>%
    summarize(test = 10^mean(log_crowding_distance_deg, na.rm = T),
              .groups = "drop")
    
  pAcuity <- df_list$acuity %>%
    filter(targetEccentricityXDeg != 0 ) %>% 
    group_by(experiment, participant, font, conditionName) %>%
    summarize(test = 10^mean(questMeanAtEndOfTrialsLoop, na.rm = T),
              .groups = "drop")
  
  rsvp <- df_list$rsvp %>%
    group_by(experiment, participant, font, conditionName) %>%
    summarize(test = 10^mean(block_avg_log_WPM, na.rm = T),
              .groups = "drop")
  
  reading <- df_list$reading %>% 
    group_by(experiment, participant, font, conditionName) %>%
    summarize(test = mean(wordPerMin, na.rm = T),
              .groups = "drop")
  
  
  beauty = df_list$beauty %>%
    mutate(test = questionAndAnswerResponse) %>%
    filter(!is.na(test))
  
  comfort = df_list$comfort %>% 
    mutate(test = questionAndAnswerResponse) %>%
    filter(!is.na(test))
  
  create_plot <- function(data, use_jitter = FALSE, use_log_jitter = FALSE) {
    data <- data %>% 
      mutate(group = ifelse(grepl("Repeat",experiment), "retest","test"))
    
    test <- data %>%
      filter(group == 'test')
    
    retest <- data %>%
      filter(group == 'retest')
    
    
    test_retest <- test %>% 
      select(participant, font, conditionName, test) %>% 
      inner_join(retest, by = c("participant", "font", "conditionName")) %>% 
      rename(test=test.x,
             retest = test.y) %>% 
      filter(!is.na(test), !is.na(retest))
    
    # plot
    if (nrow(test_retest) == 0) return (NULL)
    n = nrow(test_retest)
    
    # Apply jitter based on scale type
    if (use_jitter) {
      if (use_log_jitter) {
        # Logarithmic jitter for log scales (unbiased)
        test_retest <- test_retest %>%
          mutate(
            test_jitter = add_log_jitter(test, jitter_percent = 2, seed = 42),
            retest_jitter = add_log_jitter(retest, jitter_percent = 2, seed = 43)
          )
      } else {
        # Linear jitter for linear scales
        test_retest <- test_retest %>%
          mutate(
            test_jitter = test + runif(n(), -0.25, 0.25),
            retest_jitter = retest + runif(n(), -0.25, 0.25)
          )
      }
      
      p <- ggplot(test_retest, aes(x = test_jitter, y = retest_jitter, color = font)) +
        geom_point(size = 3)
    } else {
      p <- ggplot(test_retest, aes(x = test, y = retest, color = font)) +
        geom_point(size = 3)
    }
    
    p <- p +
      coord_fixed(ratio = 1) +
      theme_bw() +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top',
                              label = paste0('N=', n,'\n')))
    
    return(p)
  }
  
  reading_p <- create_plot(reading, use_jitter = TRUE, use_log_jitter = TRUE)
  if (!is.null(reading_p)) {
    # Calculate dynamic limits based on actual data
    reading_test_retest <- reading %>% 
      mutate(group = ifelse(grepl("Repeat",experiment), "retest","test")) %>%
      filter(group %in% c('test', 'retest')) %>%
      group_by(participant, font, conditionName) %>%
      filter(n() == 2) %>%  # Only keep pairs with both test and retest
      ungroup()
    
    if (nrow(reading_test_retest) > 0) {
      # Calculate limits with some padding for better visualization
      min_reading <- min(reading_test_retest$test, na.rm = TRUE)
      max_reading <- max(reading_test_retest$test, na.rm = TRUE)
      # Use log-scale appropriate padding
      reading_limits <- c(min_reading * 0.7, max_reading * 1.4)
    } else {
      # Fallback to reasonable defaults if no data
      reading_limits <- c(50, 1500)
    }
    
    reading_p <- reading_p + 
      scale_x_log10(limits=reading_limits) +
      scale_y_log10(limits=reading_limits) +
      labs(x="Test reading (w/min)",
           y="Retest reading (w/min)",
           subtitle="Reading retest vs test") +
      annotation_logticks(
        sides = "bl",
        short = unit(2, "pt"),
        mid   = unit(2, "pt"),
        long  = unit(7, "pt")
      )
  }
  
  crowding_p <- create_plot(pCrowding, use_jitter = TRUE, use_log_jitter = TRUE)
  if (!is.null(crowding_p)) {
    # Calculate dynamic limits based on actual data
    crowding_test_retest <- pCrowding %>% 
      mutate(group = ifelse(grepl("Repeat",experiment), "retest","test")) %>%
      filter(group %in% c('test', 'retest')) %>%
      group_by(participant, font, conditionName) %>%
      filter(n() == 2) %>%  # Only keep pairs with both test and retest
      ungroup()
    
    if (nrow(crowding_test_retest) > 0) {
      # Calculate limits with some padding for better visualization
      min_crowding <- min(crowding_test_retest$test, na.rm = TRUE)
      max_crowding <- max(crowding_test_retest$test, na.rm = TRUE)
      # Use log-scale appropriate padding
      crowding_limits <- c(min_crowding * 0.7, max_crowding * 1.4)
    } else {
      # Fallback to reasonable defaults if no data
      crowding_limits <- c(0.03, 10)
    }
    
    crowding_p <- crowding_p + 
      scale_x_log10(limits=crowding_limits) +
      scale_y_log10(limits=crowding_limits) +
      labs(x="Test crowding (deg)",
           y="Retest crowding (deg)",
           subtitle="Peripheral crowding retest vs test\nGeometric mean of left and right") +
      annotation_logticks(
        sides = "bl",
        short = unit(2, "pt"),
        mid   = unit(2, "pt"),
        long  = unit(7, "pt")
      )
  }
  
  acuity_p <- create_plot(pAcuity, use_jitter = TRUE, use_log_jitter = TRUE)
  if (!is.null(acuity_p)) {
    # Calculate dynamic limits based on actual data
    acuity_test_retest <- pAcuity %>% 
      mutate(group = ifelse(grepl("Repeat",experiment), "retest","test")) %>%
      filter(group %in% c('test', 'retest')) %>%
      group_by(participant, font, conditionName) %>%
      filter(n() == 2) %>%  # Only keep pairs with both test and retest
      ungroup()
    
    if (nrow(acuity_test_retest) > 0) {
      # Calculate limits with some padding for better visualization
      min_acuity <- min(acuity_test_retest$test, na.rm = TRUE)
      max_acuity <- max(acuity_test_retest$test, na.rm = TRUE)
      # Use log-scale appropriate padding
      acuity_limits <- c(min_acuity * 0.7, max_acuity * 1.4)
    } else {
      # Fallback to reasonable defaults if no data
      acuity_limits <- c(0.03, 10)
    }
    
    acuity_p <- acuity_p + 
    scale_x_log10(limits=acuity_limits) +
    scale_y_log10(limits=acuity_limits) +
    labs(x="Test acuity (deg)",
         y="Retest acuity (deg)",
         subtitle="Peripheral cuity retest vs test\nGeometric mean of left and right") +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid   = unit(2, "pt"),
      long  = unit(7, "pt")
    )
  }
  
  beauty_p <- create_plot(beauty, use_jitter = TRUE)
  if (!is.null(beauty_p)) {
    beauty_p <- beauty_p + 
      labs(x="Test beauty ratings",
           y="Retest beauty ratings",
           subtitle="Beauty retest vs test")
  }

   comfort_p <- create_plot(comfort, use_jitter = TRUE) 
   if (!is.null(comfort_p)) {
     comfort_p <- comfort_p + 
       labs(x="Test comfort ratings",
            y="Retest comfort ratings",
            subtitle="Comfort retest vs test")
   }
   
  return(list(reading = reading_p,
              pCrowding = crowding_p,
              pAcuity = acuity_p,
              beauty = beauty_p,
              comfort = comfort_p))
}


