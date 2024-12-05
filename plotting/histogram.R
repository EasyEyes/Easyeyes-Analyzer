library(patchwork)

get_fluency_histogram <- function(fluency){
  
  if(nrow(fluency) == 0) {
    return(NULL)
  }
  
  print('inside fluency')
  fluency$accuracy <- factor(fluency$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))

  ggplot(data = fluency %>% count(accuracy, .drop = F), aes(x = accuracy, y = n)) +
    geom_bar(stat = "identity") + 
    labs(x = "fluency (proportion correct)",
         y = "Count",
         title = "English fluency histogram")

}

get_reading_retention_histogram <- function(reading) {
  if(nrow(reading) == 0) {
    return(NULL)
  }
  counts <- reading %>% count(accuracy, .drop=F)
  ggplot(data = counts) + geom_bar(aes(x = accuracy, y = n), stat = "identity") + 
    labs(x = "Reading retention (proportion correct)",
         y = "Count",
         title = "Reading retention histogram")
}


get_crowding_hist <- function(crowding) {

  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant, `Skilled reader?`, block) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = T)) %>% 
    ungroup()
  print(peripheral)
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% summarize(mean = round(mean(log_crowding_distance_deg),2), 
                                   sd = round(sd(log_crowding_distance_deg),2),
                                   N = n())
    p1 <- ggplot(foveal) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log foveal crowding (deg)',
           y = 'Count',
           title ='Histogram of foveal\ncrowding',
           subtitle = "Geometric average of left \nand right thresholds") + 
      theme(
        plot.title = element_text(size = rel(0.5)),  # Scale down title size
        axis.title = element_text(size = rel(0.5)),  # Scale down axis title size
        axis.text = element_text(size = rel(0.5))    # Scale down axis text size
      )
  } else {
    p1 <- NULL
  }
 if (nrow(peripheral) > 0) { 
   if ('Skilled reader?' %in% names(peripheral)) {
     stats2 <- peripheral %>% filter(`Skilled reader?` != FALSE)
   } else {
     stats2 <- peripheral
   }
   stats2 <- stats2 %>% 
     summarize(mean = round(mean(log_crowding_distance_deg),2),
               sd = round(sd(log_crowding_distance_deg),2),
               N = n())

   p2 <- ggplot(peripheral) + 
    geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ggpp::geom_text_npc(
     aes( npcx = 'right',
          npcy = 'top',
          label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
   ) + 
    labs(x = 'Log peripheral crowding (deg)',
         y = 'Count',
         title ='Histogram of peripheral\ncrowding')
 } else {
   p2 <- NULL
 }
  return(list(foveal=p1,peripheral=p2))
}

get_acuity_hist <- function(acuity) {

  foveal <- acuity %>% 
    filter(targetEccentricityXDeg == 0) %>%
    distinct(participant, `Skilled reader?`, questMeanAtEndOfTrialsLoop)

  peripheral <- acuity %>%
    filter(targetEccentricityXDeg != 0) %>%
    group_by(participant, `Skilled reader?`, block) %>% 
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop)) %>% 
    ungroup()
  
  
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2),
                N = n())
    
    p1 <-  ggplot(foveal) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           title ='Histogram of foveal\nacuity')
  } else {
    p1 <-  NULL
  }
  
  if (nrow(peripheral) > 0) {
    if ('Skilled reader?' %in% names(peripheral)) {
      stats2 <- peripheral %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats2 <- peripheral
    }
    stats2 <- stats2 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2),
                N = n())
    
    p2 <-  ggplot(peripheral) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           title ='Histogram of peripheral\nacuity',
           subtitle = "Geometric average of left \nand right thresholds")
  } else {
    p2 <- NULL
  }
  return(list(p1,p2))
}

get_reading_hist <- function(reading) {
  if (nrow(reading) > 0) {
    
    if (reading$targetKind[1] == 'rsvpReading') {
      reading <- reading %>% mutate(log_WPM = block_avg_log_WPM)
    }
    
    if ('Skilled reader?' %in% names(reading)) {
      stats1 <- reading %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- reading
    }
    
    stats1 <- stats1 %>% summarize(mean = round(mean(log_WPM),2), 
                                   sd = round(sd(log_WPM),2),
                                   N = n())
    p1 <- ggplot(reading) + 
      geom_histogram(aes(x = log_WPM),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) 
    if (reading$targetKind[1] == 'rsvpReading') {
      p1 <- p1 + 
        labs(x = 'Log RSVP reading speed (w/min)',
             y = 'Count',
             title ='Histogram of RSVP\nreading speed')
    } else {
      p1 <- p1 + 
        labs(x = 'Log reading speed (w/min)',
             y = 'Count',
             title ='Histogram of\nreading speed')
    }
  } else {
    p1 <- NULL
  }
  return(p1)
}

get_repeatedLetter_hist <- function(repeated) {
  if (nrow(repeated) > 0) {
    if ('Skilled reader?' %in% names(repeated)) {
      stats1 <- repeated %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- repeated
    }

    stats1 <- stats1 %>% summarize(mean = round(mean(log_crowding_distance_deg),2), 
                                   sd = round(sd(log_crowding_distance_deg),2),
                                   N = n())
    p1 <- ggplot(repeated) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color="black", fill="black") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log repeated-letter crowding (deg)',
           y = 'Count',
           title ='Histogram of\nrepeated-letter crowding')
  } else {
    p1 <- NULL
  }
  return(p1)
}



generate_histograms_by_grade <- function(data) {
  
  crowding <- data$crowding
  rsvp <- data$rsvp
  
  # Ensure the required columns exist in the datasets
  required_columns_crowding <- c("log_crowding_distance_deg", "Grade", "targetEccentricityXDeg")
  required_columns_rsvp <- c("block_avg_log_WPM", "Grade")
  
  if (!all(required_columns_crowding %in% names(crowding))) {
    stop(paste(
      "Crowding data is missing required columns:",
      paste(setdiff(required_columns_crowding, names(crowding)), collapse = ", ")
    ))
  }
  
  if (!all(required_columns_rsvp %in% names(rsvp))) {
    stop(paste(
      "RSVP data is missing required columns:",
      paste(setdiff(required_columns_rsvp, names(rsvp)), collapse = ", ")
    ))
  }
  
  # Process grades: Put "Grade R" at the beginning and others in ascending order
  unique_grades <- unique(c(crowding$Grade, rsvp$Grade))
  grade_order <- c(sort(setdiff(unique_grades, "R"), decreasing = TRUE), "R")
  
  # Combine x-axis values for consistency
  x_values_crowding <- unique(crowding$log_crowding_distance_deg)
  x_values_rsvp <- unique(rsvp$block_avg_log_WPM)
  
  x_range_crowding <- seq(
    floor(min(x_values_crowding, na.rm = TRUE)) - 0.5,
    ceiling(max(x_values_crowding, na.rm = TRUE)) + 0.5,
    length.out = 50
  )
  
  x_range_rsvp <- seq(
    floor(min(x_values_rsvp, na.rm = TRUE)) - 0.5,
    ceiling(max(x_values_rsvp, na.rm = TRUE)) + 0.5,
    length.out = 50
  )
  
  # Process crowding data by Grade
  crowding_plots <- list()
  for (grade in grade_order) {
    crowding_subset <- crowding %>% filter(Grade == grade)
    if (nrow(crowding_subset) > 0) {
      stats <- crowding_subset %>%
        summarize(
          mean = round(mean(log_crowding_distance_deg, na.rm = TRUE), 2),
          sd = round(sd(log_crowding_distance_deg, na.rm = TRUE), 2),
          N = n()
        )
      
      crowding_plot <- ggplot(crowding_subset, aes(x = log_crowding_distance_deg)) +
        geom_histogram(breaks = x_range_crowding, color = "black", fill = "black") +
        labs(
          x = NULL,  # Remove x-axis label for individual plots
          y = "Count",
          title = paste("Grade", grade)
        ) +
        scale_x_continuous(limits = range(x_range_crowding), expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        annotate(
          "text",
          x = max(x_range_crowding), y = Inf,
          label = paste0("N=", stats$N, "\nMean=", stats$mean, "\nSD=", stats$sd),
          hjust = 1.05, vjust = 1,
          size = 4,
          color = "black"
        ) +
        plt_theme
      crowding_plots[[as.character(grade)]] <- crowding_plot
    }
  }
  
  # Update the last crowding plot to include the x-axis label
  crowding_plots[[length(crowding_plots)]] <- crowding_plots[[length(crowding_plots)]] +
    xlab("Log crowding (deg)") +
   plt_theme
  
  # Combine all crowding plots vertically
  combined_crowding_plot <- wrap_plots(crowding_plots, ncol = 1, heights = rep(10, length(crowding_plots))) +
    plot_annotation(
      title = "Histogram of crowding\nstacked by grade",
      theme = plt_theme
    )
  
  # Process RSVP data by Grade
  rsvp_plots <- list()
  for (grade in grade_order) {
    rsvp_subset <- rsvp %>% filter(Grade == grade)
    if (nrow(rsvp_subset) > 0) {
      stats <- rsvp_subset %>%
        summarize(
          mean = round(mean(block_avg_log_WPM, na.rm = TRUE), 2),
          sd = round(sd(block_avg_log_WPM, na.rm = TRUE), 2),
          N = n()
        )
      
      rsvp_plot <- ggplot(rsvp_subset, aes(x = block_avg_log_WPM)) +
        geom_histogram(breaks = x_range_rsvp, color = "black", fill = "black") +
        labs(
          x = NULL,  # Remove x-axis label for individual plots
          y = "Count",
          title = paste("Grade", grade)
        ) +
        scale_x_continuous(limits = range(x_range_rsvp), expand = c(0, 0)) +
        scale_y_continuous(expand = c(0, 0)) +
        annotate(
          "text",
          x = max(x_range_rsvp), y = Inf,
          label = paste0("N=", stats$N, "\nMean=", stats$mean, "\nSD=", stats$sd),
          hjust = 1.05, vjust = 1,
          size = 4,
          color = "black"
        ) +
        plt_theme
      rsvp_plots[[as.character(grade)]] <- rsvp_plot
    }
  }
  
  # Update the last RSVP plot to include the x-axis label
  rsvp_plots[[length(rsvp_plots)]] <- rsvp_plots[[length(rsvp_plots)]] +
    xlab("Log RSVP reading speed (w/min)") +
    plt_theme
  # Combine all RSVP plots vertically
  combined_rsvp_plot <- wrap_plots(rsvp_plots, ncol = 1, heights = rep(10, length(rsvp_plots))) +
    plot_annotation(
      title = "Histogram of RSVP reading speed\nstacked by grade",
      theme = plt_theme
    )
  
  # Return combined plots
  return(list(crowding_plot = combined_crowding_plot, rsvp_plot = combined_rsvp_plot))
}









