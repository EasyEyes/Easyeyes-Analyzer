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
           title ='Histogram of foveal\ncrowding') + 
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
         title ='Histogram of peripheral\ncrowding',
         caption = 'Geometric average of the left and right thresholds')
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
      reading <- reading %>%
        group_by(participant, targetKind, `Skilled reader?`) %>% 
        summarize(log_WPM = mean(block_avg_log_WPM, na.rm=T)) %>% 
        ungroup()
    } else {
      reading <- reading %>%
        group_by(participant, targetKind, `Skilled reader?`) %>% 
        summarize(log_WPM = mean(log_WPM, na.rm=T))
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
  acuity <- data$acuity
  repeated <- data$repeated
  rsvp <- data$rsvp
  
  # Helper function to create stacked histograms
  create_stacked_histogram <- function(subset_data, variable, grade_order, x_label, title_prefix) {
    if (nrow(subset_data) == 0) {return(NULL)}
    plots <- list()
    
    # Calculate global x-axis range for all grades
    global_min <- floor(min(subset_data[[variable]], na.rm = TRUE))
    global_max <- ceiling(max(subset_data[[variable]], na.rm = TRUE))
    x_range <- seq(global_min, global_max, length.out = 50) 
    
    for (i in seq_along(grade_order)) {
      grade <- grade_order[i]
      grade_subset <- subset_data %>% filter(Grade == grade)
      if (nrow(grade_subset) > 0) {
        stats <- grade_subset %>%
          summarize(
            mean = round(mean(!!sym(variable), na.rm = TRUE), 2),
            sd = round(sd(!!sym(variable), na.rm = TRUE), 2),
            N = n()
          )
        
        plot <- ggplot(grade_subset, aes_string(x = variable)) +
          geom_histogram(breaks = x_range, color = "black", fill = "black") +
          labs(
            x = NULL,  # No x-axis label for individual plots
            y = "Count",
            title = paste("Grade", grade)
          ) +
          scale_x_continuous(
            limits = c(global_min, global_max), 
            expand = c(0, 0)
          ) +
          scale_y_continuous(expand = c(0, 0)) +
          annotate(
            "text",
            x = global_max, y = Inf,
            label = paste0("mean=", stats$mean, "\nsd=", stats$sd,  "\nN=", stats$N),
            hjust = 1.05, vjust = 1,
            size = 4
          ) +
          plt_theme +
          # Remove x-axis tick marks and labels for all but the bottom plot
          theme(
            axis.text.x = if (i < length(grade_order)) element_blank() else element_text(),
            axis.ticks.x = if (i < length(grade_order)) element_blank() else element_line(),
            plot.title = element_text(size = 14, margin = margin(b = 1)), # Smaller title with reduced bottom margin
            plot.margin = margin(t = 2, r = 5, b = 2, l = 5) # Reduced overall plot margin
          ) + theme(
            legend.position = "top",
            legend.key.size = unit(2, "mm"),
            legend.title = element_text(size = 8),
            legend.text = element_text(size = 8),
            axis.text = element_text(size = 11),
            plot.title = element_text(size = 12, margin = margin(b = 2)),
            plot.margin = margin(5, 5, 5, 5, "pt")
          )
        
        plots[[as.character(grade)]] <- plot
      }
    }
    
    # Add x-axis label to the last plot
    if (length(plots) > 0) {
      plots[[length(plots)]] <- plots[[length(plots)]] + 
        xlab(x_label) +  # Add the x-axis label
        theme(axis.title.x = element_text(vjust = -0.5))  # Adjust the position of the x-axis label
    }
    
    # Combine all plots vertically
    combined_plot <- wrap_plots(plots, ncol = 1, heights = rep(0.5, length(plots))) +
      plot_annotation(
        title = paste("Histogram of", title_prefix, "\nstacked by grade"),
        theme = plt_theme
      )
    
    return(combined_plot)
  }
  
  # Grade order
  unique_grades <- unique(c(crowding$Grade, acuity$Grade, repeated$Grade, rsvp$Grade))
  grade_order <- c(sort(setdiff(unique_grades, "R"), decreasing = TRUE), "R")
  
  # Generate stacked histograms for each variable
  rsvp_reading_plot <- create_stacked_histogram(
    rsvp, "block_avg_log_WPM", grade_order, 
    "Log RSVP reading speed (w/min)", "RSVP reading"
  )
  
  peripheral_crowding <- crowding %>% filter(targetEccentricityXDeg != 0)
  peripheral_crowding_plot <- create_stacked_histogram(
    peripheral_crowding, "log_crowding_distance_deg", grade_order, 
    "Log peripheral crowding (deg)", "peripheral crowding"
  )
  
  foveal_acuity <- acuity %>% filter(targetEccentricityXDeg == 0)
  foveal_acuity_plot <- create_stacked_histogram(
    foveal_acuity, "questMeanAtEndOfTrialsLoop", grade_order, 
    "Log acuity (deg)", "foveal acuity"
  )
  
  foveal_crowding <- crowding %>% filter(targetEccentricityXDeg == 0)
  foveal_crowding_plot <- create_stacked_histogram(
    foveal_crowding, "log_crowding_distance_deg", grade_order, 
    "Log foveal crowding (deg)", "foveal crowding"
  )
  
  foveal_repeated <- repeated %>% filter(targetEccentricityXDeg == 0)
  foveal_repeated_plot <- create_stacked_histogram(
    foveal_repeated, "log_crowding_distance_deg", grade_order, 
    "Log repeated-letter crowding (deg)", "foveal repeated-letter crowding"
  )
  
  peripheral_acuity <- acuity %>% filter(targetEccentricityXDeg != 0)
  peripheral_acuity_plot <- create_stacked_histogram(
    peripheral_acuity, "questMeanAtEndOfTrialsLoop", grade_order, 
    "Log acuity (deg)", "peripheral acuity"
  )
  
  print('done all stacked plots')
  
  # Return all stacked plots
  return(list(
    rsvp_reading_plot = rsvp_reading_plot,
    peripheral_crowding_plot = peripheral_crowding_plot,
    foveal_acuity_plot = foveal_acuity_plot,
    foveal_crowding_plot = foveal_crowding_plot,
    foveal_repeated_plot = foveal_repeated_plot,
    peripheral_acuity_plot = peripheral_acuity_plot
  ))
}

get_age_histogram <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(NULL)
  
  # Remove negative ages
  data <- data %>% filter(age >= 0)
  print(data %>% arrange(age))
  
  # Calculate summary statistics
  stats <- data %>%
    summarize(
      mean = round(mean(age, na.rm = TRUE), 2),
      sd = round(sd(age, na.rm = TRUE), 2),
      N = n()
    )
  
  # Generate histogram
  p <- ggplot(data) + 
    geom_histogram(aes(x = age), color = "black", fill = "black") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggpp::geom_text_npc(
      aes(npcx = 'right',
          npcy = 'top',
          label = paste0('mean=', stats$mean, '\n sd=', stats$sd, '\n N=', stats$N))
    ) +
    labs(x = 'Age',
         y = 'Count',
         title = 'Histogram of Age')
  
  return(p)
}


get_grade_histogram <- function(rsvp) {
  if (is.null(rsvp) || nrow(rsvp) == 0) return(NULL)
  
  # Extract Grade column from RSVP data
  grade_data <- rsvp %>% select(Grade) %>% drop_na() %>% mutate(Grade = as.numeric(Grade))
  
  # Ensure Grade is numeric and remove invalid values (if any)
  grade_data <- grade_data %>% filter(Grade >= 0)  # Ensures no negative grades
  
  # Calculate summary statistics
  stats <- grade_data %>%
    summarize(
      mean = round(mean(Grade, na.rm = TRUE), 2),
      sd = round(sd(Grade, na.rm = TRUE), 2),
      N = n()
    )
  
  # Define bin width dynamically

  
  # Generate histogram
  p <- ggplot(grade_data, aes(x = Grade)) +
    geom_histogram(color = "black", fill = "black") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggpp::geom_text_npc(
      aes(npcx = 'right',
          npcy = 'top',
          label = paste0('mean=', stats$mean, '\n sd=', stats$sd, '\n N=', stats$N))
    ) +
    labs(x = 'Grade',
         y = 'Count',
         title = 'Histogram of grades') +
    theme_minimal()  # Consistent minimal theme
  
  return(p)
}


















