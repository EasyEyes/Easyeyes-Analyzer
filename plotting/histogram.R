library(patchwork)

get_fluency_histogram <- function(fluency){
  
  if(nrow(fluency) == 0) {
    return(NULL)
  }
  
  fluency$accuracy <- factor(fluency$accuracy, levels = c(0,0.2,0.4,0.6,0.8,1))

  ggplot(data = fluency %>% count(accuracy, .drop = F), aes(x = accuracy, y = n)) +
    geom_bar(stat = "identity") + 
    labs(x = "fluency (proportion correct)",
         y = "Count",
         subtitle = "English fluency histogram")

}

get_reading_retention_histogram <- function(reading) {
  if(nrow(reading) == 0) {
    return(NULL)
  }
  counts <- reading %>% count(accuracy, .drop=F)
  ggplot(data = counts) + geom_bar(aes(x = accuracy, y = n), stat = "identity") + 
    labs(x = "Reading retention (proportion correct)",
         y = "Count",
         subtitle = "Reading retention histogram")
}

get_crowding_hist <- function(crowding) {

  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant, `Skilled reader?`) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm = T),
              .groups="drop")

  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% summarize(mean = round(mean(log_crowding_distance_deg),2), 
                                   sd = round(sd(log_crowding_distance_deg),2),
                                   N = n(),
                                   .groups="drop")
    p1 <- ggplot(foveal) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color=NA, fill="gray80") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log foveal crowding (deg)',
           y = 'Count',
           subtitle ='Histogram of foveal\ncrowding') + 
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
               N = n(),
               .groups="drop")

   p2 <- ggplot(peripheral) + 
    geom_histogram(aes(x = log_crowding_distance_deg),color=NA, fill="gray80") +
   scale_x_continuous(expand = c(0, 0)) + 
   scale_y_continuous(expand = c(0, 0)) + 
   ggpp::geom_text_npc(
     aes( npcx = 'right',
          npcy = 'top',
          label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
   ) + 
    labs(x = 'Log peripheral crowding (deg)',
         y = 'Count',
         subtitle ='Histogram of peripheral\ncrowding',
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
    group_by(participant, `Skilled reader?`) %>% 
    summarize(questMeanAtEndOfTrialsLoop = mean(questMeanAtEndOfTrialsLoop),
              .groups="drop")
  
  
  if (nrow(foveal) > 0) {
    if ('Skilled reader?' %in% names(foveal)) {
      stats1 <- foveal %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- foveal
    }
    stats1 <- stats1 %>% 
      summarize(mean = round(mean(questMeanAtEndOfTrialsLoop),2),
                sd = round(sd(questMeanAtEndOfTrialsLoop),2),
                N = n(),
                .groups="drop")
    
    p1 <-  ggplot(foveal) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color=NA, fill="gray80") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           subtitle ='Histogram of foveal\nacuity')
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
                N = n(),
                .groups="drop")
    
    p2 <-  ggplot(peripheral) + 
      geom_histogram(aes(x = questMeanAtEndOfTrialsLoop),color=NA, fill="gray80") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats2$mean,'\n sd=', stats2$sd, '\n N=', stats2$N))
      ) +
      labs(x = 'Log acuity (deg)',
           y = 'Count',
           subtitle ="Histogram of peripheral\nacuity\nGeometric average of left \nand right thresholds")
  } else {
    p2 <- NULL
  }
  return(list(p1,p2))
}

get_reading_hist <- function(data) {
  
  if (nrow(data) > 0) {
    if (data$targetKind[1] == 'rsvpReading') {
      data <- data %>%
        filter(is.finite(block_avg_log_WPM)) %>%  # Filter infinite values BEFORE mean calculation
        group_by(participant, targetKind, `Skilled reader?`) %>% 
        summarize(log_WPM = mean(block_avg_log_WPM, na.rm=T),
                  .groups="drop")
    } else {
      data <- data %>%
        filter(is.finite(log_WPM)) %>%  # Filter infinite values BEFORE mean calculation
        group_by(participant, targetKind, `Skilled reader?`) %>% 
        summarize(log_WPM = mean(log_WPM, na.rm=T),
                  .groups="drop")
    }
    data <- data %>% filter(!is.na(log_WPM) & is.finite(log_WPM))
    if (nrow(data) == 0) {
      return(NULL)
    }
    if ('Skilled reader?' %in% names(data)) {
      stats1 <- data %>% filter(`Skilled reader?` != FALSE)
    } else {
      stats1 <- data
    }
    
    stats1 <- stats1 %>% summarize(mean = round(mean(log_WPM),2), 
                                   sd = round(sd(log_WPM),2),
                                   N = n(),
                                   .groups="drop")
    stats1 = stats1[1,]
    p1 <- ggplot(data) + 
      geom_histogram(aes(x = log_WPM),color=NA, fill="gray80") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', nrow(data)))
      ) 
    if (data$targetKind[1] == 'rsvpReading') {
      p1 <- p1 + 
        labs(x = 'Log RSVP reading speed (w/min)',
             y = 'Count',
             subtitle ='Histogram of RSVP\nreading speed')
    } else {
      p1 <- p1 + 
        labs(x = 'Log reading speed (w/min)',
             y = 'Count',
             subtitle ='Histogram of\nreading speed')
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
                                   N = n(),
                                   .groups="drop")
    p1 <- ggplot(repeated) + 
      geom_histogram(aes(x = log_crowding_distance_deg),color=NA, fill="gray80") +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes( npcx = 'right',
             npcy = 'top',
             label = paste0('mean=',stats1$mean,'\n sd=', stats1$sd, '\n N=', stats1$N))
      ) +
      labs(x = 'Log repeated-letter crowding (deg)',
           y = 'Count',
           subtitle ='Histogram of\nrepeated-letter crowding')
  } else {
    p1 <- NULL
  }
  return(p1)
}

generate_histograms_by_grade <- function(data) {

  crowding <- data$crowding %>% filter(!is.na(log_crowding_distance_deg))
  acuity <- data$acuity %>% filter(!is.na(questMeanAtEndOfTrialsLoop))
  repeated <- data$repeated %>% filter(!is.na(log_crowding_distance_deg))
  rsvp <- data$rsvp %>% filter(!is.na(block_avg_log_WPM) & is.finite(block_avg_log_WPM))
  
  # Helper function to create stacked histograms
  create_stacked_histogram <- function(subset_data, variable, grade_order, x_label, title_prefix) {
    if (nrow(subset_data) == 0) {return(NULL)}
    
    # Filter out infinite and NA values
    subset_data <- subset_data %>% filter(is.finite(.data[[variable]]))
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
            N = n(),
            .groups="drop"
          )
        
        plot <- ggplot(grade_subset, aes_string(x = variable)) +
          geom_histogram(breaks = x_range, color = NA, fill = "gray80") +
          labs(
            x = NULL,  # No x-axis label for individual plots
            y = "Count",
            subtitle = paste("Grade", grade)
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
  
  peripheral_crowding <- crowding %>% 
    filter(targetEccentricityXDeg != 0) %>% 
    group_by(participant,Grade) %>% 
    summarize(log_crowding_distance_deg = mean(log_crowding_distance_deg, na.rm=T),
              .groups = "drop")
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
  data <- data %>% filter(age >= 0)
  if (nrow(data) == 0 || n_distinct(data$age) == 1) return(NULL)
  # Calculate summary statistics
  stats <- data %>%
    summarize(
      mean = round(mean(age, na.rm = TRUE), 2),
      sd = round(sd(age, na.rm = TRUE), 2),
      N = n(),
      .groups="drop"
    )
  
  # Generate histogram
  
  p <- ggplot(data) + 
    geom_histogram(aes(x = age), color = NA, fill = "gray80") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggpp::geom_text_npc(
      aes(npcx = 'right',
          npcy = 'top',
          label = paste0('mean=', stats$mean, '\n sd=', stats$sd, '\n N=', stats$N))
    ) +
    labs(x = 'Age',
         y = 'Count',
         subtitle = 'Histogram of age')
  
  return(p)
}

get_grade_histogram <- function(age) {
  if (is.null(age) || n_distinct(age$Grade) <= 1) return(NULL)
  
  grade_data <- age %>%
    distinct(participant, Grade) %>%
    drop_na() %>%
    mutate(Grade = as.numeric(Grade))

  # Calculate summary statistics
  stats <- grade_data %>%
    summarize(
      mean = round(mean(Grade, na.rm = TRUE), 2),
      sd = round(sd(Grade, na.rm = TRUE), 2),
      N = n(),
      .groups="drop"
    )
  
  # Generate histogram
  p <- ggplot(grade_data, aes(x = Grade)) +
    geom_histogram(color = NA, fill = "gray80") +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    ggpp::geom_text_npc(
      aes(npcx = 'right',
          npcy = 'top',
          label = paste0('mean=', stats$mean, '\n sd=', stats$sd, '\n N=', stats$N))
    ) +
    labs(x = 'Grade',
         y = 'Count',
         subtitle = 'Histogram of grades') +
    theme_minimal()  # Consistent minimal theme
  
  return(p)
}

add_questsd_hist <- function(quest, lists) {
  i = length(lists$plotList) + 1
  plotList = lists$plotList
  fileNames = lists$fileNames

  for (qt in c('Foveal crowding', 'Peripheral crowding', 'Foveal acuity', 'Peripheral acuity', 'RSVP reading')) {
    
    t <- quest %>% filter(questType == qt)
    if (nrow(t) == 0) {
      next
    }

    stats <- t %>%
      summarize(mean = round(mean(questSDAtEndOfTrialsLoop, na.rm = TRUE), 2),
                sd = round(sd(questSDAtEndOfTrialsLoop, na.rm = TRUE), 2),
                N = n(),
                .groups = "drop")
    
    p <- ggplot(t, aes(x = questSDAtEndOfTrialsLoop)) +
      geom_histogram(color = NA, fill = "gray80", bins = 30) +
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      ggpp::geom_text_npc(
        aes(npcx = 'right',
            npcy = 'top'),
        label = paste0('mean = ', stats$mean, '\n sd = ', stats$sd, '\n N = ', stats$N),
        hjust = 1, vjust = 1
      ) +
      labs(
        x = 'Quest SD',
        y = 'Count',
        subtitle = paste0('Histogram of quest SD, ', tolower(qt))
      )
    
    plotList[[i]] <- p
    fileNames[[i]] <- paste0('hist-of-questsd-', gsub(" ", "-", tolower(qt)))
    i <- i + 1
  }
  
  return(list(plotList = plotList,
              fileNames = fileNames))
}


get_reading_CQ_hist <- function(reading_pre, minCQAccuracy) {
  # plot histogram of CQAccuracy
  # Add vertical line at minCQAccuracy

  reading_pre <- reading_pre %>%
    filter(!is.na(CQAccuracy) & is.finite(CQAccuracy))

  if (nrow(reading_pre) == 0) {
    return(NULL)
  }

  # use global wrap_words from other/utility.R

  make_plot <- function(df, subtitle_text) {
    # N should be number of distinct (experiment, participant, block)
    n_combo <- tryCatch({
      if (all(c("experiment","participant","block") %in% names(df))) {
        nrow(dplyr::distinct(df, experiment, participant, block))
      } else {
        nrow(df)
      }
    }, error = function(e) nrow(df))

    stats_sub <- df %>%
      summarize(mean = mean(CQAccuracy),
                sd   = sd(CQAccuracy),
                .groups = "drop")
    stats_sub$N <- n_combo

    p <- ggplot(df, aes(x = CQAccuracy)) +
      geom_histogram(color = NA, fill = "gray80") +
      scale_x_continuous(name = "Comprehension accuracy", expand = c(0, 0)) +
      scale_y_continuous(name = "Count",                 expand = c(0, 0)) +
      labs(subtitle = subtitle_text) +
      geom_text(
        data = stats_sub,
        aes(
          x = Inf, y = Inf,
          label = paste0(
            "mean=", round(mean, 2),
            "\n sd=",   round(sd,   2),
            "\n N=",    N
          )
        ),
        inherit.aes = FALSE,
        hjust = 1, vjust = 1
      ) +
      theme_minimal() +
      theme(
        plot.title          = element_text(size = 10, lineheight = 0.9, margin = margin(b = 5)),
        plot.title.position = "plot"
      )

    if (!is.null(minCQAccuracy) && is.finite(minCQAccuracy)) {
      p <- p + geom_vline(xintercept = minCQAccuracy, linetype = "dashed", color = "red", linewidth = 0.8)
    }

    p
  }

  if ("conditionName" %in% names(reading_pre)) {
    reading_list <- split(reading_pre, reading_pre$conditionName)

    hist_list <- lapply(names(reading_list), function(cond) {
      full_title    <- paste("Histogram of comprehension accuracy", cond)
      wrapped_title <- wrap_words(full_title, 25)
      make_plot(reading_list[[cond]], wrapped_title)
    })
    names(hist_list) <- names(reading_list)

    return(hist_list)
  } else {
    subtitle_text <- wrap_words("Histogram of comprehension accuracy", 25)
    p <- make_plot(reading_pre, subtitle_text)

    return(p)
  }
}


get_prop_correct_hist_list <- function(quest, max_chars_per_line = 25) {
  quest <- quest %>% 
    filter(is.finite(frac)) %>% 
    mutate(prop_correct = 1 - frac)
  
  if (nrow(quest) == 0) return(NULL)
  
  # use global wrap_words from other/utility.R
  
  quest_list <- split(quest, quest$conditionName)
  stats_df   <- quest %>% 
    group_by(conditionName) %>% 
    summarize(mean = mean(prop_correct),
              sd   = sd(prop_correct),
              N    = n(),
              .groups = "drop")
  
  hist_list <- lapply(names(quest_list), function(cond) {
    # build & wrap the full title 
    full_title    <- paste("Histogram of proportion correct", cond)
    wrapped_title <- wrap_words(full_title, max_chars_per_line)
    
    data_sub  <- quest_list[[cond]]
    stats_sub <- stats_df %>% filter(conditionName == cond)
    
    ggplot(data_sub, aes(x = prop_correct)) +
      geom_histogram(color = NA, fill = "gray80") +
      scale_x_continuous(name = "Proportion correct", expand = c(0, 0)) +
      scale_y_continuous(name = "Count",            expand = c(0, 0)) +
      labs(subtitle = wrapped_title) +
      geom_text(
        data = stats_sub,
        aes(
          x = Inf, y = Inf,
          label = paste0(
            "mean=", round(mean, 2),
            "\n sd=",   round(sd,   2),
            "\n N=",    N
          )
        ),
        inherit.aes = FALSE,
        hjust = 1, vjust = 1
      ) +
      theme_minimal() +
      theme(
        plot.title          = element_text(size = 10, lineheight = 0.9, margin = margin(b = 5)),
        plot.title.position = "plot"
      )
  })
  
  names(hist_list) <- names(quest_list)
  hist_list
}


append_hist_list <- function(data_list, plot_list, fileNames, experimentNames){
  
  params <- foreach(i=1:length(data_list), .combine='rbind') %do% {
    t <- data_list[[i]] %>% 
      filter(!is.na(staircaseName)) %>%
      distinct(participant,
               cores,
               deviceMemoryGB,
               devicePixelRatio,
               screenWidthPx,
               pxPerCm,
               screenWidthCm,
               conditionName,
               thresholdParameter, 
               targetEccentricityXDeg, 
               targetEccentricityYDeg, 
               spacingOverSizeRatio, 
               viewingDistanceCm, 
               fontNominalSizePt, 
               level,
               font)
  }
  
 
  

  minDeg <- params %>% 
    filter(!grepl("practice",conditionName, ignore.case = T)) %>% 
    mutate(spacingOverSizeRatio = as.numeric(spacingOverSizeRatio),
           viewingDistanceCm = as.numeric(viewingDistanceCm),
           fontNominalSizePt = as.numeric(fontNominalSizePt),
           level = as.numeric(level),
           fontNominalSizeDeg = (180/pi) * atan2(fontNominalSizePt*2.54/72, viewingDistanceCm)) %>% 
    group_by(participant, conditionName, 
             thresholdParameter, font) %>%
    # minDeg = spacingMinDeg when thresholdParameter = spacingDeg, minDeg = sizeMinDeg when tresholdParameter = targetSizeDeg
    summarize(minDeg = case_when(!is.na(level) ~ 10^min(level),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Sloan.woff2' ~ min(fontNominalSizeDeg),
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Sloan.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'targetSizeDeg' & font == 'Pelli.woff2' ~ min(fontNominalSizeDeg) / 5,
                                 !is.na(fontNominalSizeDeg) & thresholdParameter == 'spacingDeg' & font == 'Pelli.woff2'  ~ min(fontNominalSizeDeg) * spacingOverSizeRatio / 5
    ),
    .groups="drop") %>% 
    filter(thresholdParameter == 'targetSizeDeg' | thresholdParameter == 'spacingDeg') %>% 
    distinct() %>% 
    filter(!is.na(minDeg)) %>% 
    ungroup()

  vars <- c("screenWidthPx", "screenWidthCm", "deviceMemoryGB", 
            "devicePixelRatio","cores")
  
  
  j = length(plot_list) + 1
  # Loop through summary dataset and generate histograms
  for (var in vars) {
    if (n_distinct(params[var]) > 1) {
      data <- params %>% select("participant", all_of(var)) %>% distinct()
      avg <- round(mean(as.numeric(data[[var]]), na.rm =T),2)
      sd <- round(sd(as.numeric(data[[var]]), na.rm =T),2)
      n = nrow(data)
      p <- ggplot(data, aes(x = .data[[var]])) +
        geom_histogram(color=NA, fill="gray80") + 
        theme_bw() +
        ggpp::geom_text_npc(
          aes(npcx = 'right',
              npcy = 'top'),
          label = paste0('mean = ', avg, '\n sd = ', sd, '\n N = ', n)
        ) +
        labs(subtitle = paste("Histogram of\n", var))
      
      if (var == "deviceMemoryGB") {
        p <- p + scale_x_continuous(limits = c(1,9),  
                                    breaks = seq(2, 8, by = 2), 
                                    expand = expansion(add = 0)) 
        
      }
      p <-  add_experiment_title(p, experimentNames)
      plot_list[[j]] <- p
      fileNames[[j]] <- paste0(var,'-histogram')
      j = j + 1
    }
  }
  if (nrow(minDeg) > 0) {
    # histogram of spacingMinDeg (log-x, no ticks)
    stats <- minDeg %>%
      filter(thresholdParameter == 'spacingDeg',
             !is.na(minDeg)) %>%
      summarize(mean = round(mean(minDeg),2),
                sd = round(sd(minDeg),2),
                N = n(),
                .groups="drop")
    p <- ggplot(minDeg %>% filter(thresholdParameter == 'spacingDeg')) + 
      geom_histogram(aes(x = minDeg),
                     color = NA, fill = "gray80") +
      ggpp::geom_text_npc(
        aes(npcx = 'right',
            npcy = 'top'),
        label = paste0('mean = ', stats$mean, '\n sd = ', stats$sd, '\n N = ', stats$N)
      ) +
      scale_x_log10(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      labs(
        x     = 'spacingMinDeg',
        y     = 'Count',
        subtitle = 'Histogram of\nspacingMinDeg'
      ) + 
      hist_theme
    
    p <-  add_experiment_title(p, experimentNames)
    
    if (stats$N > 0) {
      plot_list[[j]] <- p
      fileNames[[j]] <- "spacingMinDeg-histogram"
      j = j + 1
    }
  }
 
  
  return(list(plotList = plot_list,
              fileNames = fileNames))
}













