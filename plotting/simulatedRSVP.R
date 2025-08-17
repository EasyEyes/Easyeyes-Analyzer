

library(dplyr)
library(stringr)
library(ggplot2)

# ------------------------------------------------------------------------------
#####                               Load Function                           ####
# ------------------------------------------------------------------------------
is.contained = function(vec1, vec2) {
  x = vector(length = length(vec1))
  for (i in 1:length(vec1)) {
    x[i] = vec1[i] %in% vec2
    if (length(which(vec1[i] %in% vec2)) == 0)
      vec2
    else
      vec2 = vec2[-match(vec1[i], vec2)]
  }
  y = all(x == T)
  return(y)
}

extractRSVPStaircases <- function(df) {
  stairdf <- df %>%
    filter(!is.na(staircaseName) &
             (str_detect(conditionName, "rsvp") |
                 str_detect(conditionName, "RSVP")
             )) %>%
    select(
      participant,
      date,
      ProlificParticipantID,
      conditionName,
      staircaseName,
      questMeanBeforeThisTrialResponse,
      trialGivenToQuest,
      rsvpReadingResponseCorrectBool,
      levelProposedByQUEST,
      rsvpReadingWordDurationSec,
      simulationThreshold,
      font
    )
}



extractStaircases <- function(df, info) {
  if (!'levelProposedByQUEST' %in% names(df)) {
    df$levelProposedByQUEST = NA
  }
  if (!'trialGivenToQuest' %in% names(df)) {
    df$trialGivenToQuest = NA
  }
  if (!'key_resp.corr' %in% names(df)) {
    df$`key_resp.corr` = NA
  }
  
  stairdf <- df %>%
    filter(!is.na(staircaseName)) %>%
    select(
      staircaseName,
      date,
      levelProposedByQUEST,
      trialGivenToQuest,
      rsvpReadingResponseCorrectBool,
      `key_resp.corr`
    ) %>% 
    inner_join(info, by = 'staircaseName') %>% 
    mutate(questType = case_when(
      thresholdParameter != "targetSizeDeg" &
        thresholdParameter != 'size' &
        targetKind == "letter" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Crowding',
      targetKind == "rsvpReading" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'RSVP reading',
      thresholdParameter != "targetSizeDeg" &
        thresholdParameter != 'size' &
        targetKind == "repeatedLetters" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Repeated letters',
      (thresholdParameter == "targetSizeDeg" | thresholdParameter == 'size') &
        targetKind == "letter" &
        !grepl("practice",conditionName, ignore.case = T) ~ 'Acuity',
      grepl("practice",conditionName, ignore.case = T) ~ 'practice',
      thresholdParameter == "targetSoundDBSPL" ~ 'Sound',
      .default = 'unknown'
    )) %>% 
    drop_na(levelProposedByQUEST) %>% 
    mutate(thresholdParameter = ifelse(is.na(thresholdParameter), 'unknown', thresholdParameter)) %>% 
    tidyr::separate_rows(rsvpReadingResponseCorrectBool,sep=',') %>% 
    # use rsvpReadingResponseCorrectBool to determine quest trial right or wrong for RSVP
    mutate(`key_resp.corr` = ifelse(targetKind == "rsvpReading", rsvpReadingResponseCorrectBool == "TRUE", `key_resp.corr` )) %>% 
    select(experiment, 
             participant, 
             date,
             block, 
             block_condition, 
             staircaseName, 
             conditionName, 
             targetKind, 
             font, 
             thresholdParameter,
             levelProposedByQUEST,
             trialGivenToQuest,
           `key_resp.corr` )

  # "questMeanBeforeThisTrialResponse", "", "targetMeasuredLatenessSec",
  # "targetMeasuredDurationSec", "targetDurationSec", "key_resp.corr", "level", "heightPx", "targetDurationSec", "markingOffsetBeforeTargetOnsetSecs")#, "targetSpacingPx")
}

plotStaircases <- function(Staircases, thresholdParameterSelected, conditionNameInput) {
    stairdf <- Staircases %>%
      filter(questType != 'practice')
    print('inside plotStaircases')

    if (!is.null(conditionNameInput) & length(conditionNameInput) > 0 ) {
      stairdf <- stairdf %>% filter(conditionName %in% conditionNameInput)
    } 

    if (is.null(thresholdParameterSelected) | nrow(stairdf) == 0) {
      return(NULL)
    }
    t <- stairdf %>%
      # Order since trials were randomized
      arrange(participant, staircaseName) %>%
      filter(thresholdParameter == thresholdParameterSelected) %>%
      group_by(participant, staircaseName) %>%
      mutate(trial = row_number(),
             nTrials = sum(trialGivenToQuest,na.rm = T),
             questTrials = paste0(sum(trialGivenToQuest,na.rm = T), ' good Trials'))
    
    if (is.null(thresholdParameterSelected) | nrow(stairdf) == 0) {
      return(NULL)
    }
    height = n_distinct(t %>% select(participant,thresholdParameter, conditionName)) * 1.3 + 2
    maxTrials <- max(t$nTrials)
    p <- ggplot(t, aes(x = trial, y = levelProposedByQUEST)) +
      geom_point() +
      geom_line() +
      theme_minimal() +
      # facet_grid(rows = vars(thresholdParameter),
      #            cols = vars(conditionName))
      labs(x = 'trials', y  = thresholdParameterSelected) + 
      facet_wrap(~conditionName + participant + thresholdParameter + questTrials,
                 ncol = 2, 
                 axes = 'all', 
                 scales = 'free_x',
                 axis.labels = 'all_x')
    if (max(t$nTrials) > 5) {
      p <- p + 
      scale_x_continuous(breaks = scales::breaks_width(5))
    } else {
      p <- p + 
      scale_x_continuous(breaks = scales::breaks_width(1))
    }
      
      theme(
        axis.ticks = element_line(),
        axis.ticks.length = unit(0.3, "line"),
        strip.text.x = element_text(hjust = 1),
        plot.background = element_rect(fill = "white", colour = 'white'),
        strip.background =  element_rect(fill = "white", colour = 'white'),
        panel.background = element_rect(fill = "white", colour = 'white'))
      # scale_y_continuous(limits=c(-2,0))
    return(list(plot = p,
                height = height))
}

plotCrowdingStaircasesVsQuestTrials <- function(df_list, stairs) {
  if (is.null(df_list$crowding) || nrow(df_list$crowding) == 0 || is.null(stairs) || nrow(stairs) == 0) {
    return(list(NULL, NULL))
  }
  
  stairdf <- stairs %>%
    drop_na(levelProposedByQUEST) %>%
    group_by(participant, staircaseName) %>%
    summarize(questTrials = sum(trialGivenToQuest, na.rm = TRUE),
              .groups="drop")

  crowding <- df_list$quest_all_thresholds %>%
    filter(grepl('crowding', questType)) %>% 
    select(-questTrials) %>% 
    left_join(stairdf, by = c("participant", "block_condition" = "staircaseName")) %>%
    mutate(
      age = ifelse(is.na(age), 0, age),
      Age = format(age, nsmall = 2),
      ageN = as.numeric(age),
      Grade = as.factor(Grade)
    ) %>%
    filter(!is.na(Grade)) # Drop rows with NA in Grade
  
  # Helper function to create analysis plots
  create_analysis_plot <- function(data, title) {
    data <- data %>%
      mutate(
        questTrials = as.numeric(questTrials),
        Y = 10^(questMeanAtEndOfTrialsLoop)
      ) %>%
      filter(!is.na(questTrials), !is.na(Y))

    if (nrow(data) == 0) {
      return(NULL)
    }
    
    # Compute correlation
    corr <- cor(data$questTrials, data$Y, method = "pearson", use = "complete.obs")
    N <- nrow(data)
    
    # Compute partial correlation factoring out age
    if ("ageN" %in% colnames(data) && any(!is.na(data$ageN))) {
      valid_data <- data %>% select(questTrials, Y, ageN) %>% drop_na()
      if (nrow(valid_data) > 1) {
        pcor <- ppcor::pcor(valid_data)
        R_factor_out_age <- round(pcor$estimate[2, 1], 2)
      } else {
        R_factor_out_age <- NA
      }
    } else {
      R_factor_out_age <- NA
    }
    
    # Plot limits
    xMin <- min(data$questTrials, na.rm = TRUE) / 1.5
    xMax <- max(data$questTrials, na.rm = TRUE) * 1.5
    yMin <- min(data$Y, na.rm = TRUE) / 1.5
    yMax <- max(data$Y, na.rm = TRUE) * 1.5
    
   
    eccs     <- sort(unique(data$targetEccentricityXDeg))
    eccs_int <- as.integer(round(eccs))
    
    # 2) Build the stats label differently for foveal vs peripheral
    if (all(eccs_int == 0)) {
      # Foveal only → no EccX line
      stats_label <- paste0(
        "R_factor_out_age = ", R_factor_out_age,  "\n",
        "R = ",                  round(corr, 2), "\n",
        "N = ",                  N
      )
    } else {
      # Peripheral → include EccX on top
      ecc_label   <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
      stats_label <- paste0(
        "R_factor_out_age = ", R_factor_out_age,  "\n",
        ecc_label,                                "\n",
        "R = ",                  round(corr, 2), "\n",
        "N = ",                  N
      )
    }
    # Generate the plot
    plot <- ggplot(data, aes(x = questTrials, y = Y, color = Grade, shape = conditionName)) +
      geom_point(size = 3) +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
      color_scale(n = length(unique(data$Grade))) +  # Directly use color_scale()
      scale_y_log10(breaks = scales::log_breaks(), limits = c(yMin, yMax)) +
      scale_x_continuous(limits = c(xMin, xMax)) +
      annotation_logticks(
        sides = "l", 
        short = unit(2, "pt"), 
        mid   = unit(2, "pt"), 
        long  = unit(7, "pt")
      ) +
      labs(
        x = "Good trials",
        y = "Crowding distance (deg)",
        title = title
      ) +
      theme_classic() + 
      plt_theme +
      annotate(
        "text",
        x = xMax * 0.9,
        y = yMax * 0.9,
        label = stats_label,
        hjust = 1, vjust = 1, size = 4, color = "black"
      )
    guides(color = guide_legend(title='Grade'),
           shape = guide_legend(title='', 
                                ncol = 1))
    
    return(plot)
  }
  
  # Separate foveal and peripheral crowding
  foveal <- crowding %>% filter(targetEccentricityXDeg == 0)
  peripheral <- crowding %>% filter(targetEccentricityXDeg != 0)
  
  # Create plots
  fovealPlot <- create_analysis_plot(foveal, "Foveal crowding vs good trials\ncolored by grade")
  peripheralPlot <- create_analysis_plot(peripheral, "Peripheral crowding vs good trials\ncolored by grade")
  
  return(list(fovealPlot = fovealPlot, peripheralPlot = peripheralPlot))
}

