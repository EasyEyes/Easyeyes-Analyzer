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
  
  
  beauty = df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(test = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.otf",
           )) %>% 
    filter(!is.na(test))
  
  cmfrt = df_list$QA %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(test = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
           )) %>% 
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
    reading_p <- reading_p + 
      scale_x_log10(limits=c(50,1500)) +
      scale_y_log10(limits=c(50,1500)) +
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
    crowding_p <- crowding_p + 
      scale_x_log10(limits=c(0.03,10)) +
      scale_y_log10(limits=c(0.03,10)) +
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
    acuity_p <- acuity_p + 
    scale_x_log10(limits=c(0.03,10)) +
    scale_y_log10(limits=c(0.03,10)) +
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

   comfort_p <- create_plot(cmfrt, use_jitter = TRUE) 
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


