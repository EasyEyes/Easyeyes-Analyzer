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
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           )) %>% 
    filter(!is.na(test))
  
  cmfrt = df_list$QA %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(test = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           )) %>% 
    filter(!is.na(test))
  
  create_plot <- function(data) {
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
    
    print(test_retest)
    # plot
    if (nrow(test_retest) == 0) return (NULL)
    n = nrow(test_retest)
    p <- ggplot() +
      geom_point(data=test_retest, 
                 aes(x=test, 
                     y=retest,
                     color = font),
                 size = 3) +
      geom_smooth(data=test_retest, 
                  aes(x=test, 
                      y=retest,
                      color=font),
                  method = 'lm', 
                  se = FALSE) + 
      coord_fixed(ratio = 1) +
      theme_bw() +
      ggpp::geom_text_npc(aes(npcx = 'left',
                              npcy = 'top',
                              label = paste0('N=', n,'\n')))
  }
  
  reading_p <- create_plot(reading)
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
  
  crowding_p <- create_plot(pCrowding)
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
  
  acuity_p <- create_plot(pAcuity)
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
  
  beauty_p <- create_plot(beauty)
  if (!is.null(beauty_p)) {
    beauty_p <- beauty_p + 
      labs(x="Test beauty ratings",
           y="Retest beauty ratings",
           subtitle="Beauty retest vs test")
  }

   comfort_p <- create_plot(cmfrt) 
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


