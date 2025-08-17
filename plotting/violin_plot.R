plot_violins <- function(df_list) {
  crowding = df_list$crowding %>% mutate(y = log_crowding_distance_deg)
  rsvp = df_list$rsvp %>% mutate(y = block_avg_log_WPM)
  reading = df_list$reading %>% mutate(y = log_WPM)
  acuity = df_list$acuity %>% mutate(y = questMeanAtEndOfTrialsLoop)
  beauty = df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           )) %>% 
    filter(!is.na(y))
  
  cmfrt = df_list$QA %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(y = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.ttf",
           ))
  print("inside plot_violins")
  create_plot <- function(data, ylabel, title) {
    p <- NULL
    print(nrow(data))
    if (nrow(data) > 0) {
      p <- ggplot(data, aes(x = font, y = y)) +
        geom_violin(trim = FALSE, alpha = 0.5) +
        geom_jitter(width = 0.15, alpha = 0.7) +
        coord_flip() +
        theme_minimal(base_size = 14) +
        labs(
          subtitle = title,
          x = "Font",
          y = ylabel
        )
    }
    return(p)
  }
  
  return(list(
    reading = create_plot(reading, "Log Reading Speed(word/min)", "Log Reading Speed by Font"),
    rsvp = create_plot(rsvp, "Log RSVP Reading Speed(word/min)", "Log RSVP Reading Speed by Font"),
    crowding = create_plot(crowding, "Log Crowding Distance (deg)", "Crowding Threshold by Font"),
    acuity = create_plot(acuity, "Log acuity (deg)", "Acuity Threshold by Font"),
    beauty = create_plot(beauty, "Beauty Rating", "Beauty Rating by Font"),
    cmfrt = create_plot(cmfrt, "Comfort Rating", "Comfort Rating by Font")
  ))
}