plot_violins <- function(df_list) {
  crowding = df_list$crowding %>% mutate(y = log_crowding_distance_deg)
  rsvp = df_list$rsvp %>% mutate(y = block_avg_log_WPM)
  reading = df_list$reading %>% mutate(y = log_WPM)
  acuity = df_list$acuity %>% mutate(y = questMeanAtEndOfTrialsLoop)
  beauty = df_list$ratings %>%
    filter(questionAndAnswerNickname == 'Bty') %>%
    mutate(y = `mean rating`,
           font = conditionName)
  cmfrt = df_list$ratings %>% 
    filter(grepl('CMFRT',questionAndAnswerNickname)) %>%
    mutate(y = `mean rating`,
           font = questionAndAnswerNickname)
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
          title = title,
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