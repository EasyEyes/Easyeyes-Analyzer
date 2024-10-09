
# Add a plot of reading speed (word/min) vs. letter size (mm).  
# Use log scaling in X and Y. We’ll have two connected points 
# for 30 cm viewing distance and two connected points for 60 
# cm viewing distance. Add error bars for each point. Shift 
# some points horizontally slightly so they don’t occlude each other.
# Use the SE from “Summary across all participants”. 
# For each condition we have m and se. The mean is at 60/10^m. 
# The error bar goes from 60/10^(m+se/2) to 60/10^(m-se/2)
plot_rsvp_vs_x_height <- function(rsvp_speed){
  rsvp_summary <- rsvp_speed %>%
    group_by(participant, conditionName, viewingDistanceDesiredCm) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_summary <- rsvp_summary %>% 
    group_by(conditionName, viewingDistanceDesiredCm) %>% 
    dplyr::summarize(
      m = mean(pm, na.rm = T),
      se = sd(pm)/sqrt(n()), 
      N = n(),
      parameter = "threshold")
  
  rsvp_summary$x_height <- NA
  rsvp_summary$x_height <- ifelse(str_detect(rsvp_summary$conditionName, "1.2 mm"), 
                                  1.2, rsvp_summary$x_height)
  rsvp_summary$x_height <- ifelse(str_detect(rsvp_summary$conditionName, "1.4 mm"), 
                                  1.4, rsvp_summary$x_height)
  pd <- position_dodge(width = 0.005)
  minN <- min(rsvp_summary$N)
  maxN <- max(rsvp_summary$N)
  rsvp_summary <- rsvp_summary %>% 
    mutate(x_height = ifelse(viewingDistanceDesiredCm == 60, x_height + 0.005, x_height))
  N_text <- ifelse(minN == maxN, minN, paste0(minN, "~to~",maxN))
  rsvp_summary
  p1 <- ggplot(rsvp_summary, aes(x = x_height, y = 60/(10^(m)), color = as.factor(viewingDistanceDesiredCm))) + 
    geom_point() + 
    geom_line() +
    theme_bw() + 
    scale_x_log10() + 
    scale_y_log10(limits = c(250, 1000),breaks = c(250, 500, 750, 1000)) + 
    # this is to dodge error bar
    # geom_errorbar(aes(ymin=60/(10^(m-se/2)),
    #                   ymax=60/(10^(m+se/2))), width=0, position = pd) +
    geom_errorbar(aes(ymin=60/(10^(m-se/2)),
                      ymax=60/(10^(m+se/2))), width=0) +
    xlab("x height (mm)") +
    ylab("Reading speed (w/min) ") +
    guides(color = guide_legend(title = "viewing distance (cm)")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  p2 <- ggplot(rsvp_summary, aes(x = x_height, y = 60/(10^(m)), color = as.factor(viewingDistanceDesiredCm))) + 
    geom_point() + 
    geom_line() +
    theme_bw() + 
    scale_y_continuous(limits = c(250, 1000),breaks = c(250, 500, 750, 1000)) + 
    # this is to dodge error bar
    # geom_errorbar(aes(ymin=60/(10^(m-se/2)),
    #                   ymax=60/(10^(m+se/2))), width=0, position = pd) +
    geom_errorbar(aes(ymin=60/(10^(m-se/2)),
                      ymax=60/(10^(m+se/2))), width=0) +
    xlab("x height (mm)") +
    ylab("Reading speed (w/min)") +
    guides(color = guide_legend(title = "viewing distance (cm)")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  return(list(p1,p2))
}

get_60cm_data <- function(rsvp_speed){
  rsvp_speed <- rsvp_speed %>% 
    group_by(participant, conditionName, viewingDistanceDesiredCm, age) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_speed$x_height <-  NA
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.2 mm"), 
                                1.2, rsvp_speed$x_height)
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.4 mm"), 
                                1.4, rsvp_speed$x_height)
  
  rsvp_speed_60 <- rsvp_speed %>% filter(viewingDistanceDesiredCm == 60)
  rsvp_speed_60_1.2 <- rsvp_speed_60 %>% filter(x_height == 1.2) %>% select(participant, pm, age)
  rsvp_speed_60_1.4 <- rsvp_speed_60 %>% filter(x_height == 1.4) %>% select(participant, pm)
  
  rsvp_speed_60_1.2_vs_1.4 <- rsvp_speed_60_1.2 %>% left_join(rsvp_speed_60_1.4, by = "participant")
  return(rsvp_speed_60_1.2_vs_1.4)
}

get_60cm_scatter <- function(rsvp_speed) {
  rsvp_speed_60_1.2_vs_1.4 <- get_60cm_data(rsvp_speed)
  ggplot(rsvp_speed_60_1.2_vs_1.4, aes(60/10^(pm.x), 60/10^(pm.y))) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks() +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    xlab("reading speed (w/min) 1.2 mm") +
    ylab("reading speed (w/min) 1.4 mm") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_60_1.2_vs_1.4))), 
      parse = T) + 
    geom_abline(intercept = 0, slope = 1)
}

get_30cm_scatter <- function(rsvp_speed) {
  rsvp_speed <- rsvp_speed %>% group_by(participant, conditionName, viewingDistanceDesiredCm) %>%
    dplyr::summarize(
      pm = mean(log_duration_s_RSVP),
      sd = sd(log_duration_s_RSVP)) %>% 
    ungroup() 
  
  rsvp_speed$x_height <-  NA
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.2 mm"), 
                                1.2, rsvp_speed$x_height)
  rsvp_speed$x_height <- ifelse(str_detect(rsvp_speed$conditionName, "1.4 mm"), 
                                1.4, rsvp_speed$x_height)
  
  rsvp_speed_30 <- rsvp_speed %>% filter(viewingDistanceDesiredCm == 30)
  rsvp_speed_30_1.2 <- rsvp_speed_30 %>% filter(x_height == 1.2) %>% select(participant, pm)
  rsvp_speed_30_1.4 <- rsvp_speed_30 %>% filter(x_height == 1.4) %>% select(participant, pm)
  
  rsvp_speed_30_1.2_vs_1.4 <- rsvp_speed_30_1.2 %>% left_join(rsvp_speed_30_1.4, by = "participant")
  
  ggplot(rsvp_speed_30_1.2_vs_1.4, aes(60/10^(pm.x), 60/10^(pm.y))) +
    geom_point() + 
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks() +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    xlab("reading speed (w/min)1.2 mm") +
    ylab("reading speed (w/min) 1.4 mm") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_30_1.2_vs_1.4))), 
      parse = T) + 
    geom_abline(intercept = 0, slope = 1)
}

plot_60cm_speed_diff_vs_age <- function(rsvp_speed){
  rsvp_speed_60_1.2_vs_1.4 <- get_60cm_data(rsvp_speed)
  rsvp_speed_60_1.2_vs_1.4 <- rsvp_speed_60_1.2_vs_1.4 %>% 
    mutate(speed_diff = (60/10^(pm.y) - 60/10^(pm.x)))
  ggplot(rsvp_speed_60_1.2_vs_1.4, aes(x = age, y = speed_diff)) +
    geom_point() + 
    theme_bw() + 
    xlab("age") +
    ylab("Reading speed difference (w/min)") + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", nrow(rsvp_speed_60_1.2_vs_1.4))), 
      parse = T)
}



plot_reading_age <- function(reading){
  t <- reading %>% filter(!is.na(age))
  if (nrow(t) == 0) {
    return(ggplot() + theme_bw() + ggtitle('Reading vs age'))
  } else {
    p <-  ggplot(t, aes(x = age, y = 10^(log_WPM))) +
      scale_y_log10() + 
      geom_point() +
      theme_bw() +
      labs(title = 'Reading vs age',
           x = 'Age',
           y = 'Reading speed (w/min)')
    return(p)
  }
}

plot_rsvp_age <- function(rsvp){
  t <- rsvp %>% filter(!is.na(age))
  print('rsvp max')
  print(max(10^(t$block_avg_log_WPM)))
  if (nrow(t) == 0) {
    return(ggplot() + theme_bw() + ggtitle('Rsvp reading vs age'))
  } else {
    p <-  ggplot(t, aes(x = age, y = 10^(block_avg_log_WPM))) +
      scale_y_log10() + 
      geom_point() +
      theme_bw() +
      labs(title = 'Rsvp reading vs age',
           x = 'Age',
           y = 'RSVP reading speed (w/min)')
    return(p)
  }
}

plot_reading_rsvp <- function(reading,rsvp){
  if (nrow(reading) == 0 | nrow(rsvp) == 0) return(
    labs(x="Rsvp reading (w/min)", 
         y = "Reading (w/min)",
         title = 'Reading vs rsvp reading') +
      theme_bw()
  )
  rsvp <-rsvp %>% 
    mutate(participant = tolower(participant)) %>% 
    group_by(participant,targetKind) %>% 
    summarize( avg_log_WPM = mean(block_avg_log_WPM))
  t <- reading %>%
    mutate(participant = tolower(participant)) %>% 
    group_by(participant, block_condition, targetKind) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)), .groups = "keep") %>% 
    ungroup() %>% 
    filter(avg_wordPerMin <= 1500) %>% 
    mutate(log_WPM = log10(avg_wordPerMin)) %>% 
    group_by(participant, targetKind) %>% 
    summarize(avg_log_WPM = mean(log10(avg_wordPerMin))) %>% 
    left_join(rsvp, by = 'participant')
  
  p <- ggplot(t,aes(x = 10^(avg_log_WPM.y), y = 10^(avg_log_WPM.x))) + 
    geom_point() +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_x_log10() + 
    scale_y_log10() +
    labs(x="Rsvp reading (w/min)", 
         y = "Reading (w/min)",
         title = 'Reading vs rsvp reading') +
    theme_bw() + 
    theme(legend.position='top') + 
    annotation_logticks() +
    guides(color=guide_legend(title=""))+
  return(p)
}
