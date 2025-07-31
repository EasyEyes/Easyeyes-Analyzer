### retest experiments ###
library(ggpubr)

# compare test and retest results
### test vs retest reading ###
get_test_retest_reading <- function(reading){
  # prepare data
  reading_each <- reading %>% 
    group_by(font, participant, block_condition, conditionName) %>%
    summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)), .groups = "drop")
  
  conditionNames <- unique(reading_each$conditionName)
  if (!n_distinct(conditionNames) == 2) {
    return(ggplot() + theme_bw() + ggtitle('test retest reading plot'))
  }
  reading_test_retest <- tibble()
  for (i in 1:length(conditionNames)){
    tmp <- reading_each %>% filter(conditionName == conditionNames[i]) %>% 
      pivot_wider(names_from = block_condition, values_from = avg_wordPerMin) %>% 
      rename(test = 4,
             retest = 5)
    reading_test_retest <- rbind(reading_test_retest, tmp)
  }
  # plot 
  ggplot(reading_test_retest %>% filter(test<3000, retest < 3000), aes(test, retest)) + 
    geom_point(aes(shape = participant, color = participant),  size = 3) +
    facet_wrap(~conditionName)+
    geom_abline(slope = 1) + 
    scale_x_log10(limits = c(100,3000)) + 
    scale_y_log10(limits = c(100,3000)) +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          strip.text = element_text(size=12),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 16)) +
    xlab("Retest reading (w/min)”") +
    ylab("Retest reading (w/min)”") + 
    annotate("text", x = 2800, 
             y = 100, 
             label = paste("italic(n)==",length(unique(reading_test_retest$participant))), 
             parse = TRUE) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    scale_shape_manual(values = sample(1:25, length(unique(reading_test_retest$participant)))) + 
    stat_cor(aes(label = ..r.label..), label.x.npc = "left", label.y.npc = "top", r.accuracy = 0.01)
}

#### crowding ####

get_test_retest_crowding <- function(crowding){
  #TODO fix condition fail to generate plot
  # prepare data
  conditionNames <- unique(crowding$conditionName)
  crowding_test_retest <- tibble()
  if (!n_distinct(conditionNames) == 2) {
    return(ggplot() + theme_bw() + ggtitle('test retest crowding plot'))
  }
  for (i in 1:length(conditionNames)){
    tmp <- crowding %>% 
      filter(conditionName == conditionNames[i]) %>%
      select(participant, block_condition, conditionName,bouma_factor) %>% 
      pivot_wider(names_from = block_condition, values_from = bouma_factor) %>% 
      rename(test = 3,
             retest = 4)
    crowding_test_retest <- rbind(crowding_test_retest, tmp)
    
  }
  # plot 
  ggplot(crowding_test_retest, aes(10^(test), 10^(retest))) + 
    geom_point(aes(shape = participant, color = participant),  size = 3) +
    facet_wrap(~str_sub(conditionName, 7, -1))+
    geom_abline(slope = 1) + 
    scale_x_log10(limits = c(0.2, 100), breaks = c(1,10, 100)) + 
    scale_y_log10(limits = c(0.2, 100), breaks = c(1,10, 100)) +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          strip.text = element_text(size=12,hjust = 0),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1), 
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 16)) +
    xlab("Retest Bouma factor") +
    ylab("Bouma factor") + 
    annotate("text", x = 90, 
             y = 0.3, 
             label = paste("italic(n)==",length(unique(crowding_test_retest$participant))), 
             parse = TRUE) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    scale_shape_manual(values = sample(1:25, length(unique(crowding_test_retest$participant)))) + 
    stat_cor(aes(label = ..r.label..), label.x.npc = "left", label.y.npc = "top", r.accuracy = 0.01)
}

#### rsvp ####
get_test_retest_rsvp <- function(rsvp_speed){
  rsvp_speed <- rsvp_speed %>% select(-log_duration_s_RSVP, -targetKind,-thresholdParameter)
  conditionNames <- unique(rsvp_speed$conditionName)
  if (!n_distinct(conditionNames) == 2) {
    return(ggplot() + theme_bw() + ggtitle('test retest rsvp reading plot'))
  }
  rsvp_test_retest <- tibble()
  for (i in 1:length(conditionNames)){
    tmp <- rsvp_speed %>% filter(conditionName == conditionNames[i]) %>% 
      pivot_wider(names_from = block_condition, values_from = block_avg_log_WPM) %>% 
      rename(test = 4,
             retest = 5)
    rsvp_test_retest <- rbind(rsvp_test_retest, tmp)
  }
  
  ggplot(rsvp_test_retest, aes(10^(test), 10^(retest))) + 
    geom_point(aes(shape = participant, color = participant),  size = 3) +
    facet_wrap(~font)+
    geom_abline(slope = 1) + 
    scale_x_log10() + 
    scale_y_log10() +
    coord_fixed(ratio = 1) +
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          strip.text = element_text(size=12),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),
          plot.title = element_text(size = 16),
          plot.subtitle = element_text(size = 16)) +
    xlab("RSVP reading (w/min)") +
    ylab("Retest RSVP reading (w/min)") + 
    ggtitle("RSVP Reading") + 
    annotate("text", x = 10^(max(rsvp_test_retest$test)*0.9), 
             y = 10^(min(rsvp_test_retest$retest)*0.9), 
             label = paste("italic(n)==",length(unique(rsvp_test_retest$participant))), 
             parse = TRUE) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    scale_shape_manual(values = sample(1:25, length(unique(rsvp_test_retest$participant)))) +
    stat_cor(aes(label = ..r.label..), label.x.npc = "left", label.y.npc = "top", r.accuracy = 0.01)
}
