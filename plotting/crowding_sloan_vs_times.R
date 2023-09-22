
sloan_vs_times_plots <- function(crowding) {
  t <- crowding %>% group_by(participant, font) %>% 
    summarize(MEAN = mean(log10(bouma_factor)),
              SD = sd(log10(bouma_factor))) %>% 
    mutate(MEAN = round(MEAN,2),
           SD = round(SD, 2)) %>% 
    arrange(participant, font)
  n <- n_distinct(crowding$participant)
  # write.csv(t, "CrowdingSloanVsTimes24_2023-6-1_21-21-00 (1)_bouma-factors.csv", row.names=TRUE)
  
  for_plot_means <- t %>% pivot_wider(names_from = font, values_from = MEAN)
  sloan_means <- for_plot_means %>% filter(!is.na(Sloan.woff2)) %>% select(-TimesNewRomanRegularMonotype.woff2)
  time_means <- for_plot_means %>% filter(!is.na(TimesNewRomanRegularMonotype.woff2)) %>% select(-Sloan.woff2)
  for_plot <- sloan_means %>% inner_join(time_means,by = "participant")
  
  mean_plot <- ggplot(for_plot, aes(x = 10^(Sloan.woff2), y = 10^(TimesNewRomanRegularMonotype.woff2))) + 
    geom_point() + 
    scale_y_log10() +
    scale_x_log10() + 
    geom_smooth(method = "lm",
                formula = y ~ x, 
                se=F) + 
    theme_bw() + 
    coord_fixed(ratio = 1) + 
    labs(x = "sloan Bouma Factor", y = "TimesNewRoman Bouma Factor") +
    downloadtheme + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", n)), 
      parse = T) + 
    stat_cor(aes(label = ..r.label..),r.accuracy = 0.001)
  
  for_plot_sd <- t %>% pivot_wider(names_from = font, values_from = SD)
  sloan_sd <- for_plot_sd %>% filter(!is.na(Sloan.woff2)) %>% select(-TimesNewRomanRegularMonotype.woff2)
  time_sd <- for_plot_sd %>% filter(!is.na(TimesNewRomanRegularMonotype.woff2)) %>% select(-Sloan.woff2)
  for_plot <- sloan_sd %>% inner_join(time_sd,by = "participant")
  
  sd_plot <- ggplot(for_plot, aes(x = 10^(Sloan.woff2), y = 10^(TimesNewRomanRegularMonotype.woff2))) + 
    geom_point() + 
    scale_y_log10() +
    scale_x_log10() + 
    geom_smooth(method = "lm",
                formula = y ~ x, 
                se=F) + 
    theme_bw() + 
    coord_fixed(ratio = 1) + 
    labs(x = "sloan Bouma Factor", y = "TimesNewRoman Bouma Factor") +
    downloadtheme + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", n)), 
      parse = T) + 
    stat_cor(aes(label = ..r.label..),r.accuracy = 0.001)
  return(list(mean_plot, sd_plot))
}


