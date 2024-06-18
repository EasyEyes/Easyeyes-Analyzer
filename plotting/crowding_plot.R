crowding_by_side <- function(crowding) {
  crowding$side <- ifelse(grepl("left", crowding$conditionName), "L", "R")
  crowding_L <- crowding %>% filter(side == "L") %>% select(-conditionName, -side)
  crowding_R <- crowding %>% filter(side == "R") %>% select(-conditionName, -side)
  crowding_L_R <- crowding_L %>% 
    left_join(crowding_R, by = c("participant","font")) %>% 
    rename("bouma_factor_Left" = "bouma_factor.x",
           "bouma_factor_Right" = "bouma_factor.y")
  print('==========================  crowding_L_R ==========================')
  print(crowding_L_R)
  return(crowding_L_R)
}

crowding_scatter_plot <- function(crowding_L_R){
  summ <- group_by(crowding_L_R, font) %>% 
    summarize(R = cor.test(bouma_factor_Left, bouma_factor_Right)$estimate,
              ratio = round(mean(bouma_factor_Right) / mean(bouma_factor_Left),2),
              N = n()) %>%
    mutate(R = formatC(R, format = "fg", digits = 2),
           ratio = formatC(ratio, format = "fg", digits = 2)) %>% 
    mutate(label =  paste0("italic('R=')~", 
                           R,
                           "~italic(', right:left=')~", ratio),
           N = paste0("italic('N=')~", N))
  
  ggplot(crowding_L_R,aes(x = bouma_factor_Left, y = bouma_factor_Right)) + 
    geom_point(size = 1) + 
    facet_wrap(~font) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_y_log10(breaks = c(1,3,10,30)) +
    scale_x_log10(breaks = c(1,3,10,30)) + 
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    xlab("Left Bouma factor") + 
    ylab("Right Bouma factor") + 
    coord_fixed(ratio = 1) + 
    theme_bw() + 
    ggpp::geom_text_npc(
      data = summ,
      aes(npcx = "left",
          npcy = "bottom",
          label = label), 
      parse = T) + 
    ggpp::geom_text_npc(
      data = summ,
      aes(npcx = "left",
          npcy = "top",
          label = N),
      parse = T)
}

crowding_mean_scatter_plot <- function(crowding_L_R){
  t <- crowding_L_R %>% group_by(font) %>% summarize(avg_bouma_factor_Left = mean(bouma_factor_Left),
                                                     avg_bouma_factor_Right = mean(bouma_factor_Right))
  ggplot(t, aes(x = avg_bouma_factor_Left, y = avg_bouma_factor_Right, color = font)) + 
    geom_point(size = 2) +
    scale_y_log10(limits = c(0.1, ceiling(max(t$avg_bouma_factor_Left, t$avg_bouma_factor_Right)))) +
    scale_x_log10(limits = c(0.1, ceiling(max(t$avg_bouma_factor_Left, t$avg_bouma_factor_Right)))) + 
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    stat_cor() +  
    coord_fixed(ratio = 1) + 
    guides(color = guide_legend(title="Font")) + 
    xlab("Left Bouma factor") + 
    ylab("Right Bouma factor") + 
    theme_bw() + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "top",
          label = paste0("italic('R=')~", cor(t$avg_bouma_factor_Left, t$avg_bouma_factor_Left))), 
      parse = T) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", dplyr::n_distinct(crowding_L_R$participant))), 
      parse = T)
}