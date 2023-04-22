crowding_by_side <- function(crowding) {
  crowding$side <- str_sub(crowding$conditionName,-1,-1)
  crowding_L <- crowding %>% filter(side == "L") %>% select(-conditionName, -side)
  crowding_R <- crowding %>% filter(side == "R") %>% select(-conditionName, -side)
  crowding_L_R <- crowding_L %>% 
    left_join(crowding_R, by = c("participant","font")) %>% 
    rename("bouma_factor_Left" = "bouma_factor.x",
           "bouma_factor_Right" = "bouma_factor.y")
  return(crowding_L_R)
}

crowding_scatter_plot <- function(crowding_L_R){
  ggplot(crowding_L_R,aes(x = 10^(bouma_factor_Left), y = 10^(bouma_factor_Right))) + 
    geom_point(size = 1) + 
    facet_wrap(~font) + 
    scale_y_log10(breaks = c(1,3,10,30)) +
    scale_x_log10(breaks = c(1,3,10,30)) + 
    annotation_logticks(short = unit(0.1, "cm"),                                                
                        mid = unit(0.1, "cm"),
                        long = unit(0.3, "cm")) + 
    xlab("Left Bouma factor") + 
    ylab("Right Bouma factor") + 
    coord_fixed(ratio = 1) + 
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=16),
          plot.subtitle = element_text(size = 16),
          strip.text = element_text(hjust = 0))
}

crowding_mean_scatter_plot <- function(crowding_L_R){
  t <- crowding_L_R %>% group_by(font) %>% summarize(avg_bouma_factor_Left = mean(bouma_factor_Left),
                                                     avg_bouma_factor_Right = mean(bouma_factor_Right))
  ggplot(t, aes(x = 10^(avg_bouma_factor_Left), y = 10^(avg_bouma_factor_Right), color = font, shape = font)) + 
    geom_point(size = 2) +
    scale_y_log10() +
    scale_x_log10() + 
    scale_shape_manual(values = sample(0:25, length(unique(t$font)), F)) + 
    xlab("Left Bouma factor") + 
    ylab("Right Bouma factor") + 
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=16))
}