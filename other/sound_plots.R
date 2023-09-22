plot_impulse_response <- function(micro_cali_result){
  ggplot(micro_cali_result) +
    geom_line(aes(x = `Recording (Hz)`, 
                  y = `Recording (dB)`, 
                  color = filter)) +
    theme_bw() +
    sound_theme + 
    ggtitle(unique(micro_cali_result$name))
}

plot_microphone_recording_1000hz <- function(micro_cali_result){
  ggplot(micro_cali_result, aes(x = in_dB_1000Hz, y = out_dBSPL_1000Hz)) +
    geom_point() + 
    theme_bw() +
    sound_theme + 
    scale_x_continuous(n.breaks = 7, 
                       limits = c(-60, 0),
                       expand = c(0,0)) + 
    ggtitle(unique(micro_cali_result$name)) + 
    xlab("in (dB)") +
    ylab("out (DB)")
}


plot_IR_response_0to6 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR <- IR %>% filter(time>=0,
                      time <= 6)
  
  ggplot(IR,aes(x = time, y = Gain)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") + 
    theme_bw() +
    sound_theme + 
    ggtitle(IR$name[1])
}

plot_IR_response_0to50 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR <- IR %>% filter(time>=0,
                      time <= 50)
  ggplot(IR,aes(x = time, y = Gain)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") + 
    theme_bw() +
    sound_theme + 
    ggtitle(IR$name[1])
}

plot_IR_response_0to400 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR$db <- cumsum((IR$Gain)^2)
  IR <- IR %>% filter(time>=0,
                      time <= 400)
  ggplot(IR,aes(x = time, y = db)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("db") + 
    theme_bw() +
    sound_theme + 
    ggtitle(IR$name[1])
}

sound_theme <- theme(legend.direction = "vertical", 
                         legend.box = "horizontal",
                         legend.position = "right", 
                         legend.key = element_rect(fill = "white"),
                         legend.key.size = unit(4.5, "mm"),
                         legend.title = element_text(size=14),
                         legend.text = element_text(size=14),
                         panel.background = element_blank(), 
                         axis.title = element_text(size = 14),
                         axis.text = element_text(size = 14),
                         axis.line = element_line(colour = "black"),
                         plot.title = element_text(size=11),
                         plot.subtitle = element_text(size=14))

title = "Power spectral density of sound recording of white noise (MLS) source eplayed through the loudspeakers"
