


plot_IR_response_0to6 <- function(IR){
  defaultF <- 48
  
  IR$time <- IR$Freq/defaultF
  IR <- IR %>% filter(time>=0,
                      time <= 6)
  
  ggplot(IR,aes(x = time, y = Gain)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") + 
    theme_bw()
}

plot_IR_response_0to50 <- function(IR){
  defaultF <- 48
  
  IR$time <- IR$Freq/defaultF
  IR <- IR %>% filter(time>=0,
                      time <= 50)
  ggplot(IR,aes(x = time, y = Gain)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") + 
    theme_bw()
}

plot_IR_response_0to400 <- function(IR){
  defaultF <- 48
  
  IR$time <- IR$Freq/defaultF
  IR$db <- cumsum((IR$Gain)^2)
  IR <- IR %>% 
    filter(time>=0,
           time <= 400) %>% 
    mutate(db = 10*log(db))
  ggplot(IR,aes(x = time, y = db)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("db") + 
    theme_bw() +
    ggtitle("Schroeder plot") +
    labs(caption = "Schroeder plot: cumulative sum of the squared impulse response, plotted on a dB scale vs time.")
}

