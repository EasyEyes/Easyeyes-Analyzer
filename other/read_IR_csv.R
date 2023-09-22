get_IR_plot <- function(IR_file) {
  IR <- read_csv(IR_file$datapath, col_names = F)
  colnames(IR) <- c("seq", "impulse response (dB)")
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  
  IR_0to6 <- IR %>% filter(time>=0,
                           time <= 6)
  IR_0to50 <- IR %>% filter(time>=0,
                           time <= 50)
  IR_0to400<- IR %>% 
    filter(time>=0, time <= 400) %>% 
    mutate(db = cumsum((`impulse response (dB)`)^2))
  p1 <- ggplot(IR_0to6,aes(x = time, y = `impulse response (dB)`)) +
    geom_line() +
    xlab("Time (ms)") +
    theme_bw()
  p2 <- ggplot(IR_0to50,aes(x = time, y = `impulse response (dB)`)) +
    geom_line() +
    xlab("Time (ms)") +
    theme_bw()
  p3 <- ggplot(IR_0to400,aes(x = time, y = db)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("dB") + 
    theme_bw()
    return(list(p1,p2,p3))
}

