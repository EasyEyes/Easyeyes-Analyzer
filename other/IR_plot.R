library(readr)
library(ggplot2)
defaultF <- 48
time <- 1/defaultF

plot_IR_response <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  ggplot(IR,aes(x = time, y = X2)) +
    geom_line() +
    xlab("time (ms)") +
    ylab("IR") + 
    theme_bw() +
    ggtitle(IR$name[1])
}


plot_IR_response_0to6 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR <- IR %>% filter(time>=0,
                      time <= 6)
  ggplot(IR,aes(x = time, y = X2)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") +
    theme_bw() +
    ggtitle(IR$name[1])
}

plot_IR_response_0to50 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR <- IR %>% filter(time>=0,
                      time <= 50)
  ggplot(IR,aes(x = time, y = X2)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("IR") + 
    theme_bw()  +
    ggtitle(IR$name[1])
}

plot_IR_response_0to400 <- function(IR){
  defaultF <- 48
  time <- 1/defaultF
  
  IR$time <- seq(0,(nrow(IR) - 1)*time,time)
  IR$db <- cumsum((IR$X2)^2)
  IR <- IR %>% filter(time>=0,
                      time <= 400)
  ggplot(IR,aes(x = time, y = db)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("db") + 
    theme_bw() +
    ggtitle(IR$name[1])
}




