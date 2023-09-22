require(jsonlite)
read_IR_JSON <- function(fileJSON) {
  file_list <- fileJSON$data
  result <- tibble()
  for (i in 1:length(file_list)){
    IR <- fromJSON(file_list[i])
    for (j in 1:length(IR)){
      if (class(IR[[names(IR)[j]]]) == "data.frame"){
        tmp <- tibble(IR = unlist(IR[[names(IR)[j]]]$Gain))
        tmp$name <-  names(IR)[j]
      } else {
        tmp <- tibble(IR = unlist(IR[[names(IR)[j]]]))
        tmp$name <-  names(IR)[j]
      }
      result <- rbind(result, tmp)
    }
  }
  return(result)
}

get_json_plots <- function(result, selected) {
  if (is.null(selected)) {
    return(list(ggplot(),
                ggplot(),
                ggplot()))
  }
  
  t <- result %>% filter(name == selected)
  
  defaultF <- 48
  time <- 1/defaultF
  t$time <- seq(0,(nrow(t) - 1)*time,time)

  IR_0to6 <- t %>% filter(time>=0,
                           time <= 6)
  IR_0to50 <- t %>% filter(time>=0,
                            time <= 50)
  IR_0to400<- t %>% 
    filter(time>=0, time <= 400) %>% 
    mutate(db = cumsum((IR)^2))
  
  p1 <- ggplot(IR_0to6,aes(x = time, y = IR)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("v/v.s") + 
    theme_bw() +
    ggtitle("0 to 6 ms")
  
  p2 <- ggplot(IR_0to50,aes(x = time, y = IR)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("v/v.s") + 
    theme_bw() +
    ggtitle("0 to 50 ms")
  
  p3 <- ggplot(IR_0to400,aes(x = time, y = db)) +
    geom_line() +
    xlab("Time (ms)") +
    ylab("dB") + 
    theme_bw() +
    ggtitle("schroeder plot, 0 to 400 ms")
  return(list(p1,p2,p3))
  
}

