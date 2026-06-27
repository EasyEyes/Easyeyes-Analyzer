library(tidyr)
random_rgb <- function(n){
  zero <- rep(255,n)
  r <- sample(239:244, n, T)
  g <- sample(239:244, n, T)
  b <- sample(239:244, n, T)
  t1 <- tibble(r,g,b,zero) %>% 
    mutate(rg = rgb(r,g,zero,maxColorValue = 255),
           rb = rgb(r,zero,b,maxColorValue = 255),
           gb = rgb(zero,g,b,maxColorValue = 255),
           rgb = rgb(r,g,b,maxColorValue = 255),
           r = rgb(r,zero,zero,maxColorValue = 255),
           g = rgb(zero,g,zero,maxColorValue = 255),
           b = rgb(zero,zero,b,maxColorValue = 255)) %>% 
    select(-zero) %>% 
    pivot_longer(1:7, names_to = "color type", values_to = "rgb")
  if (n < 7) {
   t1 <- t1 %>% slice(1:n)
   return(sample(t1$rgb,n))
  } else {
    t1 <- t1 %>% 
      slice(1:floor(n/4))
  }

  r <- sample(239:244, n, T)
  g <- sample(239:244, n, T)
  b <- sample(245:249, n, T)
  
  t2 <- tibble(r,g,b,zero) %>% 
    mutate(rg = rgb(r,g,zero,maxColorValue = 255),
           rb = rgb(r,zero,b,maxColorValue = 255),
           gb = rgb(zero,g,b,maxColorValue = 255),
           rgb = rgb(r,g,b,maxColorValue = 255),
           r = rgb(r,zero,zero,maxColorValue = 255),
           g = rgb(zero,g,zero,maxColorValue = 255),
           b = rgb(zero,zero,b,maxColorValue = 255)) %>% 
    select(-zero) %>% 
    pivot_longer(1:7, names_to = "color type", values_to = "rgb") %>% 
    slice(1:floor(n/4))
  
  r <- sample(250:254, n, T)
  g <- sample(239:244, n, T)
  b <- sample(239:244, n, T)
  
  t3 <- tibble(r,g,b,zero) %>% 
    mutate(rg = rgb(r,g,zero,maxColorValue = 255),
           rb = rgb(r,zero,b,maxColorValue = 255),
           gb = rgb(zero,g,b,maxColorValue = 255),
           rgb = rgb(r,g,b,maxColorValue = 255),
           r = rgb(r,zero,zero,maxColorValue = 255),
           g = rgb(zero,g,zero,maxColorValue = 255),
           b = rgb(zero,zero,b,maxColorValue = 255)) %>% 
    select(-zero) %>% 
    pivot_longer(1:7, names_to = "color type", values_to = "rgb") %>% 
    slice(1:floor(n/4))
  
  r <- sample(239:244, n, T)
  g <- sample(250:254, n, T)
  b <- sample(239:244, n, T)
  
  t4 <- tibble(r,g,b,zero) %>% 
    mutate(rg = rgb(r,g,zero,maxColorValue = 255),
           rb = rgb(r,zero,b,maxColorValue = 255),
           gb = rgb(zero,g,b,maxColorValue = 255),
           rgb = rgb(r,g,b,maxColorValue = 255),
           r = rgb(r,zero,zero,maxColorValue = 255),
           g = rgb(zero,g,zero,maxColorValue = 255),
           b = rgb(zero,zero,b,maxColorValue = 255)) %>% 
    select(-zero) %>% 
    pivot_longer(1:7, names_to = "color type", values_to = "rgb") %>% 
    slice(1:(n-3*floor(n/4)))
  
  t <- rbind(t1,t2,t3,t4)
  
  return(sample(t$rgb,n))
}