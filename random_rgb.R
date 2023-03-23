library(tidyr)
random_rgb <- function(n){
  zero <- rep(255,n)
  r <- sample(230:250, n, T)
  g <- sample(230:250, n, T)
  b <- sample(230:250, n, T)
  
  t <- tibble(r,g,b,zero) %>% 
    mutate(rg = rgb(r,g,zero,maxColorValue = 255),
           rb = rgb(r,zero,b,maxColorValue = 255),
           gb = rgb(zero,g,b,maxColorValue = 255),
           rgb = rgb(r,g,b,maxColorValue = 255),
           r = rgb(r,zero,zero,maxColorValue = 255),
           g = rgb(zero,g,zero,maxColorValue = 255),
           b = rgb(zero,zero,b,maxColorValue = 255)) %>% 
    select(-zero) %>% 
    pivot_longer(1:7, names_to = "color type", values_to = "rgb") %>% 
    slice(sample(1:n()))
  return(sample(t$rgb, n))
}