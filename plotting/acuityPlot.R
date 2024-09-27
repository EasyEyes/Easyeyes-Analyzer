get_acuity_vs_age <- function(acuity) {
  if (nrow(acuity) == 0) {
    return(ggplot() + ggtitle('acuity vs age')) + theme_bw()
  }
  t <- acuity %>% filter(!is.na(age))
  if (nrow(t) == 0) {
    return(ggplot() + theme_bw() + ggtitle('acuity vs age'))
  } else {
    p <-  ggplot(t, aes(x = age, y = questMeanAtEndOfTrialsLoop)) +
      geom_point() +
      theme_bw() +
      labs(title = 'Acuity vs age',
           x = 'Age',
           y = 'Acuity (deg)')
    return(p)
  }
}

plot_acuity_reading <- function(acuity,reading){
  
}

plot_acuity_rsvp <- function(acuity, rsvp) {
  
}