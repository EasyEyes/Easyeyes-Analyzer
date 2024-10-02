get_quest_diag <- function(quest){
  if (nrow(quest) == 0) {
    return(ggplot() + theme_bw() + ggtitle('Quest means vs sd'))
  } else {
    p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop)) +
      geom_point() +
      facet_wrap(~questType) + 
      theme_bw() +
      labs(title = 'Quest means vs SD',
           x = 'Quest means',
           y = 'Quest SD')
    return(p)
  }
}