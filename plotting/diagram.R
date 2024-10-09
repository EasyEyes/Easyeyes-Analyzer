get_quest_diag <- function(quest, pretest){
  if (nrow(quest) == 0) {
    return(ggplot() + theme_bw() + ggtitle('Quest means vs sd'))
  } else {
    if (n_distinct(quest$age) > 1) {
      quest <- quest %>% 
        mutate(Age = format(age, nsmall=2))
      p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop, color = Age))
    } else {
      
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop))
    }
   p <- p +
     geom_point() +
     facet_wrap(~questType) + 
     theme_bw() +
     labs(title = 'Quest means vs SD',
          x = 'Quest means',
          y = 'Quest SD')
    return(p)
  }
}
