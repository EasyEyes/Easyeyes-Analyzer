get_quest_diag <- function(quest, pretest){
  if (nrow(quest) == 0) {
    return(ggplot() + theme_bw() + ggtitle('Quest means vs sd'))
  } else {
    if (nrow(pretest) > 0) {
      quest <- quest %>% left_join(pretest, by = 'participant') %>% 
        mutate(Age = as.character(Age))
      p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop, color = Age)) +
        geom_point() +
        facet_wrap(~questType) + 
        theme_bw() +
        labs(title = 'Quest means vs SD',
             x = 'Quest means',
             y = 'Quest SD')
    } else {
      if (n_distinct(quest$age) > 1) {
        quest$age = as.character(quest$age)
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop, color = age)) +
          geom_point() +
          facet_wrap(~questType) + 
          theme_bw() +
          labs(title = 'Quest means vs SD',
               x = 'Quest means',
               y = 'Quest SD')
      } else {
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop)) +
          geom_point() +
          facet_wrap(~questType) + 
          theme_bw() +
          labs(title = 'Quest means vs SD',
               x = 'Quest means',
               y = 'Quest SD')
      }
     
    }
   
    return(p)
  }
}
