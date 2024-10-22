source('./constant.R')
get_quest_diag <- function(quest, pretest){
  if (nrow(quest) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    quest <- quest %>% 
      group_by(questType) %>% 
      mutate(N = paste0('N=',n()))
    if (nrow(pretest) > 0) {
      quest <- quest %>% 
        mutate(participant = tolower(participant)) %>% 
        left_join(pretest %>% mutate(participant =tolower(participant))) %>% 
        mutate(Age = format(age, nsmall=2),
               Grade = as.character(Grade))
      p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, 
                              y = questSDAtEndOfTrialsLoop, 
                              shape = `Skilled reader?`,
                              color = Age))
      p1 <- ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, 
                                    y = questSDAtEndOfTrialsLoop, 
                                    shape = `Skilled reader?`,
                                    color = Grade)) + 
        geom_point() +
        facet_wrap(~questType) + 
        theme_bw() +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        labs(title = 'Quest SD vs mean by grade',
             x = 'Quest means',
             y = 'Quest SD') +
        coord_fixed()
    } else {
      if (n_distinct(quest$age) > 1) {
        quest <- quest %>% 
          mutate(Age = format(age, nsmall=2))
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop, color = Age))
        p1 <- NULL
      } else {
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop))
        p1 <- NULL
      }
    }
    
   p <- p +
     ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
     geom_point() +
     facet_wrap(~questType) + 
     theme_bw() +
     labs(title = 'Quest SD vs mean by age',
          x = 'Quest means',
          y = 'Quest SD') +
     coord_fixed()
    return(list(age = p,
                grade = p1))
  }
}
