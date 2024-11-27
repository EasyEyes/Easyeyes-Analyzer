source('./constant.R')
get_quest_diag <- function(quest){
  if (nrow(quest) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    quest <- quest %>% 
      group_by(questType) %>% 
      mutate(N = paste0('N=',n()))
    p <- NULL
    p1 <- NULL
    if (n_distinct(quest$Grade) > 1) {
      quest <- quest %>% 
        mutate(Grade = as.character(Grade))
      p1 <- ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, 
                                    y = questSDAtEndOfTrialsLoop, 
                                    color = Grade)) + 
        facet_wrap(~questType) + 
        theme_bw() +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        labs(title = 'Quest SD vs mean colored by grade',
             x = 'Quest mean',
             y = 'Quest SD') +
        theme(aspect.ratio = 1)
    }
      if (n_distinct(quest$age) > 1) {
        quest <- quest %>% 
          mutate(Age = format(age, nsmall=2))
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop, color = Age))
      } else {
        p <-  ggplot(quest, aes(x = questMeanAtEndOfTrialsLoop, y = questSDAtEndOfTrialsLoop))
      }
    
   p <- p +
     ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
     facet_wrap(~questType) + 
     theme_bw() +
     labs(title = 'Quest SD vs mean by age\ncolored by grade',
          x = 'Quest mean',
          y = 'Quest SD') +
     theme(aspect.ratio = 1)
   
   if (n_distinct(quest$`Skilled reader?`) == 1) {
     p1 <- p1 + geom_point()
     p <- p + geom_point()
   } else {
     p1 <- p1 + 
       geom_point(aes(shape = `Skilled reader?`)) +
       scale_shape_manual(values = c(4,19))
     p <- p + 
       geom_point(aes(shape = `Skilled reader?`)) +
       scale_shape_manual(values = c(4,19))
   }
   
    return(list(age = p,
                grade = p1))
  }
}
