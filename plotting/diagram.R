source('./constant.R')
get_quest_diag <- function(quest){
  if (nrow(quest) == 0) {
    return(list(age = NULL, grade = NULL))
  } else {
    print("Quest Tibble")
    print(quest)
    quest <- quest %>% 
      group_by(questType) %>% 
      mutate(N = paste0('N=',n()))
    p <- NULL
    p1 <- NULL
    if (n_distinct(quest$Grade) > 1) {
      quest <- quest %>% 
        mutate(Grade = as.character(Grade))
     
      p1 <- ggplot(quest, aes(x = questSDAtEndOfTrialsLoop, 
                                    y =  questMeanAtEndOfTrialsLoop, 
                                    color = Grade)) + 
        facet_wrap(~questType) + 
        theme_bw() +
        ggpp::geom_text_npc(aes(npcx="left", npcy = 'top', label = N)) + 
        labs(title = 'Quest SD vs mean colored by grade',
             x = 'Quest SD',
             y = 'Quest mean') +
        theme(aspect.ratio = 1, panel.spacing = unit(1, "lines"))
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
     labs(title = 'Quest SD vs mean by age\ncolored by age',
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

get_quest_sd_vs_trials <- function(quest) {
  # Check if the quest tibble is empty
  if (nrow(quest) == 0) {
    return(NULL)
  }
  
  # Ensure required columns are present
  required_columns <- c("participant", "questType", "Grade", "questSDAtEndOfTrialsLoop")
  if (!all(required_columns %in% names(quest))) {
    stop("The `quest` data frame is missing required columns.")
  }
  
  # Add trial count (`questTrials`) to the quest data
  quest <- quest %>%
    group_by(participant, questType) %>%
    mutate(questTrials = row_number())  # Assign a trial number to each row
  
  # Add trial count (`N`) for each questType group
  quest <- quest %>%
    group_by(questType) %>%
    mutate(N = paste0('N=', n()))
  
  # Remove rows with missing data in key columns
  quest <- quest %>%
    drop_na(questTrials, questSDAtEndOfTrialsLoop, Grade)
  
  # Initialize the plot
  p <- NULL
  
  # Check if Grade is present and has more than one unique value
  if (n_distinct(quest$Grade) > 1) {
    quest <- quest %>%
      mutate(Grade = as.character(Grade))  # Ensure Grade is a character for consistent coloring
    
    # Create plot colored by Grade
    p <- ggplot(quest, aes(x = questTrials, y = questSDAtEndOfTrialsLoop, color = Grade)) +
      facet_wrap(~questType) +  # Facet by Quest type
      theme_bw() +  # Add a clean theme
      ggpp::geom_text_npc(aes(npcx = "left", npcy = 'top', label = N)) +  # Add group size annotations
      labs(
        title = 'Quest SD vs quest trials colored by grade',
        x = 'Quest trials',
        y = 'Quest SD'
      ) +
      theme(
        aspect.ratio = 1,  # Equal aspect ratio
        panel.spacing = unit(1, "lines"),  # Spacing between facets
        legend.position = "top"  # Legend on top
      )
  }
  
  # Add points with appropriate shapes for Skilled Reader
  if (n_distinct(quest$`Skilled reader?`) == 1) {
    p <- p + geom_point()
  } else {
    p <- p +
      geom_point(aes(shape = `Skilled reader?`)) +
      scale_shape_manual(values = c(4, 19))  # Different shapes for Skilled Reader categories
  }
  
  # Return the plot
  return(p)
}



