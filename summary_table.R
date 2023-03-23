generate_summary_table <- function(data_list){
  all_files <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(ProlificParticipantID, participant, deviceType, cores,                             browser, deviceSystemFamily,  system, deviceLanguage,
                              block, block_condition,conditionName, targetTask, targetKind, 
                              resolution,error, warning)
    
  }
  max_trails <- as.numeric(all_files %>% count(participant) %>% summarize(max(n)))
  
  error <- all_files %>% 
    filter(error != "") %>%
    group_by(participant) %>%
    mutate(ok = emoji("x"))
  
  warnings <- all_files %>% 
    filter(warning != "") %>%
    group_by(participant) %>%
    mutate(ok = emoji("large_orange_diamond"))
  
  noerror_fails = tibble()
  for (i in 1 : length(data_list)) {
    if (nrow(data_list[[i]]) < max_trails) {
      if (!data_list[[i]]$participant[1] %in% error$participant) {
        t <- data_list[[i]] %>% 
          distinct(ProlificParticipantID,participant, deviceType,
                   cores, system, browser, resolution) %>% 
          mutate(error = "invalid file") %>% 
          select(error,ProlificParticipantID,participant, deviceType, 
                 cores, system, browser, resolution)
        info <- data_list[[i]] %>% 
          distinct(block, block_condition, conditionName, 
                   targetTask, targetKind) %>% 
          filter(block_condition != "") 
        if (nrow(info) > 0) {
          info <- info %>% tail(1)
        } else {
          info <- tibble(block = NA,
                         block_condition = NA, 
                         conditionName = NA, 
                         targetTask = NA, 
                         targetKind = NA)
        }
        t <- cbind(t, info)
        t$ok <- emoji("x")
        noerror_fails <- rbind(noerror_fails,t)
      }
    }
  }
  noerror_fails$warning = ""
  completes = tibble()
  for (i in 1 : length(data_list)) {
    if (!data_list[[i]]$participant[1] %in% error$participant 
        &!data_list[[i]]$participant[1] %in% noerror_fails$participant) {
      t <- data_list[[i]] %>% 
        distinct(ProlificParticipantID,participant, deviceType, error,
                 cores, system, browser, resolution)
      info <- data_list[[i]] %>% 
        distinct(block, block_condition, conditionName, 
                 targetTask, targetKind) %>% 
        filter(block_condition != "") %>% 
        tail(1)
      t <- cbind(t, info)
      t$ok <- emoji("white_check_mark")
      completes <- rbind(completes,t)
    }
  }
  completes$warning <- ""
  
  summary_df <- rbind(noerror_fails,
               completes,
               error %>% 
                 select(error, warning, ProlificParticipantID, participant, deviceType, 
                        cores, system, browser, resolution,
                        block, block_condition, conditionName, targetTask, targetKind, ok),
               warnings %>% 
                 select(error, warning, ProlificParticipantID, participant, deviceType, 
                        cores, system, browser, resolution,
                        block, block_condition, conditionName, targetTask, targetKind, ok)) %>% 
    arrange(desc(block_condition)) %>% 
    rename("Prolific ID" = "ProlificParticipantID", "Pavlovia ID" = "participant") %>% 
    mutate(ok = factor(ok, levels = c(emoji("x"), emoji("white_check_mark"), emoji("large_orange_diamond")))) %>% 
    select(`Prolific ID`, `Pavlovia ID`, deviceType, cores, system, browser, 
           resolution, ok, error, warning, block_condition, conditionName, targetTask, targetKind)
  summary_df <- summary_df %>%
    arrange(ok)
  summary_df[is.na(summary_df)] <- ""
  return(summary_df)
}