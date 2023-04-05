generate_summary_table <- function(data_list){
  all_files <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(ProlificParticipantID, participant, deviceType, 
                              cores, browser, deviceSystemFamily, system, deviceLanguage,
                              block, block_condition,conditionName, targetTask, targetKind, 
                              resolution,error, warning)
    
  }
  max_trails <- as.numeric(all_files %>% count(participant) %>% summarize(max(n)))
  
  error <- all_files %>% 
    filter(error != "" & error != "Incomplete") %>%
    mutate(warning = "") %>% 
    mutate(ok = emoji("x"))
  
  warnings <- all_files %>% 
    filter(warning != "") %>%
    mutate(error = "") %>% 
    mutate(ok = emoji("large_orange_diamond"))
  
  noerror_fails = tibble()
  for (i in 1 : length(data_list)) {
    if (tail(data_list[[i]]$experimentCompleteBool, 1) == 'FALSE') {
      if (!data_list[[i]]$participant[1] %in% error$participant) {
        t <- data_list[[i]] %>% 
          distinct(ProlificParticipantID,participant, deviceType,
                   cores, system, browser, resolution) %>% 
          mutate(error = "Incomplete") %>% 
          select(error,ProlificParticipantID,participant, deviceType, 
                 cores, system, browser, resolution)
        info <- data_list[[i]] %>% 
          distinct(block, block_condition, conditionName, 
                   targetTask, targetKind) %>% 
          filter(block_condition != "", conditionName != "") 
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
        t$ok <- emoji("construction")
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
        filter(block_condition != "" & conditionName != "") %>% 
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
    mutate(ok = factor(ok, levels = c(emoji("x"), 
                                      emoji("construction"),
                                      emoji("large_orange_diamond"),
                                      emoji("white_check_mark")
                                      ))) %>% 
    rename("Prolific participant ID" = "ProlificParticipantID",
           "Pavlovia session ID" = "participant",
           "target kind" = "targetKind",
           "target task" = "targetTask",
           "condition name" ="conditionName",
           "device type" = "deviceType",
           "block condition" = "block_condition") %>% 
    select(`Prolific participant ID`, `Pavlovia session ID`, `device type`, system, browser, 
           resolution, cores, ok, error, warning, `block condition`, `condition name`, `target task`, `target kind`)
  summary_df <- summary_df %>%
    arrange(ok)
  summary_df[is.na(summary_df)] <- ""
  return(summary_df)
}