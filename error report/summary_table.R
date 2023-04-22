generate_summary_table <- function(data_list){
  all_files <- foreach(i = 1 : length(data_list), .combine = "rbind") %do% {
    data_list[[i]] %>% select(ProlificParticipantID, participant, deviceType, 
                              cores, browser, deviceSystemFamily, deviceLanguage,
                              block, block_condition,conditionName, targetTask, targetKind, 
                              thresholdParameter, resolution,error, warning)
    
  }
  max_trails <- as.numeric(all_files %>% count(participant) %>% summarize(max(n)))
  
  
  #### errors ####
  error <- all_files %>% 
    filter(error != "" & error != "Incomplete") %>%
    mutate(warning = "") %>% 
    mutate(ok = emoji("x"))
  
  
  #### warnings ####
  warnings <- all_files %>% 
    filter(warning != "") %>%
    mutate(error = "") %>% 
    mutate(ok = emoji("large_orange_diamond"))
  
  
  #### incomplete files ####
  noerror_fails = tibble()
  for (i in 1 : length(data_list)) {
    if (tail(data_list[[i]]$experimentCompleteBool, 1) == 'FALSE') {
      if (!data_list[[i]]$participant[1] %in% error$participant) {
        t <- data_list[[i]] %>% 
          distinct(ProlificParticipantID,participant, deviceType,
                   cores, deviceSystemFamily, browser, resolution) %>% 
          mutate(error = "Incomplete") %>% 
          select(error,ProlificParticipantID,participant, deviceType, 
                 cores, deviceSystemFamily, browser, resolution)
        info <- data_list[[i]] %>% 
          distinct(block, block_condition, conditionName, 
                   targetTask, targetKind, thresholdParameter) %>% 
          filter(block_condition != "", conditionName != "") 
        if (nrow(info) > 0) {
          info <- info %>% tail(1)
        } else {
          info <- tibble(block = NA,
                         block_condition = NA, 
                         conditionName = NA, 
                         targetTask = NA, 
                         targetKind = NA,
                         thresholdParameter = NA)
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
                 cores, deviceSystemFamily, browser, resolution)
      info <- data_list[[i]] %>% 
        distinct(block, block_condition, conditionName, 
                 targetTask, targetKind, thresholdParameter) %>% 
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
                        cores, deviceSystemFamily, browser, resolution,
                        block, block_condition, conditionName, targetTask, targetKind, thresholdParameter, ok),
               warnings %>% 
                 select(error, warning, ProlificParticipantID, participant, deviceType, 
                        cores, deviceSystemFamily, browser, resolution,
                        block, block_condition, conditionName, targetTask, targetKind, thresholdParameter, ok)) %>% 
    mutate(ok = factor(ok, levels = c(emoji("x"), 
                                      emoji("construction"),
                                      emoji("large_orange_diamond"),
                                      emoji("white_check_mark")
                                      ))) %>% 
    rename("Prolific participant ID" = "ProlificParticipantID",
           "Pavlovia session ID" = "participant",
           "target kind" = "targetKind",
           "target task" = "targetTask",
           "threshold parameter" = "thresholdParameter",
           "condition name" ="conditionName",
           "device type" = "deviceType",
           "block condition" = "block_condition",
           "system" = "deviceSystemFamily") %>% 
    select(`Prolific participant ID`, `Pavlovia session ID`, `device type`, system, browser, 
           resolution, cores, ok, error, warning, `block condition`, `condition name`, `target task`, `threshold parameter`, `target kind`)
  #### order block_condition by splitting and order block and condition order ####
  summary_df <- summary_df %>% 
    mutate(block = as.numeric(unlist(lapply(summary_df$`block condition`, FUN = function(x){unlist(str_split(x, "[_]"))[1]}))),
           condition = as.numeric(unlist(lapply(summary_df$`block condition`, FUN = function(x){unlist(str_split(x, "[_]"))[2]}))))
  block_condition_order <- summary_df %>%
    distinct(block, condition) %>%
    arrange(block, condition) %>%
    mutate(order = row_number())
  summary_df <- summary_df %>%
    left_join(block_condition_order, by = c("block", "condition")) %>%
    select(-block, -condition)
  summary_df[is.na(summary_df)] <- ""
  return(summary_df)
}