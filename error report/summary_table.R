library(dplyr)
get_lateness_and_duration <- function(all_files){
  t <- all_files %>% 
    select(participant, date, targetMeasuredLatenessSec, targetMeasuredDurationSec, targetDurationSec) %>% 
    mutate(targetDurationSec = as.numeric(targetDurationSec)) %>% 
    group_by(participant, date) %>% 
    summarize(targetMeasuredLatenessMeanSec = mean(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
              targetMeasuredLatenessSDSec = sd(targetMeasuredLatenessSec, na.rm = TRUE) * 1000,
              targetMeasuredDurationMeanSec = mean(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
              targetMeasuredDurationSDSec = sd(targetMeasuredDurationSec - targetDurationSec, na.rm = TRUE) * 1000,
              .groups = "keep") %>% 
    mutate(tardyMs =
             paste0(round(targetMeasuredLatenessMeanSec, 2), "±",
                    round(targetMeasuredLatenessSDSec,2)),
           excessMs =
             paste0(round(targetMeasuredDurationMeanSec,2), "±",
                    round(targetMeasuredDurationSDSec,2))) %>% 
    select(-targetMeasuredLatenessMeanSec,
           -targetMeasuredLatenessSDSec,
           -targetMeasuredDurationMeanSec,
           -targetMeasuredDurationSDSec)
  t <- t %>% 
    mutate(date = parse_date_time(date, orders = c('ymdHMS', 'mdyHMS'))) %>% 
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
  return(t)
}

generate_summary_table <- function(data_list){
  all_files <- tibble()
 for (i in 1 : length(data_list)) {
    t <- data_list[[i]] %>% select(ProlificParticipantID, participant, deviceType, 
                              cores, browser, deviceSystemFamily, deviceLanguage,
                              block, block_condition,conditionName, targetTask, targetKind, 
                              thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
                              targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
                              ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`) %>% 
      distinct(ProlificParticipantID, participant, deviceType, 
               cores, browser, deviceSystemFamily, deviceLanguage,
               block, block_condition,conditionName, targetTask, targetKind, 
               thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
               targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
               ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`) %>% 
      arrange(`Loudspeaker survey`)
    tmp <- t$`Loudspeaker survey`[1]
    t <- t %>% mutate(`Loudspeaker survey` = ifelse(is.na(tmp), '',tmp))
    t <- t %>% arrange(`_needsUnmet`)
    tmp <- t$`_needsUnmet`[1]
    t <- t %>% mutate(`_needsUnmet` = ifelse(is.na(tmp), '',tmp))
    t <- t %>% arrange(`Microphone survey`)
    tmp <- t$`Microphone survey`[1]
    t <- t %>% mutate(`Microphone survey` = ifelse(is.na(tmp), '',tmp))
    t <- t %>%   
      distinct(ProlificParticipantID, participant, deviceType, 
       cores, browser, deviceSystemFamily, deviceLanguage,
       block, block_condition,conditionName, targetTask, targetKind, 
       thresholdParameter, resolution,error, warning, targetMeasuredLatenessSec,
       targetMeasuredDurationSec,date, targetDurationSec, rows, cols, kb, 
       ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`)
    all_files <- rbind(all_files,t)
  }
  trial <- all_files %>% group_by(participant, block_condition) %>% count()
  lateness_duration <- get_lateness_and_duration(all_files)
  
  #### errors ####
  error <- all_files %>% 
    dplyr::filter(error != "" & error != "Incomplete") %>%
    mutate(warning = "") %>% 
    mutate(ok = paste(emoji("x")))

  #### warnings ####
  warnings <- all_files %>% 
    dplyr::filter(warning != "") %>%
    mutate(error = "") %>% 
    mutate(ok = emoji("large_orange_diamond"))
  
  
  #### incomplete files ####
  noerror_fails = tibble()
  for (i in 1 : length(data_list)) {
    if (tail(data_list[[i]]$experimentCompleteBool, 1) == 'FALSE' | 
        is.na(tail(data_list[[i]]$experimentCompleteBool, 1))) {
      if (!data_list[[i]]$participant[1] %in% error$participant) {
        t <- data_list[[i]] %>% 
          distinct(ProlificParticipantID,participant, deviceType,
                   cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                   ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`) %>% 
          mutate(error = "Incomplete") %>% 
          select(error,ProlificParticipantID,participant, deviceType, 
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`) %>% 
          arrange(`Loudspeaker survey`)
        tmp <- t$`Loudspeaker survey`[1]
        t <- t %>% mutate(`Loudspeaker survey` = ifelse(is.na(tmp), '',tmp))
          t <- t %>% arrange(`_needsUnmet`)
        tmp <- t$`_needsUnmet`[1]
        t <- t %>% mutate(`_needsUnmet` = ifelse(is.na(tmp), '',tmp))
        t <- t %>% arrange(`Microphone survey`)
        tmp <- t$`Microphone survey`[1]
        t <- t %>% mutate(`Microphone survey` = ifelse(is.na(tmp), '',tmp))
        info <- data_list[[i]] %>% 
          distinct(block, block_condition, conditionName, 
                   targetTask, targetKind, thresholdParameter) %>% 
          dplyr::filter(block_condition != "", conditionName != "") 
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
        & !data_list[[i]]$participant[1] %in% noerror_fails$participant) {
      t <- data_list[[i]] %>% 
        distinct(ProlificParticipantID, participant, deviceType, error,
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`) %>% 
        arrange(`Loudspeaker survey`)
      t$`Loudspeaker survey` = t$`Loudspeaker survey`[1]
      t <- t %>% arrange( `_needsUnmet`)
      t$`_needsUnmet` = t$`_needsUnmet`[1]
      t <- t %>% arrange(`Microphone survey`)
      t$`Microphone survey` = t$`Microphone survey`[1]
      t <- t %>%   
        distinct(ProlificParticipantID, participant, deviceType, error,
                 cores, deviceSystemFamily, browser, resolution, rows, cols, kb, 
                 ComputerInfoFrom51Degrees, `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`)
      info <- data_list[[i]] %>% 
        distinct(block, block_condition, conditionName, 
                 targetTask, targetKind, thresholdParameter) %>% 
        dplyr::filter(block_condition != "" & conditionName != "") %>% 
        tail(1)
      if (nrow(t) == nrow(info)) {
        t <- cbind(t, info)
      } else {
        t <- t %>% mutate(block='', block_condition='', conditionName='', 
                          targetTask='', targetKind='', thresholdParameter='')
      }
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
                               block, block_condition, conditionName, targetTask, targetKind, 
                               thresholdParameter, ok, rows, cols, kb, ComputerInfoFrom51Degrees,
                               `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`),
                      warnings %>% 
                        select(error, warning, ProlificParticipantID, participant, deviceType, 
                               cores, deviceSystemFamily, browser, resolution,
                               block, block_condition, conditionName, targetTask, targetKind, 
                               thresholdParameter, ok, rows, cols, kb, ComputerInfoFrom51Degrees,
                               `_needsUnmet`,`Loudspeaker survey`,`Microphone survey`)) %>% 
    mutate(ok = factor(ok, levels = c(paste(emoji("x")), 
                                      emoji("construction"),
                                      emoji("large_orange_diamond"),
                                      emoji("white_check_mark")
    ))) %>% 
    left_join(lateness_duration, by = "participant") %>% 
    left_join(trial, by = c("participant", 'block_condition')) %>% 
    rename("Prolific participant ID" = "ProlificParticipantID",
           "Pavlovia session ID" = "participant",
           "target kind" = "targetKind",
           "target task" = "targetTask",
           "threshold parameter" = "thresholdParameter",
           "condition name" ="conditionName",
           "device type" = "deviceType",
           "block condition" = "block_condition",
           "system" = "deviceSystemFamily",
           "trial" = "n",
           'kB' = 'kb',
           "unmetNeeds" = '_needsUnmet',
           "computer51Deg" ="ComputerInfoFrom51Degrees",
           "Loudspeaker" = "Loudspeaker survey",
           "Microphone" = "Microphone survey") %>% 
    distinct(`Prolific participant ID`, `Pavlovia session ID`, `device type`, system,
           browser, resolution, computer51Deg, cores, tardyMs, excessMs, date, kB, rows, cols, 
           ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
           `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone)
  
  #### order block_condition by splitting and order block and condition order ####
  summary_df <- summary_df %>% 
    mutate(block = as.numeric(unlist(lapply(summary_df$`block condition`, 
                                            FUN = function(x){unlist(str_split(x, "[_]"))[1]}))),
           condition = as.numeric(unlist(lapply(summary_df$`block condition`, 
                                                FUN = function(x){unlist(str_split(x, "[_]"))[2]}))))
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