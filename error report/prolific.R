source('./other/formSpree.R')

read_prolific <- function(fileProlific) {
  t <- tibble()
  try(t <- read.csv(fileProlific))
  if ('Participant.id' %in% names(t) & 'Submission.id' %in% names(t)) {
    t <- t %>% select(`Participant.id`, `Submission.id`, Status, `Completion.code`,
                      `Time.taken`, Age, Sex, Nationality) %>% 
      rename("prolificSessionID" = "Submission.id",
             "Prolific participant ID" = "Participant.id",
             "ProlificStatus" = "Status",
             "prolificMin" = "Time.taken",
             "Completion code" = "Completion.code") %>% 
      mutate(`prolificMin` =format(
        round(
          prolificMin/60,
          1
        ),
        nsmall = 1
      ),
      `Completion code` = as.character(`Completion code`))
    return(t)
    print(t)
  } else {
    return(tibble())
  }
 
}

find_prolific_from_files <- function(file) {
  file_list <- file$data
  n = length(file$data)
  prolificDT <- tibble()
  for (k in 1:length(file_list)) {
    if (grepl(".zip", file_list[k])) {
      file_names <- unzip(file_list[k], list = TRUE)$Name
      all_csv <- file_names[grepl(".csv", file_names)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
      prolificDT <- foreach(u=1:length(all_csv), .combine = 'rbind') %do% {
        prolific <- read_prolific(unzip(file_list[k], all_csv[u]))
      }

      return(prolificDT)
    }
  }
  print('done find prolific')
  return(prolificDT)
}

combineProlific <- function(prolificData, summary_table){
  if (is.null(prolificData) | nrow(prolificData) == 0) {
    t <- summary_table %>% mutate(ProlificStatus= ' ',
                                  prolificMin = NaN,
                                  `Completion code` = NA,
                                  Age = NA,
                                  Sex = NA,
                                  Nationality = NA)
    formSpree <- tibble()
  } else {
    formSpree <- getFormSpree() %>% filter(`prolificSessionID` %in% unique(prolificData$prolificSessionID),
                                            !`prolificSessionID` %in% unique(summary_table$prolificSessionID))
    tmp <- prolificData %>%
      filter(prolificSessionID %in% unique(summary_table$prolificSessionID))
    print
    t <- summary_table %>% 
      group_by(prolificSessionID) %>% 
      arrange(desc(date)) %>% 
      slice(1) %>% 
      left_join(tmp, by = c('Prolific participant ID', 'prolificSessionID'))
    t2 <-  summary_table %>%
      filter(!`Pavlovia session ID` %in% unique(t$`Pavlovia session ID`)) %>% 
      left_join(tmp, by = c('Prolific participant ID', 'prolificSessionID')) %>% 
      mutate(ProlificStatus= 'TRIED AGAIN',
             prolificMin = '',
             `Completion code` = 'TRIED AGAIN')
    tmp <- prolificData %>% 
      filter(!prolificSessionID %in% unique(summary_table$prolificSessionID))
    formSpree <- tmp %>% 
    full_join(formSpree, by = c('Prolific participant ID', 'prolificSessionID'))
    t <- rbind(t, t2, formSpree) 
    
  }
    t <- t %>%
    rename('Prolific session ID' = 'prolificSessionID',
           'Computer 51 deg' = 'computer51Deg') %>% 
    distinct(`Prolific participant ID`, `Prolific session ID`, `Pavlovia session ID`,
             `device type`, system,browser, resolution, QRConnect, date, prolificMin,
             ProlificStatus,`Completion code`, ok, unmetNeeds, error, warning, cores,
             tardyMs, excessMs, KB, rows, cols,`block condition`, trial, `condition name`,
             `target task`, `threshold parameter`, `target kind`, `Computer 51 deg`,
             Loudspeaker, Microphone, Age, Sex, Nationality, comment, order)
  return(list(t, formSpree))
}