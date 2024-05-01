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
             "Prolific (min)" = "Time.taken",
             "Completion code" = "Completion.code") %>% 
      mutate(`Prolific (min)` = `Prolific (min)` / 60)
    return(t)
  } else {
    return(tibble())
  }
 
}

find_prolific_from_files <- function(file) {
  file_list <- file$data
  n = length(file$data)
  prolificDT <- tibble()
  for (k in 1:length(file_list)) {
    print('inside find prolific')
    if (grepl(".zip", file_list[k])) {
      file_names <- unzip(file_list[k], list = TRUE)$Name
      all_csv <- file_names[grepl(".csv", file_names)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
      prolificDT <- foreach(u=1:length(all_csv), .combine = 'rbind') %do% {
        prolific <- read_prolific(unzip(file_list[k], all_csv[u]))
      }

      return(prolificDT)
    }
    print('done find prolific')
  }
  return(prolificDT)
}

combineProlific <- function(prolificData, summary_table){
  if (is.null(prolificData) | nrow(prolificData) == 0) {
    t <- summary_table %>% mutate(ProlificStatus= ' ',
                                  `Prolific (min)` = NA,
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
      left_join(tmp, by = c('Prolific participant ID', 'prolificSessionID'))
    tmp <- prolificData %>%
      filter(!prolificSessionID %in% unique(summary_table$prolificSessionID))
    formSpree <- formSpree %>% 
    left_join(tmp, by = c('Prolific participant ID', 'prolificSessionID'))
    t <- rbind(t, formSpree)
    
  }
  t <- t %>% distinct(`Prolific participant ID`, `Pavlovia session ID`, prolificSessionID, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    `Prolific (min)`, ProlificStatus, `Completion code`, ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, Age, Sex, Nationality, comment, order)
  return(list(t, formSpree))
}