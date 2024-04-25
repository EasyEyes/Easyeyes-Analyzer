source('./other/formSpree.R')

read_prolific <- function(fileProlific) {
  t <- read.csv(fileProlific)
  if ('Participant.id' %in% names(t) & 'Submission.id' %in% names(t)) {
    t <- t %>% select(`Participant.id`, `Submission.id`, Status, `Completion.code`,
                      `Time.taken`, Age, Sex, Nationality) %>% 
      rename("ProlificSessionID" = "Submission.id",
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
  for (k in 1:n) {
    if (grepl(".zip", file_list[k])) {
      print('inside find_prolific_from_files')
      file_names <- unzip(file_list[k], list = TRUE)$Name
      all_csv <- file_names[grepl(".csv", file_names)]
      all_csv <- all_csv[!grepl("__MACOSX", all_csv)]
      prolificDT <- foreach(u=1:length(all_csv), .combine = 'rbind') %do% {
        prolific <- read_prolific(unzip(file_list[], all_csv[u]))
      }
      print(summary(prolificDT))
      return(prolificDT)
    }
  }
}

combineProlific <- function(prolificData, summary_table){
  if (is.null(prolificData)) {
    t <- summary_table %>% mutate(ProlificStatus= ' ',
                                  `Prolific (min)` = NA,
                                  `Completion code` = NA,
                                  Age = NA,
                                  Sex = NA,
                                  Nationality = NA)
    formSpree <- tibble()
  } else {
    formSpree <- getFormSpree() %>% filter(`ProlificSessionID` %in% unique(prolificData$ProlificSessionID),
                                            !`ProlificSessionID` %in% unique(summary_table$ProlificSessionID))
    tmp <- prolificData %>%
      filter(ProlificSessionID %in% unique(summary_table$ProlificSessionID))
    print
    t <- summary_table %>% 
      left_join(tmp, by = c('Prolific participant ID', 'ProlificSessionID'))
    tmp <- prolificData %>%
      filter(!ProlificSessionID %in% unique(summary_table$ProlificSessionID))
    formSpree <- formSpree %>% 
    left_join(tmp, by = c('Prolific participant ID', 'ProlificSessionID'))
    t <- rbind(t, formSpree)
    
  }
  t <- t %>% distinct(`Prolific participant ID`, `Pavlovia session ID`, ProlificSessionID, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    `Prolific (min)`, ProlificStatus, `Completion code`, ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, Age, Sex, Nationality, comment, order)
  return(list(t, formSpree))
}