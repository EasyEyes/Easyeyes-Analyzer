source('./other/formSpree.R')

read_prolific <- function(fileProlific) {
  t <- read_csv(fileProlific$data, quote = "\"")
  t <- t %>% select(`Participant id`, `Submission id`, Status, `Completion code`,
                    `Time taken`, Age, Sex, Nationality) %>% 
    rename("ProlificSessionID" = "Submission id",
           "Prolific participant ID" = "Participant id",
           "ProlificStatus" = "Status",
           "Prolific (min)" = "Time taken")
  return(t)
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
    t <- summary_table %>% 
      full_join(tmp, by = c('Prolific participant ID', 'ProlificSessionID'))
    tmp <- prolificData %>%
      filter(!ProlificSessionID %in% unique(summary_table$ProlificSessionID))
    formSpree <- formSpree %>% 
      full_join(tmp, by = c('Prolific participant ID', 'ProlificSessionID'))
    t <- rbind(t, formSpree)
    
  }
  t <- t %>% distinct(`Prolific participant ID`, `Pavlovia session ID`, ProlificSessionID, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    `Prolific (min)`, ProlificStatus, `Completion code`, ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, Age, Sex, Nationality, comment, order)
  print(t)
  return(list(t, formSpree))
}