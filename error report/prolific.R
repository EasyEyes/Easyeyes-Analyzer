source('./other/formSpree.R')

read_prolific <- function(fileProlific) {
  t <- read_csv(fileProlific$data)
  t <- t %>% select(`Participant id`, `Submission id`, Status) %>% 
    rename("ProlificSessionID" = "Submission id",
           "Prolific participant ID" = "Participant id",
           "ProlificStatus" = "Status")
  return(t)
}


combineProlific <- function(prolificData, summary_table){
  if (is.null(prolificData)) {
    t <- summary_table %>% mutate( ProlificStatus= ' ')
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
  t <- t %>% distinct(`Prolific participant ID`, `Pavlovia session ID`, ProlificSessionID, ProlificStatus, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, comment, order)
  return(list(t, formSpree))
}