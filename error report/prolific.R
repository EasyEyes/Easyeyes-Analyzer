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
  } else {
    t <- prolificData %>% full_join(summary_table, by = c('Prolific participant ID', 'ProlificSessionID'))
  }
  t <- t %>% distinct(`Prolific participant ID`, `Pavlovia session ID`, ProlificSessionID, ProlificStatus, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, order)
  return(t)
}