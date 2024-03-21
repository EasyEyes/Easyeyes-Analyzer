read_prolific <- function(fileProlific) {
  t <- read_csv(fileProlific$data)
  t <- t %>% select(`Participant id`, Status) %>% 
    rename("ProlificSessionID" = "Participant id",
           "ProlificStatus" = "Status")
  return(t)
}


combineProlific <- function(prolificData, summary_table){
  if (is.null(prolificData)) {
    t <- summary_table %>% mutate(ProlificSessionID = '', ProlificStatus= '')
  } else {
    summary_table <- summary_table %>% mutate(`ProlificSessionID` = `Prolific participant ID`) 
    t <- prolificData %>% full_join(summary_table, by = 'ProlificSessionID')
  }
  # t$cores[is.na(t$cores)] <- 0
  # t$trial[is.na(t$trial)] <- 0
  # t$KB[is.na(t$KB)] <- 0
  # t$rows[is.na(t$rows)] <- 
  # t$cols[is.na(t$cols)] <- 0
  # t$order[is.na(t$order)] <- 0
  # t$ok[is.na(t$ok)] <-  'RETRUNED'
  # t[is.na(t)] <- ''
  t <- t %>% select(`Prolific participant ID`, `Pavlovia session ID`, ProlificSessionID, ProlificStatus, `device type`, system,
                    browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
                    ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
                    `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, order)
  return(t)
}