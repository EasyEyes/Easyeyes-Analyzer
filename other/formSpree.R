getFormSpree <- function(experiment){
  url <- "https://formspree.io/api/0/forms/mqkrdveg/submissions"
  response <- httr::GET(url, httr::authenticate("", "fd58929dc7864b6494f2643cd2113dc9"))
  if (httr::status_code(response)) {
    content <- httr::content(response, as = "text", encoding='UTF-8')
    t <- jsonlite::fromJSON(content)$submissions %>% 
      filter(ExperimentName == experiment) %>% 
      rename('system' = 'OS',
             "device type" = "deviceType",
            'Pavlovia session ID' = 'pavloviaID',
            "Prolific participant ID" = "prolificParticipantID",
            'ProlificSessionID' = 'prolificSession') %>% 
      mutate(date = parse_date_time(substr(`_date`,1,19), orders = c('ymdHMS'))) %>% 
      mutate(date = format(date, "%b %d, %Y, %H:%M:%S")) %>% 
      mutate(browser = ifelse(browser == "", "", paste0(browser,  " ", str_split(browserVersion, "[.]")[[1]][1])),
             resolution = NA, 
             QRConnect = NA, 
             computer51Deg = NA,
             cores = NA, 
             tardyMs = NA, 
             excessMs = NA, 
             KB = NA,
             rows = NA,
             cols = NA, 
             ok = NA,
             unmetNeeds = NA,
             error = NA,
             warning = NA,
             `block condition` = NA,
             trial = NA,
             `condition name` = NA,
             `target task` = NA, 
             `threshold parameter` = NA,
             `target kind` = NA,
             Loudspeaker = NA,
             Microphone = NA,
             QRConnect = NA) %>% 
      select(`Prolific participant ID`, `Pavlovia session ID`, `device type`, system,
               browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
               ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
               `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, QRConnect)
    return(t)
  } else {
    print(httr::status_code(response))
    return(tibble())
  }
}

