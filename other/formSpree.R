url <- "https://formspree.io/api/0/forms/mqkrdveg/submissions"
getFormSpree <- function(){
  response <- httr::GET(url, httr::authenticate("", "fd58929dc7864b6494f2643cd2113dc9"))
  if (httr::status_code(response)) {
    content <- httr::content(response, as = "text", encoding='UTF-8')
    t <- jsonlite::fromJSON(content)$submissions %>% 
      filter(prolificParticipantID != '',
             prolificSession != '') %>% 
      rename('system' = 'OS',
             "device type" = "deviceType",
            'Pavlovia session ID' = 'pavloviaID',
            "Prolific participant ID" = "prolificParticipantID",
            'prolificSessionID' = 'prolificSession') %>% 
      mutate(date = parse_date_time(substr(`_date`,1,19), orders = c('ymdHMS'))) %>% 
      mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
      func = function(x) {str_split(x,'[.]')[[1]][1]}
      t$browserVersion <- unlist(lapply(t$browserVersion, FUN=func))
      t$system = str_replace_all(t$system, "OS X","macOS")
      t <- t %>% 
      mutate(browser = ifelse(browser == "", "", paste0(browser,  " ", browserVersion)),
             resolution = '', 
             QRConnect = '', 
             computer51Deg = '',
             cores = NaN, 
             tardyMs = '', 
             excessMs = '', 
             KB = NaN,
             rows = NaN,
             cols = NaN, 
             ok = '',
             unmetNeeds = '',
             error = '',
             warning = '',
             `block condition` = '',
             trial = NaN,
             `condition name` = '',
             `target task` = '', 
             `threshold parameter` = '',
             `target kind` = '',
             Loudspeaker = '',
             Microphone = '',
             QRConnect = '',
             comment = '',
             order = NaN) %>% 
      select(`Prolific participant ID`, `Pavlovia session ID`, prolificSessionID, `device type`, system,
               browser, resolution, QRConnect, computer51Deg, cores, tardyMs, excessMs, date, KB, rows, cols, 
               ok, unmetNeeds, error, warning, `block condition`, trial, `condition name`,
               `target task`, `threshold parameter`, `target kind`, Loudspeaker, Microphone, QRConnect, comment, order)
      t$ok = as.factor(t$ok)
    return(t)
  } else {
    print(httr::status_code(response))
    return(tibble())
  }
}

monitorFormSpree <- function(listFontParameters) {
  response <- httr::GET(url, httr::authenticate("", "fd58929dc7864b6494f2643cd2113dc9"))
  if (httr::status_code(response)) {
    content <- httr::content(response, as = "text", encoding='UTF-8')
    
    initial <- jsonlite::fromJSON(content)$submissions %>% 
      filter(!is.na(OS)) %>% 
      select(pavloviaID, prolificParticipantID, prolificSession, ExperimentName,
             OS, browser, browserVersion, deviceType)
    
    t <- jsonlite::fromJSON(content)$submissions %>% 
      mutate(date = parse_date_time(substr(`_date`,1,19), orders = c('ymdHMS')),
             pavloviaID = ifelse(is.na(pavloviaID), pavloviaId, pavloviaID)) %>% 
      group_by(pavloviaID) %>% 
      slice(which.max(date)) %>% 
      ungroup() %>% 
      mutate(date = format(date, "%b %d, %Y, %H:%M:%S")) %>% 
      arrange(desc(`_date`)) %>% 
      select(-`_date`)
    
    t <- t %>%
      select(pavloviaID, date, font, fontPt, fontMaxPx,
             fontRenderMaxPx, fontString, block, conditionName, trial, fontLatencySec) %>% 
      left_join(initial, by = "pavloviaID") %>% 
      mutate(hl = is.na(fontLatencySec)) %>% 
      select(pavloviaID, prolificParticipantID, prolificSession, ExperimentName,
             date, OS, browser, browserVersion, deviceType, font, fontPt, fontMaxPx, fontRenderMaxPx, fontString,
             block, conditionName, trial, fontLatencySec, hl)
    
    t$OS = stringr::str_replace_all(t$OS, "OS X","macOS")
    if (listFontParameters) {
      t <- t %>% select(pavloviaID, date, font, fontPt, fontMaxPx, fontRenderMaxPx, fontString,
                        block, conditionName, trial, fontLatencySec, hl)
        
    }
    return(t)
  }
  return(tibble())
}

