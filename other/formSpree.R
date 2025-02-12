url <- "https://formspree.io/api/0/forms/mqkrdveg/submissions?limit=3000"
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
             `Computer 51 deg` = '',
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
             `heapLimitAfterDrawing (MB)` = NaN,
             deviceMemoryGB = NaN,
             mustTrackSec = NaN,
             goodTrials = NaN,
             badTrials = NaN,
             WebGLVersion = '',
             maxTextureSize= NaN,
             maxViewportSize = NaN,
             WebGLUnmaskedRenderer = '',
             order = NaN) %>% 
      select(`Prolific participant ID`, `Prolific session ID`, `Pavlovia session ID`,
             `device type`, system, browser, resolution, QRConnect, date, ok, unmetNeeds, 
             error, warning, cores, tardyMs, excessMs, KB, rows, cols,`block condition`, 
             trial, `condition name`, `target task`, `threshold parameter`, `target kind`,
             `Computer 51 deg`, Loudspeaker, Microphone, comment,`heapLimitAfterDrawing (MB)`,
             deviceMemoryGB, mustTrackSec, goodTrials, badTrials, WebGLVersion, 
             maxTextureSize, maxViewportSize, WebGLUnmaskedRenderer, order)
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
    
    submissions <- jsonlite::fromJSON(content)$submissions
    if (!('pavloviaID' %in% colnames(submissions))) {
      submissions$pavloviaID <- NA
    }
    if (!('prolificParticipantID' %in% colnames(submissions))) {
      submissions$prolificParticipantID <- NA
    }
    if (!('prolificSession' %in% colnames(submissions))) {
      submissions$prolificSession <- NA
    }
    if (!('ExperimentName' %in% colnames(submissions))) {
      submissions$ExperimentName <- NA
    }
    if (!('OS' %in% colnames(submissions))) {
      submissions$OS <- NA
    }
    if (!('browser' %in% colnames(submissions))) {
      submissions$browser <- NA
    }
    if (!('browserVersion' %in% colnames(submissions))) {
      submissions$browserVersion <- NA
    }
    if (!('deviceType' %in% colnames(submissions))) {
      submissions$deviceType <- NA
    }

    initial <- submissions %>% 
      filter(!is.na(OS)) %>% 
      select(pavloviaID, prolificParticipantID, prolificSession, ExperimentName,
             OS, browser, browserVersion, deviceType)
    
    t <- submissions %>% 
      mutate(date = parse_date_time(substr(`_date`,1,19), orders = c('ymdHMS')),
             pavloviaID = ifelse(is.na(pavloviaID), pavloviaId, pavloviaID)) %>% 
      group_by(pavloviaID) %>% 
      # slice(which.max(date)) %>% 
      ungroup() %>% 
      mutate(date = format(date, "%b %d, %Y, %H:%M:%S")) %>% 
      arrange(desc(`_date`)) %>% 
      select(-`_date`)
    
    t <- t %>% select(-c(prolificParticipantID, prolificSession, ExperimentName,
                          OS, browser, browserVersion, deviceType, timestamp))
    t <- initial %>% full_join(t, by = "pavloviaID") %>% 
      mutate(hl = is.na(fontLatencySec))
    
    t$OS = stringr::str_replace_all(t$OS, "OS X","macOS")
    if (listFontParameters) {
      t <- t %>% select(pavloviaID, date, font, fontMaxPx, fontRenderMaxPx, fontString,
                        block, conditionName, trial, fontLatencySec, hl)
        
    }
    return(t)
  }
  return(tibble())
}

get_font_parameters_from_formSpree <- function(participant) {
  print('inside get_font_parameters_from_formSpree')
  # we might need to add _logFontBool so that those doesn't have fontSpree log, no need to pull font parameters
  response <- try(httr::GET(url, httr::authenticate("", "fd58929dc7864b6494f2643cd2113dc9")))
  if (httr::status_code(response)) {
    content <- httr::content(response, as = "text", encoding='UTF-8')
    
    try(t <- jsonlite::fromJSON(content))
    if(!is.null(t))
    t <- t$submissions %>% 
      mutate(`Pavlovia session ID` = ifelse(is.na(pavloviaID), pavloviaId, pavloviaID)) %>% 
      filter(`Pavlovia session ID` %in% participant) %>% 
      filter(!is.na(fixationXYPx)) %>% 
      select(`Pavlovia session ID`, fontSizePx, fixationXYPx, fontMaxPx, viewingDistanceCm, fontRenderMaxPx, timestamp) %>% 
      arrange(desc(timestamp)) %>% 
      group_by(`Pavlovia session ID`) %>% 
      slice(1) %>% 
      select(-timestamp)
  } else {
    return(t)
  }
  return(t)
}