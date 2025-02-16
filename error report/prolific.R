source('./other/formSpree.R')

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
    }
    if (grepl('prolific', file$name[k])) {
      prolificDT <- read_prolific(file_list[k])
    }
  }
  return(prolificDT)
}

read_prolific <- function(fileProlific) {
  t <- tibble()
  try(t <- read.csv(fileProlific))
  if ('Participant.id' %in% names(t) & 'Submission.id' %in% names(t)) {
    t <- t %>% select(`Participant.id`, `Submission.id`, Status, `Completion.code`,
                      `Time.taken`, Age, Sex, Nationality) %>% 
      mutate(Sex = case_when(Sex == 'Female' ~ 'F',
                             Sex == 'Male' ~ 'M',
                             Sex == 'CONSENT_REVOKED' ~ '',
                             .default = ''),
             Age = ifelse(Age == 'CONSENT_REVOKED', '', Age),
             Nationality = ifelse(Nationality == 'CONSENT_REVOKED', '', Nationality)) %>% 
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
  } else {
    return(tibble())
  }
  
}

combineProlific <- function(prolificData, summary_table, pretest){
  print('inside combineProlific')
  if (is.null(prolificData) | nrow(prolificData) == 0) {
    t <- summary_table %>% mutate(ProlificStatus= ' ',
                                  prolificMin = NaN,
                                  `Completion code` = NA,
                                  Age = NA,
                                  Sex = NA,
                                  Nationality = NA)
    formSpree <- tibble()
  } else {
    # formSpree temporarily problematic
    # formSpree <- getFormSpree() %>% filter(`prolificSessionID` %in% unique(prolificData$prolificSessionID),
    #                                         !`prolificSessionID` %in% unique(summary_table$prolificSessionID))
    formSpree <- tibble()
    tmp <- prolificData %>%
      filter(prolificSessionID %in% unique(summary_table$prolificSessionID))
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
    # tmp <- prolificData %>% 
    #   filter(!prolificSessionID %in% unique(summary_table$prolificSessionID))
    # formSpree <- tmp %>% 
    # full_join(formSpree, by = c('Prolific participant ID', 'prolificSessionID'))

    # print('formSpree')
    # print(summary(formSpree))
    # t <- rbind(t, t2, formSpree) 
    t <- rbind(t, t2) 
  }
  
  t <- t %>%
    rename('Prolific session ID' = 'prolificSessionID',
           'Computer 51 deg' = 'computer51Deg',
           'Phone QR connect'='QRConnect',
           'Prolific min' = 'prolificMin',
           'Prolific status' = 'ProlificStatus',
           'heapLimitAfterDrawing' = 'heapLimitAfterDrawing (MB)',
           'Lateness ms' = 'tardyMs',
           'Duration ms' = 'excessMs')
  
  if (TRUE %in% summary_table$`_logFontBool`) {
    fontParameters <- get_font_parameters_from_formSpree(summary_table$`Pavlovia session ID`)
    t <- t %>%
      left_join(fontParameters, by = 'Pavlovia session ID')
  } else {
    t <- t %>%
      mutate(fontSizePx = '',
             fixationXYPx = '',
             fontMaxPx = '',
             viewingDistanceCm = '',
             fontRenderMaxPx = '')
  } 
  print(pretest)
  if( nrow(pretest) == 0){
    pretest <- tibble(participant = t$`Pavlovia session ID`,
                      `Participant ID` = '')
  }
  t <- t %>%
    left_join(pretest %>%
                rename('Pavlovia session ID' = 'participant') %>%
                select(`Pavlovia session ID`, `Participant ID`),
              by = 'Pavlovia session ID') %>% 
    distinct(`Participant ID`,`Prolific participant ID`, `Prolific session ID`, `Pavlovia session ID`,
             `device type`, system,browser, resolution, `Phone QR connect`, date, `Prolific min`,
             `Prolific status`,`Completion code`, ok, unmetNeeds, error, warning, cores,
             `Lateness ms`, `Duration ms`, KB, rows, cols,block,condition, trial, `condition name`,
             `target task`, `threshold parameter`, `target kind`, `Computer 51 deg`,
             Loudspeaker, Microphone, Age, Sex, Nationality, comment, fontSizePx, fixationXYPx,
             fontMaxPx, viewingDistanceCm, fontRenderMaxPx, heapLimitAfterDrawing, heapTotalAvgMB,
             deviceMemoryGB, mustTrackSec, goodTrials, badTrials, WebGLVersion, 
             maxTextureSize, maxViewportSize, WebGLUnmaskedRenderer, order) %>% 
    filter(date != '', !is.na(date))
  print('done combine prolific')
  return(list(t, formSpree))
}