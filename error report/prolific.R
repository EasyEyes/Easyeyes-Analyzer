source('./other/formSpree.R')



find_prolific_from_files <- function(file) {
  file_list <- file$data
  file_names <- file$name
  prolificDT <- tibble()
  
  for (i in 1: length(file_list)) {
    # Handle ZIP files
    if (grepl("\\.zip$", file_list[[i]], ignore.case = TRUE)) {
      zip_file_names <- unzip(file_list[[i]], list = TRUE)$Name
      
      # Only keep files that end with prolific.csv and are not __MACOSX
      prolific_csvs <- zip_file_names[grepl("prolific\\.csv$", zip_file_names, ignore.case = TRUE)]
      prolific_csvs <- prolific_csvs[!grepl("__MACOSX", prolific_csvs)]
      
      # Extract and read each prolific.csv from the ZIP
      if (length(prolific_csvs) > 0) {
        temp_dir <- tempdir()
        unzip(f, files = prolific_csvs, exdir = temp_dir, overwrite = TRUE)
        
        for (csv_file in prolific_csvs) {
          full_path <- file.path(temp_dir, csv_file)
          temp_data <- read_prolific(full_path)
          if (nrow(temp_data) > 0) {
            prolificDT <- bind_rows(prolificDT, temp_data)
          }
        }
      }
    }
    
    # Handle standalone prolific.csv files
    if (grepl("prolific\\.csv$", file_names[[i]], ignore.case = TRUE)) {
      temp_data <- read_prolific( file_list[[i]])
      if (nrow(temp_data) > 0) {
        prolificDT <- bind_rows(prolificDT, temp_data)
      }
    }
  }
  
  print('done find prolific')
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
      rename("ProlificSessionID" = "Submission.id",
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
    # Get most recent logs from fromSpree 
    formSpree <- getFormSpree() 
    if (is.null(formSpree)) {
      formSpree <- tibble()
    } else {
      formSpree <- formSpree %>% filter(`ProlificSessionID` %in% unique(prolificData$prolificSessionID),
                                        !`ProlificSessionID` %in% unique(summary_table$prolificSessionID))
    }

    t <- summary_table %>% 
      left_join(prolificData, by = c('Prolific participant ID', 'ProlificSessionID')) %>% 
      mutate(`Completion code` = ifelse(`Completion code` == "" & `ProlificSessionID` %in% unique(prolificData$prolificSessionID), 'TRIED AGAIN', `Completion code`))
  
  }
  
  t <- t %>%
    rename('Prolific session ID' = 'ProlificSessionID',
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
      select(-c(fontSizePx, fontMaxPx, viewingDistanceCm, fontRenderMaxPx)) %>% 
      left_join(fontParameters, by = 'Pavlovia session ID')
  } else {
    t <- t %>%
      mutate(fixationXYPx = '')
  } 
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
             `device type`, system, browser, resolution, screenWidthCm, `Phone QR connect`, date, `Prolific min`,
             `Prolific status`,`Completion code`, ok, unmetNeeds, error, warning, cores, GB,
             `Lateness ms`, `Duration ms`, KB, rows, cols,block,condition, trial, `condition name`,
             `target task`, `threshold parameter`, `target kind`, `Computer 51 deg`,
             Loudspeaker, Microphone, Age, Sex, Nationality, comment, fontSizePx, fixationXYPx,
             fontMaxPx, viewingDistanceCm, fontRenderMaxPx, heapLimitAfterDrawing, heapTotalAvgMB,
             mustTrackSec, goodTrials, badTrials, WebGLVersion, 
             maxTextureSize, maxViewportSize, WebGLUnmaskedRenderer, order) %>% 
    filter(date != '', !is.na(date))
  print('done combine prolific')
  return(list(t, formSpree))
}


get_prolific_file_counts <- function(prolificData, summary_table) {
  print("DEBUG: Prolific Data Loaded")
  
  prolific_count <- if (!is.null(prolificData) && nrow(prolificData) > 0) {
    nrow(prolificData)
  } else {
    0
  }
  
  formSpree <- getFormSpree()
  formSpree_count <- 0
  if (!is.null(formSpree) && nrow(formSpree) > 0) {
    formSpree <- formSpree %>%
      filter(`ProlificSessionID` %in% unique(prolificData$prolificSessionID),
             !`ProlificSessionID` %in% unique(summary_table$ProlificSessionID))
    formSpree_count <- nrow(formSpree)
  }

  print(paste("DEBUG: Prolific Count:", prolific_count))
  print(paste("DEBUG: FormSpree Count:", formSpree_count))
  
  return(list(prolific_count = prolific_count, formSpree_count = formSpree_count))
}


