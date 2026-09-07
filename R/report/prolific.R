source("R/utils/formSpree.R")

# =============================================================================
# Prolific ↔ Sessions summary mapping (overview)
# =============================================================================
# Upstream:
#   - Experiment CSVs carry ProlificParticipantID / ProlificSessionID (from the
#     file or Pavlovia columns). summary_table.R renames them to
#     "Prolific participant ID" and keeps ProlificSessionID until combineProlific
#     renames it to "Prolific session ID".
#   - prolific.csv (standalone or *.prolific.csv inside a results ZIP) is read
#     into `prolificData` via read_prolific() / read_prolific_from_zip() during
#     preprocess (append_prolific_rows).
#
# Join keys (one Prolific submission → many Pavlovia retries is common):
#   "Prolific participant ID"  ↔  Participant.id
#   ProlificSessionID          ↔  Submission.id
#
# Intended display rule (boss / product intent):
#   For a given (Prolific participant ID, ProlificSessionID), only the *last*
#   summary row (latest `date`) should show the real Prolific Status from
#   prolific.csv. Earlier retries should show Prolific status "Tried again"
#   (and Completion code "Tried again" when blank).
#
# What combineProlific does:
#   1. Pick the single max-`date` row per (Prolific participant ID,
#      ProlificSessionID), join prolific.csv fields onto that key triple
#      including `date`.
#   2. left_join back to all summary rows on
#      (Prolific participant ID, ProlificSessionID, date).
#      → Only rows whose `date` equals that max date receive ProlificStatus /
#        Completion code / Age / Sex / etc. Other dates stay NA/blank for
#        those columns.
#   3. Then, for any row still missing Completion code whose ProlificSessionID
#      appears in prolific.csv, set both Completion code and ProlificStatus to
#      "Tried again" (Shiny label, mixed case — not from Prolific).
#   4. FormSpree is fetched and filtered below but is not joined into `t` here;
#      it does not affect Sessions Prolific status.
#
# Note: If several summary rows share the same max date, step 2 still attaches
#   the real Prolific status to all of them. Reliable dates (including UTC+5:30)
#   are required so retries are distinguishable.
# =============================================================================


# Read one prolific.csv entry from a ZIP via unzip -p (no full archive extract).
read_prolific_from_zip <- function(zip_path, entry_name) {
  tmpf <- tempfile(fileext = ".csv")
  on.exit(unlink(tmpf), add = TRUE)
  status <- system2("unzip", c("-p", zip_path, entry_name), stdout = tmpf, stderr = FALSE)
  if (!identical(status, 0L) || !file.exists(tmpf) || file.info(tmpf)$size == 0) {
    return(tibble())
  }
  read_prolific(tmpf)
}

# Bind new prolific rows into the accumulator; no-op when chunk is empty.
append_prolific_rows <- function(prolificDT, chunk) {
  if (nrow(chunk) > 0) {
    dplyr::bind_rows(prolificDT, chunk)
  } else {
    prolificDT
  }
}

# Discover prolific.csv / *.prolific.csv from uploaded files or ZIPs.
# (Primary ingest path in the app is preprocess::read_files + append_prolific_rows;
#  this helper remains for standalone / legacy callers.)
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
        unzip(file_list[[i]], files = prolific_csvs, exdir = temp_dir, overwrite = TRUE)
        
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
      temp_data <- read_prolific(file_list[[i]])
      if (nrow(temp_data) > 0) {
        prolificDT <- bind_rows(prolificDT, temp_data)
      }
    }
  }
  
  return(prolificDT)
}

# Parse one Prolific export CSV into the columns used by combineProlific.
#
# prolific.csv (read.csv → spaces become dots)          →  internal name
# -------------------------------------------------------------------------
# Participant.id                                        →  Prolific participant ID
# Submission.id                                         →  ProlificSessionID
# Status                                                →  ProlificStatus
# Completion.code                                       →  Completion code
# Time.taken (seconds)                                  →  prolificMin (minutes, char)
# Age / Sex / Nationality                               →  Age / Sex / Nationality
#   (Sex shortened F/M; CONSENT_REVOKED cleared)
#
# Returns empty tibble if required id columns are missing.
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
          as.numeric(prolificMin)/60,
          1
        ),
        nsmall = 1)
      ) %>%
      # Force all columns to character to ensure compatibility with bind_rows
      mutate(across(everything(), as.character))
    return(t)
  } else {
    return(tibble())
  }
  
}

# Attach Prolific (and optional pretest / font) fields onto the Sessions summary.
#
# Inputs:
#   prolificData  – rows from read_prolific(); may be empty.
#   summary_table – per session/condition rows from generate_summary_table(),
#                   already renamed to "Prolific participant ID" and still using
#                   ProlificSessionID + formatted character `date`.
#   pretest       – optional pretest Participant ID by Pavlovia session.
#
# prolificData columns joined onto the "latest date" summary key:
#   ProlificStatus, Completion code, prolificMin, Age, Sex, Nationality
# (then renamed for the UI: Prolific status, Prolific min, Prolific session ID, …).
#
# See file-header comment for join semantics and known inconsistency.
combineProlific <- function(prolificData, summary_table, pretest){

  if (is.null(prolificData) | nrow(prolificData) == 0) {
    # No prolific.csv: leave placeholder Prolific columns; nothing to join.
    t <- summary_table %>% mutate(ProlificStatus= ' ',
                                  prolificMin = NaN,
                                  `Completion code` = NA,
                                  Age = NA,
                                  Sex = NA,
                                  Nationality = NA)
    formSpree <- tibble()
  } else {
    # Optional FormSpree pull (counts / legacy path). Filtered here but not
    # merged into `t` below — Sessions Prolific status comes only from prolific.csv.
    formSpree <- getFormSpree()
    if (
      !is.null(formSpree) &&
        nrow(formSpree) > 0 &&
        "ProlificSessionID" %in% names(formSpree)
    ) {
      formSpree <- formSpree %>%
        filter(
          ProlificSessionID %in% unique(prolificData$ProlificSessionID),
          !ProlificSessionID %in% unique(summary_table$ProlificSessionID)
        )
    } else {
      formSpree <- tibble()
    }

    # --- Map prolific.csv → summary rows (current behavior) ----------------
    # Step A: one "latest" key per (Prolific participant ID, ProlificSessionID)
    #         = row with maximum `date` string; attach prolific.csv fields.
    latest_per_participant <- summary_table %>%
      group_by(`Prolific participant ID`, ProlificSessionID) %>%
      slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
        select(`Prolific participant ID`, ProlificSessionID, date) %>%
      left_join(prolificData, by = c("Prolific participant ID","ProlificSessionID")) %>% 
      mutate(`Prolific participant ID` = as.character(`Prolific participant ID`),
             date = as.character(date),
             ProlificSessionID = as.character(ProlificSessionID))
    
    # Step B: re-join to *all* summary rows on participant + session + date.
    #         Only rows matching that latest date get ProlificStatus / etc.
    # Step C: blank Completion code on any row whose ProlificSessionID is in
    #         prolific.csv → set Completion code and ProlificStatus to
    #         "Tried again" (Shiny label for non-final / unmatched retries).
    t <- summary_table %>%
      left_join(latest_per_participant, by = c("Prolific participant ID","ProlificSessionID", "date")) %>% 
      mutate(
        .tried_again = (is.na(`Completion code`) | `Completion code` == "") &
          ProlificSessionID %in% unique(prolificData$ProlificSessionID),
        `Completion code` = ifelse(.tried_again, "Tried again", `Completion code`),
        ProlificStatus = ifelse(.tried_again, "Tried again", ProlificStatus)
      ) %>%
      select(-.tried_again)
  }
  
  # Display names for the Sessions table / downloads.
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
              by = 'Pavlovia session ID',
              relationship = 'many-to-many') %>% 
    distinct(`Participant ID`,`Prolific participant ID`, `Prolific session ID`, `Pavlovia session ID`,
             `device type`, system, browser, resolution, screenWidthCm, cameraIsTopCenter, `Phone QR connect`, date, `Prolific min`,
             `Prolific status`,`Completion code`, ok, unmetNeeds, error, warning, cores, GB,
             `Lateness ms`, `Duration ms`, KB, rows, cols, block,condition, trial, `condition name`,
             `target task`, `threshold parameter`, `target kind`, `Computer 51 deg`,
             Loudspeaker, Microphone, Age, Sex, Nationality, comment, fontSizePx, fixationXYPx,
             fontMaxPx, viewingDistanceCm, fontRenderMaxPx, heapLimitAfterDrawing, heapTotalAvgMB,
             mustTrackSec, goodTrials, badTrials, WebGLVersion, 
             maxTextureSize, maxViewportSize, WebGLUnmaskedRenderer, snapshotsLink)
  return(t)
}

# UI file-status counts: prolific.csv row count + FormSpree rows for submission
# IDs in prolific.csv that are not already present in the summary table.
get_prolific_file_counts <- function(prolificData, summary_table) {
  prolific_count <- if (!is.null(prolificData) && nrow(prolificData) > 0) {
    nrow(prolificData)
  } else {
    0
  }
  
  formSpree <- getFormSpree()
  formSpree_count <- 0
  if (
    !is.null(formSpree) &&
      nrow(formSpree) > 0 &&
      "ProlificSessionID" %in% names(formSpree) &&
      "ProlificSessionID" %in% names(summary_table)
  ) {
    formSpree <- formSpree %>%
      filter(
        ProlificSessionID %in% unique(prolificData$ProlificSessionID),
        !ProlificSessionID %in% unique(summary_table$ProlificSessionID)
      )
    formSpree_count <- nrow(formSpree)
  }
  
  log_info("Prolific count: ", prolific_count)
  log_info("FormSpree count: ", formSpree_count)
  
  return(list(prolific_count = prolific_count, formSpree_count = formSpree_count))
}
