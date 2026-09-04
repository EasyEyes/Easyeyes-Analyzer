url <- "https://formspree.io/api/0/forms/mqkrdveg/submissions?limit=3000"

# Safe FormSpree GET: returns the httr response on HTTP 200, otherwise NULL.
# Timeouts and other network errors must not abort the Shiny session.
formspree_get <- function() {
  response <- tryCatch(
    httr::GET(
      url,
      httr::authenticate("", "fd58929dc7864b6494f2643cd2113dc9"),
      httr::timeout(10)
    ),
    error = function(e) {
      log_warn("FormSpree request failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(response)) {
    return(NULL)
  }
  code <- tryCatch(httr::status_code(response), error = function(e) NA_integer_)
  if (!identical(as.integer(code), 200L)) {
    log_warn("FormSpree non-200 response: ", code)
    return(NULL)
  }
  response
}

getFormSpree <- function() {
  response <- formspree_get()
  if (is.null(response)) {
    return(tibble())
  }

  tryCatch(
    {
      content <- httr::content(response, as = "text", encoding = "UTF-8")
      t <- jsonlite::fromJSON(content)$submissions
      if (is.null(t) || !is.data.frame(t) || !"prolificParticipantID" %in% names(t)) {
        return(tibble())
      }

      # API field name has varied; normalize before rename.
      if (!"prolificSessionID" %in% names(t) && "prolificSession" %in% names(t)) {
        t$prolificSessionID <- t$prolificSession
      }
      if (!"prolificSessionID" %in% names(t)) {
        t$prolificSessionID <- NA_character_
      }
      if (!"pavloviaID" %in% names(t) && "pavloviaId" %in% names(t)) {
        t$pavloviaID <- t$pavloviaId
      }
      if (!"pavloviaID" %in% names(t)) {
        t$pavloviaID <- NA_character_
      }
      if (!"OS" %in% names(t)) t$OS <- NA_character_
      if (!"deviceType" %in% names(t)) t$deviceType <- NA_character_
      if (!"browser" %in% names(t)) t$browser <- ""
      if (!"browserVersion" %in% names(t)) t$browserVersion <- ""

      t <- t %>%
        filter(prolificParticipantID != "") %>%
        rename(
          "system" = "OS",
          "device type" = "deviceType",
          "Pavlovia session ID" = "pavloviaID",
          "Prolific participant ID" = "prolificParticipantID",
          "ProlificSessionID" = "prolificSessionID"
        ) %>%
        mutate(date = parse_date_time(substr(`_date`, 1, 19), orders = c("ymdHMS"))) %>%
        mutate(date = format(date, "%b %d, %Y, %H:%M:%S"))
      func <- function(x) {
        str_split(x, "[.]")[[1]][1]
      }
      t$browserVersion <- unlist(lapply(t$browserVersion, FUN = func))
      t$system <- str_replace_all(as.character(t$system), "OS X", "macOS")
      t <- t %>%
        mutate(
          browser = ifelse(browser == "" | is.na(browser), "", paste0(browser, " ", browserVersion)),
          resolution = "",
          QRConnect = "",
          `Computer 51 deg` = "",
          cores = NaN,
          tardyMs = "",
          excessMs = "",
          KB = NaN,
          rows = NaN,
          cols = NaN,
          ok = "",
          unmetNeeds = "",
          error = "",
          warning = "",
          `block condition` = "",
          trial = NaN,
          `condition name` = "",
          `target task` = "",
          `threshold parameter` = "",
          `target kind` = "",
          Loudspeaker = "",
          Microphone = "",
          QRConnect = "",
          comment = "",
          `heapLimitAfterDrawing (MB)` = NaN,
          deviceMemoryGB = NaN,
          mustTrackSec = NaN,
          goodTrials = NaN,
          badTrials = NaN,
          WebGLVersion = "",
          maxTextureSize = NaN,
          maxViewportSize = NaN,
          WebGLUnmaskedRenderer = "",
          order = NaN
        ) %>%
        select(
          `Prolific participant ID`, ProlificSessionID, `Pavlovia session ID`,
          `device type`, system, browser, resolution, QRConnect, date, ok, unmetNeeds,
          error, warning, cores, tardyMs, excessMs, KB, rows, cols, `block condition`,
          trial, `condition name`, `target task`, `threshold parameter`, `target kind`,
          `Computer 51 deg`, Loudspeaker, Microphone, comment, `heapLimitAfterDrawing (MB)`,
          deviceMemoryGB, mustTrackSec, goodTrials, badTrials, WebGLVersion,
          maxTextureSize, maxViewportSize, WebGLUnmaskedRenderer, order
        )
      t$ok <- as.factor(t$ok)
      t
    },
    error = function(e) {
      log_warn("FormSpree parse/transform failed: ", conditionMessage(e))
      tibble()
    }
  )
}

monitorFormSpree <- function(listFontParameters) {
  response <- formspree_get()
  if (is.null(response)) {
    return(tibble())
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  submissions <- tryCatch(
    jsonlite::fromJSON(content)$submissions,
    error = function(e) {
      log_warn("FormSpree JSON parse failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(submissions) || !is.data.frame(submissions) || nrow(submissions) == 0) {
    return(tibble())
  }

  if (!("pavloviaID" %in% colnames(submissions))) {
    submissions$pavloviaID <- NA
  }
  if (!("prolificParticipantID" %in% colnames(submissions))) {
    submissions$prolificParticipantID <- NA
  }
  if (!("prolificSession" %in% colnames(submissions))) {
    submissions$prolificSession <- NA
  }
  if (!("ExperimentName" %in% colnames(submissions))) {
    submissions$ExperimentName <- NA
  }
  if (!("OS" %in% colnames(submissions))) {
    submissions$OS <- NA
  }
  if (!("browser" %in% colnames(submissions))) {
    submissions$browser <- NA
  }
  if (!("browserVersion" %in% colnames(submissions))) {
    submissions$browserVersion <- NA
  }
  if (!("deviceType" %in% colnames(submissions))) {
    submissions$deviceType <- NA
  }

  initial <- submissions %>%
    filter(!is.na(OS)) %>%
    select(
      pavloviaID, prolificParticipantID, prolificSession, ExperimentName,
      OS, browser, browserVersion, deviceType
    )

  t <- submissions %>%
    mutate(
      date = parse_date_time(substr(`_date`, 1, 19), orders = c("ymdHMS")),
      pavloviaID = ifelse(is.na(pavloviaID), pavloviaId, pavloviaID)
    ) %>%
    group_by(pavloviaID) %>%
    ungroup() %>%
    mutate(date = format(date, "%b %d, %Y, %H:%M:%S")) %>%
    arrange(desc(`_date`)) %>%
    select(-`_date`)

  t <- t %>% select(-c(
    prolificParticipantID, prolificSession, ExperimentName,
    OS, browser, browserVersion, deviceType, timestamp
  ))
  t <- initial %>% full_join(t, by = "pavloviaID", relationship = "many-to-many")
  if ("fontLatencySec" %in% names(t)) {
    t <- t %>% mutate(hl = is.na(fontLatencySec))
  } else {
    t <- t %>% mutate(hl = FALSE)
  }

  t$OS <- stringr::str_replace_all(t$OS, "OS X", "macOS")
  if (listFontParameters) {
    t <- t %>% select(any_of(c(
      "pavloviaID", "date", "font", "fontMaxPx", "fontRenderMaxPx", "fontString",
      "block", "conditionName", "trial", "fontLatencySec", "hl"
    )))
  }
  t
}

get_font_parameters_from_formSpree <- function(participant) {
  empty <- tibble(
    `Pavlovia session ID` = character(),
    fontSizePx = numeric(),
    fixationXYPx = character(),
    fontMaxPx = numeric(),
    viewingDistanceCm = numeric(),
    fontRenderMaxPx = numeric()
  )

  response <- formspree_get()
  if (is.null(response)) {
    return(empty)
  }

  content <- httr::content(response, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(
    jsonlite::fromJSON(content),
    error = function(e) {
      log_warn("FormSpree JSON parse failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(parsed) || is.null(parsed$submissions) || !is.data.frame(parsed$submissions)) {
    return(empty)
  }

  t <- parsed$submissions %>%
    mutate(`Pavlovia session ID` = ifelse(is.na(pavloviaID), pavloviaId, pavloviaID)) %>%
    filter(`Pavlovia session ID` %in% participant) %>%
    filter(!is.na(fixationXYPx)) %>%
    select(
      `Pavlovia session ID`, fontSizePx, fixationXYPx, fontMaxPx,
      viewingDistanceCm, fontRenderMaxPx, timestamp
    ) %>%
    arrange(desc(timestamp)) %>%
    group_by(`Pavlovia session ID`) %>%
    slice(1) %>%
    select(-timestamp)
  t
}
