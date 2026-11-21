# Persian study helpers: participant status filter + longitudinal RSVP plot.
# Used by the Plots-tab scatter section (rsvp-training-sessions).

default_persian_status_path <- function() {
  file.path("data", "Persian_Study_Participant_Status.xlsx")
}

# Prefer an uploaded status spreadsheet (direct or extracted from zip);
# otherwise fall back to data/Persian_Study_Participant_Status.xlsx.
resolve_persian_status_path <- function(uploaded_path = NULL) {
  if (!is.null(uploaded_path) && length(uploaded_path) >= 1) {
    p <- as.character(uploaded_path[[1]])
    if (!is.na(p) && nzchar(p) && file.exists(p)) {
      return(normalizePath(p, winslash = "/", mustWork = FALSE))
    }
  }
  default_persian_status_path()
}

# FA-675321 and legacy "675321-FA" both map to FA-675321.
normalize_persian_participant_code <- function(x) {
  x <- trimws(as.character(x))
  empty <- is.na(x) | !nzchar(x)
  x[empty] <- NA_character_
  flipped <- !is.na(x) & grepl("^[0-9]+-FA$", x, ignore.case = TRUE)
  if (any(flipped)) {
    x[flipped] <- paste0(
      "FA-",
      sub("-FA$", "", x[flipped], ignore.case = TRUE)
    )
  }
  toupper(x)
}

read_persian_participant_status <- function(path = default_persian_status_path()) {
  # NULL / missing upload → data/Persian_Study_Participant_Status.xlsx
  path <- resolve_persian_status_path(path)
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(tibble::tibble(
      `Participant Code` = character(),
      code_key = character(),
      exclude = logical(),
      training_start = as.Date(character()),
      training_end = as.Date(character())
    ))
  }

  raw <- readxl::read_excel(path, sheet = 1L)
  names(raw) <- trimws(names(raw))

  rename_map <- c(
    "ParticipantCode" = "Participant Code",
    "Participant ID" = "Participant Code",
    "Training start date" = "First Training Session",
    "Training end date" = "Last Training Session"
  )
  for (nm in names(rename_map)) {
    if (nm %in% names(raw) && !rename_map[[nm]] %in% names(raw)) {
      names(raw)[names(raw) == nm] <- rename_map[[nm]]
    }
  }

  required <- c(
    "Participant Code", "Exclude",
    "First Training Session", "Last Training Session"
  )
  missing <- setdiff(required, names(raw))
  if (length(missing) > 0) {
    stop(
      "Persian status spreadsheet missing column(s): ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )
  }

  as_excel_date <- function(x) {
    if (inherits(x, "Date")) return(as.Date(x))
    if (inherits(x, "POSIXt")) return(as.Date(x))
    if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
    parsed <- lubridate::parse_date_time(
      as.character(x),
      orders = c("ymd", "mdy", "dmy", "ymdHMS", "mdyHMS"),
      quiet = TRUE
    )
    as.Date(parsed)
  }

  as_exclude_flag <- function(x) {
    if (is.logical(x)) return(isTRUE(x))
    if (is.numeric(x)) return(isTRUE(as.numeric(x) != 0))
    s <- tolower(trimws(as.character(x)))
    s %in% c("1", "true", "t", "yes", "y", "exclude")
  }

  raw %>%
    dplyr::filter(
      !is.na(`Participant Code`),
      trimws(as.character(`Participant Code`)) != ""
    ) %>%
    dplyr::mutate(
      `Participant Code` = trimws(as.character(`Participant Code`)),
      code_key = normalize_persian_participant_code(`Participant Code`),
      exclude = vapply(Exclude, as_exclude_flag, logical(1)),
      training_start = as_excel_date(`First Training Session`),
      training_end = as_excel_date(`Last Training Session`)
    ) %>%
    dplyr::select(
      `Participant Code`, code_key, exclude, training_start, training_end
    )
}

persian_first_nonempty_chr <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & trimws(x) != ""]
  if (length(x) == 0) return(NA_character_)
  trimws(x[[1]])
}

pretest_code_lookup <- function(pretest) {
  if (is.null(pretest) || !is.data.frame(pretest) || nrow(pretest) == 0) {
    return(tibble::tibble(participant = character(), pretest_code = character()))
  }
  code_col <- if ("ParticipantCode" %in% names(pretest)) {
    "ParticipantCode"
  } else if ("Participant ID" %in% names(pretest)) {
    "Participant ID"
  } else if ("participantID" %in% names(pretest)) {
    "participantID"
  } else {
    NA_character_
  }
  pav_col <- if ("participant" %in% names(pretest)) {
    "participant"
  } else if ("Pavlovia session ID" %in% names(pretest)) {
    "Pavlovia session ID"
  } else {
    NA_character_
  }
  if (is.na(code_col) || is.na(pav_col)) {
    return(tibble::tibble(participant = character(), pretest_code = character()))
  }
  pretest %>%
    dplyr::transmute(
      participant = as.character(.data[[pav_col]]),
      pretest_code = trimws(as.character(.data[[code_col]]))
    ) %>%
    dplyr::filter(
      !is.na(participant), participant != "",
      !is.na(pretest_code), pretest_code != ""
    ) %>%
    dplyr::distinct(participant, .keep_all = TRUE)
}

# One row per uploaded session file, classified vs the status spreadsheet.
build_persian_session_meta <- function(data_list,
                                       status = NULL,
                                       pretest = NULL,
                                       status_path = default_persian_status_path()) {
  if (is.null(status)) {
    status <- read_persian_participant_status(status_path)
  }
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    ParticipantCode = character(),
    code_key = character(),
    session_date = as.Date(character()),
    training_start = as.Date(character()),
    training_end = as.Date(character()),
    phase = character(),
    training_session = integer()
  )
  if (length(data_list) == 0 || nrow(status) == 0) return(empty)

  pretest_map <- pretest_code_lookup(pretest)

  meta <- purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
    pav <- persian_first_nonempty_chr(df$participant)
    code <- if ("ParticipantCode" %in% names(df)) {
      persian_first_nonempty_chr(df$ParticipantCode)
    } else {
      NA_character_
    }
    date_raw <- if ("date" %in% names(df)) {
      persian_first_nonempty_chr(df$date)
    } else {
      NA_character_
    }
    session_dt <- if (is.na(date_raw) || !nzchar(date_raw)) {
      as.Date(NA)
    } else if (exists("parse_pavlovia_date", mode = "function")) {
      as.Date(parse_pavlovia_date(date_raw))
    } else {
      as.Date(lubridate::parse_date_time(
        date_raw,
        orders = c("ymdHMS", "ymd", "mdyHMS"),
        quiet = TRUE
      ))
    }
    tibble::tibble(
      session_index = as.integer(i),
      participant = pav,
      ParticipantCode_raw = code,
      session_date = session_dt
    )
  })

  if (is.null(meta) || nrow(meta) == 0) return(empty)

  if (nrow(pretest_map) > 0) {
    meta <- meta %>% dplyr::left_join(pretest_map, by = "participant")
  } else {
    meta <- meta %>% dplyr::mutate(pretest_code = NA_character_)
  }

  meta %>%
    dplyr::mutate(
      code_candidate = dplyr::coalesce(
        dplyr::na_if(trimws(as.character(ParticipantCode_raw)), ""),
        dplyr::na_if(trimws(as.character(pretest_code)), ""),
        participant
      ),
      code_key = normalize_persian_participant_code(code_candidate)
    ) %>%
    dplyr::left_join(
      status %>%
        dplyr::select(
          code_key, `Participant Code`, exclude, training_start, training_end
        ),
      by = "code_key"
    ) %>%
    dplyr::filter(!is.na(`Participant Code`), !exclude) %>%
    dplyr::mutate(
      phase = dplyr::case_when(
        is.na(session_date) | is.na(training_start) | is.na(training_end) ~ "unknown",
        session_date < training_start ~ "pre",
        session_date > training_end ~ "post",
        TRUE ~ "training"
      ),
      training_session = dplyr::if_else(
        phase == "training",
        as.integer(session_date - training_start) + 1L,
        NA_integer_
      ),
      ParticipantCode = `Participant Code`
    ) %>%
    dplyr::select(
      session_index, participant, ParticipantCode, code_key, session_date,
      training_start, training_end, phase, training_session
    ) %>%
    dplyr::distinct()
}

# Shared RSVP row filter for one session file (summary preferred over raw).
# Returns rows with finite questMeanAtEndOfTrialsLoop; practice removed.
persian_session_rsvp_rows <- function(df, sm = NULL) {
  src <- if (!is.null(sm) && is.data.frame(sm) && nrow(sm) > 0) sm else df
  if (is.null(src) || !is.data.frame(src) || nrow(src) < 1) return(NULL)
  if (!"questMeanAtEndOfTrialsLoop" %in% names(src)) return(NULL)

  has_kind <- "targetKind" %in% names(src)
  has_cond <- "conditionName" %in% names(src)
  rsvp <- src %>%
    dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>%
    dplyr::mutate(
      questMeanAtEndOfTrialsLoop = suppressWarnings(
        as.numeric(questMeanAtEndOfTrialsLoop)
      )
    ) %>%
    dplyr::filter(is.finite(questMeanAtEndOfTrialsLoop))

  if (nrow(rsvp) == 0) return(NULL)

  if (has_kind) {
    rsvp <- rsvp %>%
      dplyr::filter(
        targetKind == "rsvpReading" |
          (!is.na(targetKind) & grepl("rsvp", targetKind, ignore.case = TRUE))
      )
  } else if (has_cond) {
    rsvp <- rsvp %>%
      dplyr::filter(grepl("rsvp", conditionName, ignore.case = TRUE))
  } else {
    return(NULL)
  }

  if (has_cond) {
    rsvp <- rsvp %>%
      dplyr::filter(!grepl("practice", conditionName, ignore.case = TRUE))
  }
  if (nrow(rsvp) == 0) return(NULL)
  rsvp
}

persian_session_date_from_df <- function(df) {
  date_raw <- if ("date" %in% names(df)) {
    persian_first_nonempty_chr(df$date)
  } else {
    NA_character_
  }
  if (is.na(date_raw) || !nzchar(date_raw)) return(as.Date(NA))
  if (exists("parse_pavlovia_date", mode = "function")) {
    return(as.Date(parse_pavlovia_date(date_raw)))
  }
  as.Date(lubridate::parse_date_time(
    date_raw,
    orders = c("ymdHMS", "ymd", "mdyHMS"),
    quiet = TRUE
  ))
}

# Per uploaded file: one RSVP WPM (geomean across non-practice conditions).
# Keeps session_date so the same ParticipantCode across days stays separate
# (generate_threshold would otherwise average them together).
extract_session_rsvp_thresholds <- function(data_list, summary_list = NULL) {
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    session_date = as.Date(character()),
    wpm = numeric(),
    n_conditions = integer()
  )
  if (length(data_list) == 0) return(empty)

  purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)

    sm <- NULL
    if (!is.null(summary_list) && length(summary_list) >= i) {
      sm <- summary_list[[i]]
    }
    rsvp <- persian_session_rsvp_rows(df, sm)
    if (is.null(rsvp) || nrow(rsvp) == 0) return(NULL)

    # Same transform as generate_threshold: log_duration_s → log10 WPM.
    wpm <- 10^(log10(60) - rsvp$questMeanAtEndOfTrialsLoop)
    wpm <- wpm[is.finite(wpm) & wpm > 0]
    if (length(wpm) == 0) return(NULL)

    tibble::tibble(
      session_index = as.integer(i),
      participant = persian_first_nonempty_chr(df$participant),
      session_date = persian_session_date_from_df(df),
      wpm = exp(mean(log(wpm))),
      n_conditions = length(wpm)
    )
  }) %>%
    {
      if (nrow(.) == 0) empty else .
    }
}

# Per uploaded file × font: RSVP WPM (for pre/during/post violins by font).
extract_session_rsvp_by_font <- function(data_list, summary_list = NULL) {
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    session_date = as.Date(character()),
    font = character(),
    wpm = numeric(),
    n_obs = integer()
  )
  if (length(data_list) == 0) return(empty)

  purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)

    sm <- NULL
    if (!is.null(summary_list) && length(summary_list) >= i) {
      sm <- summary_list[[i]]
    }
    rsvp <- persian_session_rsvp_rows(df, sm)
    if (is.null(rsvp) || nrow(rsvp) == 0) return(NULL)
    if (!"font" %in% names(rsvp)) return(NULL)

    rsvp %>%
      dplyr::mutate(
        font = trimws(as.character(font)),
        wpm = 10^(log10(60) - questMeanAtEndOfTrialsLoop)
      ) %>%
      dplyr::filter(
        !is.na(font), font != "",
        is.finite(wpm), wpm > 0
      ) %>%
      dplyr::group_by(font) %>%
      dplyr::summarize(
        n_obs = dplyr::n(),
        wpm = exp(mean(log(wpm))),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        session_index = as.integer(i),
        participant = persian_first_nonempty_chr(df$participant),
        session_date = persian_session_date_from_df(df)
      ) %>%
      dplyr::select(
        session_index, participant, session_date, font, wpm, n_obs
      )
  }) %>%
    {
      if (nrow(.) == 0) empty else .
    }
}

# Map a Pavlovia/CSV participant id to an included spreadsheet Participant Code.
resolve_included_participant_code <- function(participant,
                                              ParticipantCode_raw = NA_character_,
                                              pretest_code = NA_character_,
                                              status_df) {
  candidate <- dplyr::coalesce(
    dplyr::na_if(trimws(as.character(ParticipantCode_raw)), ""),
    dplyr::na_if(trimws(as.character(pretest_code)), ""),
    trimws(as.character(participant))
  )
  key <- normalize_persian_participant_code(candidate)
  if (is.null(status_df) || nrow(status_df) == 0 || is.na(key)) {
    return(NA_character_)
  }
  hit <- status_df %>%
    dplyr::filter(!exclude, code_key == !!key) %>%
    dplyr::slice(1L)
  if (nrow(hit) == 0) return(NA_character_)
  hit$`Participant Code`[[1]]
}

persian_participant_colors <- function(codes) {
  codes <- sort(unique(as.character(codes)))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0) return(character(0))
  stats::setNames(scales::hue_pal()(length(codes)), codes)
}

# Lowercased keys for Exclude=TRUE rows in the Persian status spreadsheet.
# Includes both raw and FA-normalized forms so "675321-FA" matches "FA-675321".
persian_status_exclude_keys <- function(status_path = default_persian_status_path()) {
  st <- tryCatch(
    read_persian_participant_status(status_path),
    error = function(e) NULL
  )
  if (is.null(st) || nrow(st) == 0) return(character())
  codes <- st %>%
    dplyr::filter(exclude) %>%
    dplyr::pull(`Participant Code`)
  codes <- trimws(as.character(codes))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  if (length(codes) == 0) return(character())
  unique(tolower(c(codes, normalize_persian_participant_code(codes))))
}

# TRUE when id matches an exclude key (raw lower or FA-normalized lower).
participant_matches_exclude_keys <- function(ids, exclude_keys) {
  keys <- unique(as.character(exclude_keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  n <- length(ids)
  if (n == 0 || length(keys) == 0) return(rep(FALSE, n))
  ids_chr <- as.character(ids)
  tolower(ids_chr) %in% keys |
    tolower(normalize_persian_participant_code(ids_chr)) %in% keys
}

# Drop rows whose participant / ParticipantCode is Exclude=TRUE in the status file
# (or already listed in basicExclude$lowerCaseParticipant from pretest Exclude?).
filter_out_excluded_participants <- function(df, exclude_keys) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(df)
  keys <- unique(as.character(exclude_keys))
  keys <- keys[!is.na(keys) & nzchar(keys)]
  if (length(keys) == 0) return(df)
  drop <- rep(FALSE, nrow(df))
  if ("participant" %in% names(df)) {
    drop <- drop | participant_matches_exclude_keys(df$participant, keys)
  }
  if ("ParticipantCode" %in% names(df)) {
    drop <- drop | participant_matches_exclude_keys(df$ParticipantCode, keys)
  }
  df[!drop, , drop = FALSE]
}

# RSVP days for included participants, with pre / training / post labels from
# the status spreadsheet training window.
prepare_persian_rsvp_session_data <- function(data_list,
                                             summary_list = NULL,
                                             pretest = NULL,
                                             status = NULL,
                                             status_path = default_persian_status_path()) {
  empty <- tibble::tibble(
    ParticipantCode = character(),
    session_date = as.Date(character()),
    wpm = numeric(),
    phase = character()
  )
  if (is.null(data_list) || length(data_list) == 0) return(empty)

  status_df <- if (is.null(status)) {
    tryCatch(
      read_persian_participant_status(status_path),
      error = function(e) NULL
    )
  } else {
    status
  }
  if (is.null(status_df) || nrow(status_df) == 0) return(empty)
  status_included <- status_df %>% dplyr::filter(!exclude)
  if (nrow(status_included) == 0) return(empty)

  pretest_map <- pretest_code_lookup(pretest)
  rsvp <- extract_session_rsvp_thresholds(data_list, summary_list)
  if (nrow(rsvp) == 0) return(empty)

  code_raw <- purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
    tibble::tibble(
      session_index = as.integer(i),
      ParticipantCode_raw = if ("ParticipantCode" %in% names(df)) {
        persian_first_nonempty_chr(df$ParticipantCode)
      } else {
        NA_character_
      }
    )
  })
  if (!is.null(code_raw) && nrow(code_raw) > 0) {
    rsvp <- rsvp %>% dplyr::left_join(code_raw, by = "session_index")
  } else {
    rsvp <- rsvp %>% dplyr::mutate(ParticipantCode_raw = NA_character_)
  }

  if (nrow(pretest_map) > 0) {
    rsvp <- rsvp %>% dplyr::left_join(pretest_map, by = "participant")
  } else {
    rsvp <- rsvp %>% dplyr::mutate(pretest_code = NA_character_)
  }

  rsvp <- rsvp %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ParticipantCode = resolve_included_participant_code(
        participant,
        ParticipantCode_raw,
        pretest_code,
        status_included
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(ParticipantCode),
      !is.na(session_date),
      is.finite(wpm),
      wpm > 0
    )

  if (nrow(rsvp) == 0) return(empty)

  status_dates <- status_included %>%
    dplyr::select(
      ParticipantCode = `Participant Code`,
      training_start,
      training_end
    )

  rsvp %>%
    dplyr::group_by(ParticipantCode, session_date) %>%
    dplyr::summarize(
      wpm = exp(mean(log(wpm), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::left_join(status_dates, by = "ParticipantCode") %>%
    dplyr::mutate(
      # Per-participant window from the status spreadsheet:
      # pre = before First Training Session; training = start..end inclusive;
      # post = after Last Training Session.
      phase = dplyr::case_when(
        is.na(training_start) | is.na(training_end) ~ "unknown",
        session_date < training_start ~ "pre",
        session_date > training_end ~ "post",
        TRUE ~ "training"
      )
    ) %>%
    dplyr::select(ParticipantCode, session_date, wpm, phase)
}

# One row per participant × font × session_date, with pre/training/post phase.
prepare_persian_rsvp_session_font_data <- function(data_list,
                                                  summary_list = NULL,
                                                  pretest = NULL,
                                                  status = NULL,
                                                  status_path = default_persian_status_path()) {
  empty <- tibble::tibble(
    ParticipantCode = character(),
    font = character(),
    session_date = as.Date(character()),
    wpm = numeric(),
    phase = character()
  )
  rsvp <- extract_session_rsvp_by_font(data_list, summary_list)
  rsvp <- annotate_persian_rsvp_phases(
    rsvp,
    data_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  if (nrow(rsvp) == 0) return(empty)

  rsvp %>%
    dplyr::filter(
      phase %in% c("pre", "training", "post"),
      !is.na(font), font != ""
    ) %>%
    dplyr::group_by(ParticipantCode, font, session_date, phase) %>%
    dplyr::summarize(
      wpm = exp(mean(log(wpm), na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::filter(is.finite(wpm), wpm > 0) %>%
    dplyr::select(ParticipantCode, font, session_date, wpm, phase)
}

# Sessions numbered by date within a phase filter (or all phases).
# Facets by font when a `font` column is present.
# `participant_colors` / `y_limits` keep related plots comparable.
plot_rsvp_phase_sessions <- function(session_df,
                                     phase = c("pre", "training", "post", "all"),
                                     participant_colors = NULL,
                                     y_limits = NULL,
                                     title = NULL) {
  phase <- match.arg(phase)
  if (is.null(session_df) || nrow(session_df) == 0) return(NULL)

  plot_df <- session_df
  if (!identical(phase, "all")) {
    plot_df <- plot_df %>% dplyr::filter(phase == !!phase)
  }
  facet_by_font <- "font" %in% names(plot_df) &&
    any(!is.na(plot_df$font) & nzchar(as.character(plot_df$font)))

  if (facet_by_font) {
    plot_df <- plot_df %>%
      dplyr::filter(!is.na(font), font != "") %>%
      dplyr::arrange(ParticipantCode, font, session_date) %>%
      dplyr::group_by(ParticipantCode, font) %>%
      dplyr::mutate(session_number = dplyr::row_number()) %>%
      dplyr::ungroup()
  } else {
    plot_df <- plot_df %>%
      dplyr::arrange(ParticipantCode, session_date) %>%
      dplyr::group_by(ParticipantCode) %>%
      dplyr::mutate(session_number = dplyr::row_number()) %>%
      dplyr::ungroup()
  }

  if (nrow(plot_df) == 0) return(NULL)

  if (is.null(participant_colors) || length(participant_colors) == 0) {
    participant_colors <- persian_participant_colors(plot_df$ParticipantCode)
  }
  plot_df <- plot_df %>%
    dplyr::mutate(
      ParticipantCode = factor(
        ParticipantCode,
        levels = names(participant_colors)
      )
    )

  max_sess <- max(plot_df$session_number, na.rm = TRUE)
  if (is.null(title) || !nzchar(title)) {
    title <- switch(
      phase,
      pre = "RSVP reading speed — pre-training",
      training = "RSVP reading speed — during training",
      post = "RSVP reading speed — post-training",
      all = "RSVP reading speed across sessions",
      "RSVP reading speed across sessions"
    )
  }

  # Keep every shared participant in the legend (with color) even when this
  # phase has no points for them — scale drop=FALSE alone still omits keys.
  legend_df <- tibble::tibble(
    session_number = 1,
    wpm = min(plot_df$wpm, na.rm = TRUE),
    ParticipantCode = factor(
      names(participant_colors),
      levels = names(participant_colors)
    )
  )
  if (facet_by_font) {
    fonts <- sort(unique(as.character(plot_df$font)))
    legend_df <- tidyr::crossing(legend_df, font = fonts)
  }

  p <- ggplot2::ggplot(
    plot_df,
    ggplot2::aes(
      x = session_number,
      y = wpm,
      color = ParticipantCode,
      group = ParticipantCode
    )
  ) +
    ggplot2::geom_blank(data = legend_df) +
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_continuous(
      breaks = function(x) {
        lo <- max(1L, as.integer(ceiling(min(x, na.rm = TRUE))))
        hi <- as.integer(floor(max(x, na.rm = TRUE)))
        if (!is.finite(lo) || !is.finite(hi) || hi < lo) return(1L)
        seq.int(lo, hi)
      },
      name = "Session number (by date)"
    ) +
    ggplot2::scale_color_manual(
      values = participant_colors,
      breaks = names(participant_colors),
      limits = names(participant_colors),
      drop = FALSE,
      name = "Participant"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        override.aes = list(alpha = 1, linewidth = 0.7, shape = 16, size = 2.2)
      )
    ) +
    ggplot2::labs(
      # Visible figure title: use subtitle so add_experiment_title() can put the
      # short experiment name in plot.title (size 9); subtitle stays size 18.
      subtitle = title,
      y = "RSVP reading speed (word/min)"
    ) +
    ggplot2::theme_bw() +
    plt_theme_scatter +
    ggplot2::theme(
      legend.position = "top",
      panel.grid.minor = ggplot2::element_blank()
    )

  if (facet_by_font) {
    # free_x: during training some fonts have far fewer sessions than others.
    p <- p +
      ggplot2::facet_wrap(~font, ncol = 2, scales = "free_x") +
      ggplot2::theme(strip.text = ggplot2::element_text(size = 12))
  } else {
    p <- p + ggplot2::coord_cartesian(xlim = c(1, max_sess))
  }

  if (!is.null(y_limits) && length(y_limits) == 2 && all(is.finite(y_limits))) {
    p <- p + ggplot2::scale_y_log10(limits = y_limits)
  } else {
    p <- p + ggplot2::scale_y_log10()
  }

  if (facet_by_font) {
    n_fonts <- length(unique(as.character(plot_df$font)))
    n_rows <- as.integer(ceiling(n_fonts / 2))
    attr(p, "plots_display_width_in") <- 10
    attr(p, "plots_display_height_in") <- max(7, 3.5 * n_rows + 1.5)
  }
  p
}

# Pre / during / post RSVP plots (shared participant colors + y-scale),
# faceted by font within each phase.
# Pre = before First Training Session; training = start..end inclusive;
# post = after Last Training Session.
# Returns list(pre=, training=, post=); each ggplot or NULL.
plot_rsvp_pre_post_sessions <- function(data_list,
                                        summary_list = NULL,
                                        pretest = NULL,
                                        status = NULL,
                                        status_path = default_persian_status_path()) {
  session_df <- prepare_persian_rsvp_session_font_data(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  if (nrow(session_df) == 0) {
    return(list(pre = NULL, training = NULL, post = NULL))
  }

  colors <- persian_participant_colors(session_df$ParticipantCode)
  y_limits <- range(session_df$wpm, na.rm = TRUE)
  if (!all(is.finite(y_limits)) || y_limits[1] <= 0) {
    y_limits <- NULL
  } else {
    y_limits <- c(y_limits[1] / 1.15, y_limits[2] * 1.15)
  }

  list(
    pre = plot_rsvp_phase_sessions(
      session_df,
      phase = "pre",
      participant_colors = colors,
      y_limits = y_limits
    ),
    training = plot_rsvp_phase_sessions(
      session_df,
      phase = "training",
      participant_colors = colors,
      y_limits = y_limits
    ),
    post = plot_rsvp_phase_sessions(
      session_df,
      phase = "post",
      participant_colors = colors,
      y_limits = y_limits
    )
  )
}

# Backward-compatible alias: during-training longitudinal plot.
plot_rsvp_training_sessions <- function(data_list,
                                        summary_list = NULL,
                                        pretest = NULL,
                                        status = NULL,
                                        status_path = default_persian_status_path()) {
  plot_rsvp_pre_post_sessions(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )$training
}

# Attach included ParticipantCode + pre/training/post phase to session rows.
# `value_col` is the measure to require finite & > 0 (e.g. "wpm", "crowding_deg").
annotate_persian_rsvp_phases <- function(rsvp,
                                         data_list,
                                         pretest = NULL,
                                         status = NULL,
                                         status_path = default_persian_status_path(),
                                         value_col = "wpm") {
  empty <- rsvp[0, , drop = FALSE]
  if (is.null(rsvp) || nrow(rsvp) == 0) return(empty)
  if (!value_col %in% names(rsvp)) return(empty)

  status_df <- if (is.null(status)) {
    tryCatch(
      read_persian_participant_status(status_path),
      error = function(e) NULL
    )
  } else {
    status
  }
  if (is.null(status_df) || nrow(status_df) == 0) return(empty)
  status_included <- status_df %>% dplyr::filter(!exclude)
  if (nrow(status_included) == 0) return(empty)

  pretest_map <- pretest_code_lookup(pretest)
  code_raw <- purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
    tibble::tibble(
      session_index = as.integer(i),
      ParticipantCode_raw = if ("ParticipantCode" %in% names(df)) {
        persian_first_nonempty_chr(df$ParticipantCode)
      } else {
        NA_character_
      }
    )
  })
  if (!is.null(code_raw) && nrow(code_raw) > 0) {
    rsvp <- rsvp %>% dplyr::left_join(code_raw, by = "session_index")
  } else {
    rsvp <- rsvp %>% dplyr::mutate(ParticipantCode_raw = NA_character_)
  }

  if (nrow(pretest_map) > 0) {
    rsvp <- rsvp %>% dplyr::left_join(pretest_map, by = "participant")
  } else {
    rsvp <- rsvp %>% dplyr::mutate(pretest_code = NA_character_)
  }

  status_dates <- status_included %>%
    dplyr::select(
      ParticipantCode = `Participant Code`,
      training_start,
      training_end
    )

  value_ok <- is.finite(rsvp[[value_col]]) & rsvp[[value_col]] > 0

  rsvp %>%
    dplyr::mutate(.value_ok = value_ok) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      ParticipantCode = resolve_included_participant_code(
        participant,
        ParticipantCode_raw,
        pretest_code,
        status_included
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(
      !is.na(ParticipantCode),
      !is.na(session_date),
      .value_ok
    ) %>%
    dplyr::select(-.value_ok) %>%
    dplyr::left_join(status_dates, by = "ParticipantCode") %>%
    dplyr::mutate(
      phase = dplyr::case_when(
        is.na(training_start) | is.na(training_end) ~ "unknown",
        session_date < training_start ~ "pre",
        session_date > training_end ~ "post",
        TRUE ~ "training"
      )
    )
}

# Multi-file same participant in pre/post (interrupted resume, partial + retest,
# failed then restart): do not average split attempts.
# For each font keep the session with the most observations; ties → session with
# more total obs in that phase, then later date. Fonts only in the other file
# are kept (complementary union). Training days stay multi-session (geomeaned).
persian_collapse_phase_sessions <- function(df, value_col, n_obs_col = "n_obs") {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!value_col %in% names(df)) return(df)
  needed <- c("ParticipantCode", "font", "phase", "session_index")
  if (!all(needed %in% names(df))) return(df)

  if (!n_obs_col %in% names(df)) {
    df[[n_obs_col]] <- 1L
  }

  df <- df %>%
    dplyr::mutate(
      .n_obs = suppressWarnings(as.integer(.data[[n_obs_col]])),
      .n_obs = dplyr::if_else(is.finite(.n_obs) & .n_obs > 0L, .n_obs, 1L)
    )

  session_tot <- df %>%
    dplyr::group_by(ParticipantCode, phase, session_index) %>%
    dplyr::summarize(.session_n = sum(.n_obs, na.rm = TRUE), .groups = "drop")

  df %>%
    dplyr::left_join(
      session_tot,
      by = c("ParticipantCode", "phase", "session_index")
    ) %>%
    dplyr::group_by(ParticipantCode, font, phase) %>%
    dplyr::arrange(
      dplyr::desc(.n_obs),
      dplyr::desc(.session_n),
      dplyr::desc(session_date),
      dplyr::desc(session_index),
      .by_group = TRUE
    ) %>%
    dplyr::slice(1L) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.n_obs, -.session_n)
}

# Aggregate annotated session rows → one y per participant × font × phase.
# Pre/post: complementary collapse of split files. Training: geomean/mean.
persian_aggregate_font_phase <- function(df,
                                         value_col,
                                         use_geomean = TRUE) {
  empty <- tibble::tibble(
    participant = character(),
    font = character(),
    phase = character(),
    y = numeric()
  )
  if (is.null(df) || nrow(df) == 0) return(empty)
  if (!value_col %in% names(df)) return(empty)

  df <- df %>% dplyr::filter(phase %in% c("pre", "training", "post"))
  if (nrow(df) == 0) return(empty)

  pre_post <- persian_collapse_phase_sessions(
    df %>% dplyr::filter(phase %in% c("pre", "post")),
    value_col = value_col
  )
  training <- df %>% dplyr::filter(phase == "training")
  combined <- dplyr::bind_rows(pre_post, training)
  if (nrow(combined) == 0) return(empty)

  if (use_geomean) {
    combined %>%
      dplyr::group_by(ParticipantCode, font, phase) %>%
      dplyr::summarize(
        y = exp(mean(log(.data[[value_col]]), na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(y), y > 0) %>%
      dplyr::transmute(
        participant = ParticipantCode,
        font,
        phase,
        y
      )
  } else {
    combined %>%
      dplyr::group_by(ParticipantCode, font, phase) %>%
      dplyr::summarize(
        y = mean(.data[[value_col]], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::filter(is.finite(y), y > 0) %>%
      dplyr::transmute(
        participant = ParticipantCode,
        font,
        phase,
        y
      )
  }
}

# One row per participant × font × phase.
# Columns font/participant/y match create_violin_plot().
prepare_persian_rsvp_font_phase_data <- function(data_list,
                                                summary_list = NULL,
                                                pretest = NULL,
                                                status = NULL,
                                                status_path = default_persian_status_path()) {
  empty <- tibble::tibble(
    participant = character(),
    font = character(),
    phase = character(),
    y = numeric()
  )
  rsvp <- extract_session_rsvp_by_font(data_list, summary_list)
  rsvp <- annotate_persian_rsvp_phases(
    rsvp,
    data_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  if (nrow(rsvp) == 0) return(empty)
  persian_aggregate_font_phase(rsvp, value_col = "wpm", use_geomean = TRUE)
}

# Shared helper: build phase font violins from phase_df (font/participant/y/phase).
# Shared y-limits across `phases` so pre/during/post on one row are comparable.
plot_persian_phase_font_violins <- function(phase_df,
                                            ylabel,
                                            titles,
                                            use_log_scale = TRUE,
                                            phases = c("pre", "training", "post")) {
  empty <- list(pre = NULL, training = NULL, post = NULL)
  if (is.null(phase_df) || nrow(phase_df) == 0) return(empty)
  if (!exists("create_violin_plot", mode = "function")) return(empty)

  phases <- intersect(phases, c("pre", "training", "post"))
  if (length(phases) == 0) return(empty)

  phase_df <- phase_df %>% dplyr::filter(phase %in% phases)
  if (nrow(phase_df) == 0) return(empty)

  y_limits <- range(phase_df$y, na.rm = TRUE)
  if (!all(is.finite(y_limits))) {
    y_limits <- NULL
  } else if (use_log_scale && y_limits[1] <= 0) {
    y_limits <- NULL
  } else if (use_log_scale) {
    y_limits <- c(y_limits[1] / 1.15, y_limits[2] * 1.15)
  } else {
    pad <- diff(y_limits)
    if (!is.finite(pad) || pad <= 0) pad <- 1
    y_limits <- c(y_limits[1] - 0.15 * pad, y_limits[2] + 0.15 * pad)
  }

  out <- empty
  for (ph in phases) {
    d <- phase_df %>% dplyr::filter(phase == !!ph)
    if (nrow(d) == 0) next
    p <- create_violin_plot(
      d,
      ylabel,
      titles[[ph]],
      use_log_scale = use_log_scale
    )
    if (is.null(p)) next
    # Shared y-range without clipping violin density (scale limits would).
    if (!is.null(y_limits)) {
      p <- p + ggplot2::coord_cartesian(ylim = y_limits)
    }
    out[[ph]] <- p
  }
  out
}

# Pre / during / post RSVP violins by font (shared y-scale).
# Returns list(pre=, training=, post=); each ggplot or NULL.
plot_rsvp_phase_font_violins <- function(data_list,
                                         summary_list = NULL,
                                         pretest = NULL,
                                         status = NULL,
                                         status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_rsvp_font_phase_data(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "RSVP Reading Speed (word/min)",
    titles = c(
      pre = "RSVP Reading Speed by Font — pre-training",
      training = "RSVP Reading Speed by Font — during training",
      post = "RSVP Reading Speed by Font — post-training"
    ),
    use_log_scale = TRUE
  )
}

# Crowding rows for one session (letter + spacingDeg/spacing, practice removed).
persian_session_crowding_rows <- function(df, sm = NULL) {
  src <- if (!is.null(sm) && is.data.frame(sm) && nrow(sm) > 0) sm else df
  if (is.null(src) || !is.data.frame(src) || nrow(src) < 1) return(NULL)
  if (!"questMeanAtEndOfTrialsLoop" %in% names(src)) return(NULL)

  crowding <- src %>%
    dplyr::filter(!is.na(questMeanAtEndOfTrialsLoop)) %>%
    dplyr::mutate(
      questMeanAtEndOfTrialsLoop = suppressWarnings(
        as.numeric(questMeanAtEndOfTrialsLoop)
      )
    ) %>%
    dplyr::filter(is.finite(questMeanAtEndOfTrialsLoop))

  if (nrow(crowding) == 0) return(NULL)

  if ("targetKind" %in% names(crowding)) {
    crowding <- crowding %>% dplyr::filter(targetKind == "letter")
  }
  if ("thresholdParameter" %in% names(crowding)) {
    crowding <- crowding %>%
      dplyr::filter(thresholdParameter %in% c("spacingDeg", "spacing"))
  } else {
    return(NULL)
  }
  if ("conditionName" %in% names(crowding)) {
    crowding <- crowding %>%
      dplyr::filter(!grepl("practice", conditionName, ignore.case = TRUE))
  }
  if (nrow(crowding) == 0) return(NULL)
  crowding
}

# Per uploaded file × font: crowding distance (deg). L/R geomean within font.
extract_session_crowding_by_font <- function(data_list, summary_list = NULL) {
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    session_date = as.Date(character()),
    font = character(),
    crowding_deg = numeric(),
    n_obs = integer()
  )
  if (length(data_list) == 0) return(empty)

  purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)

    sm <- NULL
    if (!is.null(summary_list) && length(summary_list) >= i) {
      sm <- summary_list[[i]]
    }
    crowding <- persian_session_crowding_rows(df, sm)
    if (is.null(crowding) || nrow(crowding) == 0) return(NULL)
    if (!"font" %in% names(crowding)) return(NULL)

    crowding %>%
      dplyr::mutate(font = trimws(as.character(font))) %>%
      dplyr::filter(!is.na(font), font != "") %>%
      dplyr::group_by(font) %>%
      # L/R (and any repeats): mean of log10 spacing → linear geomean.
      dplyr::summarize(
        n_obs = dplyr::n(),
        log_crowding = mean(questMeanAtEndOfTrialsLoop, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        crowding_deg = 10^log_crowding,
        session_index = as.integer(i),
        participant = persian_first_nonempty_chr(df$participant),
        session_date = persian_session_date_from_df(df)
      ) %>%
      dplyr::filter(is.finite(crowding_deg), crowding_deg > 0) %>%
      dplyr::select(
        session_index, participant, session_date, font, crowding_deg, n_obs
      )
  }) %>%
    {
      if (nrow(.) == 0) empty else .
    }
}

# One row per participant × font × phase.
prepare_persian_crowding_font_phase_data <- function(data_list,
                                                    summary_list = NULL,
                                                    pretest = NULL,
                                                    status = NULL,
                                                    status_path = default_persian_status_path()) {
  empty <- tibble::tibble(
    participant = character(),
    font = character(),
    phase = character(),
    y = numeric()
  )
  crowding <- extract_session_crowding_by_font(data_list, summary_list)
  crowding <- annotate_persian_rsvp_phases(
    crowding,
    data_list,
    pretest = pretest,
    status = status,
    status_path = status_path,
    value_col = "crowding_deg"
  )
  if (nrow(crowding) == 0) return(empty)
  persian_aggregate_font_phase(
    crowding, value_col = "crowding_deg", use_geomean = TRUE
  )
}

# Pre / during / post crowding violins by font (shared y-scale).
# One point per participant per font (L/R geomeaned) — up to 4 fonts each.
plot_crowding_phase_font_violins <- function(data_list,
                                             summary_list = NULL,
                                             pretest = NULL,
                                             status = NULL,
                                             status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_crowding_font_phase_data(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "Crowding Distance (deg)",
    titles = c(
      pre = "Crowding Threshold by Font — pre-training",
      training = "Crowding Threshold by Font — during training",
      post = "Crowding Threshold by Font — post-training"
    ),
    use_log_scale = TRUE,
    phases = c("pre", "training", "post")
  )
}

# Map Persian comfort/beauty labels (CMFRT-Barik, beauty-Nazanin, …) to font files.
persian_map_rating_font <- function(font_or_label) {
  x <- as.character(font_or_label)
  if (exists("comparison_rating_font", mode = "function")) {
    x <- comparison_rating_font(x)
  }
  key <- tolower(sub(
    "^(cmfrt-|beauty-|fmlrty-|familiarity-)",
    "",
    x,
    ignore.case = TRUE
  ))
  mapped <- dplyr::case_when(
    grepl("nazanin", key) ~ "B-NAZANIN.TTF",
    grepl("iran.?nastaliq|nastaliq", key) ~ "IranNastaliq.ttf",
    grepl("hoor|mj_hoor", key) ~ "Mj_Hoor_0.ttf",
    grepl("barik", key) ~ "Mj_Barik.ttf",
    grepl("titr", key) ~ "Titr.bold.woff2",
    grepl("kalameh", key) ~ "Kalameh-Regular.ttf",
    grepl("moalla", key) ~ "Moalla.ttf",
    TRUE ~ x
  )
  mapped
}

# Ordinary reading page rows (targetKind reading; WPM from words/duration).
# Persian files often have readingPages = -1, so threshold's readingPages > 1
# filter drops them — recompute WPM here from page metrics.
persian_session_reading_rows <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
  needed <- c(
    "targetKind", "font", "readingPageWords",
    "readingPageDurationOnsetToOffsetSec"
  )
  if (!all(needed %in% names(df))) return(NULL)

  reading <- df %>%
    dplyr::filter(
      targetKind == "reading",
      !is.na(font),
      trimws(as.character(font)) != ""
    )
  if ("conditionName" %in% names(reading)) {
    reading <- reading %>%
      dplyr::filter(!grepl("practice", conditionName, ignore.case = TRUE))
  }
  if (nrow(reading) == 0) return(NULL)

  reading %>%
    dplyr::mutate(
      font = trimws(as.character(font)),
      words = suppressWarnings(as.numeric(readingPageWords)),
      dur_sec = suppressWarnings(
        as.numeric(readingPageDurationOnsetToOffsetSec)
      ),
      wpm = dplyr::if_else(
        is.finite(words) & is.finite(dur_sec) & dur_sec > 0.2 & words > 0,
        words / dur_sec * 60,
        NA_real_
      )
    ) %>%
    dplyr::filter(is.finite(wpm), wpm > 0, wpm <= 2000)
}

# Per uploaded file × font: ordinary reading WPM (geomean of page WPMs).
extract_session_reading_by_font <- function(data_list, summary_list = NULL) {
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    session_date = as.Date(character()),
    font = character(),
    wpm = numeric(),
    n_obs = integer()
  )
  if (length(data_list) == 0) return(empty)
  # summary_list unused; reading page metrics live on raw trial rows.
  invisible(summary_list)

  purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
    reading <- persian_session_reading_rows(df)
    if (is.null(reading) || nrow(reading) == 0) return(NULL)

    reading %>%
      dplyr::group_by(font) %>%
      dplyr::summarize(
        n_obs = dplyr::n(),
        wpm = exp(mean(log(wpm), na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        session_index = as.integer(i),
        participant = persian_first_nonempty_chr(df$participant),
        session_date = persian_session_date_from_df(df)
      ) %>%
      dplyr::filter(is.finite(wpm), wpm > 0) %>%
      dplyr::select(
        session_index, participant, session_date, font, wpm, n_obs
      )
  }) %>%
    {
      if (nrow(.) == 0) empty else .
    }
}

prepare_persian_reading_font_phase_data <- function(data_list,
                                                   summary_list = NULL,
                                                   pretest = NULL,
                                                   status = NULL,
                                                   status_path = default_persian_status_path()) {
  empty <- tibble::tibble(
    participant = character(),
    font = character(),
    phase = character(),
    y = numeric()
  )
  reading <- extract_session_reading_by_font(data_list, summary_list)
  reading <- annotate_persian_rsvp_phases(
    reading,
    data_list,
    pretest = pretest,
    status = status,
    status_path = status_path,
    value_col = "wpm"
  )
  if (nrow(reading) == 0) return(empty)
  persian_aggregate_font_phase(reading, value_col = "wpm", use_geomean = TRUE)
}

plot_reading_phase_font_violins <- function(data_list,
                                            summary_list = NULL,
                                            pretest = NULL,
                                            status = NULL,
                                            status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_reading_font_phase_data(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "Ordinary Reading Speed (word/min)",
    titles = c(
      pre = "Ordinary Reading Speed by Font — pre-training",
      training = "Ordinary Reading Speed by Font — during training",
      post = "Ordinary Reading Speed by Font — post-training"
    ),
    use_log_scale = TRUE,
    phases = c("pre", "training", "post")
  )
}

# Expand Q&A nicknames for one session (uses threshold helper when available).
persian_session_qa_rows <- function(df) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
  if (exists("session_question_answers", mode = "function")) {
    qa <- tryCatch(session_question_answers(df), error = function(e) NULL)
  } else {
    qa <- NULL
  }
  if (is.null(qa) || nrow(qa) == 0) return(NULL)

  resp <- qa$questionAndAnswerResponse
  if (exists("arabic_to_western", mode = "function")) {
    resp <- arabic_to_western(resp)
  }
  qa %>%
    dplyr::mutate(
      questionAndAnswerResponse = suppressWarnings(as.numeric(resp)),
      questionAndAnswerNickname = as.character(questionAndAnswerNickname),
      conditionName = if ("conditionName" %in% names(.)) {
        as.character(conditionName)
      } else {
        NA_character_
      }
    ) %>%
    dplyr::filter(is.finite(questionAndAnswerResponse))
}

# Per uploaded file × font: comfort / beauty / familiarity rating.
# rating_type: "CMFRT" | "BTY" | "FAMILIARITY"
extract_session_rating_by_font <- function(data_list,
                                           rating_type = c("CMFRT", "BTY", "FAMILIARITY"),
                                           summary_list = NULL) {
  rating_type <- match.arg(rating_type)
  empty <- tibble::tibble(
    session_index = integer(),
    participant = character(),
    session_date = as.Date(character()),
    font = character(),
    rating = numeric(),
    n_obs = integer()
  )
  if (length(data_list) == 0) return(empty)
  invisible(summary_list)

  purrr::map_dfr(seq_along(data_list), function(i) {
    df <- data_list[[i]]
    if (is.null(df) || !is.data.frame(df) || nrow(df) < 1) return(NULL)
    qa <- persian_session_qa_rows(df)
    if (is.null(qa) || nrow(qa) == 0) return(NULL)

    typed <- qa %>%
      dplyr::mutate(
        type = dplyr::case_when(
          substr(questionAndAnswerNickname, 1, 5) == "CMFRT" ~ "CMFRT",
          grepl("bty", tolower(questionAndAnswerNickname)) &
            toupper(questionAndAnswerNickname) != "BTYSFL" ~ "BTY",
          grepl("familiarity|^fmlrty$", tolower(questionAndAnswerNickname)) ~
            "FAMILIARITY",
          TRUE ~ ""
        )
      ) %>%
      dplyr::filter(type == !!rating_type)
    if (nrow(typed) == 0) return(NULL)

    if (rating_type == "CMFRT") {
      typed <- typed %>%
        dplyr::mutate(font = persian_map_rating_font(questionAndAnswerNickname))
    } else {
      # Beauty / familiarity: font from beauty-* conditionName.
      typed <- typed %>%
        dplyr::filter(grepl("^beauty-", conditionName, ignore.case = TRUE)) %>%
        dplyr::mutate(font = persian_map_rating_font(conditionName))
    }
    if (nrow(typed) == 0) return(NULL)

    typed %>%
      dplyr::mutate(font = trimws(as.character(font))) %>%
      dplyr::filter(!is.na(font), font != "") %>%
      dplyr::group_by(font) %>%
      dplyr::summarize(
        n_obs = dplyr::n(),
        rating = mean(questionAndAnswerResponse, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        session_index = as.integer(i),
        participant = persian_first_nonempty_chr(df$participant),
        session_date = persian_session_date_from_df(df)
      ) %>%
      dplyr::filter(is.finite(rating), rating > 0) %>%
      dplyr::select(
        session_index, participant, session_date, font, rating, n_obs
      )
  }) %>%
    {
      if (nrow(.) == 0) empty else .
    }
}

prepare_persian_rating_font_phase_data <- function(data_list,
                                                  rating_type = c("CMFRT", "BTY", "FAMILIARITY"),
                                                  summary_list = NULL,
                                                  pretest = NULL,
                                                  status = NULL,
                                                  status_path = default_persian_status_path()) {
  rating_type <- match.arg(rating_type)
  empty <- tibble::tibble(
    participant = character(),
    font = character(),
    phase = character(),
    y = numeric()
  )
  ratings <- extract_session_rating_by_font(
    data_list,
    rating_type = rating_type,
    summary_list = summary_list
  )
  ratings <- annotate_persian_rsvp_phases(
    ratings,
    data_list,
    pretest = pretest,
    status = status,
    status_path = status_path,
    value_col = "rating"
  )
  if (nrow(ratings) == 0) return(empty)
  persian_aggregate_font_phase(
    ratings, value_col = "rating", use_geomean = FALSE
  )
}

plot_comfort_phase_font_violins <- function(data_list,
                                            summary_list = NULL,
                                            pretest = NULL,
                                            status = NULL,
                                            status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_rating_font_phase_data(
    data_list,
    rating_type = "CMFRT",
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "Comfort Rating",
    titles = c(
      pre = "Comfort Rating by Font — pre-training",
      training = "Comfort Rating by Font — during training",
      post = "Comfort Rating by Font — post-training"
    ),
    use_log_scale = FALSE,
    phases = c("pre", "training", "post")
  )
}

plot_beauty_phase_font_violins <- function(data_list,
                                           summary_list = NULL,
                                           pretest = NULL,
                                           status = NULL,
                                           status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_rating_font_phase_data(
    data_list,
    rating_type = "BTY",
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "Beauty Rating",
    titles = c(
      pre = "Beauty Rating by Font — pre-training",
      training = "Beauty Rating by Font — during training",
      post = "Beauty Rating by Font — post-training"
    ),
    use_log_scale = FALSE,
    phases = c("pre", "training", "post")
  )
}

plot_familiarity_phase_font_violins <- function(data_list,
                                                summary_list = NULL,
                                                pretest = NULL,
                                                status = NULL,
                                                status_path = default_persian_status_path()) {
  phase_df <- prepare_persian_rating_font_phase_data(
    data_list,
    rating_type = "FAMILIARITY",
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  plot_persian_phase_font_violins(
    phase_df,
    ylabel = "Familiarity Rating",
    titles = c(
      pre = "Familiarity Rating by Font — pre-training",
      training = "Familiarity Rating by Font — during training",
      post = "Familiarity Rating by Font — post-training"
    ),
    use_log_scale = FALSE,
    phases = c("pre", "training", "post")
  )
}
