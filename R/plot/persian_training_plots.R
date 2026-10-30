# Persian study helpers: participant status filter + longitudinal RSVP plot.
# Used by the Plots-tab scatter section (rsvp-training-sessions).

default_persian_status_path <- function() {
  file.path("data", "Persian_Study_Participant_Status.xlsx")
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

    pav <- persian_first_nonempty_chr(df$participant)
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

    sm <- NULL
    if (!is.null(summary_list) && length(summary_list) >= i &&
          is.data.frame(summary_list[[i]]) && nrow(summary_list[[i]]) > 0) {
      sm <- summary_list[[i]]
    }
    src <- if (!is.null(sm)) sm else df
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

    # Same transform as generate_threshold: log_duration_s → log10 WPM.
    wpm <- 10^(log10(60) - rsvp$questMeanAtEndOfTrialsLoop)
    wpm <- wpm[is.finite(wpm) & wpm > 0]
    if (length(wpm) == 0) return(NULL)

    tibble::tibble(
      session_index = as.integer(i),
      participant = pav,
      session_date = session_dt,
      wpm = exp(mean(log(wpm))),
      n_conditions = length(wpm)
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
      # Pre = before first training day; post = first training day and after.
      phase = dplyr::case_when(
        is.na(training_start) ~ "unknown",
        session_date < training_start ~ "pre",
        TRUE ~ "post"
      )
    ) %>%
    dplyr::select(ParticipantCode, session_date, wpm, phase)
}

# Sessions numbered by date within a phase filter (or all phases).
# `participant_colors` / `y_limits` keep related plots comparable.
plot_rsvp_phase_sessions <- function(session_df,
                                     phase = c("pre", "post", "all"),
                                     participant_colors = NULL,
                                     y_limits = NULL,
                                     title = NULL) {
  phase <- match.arg(phase)
  if (is.null(session_df) || nrow(session_df) == 0) return(NULL)

  plot_df <- session_df
  if (!identical(phase, "all")) {
    plot_df <- plot_df %>% dplyr::filter(phase == !!phase)
  }
  plot_df <- plot_df %>%
    dplyr::arrange(ParticipantCode, session_date) %>%
    dplyr::group_by(ParticipantCode) %>%
    dplyr::mutate(session_number = dplyr::row_number()) %>%
    dplyr::ungroup()

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
      post = "RSVP reading speed — post-training",
      all = "RSVP reading speed across sessions",
      "RSVP reading speed across sessions"
    )
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
    ggplot2::geom_line(linewidth = 0.7, alpha = 0.85) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_x_continuous(
      breaks = seq_len(as.integer(max_sess)),
      limits = c(1, max_sess),
      name = "Session number (by date)"
    ) +
    ggplot2::scale_color_manual(
      values = participant_colors,
      breaks = names(participant_colors),
      drop = FALSE,
      name = "Participant"
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

  if (!is.null(y_limits) && length(y_limits) == 2 && all(is.finite(y_limits))) {
    p <- p + ggplot2::scale_y_log10(limits = y_limits)
  } else {
    p <- p + ggplot2::scale_y_log10()
  }
  p
}

# Pre-training and post-training RSVP plots (shared participant colors + y-scale).
# Pre = before First Training Session; post = on/after that day (includes
# training-window days and anything after Last Training Session).
# Returns list(pre=, post=); each ggplot or NULL.
plot_rsvp_pre_post_sessions <- function(data_list,
                                        summary_list = NULL,
                                        pretest = NULL,
                                        status = NULL,
                                        status_path = default_persian_status_path()) {
  session_df <- prepare_persian_rsvp_session_data(
    data_list,
    summary_list = summary_list,
    pretest = pretest,
    status = status,
    status_path = status_path
  )
  if (nrow(session_df) == 0) {
    return(list(pre = NULL, post = NULL))
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
    post = plot_rsvp_phase_sessions(
      session_df,
      phase = "post",
      participant_colors = colors,
      y_limits = y_limits
    )
  )
}

# Backward-compatible alias: post-training longitudinal plot.
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
  )$post
}
