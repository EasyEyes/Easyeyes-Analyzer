# Utilities for the Languages tab.
#
# `_language` is a column in the results CSVs, not a filename suffix.
# Keep a compact session lookup because threshold extraction drops that column.

# Language codes supported on the Languages tab.
SUPPORTED_LANGUAGES <- c("ar", "fa", "ur")

# Human-readable name for each supported language.
LANGUAGE_LABELS <- c(
  ar = "Arabic",
  fa = "Persian",
  ur = "Urdu"
)

# Consistent color for each language (used across every plot on the tab).
LANGUAGE_COLORS <- c(
  ar = "#E41A1C",  # red
  fa = "#377EB8",  # blue
  ur = "#4DAF4A"   # green
)

detect_language <- function(language) {
  code <- tolower(trimws(as.character(language)))
  ifelse(code %in% SUPPORTED_LANGUAGES, code, NA_character_)
}

language_session_metadata <- function(data_list) {
  empty <- tibble::tibble(experiment = character(), participant = character(),
                         language = character(), language_participant = character())
  chunks <- lapply(data_list, function(df) {
    if (!is.data.frame(df) || nrow(df) == 0 ||
        !all(c("experiment", "participant") %in% names(df))) return(empty)
    # Do not substitute fontLanguage, deviceLanguage or instructionLanguage.
    codes <- unique(detect_language(df[["_language"]]))
    codes <- codes[!is.na(codes)]
    code <- if (length(codes) == 1L) codes else NA_character_
    identities <- function(column) {
      x <- trimws(as.character(df[[column]]))
      unique(x[!is.na(x) & nzchar(x) & !tolower(x) %in% c("na", "null", "undefined")])
    }
    id <- identities("ProlificParticipantID")
    prefix <- "prolific:"
    if (length(id) == 0L) {
      id <- identities("ParticipantCode")
      prefix <- "code:"
    }
    df %>%
      dplyr::distinct(experiment, participant) %>%
      dplyr::mutate(
        experiment = as.character(experiment), participant = as.character(participant),
        language = code,
        language_participant = if (length(id) == 1L) paste0(prefix, id) else
          paste("session", experiment, participant, sep = ":")
      )
  })
  metadata <- dplyr::bind_rows(empty, chunks)
  # A lookup must never multiply plotted rows. Ambiguous sessions stay unknown.
  metadata %>%
    dplyr::group_by(experiment, participant) %>%
    dplyr::summarize(
      language = {
        codes <- unique(stats::na.omit(language))
        if (length(codes) == 1L) codes else NA_character_
      },
      language_participant = dplyr::first(language_participant),
      .groups = "drop"
    )
}

add_language_column <- function(df, metadata = NULL) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(df)
  }
  if (!is.null(metadata) && all(c("experiment", "participant") %in% names(df))) {
    df <- df %>%
      dplyr::select(-dplyr::any_of(c("language", "language_participant"))) %>%
      dplyr::mutate(experiment = as.character(experiment), participant = as.character(participant)) %>%
      dplyr::left_join(metadata, by = c("experiment", "participant"), relationship = "many-to-one")
  } else {
    df$language <- if ("_language" %in% names(df)) detect_language(df[["_language"]]) else
      if ("language" %in% names(df)) detect_language(df$language) else NA_character_
  }
  df
}

language_measurement_data <- function(df) {
  df <- add_language_column(df)
  if (!is.null(df) && "language_participant" %in% names(df)) {
    df$participant <- df$language_participant
  }
  df
}

# Build legend labels of the form "ar (Arabic), Average N=4.67".
# Counts at individual fonts are integers, but their average can be fractional.
# `n_by_language` is a named numeric vector keyed by language code.
language_legend_labels <- function(n_by_language) {
  vapply(SUPPORTED_LANGUAGES, function(code) {
    label <- LANGUAGE_LABELS[[code]]
    n_val <- if (!is.null(n_by_language) && code %in% names(n_by_language)) {
      n_by_language[[code]]
    } else {
      NA_real_
    }
    if (is.null(n_val) || is.na(n_val)) {
      paste0(code, " (", label, ")")
    } else {
      paste0(code, " (", label, "), Average N=",
             format(round(n_val, 2), trim = TRUE, scientific = FALSE))
    }
  }, character(1), USE.NAMES = FALSE)
}

# Short labels for the four comparison fonts; preserve all other font names.
language_font_labels <- function(fonts) {
  labels <- c("BadeenDisplay-Regular.ttf" = "Badeen", "Kufi LT Regular.woff2" = "Kufi",
              "NotoNaskhArabic-Regular.ttf" = "Naskh", "NotoNastaliqUrdu-Regular.woff2" = "Nastaliq")
  result <- unname(labels[as.character(fonts)])
  result[is.na(result)] <- as.character(fonts)[is.na(result)]
  result
}
