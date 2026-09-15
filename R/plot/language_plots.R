# Plots for the "Languages" tab.
#
# Each plot shows one line per supported language (ar / fa / ur), plotted
# against font (x-axis), with error bars over participants and a legend that
# shows the language name and its average participant count across plotted fonts.

# ---------------------------------------------------------------------------
# Data preparation: reduce each measurement to one (language, font,
# participant, value) triple. Aggregation across participants happens later
# inside `plot_language_by_font()`.
# ---------------------------------------------------------------------------

# One row per (language, font, participant) with the geometric mean of the
# participant's reading speeds (word/min).
prepare_language_reading_speed <- function(df_list) {
  reading <- language_measurement_data(df_list$reading)
  if (is.null(reading) || nrow(reading) == 0) return(NULL)
  reading %>%
    dplyr::filter(!is.na(language), !is.na(participant), participant != "",
                  !is.na(wordPerMin), is.finite(wordPerMin), wordPerMin > 0,
                  !is.na(font), font != "") %>%
    dplyr::group_by(language, font, participant) %>%
    dplyr::summarize(value = exp(mean(log(wordPerMin), na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::filter(is.finite(value), value > 0)
}

# One row per (language, font, participant) with the proportion of
# comprehension questions the participant answered correctly (0-1).
prepare_language_reading_proportion_correct <- function(df_list) {
  reading <- language_measurement_data(df_list$reading)
  if (is.null(reading) || nrow(reading) == 0) return(NULL)
  if (!"CQAccuracy" %in% names(reading)) return(NULL)
  reading %>%
    dplyr::filter(!is.na(language), !is.na(participant), participant != "",
                  !is.na(CQAccuracy), is.finite(CQAccuracy), CQAccuracy >= 0, CQAccuracy <= 100,
                  !is.na(font), font != "") %>%
    dplyr::group_by(language, font, participant) %>%
    dplyr::summarize(value = mean(CQAccuracy, na.rm = TRUE) / 100,
                     .groups = "drop") %>%
    dplyr::filter(is.finite(value))
}

# One row per (language, font, participant) with RSVP speed (word/min),
# computed by exponentiating the mean of the participant's log10 WPM.
prepare_language_rsvp_speed <- function(df_list) {
  rsvp <- language_measurement_data(df_list$rsvp)
  if (is.null(rsvp) || nrow(rsvp) == 0) return(NULL)
  rsvp %>%
    dplyr::filter(!is.na(language), !is.na(participant), participant != "",
                  !is.na(block_avg_log_WPM), is.finite(block_avg_log_WPM),
                  !is.na(font), font != "") %>%
    dplyr::group_by(language, font, participant) %>%
    dplyr::summarize(value = 10^(mean(block_avg_log_WPM, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::filter(is.finite(value), value > 0)
}

# One row per (language, font, participant) with crowding threshold in
# degrees (average across all crowding conditions for that participant/font).
prepare_language_crowding <- function(df_list) {
  crowding <- language_measurement_data(df_list$crowding)
  if (is.null(crowding) || nrow(crowding) == 0) return(NULL)
  crowding %>%
    dplyr::filter(!is.na(language), !is.na(participant), participant != "",
                  !is.na(log_crowding_distance_deg),
                  is.finite(log_crowding_distance_deg),
                  !is.na(font), font != "") %>%
    dplyr::group_by(language, font, participant) %>%
    dplyr::summarize(value = 10^(mean(log_crowding_distance_deg, na.rm = TRUE)),
                     .groups = "drop") %>%
    dplyr::filter(is.finite(value), value > 0)
}

# Shared helper for comfort/beauty/familiarity ratings. The three per-rating
# dataframes each store the participant's numeric response in
# `questionAndAnswerResponse` (already coerced to numeric upstream).
prepare_language_rating <- function(df_list, rating_slot) {
  ratings <- language_measurement_data(df_list[[rating_slot]])
  if (is.null(ratings) || nrow(ratings) == 0) return(NULL)
  ratings %>%
    dplyr::mutate(value = suppressWarnings(as.numeric(questionAndAnswerResponse))) %>%
    dplyr::filter(!is.na(language), !is.na(participant), participant != "",
                  !is.na(value), is.finite(value),
                  !is.na(font), font != "") %>%
    dplyr::group_by(language, font, participant) %>%
    dplyr::summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(is.finite(value))
}

# ---------------------------------------------------------------------------
# Plotting
# ---------------------------------------------------------------------------

# Aggregate per (language, font) and plot one line per language with error
# bars over participants.
#
# When log_y = TRUE we treat `value` as geometric: aggregation is in log10
# space, and error bars are ±1 SE in log space (asymmetric on the linear
# axis).
plot_language_by_font <- function(data,
                                  y_label,
                                  subtitle = NULL,
                                  log_y = FALSE,
                                  y_limits = NULL) {
  if (is.null(data) || nrow(data) == 0) return(NULL)

  # Aggregate over participants for each (language, font).
  if (log_y) {
    summ <- data %>%
      dplyr::mutate(logval = log10(value)) %>%
      dplyr::filter(is.finite(logval)) %>%
      dplyr::group_by(language, font) %>%
      dplyr::summarize(
        n = dplyr::n(),
        mean_log = mean(logval, na.rm = TRUE),
        se_log = if (dplyr::n() > 1) stats::sd(logval, na.rm = TRUE) / sqrt(dplyr::n()) else NA_real_,
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        mean = 10^mean_log,
        lower = 10^(mean_log - se_log),
        upper = 10^(mean_log + se_log)
      )
  } else {
    summ <- data %>%
      dplyr::filter(is.finite(value)) %>%
      dplyr::group_by(language, font) %>%
      dplyr::summarize(
        n = dplyr::n(),
        mean = mean(value, na.rm = TRUE),
        se = if (dplyr::n() > 1) stats::sd(value, na.rm = TRUE) / sqrt(dplyr::n()) else NA_real_,
        .groups = "drop"
      ) %>%
      dplyr::mutate(
        lower = mean - se,
        upper = mean + se
      )
  }

  if (nrow(summ) == 0) return(NULL)

  # Each point's n counts unique participants, after within-participant averaging.
  # Average those counts over the fonts shown. A missing language/font cell
  # contributes zero; a font absent for every language is not shown or averaged.
  average_n <- summ %>%
    dplyr::group_by(language) %>%
    dplyr::summarize(n = sum(n) / dplyr::n_distinct(summ$font), .groups = "drop")
  n_by_language <- stats::setNames(rep(0, length(SUPPORTED_LANGUAGES)), SUPPORTED_LANGUAGES)
  n_by_language[average_n$language] <- average_n$n

  # Full legend labels for every supported language (missing languages still
  # appear so the color mapping stays consistent across plots).
  legend_labels <- language_legend_labels(n_by_language)

  # Deterministic font order so that all plots share a common x-axis.
  summ$font <- factor(summ$font, levels = sort(unique(summ$font)))

  # Language ordering is fixed by SUPPORTED_LANGUAGES so colours stay stable.
  summ$language <- factor(summ$language, levels = SUPPORTED_LANGUAGES)

  p <- ggplot2::ggplot(summ,
                       ggplot2::aes(x = font, y = mean,
                                    color = language, group = language)) +
    ggplot2::geom_line(linewidth = 0.8) +
    ggplot2::geom_point(size = 3) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = lower, ymax = upper),
                           width = 0.2, linewidth = 0.6, na.rm = TRUE) +
    ggplot2::scale_color_manual(
      values = LANGUAGE_COLORS,
      breaks = SUPPORTED_LANGUAGES,
      limits = SUPPORTED_LANGUAGES,
      labels = legend_labels,
      drop = FALSE,
      name = NULL
    ) +
    ggplot2::labs(x = "Fonts", y = y_label, subtitle = subtitle,
                  caption = "Error bars: ±1 SE across participants (per-font N ≥ 2).\nAverage N: mean participant count across the fonts shown, after filtering.") +
    ggplot2::guides(color = ggplot2::guide_legend(ncol = 1)) +
    ggplot2::scale_x_discrete(labels = language_font_labels) +
    ggplot2::theme_classic() +
    plt_theme +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 1)
    )

  if (log_y) {
    y_breaks <- scales::log_breaks()(c(min(c(summ$mean, summ$lower), na.rm = TRUE),
                                       max(c(summ$mean, summ$upper), na.rm = TRUE)))
    p <- p +
      ggplot2::scale_y_log10(breaks = y_breaks,
                             labels = function(x) format(x, nsmall = 0,
                                                         scientific = FALSE)) +
      ggplot2::annotation_logticks(sides = "l",
                                   short = ggplot2::unit(2, "pt"),
                                   mid = ggplot2::unit(2, "pt"),
                                   long = ggplot2::unit(7, "pt"))
  }
  if (!is.null(y_limits)) {
    p <- p + ggplot2::coord_cartesian(ylim = y_limits)
  }
  p
}

# ---------------------------------------------------------------------------
# Convenience wrappers, one per plot on the Languages tab.
# ---------------------------------------------------------------------------

plot_language_reading_speed <- function(df_list) {
  data <- prepare_language_reading_speed(df_list)
  plot_language_by_font(
    data,
    y_label = "Ordinary Reading Speed (WPM)",
    subtitle = "Reading",
    log_y = TRUE
  )
}

plot_language_reading_proportion_correct <- function(df_list) {
  data <- prepare_language_reading_proportion_correct(df_list)
  plot_language_by_font(
    data,
    y_label = "Proportion Correct",
    subtitle = "Reading proportion correct",
    log_y = FALSE,
    y_limits = c(0, 1)
  )
}

plot_language_rsvp_speed <- function(df_list) {
  data <- prepare_language_rsvp_speed(df_list)
  plot_language_by_font(
    data,
    y_label = "RSVP Reading Speed (WPM)",
    subtitle = "RSVP",
    log_y = TRUE
  )
}

plot_language_crowding <- function(df_list) {
  data <- prepare_language_crowding(df_list)
  plot_language_by_font(
    data,
    y_label = "Crowding Distance (deg)",
    subtitle = "Crowding",
    log_y = TRUE
  )
}

plot_language_comfort <- function(df_list) {
  data <- prepare_language_rating(df_list, "comfort")
  plot_language_by_font(
    data,
    y_label = "Comfort Rating",
    subtitle = "Comfort",
    log_y = FALSE
  )
}

plot_language_beauty <- function(df_list) {
  data <- prepare_language_rating(df_list, "beauty")
  plot_language_by_font(
    data,
    y_label = "Beauty Rating",
    subtitle = "Beauty",
    log_y = FALSE
  )
}

plot_language_familiarity <- function(df_list) {
  data <- prepare_language_rating(df_list, "familiarity")
  plot_language_by_font(
    data,
    y_label = "Familiarity",
    subtitle = "Familiarity",
    log_y = FALSE
  )
}
