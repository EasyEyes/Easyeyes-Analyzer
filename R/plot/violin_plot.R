# Helper function for logarithmic jitter (unbiased for log scales)
# NOTE: geom_jitter in violin plots uses mainly x-axis jitter, but when
# y-axis uses log scale, any y-jitter should ideally be logarithmic
add_log_jitter <- function(values, jitter_percent = 1, seed = 42) {
  # Apply logarithmic jitter for unbiased results on log scales
  # jitter_percent: percentage jitter (e.g., 1 for ±1%)
  set.seed(seed)
  log_max <- log10(1 + jitter_percent/100)
  log_min <- -log_max
  log_factor <- log_min + runif(length(values)) * (log_max - log_min)
  return(values * 10^log_factor)
}

plot_violins <- function(df_list) {
  crowding = df_list$crowding %>% mutate(y = 10^log_crowding_distance_deg)
  rsvp = df_list$rsvp %>% mutate(y = 10^block_avg_log_WPM)
  reading = df_list$reading %>%
    mutate(log_WPM = suppressWarnings(as.numeric(log_WPM))) %>%
    filter(!is.na(log_WPM), is.finite(log_WPM)) %>%
    mutate(y = 10^log_WPM)
  
  # Linear acuity (deg), same transform as font-comparison bar plot
  acuity = df_list$acuity %>% mutate(y = 10^questMeanAtEndOfTrialsLoop)
  beauty = df_list$beauty %>%
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  comfort = df_list$comfort %>% 
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  familiarity_data = df_list$familiarity %>% 
    mutate(y = questionAndAnswerResponse) %>%
    filter(!is.na(y))
  
  create_plot <- function(data, ylabel, title, xlimits = NULL,
                          abbreviate_fonts = FALSE,
                          use_log_scale = FALSE,
                          axis_label_scale = 1,
                          color_by_phrase_group = FALSE,
                          group_by = c("font", "participant")) {
    group_by <- match.arg(group_by)
    p <- NULL
    
    if (nrow(data) > 0) {

      # Define font order
      font_order <- c(
        "Al-Awwal-Regular.ttf",
        "majalla.ttf",
        "Saudi-Regular.ttf",
        "SaudiTextv1-Regular.otf",
        "SaudiTextv2-Regular.otf",
        "SaudiTextv3-Regular.otf"
      )

      if (group_by == "participant") {
        # One violin per participant; N = number of thresholds for that person.
        # Order left→right by ascending mean so the series rises across the plot.
        group_labels <- data %>%
          filter(is.finite(y)) %>%
          group_by(participant) %>%
          summarise(
            n_points = n(),
            mean_y = mean(y, na.rm = TRUE),
            .groups = "drop"
          ) %>%
          arrange(mean_y, participant) %>%
          mutate(label = paste0(participant, " (N=", n_points, ")"))

        plot_data <- data %>%
          left_join(group_labels, by = "participant") %>%
          mutate(group_label = factor(label, levels = group_labels$label)) %>%
          filter(is.finite(y))
        x_lab <- "Participant"
        x_angle <- 90
        n_groups <- nrow(group_labels)
      } else {
        # Calculate participant count by font
        participant_counts <- data %>%
          group_by(font) %>%
          summarise(n_participants = n_distinct(participant), .groups = "drop")

        # Create labels with N counts for each font and apply ordering
        group_labels <- participant_counts %>%
          mutate(
            font_display = if (abbreviate_fonts) {
              font_comparison_axis_label(font)
            } else {
              font
            },
            label = paste0(font_display, " (N=", n_participants, ")"),
            font_factor = factor(font, levels = font_order)
          ) %>%
          arrange(font_factor)

        plot_data <- data %>%
          left_join(group_labels, by = "font") %>%
          mutate(group_label = factor(label, levels = group_labels$label)) %>%
          filter(is.finite(y))  # Remove -Inf, Inf, NA values for plotting
        x_lab <- "Font"
        x_angle <- 45
        n_groups <- nrow(group_labels)
      }

      if (use_log_scale) {
        plot_data <- plot_data %>% filter(y > 0)
      }
      
      # Apply x-axis limits if specified (filter data to limits)
      if (!is.null(xlimits)) {
        plot_data <- plot_data %>%
          filter(y >= xlimits[1] & y <= xlimits[2])
      }

      if (nrow(plot_data) == 0) {
        return(NULL)
      }
      
      # Mean line per group (font or participant)
      mean_data <- plot_data %>%
        group_by(group_label) %>%
        summarise(mean_y = mean(y, na.rm = TRUE), .groups = "drop")

      axis_title_size <- 14 * axis_label_scale

      if (color_by_phrase_group) {
        if (!"phrasesColumnName" %in% names(plot_data)) {
          plot_data$phrasesColumnName <- NA_character_
        }
        plot_data <- plot_data %>%
          mutate(
            phrase_group = ifelse(
              is.na(phrasesColumnName) | phrasesColumnName == "",
              "unknown",
              as.character(phrasesColumnName)
            )
          )
        group_levels <- sort(unique(plot_data$phrase_group))
        # Prefer A'..H' order when those labels are present
        preferred <- paste0(LETTERS[1:8], "'")
        group_levels <- c(
          intersect(preferred, group_levels),
          setdiff(group_levels, preferred)
        )
        plot_data$phrase_group <- factor(plot_data$phrase_group, levels = group_levels)

        p <- ggplot(plot_data, aes(x = group_label, y = y)) +
          geom_violin(trim = FALSE, alpha = 0.35, fill = "grey80", color = "grey40") +
          geom_jitter(
            aes(color = phrase_group),
            width = 0.15,
            alpha = 0.85,
            size = 2
          ) +
          geom_segment(
            data = mean_data,
            aes(
              x = as.numeric(group_label) - 0.4,
              xend = as.numeric(group_label) + 0.4,
              y = mean_y,
              yend = mean_y
            ),
            color = "red",
            linewidth = 1,
            alpha = 0.8,
            inherit.aes = FALSE
          ) +
          scale_color_manual(
            name = "_phrasesColumnName",
            values = setNames(
              rep(colorPalette, length.out = length(group_levels)),
              group_levels
            )
          ) +
          theme_minimal(base_size = 14) +
          theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            axis.title.x = element_text(size = axis_title_size),
            axis.title.y = element_text(size = axis_title_size),
            axis.text.x = element_text(angle = x_angle, hjust = 1, vjust = 1),
            legend.position = "right",
            plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
          ) +
          labs(
            subtitle = title,
            x = x_lab,
            y = ylabel
          )
      } else {
        p <- ggplot(plot_data, aes(x = group_label, y = y)) +
          geom_violin(trim = FALSE, alpha = 0.5) +
          geom_jitter(width = 0.15, alpha = 0.7) +
          geom_segment(data = mean_data, 
                       aes(x = as.numeric(group_label) - 0.4, 
                           xend = as.numeric(group_label) + 0.4,
                           y = mean_y, 
                           yend = mean_y),
                       color = "red", linewidth = 1, alpha = 0.8) +
          theme_minimal(base_size = 14) +
          theme(
            plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            axis.title.x = element_text(size = axis_title_size),
            axis.title.y = element_text(size = axis_title_size),
            axis.text.x = element_text(angle = x_angle, hjust = 1, vjust = 1),
            plot.margin = margin(t = 5.5, r = 5.5, b = 18, l = 5.5)
          ) +
          labs(
            subtitle = title,
            x = x_lab,
            y = ylabel
          )
      }

      # Participant plots: keep subtitle over the panel (not the y-axis title)
      # so "Crowding Distance (deg)" does not collide with the plot title, and
      # lengthen only the x-axis with N (same point/font scale as other violins).
      if (identical(group_by, "participant")) {
        # ~0.18 in per participant — enough for vertical IDs without fat violins.
        width_in <- max(7, min(0.18 * n_groups + 2.0, 48))
        x_text_size <- if (n_groups > 40) 8 else if (n_groups > 25) 9 else 10
        p <- p + theme(
          plot.title.position = "panel",
          plot.subtitle = element_text(margin = margin(b = 8)),
          axis.title.y = element_text(
            size = axis_title_size,
            margin = margin(r = 6)
          ),
          axis.text.x = element_text(
            angle = x_angle,
            hjust = 1,
            vjust = 0.5,
            size = x_text_size
          ),
          # Room for long rotated participant IDs.
          plot.margin = margin(t = 10, r = 8, b = 28, l = 8)
        )
        attr(p, "plots_display_width_in") <- width_in
        # Same height as other violins so vertical scale matches.
        attr(p, "plots_display_height_in") <- 6
        # Full Plots-tab row + horizontal scroll (see flexible_plot_rows).
        attr(p, "plots_full_row") <- TRUE
      }
      
      # Log scale: linear tick labels with log spacing (matches font-comparison bars)
      if (use_log_scale || grepl("Reading|RSVP|Crowding", title)) {
        p <- p + scale_y_log10(breaks = scales::log_breaks()) +
          annotation_logticks(sides = "l",
                      short = unit(2, "pt"),
                      mid   = unit(2, "pt"),
                      long  = unit(7, "pt"))
      } else {
        # For non-log scale plots, add standard tick marks
        p <- p + theme(
          axis.ticks.x = element_line(color = "black", size = 0.5),
          axis.ticks.y = element_line(color = "black", size = 0.5),
          axis.ticks.length = unit(4, "pt")
        )
      }
    }
    return(p)
  }
  
  # Calculate dynamic limits based on actual data
  reading_limits <- if (nrow(reading) > 0 && !all(is.na(reading$y))) {
    c(min(reading$y, na.rm = TRUE) * 0.8, max(reading$y, na.rm = TRUE) * 1.2)
  } else {
    NULL  # Let ggplot auto-scale
  }
  
  rsvp_limits <- if (nrow(rsvp) > 0 && !all(is.na(rsvp$y))) {
    c(min(rsvp$y, na.rm = TRUE) * 0.8, max(rsvp$y, na.rm = TRUE) * 1.2)
  } else {
    NULL  # Let ggplot auto-scale
  }

  return(list(
    reading = create_plot(reading, "Reading Speed (word/min)", "Reading Speed by Font"),
    rsvp = create_plot(rsvp, "RSVP Reading Speed (word/min)", "RSVP Reading Speed by Font"),
    crowding = create_plot(crowding, "Crowding Distance (deg)", "Crowding Threshold by Font"),
    crowding_by_participant = create_plot(
      crowding,
      "Crowding Distance (deg)",
      "Crowding Threshold by Participant",
      group_by = "participant"
    ),
    acuity = create_plot(acuity, "Acuity (deg)", "Acuity vs. font",
                         abbreviate_fonts = TRUE,
                         use_log_scale = TRUE,
                         axis_label_scale = 1.4),
    acuity_by_participant = create_plot(
      acuity,
      "Acuity (deg)",
      "Acuity by Participant",
      use_log_scale = TRUE,
      axis_label_scale = 1.4,
      group_by = "participant"
    ),
    acuity_by_phrase_group = create_plot(
      acuity,
      "Acuity (deg)",
      "Acuity vs. font (colored by phrase group)",
      abbreviate_fonts = TRUE,
      use_log_scale = TRUE,
      axis_label_scale = 1.4,
      color_by_phrase_group = TRUE
    ),
    beauty = create_plot(beauty, "Beauty Rating", "Beauty Rating by Font"),
    cmfrt = create_plot(comfort, "Comfort Rating", "Comfort Rating by Font"),
    familiarity = create_plot(familiarity_data, "familiarity", "Familiarity by Font")
  ))
}
