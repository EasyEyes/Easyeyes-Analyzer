library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)

# Function to calculate geometric mean and standard error
geometric_mean_se <- function(x) {
  x <- x[!is.na(x) & x > 0]
  
  if (length(x) == 0) {
    return(c(mean = NA, se_lower = NA, se_upper = NA))
  }
  
  # CRITICAL FIX: Handle single data points properly
  if (length(x) == 1) {
    # For single data point, mean is the value itself, SE is 0
    geom_mean <- x[1]
    return(c(mean = geom_mean, se_lower = 0, se_upper = 0))
  }
  
  # For multiple data points, calculate geometric mean and SE normally
  log_x <- log(x)
  log_mean <- mean(log_x)
  log_se <- sd(log_x) / sqrt(length(log_x))
  geom_mean <- exp(log_mean)
  se_lower <- geom_mean - exp(log_mean - log_se)
  se_upper <- exp(log_mean + log_se) - geom_mean
  
  c(mean = geom_mean, se_lower = se_lower, se_upper = se_upper)
}

# Function to calculate ordinary mean and standard error
ordinary_mean_se <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(c(mean = NA, se = NA))
  c(mean = mean(x), se = sd(x) / sqrt(length(x)))
}

# Main function to create font comparison plots (fixed)
plot_font_comparison <- function(df_list) {
  # Extract data for each measure
  crowding_data <- df_list$crowding %>%
    mutate(measure = 10^log_crowding_distance_deg) %>%
    select(participant, font, measure)
  
  rsvp_data <- df_list$rsvp %>%
    mutate(measure = 10^block_avg_log_WPM) %>%
    select(participant, font, measure)
  
  reading_data <- df_list$reading %>%
    mutate(measure =  10^log_WPM) %>%
    select(participant, font, measure)
  
  acuity_data <- df_list$acuity %>%
    mutate(measure = 10^questMeanAtEndOfTrialsLoop) %>%
    select(participant, font, measure)
  
  comfort_data <- df_list$comfort %>%
    mutate(measure = questionAndAnswerResponse) %>%
    filter(!is.na(measure)) %>%
    select(participant, font, measure)
  
  beauty_data <- df_list$beauty %>%
    mutate(measure = questionAndAnswerResponse) %>%
    filter(!is.na(measure)) %>%
    select(participant, font, measure)
  
  familiarity_data <- df_list$familiarity %>%
    mutate(measure = questionAndAnswerResponse) %>%
    filter(!is.na(measure)) %>%
    select(participant, font, measure)

  # ---- helper: dynamic y-limits from mean ± SE ----
  compute_y_limits <- function(df_summary, use_log) {
    if (nrow(df_summary) == 0) return(NULL)
    low  <- pmax(df_summary$mean_val - df_summary$se_lower, if (use_log) 1e-4 else -Inf)
    high <- df_summary$mean_val + df_summary$se_upper
    rng  <- range(c(low, high), na.rm = TRUE)
    if (use_log) {
      # multiplicative padding; keep > 0
      lo <- max(rng[1] / 1.25, 1e-4)
      hi <- rng[2] * 1.25
    } else {
      pad <- diff(rng) * 0.10
      if (!is.finite(pad) || pad == 0) pad <- max(1, rng[2] * 0.10)
      lo <- max(0, rng[1] - pad)   # clamp to 0 for linear scales
      hi <- rng[2] + pad
    }
    c(lo, hi)
  }

  # ---- helper: one plot ----
  create_font_plot <- function(data, title, ylabel,
                               use_log_scale = TRUE,
                               use_geometric_mean = TRUE) {
    if (nrow(data) == 0) return(NULL)
    
    # summary stats
    if (use_geometric_mean) {
      summary_data <- data %>%
        group_by(font) %>%
        summarise(stats = list(geometric_mean_se(measure)), .groups = "drop") %>%
        mutate(
          mean_val = map_dbl(stats, ~ .x["mean"]),
          se_lower = map_dbl(stats, ~ .x["se_lower"]),
          se_upper = map_dbl(stats, ~ .x["se_upper"])
        ) %>%
        select(-stats)
    } else {
      summary_data <- data %>%
        group_by(font) %>%
        summarise(stats = list(ordinary_mean_se(measure)), .groups = "drop") %>%
        mutate(
          mean_val = map_dbl(stats, ~ .x["mean"]),
          se_val   = map_dbl(stats, ~ .x["se"]),
          se_lower = se_val,
          se_upper = se_val
        ) %>%
        select(-stats, -se_val)
    }
    
    summary_data <- summary_data %>%
      filter(!is.na(mean_val), !is.na(se_lower), !is.na(se_upper), mean_val > 0)

    if (nrow(summary_data) == 0) return(ggplot())

    # add participant counts
    participant_counts <- data %>%
      group_by(font) %>%
      summarise(n_participants = n_distinct(participant), .groups = "drop")
    summary_data <- left_join(summary_data, participant_counts, by = "font")

         # ---- alphabetical font order ----
     font_levels <- summary_data %>% arrange(font) %>% pull(font) %>% as.character()
     summary_data$font <- factor(summary_data$font, levels = font_levels)

    # colors (assume you have font_color_palette; fallback to rainbow)
    font_colors <- if (exists("font_color_palette")) {
      font_color_palette(levels(summary_data$font))
    } else {
      cols <- grDevices::rainbow(length(levels(summary_data$font)))
      names(cols) <- levels(summary_data$font); cols
    }

    # ---- dynamic y-limits ----
    y_limits <- compute_y_limits(summary_data, use_log = (use_log_scale && use_geometric_mean))
    baseline <- y_limits[1]

    # label position near bottom
    label_y <- if (use_log_scale && use_geometric_mean) baseline * 1.05 else baseline + diff(y_limits) * 0.05

    # plot
    p <- ggplot(summary_data, aes(x = font, fill = font)) +
      # bars drawn with explicit baseline to support log scale
      geom_rect(
        aes(
          xmin = as.numeric(font) - 0.35,
          xmax = as.numeric(font) + 0.35,
          ymin = baseline,
          ymax = mean_val
        ),
        alpha = 0.85,
        color = NA
      ) +
      geom_errorbar(
        aes(ymin = pmax(mean_val - se_lower, if (use_log_scale && use_geometric_mean) 1e-4 else -Inf),
            ymax = mean_val + se_upper),
        width = 0.18, size = 0.5, color = "black"
      ) +
      geom_text(
        aes(label = paste0("N=", n_participants), y = label_y),
        vjust = 0.5, size = 3.6, color = "black", fontface = "bold"
      ) +
      scale_fill_manual(values = font_colors, guide = "none") +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 9, color = "black", angle = 45, hjust = 1, vjust = 1),
        axis.ticks.x = element_line(color = "black"),
        axis.text.y = element_text(size = 10, color = "black"),
        axis.title.x = element_text(size = 12, margin = margin(t = 10), color = "black"),
        axis.title.y = element_text(size = 12, margin = margin(r = 10), color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", size = 0.3),
        panel.grid.minor.y = element_blank(),
        axis.line = element_line(color = "black", size = 0.5)
      ) +
      labs(subtitle = title, x = "Fonts", y = ylabel)

    if (use_log_scale && use_geometric_mean) {
      p <- p + scale_y_log10(limits = y_limits, expand = c(0, 0)) +
        annotation_logticks(sides = "l",
                            short = unit(2, "pt"),
                            mid   = unit(2, "pt"),
                            long  = unit(7, "pt"))
    } else {
      p <- p + scale_y_continuous(limits = y_limits, expand = c(0, 0))
    }
    p
  }

  # Build plots (ylims now dynamic—don’t pass them)
  plots <- list(
    rsvp    = create_font_plot(rsvp_data,    "RSVP",   "RSVP Reading Speed (WPM)",            use_log_scale = TRUE,  use_geometric_mean = TRUE),
    crowding= create_font_plot(crowding_data,"Crowding","Crowding Distance (deg)",            use_log_scale = TRUE,  use_geometric_mean = TRUE),
    reading = create_font_plot(reading_data, "Reading","Ordinary Reading Speed (WPM)",        use_log_scale = TRUE,  use_geometric_mean = TRUE),
    acuity  = create_font_plot(acuity_data,  "Acuity", "Acuity Threshold (deg)",              use_log_scale = TRUE,  use_geometric_mean = TRUE),
    comfort = create_font_plot(comfort_data, "Comfort","Comfort Rating",                      use_log_scale = FALSE, use_geometric_mean = FALSE),
    beauty  = create_font_plot(beauty_data,  "Beauty", "Beauty Rating",                       use_log_scale = FALSE, use_geometric_mean = FALSE),
    familiarity  = create_font_plot(familiarity_data,  "Familiarity", "Familiarity",          use_log_scale = FALSE, use_geometric_mean = FALSE)
  )

  return(plots)
}
