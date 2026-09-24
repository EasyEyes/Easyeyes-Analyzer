#### scatter plots ####

# Helper function for logarithmic jitter (unbiased for log scales)
add_log_jitter <- function(values, jitter_percent = 1, seed = 42) {
  # Apply logarithmic jitter for unbiased results on log scales
  # jitter_percent: percentage jitter (e.g., 1 for ±1%)
  set.seed(seed)
  log_max <- log10(1 + jitter_percent/100)
  log_min <- -log_max
  log_factor <- log_min + runif(length(values)) * (log_max - log_min)
  return(values * 10^log_factor)
}

fontColors_perisan <- tibble(
  color = 
  c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00",
  "#F781BF"),
  font = c("B-NAZANIN.TTF", "IranNastaliq.ttf", "Kalameh-Regular.ttf", "Mj_Hoor_0.ttf",
           "Moalla.ttf","Titr.bold.woff2")
)

reading_speed_vs_retention <- function(reading){
  #TODO
  t <- reading %>% group_by(participant,
                       block_condition, 
                       conditionName,
                       font,
                       accuracy) %>% 
    summarize(wordPerMin = 10^(mean(log_WPM)), .groups = "drop")
  ggplot(t) + 
    geom_point(aes(x = accuracy, y = wordPerMin)) +
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    scale_y_log10() + 
    theme_bw() + 
    theme(legend.position = "right", 
          legend.box = "vertical", 
          legend.justification = c(1,1),
          legend.margin = margin(-0.4),
          legend.key.size = unit(4.5, "mm"),
          legend.title = element_text(size=16),
          legend.text = element_text(size=16),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=16),
          plot.subtitle = element_text(size=16)) +
    xlab("Reading retention (proportion correct)") +
    ylab("Reading speed (w/min)")
}

#### New scatter plots for beauty/comfort vs crowding ####

# Scatter plot: Comfort vs Crowding
comfort_vs_crowding_scatter <- function(df_list, font_colors = NULL) {
  # Get comfort data from QA
  comfort_data <- df_list$QA %>%
     filter(!is.na(questionAndAnswerNickname) & substr(questionAndAnswerNickname, 1, 5) == "CMFRT") %>%
   mutate(comfort_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(questionAndAnswerNickname=="CMFRTAlAwwal" ~"Al-Awwal-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTmajalla" ~"majalla.ttf",
                            questionAndAnswerNickname=="CMFRTAmareddine" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTMakdessi" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTKafa" ~"SaudiTextv3-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudi" ~"Saudi-Regular.ttf",
                            questionAndAnswerNickname=="CMFRTB-Nazanin" ~ "B-NAZANIN.TTF",
                            questionAndAnswerNickname=="CMFRT-Nazanin" ~ "B-NAZANIN.TTF",
                            questionAndAnswerNickname=="CMFRT-Titr" ~ "Titr.bold.woff2",
                            questionAndAnswerNickname=="CMFRT-Kalameh" ~ "Kalameh-Regular.ttf",
                            questionAndAnswerNickname=="CMFRT-IranNastaliq" ~ "IranNastaliq.ttf",
                            questionAndAnswerNickname=="CMFRT-Moalla" ~ "Moalla.ttf",
                            questionAndAnswerNickname=="CMFRT-MJ_Hoor" ~ "Mj-Hoor_0.ttf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv1" ~"SaudiTextv1-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv2" ~"SaudiTextv2-Regular.otf",
                            questionAndAnswerNickname=="CMFRTSaudiTextv3" ~"SaudiTextv3-Regular.otf",
                            TRUE ~ questionAndAnswerNickname  # fallback for any unmatched cases
           )) %>%
    filter(!is.na(comfort_rating)) %>%
    group_by(participant, font) %>%
    summarize(comfort_rating = mean(comfort_rating), .groups = "drop")
  
  # Get crowding data (average across conditions for each participant-font combination)
  # Standardize font names to match beauty/comfort mapping
  crowding_data <- df_list$crowding %>%
    mutate(
      crowding_distance = 10^log_crowding_distance_deg
    ) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = mean(crowding_distance, na.rm = TRUE), .groups = "drop")
  
  # Join comfort and crowding data
  combined_data <- comfort_data %>%
    inner_join(crowding_data, by = c("participant", "font")) %>%
    filter(!is.na(comfort_rating), !is.na(crowding_distance))
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$crowding_distance, combined_data$comfort_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Add logarithmic jitter to x-axis (log scale) and linear jitter to y-axis (integer ratings)
  combined_data <- combined_data %>%
    mutate(
      crowding_distance_jitter = add_log_jitter(crowding_distance, jitter_percent = 2, seed = 42),
      comfort_rating_jitter = comfort_rating + runif(n(), -0.25, 0.25)
    )
  
  # Create the plot
  p <- ggplot(combined_data, aes(x = crowding_distance_jitter, y = comfort_rating_jitter, color = font)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_log10() +
    annotation_logticks(sides = "b", 
                        short = unit(2, "pt"), 
                        mid = unit(2, "pt"), 
                        long = unit(7, "pt")) +
    annotate("text", x = min(combined_data$crowding_distance) * 1.1, 
             y = max(combined_data$comfort_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Comfort vs crowding",
         x = "Crowding Distance (deg)", 
         y = "Comfort Rating")
  
  # Merge Persian font colors with palette for any missing fonts
  perisan_map <- fontColors_perisan %>% dplyr::mutate(font = trimws(font))
  combined_data <- combined_data %>% dplyr::mutate(font = trimws(font))
  fonts_in_data <- unique(combined_data$font)
  known_map <- perisan_map %>% dplyr::semi_join(tibble::tibble(font = fonts_in_data), by = "font")
  cols_vec <- stats::setNames(dplyr::distinct(known_map, font, color)$color,
                              dplyr::distinct(known_map, font, color)$font)
  missing_fonts <- setdiff(fonts_in_data, names(cols_vec))
  if (length(missing_fonts) > 0) {
    add_cols <- rep(colorPalette, length.out = length(missing_fonts))
    names(add_cols) <- missing_fonts
    cols_vec <- c(cols_vec, add_cols)
  }
  p <- p + scale_color_manual(values = cols_vec)
  
  p + guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Crowding  
beauty_vs_crowding_scatter <- function(df_list, font_colors = NULL) {
  # Get beauty data from QA
  beauty_data <- df_list$beauty %>%
    mutate(beauty_rating = questionAndAnswerResponse) %>%
    filter(!is.na(beauty_rating)) %>%
    group_by(participant, font) %>%
    summarize(beauty_rating = mean(beauty_rating), .groups = "drop")
  
  # Get crowding data (average across conditions for each participant-font combination)
  # Standardize font names to match beauty/comfort mapping
  crowding_data <- df_list$crowding %>%
    mutate(
      crowding_distance = 10^log_crowding_distance_deg
    ) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = mean(crowding_distance, na.rm = TRUE), .groups = "drop")
  
  # Join beauty and crowding data
  combined_data <- beauty_data %>%
    inner_join(crowding_data, by = c("participant", "font")) %>%
    filter(!is.na(beauty_rating), !is.na(crowding_distance))
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$crowding_distance, combined_data$beauty_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Add logarithmic jitter to x-axis (log scale) and linear jitter to y-axis (integer ratings)
  combined_data <- combined_data %>%
    mutate(
      crowding_distance_jitter = add_log_jitter(crowding_distance, jitter_percent = 2, seed = 42),
      beauty_rating_jitter = beauty_rating + runif(n(), -0.25, 0.25)
    )
  
  # Create the plot
  p <- ggplot(combined_data, aes(x = crowding_distance_jitter, y = beauty_rating_jitter, color = font)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_log10() +
    annotation_logticks(sides = "b", 
                        short = unit(2, "pt"), 
                        mid = unit(2, "pt"), 
                        long = unit(7, "pt")) +
    annotate("text", x = min(combined_data$crowding_distance) * 1.1, 
             y = max(combined_data$beauty_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Beauty vs Crowding",
         x = "Crowding Distance (deg)",
         y = "Beauty Rating")
  
  # Merge Persian font colors with palette for any missing fonts
  perisan_map <- fontColors_perisan %>% dplyr::mutate(font = trimws(font))
  combined_data <- combined_data %>% dplyr::mutate(font = trimws(font))
  fonts_in_data <- unique(combined_data$font)
  known_map <- perisan_map %>% dplyr::semi_join(tibble::tibble(font = fonts_in_data), by = "font")
  cols_vec <- stats::setNames(dplyr::distinct(known_map, font, color)$color,
                              dplyr::distinct(known_map, font, color)$font)
  missing_fonts <- setdiff(fonts_in_data, names(cols_vec))
  if (length(missing_fonts) > 0) {
    add_cols <- rep(colorPalette, length.out = length(missing_fonts))
    names(add_cols) <- missing_fonts
    cols_vec <- c(cols_vec, add_cols)
  }
  p <- p + scale_color_manual(values = cols_vec)
  
  p + guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Comfort
beauty_vs_comfort_scatter <- function(df_list, font_colors = NULL) {
  # Get beauty data from QA
  beauty_data <- df_list$beauty %>%
    mutate(beauty_rating = questionAndAnswerResponse) %>%
    filter(!is.na(beauty_rating)) %>%
    group_by(participant, font) %>%
    summarize(beauty_rating = mean(beauty_rating), .groups = "drop")

  # Get comfort data from QA
  comfort_data <- df_list$comfort %>%
    mutate(comfort_rating = questionAndAnswerResponse) %>%
    filter(!is.na(comfort_rating)) %>%
    group_by(participant, font) %>%
    summarize(comfort_rating = mean(comfort_rating), .groups = "drop")
  
  
  # Join beauty and comfort data (only for common fonts)
  combined_data <- beauty_data %>%
    inner_join(comfort_data, by = c("participant", "font")) %>%
    filter(!is.na(beauty_rating), !is.na(comfort_rating))
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$comfort_rating, combined_data$beauty_rating, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Add linear jitter to both axes (both are integer ratings)
  combined_data <- combined_data %>%
    mutate(
      comfort_rating_jitter = comfort_rating + runif(n(), -0.25, 0.25),
      beauty_rating_jitter = beauty_rating + runif(n(), -0.25, 0.25)
    )
  
  # Create the plot

  p <- ggplot(combined_data, aes(x = comfort_rating_jitter, y = beauty_rating_jitter, color = font)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    annotate("text", x = min(combined_data$comfort_rating) * 1.1, 
             y = max(combined_data$beauty_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                           "\nR = ", round(correlation, 3),
                           "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Beauty vs Comfort",
         x = "Comfort Rating",
         y = "Beauty Rating")
  
  # Merge Persian font colors with palette for any missing fonts
  perisan_map <- fontColors_perisan %>% dplyr::mutate(font = trimws(font))
  combined_data <- combined_data %>% dplyr::mutate(font = trimws(font))
  fonts_in_data <- unique(combined_data$font)
  known_map <- perisan_map %>% dplyr::semi_join(tibble::tibble(font = fonts_in_data), by = "font")
  cols_vec <- stats::setNames(dplyr::distinct(known_map, font, color)$color,
                              dplyr::distinct(known_map, font, color)$font)
  missing_fonts <- setdiff(fonts_in_data, names(cols_vec))
  if (length(missing_fonts) > 0) {
    add_cols <- rep(colorPalette, length.out = length(missing_fonts))
    names(add_cols) <- missing_fonts
    cols_vec <- c(cols_vec, add_cols)
  }
  p <- p + scale_color_manual(values = cols_vec)
  
  p + guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Crowding  
familiarity_vs_crowding_scatter <- function(df_list, font_colors = NULL) {
  # Get beauty data from QA
  familiarity_data <- df_list$familiarity %>%
    mutate(familiarity = questionAndAnswerResponse) %>%
    filter(!is.na(familiarity)) %>%
    group_by(participant, font) %>%
    summarize(familiarity = mean(familiarity), .groups = "drop")
  
  # Get crowding data (average across conditions for each participant-font combination)
  # Standardize font names to match beauty/comfort mapping
  crowding_data <- df_list$crowding %>%
    mutate(
      crowding_distance = 10^log_crowding_distance_deg
    ) %>%
    group_by(participant, font) %>%
    summarize(crowding_distance = mean(crowding_distance, na.rm = TRUE), .groups = "drop")
  
  # Join beauty and crowding data
  combined_data <- familiarity_data %>%
    inner_join(crowding_data, by = c("participant", "font")) %>%
    filter(!is.na(familiarity), !is.na(crowding_distance))
  
  if (nrow(combined_data) == 0) {
    return(NULL)
  }
  
  # Calculate correlation and p-value
  cor_test <- cor.test(combined_data$crowding_distance, combined_data$familiarity, 
                       method = "pearson")
  correlation <- cor_test$estimate
  p_value <- cor_test$p.value
  
  # Add logarithmic jitter to x-axis (log scale) and linear jitter to y-axis (integer ratings)
  combined_data <- combined_data %>%
    mutate(
      crowding_distance_jitter = add_log_jitter(crowding_distance, jitter_percent = 2, seed = 42),
      familiarity_jitter = familiarity + runif(n(), -0.25, 0.25)
    )
  
  # Create the plot
  p <- ggplot(combined_data, aes(x = crowding_distance_jitter, y = familiarity_jitter, color = font)) +
    geom_point(size = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black") +
    scale_x_log10() +
    annotation_logticks(sides = "b", 
                        short = unit(2, "pt"), 
                        mid = unit(2, "pt"), 
                        long = unit(7, "pt")) +
    annotate("text", x = min(combined_data$crowding_distance) * 1.1, 
             y = max(combined_data$beauty_rating) * 0.9,
             label = paste0("N = ", nrow(combined_data), 
                            "\nR = ", round(correlation, 3),
                            "\np = ", format.pval(p_value, digits = 3)),
             hjust = 0, vjust = 1, size = 4, color = "black") +
    theme_bw() +
    labs(subtitle = "Familiarity vs Crowding",
         x = "Crowding Distance (deg)",
         y = "Familiarity")
  
  # Merge Persian font colors with palette for any missing fonts
  perisan_map <- fontColors_perisan %>% dplyr::mutate(font = trimws(font))
  combined_data <- combined_data %>% dplyr::mutate(font = trimws(font))
  fonts_in_data <- unique(combined_data$font)
  known_map <- perisan_map %>% dplyr::semi_join(tibble::tibble(font = fonts_in_data), by = "font")
  cols_vec <- stats::setNames(dplyr::distinct(known_map, font, color)$color,
                              dplyr::distinct(known_map, font, color)$font)
  missing_fonts <- setdiff(fonts_in_data, names(cols_vec))
  if (length(missing_fonts) > 0) {
    add_cols <- rep(colorPalette, length.out = length(missing_fonts))
    names(add_cols) <- missing_fonts
    cols_vec <- c(cols_vec, add_cols)
  }
  p <- p + scale_color_manual(values = cols_vec)
  
  p + guides(color = guide_legend(title = "Font", ncol = 2))
}

#### Font-aggregated acuity scatters ####

# Resolve a named font->color vector from optional tibble/named vector + palette.
resolve_font_colors <- function(fonts, font_colors = NULL) {
  fonts <- sort(unique(as.character(fonts)))
  fonts <- fonts[!is.na(fonts) & fonts != ""]
  if (length(fonts) == 0) {
    return(character())
  }
  cols_map <- NULL
  if (!is.null(font_colors)) {
    if (is.data.frame(font_colors) && all(c("font", "color") %in% names(font_colors))) {
      cols_map <- stats::setNames(as.character(font_colors$color), as.character(font_colors$font))
    } else if (is.vector(font_colors) && !is.null(names(font_colors))) {
      cols_map <- font_colors
    }
  }
  if (is.null(cols_map)) {
    return(font_color_palette(fonts))
  }
  missing <- setdiff(fonts, names(cols_map))
  if (length(missing) > 0) {
    fill <- rep(colorPalette, length.out = length(missing))
    names(fill) <- missing
    cols_map <- c(cols_map, fill)
  }
  out <- cols_map[fonts]
  names(out) <- fonts
  out
}

# One point per font: geometric mean acuity vs SD of log acuity.
acuity_geomean_vs_sd_scatter <- function(df_list, font_colors = NULL) {
  acuity <- df_list$acuity
  if (is.null(acuity) || nrow(acuity) == 0) {
    return(NULL)
  }

  summary_data <- acuity %>%
    mutate(
      log_acuity = suppressWarnings(as.numeric(questMeanAtEndOfTrialsLoop)),
      acuity_deg = 10^log_acuity
    ) %>%
    filter(is.finite(log_acuity), is.finite(acuity_deg), acuity_deg > 0) %>%
    group_by(font) %>%
    summarise(
      geomean_acuity = 10^mean(log_acuity, na.rm = TRUE),
      sd_log_acuity = sd(log_acuity, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    filter(is.finite(geomean_acuity), is.finite(sd_log_acuity), n >= 2)

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    mutate(
      font_label = font_comparison_axis_label(font),
      font_label = factor(font_label, levels = sort(unique(font_label)))
    )

  cols <- resolve_font_colors(summary_data$font_label, {
    if (is.null(font_colors)) {
      NULL
    } else if (is.data.frame(font_colors) && all(c("font", "color") %in% names(font_colors))) {
      font_colors %>%
        mutate(font = font_comparison_axis_label(font))
    } else if (is.vector(font_colors) && !is.null(names(font_colors))) {
      stats::setNames(unname(font_colors), font_comparison_axis_label(names(font_colors)))
    } else {
      NULL
    }
  })

  ggplot(summary_data, aes(x = sd_log_acuity, y = geomean_acuity, color = font_label)) +
    geom_point(size = 3.5) +
    scale_y_log10() +
    annotation_logticks(
      sides = "l",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    scale_color_manual(values = cols, name = "Font") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      subtitle = "Geometric mean acuity vs SD of log acuity",
      x = "SD of log acuity",
      y = "Geometric mean acuity (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Crowding24FontsTable1.xlsx: col A = font, col J = geometric-mean Bouma b.
# Crowding threshold at 5°: s = b * |ecc| with ecc = 5°.
CROWDING24_BOUMA_PATH <- "data/Crowding24FontsTable1.xlsx"
CROWDING24_ECCENTRICITY_DEG <- 5

# Hardcoded bridge: archive result-file font names ↔ Excel Table 1 display names.
# (Independent of the standalone Acuity24Fonts-metrics CSV.)
# fontBoundingBoxWidthReNominal is computed per upload from acuity rows
# (fontBoundingBoxReNominalRect + pxPerCm), not hardcoded here.
CROWDING24_FONT_BRIDGE <- tibble::tibble(
  excel_font = c(
    "Adobe Caslon Regular",
    "Agoesa",
    "Arial Regular",
    "Baskerville Pro Regular",
    "Courier Prime",
    "Edwardian Script ITC Pro Regular",
    "Extenda 10 Pica",
    "Frutiger Pro 55 Roman",
    "Georgia Regular",
    "Haut Relief NF",
    "Le Monde Livre Std Regular",
    "Letraflex Regular",
    "LiebeLotte",
    "Museo Sans 500",
    "OMFUG",
    "Optimistic Text",
    "Proxima Nova",
    "Rollerscript Smooth",
    "Sabon Next Pro Regular",
    "Scarlet Wood Bold",
    "TheSans Plain",
    "Times New Roman",
    "Tiny 5x3 100",
    "Zapfino Extra Pro Regular"
  ),
  font_from_csv = c(
    "Caslon.woff2",
    "AgoesaDisplayRegular.woff2",
    "Arial.woff2",
    "Baskerville.woff2",
    "Courier.ttf",
    "Edwardian.ttf",
    "Extenda 10 Pica.woff2",
    "Frutiger.woff2",
    "Georgia.woff2",
    "HautRelief.woff2",
    "LeMonde.otf",
    "Letraflex.woff2",
    "LiebeLotte.woff2",
    "Museo.woff2",
    "Omfug.woff",
    "Optimistic.woff2",
    "ProximaNova.woff2",
    "Rollerscript.woff2",
    "Sabon.woff2",
    "ScarletWood.woff",
    "TheSans.woff2",
    "TimesNewRoman.woff2",
    "Tiny.otf",
    "Zapfino.otf"
  )
)

normalize_font_match_key <- function(fonts) {
  fonts <- as.character(fonts)
  fonts <- gsub("\u00AD", "", fonts, fixed = TRUE) # soft hyphen
  fonts <- strip_font_filetype(fonts)
  fonts <- tolower(trimws(fonts))
  fonts <- gsub("[^a-z0-9]+", "", fonts)
  fonts
}

load_crowding24_bouma_table <- function(path = CROWDING24_BOUMA_PATH,
                                        eccentricity_deg = CROWDING24_ECCENTRICITY_DEG) {
  empty <- tibble::tibble(
    excel_font = character(),
    bouma = numeric(),
    crowding_deg = numeric(),
    # Excel col G: spacing over nominal size (= fontSpacingReNominal)
    fontSpacingReNominal = numeric(),
    excel_key = character()
  )
  if (!file.exists(path)) {
    return(empty)
  }
  raw <- suppressMessages(readxl::read_excel(path, col_names = FALSE))
  if (ncol(raw) < 10) {
    return(empty)
  }
  font <- gsub("\u00AD", "", trimws(as.character(raw[[1]])), fixed = TRUE)
  bouma <- suppressWarnings(as.numeric(as.character(raw[[10]])))
  spacing <- if (ncol(raw) >= 7) {
    suppressWarnings(as.numeric(as.character(raw[[7]])))
  } else {
    rep(NA_real_, length(font))
  }
  keep <- !is.na(font) & font != "" & font != "Font" &
    is.finite(bouma) & bouma > 0
  if (!any(keep)) {
    return(empty)
  }
  ecc <- suppressWarnings(as.numeric(eccentricity_deg)[1])
  if (!is.finite(ecc) || ecc <= 0) {
    ecc <- CROWDING24_ECCENTRICITY_DEG
  }
  kept_font <- font[keep]
  kept_bouma <- bouma[keep]
  kept_spacing <- spacing[keep]
  tibble::tibble(
    excel_font = kept_font,
    bouma = kept_bouma,
    crowding_deg = kept_bouma * abs(ecc),
    fontSpacingReNominal = kept_spacing,
    excel_key = normalize_font_match_key(kept_font)
  ) %>%
    distinct(excel_key, .keep_all = TRUE)
}

# Map archive font labels → Excel Bouma crowding via the hardcoded bridge.
match_archive_fonts_to_bouma <- function(archive_fonts,
                                         bridge = CROWDING24_FONT_BRIDGE,
                                         bouma_table = NULL) {
  if (is.null(bouma_table)) {
    bouma_table <- load_crowding24_bouma_table()
  }
  archive_fonts <- unique(as.character(archive_fonts))
  archive_fonts <- archive_fonts[!is.na(archive_fonts) & archive_fonts != ""]
  empty <- tibble::tibble(
    archive_font = character(),
    excel_font = character(),
    bouma = numeric(),
    crowding_deg = numeric(),
    fontSpacingReNominal = numeric()
  )
  if (length(archive_fonts) == 0 || nrow(bridge) == 0 || nrow(bouma_table) == 0) {
    return(empty)
  }

  bridge <- bridge %>%
    mutate(
      archive_key = normalize_font_match_key(font_from_csv),
      excel_key = normalize_font_match_key(excel_font)
    )

  arch_df <- tibble::tibble(archive_font = archive_fonts) %>%
    mutate(archive_key = normalize_font_match_key(archive_font))

  # Prefer exact archive-key match.
  exact <- arch_df %>%
    inner_join(
      bridge %>% select(archive_key, excel_font, excel_key),
      by = "archive_key"
    )

  unmatched <- arch_df %>%
    filter(!archive_font %in% exact$archive_font)

  # Fallback: unique substring match against archive or excel keys.
  fuzzy_rows <- list()
  if (nrow(unmatched) > 0) {
    for (i in seq_len(nrow(unmatched))) {
      ak <- unmatched$archive_key[[i]]
      if (!nzchar(ak)) next
      scores <- vapply(seq_len(nrow(bridge)), function(j) {
        score <- 0L
        ark <- bridge$archive_key[[j]]
        ek <- bridge$excel_key[[j]]
        if (nzchar(ark) && (grepl(ak, ark, fixed = TRUE) || grepl(ark, ak, fixed = TRUE))) {
          score <- max(score, nchar(ak) + nchar(ark) + 1000L)
        }
        if (nzchar(ek) && (grepl(ak, ek, fixed = TRUE) || grepl(ek, ak, fixed = TRUE))) {
          score <- max(score, nchar(ak) + nchar(ek))
        }
        score
      }, integer(1))
      if (!any(scores > 0L)) next
      best <- which(scores == max(scores))
      if (length(best) != 1L) next
      j <- best[[1L]]
      fuzzy_rows[[length(fuzzy_rows) + 1L]] <- tibble::tibble(
        archive_font = unmatched$archive_font[[i]],
        archive_key = ak,
        excel_font = bridge$excel_font[[j]],
        excel_key = bridge$excel_key[[j]]
      )
    }
  }

  mapped <- bind_rows(exact, bind_rows(fuzzy_rows))
  if (nrow(mapped) == 0) {
    return(empty)
  }

  mapped %>%
    distinct(archive_font, .keep_all = TRUE) %>%
    inner_join(
      bouma_table %>% select(
        excel_key, bouma, crowding_deg, fontSpacingReNominal
      ),
      by = "excel_key"
    ) %>%
    select(
      archive_font, excel_font, bouma, crowding_deg,
      fontSpacingReNominal
    ) %>%
    filter(is.finite(crowding_deg), crowding_deg > 0)
}

# One point per font: archive geometric-mean acuity vs Excel Bouma crowding
# (s = b×5°), with hardcoded archive↔Excel font-name bridge.
acuity_vs_crowding_by_font_scatter <- function(df_list, font_colors = NULL) {
  acuity <- df_list$acuity
  if (is.null(acuity) || nrow(acuity) == 0) {
    return(NULL)
  }

  bouma_table <- load_crowding24_bouma_table()
  if (nrow(bouma_table) == 0) {
    return(NULL)
  }

  acuity_summary <- acuity %>%
    mutate(log_acuity = suppressWarnings(as.numeric(questMeanAtEndOfTrialsLoop))) %>%
    filter(is.finite(log_acuity)) %>%
    group_by(font) %>%
    summarise(
      geomean_acuity = 10^mean(log_acuity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(is.finite(geomean_acuity), geomean_acuity > 0)

  font_map <- match_archive_fonts_to_bouma(acuity_summary$font, bouma_table = bouma_table)
  if (nrow(font_map) == 0) {
    return(NULL)
  }

  summary_data <- acuity_summary %>%
    inner_join(font_map, by = c("font" = "archive_font"))

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    mutate(
      font_label = font_comparison_axis_label(font),
      font_label = factor(font_label, levels = sort(unique(font_label)))
    )

  cols <- resolve_font_colors(summary_data$font_label, {
    if (is.null(font_colors)) {
      NULL
    } else if (is.data.frame(font_colors) && all(c("font", "color") %in% names(font_colors))) {
      font_colors %>%
        mutate(font = font_comparison_axis_label(font))
    } else if (is.vector(font_colors) && !is.null(names(font_colors))) {
      stats::setNames(unname(font_colors), font_comparison_axis_label(names(font_colors)))
    } else {
      NULL
    }
  })

  ggplot(summary_data, aes(x = crowding_deg, y = geomean_acuity, color = font_label)) +
    geom_point(size = 3.5) +
    scale_x_log10() +
    scale_y_log10() +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    scale_color_manual(values = cols, name = "Font") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      subtitle = "Acuity vs Crowding (Bouma×5° from Crowding24FontsTable1)",
      x = "Crowding (deg)",
      y = "Acuity (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# One point per font: Crowding:Acuity size ratio r vs Bouma factor.
# r = (crowdingThresholdDeg / fontSpacingReNominal) /
#     (acuityDeg / fontBoundingBoxWidthReNominal)
# fontSpacingReNominal from Excel col G; bbox width from uploaded archives
# (fontBoundingBoxReNominalRect + pxPerCm on acuity rows).
crowding_acuity_size_ratio_vs_bouma_scatter <- function(df_list, font_colors = NULL) {
  acuity <- df_list$acuity
  if (is.null(acuity) || nrow(acuity) == 0) {
    return(NULL)
  }

  # Recompute bbox width from raw rect + pxPerCm when width is missing but inputs exist.
  if (!"fontBoundingBoxWidthReNominal" %in% names(acuity) ||
      !any(is.finite(suppressWarnings(as.numeric(acuity$fontBoundingBoxWidthReNominal))))) {
    if (all(c("fontBoundingBoxReNominalRect", "pxPerCm") %in% names(acuity))) {
      acuity <- acuity %>%
        mutate(
          fontBoundingBoxWidthReNominal = font_bbox_width_re_nominal(
            fontBoundingBoxReNominalRect,
            pxPerCm
          )
        )
    }
  }

  if (!"fontBoundingBoxWidthReNominal" %in% names(acuity)) {
    return(NULL)
  }

  bouma_table <- load_crowding24_bouma_table()
  if (nrow(bouma_table) == 0) {
    return(NULL)
  }

  acuity_summary <- acuity %>%
    mutate(
      log_acuity = suppressWarnings(as.numeric(questMeanAtEndOfTrialsLoop)),
      fontBoundingBoxWidthReNominal = suppressWarnings(
        as.numeric(fontBoundingBoxWidthReNominal)
      )
    ) %>%
    filter(is.finite(log_acuity)) %>%
    group_by(font) %>%
    summarise(
      acuityDeg = 10^mean(log_acuity, na.rm = TRUE),
      fontBoundingBoxWidthReNominal = median(
        fontBoundingBoxWidthReNominal[is.finite(fontBoundingBoxWidthReNominal) &
                                        fontBoundingBoxWidthReNominal > 0],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    filter(
      is.finite(acuityDeg), acuityDeg > 0,
      is.finite(fontBoundingBoxWidthReNominal),
      fontBoundingBoxWidthReNominal > 0
    )

  font_map <- match_archive_fonts_to_bouma(acuity_summary$font, bouma_table = bouma_table)
  if (nrow(font_map) == 0) {
    return(NULL)
  }

  summary_data <- acuity_summary %>%
    inner_join(font_map, by = c("font" = "archive_font")) %>%
    mutate(
      crowding_over_spacing = crowding_deg / fontSpacingReNominal,
      acuity_over_bbox = acuityDeg / fontBoundingBoxWidthReNominal,
      r = crowding_over_spacing / acuity_over_bbox
    ) %>%
    filter(
      is.finite(r), r > 0,
      is.finite(bouma), bouma > 0,
      is.finite(fontSpacingReNominal), fontSpacingReNominal > 0
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    mutate(
      font_label = font_comparison_axis_label(font),
      font_label = factor(font_label, levels = sort(unique(font_label)))
    )

  cols <- resolve_font_colors(summary_data$font_label, {
    if (is.null(font_colors)) {
      NULL
    } else if (is.data.frame(font_colors) && all(c("font", "color") %in% names(font_colors))) {
      font_colors %>%
        mutate(font = font_comparison_axis_label(font))
    } else if (is.vector(font_colors) && !is.null(names(font_colors))) {
      stats::setNames(unname(font_colors), font_comparison_axis_label(names(font_colors)))
    } else {
      NULL
    }
  })

  # Equal log-decade length on x and y (both log10 → coord_fixed ratio 1).
  x_range <- range(log10(summary_data$bouma), finite = TRUE)
  y_range <- range(log10(summary_data$r), finite = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  pad <- 0.05 * max(x_span, y_span, 0.1)
  half <- 0.5 * max(x_span, y_span) + pad
  x_mid <- mean(x_range)
  y_mid <- mean(y_range)
  x_lim <- 10^c(x_mid - half, x_mid + half)
  y_lim <- 10^c(y_mid - half, y_mid + half)

  ggplot(summary_data, aes(x = bouma, y = r, color = font_label)) +
    geom_point(size = 3.5) +
    scale_x_log10(limits = x_lim) +
    scale_y_log10(limits = y_lim) +
    coord_fixed(ratio = 1) +
    annotation_logticks(
      sides = "bl",
      short = unit(2, "pt"),
      mid = unit(2, "pt"),
      long = unit(7, "pt")
    ) +
    scale_color_manual(values = cols, name = "Font") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    ) +
    labs(
      subtitle = "Crowding:Acuity size ratio vs Bouma factor",
      x = "Bouma factor",
      y = "Crowding:Acuity size ratio r"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}
