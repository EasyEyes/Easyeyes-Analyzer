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

# Paper-style two-letter abbreviations + filenames to look for under fonts/
# (app-bundled; required for shinyapps.io). Prefer .otf/.ttf — ragg/systemfonts
# do not reliably render .woff/.woff2.
CROWDING24_FONT_ABBREVS <- tibble::tibble(
  excel_font = CROWDING24_FONT_BRIDGE$excel_font,
  abbr = c(
    "Ca", "Ag", "Ar", "Ba", "Co", "Ed", "Ex", "Fr", "Ge", "Ha",
    "Le", "Le", "Li", "Mu", "Om", "Op", "Pr", "Ro", "Sa", "Sc",
    "Th", "Ti", "Ti", "Za"
  ),
  # Search order in fonts/; first existing file wins (.otf/.ttf preferred).
  app_font_files = I(list(
    c("Caslon.otf", "Caslon.ttf"),
    c("Agoesa.otf", "AgoesaDisplay-Regular.otf", "AgoesaDisplayRegular.otf"),
    c("Arial.otf", "Arial.ttf"),
    c("Baskerville.otf", "Baskerville.ttf", "Baskerville.woff2"),
    c("Courier.ttf", "Courier.otf", "CourierPrime.ttf"),
    c("Edwardian.ttf", "Edwardian.otf"),
    c("Extenda 10 pica.otf", "Extenda 10 Pica.otf", "Extenda 10 Pica.woff2"),
    c("Frutiger.otf", "Frutiger.ttf"),
    c("Georgia.ttf", "Georgia.otf"),
    c("Haut Relief NF Regular.otf", "HautRelief.otf", "HautRelief.ttf"),
    c("LeMonde.otf", "LeMonde.ttf"),
    c("Letraflex.otf", "Letraflex.ttf"),
    c("LiebeLotte.otf", "LiebeLotte.ttf"),
    c("Museo.otf", "Museo.ttf"),
    c("OMFUG-Retro.otf", "Omfug.otf", "Omfug.ttf", "OMFUG.otf"),
    c("Optimistic_Text_Rg.ttf", "Optimistic.ttf", "Optimistic.otf", "Optimistic.woff2"),
    c("ProximaNova.otf", "ProximaNova.ttf"),
    c("Rollerscript.otf", "Rollerscript.ttf"),
    c("Sabon.otf", "Sabon.ttf"),
    c("ScarletWood.otf", "ScarletWood.ttf"),
    c("TheSans.otf", "TheSans.ttf"),
    c("Times New Roman.ttf", "TimesNewRoman.ttf", "TimesNewRoman.otf"),
    c("Tiny.otf", "Tiny.ttf"),
    c("Zapfino.otf", "Zapfino.ttf")
  ))
) %>%
  dplyr::bind_rows(
    # Sloan is archive-only (not in Excel Table 1); paper abbrev is "S", not "SL".
    tibble::tibble(
      excel_font = "Sloan",
      abbr = "S",
      app_font_files = I(list(c("Sloan.otf", "Sloan.ttf")))
    )
  )

# Two-letter (paper) font abbreviation for plots. Sloan → "S".
crowding24_font_abbreviation <- function(fonts, abbrevs = CROWDING24_FONT_ABBREVS) {
  fonts <- as.character(fonts)
  out <- rep(NA_character_, length(fonts))
  if (length(fonts) == 0) {
    return(out)
  }
  for (i in seq_along(fonts)) {
    idx <- match_crowding24_font_abbrev_index(fonts[[i]], abbrevs = abbrevs)
    if (!is.na(idx)) {
      out[[i]] <- abbrevs$abbr[[idx]]
      next
    }
    letters_only <- gsub("[^A-Za-z]", "", strip_font_filetype(fonts[[i]]))
    out[[i]] <- toupper(substr(letters_only, 1, 2))
  }
  out
}

# Match a font label to a CROWDING24_FONT_ABBREVS row (excel name, file stem, or unique fuzzy).
match_crowding24_font_abbrev_index <- function(font, abbrevs = CROWDING24_FONT_ABBREVS) {
  key <- normalize_font_match_key(font)
  if (!nzchar(key)) {
    return(NA_integer_)
  }
  excel_keys <- normalize_font_match_key(abbrevs$excel_font)
  hit <- which(excel_keys == key)
  if (length(hit) == 1L) {
    return(hit[[1]])
  }

  # File stems listed under app_font_files (Caslon.otf → caslon).
  for (i in seq_len(nrow(abbrevs))) {
    files <- unlist(abbrevs$app_font_files[[i]], use.names = FALSE)
    stems <- normalize_font_match_key(tools::file_path_sans_ext(files))
    if (key %in% stems) {
      return(i)
    }
  }

  # Bridge archive filenames (Arial.woff2 → arial).
  if (exists("CROWDING24_FONT_BRIDGE") &&
      is.data.frame(CROWDING24_FONT_BRIDGE) &&
      all(c("excel_font", "font_from_csv") %in% names(CROWDING24_FONT_BRIDGE))) {
    bridge_keys <- normalize_font_match_key(CROWDING24_FONT_BRIDGE$font_from_csv)
    bhit <- which(bridge_keys == key)
    if (length(bhit) == 1L) {
      excel_hit <- which(excel_keys == normalize_font_match_key(
        CROWDING24_FONT_BRIDGE$excel_font[[bhit[[1]]]]
      ))
      if (length(excel_hit) >= 1L) {
        return(excel_hit[[1]])
      }
    }
  }

  # Unique substring / prefix against excel keys (Arial → Arial Regular).
  fuzzy <- which(vapply(excel_keys, function(ek) {
    nzchar(ek) && nchar(key) >= 3L &&
      (grepl(key, ek, fixed = TRUE) || grepl(ek, key, fixed = TRUE))
  }, logical(1)))
  if (length(fuzzy) == 1L) {
    return(fuzzy[[1]])
  }
  if (length(fuzzy) > 1L) {
    return(fuzzy[[which.max(nchar(excel_keys[fuzzy]))]])
  }
  NA_integer_
}

.crowding24_registered_fonts <- new.env(parent = emptyenv())

crowding24_app_fonts_dir <- function() {
  cached <- .crowding24_registered_fonts$fonts_dir
  if (is.character(cached) && length(cached) == 1 && dir.exists(cached)) {
    return(cached)
  }

  # Prefer an app root that contains both fonts/ and server.R (cwd can drift in Shiny).
  starts <- unique(c(
    getwd(),
    if (exists("APP_DIR", envir = .GlobalEnv, inherits = FALSE)) {
      as.character(get("APP_DIR", envir = .GlobalEnv))
    } else {
      character()
    }
  ))
  for (start in starts) {
    if (!nzchar(start) || !dir.exists(start)) next
    d <- normalizePath(start, winslash = "/", mustWork = FALSE)
    for (i in seq_len(8)) {
      fonts_dir <- file.path(d, "fonts")
      if (dir.exists(fonts_dir) &&
          (file.exists(file.path(d, "server.R")) || file.exists(file.path(d, "ui.R")))) {
        fonts_dir <- normalizePath(fonts_dir, winslash = "/", mustWork = FALSE)
        .crowding24_registered_fonts$fonts_dir <- fonts_dir
        return(fonts_dir)
      }
      parent <- dirname(d)
      if (identical(parent, d)) break
      d <- parent
    }
  }

  # Last resort: any existing ./fonts under cwd.
  for (d in c(file.path(getwd(), "fonts"), "fonts")) {
    if (dir.exists(d)) {
      fonts_dir <- normalizePath(d, winslash = "/", mustWork = FALSE)
      .crowding24_registered_fonts$fonts_dir <- fonts_dir
      return(fonts_dir)
    }
  }
  normalizePath(file.path(getwd(), "fonts"), winslash = "/", mustWork = FALSE)
}

find_crowding24_app_font_file <- function(filenames,
                                          fonts_dir = crowding24_app_fonts_dir(),
                                          ragg_ok_only = TRUE) {
  filenames <- as.character(filenames)
  filenames <- filenames[!is.na(filenames) & nzchar(filenames)]
  if (length(filenames) == 0 || !dir.exists(fonts_dir)) {
    return(NA_character_)
  }
  cache_key <- paste0("listing:", fonts_dir)
  existing <- .crowding24_registered_fonts[[cache_key]]
  if (is.null(existing)) {
    existing <- list.files(fonts_dir, full.names = FALSE)
    .crowding24_registered_fonts[[cache_key]] <- existing
  }
  if (length(existing) == 0) {
    return(NA_character_)
  }
  existing_lc <- tolower(existing)
  for (fn in filenames) {
    hit <- which(existing_lc == tolower(fn))
    if (length(hit) > 0) {
      path <- file.path(fonts_dir, existing[[hit[[1]]]])
      # ragg/systemfonts cannot reliably open .woff/.woff2 — skip unless allowed.
      if (isTRUE(ragg_ok_only) && grepl("\\.(woff2?)$", path, ignore.case = TRUE)) {
        next
      }
      return(path)
    }
  }
  # Stem match: prefer .otf/.ttf/.ttc only when ragg_ok_only.
  stems <- unique(tolower(tools::file_path_sans_ext(filenames)))
  for (stem in stems) {
    hit <- which(
      tools::file_path_sans_ext(existing_lc) == stem &
        grepl("\\.(otf|ttf|ttc)$", existing_lc)
    )
    if (length(hit) > 0) {
      return(file.path(fonts_dir, existing[[hit[[1]]]]))
    }
  }
  if (!isTRUE(ragg_ok_only)) {
    for (stem in stems) {
      hit <- which(
        tools::file_path_sans_ext(existing_lc) == stem &
          grepl("\\.(woff2|woff)$", existing_lc)
      )
      if (length(hit) > 0) {
        return(file.path(fonts_dir, existing[[hit[[1]]]]))
      }
    }
  }
  NA_character_
}

# Remember family → path for later registration at PNG render time.
# Does NOT call systemfonts here (that is expensive and must be active only
# while drawing the two abbrev plots).
remember_crowding24_app_font <- function(excel_font, path) {
  family <- paste0("ee_", normalize_font_match_key(excel_font))
  path <- normalizePath(path, winslash = "/", mustWork = FALSE)
  .crowding24_registered_fonts[[family]] <- path
  family
}

# Register remembered (and abbrevs-table) faces into systemfonts for ragg.
register_crowding24_app_font <- function(excel_font, path) {
  family <- remember_crowding24_app_font(excel_font, path)
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(family)
  }
  path <- .crowding24_registered_fonts[[family]]
  matched <- tryCatch(
    systemfonts::match_fonts(family)$path[[1]],
    error = function(e) NA_character_
  )
  if (is.character(matched) && length(matched) == 1 &&
      !is.na(matched) && nzchar(matched) &&
      identical(normalizePath(matched, mustWork = FALSE), path)) {
    return(family)
  }
  # Never clear_registry() here — that is reserved for release after abbrev plots.
  tryCatch({
    systemfonts::register_font(name = family, plain = path)
  }, error = function(e) invisible(NULL))
  family
}

# Resolve each excel font to an ee_* family name + remember the fonts/ path.
# Registration into systemfonts is deferred until PNG render (see ensure_*).
resolve_crowding24_plot_font_families <- function(excel_fonts,
                                                  abbrevs = CROWDING24_FONT_ABBREVS) {
  excel_fonts <- as.character(excel_fonts)
  out <- rep("sans", length(excel_fonts))
  if (length(excel_fonts) == 0) {
    return(out)
  }
  fonts_dir <- crowding24_app_fonts_dir()
  if (!dir.exists(fonts_dir)) {
    return(out)
  }

  for (i in seq_along(excel_fonts)) {
    idx <- match_crowding24_font_abbrev_index(excel_fonts[[i]], abbrevs = abbrevs)
    if (is.na(idx)) next
    row <- abbrevs[idx, , drop = FALSE]
    path <- find_crowding24_app_font_file(
      unlist(row$app_font_files[[1]], use.names = FALSE),
      fonts_dir = fonts_dir,
      ragg_ok_only = TRUE
    )
    if (is.na(path)) next
    out[[i]] <- remember_crowding24_app_font(row$excel_font[[1]], path)
  }
  out
}

# Register faces for abbrev plots.
# Prefer sysfonts::font_add so showtext_auto (from emojifont) can find ee_* families
# without turning showtext off — turning it off inflates PNG-theme text sizes.
ensure_crowding24_plot_fonts_registered <- function(abbrevs = CROWDING24_FONT_ABBREVS,
                                                    families = NULL) {
  fonts_dir <- crowding24_app_fonts_dir()
  if (!dir.exists(fonts_dir)) {
    return(invisible(FALSE))
  }

  register_one <- function(family, path) {
    if (!is.character(family) || !nzchar(family)) return(invisible(NULL))
    if (!is.character(path) || length(path) != 1 || is.na(path) || !file.exists(path)) {
      return(invisible(NULL))
    }
    .crowding24_registered_fonts[[family]] <- normalizePath(path, winslash = "/", mustWork = FALSE)
    # showtext (emojifont) looks up fonts via sysfonts, not systemfonts::register_font.
    if (requireNamespace("sysfonts", quietly = TRUE)) {
      tryCatch(
        sysfonts::font_add(family, regular = path),
        error = function(e) invisible(NULL)
      )
    }
    # Also register with systemfonts for ragg when showtext is not intercepting.
    if (requireNamespace("systemfonts", quietly = TRUE)) {
      tryCatch(
        systemfonts::register_font(name = family, plain = path),
        error = function(e) invisible(NULL)
      )
    }
    invisible(NULL)
  }

  if (!is.null(families)) {
    families <- unique(as.character(families))
    families <- families[startsWith(families, "ee_")]
    for (nm in families) {
      path <- .crowding24_registered_fonts[[nm]]
      register_one(nm, path)
    }
    return(invisible(length(families) > 0))
  }

  for (i in seq_len(nrow(abbrevs))) {
    path <- find_crowding24_app_font_file(
      unlist(abbrevs$app_font_files[[i]], use.names = FALSE),
      fonts_dir = fonts_dir,
      ragg_ok_only = TRUE
    )
    if (is.na(path)) next
    family <- remember_crowding24_app_font(abbrevs$excel_font[[i]], path)
    register_one(family, path)
  }
  invisible(TRUE)
}

# Clear systemfonts aliases after abbrev renders (sysfonts/showtext entries can stay).
release_crowding24_plot_fonts <- function() {
  if (requireNamespace("systemfonts", quietly = TRUE)) {
    tryCatch(systemfonts::clear_registry(), error = function(e) invisible(NULL))
  }
  invisible(NULL)
}

plot_uses_crowding24_ee_fonts <- function(plot) {
  length(crowding24_ee_families_in_plot(plot)) > 0
}

crowding24_ee_families_in_plot <- function(plot) {
  if (is.null(plot) || !inherits(plot, "ggplot")) {
    return(character())
  }
  fams <- character()
  for (layer in plot$layers) {
    fam <- layer$aes_params$family
    if (is.null(fam)) fam <- layer$geom_params$family
    if (is.character(fam)) {
      fams <- c(fams, fam[startsWith(fam, "ee_") & !is.na(fam)])
    }
  }
  unique(fams)
}

# Add geom_text labels with one layer per family so ragg picks the right face.
# Font registration is deferred to PNG render (ensure_crowding24_plot_fonts_registered).
add_crowding24_font_abbrev_text <- function(plot, data, size = 4.5) {
  if (is.null(data) || nrow(data) == 0) {
    return(plot)
  }
  fams <- unique(as.character(data$plot_family))
  fams <- fams[!is.na(fams) & nzchar(fams) & fams != "sans"]
  for (fam in fams) {
    layer_data <- data[data$plot_family == fam, , drop = FALSE]
    plot <- plot +
      ggplot2::geom_text(
        data = layer_data,
        ggplot2::aes(label = abbr),
        family = fam,
        size = size,
        color = "black",
        show.legend = FALSE,
        inherit.aes = TRUE
      )
  }
  # Fonts with no ragg-loadable file still get a sans label so the point exists.
  sans_data <- data[is.na(data$plot_family) | data$plot_family == "" |
                      data$plot_family == "sans", , drop = FALSE]
  if (nrow(sans_data) > 0) {
    plot <- plot +
      ggplot2::geom_text(
        data = sans_data,
        ggplot2::aes(label = abbr),
        family = "sans",
        size = size,
        color = "black",
        show.legend = FALSE,
        inherit.aes = TRUE
      )
  }
  plot
}

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
    # crowdingSpacingDeg = BoumaFactor × 5°
    crowding_deg = numeric(),
    # Excel col F: x-height over nominal size
    xHeightReNominal = numeric(),
    # Excel col G: spacing over nominal size
    fontSpacingReNominal = numeric(),
    # Excel col D: Display / Script / Text: Serif / Text: Sans Serif
    font_category = character(),
    # Excel col K: SD of log Bouma (= SD of log crowding, up to additive constant)
    sd_log_bouma = numeric(),
    # Excel col O: N for crowding Bouma estimates
    N_crowding = numeric(),
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
  style <- if (ncol(raw) >= 4) {
    trimws(as.character(raw[[4]]))
  } else {
    rep(NA_character_, length(font))
  }
  xheight <- if (ncol(raw) >= 6) {
    suppressWarnings(as.numeric(as.character(raw[[6]])))
  } else {
    rep(NA_real_, length(font))
  }
  spacing <- if (ncol(raw) >= 7) {
    suppressWarnings(as.numeric(as.character(raw[[7]])))
  } else {
    rep(NA_real_, length(font))
  }
  sd_log_bouma <- if (ncol(raw) >= 11) {
    suppressWarnings(as.numeric(as.character(raw[[11]])))
  } else {
    rep(NA_real_, length(font))
  }
  n_crowding <- if (ncol(raw) >= 15) {
    suppressWarnings(as.numeric(as.character(raw[[15]])))
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
  kept_style <- style[keep]
  kept_xheight <- xheight[keep]
  kept_spacing <- spacing[keep]
  kept_sd <- sd_log_bouma[keep]
  kept_n <- n_crowding[keep]
  # Map Table-1 style → Text (sans/serif) / Display / Script (paper figure).
  # Match "Sans Serif" before "Serif" so "Text: Sans Serif" is not lumped with serif.
  font_category <- dplyr::case_when(
    grepl("^text:.*sans", kept_style, ignore.case = TRUE) ~ "Text (sans serif)",
    grepl("^text:.*serif", kept_style, ignore.case = TRUE) ~ "Text (serif)",
    grepl("^text", kept_style, ignore.case = TRUE) ~ "Text",
    grepl("^display", kept_style, ignore.case = TRUE) ~ "Display",
    grepl("^script", kept_style, ignore.case = TRUE) ~ "Script",
    TRUE ~ NA_character_
  )
  tibble::tibble(
    excel_font = kept_font,
    bouma = kept_bouma,
    # crowdingSpacingDeg from Bouma × eccentricity (5°)
    crowding_deg = kept_bouma * abs(ecc),
    xHeightReNominal = kept_xheight,
    fontSpacingReNominal = kept_spacing,
    font_category = font_category,
    sd_log_bouma = kept_sd,
    N_crowding = kept_n,
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
    xHeightReNominal = numeric(),
    fontSpacingReNominal = numeric(),
    font_category = character(),
    sd_log_bouma = numeric(),
    N_crowding = numeric()
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
        excel_key, bouma, crowding_deg,
        xHeightReNominal, fontSpacingReNominal, font_category,
        sd_log_bouma, N_crowding
      ),
      by = "excel_key"
    ) %>%
    select(
      archive_font, excel_font, bouma, crowding_deg,
      xHeightReNominal, fontSpacingReNominal, font_category,
      sd_log_bouma, N_crowding
    ) %>%
    filter(is.finite(crowding_deg), crowding_deg > 0)
}

# Per-font geometric-mean crowding (deg) from uploaded archives (df_list$crowding).
summarize_archive_crowding_by_font <- function(crowding) {
  empty <- tibble::tibble(
    font = character(),
    crowding_deg = numeric(),
    sd_log_crowding = numeric(),
    N_crowding = integer()
  )
  if (is.null(crowding) || nrow(crowding) == 0 || !"font" %in% names(crowding)) {
    return(empty)
  }
  if (!"log_crowding_distance_deg" %in% names(crowding)) {
    return(empty)
  }
  crowding %>%
    mutate(
      font = as.character(font),
      log_c = suppressWarnings(as.numeric(log_crowding_distance_deg))
    ) %>%
    filter(
      !is.na(font), font != "", font != "Roboto",
      is.finite(log_c)
    ) %>%
    group_by(font) %>%
    summarise(
      crowding_deg = 10^mean(log_c, na.rm = TRUE),
      sd_log_crowding = sd(log_c, na.rm = TRUE),
      N_crowding = dplyr::n(),
      .groups = "drop"
    ) %>%
    filter(is.finite(crowding_deg), crowding_deg > 0)
}

# One point per font: archive geometric-mean acuity vs crowding (deg).
# Crowding prefers archive thresholds when present; else Excel Bouma×5°.
acuity_vs_crowding_by_font_scatter <- function(df_list, font_colors = NULL) {
  acuity <- df_list$acuity
  if (is.null(acuity) || nrow(acuity) == 0) {
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

  if (nrow(acuity_summary) == 0) {
    return(NULL)
  }

  arch_crowd <- summarize_archive_crowding_by_font(df_list$crowding)
  bouma_table <- load_crowding24_bouma_table()
  excel_map <- if (nrow(bouma_table) > 0) {
    match_archive_fonts_to_bouma(acuity_summary$font, bouma_table = bouma_table) %>%
      transmute(
        font = archive_font,
        excel_crowding_deg = crowding_deg
      )
  } else {
    tibble::tibble(font = character(), excel_crowding_deg = numeric())
  }

  summary_data <- acuity_summary %>%
    left_join(arch_crowd %>% transmute(font, archive_crowding_deg = crowding_deg), by = "font") %>%
    left_join(excel_map, by = "font") %>%
    mutate(
      crowding_source = dplyr::case_when(
        is.finite(archive_crowding_deg) & archive_crowding_deg > 0 ~ "archive",
        is.finite(excel_crowding_deg) & excel_crowding_deg > 0 ~ "excel",
        TRUE ~ NA_character_
      ),
      crowding_deg = dplyr::if_else(
        crowding_source == "archive",
        archive_crowding_deg,
        excel_crowding_deg
      )
    ) %>%
    filter(!is.na(crowding_source), is.finite(crowding_deg), crowding_deg > 0)

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

  n_arch <- sum(summary_data$crowding_source == "archive", na.rm = TRUE)
  n_excel <- sum(summary_data$crowding_source == "excel", na.rm = TRUE)
  subtitle <- if (n_arch > 0 && n_excel > 0) {
    "Acuity vs Crowding (archive when available; else Bouma×5° from Table 1)"
  } else if (n_arch > 0) {
    "Acuity vs Crowding (from archive)"
  } else {
    "Acuity vs Crowding (Bouma×5° from Crowding24FontsTable1)"
  }

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
      subtitle = subtitle,
      x = "Crowding (deg)",
      y = "Acuity (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Build per-font Crowding:Acuity size ratio table (shared by ratio scatters).
# r = (crowdingThresholdDeg / fontSpacingReNominal) /
#     (acuityDeg / fontBoundingBoxWidthReNominal)
# Crowding threshold: archive geometric mean when available, else Excel Bouma×5°.
# Spacing / x-height for crowding size: archive CSV when available, else Excel F/G.
# SE(log r): archive SD(log crowding)/sqrt(N) or Table-1 SD(log Bouma)/sqrt(N),
# plus sample SD(log acuity)/sqrt(N_acuity).
prepare_crowding_acuity_size_ratio_data <- function(df_list) {
  acuity <- df_list$acuity
  empty <- tibble::tibble()
  if (is.null(acuity) || nrow(acuity) == 0) {
    return(empty)
  }

  if (!"fontBoundingBoxWidthReNominal" %in% names(acuity) ||
      !any(is.finite(suppressWarnings(as.numeric(acuity$fontBoundingBoxWidthReNominal))))) {
    if (all(c("fontBoundingBoxReNominalRect", "pxPerCm") %in% names(acuity))) {
      # Prefer new CSV width; else rect + optional ×k via height test (utility.R).
      height_good <- font_bbox_good_height(acuity)
      if (is.null(height_good)) {
        height_good <- rep(NA_real_, nrow(acuity))
      }
      csv_width <- if ("fontBoundingBoxWidthReNominal" %in% names(acuity)) {
        suppressWarnings(as.numeric(acuity$fontBoundingBoxWidthReNominal))
      } else {
        rep(NA_real_, nrow(acuity))
      }
      from_rect <- font_bbox_width_re_nominal(
        acuity$fontBoundingBoxReNominalRect,
        acuity$pxPerCm,
        height_good = height_good
      )
      acuity <- acuity %>%
        mutate(
          fontBoundingBoxWidthReNominal = dplyr::if_else(
            is.finite(csv_width) & csv_width > 0,
            csv_width,
            from_rect
          )
        )
    }
  }

  if (!"fontBoundingBoxWidthReNominal" %in% names(acuity)) {
    return(empty)
  }
  if (!"fontXHeightReNominal" %in% names(acuity)) {
    acuity$fontXHeightReNominal <- NA_real_
  }
  if (!"fontSpacingReNominal" %in% names(acuity)) {
    acuity$fontSpacingReNominal <- NA_real_
  }

  bouma_table <- load_crowding24_bouma_table()
  arch_crowd <- summarize_archive_crowding_by_font(df_list$crowding)
  if (nrow(bouma_table) == 0 && nrow(arch_crowd) == 0) {
    return(empty)
  }

  acuity_summary <- acuity %>%
    mutate(
      log_acuity = suppressWarnings(as.numeric(questMeanAtEndOfTrialsLoop)),
      fontBoundingBoxWidthReNominal = suppressWarnings(
        as.numeric(fontBoundingBoxWidthReNominal)
      ),
      # Archive geometry (acuity → x-height; also spacing for archive crowding path)
      archive_xHeightReNominal = suppressWarnings(as.numeric(fontXHeightReNominal)),
      archive_fontSpacingReNominal = suppressWarnings(as.numeric(fontSpacingReNominal))
    ) %>%
    filter(is.finite(log_acuity)) %>%
    group_by(font) %>%
    summarise(
      # quest threshold is acuity as bounding-box width (deg)
      acuityBoundingBoxWidthDeg = 10^mean(log_acuity, na.rm = TRUE),
      sd_log_acuity = sd(log_acuity, na.rm = TRUE),
      N_acuity = dplyr::n(),
      fontBoundingBoxWidthReNominal = median(
        fontBoundingBoxWidthReNominal[is.finite(fontBoundingBoxWidthReNominal) &
                                        fontBoundingBoxWidthReNominal > 0],
        na.rm = TRUE
      ),
      archive_xHeightReNominal = median(
        archive_xHeightReNominal[is.finite(archive_xHeightReNominal) &
                                   archive_xHeightReNominal > 0],
        na.rm = TRUE
      ),
      archive_fontSpacingReNominal = median(
        archive_fontSpacingReNominal[is.finite(archive_fontSpacingReNominal) &
                                       archive_fontSpacingReNominal > 0],
        na.rm = TRUE
      ),
      .groups = "drop"
    ) %>%
    mutate(acuityDeg = acuityBoundingBoxWidthDeg) %>%
    filter(
      is.finite(acuityBoundingBoxWidthDeg), acuityBoundingBoxWidthDeg > 0,
      is.finite(fontBoundingBoxWidthReNominal),
      fontBoundingBoxWidthReNominal > 0
    )

  if (nrow(acuity_summary) == 0) {
    return(empty)
  }

  excel_map <- if (nrow(bouma_table) > 0) {
    match_archive_fonts_to_bouma(acuity_summary$font, bouma_table = bouma_table) %>%
      transmute(
        font = archive_font,
        excel_font = excel_font,
        excel_bouma = bouma,
        excel_crowding_deg = crowding_deg,
        excel_xHeightReNominal = xHeightReNominal,
        excel_fontSpacingReNominal = fontSpacingReNominal,
        excel_font_category = font_category,
        excel_sd_log_bouma = sd_log_bouma,
        excel_N_crowding = N_crowding
      )
  } else {
    tibble::tibble(
      font = character(),
      excel_font = character(),
      excel_bouma = numeric(),
      excel_crowding_deg = numeric(),
      excel_xHeightReNominal = numeric(),
      excel_fontSpacingReNominal = numeric(),
      excel_font_category = character(),
      excel_sd_log_bouma = numeric(),
      excel_N_crowding = integer()
    )
  }

  acuity_summary %>%
    left_join(excel_map, by = "font") %>%
    left_join(
      arch_crowd %>%
        transmute(
          font,
          archive_crowding_deg = crowding_deg,
          archive_sd_log_crowding = sd_log_crowding,
          archive_N_crowding = N_crowding
        ),
      by = "font"
    ) %>%
    mutate(
      crowding_source = dplyr::case_when(
        is.finite(archive_crowding_deg) & archive_crowding_deg > 0 ~ "archive",
        is.finite(excel_crowding_deg) & excel_crowding_deg > 0 ~ "excel",
        TRUE ~ NA_character_
      ),
      crowding_deg = dplyr::if_else(
        crowding_source == "archive",
        archive_crowding_deg,
        excel_crowding_deg
      ),
      # Bouma for Bouma-axis plots: archive crowding / 5°, else Excel Bouma.
      bouma = dplyr::if_else(
        crowding_source == "archive",
        crowding_deg / abs(CROWDING24_ECCENTRICITY_DEG),
        excel_bouma
      ),
      # Spacing & x-height: archive CSV first, else Excel F/G.
      fontSpacingReNominal = dplyr::coalesce(
        archive_fontSpacingReNominal,
        excel_fontSpacingReNominal
      ),
      xHeightReNominal = dplyr::coalesce(
        archive_xHeightReNominal,
        excel_xHeightReNominal
      ),
      font_category = excel_font_category,
      excel_font = dplyr::coalesce(excel_font, font_comparison_axis_label(font)),
      sd_log_bouma = dplyr::if_else(
        crowding_source == "archive",
        archive_sd_log_crowding,
        excel_sd_log_bouma
      ),
      N_crowding = dplyr::if_else(
        crowding_source == "archive",
        as.integer(archive_N_crowding),
        as.integer(excel_N_crowding)
      ),
      crowding_over_spacing = crowding_deg / fontSpacingReNominal,
      acuity_over_bbox = acuityBoundingBoxWidthDeg / fontBoundingBoxWidthReNominal,
      r = crowding_over_spacing / acuity_over_bbox,
      # Acuity x-height from archive geometry
      acuityXHeightDeg = acuityBoundingBoxWidthDeg *
        archive_xHeightReNominal / fontBoundingBoxWidthReNominal,
      # Crowding x-height: crowdingSpacingDeg prefers archive thresholds when
      # present (else Excel Bouma×5°); × (x-height / spacing) prefers archive
      # CSV metrics, else Excel F/G.
      crowdingSpacingDeg = crowding_deg,
      crowdingXHeightDeg = dplyr::if_else(
        is.finite(crowdingSpacingDeg) & crowdingSpacingDeg > 0 &
          is.finite(xHeightReNominal) & xHeightReNominal > 0 &
          is.finite(fontSpacingReNominal) & fontSpacingReNominal > 0,
        crowdingSpacingDeg * xHeightReNominal / fontSpacingReNominal,
        NA_real_
      ),
      se_log_crowding = dplyr::if_else(
        is.finite(sd_log_bouma) & is.finite(N_crowding) & N_crowding > 0,
        sd_log_bouma / sqrt(N_crowding),
        NA_real_
      ),
      se_log_acuity = dplyr::if_else(
        is.finite(sd_log_acuity) & is.finite(N_acuity) & N_acuity > 1,
        sd_log_acuity / sqrt(N_acuity),
        NA_real_
      ),
      se_log_r = sqrt(
        dplyr::coalesce(se_log_crowding, 0)^2 +
          dplyr::coalesce(se_log_acuity, 0)^2
      ),
      se_log_r = dplyr::if_else(
        is.finite(se_log_crowding) | is.finite(se_log_acuity),
        se_log_r,
        NA_real_
      ),
      log10_r = log10(r),
      r_lo = 10^(log10_r - se_log_r),
      r_hi = 10^(log10_r + se_log_r),
      se_log_bouma = se_log_crowding,
      log10_bouma = log10(bouma),
      bouma_lo = 10^(log10_bouma - se_log_bouma),
      bouma_hi = 10^(log10_bouma + se_log_bouma),
      log10_acuity_xheight = log10(acuityXHeightDeg),
      acuityXHeight_lo = 10^(log10_acuity_xheight - se_log_bouma),
      acuityXHeight_hi = 10^(log10_acuity_xheight + se_log_bouma)
    ) %>%
    filter(
      !is.na(crowding_source),
      is.finite(r), r > 0,
      is.finite(crowding_deg), crowding_deg > 0,
      is.finite(bouma), bouma > 0,
      is.finite(fontSpacingReNominal), fontSpacingReNominal > 0
    )
}

font_scatter_legend_cols <- function(summary_data, font_colors = NULL) {
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
  list(data = summary_data, cols = cols)
}

# One point per font: Crowding:Acuity size ratio r vs Bouma factor,
# with vertical ±SE(log r) error bars on the log scale.
crowding_acuity_size_ratio_vs_bouma_scatter <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  styled <- font_scatter_legend_cols(summary_data, font_colors)
  summary_data <- styled$data
  cols <- styled$cols

  # Equal log-decade length on x and y; pad for error-bar extent.
  y_vals <- c(summary_data$r, summary_data$r_lo, summary_data$r_hi)
  y_vals <- y_vals[is.finite(y_vals) & y_vals > 0]
  x_vals <- c(summary_data$bouma, summary_data$bouma_lo, summary_data$bouma_hi)
  x_vals <- x_vals[is.finite(x_vals) & x_vals > 0]
  x_range <- range(log10(x_vals), finite = TRUE)
  y_range <- range(log10(y_vals), finite = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  pad <- 0.05 * max(x_span, y_span, 0.1)
  half <- 0.5 * max(x_span, y_span) + pad
  x_mid <- mean(x_range)
  y_mid <- mean(y_range)
  x_lim <- 10^c(x_mid - half, x_mid + half)
  y_lim <- 10^c(y_mid - half, y_mid + half)

  ggplot(summary_data, aes(x = bouma, y = r, color = font_label)) +
    geom_errorbar(
      aes(ymin = r_lo, ymax = r_hi),
      width = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = bouma_lo, xmax = bouma_hi),
      height = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
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

# Hybrid: Crowding:Acuity size ratio r vs SD of log acuity (both log-spaced).
crowding_acuity_size_ratio_vs_sd_log_acuity_scatter <- function(df_list,
                                                                font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    filter(is.finite(sd_log_acuity), sd_log_acuity > 0, N_acuity >= 2)

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  styled <- font_scatter_legend_cols(summary_data, font_colors)
  summary_data <- styled$data
  cols <- styled$cols

  ggplot(summary_data, aes(x = sd_log_acuity, y = r, color = font_label)) +
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
      subtitle = "Crowding:Acuity size ratio vs SD of log acuity",
      x = "SD of log acuity",
      y = "Crowding:Acuity size ratio r"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Equal-aspect log10 limits covering points (and optional error-bar extents).
# X and y share the same log-span length but are centered independently.
equal_log10_limits <- function(x, y) {
  x <- x[is.finite(x) & x > 0]
  y <- y[is.finite(y) & y > 0]
  if (length(x) == 0 || length(y) == 0) {
    return(list(x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_)))
  }
  x_range <- range(log10(x), finite = TRUE)
  y_range <- range(log10(y), finite = TRUE)
  x_span <- diff(x_range)
  y_span <- diff(y_range)
  pad <- 0.05 * max(x_span, y_span, 0.1)
  half <- 0.5 * max(x_span, y_span) + pad
  list(
    x = 10^c(mean(x_range) - half, mean(x_range) + half),
    y = 10^c(mean(y_range) - half, mean(y_range) + half)
  )
}

# Identical x/y log10 limits (union of both axes) so y = x is a true diagonal.
shared_log10_limits <- function(x, y) {
  vals <- c(x, y)
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) == 0) {
    return(list(x = c(NA_real_, NA_real_), y = c(NA_real_, NA_real_)))
  }
  r <- range(log10(vals), finite = TRUE)
  pad <- 0.05 * max(diff(r), 0.1)
  lim <- 10^c(r[1] - pad, r[2] + pad)
  list(x = lim, y = lim)
}

# Expand a log10 [lo, hi] limit so value lies strictly inside (for reference lines).
expand_log10_limits_to_include <- function(lim, value, pad_frac = 0.05) {
  if (length(lim) < 2 || !all(is.finite(lim)) || !is.finite(value) || value <= 0) {
    return(lim)
  }
  log_lim <- log10(lim)
  log_v <- log10(value)
  span <- max(diff(log_lim), 0.1)
  pad <- pad_frac * span
  if (log_v <= log_lim[1]) {
    log_lim[1] <- log_v - pad
  }
  if (log_v >= log_lim[2]) {
    log_lim[2] <- log_v + pad
  }
  10^log_lim
}

# Numbered log breaks: …, 0.1, 0.3, 1, 3, 10, 30, … (no 2 or 5).
# Same rule on every axis so x/y labeling stays consistent.
log10_breaks_1_3 <- function(lims) {
  lims <- lims[is.finite(lims) & lims > 0]
  if (length(lims) == 0) {
    return(numeric())
  }
  lo <- floor(log10(min(lims))) - 1L
  hi <- ceiling(log10(max(lims))) + 1L
  br <- as.vector(outer(c(1, 3), 10^(lo:hi)))
  sort(unique(br[br >= min(lims) * 0.999 & br <= max(lims) * 1.001]))
}

# Equal log-unit length (coord_fixed), numbered breaks at 1 & 3 per decade.
# Tick lengths via guide_axis_logticks: only powers of 10 are long; every other
# decade mark (including labeled 3) uses the same ordinary short length — so
# labels do not inflate tick size the way scale major ticks would.
# Pair with theme(axis.ticks.length = unit(-…, "pt")) so ticks point into the
# panel (guide_axis_logticks defaults to outside; annotation_logticks was inside).
apply_equal_log10_scatter_scales <- function(lims) {
  logtick_guide <- ggplot2::guide_axis_logticks(
    long = 2.5,
    mid = 0.75,
    short = 0.75
  )
  list(
    scale_x_log10(
      limits = lims$x,
      breaks = log10_breaks_1_3(lims$x),
      labels = scales::label_number(accuracy = NULL),
      expand = c(0, 0),
      guide = logtick_guide
    ),
    scale_y_log10(
      limits = lims$y,
      breaks = log10_breaks_1_3(lims$y),
      labels = scales::label_number(accuracy = NULL),
      expand = c(0, 0),
      guide = logtick_guide
    ),
    coord_fixed(ratio = 1, xlim = lims$x, ylim = lims$y, expand = FALSE)
  )
}

# Crowding:Acuity size ratio r vs acuity x-height (deg), with horizontal
# ±SE from Table-1 SD(log Bouma)/sqrt(N).
crowding_acuity_size_ratio_vs_acuity_xheight_scatter <- function(df_list,
                                                                 font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  styled <- font_scatter_legend_cols(summary_data, font_colors)
  summary_data <- styled$data
  cols <- styled$cols

  lims <- equal_log10_limits(
    c(summary_data$acuityXHeightDeg, summary_data$acuityXHeight_lo, summary_data$acuityXHeight_hi),
    c(summary_data$r, summary_data$r_lo, summary_data$r_hi)
  )
  # Keep horizontal r = 1 inside the panel (not clipped when all r > 1).
  lims$y <- expand_log10_limits_to_include(lims$y, 1)

  ggplot(summary_data, aes(x = acuityXHeightDeg, y = r, color = font_label)) +
    geom_hline(
      yintercept = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_errorbar(
      aes(ymin = r_lo, ymax = r_hi),
      width = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = acuityXHeight_lo, xmax = acuityXHeight_hi),
      height = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_point(size = 3.5) +
    apply_equal_log10_scatter_scales(lims) +
    scale_color_manual(values = cols, name = "Font") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = "Crowding:Acuity size ratio vs acuity x-height",
      x = "Acuity x-height (deg)",
      y = "Crowding:Acuity size ratio r"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Same ratio vs acuity x-height plot, but each font is a two-letter abbreviation
# typeset in that font (fonts/), not a colored dot.
crowding_acuity_size_ratio_vs_acuity_xheight_font_abbrev_scatter <- function(df_list,
                                                                            font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0,
      is.finite(r), r > 0
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    mutate(
      abbr = crowding24_font_abbreviation(excel_font),
      plot_family = resolve_crowding24_plot_font_families(excel_font)
    )

  lims <- equal_log10_limits(
    c(summary_data$acuityXHeightDeg, summary_data$acuityXHeight_lo, summary_data$acuityXHeight_hi),
    c(summary_data$r, summary_data$r_lo, summary_data$r_hi)
  )
  lims$y <- expand_log10_limits_to_include(lims$y, 1)

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = r)) +
    geom_hline(
      yintercept = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_errorbar(
      aes(ymin = r_lo, ymax = r_hi),
      width = 0,
      linewidth = 0.6,
      color = "gray40",
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = acuityXHeight_lo, xmax = acuityXHeight_hi),
      height = 0,
      linewidth = 0.6,
      color = "gray40",
      na.rm = TRUE
    )
  p <- add_crowding24_font_abbrev_text(p, summary_data) +
    apply_equal_log10_scatter_scales(lims) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = "Crowding:Acuity size ratio vs acuity x-height (font abbreviations)",
      x = "Acuity x-height (deg)",
      y = "Crowding:Acuity size ratio r"
    )
  p
}

# Colors for Text (sans) / Text (serif) / Display / Script.
# Display & Script match the paper figure; Text is split into two hues.
CROWDING24_FONT_CATEGORY_COLORS <- c(
  `Text (sans serif)` = "#5CAFA9",
  `Text (serif)` = "#3A7CA5",
  Display = "#A84464",
  Script = "#F4A4A0"
)

crowding_acuity_size_ratio_vs_acuity_xheight_by_category_scatter <- function(df_list,
                                                                            font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  category_levels <- c(
    "Text (sans serif)",
    "Text (serif)",
    "Display",
    "Script"
  )

  summary_data <- summary_data %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0,
      !is.na(font_category), font_category != "",
      font_category %in% category_levels
    ) %>%
    mutate(
      font_category = factor(font_category, levels = category_levels)
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  lims <- equal_log10_limits(
    c(summary_data$acuityXHeightDeg, summary_data$acuityXHeight_lo, summary_data$acuityXHeight_hi),
    c(summary_data$r, summary_data$r_lo, summary_data$r_hi)
  )
  # Keep horizontal r = 1 inside the panel (not clipped when all r > 1).
  lims$y <- expand_log10_limits_to_include(lims$y, 1)

  cols <- CROWDING24_FONT_CATEGORY_COLORS[
    intersect(names(CROWDING24_FONT_CATEGORY_COLORS), levels(summary_data$font_category))
  ]

  ggplot(summary_data, aes(x = acuityXHeightDeg, y = r, color = font_category)) +
    geom_hline(
      yintercept = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_errorbar(
      aes(ymin = r_lo, ymax = r_hi),
      width = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = acuityXHeight_lo, xmax = acuityXHeight_hi),
      height = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    geom_point(size = 3.5) +
    apply_equal_log10_scatter_scales(lims) +
    scale_color_manual(values = cols, name = "Font group", drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = paste0(
        "Crowding:Acuity size ratio vs acuity x-height\n",
        "colored by font group"
      ),
      x = "Acuity x-height (deg)",
      y = "Crowding:Acuity size ratio r"
    ) +
    guides(color = guide_legend(title = "Font group", nrow = 1))
}

# Crowding x-height vs acuity x-height (both deg, log-spaced).
# Each font is a two-letter abbreviation typeset in that font (fonts/), not a
# colored dot. Crowding x-height uses archive crowding when available.
crowding_xheight_vs_acuity_xheight_scatter <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(crowdingXHeightDeg), crowdingXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  summary_data <- summary_data %>%
    mutate(
      abbr = crowding24_font_abbreviation(excel_font),
      plot_family = resolve_crowding24_plot_font_families(excel_font)
    )

  lims <- shared_log10_limits(
    summary_data$acuityXHeightDeg,
    summary_data$crowdingXHeightDeg
  )

  n_arch <- sum(summary_data$crowding_source == "archive", na.rm = TRUE)
  n_excel <- sum(summary_data$crowding_source == "excel", na.rm = TRUE)
  subtitle <- if (n_arch > 0 && n_excel > 0) {
    "Crowding x-height vs acuity x-height\n(archive crowding when available; else Bouma×5°)"
  } else if (n_arch > 0) {
    "Crowding x-height vs acuity x-height (from archive)"
  } else {
    "Crowding x-height vs acuity x-height (Bouma×5° from Table 1)"
  }

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = crowdingXHeightDeg)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    )
  p <- add_crowding24_font_abbrev_text(p, summary_data) +
    apply_equal_log10_scatter_scales(lims) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = subtitle,
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    )
  p
}

# Same axes as crowding_xheight_vs_acuity_xheight_scatter, colored by font group.
crowding_xheight_vs_acuity_xheight_by_category_scatter <- function(df_list,
                                                                   font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  category_levels <- c(
    "Text (sans serif)",
    "Text (serif)",
    "Display",
    "Script"
  )

  summary_data <- summary_data %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(crowdingXHeightDeg), crowdingXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0,
      !is.na(font_category), font_category != "",
      font_category %in% category_levels
    ) %>%
    mutate(
      font_category = factor(font_category, levels = category_levels)
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  lims <- shared_log10_limits(
    summary_data$acuityXHeightDeg,
    summary_data$crowdingXHeightDeg
  )

  cols <- CROWDING24_FONT_CATEGORY_COLORS[
    intersect(names(CROWDING24_FONT_CATEGORY_COLORS), levels(summary_data$font_category))
  ]

  ggplot(summary_data, aes(x = acuityXHeightDeg, y = crowdingXHeightDeg, color = font_category)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_point(size = 3.5) +
    apply_equal_log10_scatter_scales(lims) +
    scale_color_manual(values = cols, name = "Font group", drop = FALSE) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = paste0(
        "Crowding x-height vs acuity x-height\n",
        "colored by font group"
      ),
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    ) +
    guides(color = guide_legend(title = "Font group", nrow = 1))
}

# Alias kept for older call sites / plot lists.
crowding_xheight_vs_acuity_xheight_font_abbrev_scatter <- function(df_list,
                                                                   font_colors = NULL) {
  crowding_xheight_vs_acuity_xheight_scatter(df_list, font_colors = font_colors)
}

# Shared prep for Crowding:Acuity ratio r histograms (one value per font).
prepare_crowding_acuity_ratio_r_hist_data <- function(df_list) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(summary_data)
  }
  summary_data %>%
    filter(is.finite(r), r > 0) %>%
    mutate(
      is_sloan = grepl(
        "sloan",
        normalize_font_match_key(dplyr::coalesce(excel_font, font)),
        ignore.case = TRUE
      ),
      plot_category = dplyr::case_when(
        is_sloan ~ "Sloan",
        font_category %in% names(CROWDING24_FONT_CATEGORY_COLORS) ~ font_category,
        TRUE ~ NA_character_
      )
    )
}

# Log-x limits for ratio-r histograms; always long enough to include r = 1.
ratio_r_hist_log10_limits <- function(r, pad_frac = 0.05) {
  r <- r[is.finite(r) & r > 0]
  if (length(r) == 0) {
    return(c(0.5, 2))
  }
  lim <- range(r, finite = TRUE)
  lim <- expand_log10_limits_to_include(lim, 1, pad_frac = pad_frac)
  # Slight pad so edge bars/dots are not clipped.
  log_lim <- log10(lim)
  span <- max(diff(log_lim), 0.1)
  pad <- pad_frac * span
  10^c(log_lim[1] - pad, log_lim[2] + pad)
}

ratio_r_hist_log_breaks <- function(lims, n_bins = 12L) {
  lims <- lims[is.finite(lims) & lims > 0]
  if (length(lims) < 2) {
    return(numeric())
  }
  10^seq(log10(min(lims)), log10(max(lims)), length.out = n_bins + 1L)
}

# Bar histogram of Crowding:Acuity size ratio r (one bar-bin count of fonts), log x.
crowding_acuity_size_ratio_r_histogram <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_ratio_r_hist_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  lims <- ratio_r_hist_log10_limits(summary_data$r)
  breaks <- ratio_r_hist_log_breaks(lims)
  logtick_guide <- ggplot2::guide_axis_logticks(
    long = 2.5,
    mid = 0.75,
    short = 0.75
  )

  ggplot(summary_data, aes(x = r)) +
    geom_histogram(
      breaks = breaks,
      fill = "gray80",
      color = NA,
      closed = "left"
    ) +
    scale_x_log10(
      limits = lims,
      breaks = log10_breaks_1_3(lims),
      labels = scales::label_number(accuracy = NULL),
      expand = c(0, 0),
      guide = logtick_guide
    ) +
    scale_y_continuous(
      breaks = function(lim) {
        lo <- max(0L, floor(min(lim, na.rm = TRUE)))
        hi <- ceiling(max(lim, na.rm = TRUE))
        if (!is.finite(lo) || !is.finite(hi) || hi < lo) {
          return(numeric())
        }
        seq.int(lo, hi)
      },
      expand = expansion(mult = c(0, 0.08)),
      guide = guide_axis(check.overlap = FALSE)
    ) +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = "Histogram of Crowding:Acuity size ratio r",
      x = "Crowding:Acuity size ratio r",
      y = "Number of fonts"
    )
}

# Same bins as the bar histogram, but each font is a stacked dot colored by
# font group (Text sans / Text serif / Display / Script). Sloan is black.
crowding_acuity_size_ratio_r_dot_histogram <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_ratio_r_hist_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  lims <- ratio_r_hist_log10_limits(summary_data$r)
  breaks <- ratio_r_hist_log_breaks(lims)
  if (length(breaks) < 2) {
    return(NULL)
  }

  log_breaks <- log10(breaks)
  summary_data <- summary_data %>%
    mutate(
      log_r = log10(r),
      bin = cut(
        log_r,
        breaks = log_breaks,
        include.lowest = TRUE,
        right = FALSE,
        labels = FALSE
      )
    ) %>%
    filter(!is.na(bin)) %>%
    group_by(bin) %>%
    mutate(
      stack_y = dplyr::row_number(r),
      # Place dots at geometric center of each log bin.
      r_bin = 10^((log_breaks[bin] + log_breaks[bin + 1L]) / 2)
    ) %>%
    ungroup()

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  category_levels <- c(
    "Text (sans serif)",
    "Text (serif)",
    "Display",
    "Script",
    "Sloan"
  )
  cols <- c(
    CROWDING24_FONT_CATEGORY_COLORS,
    Sloan = "black"
  )

  summary_data <- summary_data %>%
    mutate(
      plot_category = factor(
        dplyr::if_else(
          is_sloan,
          "Sloan",
          as.character(plot_category)
        ),
        levels = category_levels
      )
    ) %>%
    filter(!is.na(plot_category))

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  cols <- cols[intersect(names(cols), levels(summary_data$plot_category))]
  logtick_guide <- ggplot2::guide_axis_logticks(
    long = 2.5,
    mid = 0.75,
    short = 0.75
  )

  ggplot(summary_data, aes(x = r_bin, y = stack_y, color = plot_category)) +
    geom_point(size = 3.2, alpha = 0.95) +
    scale_x_log10(
      limits = lims,
      breaks = log10_breaks_1_3(lims),
      labels = scales::label_number(accuracy = NULL),
      expand = c(0, 0),
      guide = logtick_guide
    ) +
    scale_y_continuous(
      breaks = seq_len(max(1L, max(summary_data$stack_y, na.rm = TRUE))),
      expand = expansion(mult = c(0, 0.08)),
      guide = guide_axis(check.overlap = FALSE)
    ) +
    scale_color_manual(values = cols, name = "Font group", drop = FALSE) +
    coord_cartesian(ylim = c(0.5, max(summary_data$stack_y) + 0.5)) +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Negative length → ticks inside the panel (like annotation_logticks).
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = paste0(
        "Crowding:Acuity size ratio r\n",
        "one dot per font, colored by font group"
      ),
      x = "Crowding:Acuity size ratio r",
      y = "Number of fonts"
    ) +
    guides(color = guide_legend(title = "Font group", nrow = 1, override.aes = list(size = 3)))
}
