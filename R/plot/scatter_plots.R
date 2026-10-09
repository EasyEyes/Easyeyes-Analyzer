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
  # Names as printed in the paper's font-set legend (Fig. 1).
  legend_name = c(
    "Caslon", "Agoesa", "Arial", "Baskerville", "Courier", "Edwardian",
    "Extenda", "Frutiger", "Georgia", "Haut Relief", "Le Monde", "Letraflex",
    "LiebeLotte", "Museo", "Omfug", "Optimistic", "Proxima Nova",
    "Rollerscript", "Sabon", "Scarlet Wood", "TheSans", "Times New Roman",
    "Tiny", "Zapfino"
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
      # Sloan has capital letters only.
      legend_name = "SLOAN",
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

  # Registration is idempotent and ee_* names are private to these plots, so
  # skip faces already registered with the same path (systemfonts::register_font
  # costs ~12 ms/font; re-doing 24 faces on every display+download render adds up).
  sysfonts_known <- if (requireNamespace("sysfonts", quietly = TRUE)) {
    tryCatch(sysfonts::font_families(), error = function(e) character())
  } else {
    character()
  }
  systemfonts_known <- if (requireNamespace("systemfonts", quietly = TRUE)) {
    tryCatch({
      reg <- systemfonts::registry_fonts()
      stats::setNames(
        normalizePath(as.character(reg$path), winslash = "/", mustWork = FALSE),
        as.character(reg$family)
      )
    }, error = function(e) character())
  } else {
    character()
  }

  register_one <- function(family, path) {
    if (!is.character(family) || !nzchar(family)) return(invisible(NULL))
    if (!is.character(path) || length(path) != 1 || is.na(path) || !file.exists(path)) {
      return(invisible(NULL))
    }
    path_norm <- normalizePath(path, winslash = "/", mustWork = FALSE)
    .crowding24_registered_fonts[[family]] <- path_norm
    # showtext (emojifont) looks up fonts via sysfonts, not systemfonts::register_font.
    if (requireNamespace("sysfonts", quietly = TRUE) && !(family %in% sysfonts_known)) {
      tryCatch(
        sysfonts::font_add(family, regular = path),
        error = function(e) invisible(NULL)
      )
    }
    # Also register with systemfonts for ragg when showtext is not intercepting.
    already <- family %in% names(systemfonts_known) &&
      identical(unname(systemfonts_known[[family]]), path_norm)
    if (requireNamespace("systemfonts", quietly = TRUE) && !already) {
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

# Called after each ee_* render. Registrations are kept: ee_* aliases are
# private to these plots, and clearing the registry forced a full
# re-registration (~0.3 s) on every subsequent display/download render.
release_crowding24_plot_fonts <- function() {
  invisible(NULL)
}

plot_uses_crowding24_ee_fonts <- function(plot) {
  length(crowding24_ee_families_in_plot(plot)) > 0
}

crowding24_ee_families_in_plot <- function(plot) {
  if (is.null(plot)) {
    return(character())
  }
  tagged <- attr(plot, "crowding24_ee_families", exact = TRUE)
  if (is.character(tagged) && length(tagged) > 0) {
    return(unique(tagged[startsWith(tagged, "ee_") & !is.na(tagged)]))
  }
  # patchwork: walk child plots (and the assembled ggplot layers if present).
  if (inherits(plot, "patchwork")) {
    fams <- character()
    children <- tryCatch(plot$patches$plots, error = function(e) NULL)
    if (is.list(children)) {
      for (child in children) {
        fams <- c(fams, crowding24_ee_families_in_plot(child))
      }
    }
  } else {
    fams <- character()
  }
  if (!inherits(plot, "ggplot")) {
    return(unique(fams))
  }
  for (layer in plot$layers) {
    fam <- layer$aes_params$family
    if (is.null(fam)) fam <- layer$geom_params$family
    if (is.character(fam)) {
      fams <- c(fams, fam[startsWith(fam, "ee_") & !is.na(fam)])
    }
    # Mapped family (aes(family = plot_family)) lives in layer data, not params.
    ld <- tryCatch(layer$data, error = function(e) NULL)
    if (is.data.frame(ld) && "plot_family" %in% names(ld)) {
      pf <- as.character(ld$plot_family)
      fams <- c(fams, pf[startsWith(pf, "ee_") & !is.na(pf)])
    }
  }
  unique(fams)
}

# Tag a plot/patchwork so PNG render registers these ee_* faces.
tag_crowding24_ee_families <- function(plot, families) {
  families <- unique(as.character(families))
  families <- families[startsWith(families, "ee_") & !is.na(families) & nzchar(families)]
  attr(plot, "crowding24_ee_families") <- families
  plot
}

# Measure lowercase "x" height (px at size/res) for an ee_* family or font file.
crowding24_measure_x_glyph_height <- function(family,
                                              path = NULL,
                                              size = 100,
                                              res = 72) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    return(NA_real_)
  }
  if (is.null(path) || !is.character(path) || !nzchar(path) || !file.exists(path)) {
    path <- tryCatch(
      .crowding24_registered_fonts[[family]],
      error = function(e) NULL
    )
  }
  cache_key <- paste0(
    "xheight:",
    if (is.character(path) && length(path) == 1 && nzchar(path)) path else as.character(family)[1],
    ":s", size, ":r", res
  )
  cached <- tryCatch(
    .crowding24_registered_fonts[[cache_key]],
    error = function(e) NULL
  )
  if (is.numeric(cached) && length(cached) == 1L && is.finite(cached) && cached > 0) {
    return(cached)
  }
  g <- tryCatch({
    if (is.character(path) && length(path) == 1 && nzchar(path) && file.exists(path)) {
      systemfonts::glyph_info("x", path = path, size = size, res = res)
    } else {
      systemfonts::glyph_info("x", family = family, size = size, res = res)
    }
  }, error = function(e) NULL)
  if (is.null(g) || nrow(g) == 0) {
    return(NA_real_)
  }
  h <- suppressWarnings(as.numeric(g$height[[1]]))
  if (!is.finite(h) || h <= 0) {
    return(NA_real_)
  }
  .crowding24_registered_fonts[[cache_key]] <- h
  h
}

# ggplot size so each family's lowercase x has the same visual height.
# base_size is the size for a font whose measured x-height equals the reference.
crowding24_equalize_xheight_sizes <- function(families,
                                              base_size = 9,
                                              paths = NULL,
                                              xheight_re_nominal = NULL) {
  families <- as.character(families)
  n <- length(families)
  out <- rep(as.numeric(base_size)[1], n)
  if (n == 0) {
    return(out)
  }
  if (is.null(paths)) {
    paths <- lapply(families, function(fam) {
      tryCatch(.crowding24_registered_fonts[[fam]], error = function(e) NA_character_)
    })
    paths <- vapply(paths, function(p) {
      if (is.character(p) && length(p) == 1 && !is.na(p)) p else NA_character_
    }, character(1))
  } else {
    paths <- as.character(paths)
    if (length(paths) == 1L) paths <- rep(paths, n)
  }

  heights <- mapply(
    crowding24_measure_x_glyph_height,
    family = families,
    path = paths,
    SIMPLIFY = TRUE,
    USE.NAMES = FALSE
  )

  # Fallback: archive/Excel x-height over nominal (same units across fonts).
  if (!is.null(xheight_re_nominal)) {
    xh <- suppressWarnings(as.numeric(xheight_re_nominal))
    if (length(xh) == 1L) xh <- rep(xh, n)
    miss <- !is.finite(heights) | heights <= 0
    if (any(miss) && any(is.finite(xh) & xh > 0)) {
      # Scale so median measured height matches median nominal ratio.
      ref_h <- stats::median(heights[is.finite(heights) & heights > 0], na.rm = TRUE)
      ref_x <- stats::median(xh[is.finite(xh) & xh > 0], na.rm = TRUE)
      if (is.finite(ref_h) && is.finite(ref_x) && ref_x > 0) {
        heights[miss] <- ref_h * (xh[miss] / ref_x)
      } else {
        heights[miss] <- xh[miss]
      }
    }
  }

  ok <- is.finite(heights) & heights > 0
  if (!any(ok)) {
    return(out)
  }
  ref <- stats::median(heights[ok], na.rm = TRUE)
  if (!is.finite(ref) || ref <= 0) {
    return(out)
  }
  out[ok] <- base_size * (ref / heights[ok])
  # Keep sizes in a sane range if a font reports a tiny x.
  out <- pmin(pmax(out, base_size * 0.45), base_size * 2.5)
  out
}

# Light collision nudge in log10 space; total move capped at max_frac of the
# larger axis span (default 10%). Returns data-space x/y.
crowding24_nudge_abbrevs_log10 <- function(x,
                                           y,
                                           max_frac = 0.1,
                                           iterations = 50,
                                           seed = 42) {
  x <- as.numeric(x)
  y <- as.numeric(y)
  n <- length(x)
  out_x <- x
  out_y <- y
  if (n == 0) {
    return(list(x = out_x, y = out_y))
  }
  lx <- log10(x)
  ly <- log10(y)
  ok <- which(is.finite(lx) & is.finite(ly))
  if (length(ok) < 2L) {
    return(list(x = out_x, y = out_y))
  }

  rx <- diff(range(lx[ok], finite = TRUE))
  ry <- diff(range(ly[ok], finite = TRUE))
  span <- max(rx, ry, 1e-6)
  max_d <- max(as.numeric(max_frac)[1], 0) * span
  if (!is.finite(max_d) || max_d <= 0) {
    return(list(x = out_x, y = out_y))
  }
  # Only push labels that are closer than this (in log10 units).
  min_dist <- 0.45 * max_d

  dx <- rep(0, n)
  dy <- rep(0, n)
  set.seed(seed)
  for (iter in seq_len(iterations)) {
    for (ii in seq_along(ok)) {
      i <- ok[[ii]]
      if (ii == length(ok)) next
      for (jj in (ii + 1L):length(ok)) {
        j <- ok[[jj]]
        ddx <- (lx[i] + dx[i]) - (lx[j] + dx[j])
        ddy <- (ly[i] + dy[i]) - (ly[j] + dy[j])
        dist <- sqrt(ddx * ddx + ddy * ddy) + 1e-12
        if (dist >= min_dist) next
        push <- 0.2 * (min_dist - dist) / dist
        dx[i] <- dx[i] + push * ddx
        dy[i] <- dy[i] + push * ddy
        dx[j] <- dx[j] - push * ddx
        dy[j] <- dy[j] - push * ddy
      }
    }
    mag <- sqrt(dx * dx + dy * dy)
    too <- which(mag > max_d & mag > 0)
    if (length(too) > 0) {
      dx[too] <- dx[too] * (max_d / mag[too])
      dy[too] <- dy[too] * (max_d / mag[too])
    }
  }

  out_x[ok] <- 10^(lx[ok] + dx[ok])
  out_y[ok] <- 10^(ly[ok] + dy[ok])
  list(x = out_x, y = out_y)
}

# Add font-abbrev labels (one geom_text layer per ee_* family for showtext/ragg).
# When repel=TRUE, labels may move at most max_move_frac of the log-axis span
# (default 10%) to reduce collisions.
add_crowding24_font_abbrev_text <- function(plot,
                                            data,
                                            size = 4.5,
                                            equalize_xheight = FALSE,
                                            repel = FALSE,
                                            max_move_frac = 0.1,
                                            seed = 42) {
  if (is.null(data) || nrow(data) == 0) {
    return(plot)
  }
  data <- as.data.frame(data)
  if (!"plot_family" %in% names(data)) {
    data$plot_family <- "sans"
  }
  data$plot_family <- as.character(data$plot_family)
  data$plot_family[is.na(data$plot_family) | !nzchar(data$plot_family)] <- "sans"

  if (!"abbrev_size" %in% names(data)) {
    if (isTRUE(equalize_xheight)) {
      xh_col <- if ("archive_xHeightReNominal" %in% names(data)) {
        data$archive_xHeightReNominal
      } else if ("xHeightReNominal" %in% names(data)) {
        data$xHeightReNominal
      } else {
        NULL
      }
      data$abbrev_size <- crowding24_equalize_xheight_sizes(
        data$plot_family,
        base_size = size,
        xheight_re_nominal = xh_col
      )
    } else {
      data$abbrev_size <- size
    }
  }

  # Resolve x/y columns from the plot mapping (log–log abbrev scatters).
  xvar <- tryCatch(rlang::as_name(plot$mapping$x), error = function(e) NA_character_)
  yvar <- tryCatch(rlang::as_name(plot$mapping$y), error = function(e) NA_character_)
  if (isTRUE(repel) &&
      is.character(xvar) && nzchar(xvar) && xvar %in% names(data) &&
      is.character(yvar) && nzchar(yvar) && yvar %in% names(data)) {
    nudged <- crowding24_nudge_abbrevs_log10(
      data[[xvar]],
      data[[yvar]],
      max_frac = max_move_frac,
      seed = seed
    )
    data$abbrev_x <- nudged$x
    data$abbrev_y <- nudged$y
  } else {
    data$abbrev_x <- if (is.character(xvar) && xvar %in% names(data)) data[[xvar]] else NA_real_
    data$abbrev_y <- if (is.character(yvar) && yvar %in% names(data)) data[[yvar]] else NA_real_
  }

  # One layer per family so showtext/ragg pick the right face (family-as-aes
  # is less reliable). Positions already lightly nudged when repel=TRUE.
  fams <- unique(data$plot_family)
  for (fam in fams) {
    layer_data <- data[data$plot_family == fam, , drop = FALSE]
    if (nrow(layer_data) == 0) next
    if (all(is.finite(layer_data$abbrev_x)) && all(is.finite(layer_data$abbrev_y))) {
      plot <- plot +
        ggplot2::geom_text(
          data = layer_data,
          ggplot2::aes(x = abbrev_x, y = abbrev_y, label = abbr, size = abbrev_size),
          family = fam,
          color = "black",
          show.legend = FALSE,
          inherit.aes = FALSE
        )
    } else {
      plot <- plot +
        ggplot2::geom_text(
          data = layer_data,
          ggplot2::aes(label = abbr, size = abbrev_size),
          family = fam,
          color = "black",
          show.legend = FALSE,
          inherit.aes = TRUE
        )
    }
  }
  plot + ggplot2::scale_size_identity()
}

normalize_font_match_key <- function(fonts) {
  fonts <- as.character(fonts)
  fonts <- gsub("\u00AD", "", fonts, fixed = TRUE) # soft hyphen
  fonts <- strip_font_filetype(fonts)
  fonts <- tolower(trimws(fonts))
  fonts <- gsub("[^a-z0-9]+", "", fonts)
  fonts
}

# Cache for parsed Table 1 (readxl costs ~0.1 s per call and this is read by
# every Crowding24 plot); keyed on path + mtime + eccentricity.
.crowding24_bouma_cache <- new.env(parent = emptyenv())

load_crowding24_bouma_table <- function(path = CROWDING24_BOUMA_PATH,
                                        eccentricity_deg = CROWDING24_ECCENTRICITY_DEG) {
  mtime <- if (file.exists(path)) as.numeric(file.info(path)$mtime) else NA_real_
  cache_key <- paste(path, mtime, eccentricity_deg, sep = "|")
  cached <- .crowding24_bouma_cache[[cache_key]]
  if (!is.null(cached)) {
    return(cached)
  }
  out <- load_crowding24_bouma_table_uncached(path, eccentricity_deg)
  rm(list = ls(.crowding24_bouma_cache), envir = .crowding24_bouma_cache)
  .crowding24_bouma_cache[[cache_key]] <- out
  out
}

load_crowding24_bouma_table_uncached <- function(path = CROWDING24_BOUMA_PATH,
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

# Shared per-font acuity vs crowding (deg). Crowding prefers archive; else Bouma×5°.
prepare_acuity_vs_crowding_by_font_data <- function(df_list) {
  empty <- tibble::tibble()
  acuity <- df_list$acuity
  if (is.null(acuity) || nrow(acuity) == 0) {
    return(empty)
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
    return(empty)
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
    return(empty)
  }

  summary_data %>%
    mutate(
      font_label = font_comparison_axis_label(font),
      font_label = factor(font_label, levels = sort(unique(font_label)))
    )
}

# One point per font: acuity (y) vs crowding (x), dashed y = x.
# Crowding prefers archive thresholds when present; else Excel Bouma×5°.
acuity_vs_crowding_by_font_scatter <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_acuity_vs_crowding_by_font_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

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

  lims <- shared_log10_limits(
    summary_data$crowding_deg,
    summary_data$geomean_acuity
  )

  ggplot(summary_data, aes(x = crowding_deg, y = geomean_acuity, color = font_label)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
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
      subtitle = "Acuity vs Crowding",
      x = "Crowding (deg)",
      y = "Acuity (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Same data as acuity_vs_crowding_by_font_scatter with axes swapped:
# crowding (y) vs acuity (x), dashed y = x.
crowding_vs_acuity_by_font_scatter <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_acuity_vs_crowding_by_font_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

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

  lims <- shared_log10_limits(
    summary_data$geomean_acuity,
    summary_data$crowding_deg
  )

  ggplot(summary_data, aes(x = geomean_acuity, y = crowding_deg, color = font_label)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
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
      axis.ticks.length = unit(-4, "pt")
    ) +
    labs(
      subtitle = "Crowding vs Acuity",
      x = "Acuity (deg)",
      y = "Crowding (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Build per-font Crowding:acuity size ratio table (shared by ratio scatters).
# r = (crowdingThresholdDeg / fontSpacingReNominal) /
#     (acuityDeg / fontBoundingBoxWidthReNominal)
# Crowding threshold: archive geometric mean when available, else Excel Bouma×5°.
# Spacing / x-height for crowding size: archive CSV when available, else Excel F/G.
# SE(log r): archive SD(log crowding)/sqrt(N) or Table-1 SD(log Bouma)/sqrt(N),
# plus sample SD(log acuity)/sqrt(N_acuity).
# Memoized: ~14 plots/limit helpers per Plots-tab build call this with the same
# df_list. Single-entry cache keyed on a hash of only the columns used below
# (keep CROWDING24_RATIO_DATA_ACUITY_COLS in sync when adding inputs).
.crowding24_ratio_data_cache <- new.env(parent = emptyenv())
CROWDING24_RATIO_DATA_ACUITY_COLS <- c(
  "font", "questMeanAtEndOfTrialsLoop",
  "fontBoundingBoxWidthReNominal", "fontBoundingBoxReNominalRect", "pxPerCm",
  "fontBoundingBoxHeightReNominal", "fontCharacterSetHeightReNominal",
  "fontXHeightReNominal", "fontSpacingReNominal"
)
CROWDING24_RATIO_DATA_CROWDING_COLS <- c("font", "log_crowding_distance_deg")

crowding24_ratio_data_cache_key <- function(df_list) {
  pick <- function(df, cols) {
    if (is.null(df) || !is.data.frame(df)) return(NULL)
    df[intersect(cols, names(df))]
  }
  bouma_path <- CROWDING24_BOUMA_PATH
  bouma_mtime <- if (file.exists(bouma_path)) as.numeric(file.info(bouma_path)$mtime) else NA_real_
  tryCatch(
    rlang::hash(list(
      acuity = pick(df_list$acuity, CROWDING24_RATIO_DATA_ACUITY_COLS),
      crowding = pick(df_list$crowding, CROWDING24_RATIO_DATA_CROWDING_COLS),
      bouma = c(bouma_path, bouma_mtime)
    )),
    error = function(e) NULL
  )
}

prepare_crowding_acuity_size_ratio_data <- function(df_list) {
  key <- crowding24_ratio_data_cache_key(df_list)
  if (!is.null(key)) {
    cached <- .crowding24_ratio_data_cache[[key]]
    if (!is.null(cached)) {
      return(cached)
    }
  }
  out <- prepare_crowding_acuity_size_ratio_data_uncached(df_list)
  if (!is.null(key)) {
    rm(list = ls(.crowding24_ratio_data_cache), envir = .crowding24_ratio_data_cache)
    .crowding24_ratio_data_cache[[key]] <- out
  }
  out
}

prepare_crowding_acuity_size_ratio_data_uncached <- function(df_list) {
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

# Hybrid: Crowding:acuity size ratio r vs SD of log acuity (both log-spaced).
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
      subtitle = "Crowding:acuity size ratio vs SD of log acuity",
      x = "SD of log acuity",
      y = "Crowding:acuity size ratio r"
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

# Clip an acuity-x-height X limit so the axis ends at exactly xmax (default 3 deg).
# Pads only the low end so 0.3 stays inside; never pads past xmax (with
# expand = FALSE on the scale/coord, the right edge is exactly xmax).
clip_acuity_xheight_xlim_at_3 <- function(x_lim, xmax = 3, pad_frac = 0.05) {
  x_lim <- suppressWarnings(as.numeric(x_lim))
  if (length(x_lim) < 2L || !all(is.finite(x_lim)) || any(x_lim <= 0)) {
    x_lim <- c(xmax / 100, xmax)
  }
  lo <- min(x_lim[1], xmax)
  if (!(is.finite(lo) && lo > 0 && lo < xmax)) {
    lo <- xmax / 100
  }
  log_hi <- log10(xmax)
  log_lo <- log10(lo)
  log_tick_lo <- log10(0.3)
  span <- max(log_hi - min(log_lo, log_tick_lo), 0.1)
  pad <- pad_frac * span
  if (log_lo >= log_tick_lo - pad) {
    log_lo <- log_tick_lo - pad
  }
  c(10^log_lo, xmax)
}

# Expand log10 [lo, hi] to at least target_span while keeping values in `keep` inside.
expand_log10_limits_to_span <- function(lim, target_span, keep = NULL) {
  lim <- suppressWarnings(as.numeric(lim))
  lim <- lim[is.finite(lim) & lim > 0]
  if (length(lim) < 1L) {
    return(c(NA_real_, NA_real_))
  }
  lim <- range(lim)
  log_lim <- log10(lim)
  if (!is.finite(target_span) || target_span <= 0) {
    target_span <- max(diff(log_lim), 0.1)
  }
  if (diff(log_lim) < target_span) {
    mid <- mean(log_lim)
    log_lim <- c(mid - target_span / 2, mid + target_span / 2)
  }
  keep <- suppressWarnings(as.numeric(keep))
  keep <- keep[is.finite(keep) & keep > 0]
  for (v in keep) {
    lv <- log10(v)
    if (lv < log_lim[1]) {
      log_lim[1] <- lv
      log_lim[2] <- max(log_lim[2], log_lim[1] + target_span)
    }
    if (lv > log_lim[2]) {
      log_lim[2] <- lv
      log_lim[1] <- min(log_lim[1], log_lim[2] - target_span)
    }
  }
  if (diff(log_lim) < target_span) {
    mid <- mean(log_lim)
    log_lim <- c(mid - target_span / 2, mid + target_span / 2)
  }
  10^log_lim
}

# Expand log limits so labeled ticks (default 0.3, 1, 3, 10) sit inside the
# scale with room beyond the extremes (min < 0.3, max > 10).
ensure_log10_limits_cover_labeled_ticks <- function(lim,
                                                     ticks = c(0.3, 1, 3, 10),
                                                     pad_frac = 0.05) {
  ticks <- ticks[is.finite(ticks) & ticks > 0]
  lim <- suppressWarnings(as.numeric(lim))
  if (length(lim) < 2 || !all(is.finite(lim)) || any(lim <= 0)) {
    if (length(ticks) == 0) {
      return(c(0.1, 30))
    }
    lim <- range(ticks)
  }
  lim <- range(lim, finite = TRUE)
  for (t in ticks) {
    lim <- expand_log10_limits_to_include(lim, t, pad_frac = pad_frac)
  }
  if (length(ticks) == 0) {
    return(lim)
  }
  log_lim <- log10(lim)
  log_lo <- log10(min(ticks))
  log_hi <- log10(max(ticks))
  span <- max(diff(log_lim), log_hi - log_lo, 0.1)
  pad <- pad_frac * span
  if (log_lim[1] >= log_lo) {
    log_lim[1] <- log_lo - pad
  }
  if (log_lim[2] <= log_hi) {
    log_lim[2] <- log_hi + pad
  }
  10^log_lim
}

# Shared limits for the paired plots:
#   - crowding x-height vs acuity x-height (X = acuity x-height clipped at 3 deg)
#   - crowding:acuity ratio vs acuity x-height (same clipped X)
# Both get the identical X and identical Y range (union of both datasets), so
# the two panels have the same size and the same position for every Y value.
paired_xheight_and_ratio_plot_limits <- function(df_list) {
  empty_x <- clip_acuity_xheight_xlim_at_3(c(0.1, 3))
  empty_y <- ensure_log10_limits_cover_labeled_ticks(c(0.1, 30))
  empty_span <- max(diff(log10(empty_x)), diff(log10(empty_y)), 0.1)
  empty_y <- expand_log10_limits_to_span(empty_y, empty_span, keep = empty_y)
  empty <- list(
    xheight = list(x = empty_x, y = empty_y),
    ratio = list(x = empty_x, y = empty_y)
  )
  d <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(d) == 0) {
    return(empty)
  }
  d <- d %>%
    dplyr::filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(crowdingXHeightDeg), crowdingXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0,
      is.finite(r), r > 0
    )
  if (nrow(d) == 0) {
    return(empty)
  }

  sq <- shared_log10_limits(d$acuityXHeightDeg, d$crowdingXHeightDeg)
  sq_lim <- ensure_log10_limits_cover_labeled_ticks(sq$x)
  # Hard right edge at exactly 3 deg (no pad past 3).
  x_lim <- clip_acuity_xheight_xlim_at_3(sq_lim)

  y_xh <- sq_lim

  r_vals <- c(d$r, d$r_lo, d$r_hi)
  r_vals <- r_vals[is.finite(r_vals) & r_vals > 0]
  y_r <- if (length(r_vals) > 0) range(r_vals) else c(0.5, 20)
  y_r <- expand_log10_limits_to_include(y_r, 1)
  y_r <- ensure_log10_limits_cover_labeled_ticks(y_r)

  y_shared <- range(c(y_xh, y_r))
  y_shared <- expand_log10_limits_to_span(
    y_shared,
    diff(log10(x_lim)),
    keep = c(y_shared, 0.3, 1, 3, 10)
  )

  list(
    xheight = list(x = x_lim, y = y_shared),
    ratio = list(x = x_lim, y = y_shared)
  )
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

# Shared panel chrome for paired acuity-x-height scatters (same margins/ticks
# so on-screen and downloaded panels stay comparable).
paired_acuity_xheight_scatter_theme <- function() {
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(-4, "pt"),
    plot.margin = ggplot2::margin(t = 6, r = 8, b = 6, l = 8, unit = "pt")
  )
}

# Crowding:acuity size ratio r vs acuity x-height (deg), with horizontal
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

  lims <- paired_xheight_and_ratio_plot_limits(df_list)$ratio

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = r, color = font_label)) +
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
      legend.box = "horizontal"
    ) +
    paired_acuity_xheight_scatter_theme() +
    labs(
      subtitle = "Crowding:acuity size ratio vs acuity x-height",
      x = "Acuity x-height (deg)",
      y = "Crowding:acuity size ratio r"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
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

  paired_lims <- paired_xheight_and_ratio_plot_limits(df_list)
  lims <- paired_lims$ratio

  cols <- CROWDING24_FONT_CATEGORY_COLORS[
    intersect(names(CROWDING24_FONT_CATEGORY_COLORS), levels(summary_data$font_category))
  ]

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = r, color = font_category)) +
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
    scale_color_manual(values = cols, name = "Font category", drop = FALSE) +
    theme_bw() +
    plots_legend_inside_theme(x = 0, y = 0, just = c(0, 0)) +
    paired_acuity_xheight_scatter_theme() +
    labs(
      subtitle = paste0(
        "Crowding:acuity size ratio vs acuity x-height\n",
        "colored by font category"
      ),
      x = "Acuity x-height (deg)",
      y = "Crowding:acuity size ratio r"
    ) +
    guides(color = guide_legend(
      title = "Font category",
      ncol = 2,
      byrow = TRUE,
      title.position = "top"
    ))
  # Survive plot + plt_theme_scatter (which sets legend.position = "top").
  tag_legend_inside_panel(p, x = 0, y = 0, just = c(0, 0))
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

  lims <- paired_xheight_and_ratio_plot_limits(df_list)$xheight

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = crowdingXHeightDeg)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    )
  # ~2× prior label size (4.5 → 9), then +30% (→ 11.7); equalize lowercase
  # x-height across faces; nudge at most 10% of the log-axis span.
  p <- add_crowding24_font_abbrev_text(
    p,
    summary_data,
    size = 11.7,
    equalize_xheight = TRUE,
    repel = TRUE,
    max_move_frac = 0.1
  ) +
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
      subtitle = "Crowding x-height vs acuity x-height",
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

  lims <- paired_xheight_and_ratio_plot_limits(df_list)$xheight

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
      legend.box = "horizontal"
    ) +
    paired_acuity_xheight_scatter_theme() +
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

# Font-set legend (paper Fig. 1 style): category headings, then one row per
# font with a category-colored disk holding the abbreviation and the font name
# typeset in its own face. `columns` lists the groups stacked in each column;
# a group named "Sloan" is drawn without a heading. Sizes are in points and
# scale with fontsize (set by the fit hook at render time).
crowding24_category_font_legend_grob <- function(items,
                                                 columns,
                                                 disk_colors,
                                                 label_colors,
                                                 fontsize = 16,
                                                 line_pitch = 1.3) {
  items <- as.data.frame(items)
  if (!"note" %in% names(items)) items$note <- NA_character_
  size_factors <- crowding24_equalize_xheight_sizes(items$family, base_size = 1)
  size_factors[!is.finite(size_factors) | size_factors <= 0] <- 1
  items$size_factor <- size_factors
  rows <- list()
  for (k in seq_along(columns)) {
    y <- 0
    groups <- columns[[k]][columns[[k]] %in% items$category]
    for (g in seq_along(groups)) {
      grp <- groups[[g]]
      if (g > 1) y <- y + 0.5
      if (grp != "Sloan") {
        rows[[length(rows) + 1L]] <- data.frame(
          col = k, slot = y, kind = "heading", text = grp, abbr = NA_character_,
          family = "sans", size_factor = 1, fill = NA_character_, label_col = NA_character_,
          note = NA_character_
        )
        y <- y + 1
      }
      grp_items <- items[items$category == grp, , drop = FALSE]
      grp_items <- grp_items[order(grp_items$name), , drop = FALSE]
      for (i in seq_len(nrow(grp_items))) {
        rows[[length(rows) + 1L]] <- data.frame(
          col = k, slot = y, kind = "item", text = grp_items$name[[i]],
          abbr = grp_items$abbr[[i]], family = grp_items$family[[i]],
          size_factor = grp_items$size_factor[[i]],
          fill = unname(disk_colors[[grp]]), label_col = unname(label_colors[[grp]]),
          note = grp_items$note[[i]]
        )
        y <- y + 1
      }
    }
  }
  grid::gTree(
    rows = do.call(rbind, rows),
    fontsize = fontsize,
    line_pitch = line_pitch,
    text_mult = 1,
    cl = "ee_category_font_legend"
  )
}

ee_category_text_width_pt <- function(text, family, fontsize) {
  g <- grid::textGrob(text, gp = grid::gpar(fontfamily = family, fontsize = fontsize))
  grid::convertWidth(grid::grobWidth(g), "pt", valueOnly = TRUE)
}

ee_category_font_legend_metrics <- function(x) {
  r <- x$rows
  fs <- x$fontsize
  tm <- x$text_mult
  disk <- 1.1 * fs
  gap <- 0.35 * fs
  col_gap <- 1.2 * fs
  name_w <- vapply(seq_len(nrow(r)), function(i) {
    ee_category_text_width_pt(r$text[[i]], r$family[[i]], fs * tm * r$size_factor[[i]])
  }, numeric(1))
  note_w <- vapply(seq_len(nrow(r)), function(i) {
    if (is.na(r$note[[i]])) 0 else ee_category_text_width_pt(r$note[[i]], "sans", fs * tm)
  }, numeric(1))
  text_w <- name_w + note_w + ifelse(r$kind == "item", disk + gap, 0)
  ncol_used <- max(r$col)
  col_w <- vapply(seq_len(ncol_used), function(k) max(c(0, text_w[r$col == k])), numeric(1))
  col_x <- cumsum(c(0, utils::head(col_w + col_gap, -1)))
  n_slots <- max(vapply(seq_len(ncol_used), function(k) max(r$slot[r$col == k]) + 1, numeric(1)))
  list(
    disk = disk,
    gap = gap,
    col_x = col_x,
    name_w = name_w,
    n_slots = n_slots,
    width = sum(col_w) + (ncol_used - 1L) * col_gap,
    height = n_slots * x$line_pitch * fs
  )
}

makeContent.ee_category_font_legend <- function(x) {
  m <- ee_category_font_legend_metrics(x)
  r <- x$rows
  fs <- x$fontsize
  pitch <- x$line_pitch * fs
  center_y <- m$height - (r$slot + 0.5) * pitch
  left <- m$col_x[r$col]
  # Names share an x-height of ~0.5 em, so a baseline 0.25 em below the row
  # center centers their lowercase letters on the disk.
  baseline <- center_y - 0.25 * fs
  is_item <- r$kind == "item"
  children <- list()
  if (any(is_item)) {
    children[[1]] <- grid::circleGrob(
      x = grid::unit(left[is_item] + m$disk / 2, "pt"),
      y = grid::unit(center_y[is_item], "pt"),
      r = grid::unit(m$disk / 2, "pt"),
      gp = grid::gpar(fill = r$fill[is_item], col = NA)
    )
    children[[2]] <- grid::textGrob(
      r$abbr[is_item],
      x = grid::unit(left[is_item] + m$disk / 2, "pt"),
      y = grid::unit(center_y[is_item], "pt"),
      gp = grid::gpar(
        fontsize = 0.5 * fs * x$text_mult,
        fontface = "bold",
        col = r$label_col[is_item]
      )
    )
  }
  text_x <- left + ifelse(is_item, m$disk + m$gap, 0)
  texts <- lapply(seq_len(nrow(r)), function(i) {
    grid::textGrob(
      r$text[[i]],
      x = grid::unit(text_x[[i]], "pt"),
      y = grid::unit(baseline[[i]], "pt"),
      hjust = 0,
      vjust = 0,
      gp = grid::gpar(
        fontfamily = r$family[[i]],
        fontsize = fs * x$text_mult * r$size_factor[[i]],
        col = "black"
      )
    )
  })
  has_note <- which(!is.na(r$note))
  notes <- lapply(has_note, function(i) {
    grid::textGrob(
      r$note[[i]],
      x = grid::unit(text_x[[i]] + m$name_w[[i]], "pt"),
      y = grid::unit(baseline[[i]], "pt"),
      hjust = 0,
      vjust = 0,
      gp = grid::gpar(fontsize = fs * x$text_mult, col = "black")
    )
  })
  grid::setChildren(x, do.call(grid::gList, c(children, texts, notes)))
}

widthDetails.ee_category_font_legend <- function(x) {
  grid::unit(ee_category_font_legend_metrics(x)$width, "pt")
}

heightDetails.ee_category_font_legend <- function(x) {
  grid::unit(ee_category_font_legend_metrics(x)$height, "pt")
}

registerS3method("makeContent", "ee_category_font_legend", makeContent.ee_category_font_legend,
                 envir = asNamespace("grid"))
registerS3method("widthDetails", "ee_category_font_legend", widthDetails.ee_category_font_legend,
                 envir = asNamespace("grid"))
registerS3method("heightDetails", "ee_category_font_legend", heightDetails.ee_category_font_legend,
                 envir = asNamespace("grid"))

# plots_fit_to_content hook for [scatter | font-set legend]: the legend spans
# from the top of the panel to the bottom of the x-axis title.
crowding24_fit_disk_legend <- function(plot, dpi = 200) {
  info <- attr(plot, "crowding24_disk_layout", exact = TRUE)
  if (!is.list(info)) {
    return(NULL)
  }
  dev_file <- tempfile(fileext = ".png")
  ragg::agg_png(dev_file, width = 40, height = 20, units = "in", res = dpi)
  dev_id <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(dev_id)
    unlink(dev_file)
  }, add = TRUE)
  # showtext needs a page before measuring ee_* text (segfaults otherwise).
  grid::grid.newpage()

  main <- plot$patches$plots[[info$main_index]]
  gt <- ggplot2::ggplotGrob(main)
  panel_row <- unique(gt$layout$t[grepl("^panel", gt$layout$name)])[1]
  xlab_row <- unique(gt$layout$b[gt$layout$name == "xlab-b"])[1]
  below_in <- if (is.finite(panel_row) && is.finite(xlab_row) && xlab_row > panel_row) {
    grid::convertHeight(sum(gt$heights[(panel_row + 1):xlab_row]), "in", valueOnly = TRUE)
  } else {
    0.8
  }
  n_h <- length(gt$heights)
  margin_b <- if (is.finite(xlab_row) && xlab_row < n_h) {
    grid::convertHeight(sum(gt$heights[(xlab_row + 1):n_h]), "pt", valueOnly = TRUE)
  } else {
    0
  }

  leg <- info$legend
  # showtext (96 dpi) draws grob text at 96/dpi of nominal; disks are unaffected.
  leg$text_mult <- dpi / 96
  span_pt <- (info$panel_h_in + below_in) * 72.27
  leg$fontsize <- 1
  leg$fontsize <- span_pt / (ee_category_font_legend_metrics(leg)$n_slots * leg$line_pitch)
  leg_w_in <- grid::convertWidth(grid::widthDetails(leg), "in", valueOnly = TRUE)

  holder <- grid::gTree(
    children = grid::gList(leg),
    vp = grid::viewport(
      x = grid::unit(0.15, "in"), y = grid::unit(margin_b, "pt"),
      just = c(0, 0),
      width = grid::unit(1, "npc") - grid::unit(0.15, "in"),
      height = grid::unit(1, "npc") - grid::unit(margin_b, "pt")
    )
  )
  plot$patches$plots[[info$legend_index]] <- patchwork::wrap_elements(full = holder, clip = FALSE)
  plot$patches$layout$widths <- grid::unit(c(info$panel_w_in, leg_w_in + 0.15, 0), "in")
  plot$patches$layout$heights <- grid::unit(info$panel_h_in, "in")

  pg <- patchwork::patchworkGrob(plot)
  abs_in <- function(u, conv) {
    sum(vapply(seq_along(u), function(i) {
      if (grid::unitType(u[i]) == "null") return(0)
      conv(u[i], "in", valueOnly = TRUE)
    }, numeric(1)))
  }
  list(
    plot = plot,
    width_in = abs_in(pg$widths, grid::convertWidth),
    height_in = abs_in(pg$heights, grid::convertHeight)
  )
}

# Crowding x-height vs acuity x-height: one disk per font, filled by font
# category, with the two-letter font abbreviation inside (paper Fig. 1D style).
# Error bars are ±1 SE of log acuity (x) and log crowding (y).
crowding_xheight_vs_acuity_xheight_category_disk_scatter <- function(df_list,
                                                                     font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_size_ratio_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  category_levels <- c("Text (sans serif)", "Text (serif)", "Display", "Script")
  disk_colors <- c(CROWDING24_FONT_CATEGORY_COLORS[category_levels], Sloan = "black")
  label_colors <- c(
    `Text (sans serif)` = "white",
    `Text (serif)` = "white",
    Display = "white",
    Script = "black",
    Sloan = "white"
  )

  summary_data <- summary_data %>%
    mutate(
      abbr = crowding24_font_abbreviation(excel_font),
      font_category = dplyr::case_when(
        abbr == "S" & (is.na(font_category) | font_category == "") ~ "Sloan",
        TRUE ~ as.character(font_category)
      )
    ) %>%
    filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(crowdingXHeightDeg), crowdingXHeightDeg > 0,
      font_category %in% names(disk_colors)
    ) %>%
    mutate(
      font_category = factor(font_category, levels = names(disk_colors)),
      acuity_lo = 10^(log10(acuityXHeightDeg) - se_log_acuity),
      acuity_hi = 10^(log10(acuityXHeightDeg) + se_log_acuity),
      crowding_lo = 10^(log10(crowdingXHeightDeg) - se_log_crowding),
      crowding_hi = 10^(log10(crowdingXHeightDeg) + se_log_crowding)
    )

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  present <- levels(droplevels(summary_data$font_category))
  lims <- paired_xheight_and_ratio_plot_limits(df_list)$xheight

  p <- ggplot(summary_data, aes(x = acuityXHeightDeg, y = crowdingXHeightDeg)) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_point(
      aes(fill = font_category),
      shape = 21,
      size = 11,
      stroke = 0.6,
      color = "white",
      alpha = 0.6
    ) +
    # Most SEs are smaller than the disk radius, so bars go on top of the disks.
    geom_errorbar(
      aes(ymin = crowding_lo, ymax = crowding_hi),
      width = 0.015,
      linewidth = 0.5,
      color = "black",
      na.rm = TRUE
    ) +
    geom_errorbarh(
      aes(xmin = acuity_lo, xmax = acuity_hi),
      height = 0.015,
      linewidth = 0.5,
      color = "black",
      na.rm = TRUE
    ) +
    geom_text(
      aes(label = abbr, color = paste0(font_category, "_label")),
      size = 4.6,
      fontface = "bold",
      show.legend = FALSE
    ) +
    apply_equal_log10_scatter_scales(lims) +
    scale_fill_manual(
      values = disk_colors[present],
      breaks = present,
      name = "Font category"
    ) +
    scale_color_manual(
      values = stats::setNames(label_colors[present], paste0(present, "_label")),
      guide = "none"
    ) +
    theme_bw() +
    paired_acuity_xheight_scatter_theme() +
    theme(legend.position = "none") +
    labs(
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    )
  attr(p, "crowding24_main_panel") <- TRUE

  abbrev_idx <- vapply(
    as.character(summary_data$excel_font),
    function(f) match_crowding24_font_abbrev_index(f),
    integer(1)
  )
  summary_data$legend_name <- dplyr::coalesce(
    CROWDING24_FONT_ABBREVS$legend_name[abbrev_idx],
    as.character(font_comparison_axis_label(summary_data$font))
  )
  legend_items <- summary_data %>%
    dplyr::distinct(excel_font, .keep_all = TRUE) %>%
    dplyr::transmute(
      category = as.character(font_category),
      abbr,
      name = legend_name,
      family = resolve_crowding24_plot_font_families(excel_font),
      # Extenda's glyphs are too condensed to read; repeat the name in sans.
      note = dplyr::if_else(legend_name == "Extenda", " (Extenda)", NA_character_)
    )
  legend_grob <- crowding24_category_font_legend_grob(
    legend_items,
    columns = list(c("Display", "Sloan", "Text (sans serif)"), c("Script", "Text (serif)")),
    disk_colors = disk_colors,
    label_colors = label_colors
  )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(p)
  }

  panel_h_in <- 5
  panel_w_in <- panel_h_in * diff(log10(lims$x)) / diff(log10(lims$y))
  # Zero-width spacer is the patchwork base, so the scatter and legend both
  # sit in patches$plots where the PNG theme and fit hook can reach them.
  combined <- patchwork::wrap_plots(
    p,
    patchwork::wrap_elements(full = legend_grob, clip = FALSE),
    patchwork::plot_spacer(),
    ncol = 3,
    widths = grid::unit(c(panel_w_in, 5, 0), "in"),
    heights = grid::unit(panel_h_in, "in")
  ) +
    patchwork::plot_annotation(
      subtitle = "Crowding x-height vs acuity x-height",
      theme = ggplot2::theme(plot.subtitle = ggplot2::element_text(size = 18, hjust = 0))
    )
  combined <- tag_crowding24_ee_families(combined, legend_items$family)
  attr(combined, "crowding24_native_legend_patchwork") <- TRUE
  attr(combined, "crowding24_paired_row_patchwork") <- TRUE
  attr(combined, "crowding24_disk_layout") <- list(
    legend = legend_grob,
    legend_index = 2L,
    main_index = 1L,
    panel_w_in = panel_w_in,
    panel_h_in = panel_h_in
  )
  attr(combined, "plots_fit_to_content") <- crowding24_fit_disk_legend
  attr(combined, "plots_full_row") <- TRUE
  attr(combined, "plots_display_width_in") <- panel_w_in + 6
  attr(combined, "plots_display_height_in") <- panel_h_in + 1.5
  combined
}

# Shared prep for crowding×acuity x-height scatters colored by individual font.
# resolve_fonts=TRUE only when a native-font legend / abbrev face is needed —
# remembering fonts/ paths is cheap; avoid doing it for plain color legends.
prepare_crowding_xheight_vs_acuity_xheight_by_font_data <- function(df_list,
                                                                     font_colors = NULL,
                                                                     resolve_fonts = FALSE,
                                                                     paired_lims = NULL) {
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

  styled <- font_scatter_legend_cols(summary_data, font_colors)
  summary_data <- styled$data
  cols <- styled$cols
  if (isTRUE(resolve_fonts)) {
    summary_data <- summary_data %>%
      mutate(plot_family = resolve_crowding24_plot_font_families(excel_font))
  }

  if (is.null(paired_lims)) {
    paired_lims <- paired_xheight_and_ratio_plot_limits(df_list)
  }
  lims <- paired_lims$xheight

  list(data = summary_data, cols = cols, lims = lims, paired_lims = paired_lims)
}

# Crowding x-height vs acuity x-height, one colored point per font (standard legend).
crowding_xheight_vs_acuity_xheight_by_font_scatter <- function(df_list,
                                                               font_colors = NULL,
                                                               prep = NULL) {
  if (is.null(prep)) {
    prep <- prepare_crowding_xheight_vs_acuity_xheight_by_font_data(
      df_list,
      font_colors,
      resolve_fonts = FALSE
    )
  }
  if (is.null(prep)) {
    return(NULL)
  }

  ggplot(
    prep$data,
    aes(x = acuityXHeightDeg, y = crowdingXHeightDeg, color = font_label)
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_point(size = 3.5) +
    apply_equal_log10_scatter_scales(prep$lims) +
    scale_color_manual(values = prep$cols, name = "Font") +
    theme_bw() +
    theme(
      legend.position = "bottom",
      legend.box = "horizontal"
    ) +
    paired_acuity_xheight_scatter_theme() +
    labs(
      subtitle = "Crowding x-height vs acuity x-height",
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    ) +
    guides(color = guide_legend(title = "Font", ncol = 4, byrow = TRUE))
}

# Custom legend: each font name typeset in that font (ee_* / fonts/).
# pack_top=TRUE keeps rows tight near the top when the panel is stretched tall
# (side column next to scatters); otherwise rows fill the panel evenly.
crowding24_native_font_legend_plot <- function(labels,
                                               families,
                                               colors,
                                               ncol = 4,
                                               # 3.2 × 1.2 (+20% legend text).
                                               text_size = 3.84,
                                               point_size = 3.2 * 1.4,
                                               pack_top = FALSE,
                                               row_pitch = NULL) {
  labels <- as.character(labels)
  families <- as.character(families)
  n <- length(labels)
  if (n == 0) {
    return(ggplot() + theme_void())
  }
  if (length(families) == 1L) families <- rep(families, n)
  if (length(families) != n) {
    stop("labels and families must have the same length")
  }
  col_vals <- if (!is.null(names(colors))) {
    unname(colors[as.character(labels)])
  } else {
    as.character(colors)[seq_len(n)]
  }
  col_vals[is.na(col_vals) | !nzchar(col_vals)] <- "gray40"

  nrow_leg <- ceiling(n / ncol)
  idx <- seq_len(n)
  if (is.null(row_pitch)) {
    row_pitch <- if (isTRUE(pack_top)) 0.78 else 1
  }
  # row_index 1 = top
  row_index <- ((idx - 1L) %/% ncol) + 1L
  y_top <- nrow_leg * row_pitch
  df <- data.frame(
    label = labels,
    family = families,
    color = col_vals,
    col = ((idx - 1L) %% ncol) + 1L,
    y = y_top - (row_index - 1L) * row_pitch,
    stringsAsFactors = FALSE
  )

  # Slight x-height equalization so mixed faces read at similar size.
  df$text_size <- crowding24_equalize_xheight_sizes(
    df$family,
    base_size = text_size
  )

  y_content_bottom <- min(df$y) - 0.28
  y_content_top <- max(df$y) + 0.28
  if (isTRUE(pack_top)) {
    # Leave empty space below so a tall side panel does not inflate row gaps.
    content_span <- y_content_top - y_content_bottom
    y_bottom <- y_content_top - content_span / 0.42
  } else {
    y_bottom <- y_content_bottom
  }

  p <- ggplot(df, aes(x = col, y = y)) +
    geom_point(aes(color = label), size = point_size) +
    scale_color_manual(values = stats::setNames(df$color, df$label), guide = "none") +
    scale_x_continuous(limits = c(0.55, ncol + 0.95), expand = c(0, 0), breaks = NULL) +
    scale_y_continuous(limits = c(y_bottom, y_content_top), expand = c(0, 0), breaks = NULL) +
    coord_cartesian(clip = "off") +
    theme_void() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.line = element_blank(),
      panel.grid = element_blank(),
      plot.margin = margin(t = 4, r = 4, b = 4, l = 4)
    )

  fams <- unique(df$family)
  for (fam in fams) {
    layer_data <- df[df$family == fam, , drop = FALSE]
    p <- p +
      ggplot2::geom_text(
        data = layer_data,
        # Small offset so the name starts just after the dot.
        ggplot2::aes(x = col + 0.09, y = y, label = label, size = text_size),
        family = fam,
        hjust = 0,
        vjust = 0.5,
        color = "black",
        show.legend = FALSE,
        inherit.aes = FALSE
      )
  }
  p + ggplot2::scale_size_identity()
}

# Copy of by-font x-height scatter: legend names drawn in each font's own face.
crowding_xheight_vs_acuity_xheight_by_font_native_legend_scatter <- function(
    df_list,
    font_colors = NULL,
    prep = NULL) {
  if (is.null(prep)) {
    prep <- prepare_crowding_xheight_vs_acuity_xheight_by_font_data(
      df_list,
      font_colors,
      resolve_fonts = TRUE
    )
  }
  if (is.null(prep)) {
    return(NULL)
  }
  if (!"plot_family" %in% names(prep$data)) {
    prep$data <- prep$data %>%
      dplyr::mutate(plot_family = resolve_crowding24_plot_font_families(excel_font))
  }

  summary_data <- prep$data
  cols <- prep$cols
  lims <- prep$lims

  p_main <- ggplot(
    summary_data,
    aes(x = acuityXHeightDeg, y = crowdingXHeightDeg, color = font_label)
  ) +
    geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    geom_point(size = 3.5) +
    apply_equal_log10_scatter_scales(lims) +
    scale_color_manual(values = cols, guide = "none") +
    theme_bw() +
    theme(
      legend.position = "none",
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.ticks.length = unit(-4, "pt"),
      # PNG theme sizes the base (main) panel; keep a readable fallback here.
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 14),
      plot.margin = margin(t = 0.05, r = 0.1, b = 0.1, l = 0.1, "inch")
    ) +
    labs(
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    )
  attr(p_main, "crowding24_main_panel") <- TRUE

  # Title above legend (plot "subtitle" is the visible figure title).
  # Experiment name is added later via add_experiment_title() as plot.title.
  p_title <- ggplot() +
    theme_void() +
    theme(
      plot.title = element_text(size = 9, hjust = 0, margin = margin(b = 0)),
      plot.title.position = "plot",
      plot.subtitle = element_text(size = 18, hjust = 0, margin = margin(t = 2, b = 0)),
      plot.margin = margin(t = 0.05, r = 0.1, b = 0, l = 0.1, "inch")
    ) +
    labs(subtitle = "Crowding x-height vs acuity x-height")
  attr(p_title, "crowding24_title_panel") <- TRUE

  # One legend row per unique font_label (stable color / family).
  legend_rows <- summary_data %>%
    dplyr::distinct(font_label, plot_family, .keep_all = FALSE) %>%
    dplyr::arrange(font_label)
  legend_labels <- as.character(legend_rows$font_label)
  legend_families <- as.character(legend_rows$plot_family)

  p_leg <- crowding24_native_font_legend_plot(
    labels = legend_labels,
    families = legend_families,
    colors = cols,
    ncol = 4
  )
  attr(p_leg, "crowding24_legend_panel") <- TRUE

  n_fonts <- length(legend_labels)
  n_leg_rows <- max(1L, ceiling(n_fonts / 4))
  # Compact row pitch (panel fraction per legend row) — rows nearly touching.
  leg_height <- 0.015 + 0.03 * n_leg_rows
  # patchwork heights size *panel* areas; the title text sits above its panel,
  # so any real height here is blank space between title and legend.
  title_height <- 0.005
  # Keep the scatter panel ~same absolute size as other 7×7 scatters; add
  # height for the taller native-font legend so side-by-side panels match.
  panel_height_in <- 7
  display_height_in <- panel_height_in * (title_height + leg_height + 1)

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    # Fallback: return main plot only if patchwork is unavailable.
    return(tag_crowding24_ee_families(p_main, legend_families))
  }

  # Experiment name + title → native-font legend → scatter.
  combined <- patchwork::wrap_plots(
    p_title,
    p_leg,
    p_main,
    ncol = 1,
    heights = c(title_height, leg_height, 1)
  )
  combined <- tag_crowding24_ee_families(combined, legend_families)
  attr(combined, "crowding24_native_legend_patchwork") <- TRUE
  attr(combined, "plots_display_height_in") <- display_height_in
  combined
}

# Alias kept for older call sites / plot lists.
crowding_xheight_vs_acuity_xheight_font_abbrev_scatter <- function(df_list,
                                                                   font_colors = NULL) {
  crowding_xheight_vs_acuity_xheight_scatter(df_list, font_colors = font_colors)
}

# Native-font legend as a grid grob laid out in points from the bottom-left
# corner: columns fill top-to-bottom, each name typeset in its own face.
# Column widths are measured at draw time, when the ee_* faces are registered.
crowding24_native_font_legend_grob <- function(labels,
                                               families,
                                               colors,
                                               ncol = 2,
                                               fontsize = 24,
                                               line_pitch = 0.76) {
  labels <- as.character(labels)
  families <- as.character(families)
  n <- length(labels)
  if (length(families) == 1L) families <- rep(families, n)
  col_vals <- if (!is.null(names(colors))) {
    unname(colors[labels])
  } else {
    as.character(colors)[seq_len(n)]
  }
  col_vals[is.na(col_vals) | !nzchar(col_vals)] <- "gray40"
  size_factors <- crowding24_equalize_xheight_sizes(families, base_size = 1)
  size_factors[!is.finite(size_factors) | size_factors <= 0] <- 1
  grid::gTree(
    labels = labels,
    families = families,
    colors = col_vals,
    size_factors = size_factors,
    ncol = max(1L, as.integer(ncol)),
    fontsize = fontsize,
    line_pitch = line_pitch,
    cl = "ee_font_legend"
  )
}

ee_font_legend_metrics <- function(x) {
  n <- length(x$labels)
  nrow_leg <- max(1L, ceiling(n / x$ncol))
  idx <- seq_len(n) - 1L
  col_i <- idx %/% nrow_leg + 1L
  row_i <- idx %% nrow_leg + 1L
  sizes <- x$fontsize * x$size_factors
  text_w <- vapply(seq_len(n), function(i) {
    g <- grid::textGrob(
      x$labels[[i]],
      gp = grid::gpar(fontfamily = x$families[[i]], fontsize = sizes[[i]])
    )
    grid::convertWidth(grid::grobWidth(g), "pt", valueOnly = TRUE)
  }, numeric(1))
  dot <- 0.45 * x$fontsize
  gap <- 0.3 * x$fontsize
  col_gap <- 0.8 * x$fontsize
  ncol_used <- max(col_i, 1L)
  col_w <- vapply(seq_len(ncol_used), function(k) {
    max(c(0, text_w[col_i == k]))
  }, numeric(1))
  block_w <- dot + gap + col_w
  col_x <- cumsum(c(0, utils::head(block_w + col_gap, -1)))
  list(
    nrow = nrow_leg,
    col_i = col_i,
    row_i = row_i,
    sizes = sizes,
    dot = dot,
    gap = gap,
    col_x = col_x,
    width = sum(block_w) + (ncol_used - 1L) * col_gap,
    height = nrow_leg * x$line_pitch * x$fontsize
  )
}

makeContent.ee_font_legend <- function(x) {
  m <- ee_font_legend_metrics(x)
  pitch <- x$line_pitch * x$fontsize
  # Baseline sits so the bottom row's descenders reach y = 0.
  baseline <- (m$nrow - m$row_i) * pitch + 0.24 * x$fontsize
  dot_y <- baseline + 0.25 * x$fontsize
  dots <- grid::circleGrob(
    x = grid::unit(m$col_x[m$col_i] + m$dot / 2, "pt"),
    y = grid::unit(dot_y, "pt"),
    r = grid::unit(m$dot / 2, "pt"),
    gp = grid::gpar(fill = x$colors, col = NA)
  )
  texts <- lapply(seq_along(x$labels), function(i) {
    grid::textGrob(
      x$labels[[i]],
      x = grid::unit(m$col_x[m$col_i[[i]]] + m$dot + m$gap, "pt"),
      y = grid::unit(baseline[[i]], "pt"),
      hjust = 0,
      vjust = 0,
      gp = grid::gpar(
        fontfamily = x$families[[i]],
        fontsize = m$sizes[[i]],
        col = "black"
      )
    )
  })
  grid::setChildren(x, do.call(grid::gList, c(list(dots), texts)))
}

widthDetails.ee_font_legend <- function(x) {
  grid::unit(ee_font_legend_metrics(x)$width, "pt")
}

heightDetails.ee_font_legend <- function(x) {
  grid::unit(ee_font_legend_metrics(x)$height, "pt")
}

registerS3method("makeContent", "ee_font_legend", makeContent.ee_font_legend,
                 envir = asNamespace("grid"))
registerS3method("widthDetails", "ee_font_legend", widthDetails.ee_font_legend,
                 envir = asNamespace("grid"))
registerS3method("heightDetails", "ee_font_legend", heightDetails.ee_font_legend,
                 envir = asNamespace("grid"))

# plots_fit_to_content hook for the one-row figure. Runs after the PNG theme,
# with fonts registered: sizes the font legend to span from the top of the
# scatter panels to the bottom of the x-axis title, then measures the whole
# layout so the saved figure has no slack (slack becomes white space).
crowding24_fit_font_row <- function(plot, dpi = 200) {
  info <- attr(plot, "crowding24_row_layout", exact = TRUE)
  if (!is.list(info)) {
    return(NULL)
  }
  dev_file <- tempfile(fileext = ".png")
  ragg::agg_png(dev_file, width = 40, height = 20, units = "in", res = dpi)
  dev_id <- grDevices::dev.cur()
  on.exit({
    grDevices::dev.off(dev_id)
    unlink(dev_file)
  }, add = TRUE)
  # showtext attaches to a device on its first page; measuring text before
  # that crashes (segfault) with showtext-only ee_* families.
  grid::grid.newpage()

  ph_in <- info$panel_h_in
  pw_in <- info$panel_w_in
  # Height from the panel's bottom edge to the bottom of the x-axis title.
  main <- plot$patches$plots[[info$main_index]]
  gt <- ggplot2::ggplotGrob(main)
  panel_row <- unique(gt$layout$t[grepl("^panel", gt$layout$name)])[1]
  xlab_row <- unique(gt$layout$b[gt$layout$name == "xlab-b"])[1]
  below_in <- if (is.finite(panel_row) && is.finite(xlab_row) && xlab_row > panel_row) {
    grid::convertHeight(sum(gt$heights[(panel_row + 1):xlab_row]), "in", valueOnly = TRUE)
  } else {
    0.8
  }

  leg <- info$legend
  span_pt <- (ph_in + below_in) * 72.27
  nrow_leg <- max(1L, ceiling(length(leg$labels) / leg$ncol))
  # Font size: rows at 1.08 em would fill the span. Rows are then packed so
  # the bottom-aligned legend occupies only the lower half of the column.
  leg$fontsize <- span_pt / (nrow_leg * 1.08)
  leg$line_pitch <- 0.5 * span_pt / (nrow_leg * leg$fontsize)
  leg_w_in <- grid::convertWidth(grid::widthDetails(leg), "in", valueOnly = TRUE)

  # The wrapped cell spans the full plot area; lift the legend by whatever
  # sits below the x-axis title (caption row, bottom plot margin).
  n_h <- length(gt$heights)
  margin_b <- if (is.finite(xlab_row) && xlab_row < n_h) {
    grid::convertHeight(sum(gt$heights[(xlab_row + 1):n_h]), "pt", valueOnly = TRUE)
  } else {
    0
  }
  holder <- grid::gTree(
    children = grid::gList(leg),
    vp = grid::viewport(
      x = 0, y = grid::unit(margin_b, "pt"),
      just = c(0, 0),
      width = grid::unit(1, "npc"),
      height = grid::unit(1, "npc") - grid::unit(margin_b, "pt")
    )
  )
  plot$patches$plots[[info$legend_index]] <- patchwork::wrap_elements(full = holder, clip = FALSE)
  plot$patches$layout$widths <- grid::unit(c(leg_w_in, pw_in, pw_in), "in")
  plot$patches$layout$heights <- grid::unit(ph_in, "in")

  pg <- patchwork::patchworkGrob(plot)
  abs_in <- function(u, conv) {
    sum(vapply(seq_along(u), function(i) {
      if (grid::unitType(u[i]) == "null") return(0)
      conv(u[i], "in", valueOnly = TRUE)
    }, numeric(1)))
  }
  list(
    plot = plot,
    width_in = abs_in(pg$widths, grid::convertWidth),
    height_in = abs_in(pg$heights, grid::convertHeight)
  )
}

# One-row figure: [native font legend | x-height by font | size-ratio by category].
# Columns 2–3 share point size, axis text size, and paired axis limits (no titles).
# Pass shared prep (resolve_fonts=TRUE) from the Plots list builder so fonts/
# paths are resolved once for all native-font scatters.
crowding_xheight_and_ratio_font_row_scatter <- function(df_list,
                                                       font_colors = NULL,
                                                       prep = NULL) {
  if (is.null(prep)) {
    prep <- prepare_crowding_xheight_vs_acuity_xheight_by_font_data(
      df_list,
      font_colors,
      resolve_fonts = TRUE
    )
  }
  if (is.null(prep)) {
    return(NULL)
  }
  if (!"plot_family" %in% names(prep$data)) {
    prep$data <- prep$data %>%
      dplyr::mutate(plot_family = resolve_crowding24_plot_font_families(excel_font))
  }

  paired_lims <- prep$paired_lims
  if (is.null(paired_lims)) {
    paired_lims <- paired_xheight_and_ratio_plot_limits(df_list)
  }
  # Shared sizes for both scatter columns (must stay identical through PNG theme).
  point_size <- 3.5
  axis_text_size <- 14
  axis_title_size <- 14

  panel_theme <- ggplot2::theme(
    legend.position = "none",
    panel.grid.major = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.background = ggplot2::element_blank(),
    axis.ticks.length = ggplot2::unit(-4, "pt"),
    axis.text = ggplot2::element_text(size = axis_text_size),
    axis.title = ggplot2::element_text(size = axis_title_size),
    plot.title = ggplot2::element_blank(),
    plot.subtitle = ggplot2::element_blank(),
    # Journals want little white space between figure elements.
    plot.margin = ggplot2::margin(t = 2, r = 4, b = 2, l = 2, unit = "pt")
  )

  p_xheight <- ggplot2::ggplot(
    prep$data,
    ggplot2::aes(x = acuityXHeightDeg, y = crowdingXHeightDeg, color = font_label)
  ) +
    ggplot2::geom_abline(
      intercept = 0,
      slope = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    ggplot2::geom_point(size = point_size) +
    apply_equal_log10_scatter_scales(paired_lims$xheight) +
    ggplot2::scale_color_manual(values = prep$cols, guide = "none") +
    ggplot2::theme_bw() +
    panel_theme +
    ggplot2::labs(
      x = "Acuity x-height (deg)",
      y = "Crowding x-height (deg)"
    )
  attr(p_xheight, "crowding24_main_panel") <- TRUE

  category_levels <- c(
    "Text (sans serif)",
    "Text (serif)",
    "Display",
    "Script"
  )
  ratio_data <- prepare_crowding_acuity_size_ratio_data(df_list) %>%
    dplyr::filter(
      is.finite(acuityXHeightDeg), acuityXHeightDeg > 0,
      is.finite(archive_xHeightReNominal), archive_xHeightReNominal > 0,
      !is.na(font_category), font_category != "",
      font_category %in% category_levels
    ) %>%
    dplyr::mutate(
      font_category = factor(font_category, levels = category_levels)
    )
  if (nrow(ratio_data) == 0) {
    return(NULL)
  }
  cat_cols <- CROWDING24_FONT_CATEGORY_COLORS[
    intersect(names(CROWDING24_FONT_CATEGORY_COLORS), levels(ratio_data$font_category))
  ]

  p_ratio <- ggplot2::ggplot(
    ratio_data,
    ggplot2::aes(x = acuityXHeightDeg, y = r, color = font_category)
  ) +
    ggplot2::geom_hline(
      yintercept = 1,
      linetype = "longdash",
      linewidth = 0.6,
      color = "gray40"
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = r_lo, ymax = r_hi),
      width = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = acuityXHeight_lo, xmax = acuityXHeight_hi),
      height = 0,
      linewidth = 0.6,
      na.rm = TRUE
    ) +
    ggplot2::geom_point(size = point_size) +
    apply_equal_log10_scatter_scales(paired_lims$ratio) +
    ggplot2::scale_color_manual(
      values = cat_cols,
      name = "Font category",
      drop = FALSE
    ) +
    ggplot2::theme_bw() +
    panel_theme +
    plots_legend_inside_theme(x = 0, y = 0, just = c(0, 0)) +
    ggplot2::labs(
      x = "Acuity x-height (deg)",
      y = "Crowding:acuity size ratio r"
    ) +
    ggplot2::guides(
      color = ggplot2::guide_legend(
        title = "Font category",
        ncol = 1,
        title.position = "top"
      )
    )
  attr(p_ratio, "crowding24_main_panel") <- TRUE
  p_ratio <- tag_legend_inside_panel(p_ratio, x = 0, y = 0, just = c(0, 0))

  legend_rows <- prep$data %>%
    dplyr::distinct(font_label, plot_family, .keep_all = FALSE) %>%
    dplyr::arrange(font_label)
  legend_grob <- crowding24_native_font_legend_grob(
    labels = as.character(legend_rows$font_label),
    families = as.character(legend_rows$plot_family),
    colors = prep$cols,
    ncol = 2
  )

  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(tag_crowding24_ee_families(p_xheight, legend_rows$plot_family))
  }

  # Absolute panel sizes: both scatters share limits, so equal panels give the
  # same inches per decade on both axes. Final width/height and the legend
  # font size are set by crowding24_fit_font_row() at render time.
  panel_h_in <- 5
  span_x <- diff(log10(paired_lims$xheight$x))
  span_y <- diff(log10(paired_lims$xheight$y))
  panel_w_in <- panel_h_in * span_x / span_y
  combined <- patchwork::wrap_plots(
    patchwork::wrap_elements(full = legend_grob, clip = FALSE),
    p_xheight,
    p_ratio,
    ncol = 3,
    widths = grid::unit(c(4, panel_w_in, panel_w_in), "in"),
    heights = grid::unit(panel_h_in, "in")
  )
  combined <- tag_crowding24_ee_families(combined, legend_rows$plot_family)
  # Reuse native-legend PNG / theme skip path; paired-row for experiment title.
  attr(combined, "crowding24_native_legend_patchwork") <- TRUE
  attr(combined, "crowding24_paired_row_patchwork") <- TRUE
  attr(combined, "crowding24_row_layout") <- list(
    legend = legend_grob,
    legend_index = 1L,
    main_index = 2L,
    panel_w_in = panel_w_in,
    panel_h_in = panel_h_in
  )
  attr(combined, "plots_fit_to_content") <- crowding24_fit_font_row
  attr(combined, "plots_full_row") <- TRUE
  # Initial canvas only; the fit hook replaces it with the measured size.
  attr(combined, "plots_display_width_in") <- 4 + 2 * panel_w_in + 3
  attr(combined, "plots_display_height_in") <- panel_h_in + 1.5
  combined
}

# Shared prep for Crowding:acuity ratio r histograms (one value per font).
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

RATIO_R_HIST_CATEGORY_LEVELS <- c(
  "Text (sans serif)",
  "Text (serif)",
  "Display",
  "Script",
  "Sloan"
)

ratio_r_hist_category_colors <- function() {
  c(CROWDING24_FONT_CATEGORY_COLORS, Sloan = "black")
}

# Log-x limits for ratio-r histograms; always long enough to include 1, 3, 10.
ratio_r_hist_log10_limits <- function(r, pad_frac = 0.05) {
  r <- r[is.finite(r) & r > 0]
  if (length(r) == 0) {
    return(c(0.5, 20))
  }
  lim <- range(r, finite = TRUE)
  for (v in c(1, 3, 10)) {
    lim <- expand_log10_limits_to_include(lim, v, pad_frac = pad_frac)
  }
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

# Labeled x ticks: always 1, 3, 10 (professor request).
ratio_r_hist_x_breaks <- function(lims = NULL) {
  c(1, 3, 10)
}

# Shared look for the paired size-ratio histograms (panel-matched pair).
ratio_r_hist_pair_theme <- function() {
  ggplot2::theme(
    legend.position = "top",
    legend.box = "horizontal",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(0, 0, 2, 0),
    legend.key.size = unit(0.9, "lines"),
    # Override hist_theme tilt — keep 1, 3, 10 horizontal.
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks.length = unit(-4, "pt")
  )
}

tag_ratio_r_hist_pair <- function(plot, legend_spacer = c("none", "top", "bottom")) {
  legend_spacer <- match.arg(legend_spacer)
  attr(plot, "ratio_r_hist_pair") <- TRUE
  attr(plot, "ratio_r_hist_legend_spacer") <- legend_spacer
  plot
}

# Invisible top legend with the same categories/rows as the dotted hist, so the
# bar hist panel height matches (professor: same panel size; use shorter height).
# White text looks invisible; sits between plot title and panel like the dotted hist.
ratio_r_hist_legend_spacer_layer <- function(lims, cols) {
  levels_use <- names(cols)
  spacer <- tibble::tibble(
    plot_category = factor(levels_use, levels = levels_use),
    r = lims[[1]],
    y = 0
  )
  list(
    ggplot2::geom_point(
      data = spacer,
      ggplot2::aes(x = r, y = y, color = plot_category),
      inherit.aes = FALSE,
      alpha = 0,
      size = 0,
      show.legend = TRUE
    ),
    ggplot2::scale_color_manual(values = cols, name = NULL, drop = FALSE),
    ggplot2::guides(color = ggplot2::guide_legend(
      title = NULL,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(alpha = 0, size = 0, stroke = 0, color = "white")
    )),
    ggplot2::theme(
      legend.position = "top",
      legend.text = ggplot2::element_text(color = "white"),
      legend.key = ggplot2::element_blank(),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank()
    )
  )
}

# Bar histogram of Crowding:acuity size ratio r (one bar-bin count of fonts), log x.
# ggplot is built in a child frame so plot_env does not retain df_list (PNG
# theming serialize(plot) would otherwise copy the whole archive bundle).
crowding_acuity_size_ratio_r_histogram <- function(df_list, font_colors = NULL) {
  summary_data <- prepare_crowding_acuity_ratio_r_hist_data(df_list)
  if (nrow(summary_data) == 0) {
    return(NULL)
  }
  build_crowding_acuity_size_ratio_r_histogram(summary_data)
}

build_crowding_acuity_size_ratio_r_histogram <- function(summary_data) {
  lims <- ratio_r_hist_log10_limits(summary_data$r)
  breaks <- ratio_r_hist_log_breaks(lims)
  logtick_guide <- ggplot2::guide_axis_logticks(
    long = 2.5,
    mid = 0.75,
    short = 0.75
  )
  cols <- ratio_r_hist_category_colors()
  cols <- cols[intersect(names(cols), RATIO_R_HIST_CATEGORY_LEVELS)]

  p <- ggplot(summary_data, aes(x = r)) +
    geom_histogram(
      breaks = breaks,
      fill = "gray80",
      color = NA,
      closed = "left"
    )
  for (layer in ratio_r_hist_legend_spacer_layer(lims, cols)) {
    p <- p + layer
  }
  p <- p +
    scale_x_log10(
      limits = lims,
      breaks = ratio_r_hist_x_breaks(lims),
      labels = c("1", "3", "10"),
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
    ratio_r_hist_pair_theme() +
    # Keep white spacer legend between title and panel (matches dotted hist).
    theme(
      legend.position = "top",
      legend.text = element_text(color = "white"),
      legend.key = element_blank(),
      legend.background = element_blank(),
      legend.box.background = element_blank()
    ) +
    labs(
      subtitle = "Histogram of Crowding:acuity\nsize ratio r",
      x = "Crowding:acuity size ratio r",
      y = "Number of fonts"
    )
  tag_ratio_r_hist_pair(p, legend_spacer = "top")
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
      r_bin = 10^((log_breaks[bin] + log_breaks[bin + 1L]) / 2)
    ) %>%
    ungroup()

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  cols <- ratio_r_hist_category_colors()
  summary_data <- summary_data %>%
    mutate(
      plot_category = factor(
        dplyr::if_else(
          is_sloan,
          "Sloan",
          as.character(plot_category)
        ),
        levels = RATIO_R_HIST_CATEGORY_LEVELS
      )
    ) %>%
    filter(!is.na(plot_category))

  if (nrow(summary_data) == 0) {
    return(NULL)
  }

  cols <- cols[intersect(names(cols), levels(summary_data$plot_category))]
  build_crowding_acuity_size_ratio_r_dot_histogram(summary_data, lims, cols)
}

build_crowding_acuity_size_ratio_r_dot_histogram <- function(summary_data, lims, cols) {
  max_y <- max(summary_data$stack_y, na.rm = TRUE)
  logtick_guide <- ggplot2::guide_axis_logticks(
    long = 2.5,
    mid = 0.75,
    short = 0.75
  )

  # Large dots for stacked “bowling ball” look (distance-page style).
  dot_size <- 6.5

  p <- ggplot(summary_data, aes(x = r_bin, y = stack_y, color = plot_category)) +
    geom_point(size = dot_size, alpha = 0.95) +
    scale_x_log10(
      limits = lims,
      breaks = ratio_r_hist_x_breaks(lims),
      labels = c("1", "3", "10"),
      expand = c(0, 0),
      guide = logtick_guide
    ) +
    scale_y_continuous(
      breaks = seq_len(max(1L, max_y)),
      expand = expansion(mult = c(0, 0.08)),
      guide = guide_axis(check.overlap = FALSE)
    ) +
    scale_color_manual(values = cols, name = NULL, drop = FALSE) +
    coord_cartesian(ylim = c(0.5, max_y + 0.5)) +
    theme_bw() +
    ratio_r_hist_pair_theme() +
    labs(
      subtitle = "Histogram of Crowding:acuity\nsize ratio by font group",
      x = "Crowding:acuity size ratio r",
      y = "Number of fonts"
    ) +
    guides(color = guide_legend(
      title = NULL,
      nrow = 2,
      byrow = TRUE,
      override.aes = list(size = 3)
    ))
  tag_ratio_r_hist_pair(p)
}
