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
  font = c("B-NAZANIN.TTF", "IranNastaliq.ttf", "Kalameh-Regular.ttf", "Mj-Hoor_0.ttf",
           "Titr.bold.woff2","Moalla.ttf")
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
  beauty_data <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(beauty_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-Nazanin" ~"B-NAZANIN.TTF",
                            conditionName=="beauty-Titr" ~ "Titr.bold.woff2",
                            conditionName=="beauty-Kalameh" ~ "Kalameh-Regular.ttf",
                            conditionName=="beauty-IranNastaliq" ~ "IranNastaliq.ttf",
                            conditionName=="beauty-Moalla" ~ "Moalla.ttf",
                            conditionName=="beauty-MJ-Hoor" ~ "Mj-Hoor_0.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
                            TRUE ~ conditionName)) %>%
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
  beauty_data <- df_list$QA %>%
    filter(grepl('bty', tolower(questionAndAnswerNickname))) %>%
    mutate(beauty_rating = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-Nazanin" ~"B-NAZANIN.TTF",
                            conditionName=="beauty-Titr" ~ "Titr.bold.woff2",
                            conditionName=="beauty-Kalameh" ~ "Kalameh-Regular.ttf",
                            conditionName=="beauty-IranNastaliq" ~ "IranNastaliq.ttf",
                            conditionName=="beauty-Moalla" ~ "Moalla.ttf",
                            conditionName=="beauty-MJ-Hoor" ~ "Mj-Hoor_0.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
                            TRUE ~ conditionName)) %>%
    filter(!is.na(beauty_rating)) %>%
    group_by(participant, font) %>%
    summarize(beauty_rating = mean(beauty_rating), .groups = "drop")
  
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
    print("Beauty vs Comfort")
  print(unique(combined_data$font))
  print(unique(fontColors_perisan$font))
  print(known_map$font)
  print(cols_vec)
  p <- p + scale_color_manual(values = cols_vec)
  
  p + guides(color = guide_legend(title = "Font", ncol = 2))
}

# Scatter plot: Beauty vs Crowding  
familiarity_vs_crowding_scatter <- function(df_list, font_colors = NULL) {
  # Get beauty data from QA
  familiarity_data <- df_list$QA %>%
    filter(grepl('familiarity', tolower(questionAndAnswerNickname))) %>%
    mutate(familiarity = as.numeric(arabic_to_western(questionAndAnswerResponse)),
           font = case_when(conditionName=="beauty-Al-Awwal" ~"Al-Awwal-Regular.ttf",
                            conditionName=="beauty-majalla" ~"majalla.ttf",
                            conditionName=="beauty-Saudi" ~"Saudi-Regular.ttf",
                            conditionName=="beauty-Nazanin" ~"B-NAZANIN.TTF",
                            conditionName=="beauty-Titr" ~ "Titr.bold.woff2",
                            conditionName=="beauty-Kalameh" ~ "Kalameh-Regular.ttf",
                            conditionName=="beauty-IranNastaliq" ~ "IranNastaliq.ttf",
                            conditionName=="beauty-Moalla" ~ "Moalla.ttf",
                            conditionName=="beauty-MJ-Hoor" ~ "Mj-Hoor_0.ttf",
                            conditionName=="beauty-SaudiTextv1" ~"SaudiTextv1-Regular.ttf",
                            conditionName=="beauty-SaudiTextv2" ~"SaudiTextv2-Regular.ttf",
                            conditionName=="beauty-SaudiTextv3" ~"SaudiTextv3-Regular.ttf",
                            TRUE ~ conditionName)) %>%
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
