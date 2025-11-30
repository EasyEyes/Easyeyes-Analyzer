library(dplyr)
library(broom)
library(purrr)
library(ggpp)
library(stringr)
prepare_regression_data <- function(df_list){
  reading <- df_list$reading %>% mutate(participant = tolower(participant))
  crowding <- df_list$crowding %>% mutate(participant = tolower(participant))
  rsvp_speed <- df_list$rsvp %>% mutate(participant = tolower(participant))

  reading_each <- reading %>% 
    group_by(participant, font) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)),
                     .groups = "drop")
  

  crowding_summary <- crowding %>% 
    mutate(type = ifelse(targetEccentricityXDeg == 0,'Foveal', 'Peripheral')) %>% 
    group_by(participant, type, conditionName, targetEccentricityXDeg, font) %>% 
    summarize(crowding_distance = 10^(mean(log_crowding_distance_deg)),
              .groups="drop")
  
  reading_crowding <- reading_each %>% 
    group_by(participant, font) %>% 
    summarize(avg_log_WPM = mean(log10(avg_wordPerMin)),
              .groups="drop") %>% 
    left_join(crowding_summary, by = c("participant", "font")) %>% 
    mutate(targetKind = "reading")
  
  crowding_vs_rsvp <- rsvp_speed %>% 
    group_by(participant, font) %>% 
    summarize(avg_log_WPM = mean(block_avg_log_WPM),
              .groups="drop") %>% 
    left_join(crowding_summary, by = c("participant", "font"))
    

  crowding_vs_rsvp_summary <- crowding_vs_rsvp %>% 
    mutate(targetKind = "rsvpReading")
  
  t <- rbind(crowding_vs_rsvp_summary, reading_crowding)
  if (nrow(t>1)) {
    corr <- t %>% group_by(targetKind, type) %>% 
      summarize(correlation = round(cor(log10(crowding_distance),avg_log_WPM, 
                                        use = "pairwise.complete.obs",
                                        method = "pearson"),2),
                .groups="drop") %>% 
      ungroup()
    t <- t %>% left_join(corr, by = c("targetKind", "type"))
  } else {
    t$correlation = NA
  }
  t <- t %>% filter(!is.na(avg_log_WPM),!is.na(crowding_distance))
  return(t)
}

prepare_regression_acuity <- function(df_list){
  reading <- df_list$reading 
  acuity <- df_list$acuity %>%  filter(targetEccentricityXDeg == 0)
  rsvp_speed <- df_list$rsvp
  if ((nrow(reading) == 0 & nrow(rsvp_speed) == 0) | nrow(acuity) == 0) {
    return(tibble())
  }
  
  reading <- reading %>%
    mutate(participant = tolower(participant)) %>% 
    group_by(participant, conditionName, targetKind, font) %>%
    dplyr::summarize(avg_wordPerMin = 10^(mean(log10(wordPerMin), na.rm = T)),
                     avg_log_WPM = mean(log10(wordPerMin), na.rm = T),
                     .groups="drop") %>% 
    mutate(targetKind = as.character(targetKind))
    
  
  acuity <- acuity %>%
    mutate(participant = tolower(participant)) %>% 
    select(participant, questMeanAtEndOfTrialsLoop, conditionName, font)
  
  rsvp_speed <- rsvp_speed %>% 
    mutate(participant = tolower(participant)) %>% 
    group_by(participant, targetKind, font) %>% 
    summarize(avg_log_WPM = mean(block_avg_log_WPM),
              avg_wordPerMin = 10^mean(block_avg_log_WPM),
              .groups="drop") %>% 
    mutate(targetKind = as.character(targetKind))
  
  dt <- rbind(reading,rsvp_speed)
  
  # if (setequal(dt$font, acuity$font)) {
  if ( n_distinct(dt$font) > 1 || n_distinct(acuity$font) > 1) {
    dt <- dt %>% 
      left_join(acuity, by = c('participant', "font"))
  } else {
    dt <- dt %>% 
      left_join(acuity, by = c('participant')) %>% 
      mutate(font = paste0(font.x, 'vs ', font.y))
  }
  
  return(dt)
}
regression_reading_plot <- function(df_list){
  t <- prepare_regression_data(df_list)
  # Foveal subset (unchanged)
  foveal <- t %>%
    filter(type == 'Foveal')
  
  p1 <- NULL
  if (nrow(foveal) > 0) {
    p1 <- ggplot(foveal, aes(x = crowding_distance, 
                             y = 10^(avg_log_WPM), 
                             color = paste(targetKind, font),
                             shape = targetKind)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
      scale_x_log10() +
      scale_y_log10() +
      coord_fixed(ratio = 1) +
      labs(
        x     = "Foveal crowding (deg)",
        y     = "Reading speed (w/min)",
        subtitle = "Reading vs foveal crowding"
      ) +
      annotation_logticks(sides = "bl") +
      ggpp::geom_text_npc(
        aes(npcx = "left", npcy = "top"),
        label = paste0("N=", nrow(foveal)),
        inherit.aes = FALSE
      ) +
      guides(color = guide_legend(ncol = 1, title = "")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }
  
  # Peripheral subset with new legend + subtitle
  peripheral <- t %>%
    filter(type == 'Peripheral')
  
  p2 <- NULL
  if (nrow(peripheral) > 0) {
    # 1) compute N & R per font
    stats_font <- peripheral %>%
      group_by(font) %>%
      mutate(N = n()) %>% 
      ungroup() %>% 
      group_by(font, targetKind, N) %>% 
      summarize(
        R = cor(log10(crowding_distance),
                avg_log_WPM,
                use = "complete.obs"),
        .groups = "drop"
      )  %>%
      mutate(
        R = round(R, 2)
      )
    print(stats_font)
    reading_stats <- stats_font %>% filter(targetKind == "reading")
    rsvp_stats <- stats_font %>% filter(targetKind == "rsvpReading")
    stats_font <- reading_stats %>%
      full_join(rsvp_stats, by = "font") %>%
      mutate(
        font_safe = ifelse(is.na(font), "", font),
        N_safe = ifelse(is.na(N.x), "NA", as.character(N.y)),
        R_reading = ifelse(is.na(R.x), "NA", sprintf("%.2f", R.x)),
        R_rsvp = ifelse(is.na(R.y), "NA", sprintf("%.2f", R.y)),
        label = paste0(font_safe, ", N=", N_safe, ", R_reading=", R_reading, ", R_rsvp=", R_rsvp)
      ) %>%
      select(-font_safe, -N_safe, -R_reading, -R_rsvp)
    print(stats_font)
    # 2) rename font factor to include N & R
    peripheral <- peripheral %>%
      mutate(
        font_label = factor(font,
                            levels = stats_font$font,
                            labels = stats_font$label)
      )
    
    print(peripheral %>% filter(is.na(font_label)), n = 100)
    
    # 3) build Ecc caption
    eccs     <- sort(unique(peripheral$targetEccentricityXDeg))
    eccs_int <- as.integer(round(eccs))
    ecc_label <- paste0("X ecc = ", paste(eccs_int, collapse = ", "), " deg")
    
    p2 <- ggplot(data= peripheral) +
      geom_point(aes(
        x = crowding_distance,
        y = 10^(avg_log_WPM),
        color = font_label,
        shape = targetKind)) +
      # one regression per font because color is mapped globally
      geom_smooth(aes(
        x = crowding_distance,
        y = 10^(avg_log_WPM),
        color = font_label,
        linetype = targetKind),
        method = "lm", formula = y ~ x, se = FALSE) +
      scale_x_log10() +
      scale_y_log10() +
      coord_fixed(ratio = 1) +
      labs(
        x        = "Peripheral crowding (deg)",
        y        = "Reading speed (w/min)",
        subtitle    = "Reading vs peripheral crowding\nGeometric mean of left and right."
      ) +
      annotation_logticks(sides = "bl") +
      ggpp::geom_text_npc(
        aes(npcx = "left", npcy = "top"),
        label = paste0(ecc_label, "\nN=", nrow(peripheral)),
        inherit.aes = FALSE
      ) +
      guides(color = guide_legend(ncol = 1, title = "")) +
      theme_bw() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.margin     = margin(10, 10, 10, 10)
      )
  }
  
  return(list(foveal = p1, peripheral = p2))
}
# regression_reading_plot <- function(df_list){
#   t <- prepare_regression_data(df_list)
#   # plot for the regression
#   
#   foveal <- t %>%
#     # mutate(targetKind = paste0(targetKind, ", ", font ,", R = ", correlation)) %>%
#     mutate(targetKind = paste0(targetKind,  ", R = ", correlation)) %>%
#     filter(type == 'Foveal')
#   
#   peripheral <- t %>% 
#     mutate(targetKind = paste0(targetKind,  ", R = ", correlation)) %>%
#     # mutate(targetKind = paste0(targetKind, ", ", font ,", R = ", correlation)) %>%
#     filter(type == 'Peripheral')
# 
#   p1 <- NULL
#   p2 <- NULL
#   
#   if (nrow(foveal) > 0) {
#     p1 <- ggplot(foveal,aes(x = crowding_distance, y = 10^(avg_log_WPM))) + 
#       geom_point(aes(color = font, shape = targetKind)) +
#       geom_smooth(method = "lm",formula = y ~ x, se=F) + 
#       scale_x_log10() + 
#       scale_y_log10() +
#       coord_fixed(ratio = 1) + 
#       labs(x="Foveal crowding (deg)", 
#            y = "Reading speed (w/min)",
#            title = "Reading vs foveal crowding") +
#       theme_bw() + 
#       annotation_logticks(
#       sides = "bl", 
#       short = unit(2, "pt"), 
#       mid   = unit(2, "pt"), 
#       long  = unit(7, "pt")
#     ) +
#       ggpp::geom_text_npc(aes(
#         npcx = "left",
#         npcy = "top",
#         label = paste0('N=', nrow(foveal))
#       )) + 
#       guides(color=guide_legend(ncol = 1,
#                                 title = '')) + 
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),# Rotate x-axis labels
#         plot.margin = margin(10, 10, 10, 10) 
#       ) 
#   }
#   
#   if (nrow(peripheral) > 0) {
#     print("in regression_reading_plot peripheral")
# 
#     eccs     <- sort(unique(peripheral$targetEccentricityXDeg))
#     eccs_int <- as.integer(round(eccs))
#     ecc_label <- paste0("EccX = ", paste(eccs_int, collapse = ", "), " deg")
#     
#     p2 <- ggplot(peripheral, aes(x = crowding_distance, y = 10^(avg_log_WPM))) + 
#       geom_point(aes(color = font, shape = targetKind)) +
#       geom_smooth(method = "lm",formula = y ~ x, se=F) + 
#       scale_x_log10() + 
#       scale_y_log10() +
#       coord_fixed(ratio = 1) + 
#       labs(x="Peripheral crowding (deg)", 
#            y = "Reading speed (w/min)",
#            title = "Reading vs peripheral crowding") +
#       theme_bw() + 
#       annotation_logticks(
#         sides = "bl", 
#         short = unit(2, "pt"), 
#         mid   = unit(2, "pt"), 
#         long  = unit(7, "pt")
#       ) +
#       ggpp::geom_text_npc(aes(
#         npcx = "left",
#         npcy = "top",
#         label = paste0(ecc_label, "\n", "N=", nrow(peripheral))
#       )) + 
#       guides(color=guide_legend(nrow=2, byrow=TRUE,
#                                 title = ''),
#              shape = guide_legend(ncol=1,
#                                   title = '')) +
#       theme(
#         axis.text.x = element_text(angle = 45, hjust = 1),# Rotate x-axis labels
#         plot.margin = margin(10, 10, 10, 10) 
#       ) 
#   }
# 
#   return(list(foveal = p1, peripheral = p2))
# 
# }

regression_acuity_plot <- function(df_list){
  t <- prepare_regression_acuity(df_list)
  if (nrow(t>1)) {
    corr <- t %>% group_by(targetKind) %>% 
      summarize(correlation = round(cor(questMeanAtEndOfTrialsLoop,avg_log_WPM, 
                                        use = "pairwise.complete.obs",
                                        method = "pearson"),2),
                .groups="drop")
    t <- t %>% left_join(corr, by = "targetKind")
  } else {
    return(NULL)
  }
  t <- t %>% mutate(targetKind = paste0(targetKind, ", R = ", correlation))
  p <- ggplot(t,aes(x = 10^(questMeanAtEndOfTrialsLoop), y = 10^(avg_log_WPM), color = targetKind)) + 
    geom_point(aes(shape = conditionName)) +
    geom_smooth(method = "lm",formula = y ~ x, se=F) + 
    scale_x_log10(breaks=c(0.01,0.03,0.1,0.3,1)) + 
    scale_y_log10(breaks=c(30,100,300,1000)) +
    labs(x="Foveal acuity (deg)",
         y = "Reading speed (w/min)",
         subtitle = 'Ordinary and RSVP reading vs\nfoveal acuity') +
    theme_bw() + 
    coord_fixed() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste0('N=', nrow(t))
    )) + 
    guides(color=guide_legend(nrow=2, byrow=TRUE,
                              title = ''))
  return(p)
}

regression_and_mean_plot_byfont <- function(df_list, reading_rsvp_crowding_df){
  t <- prepare_regression_data(df_list)
  rsvp_vs_ordinary_vs_crowding <- reading_rsvp_crowding_df[[1]]
  corr_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>% 
    summarize(correlation = cor(avg_log_SpeedWPM, mean_bouma_factor, 
                                method = "pearson"),
              .groups="drop") %>% 
    mutate(correlation = round(correlation,2))
  
  counts = t %>% group_by(font, targetKind) %>% summarize(N = n())
  if (min(counts$N) == max(counts$N)) {
    N_text <-min(counts$N)
  } else {
    N_text <- paste0(min(counts$N),"~to~",max(counts$N))
  }
  result_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>%
    do(fit = lm(avg_log_SpeedWPM ~ mean_bouma_factor, data = .)) %>% 
    ungroup() %>% 
    transmute(targetKind, coef = map(fit, tidy)) %>% 
    unnest(coef) %>% 
    filter(term == "mean_bouma_factor") %>% 
    select(-statistic, -p.value) %>% 
    rename("slope" = "estimate",
           "SD" = "std.error") %>% 
    mutate(slope = round(slope, 2),
           SD = round(SD, 2)) %>% 
    select(-term)
  
  rsvp_vs_ordinary_vs_crowding <- rsvp_vs_ordinary_vs_crowding %>% 
    inner_join(result_means, by = c("targetKind")) %>% 
    inner_join(corr_means, by = c("targetKind"))
  
  rsvp_vs_ordinary_vs_crowding <- 
    rsvp_vs_ordinary_vs_crowding %>% 
    mutate(legend = paste0(targetKind, ", slope = ", slope, ", R = ", correlation))
  
  
  # Calculate dynamic limits based on actual data including error bars
  y_values <- 10^(rsvp_vs_ordinary_vs_crowding$avg_log_SpeedWPM)
  y_with_error <- c(
    10^(rsvp_vs_ordinary_vs_crowding$avg_log_SpeedWPM - rsvp_vs_ordinary_vs_crowding$se),
    10^(rsvp_vs_ordinary_vs_crowding$avg_log_SpeedWPM + rsvp_vs_ordinary_vs_crowding$se)
  )
  all_y <- c(y_values, y_with_error)
  
  x_values <- rsvp_vs_ordinary_vs_crowding$mean_bouma_factor
  x_with_error <- c(
    rsvp_vs_ordinary_vs_crowding$mean_bouma_factor - rsvp_vs_ordinary_vs_crowding$se_bouma_factor,
    rsvp_vs_ordinary_vs_crowding$mean_bouma_factor + rsvp_vs_ordinary_vs_crowding$se_bouma_factor
  )
  all_x <- c(x_values, x_with_error)
  
  if (length(all_y) > 0 && !all(is.na(all_y))) {
    y_limits <- c(min(all_y, na.rm = TRUE) * 0.8, max(all_y, na.rm = TRUE) * 1.2)
  } else {
    y_limits <- c(100, 2000)  # fallback
  }
  
  if (length(all_x) > 0 && !all(is.na(all_x))) {
    x_limits <- c(min(all_x, na.rm = TRUE) * 0.8, max(all_x, na.rm = TRUE) * 1.2)
  } else {
    x_limits <- NULL  # let ggplot auto-scale
  }
  
  # regression and font mean
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, aes(x = mean_bouma_factor, 
                                                       y = 10^(avg_log_SpeedWPM))) + 
    geom_point(aes(shape = targetKind, color = font), size = 6) +
    geom_smooth(aes(linetype = legend),
                color = "black", method = "lm",formula = y ~ x, se=F,
                fullrange=T) +
    scale_linetype_manual(values = c(1, 2)) +
    geom_errorbar(aes(ymin=10^(avg_log_SpeedWPM-se),
                      ymax=10^(avg_log_SpeedWPM+se)), width=0) +
    geom_errorbar(aes(xmin=(mean_bouma_factor-se_bouma_factor),
                      xmax=(mean_bouma_factor+se_bouma_factor)), width=0) +
    # geom_point(data = t, aes(x = bouma_factor, y = 10^(avg_log_WPM), color = font, shape = targetKind),alpha = 0.5) +
    # geom_smooth(data = t, 
    #             aes(x = bouma_factor, 
    #                 y = 10^(avg_log_WPM), 
    #                 color = font,
    #                 linetype = targetKind),
    #             method = "lm",
    #             formula = y ~ x, 
    #             se=F,
    #             fullrange=T) + 
    scale_y_log10(limits = y_limits) +
    scale_x_log10(limits = x_limits) + 
    coord_fixed(ratio = 1) +
    labs(x="Bouma factor", 
         y = "Reading speed (w/min)") +
    theme_bw() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    guides(color = guide_legend(title="Font"),
           shape = F,
           linetype = guide_legend(title = NULL,
                                   keywidth = unit(2, "cm"))) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  return(p)
}

regression_font <- function(df_list, reading_rsvp_crowding_df){
  t <- prepare_regression_data(df_list)
  rsvp_vs_ordinary_vs_crowding <- reading_rsvp_crowding_df[[1]]
  corr_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>% 
    summarize(correlation = cor(avg_log_SpeedWPM, mean_bouma_factor, 
                                method = "pearson"),
              .groups="drop") %>% 
    mutate(correlation = round(correlation,2))
  
  counts = t %>% group_by(font, targetKind) %>% summarize(N = n())
  if (min(counts$N) == max(counts$N)) {
    N_text <-min(counts$N)
  } else {
    N_text <- paste0(min(counts$N),"~to~",max(counts$N))
  }
  result_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>%
    do(fit = lm(avg_log_SpeedWPM ~ mean_bouma_factor, data = .)) %>% 
    ungroup() %>% 
    transmute(targetKind, coef = map(fit, tidy)) %>% 
    unnest(coef) %>% 
    filter(term == "mean_bouma_factor") %>% 
    select(-statistic, -p.value) %>% 
    rename("slope" = "estimate",
           "SD" = "std.error") %>% 
    mutate(slope = round(slope, 2),
           SD = round(SD, 2)) %>% 
    select(-term)
  
  rsvp_vs_ordinary_vs_crowding <- rsvp_vs_ordinary_vs_crowding %>% 
    inner_join(result_means, by = c("targetKind")) %>% 
    inner_join(corr_means, by = c("targetKind"))
  
  rsvp_vs_ordinary_vs_crowding <- 
    rsvp_vs_ordinary_vs_crowding %>% 
    mutate(legend = paste0(targetKind, ", slope = ", slope, ", R = ", correlation)) %>% 
    mutate(fontlabel = paste0(font, " - ", substr(font, start = 1, stop = 3)))
  
  # Calculate dynamic x-axis limits based on actual data
  x_values <- rsvp_vs_ordinary_vs_crowding$mean_bouma_factor
  if (length(x_values) > 0 && !all(is.na(x_values))) {
    x_limits <- c(min(x_values, na.rm = TRUE) * 0.8, max(x_values, na.rm = TRUE) * 1.2)
  } else {
    x_limits <- NULL  # let ggplot auto-scale
  }
  
  # regression and font mean
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, aes(x = mean_bouma_factor, 
                                                       y = 10^(avg_log_SpeedWPM))) + 
    geom_text(aes(label = substr(font, start = 1, stop = 3), 
                  color = targetKind), check_overlap = T) +
    geom_smooth(aes(linetype = legend),
                color = "black", method = "lm",formula = y ~ x, se=F,
                fullrange=T) +
    scale_linetype_manual(values = c(1, 2)) +
    scale_y_log10() +
    scale_x_log10(limits = x_limits) + 
    coord_fixed(ratio = 1) +
    labs(x="Bouma factor", 
         y = "Reading speed (w/min)") +
    theme_bw() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    guides(color = guide_legend(title="TargetKind"),
           shape = F,
           linetype = guide_legend(title = NULL,
                                   keywidth = unit(2, "cm")),
           fill = guide_legend(title = "Font")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  return(p)
}

regression_font_with_label <- function(df_list, reading_rsvp_crowding_df){
  t <- prepare_regression_data(df_list)
  rsvp_vs_ordinary_vs_crowding <- reading_rsvp_crowding_df[[1]]
  corr_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>% 
    summarize(correlation = cor(avg_log_SpeedWPM, mean_bouma_factor, 
                                method = "pearson"),
              .groups="drop") %>% 
    mutate(correlation = round(correlation,2))
  
  counts = t %>% group_by(font, targetKind) %>% summarize(N = n())
  if (min(counts$N) == max(counts$N)) {
    N_text <-min(counts$N)
  } else {
    N_text <- paste0(min(counts$N),"~to~",max(counts$N))
  }
  result_means <- rsvp_vs_ordinary_vs_crowding %>% 
    group_by(targetKind) %>%
    do(fit = lm(avg_log_SpeedWPM ~ mean_bouma_factor, data = .)) %>% 
    ungroup() %>% 
    transmute(targetKind, coef = map(fit, tidy)) %>% 
    unnest(coef) %>% 
    filter(term == "mean_bouma_factor") %>% 
    select(-statistic, -p.value) %>% 
    rename("slope" = "estimate",
           "SD" = "std.error") %>% 
    mutate(slope = round(slope, 2),
           SD = round(SD, 2)) %>% 
    select(-term)
  
  rsvp_vs_ordinary_vs_crowding <- rsvp_vs_ordinary_vs_crowding %>% 
    inner_join(result_means, by = c("targetKind")) %>% 
    inner_join(corr_means, by = c("targetKind"))
  
  rsvp_vs_ordinary_vs_crowding <- 
    rsvp_vs_ordinary_vs_crowding %>% 
    mutate(legend = paste0(targetKind, ", slope = ", slope, ", R = ", correlation)) %>% 
    mutate(fontlabel = paste0(font, " - ", substr(font, start = 1, stop = 3)),
           font_family = case_when(font == "TimesNewRomanRegularMonotype.woff2" ~ "Times New Roman",
                                   font == "Agoesa.woff2" ~ "Agoesa",
                                   font == "Quela.woff2" ~ "Quela",
                                   TRUE ~ "Arial"))
  # Calculate dynamic x-axis limits based on actual data
  x_values <- rsvp_vs_ordinary_vs_crowding$mean_bouma_factor
  if (length(x_values) > 0 && !all(is.na(x_values))) {
    x_limits <- c(min(x_values, na.rm = TRUE) * 0.8, max(x_values, na.rm = TRUE) * 1.2)
  } else {
    x_limits <- NULL  # let ggplot auto-scale
  }
  
  # regression and font mean
  
  p <- ggplot(data = rsvp_vs_ordinary_vs_crowding, aes(x = mean_bouma_factor, 
                                                       y = 10^(avg_log_SpeedWPM))) + 
    geom_point(aes(fill = fontlabel), alpha = 0)+
    geom_text(aes(label = substr(font, start = 1, stop = 3), 
                  color = targetKind,
                  family = font_family), check_overlap = T) +
    geom_smooth(aes(linetype = legend),
                color = "black", method = "lm",formula = y ~ x, se=F,
                fullrange=T) +
    scale_linetype_manual(values = c(1, 2)) +
    # geom_errorbar(aes(ymin=10^(avg_log_SpeedWPM-se),
    #                   ymax=10^(avg_log_SpeedWPM+se)), width=0) +
    # geom_errorbar(aes(xmin=(mean_bouma_factor-se_bouma_factor),
    #                   xmax=(mean_bouma_factor+se_bouma_factor)), width=0) +
    # geom_point(data = t, 
    #            aes(x = bouma_factor, 
    #                y = 10^(avg_log_WPM), 
    #                color = font, 
    #                shape = targetKind),
    #            alpha = 0.5) +
    # geom_smooth(data = t,
    #             aes(x = bouma_factor,
    #                 y = 10^(avg_log_WPM),
    #                 color = font,
    #                 linetype = targetKind),
    #             method = "lm",
    #             formula = y ~ x,
    #             se=F,
    #             fullrange=T) +
    scale_y_log10() +
    scale_x_log10(limits = x_limits) + 
    coord_fixed(ratio = 1) +
    labs(x="Bouma factor", 
         y = "Reading speed (w/min)") +
    theme_bw() + 
    annotation_logticks(
      sides = "bl", 
      short = unit(2, "pt"), 
      mid   = unit(2, "pt"), 
      long  = unit(7, "pt")
    ) +
    guides(color = guide_legend(title="TargetKind"),
           shape = F,
           linetype = guide_legend(title = NULL,
                                   keywidth = unit(2, "cm")),
           fill = guide_legend(title = "Font")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "bottom",
          label = paste0("italic('N=')~", N_text)), 
      parse = T)
  return(p)
}
