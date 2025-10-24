plot_auditory_crowding <- function(quest, crowding) {
  # Return two plots:
  # 1) Histogram of each participant’s mean auditoryCrowdingMelodyDb
  # 2) Scatter of auditoryCrowdingMelodyDb vs. crowding threshold (crowding on log scale)

  if (is.null(quest) || nrow(quest) == 0) {
    return(list(hist = NULL, scatter = NULL))
  }

  # Prepare per-block QUEST thresholds
  t_melody <- quest %>%
    filter(conditionName == "Target in melody") %>%
    mutate(block = sub("(_.*)$", "", block_condition)) %>%
    select(participant, block, t_melody = questMeanAtEndOfTrialsLoop)

  t_noise <- quest %>%
    filter(conditionName == "Target in pulsing noise") %>%
    mutate(block = sub("(_.*)$", "", block_condition)) %>%
    select(participant, block, t_pulsing_noise = questMeanAtEndOfTrialsLoop)

  joined <- inner_join(t_melody, t_noise, by = c("participant", "block")) %>%
    mutate(auditoryCrowdingMelodyDb = 20 * (t_melody - t_pulsing_noise))

  sound_t <- joined %>%
    group_by(participant) %>%
    summarize(auditoryCrowdingMelodyDb = mean(auditoryCrowdingMelodyDb, na.rm = TRUE), .groups = "drop")

  p_hist <- NULL
  p_scatter <- NULL

  if (nrow(sound_t) > 0) {
    if (nrow(sound_t) == 1) {
      v <- sound_t$auditoryCrowdingMelodyDb[1]
      bw <- 0.5  # use a small fixed bin width so the single bar is thin
      p_hist <- ggplot(sound_t, aes(x = auditoryCrowdingMelodyDb)) +
        geom_histogram(color = "black", fill = "grey", binwidth = bw, alpha = 0.8,
                        boundary = v - bw/2) +
        scale_x_continuous(limits = c(max(0, v - 2.5), v + 2.5), expand = expansion(mult = c(0, 0.1))) +
        scale_y_continuous(limits = c(0, 1.2), breaks = 0:1, expand = expansion(mult = c(0, 0.1))) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0("N=", dplyr::n_distinct(sound_t$participant))) +
        labs(subtitle = "Histogram of mean \nauditoryCrowdingMelodyDb",
             x = "auditoryCrowdingMelodyDb (dB)",
             y = "Count") +
        theme_bw()
    } else {
      p_hist <- ggplot(sound_t, aes(x = auditoryCrowdingMelodyDb)) +
        geom_histogram(color = "black", fill = "grey", bins = 20, alpha = 0.8) +
        scale_x_continuous(limits = c(ifelse(is.finite(min(sound_t$auditoryCrowdingMelodyDb, na.rm = TRUE)) && min(sound_t$auditoryCrowdingMelodyDb, na.rm = TRUE) >= 0, 0, NA), NA),
                           expand = expansion(mult = c(0, 0.05))) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        ggpp::geom_text_npc(aes(npcx = "left", npcy = "top"),
                            label = paste0("N=", dplyr::n_distinct(sound_t$participant))) +
        labs(subtitle = "Histogram of mean \nauditoryCrowdingMelodyDb",
             x = "auditoryCrowdingMelodyDb (dB)",
             y = "Count") +
        theme_bw()
    }
  }

  # Scatter: join with crowding thresholds and plot with log-scale crowding
  if (!missing(crowding) && !is.null(crowding) && nrow(crowding) > 0) {
    crowd_t <- crowding %>%
      group_by(participant) %>%
      summarize(m_log = mean(log_crowding_distance_deg, na.rm = TRUE), .groups = "drop") %>%
      mutate(crowding_threshold_deg = 10^m_log)

    joined_scatter <- inner_join(sound_t, crowd_t, by = "participant")

    if (nrow(joined_scatter) > 0) {
      p_scatter <- ggplot(joined_scatter, aes(x = crowding_threshold_deg, y = auditoryCrowdingMelodyDb)) +
        geom_point(aes(color = participant), size = 2, alpha = 0.8) +
        scale_x_log10(limits = c(0.1, 2), expand = expansion(mult = c(0, 0.05))) +
        scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
        annotation_logticks(sides = "b") +
        scale_color_manual(values = colorPalette) +
        labs(subtitle = "auditoryCrowdingMelodyDb vs. crowding threshold\nGeometric mean of left and right measurements",
             x = "Crowding distance (deg)",
             y = "auditoryCrowdingMelodyDb (dB)") +
        theme_bw()
    }
  }

  return(list(hist = p_hist, scatter = p_scatter))
}