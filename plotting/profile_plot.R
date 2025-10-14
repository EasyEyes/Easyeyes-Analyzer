breakPoints = c(seq(30,100,10),seq(300,1000,100),seq(3000,10000,1000))
preprocessProfiles <- function(transducerType, t) {
  dt <- tibble()
  if (transducerType == "Microphones" && ("linear" %in% names(t))) {
    for (i in 1:length(t$linear$Freq)) {
      if (t$isDefault[i]) {
        tmp <- tibble(
          freq = seq(20,20000,5),
          gain = approx(x = t$linear$Freq[[i]], y = t$linear$Gain[[i]], xout = seq(20,20000,5))$y,
          isDefault = t$isDefault[i],
          label = ifelse(t$isDefault[i],paste0("default/", t$ID[i],"/",t$DateText[i]),t$DateText[i])
          ) %>% 
          filter(!is.na(label))
      } else {
        tmp <- tibble(
          freq = t$linear$Freq[[i]],
          gain = t$linear$Gain[[i]],
          isDefault = t$isDefault[i],
          label = ifelse(t$isDefault[i],paste0("default/", t$ID[i],"/",t$DateText[i]),t$DateText[i])
        ) %>% 
          filter(!is.na(label))
      }
      
      
      dt <- dt %>% rbind(tmp)
    }
  } else if ("ir" %in% names(t)) {
    for (i in 1:length(t$ir$Freq)) {
      tmp <- tibble(
        freq = t$ir$Freq[[i]],
        gain = t$ir$Gain[[i]],
        label = t$CalibrationDate[i]
      )
      if (ncol(tmp) == 3) {
        dt <- dt %>% rbind(tmp)
      }
    }
  }
  return(dt)
}

plot_profiles_avg <- function(dt) {
  dt <- dt %>%
    filter(is.finite(gain), freq >= 20, freq <= 20000)
  if (nrow(dt) == 0) {
    return(list(height = NA, plot = ggplot() + theme_bw(), tb = tibble()))
  }

  freq_grid <- sort(unique(c(seq(20, 200, 1), seq(200, 2000, 10), seq(2000, 20000, 100))))

  labels <- unique(dt$label)
  interp_list <- lapply(labels, function(lbl) {
    d <- dt %>%
      filter(label == lbl, is.finite(freq), is.finite(gain)) %>%
      arrange(freq)
    if (nrow(d) < 2) return(NULL)
    d <- d %>%
      group_by(freq) %>%
      summarize(gain = mean(gain, na.rm = TRUE), .groups = "drop") %>%
      arrange(freq)
    if (nrow(d) < 2) return(NULL)
    y <- approx(x = d$freq, y = d$gain, xout = freq_grid, rule = 2)$y
    tibble(freq = freq_grid, gain = y, label = lbl)
  })

  interp_list <- interp_list[!sapply(interp_list, is.null)]
  if (length(interp_list) == 0) {
    return(list(height = NA, plot = ggplot() + theme_bw(), tb = tibble()))
  }
  interp <- do.call(rbind, interp_list)

  n_profiles <- n_distinct(interp$label)
  agg <- interp %>%
    group_by(freq) %>%
    summarize(avg = mean(gain, na.rm = TRUE), std = sd(gain, na.rm = TRUE), .groups = "drop") %>%
    mutate(upper = avg + std, lower = avg - std)

  if (n_profiles == 1) {
    maxY <- ceiling(max(agg$avg, na.rm = TRUE) / 10) * 10
    minY <- floor(min(agg$avg, na.rm = TRUE) / 10) * 10
    p <- ggplot() +
      geom_line(data = agg, aes(x = freq, y = avg), color = 'black') +
      guides(color = FALSE)
  } else {
    maxY <- ceiling(max(agg$upper, na.rm = TRUE) / 10) * 10
    minY <- floor(min(agg$lower, na.rm = TRUE) / 10) * 10
    p <- ggplot() +
      geom_line(data = agg, aes(x = freq, y = avg), color = 'black') +
      geom_ribbon(data = agg, aes(x = freq, ymin = lower, ymax = upper), fill = 'pink', alpha = 0.4) +
      guides(fill = FALSE, color = FALSE)
  }

  p <- p +
    scale_x_log10(limits = c(20, 20000),
                  breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                  minor_breaks = breakPoints,
                  expand = c(0, 0)) +
    scale_y_continuous(limits = c(minY, maxY), breaks = seq(minY, maxY, 10), expand = c(0, 0)) +
    theme_bw() +
    theme(plot.margin = margin(t = .3, r = .4, b = 0, l = 0, "inch")) +
    sound_theme_display +
    labs(x = 'Frequency (Hz)', y = 'Gain (dB)', subtitle = 'Average of profiles')

  height <- ceiling((maxY - minY) / 15) + 0.3

  tb <- agg %>%
    filter(freq %in% c(50, 100, 300, 1000, 3000, 6000)) %>%
    mutate(`Freq (Hz)` = format(freq, nsmall = 0),
           `mean (dB)` = format(round(avg, 1), nsmall = 1),
           `SD (dB)` = format(round(std, 1), nsmall = 1)) %>%
    select(`Freq (Hz)`, `mean (dB)`, `SD (dB)`)

  return(list(height = height, plot = p, tb = tb))
}

plot_profiles <- function(dt, plotTitle) {
  dt <- dt %>% filter(is.finite(gain), freq >= 20, freq <= 20000) %>% arrange(label)
  tmp <- dt %>% group_by(label) %>% summarize(gain1000 = approx(x = freq, y = gain, xout = 1000)$y,
                                              .groups="drop")
  t <- tmp %>% ungroup() %>% summarize(sd = format(round(sd(gain1000) ,1), nsmall = 1),
                                       .groups="drop")
  # t <- dt %>% filter(freq == 1000) %>% summarize(sd = round(sd(gain),1))
  legendRows <- ceiling(n_distinct(dt$label)/2)
  maxY <- ceiling(max(dt$gain) /10) * 10
  minY <- floor(min(dt$gain) /10) * 10
  color_options <- c(
    "red", "mediumorchid4", 'blue3', 'forestgreen', 'turquoise4', 'mediumvioletred',
    'orangered3', 'tan4', 'seagreen4', 'aquamarine3', 'darkgoldenrod4'
  )
  linetype_options <- c("solid", "dashed", "dotted", "twodash","longdash", "dotdash")
  colors <- color_options[0 : n_distinct(dt$label) %% length(color_options) + 1]
  linetypes <- linetype_options[(0 : n_distinct(dt$label) %/% length(color_options)) %% length(linetype_options) + 1]
  if ("isDefault" %in% names(dt)) {
    dt <- dt %>% mutate(linewidths = ifelse(isDefault, 1.2, 0.6))
    p <- ggplot() +
      geom_line(data = dt, aes(x = freq, y = gain, color = label, linetype = label, linewidth = linewidths)) +     
      scale_y_continuous(limits = c(minY ,maxY), breaks = seq(minY,maxY,10), expand = c(0,0)) + 
      scale_x_log10(limits = c(20, 20000), 
                    breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                    minor_breaks = breakPoints,
                    expand = c(0, 0)) +
      scale_color_manual(values = colors) + 
      scale_linetype_manual(values = linetypes) +
      scale_linewidth_identity(guide = "none") + 
      geom_text_npc(aes(npcx="left", npcy="top", label = paste(t$sd, " dB SD at 1000 Hz"))) + 
      theme_bw() +
      theme(legend.position="top",
            plot.margin = margin(
              t = 0,
              r = .4,
              b = 0,
              l = 0,
              "inch"
            ),
            legend.text = element_text(margin = margin(t = -5, b = -5, unit = "pt"))) +
      sound_theme_display +
      guides(color=guide_legend(byrow=TRUE, ncol = 2,
                                keyheight=0.3, unit = "inch")) +
      labs(title = plotTitle,
           x = "Frequency (Hz)",
           y = "Gain (dB)",
           color = "",
           linetype = "",
           linewidth = "")
  } else {
    p <- ggplot() +
      geom_line(data = dt, aes(x = freq, y = gain, color = label, linetype = label)) +     
      scale_y_continuous(limits = c(minY ,maxY), breaks = seq(minY,maxY,10), expand = c(0,0)) + 
      scale_x_log10(limits = c(20, 20000), 
                    breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                    minor_breaks = breakPoints,
                    expand = c(0, 0)) +
      scale_color_manual(values = colors) + 
      scale_linetype_manual(values = linetypes) +
      geom_text_npc(aes(npcx="left", npcy="top", label = paste(t$sd, " dB SD at 1000 Hz"))) + 
      theme_bw() +
      theme(legend.position="top",
            plot.margin = margin(
              t = 0,
              r = .4,
              b = 0,
              l = 0,
              "inch"
            ),
            legend.text = element_text(margin = margin(t = -5, b = -5, unit = "pt"))) +
      sound_theme_display +
      guides(color=guide_legend(byrow=TRUE, ncol = 2,
                                keyheight=0.3, unit = "inch")) +
      labs(subtitle = plotTitle,
           x = "Frequency (Hz)",
           y = "Gain (dB)",
           color = "",
           linetype = "")
  }
  
  
  height = ceiling((maxY - minY) / 15) + ceiling(legendRows/2)*0.03 + 1
  return (
    list(
      height = height,
      plot = p
    )
  )
}

plot_shifted_profiles <- function(dt, plotTitle) {
  dt <- dt %>% filter(is.finite(gain), freq >= 20, freq <= 20000)
  tmp <- dt %>% group_by(label) %>% summarize(gain1000 = approx(x = freq, y = gain, xout = 1000)$y)
  t <- tmp %>% ungroup() %>% summarize(avg = mean(gain1000), sd = format(round(sd(gain1000) ,1), nsmall = 1))
  tmp$diff <- tmp$gain1000 - t$avg
  tmp <- tmp %>% select(label, diff)
  shifted_dt <- dt %>% left_join(tmp, by = "label") %>% mutate(gain = gain - diff) %>% arrange(label)
  legendRows <- ceiling(n_distinct(dt$label)/2)
  maxY <- ceiling(max(dt$gain) /10) * 10
  minY <- floor(min(dt$gain) /10) * 10
  color_options <- c(
    "red", "mediumorchid4", 'blue3', 'forestgreen', 'turquoise4', 'mediumvioletred',
    'orangered3', 'tan4', 'seagreen4', 'aquamarine3', 'darkgoldenrod4'
  )
  linetype_options <- c("solid", "dashed", "dotted", "twodash","longdash", "dotdash")
  colors <- color_options[0 : n_distinct(dt$label) %% length(color_options) + 1]
  linetypes <- linetype_options[(0 : n_distinct(dt$label) %/% length(color_options)) %% length(linetype_options) + 1]
  if ("isDefault" %in% names(dt)) {

    shifted_dt <- shifted_dt %>%  mutate(linewidths = ifelse(isDefault, 1.2, 0.6))
    p <- ggplot() +
      geom_line(data = shifted_dt, aes(x = freq, y = gain, color = label, linetype = label, linewidth = linewidths)) +     
      scale_y_continuous(limits = c(minY ,maxY), breaks = seq(minY,maxY,10), expand = c(0,0)) + 
      scale_x_log10(limits = c(20, 20000), 
                    breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                    minor_breaks = breakPoints,
                    expand = c(0, 0)) +
      scale_color_manual(values = colors) + 
      scale_linetype_manual(values = linetypes) +
      scale_linewidth_identity(guide = "none") + 
      # scale_linewidth_manual(values = udt$linewidths) + 
      geom_text_npc(aes(npcx="left", npcy="top", label = paste(t$sd, " dB SD at 1000 Hz"))) + 
      theme_bw() +
      theme(legend.position="top",
            plot.margin = margin(
              t = 0,
              r = .4,
              b = 0,
              l = 0,
              "inch"
            ),
            legend.text = element_text(margin = margin(t = -5, b = -5, unit = "pt"))) +
      sound_theme_display +
      guides(color=guide_legend(byrow=TRUE, ncol = 2,
                                keyheight=0.3, unit = "inch")) +
      labs(title = plotTitle,
           x = "Frequency (Hz)",
           y = "Gain (dB)",
           color = "",
           linetype = "",
           linewidth = "")
  } else {
    p <- ggplot() +
      geom_line(data = shifted_dt, aes(x = freq, y = gain, color = label, linetype = label)) +     
      scale_y_continuous(limits = c(minY ,maxY), breaks = seq(minY,maxY,10), expand = c(0,0)) + 
      scale_x_log10(limits = c(20, 20000), 
                    breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                    expand = c(0, 0)) +
      scale_color_manual(values = colors) + 
      scale_linetype_manual(values = linetypes) +
      geom_text_npc(aes(npcx="left", npcy="top", label = paste(t$sd, " dB SD at 1000 Hz"))) + 
      theme_bw() +
      theme(legend.position="top",
            plot.margin = margin(
              t = 0,
              r = .4,
              b = 0,
              l = 0,
              "inch"
            ),
            legend.text = element_text(margin = margin(t = -5, b = -5, unit = "pt"))) +
      sound_theme_display +
      guides(color=guide_legend(byrow=TRUE, ncol = 2,
                                keyheight=0.3, unit = "inch")) +
      labs(title = plotTitle,
           x = "Frequency (Hz)",
           y = "Gain (dB)",
           color = "",
           linetype = "")
  }
  height = ceiling((maxY - minY) / 15) + ceiling(legendRows/2)*0.03 + 1
  return (
    list(
      height = height,
      plot = p
    )
  )
}

getProfilePlots <- function(transducerType, t, plotTitle) {
  dt <- preprocessProfiles(transducerType,t)
  if (nrow(dt) == 0) {
    return(
      list(
        plot = ggplot() + 
          theme_bw() +  
          scale_y_continuous(limits = c(-40 ,40), breaks = seq(40,40,10), expand = c(0,0)) + 
          scale_x_log10(limits = c(20, 20000), 
                        breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                        minor_breaks = breakPoints,
                        expand = c(0, 0)) + 
          sound_theme_display,
        height = 5
      )
    )
  }
  profilePlots <- plot_profiles(dt, plotTitle)
  profileAvgPlots <-  plot_profiles_avg(dt)
  return (
    list(
      height = profilePlots$height,
      plot = profilePlots$plot,
      shiftedPlot = plot_shifted_profiles(dt,plotTitle)$plot,
      avgPlot = profileAvgPlots$plot,
      avgHeight = profileAvgPlots$height,
      tb = profileAvgPlots$tb,
      title = paste0('MLS (wideband) calibration, N=',n_distinct(dt$label))
    )
  )
}

getFilteredProfilePlots <- function(transducerType, t, plotTitle, options) {
  dt <- preprocessProfiles(transducerType,t)
  if (nrow(dt) > 0) {
    dt <- dt %>% filter(label %in% options)
  }
  if (nrow(dt) == 0) {
    return(
      list(
        plot = ggplot() + 
          theme_bw() +  
          scale_y_continuous(limits = c(-40 ,40), breaks = seq(40,40,10), expand = c(0,0)) + 
          scale_x_log10(limits = c(20, 20000), 
                        breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                        minor_breaks = breakPoints,
                        expand = c(0, 0)) + 
          sound_theme_display,
        height = 5
      )
    )
  }
  profilePlots <- plot_profiles(dt, plotTitle)
  profileAvgPlots <-  plot_profiles_avg(dt)
  return (
    list(
      height = profilePlots$height,
      plot = profilePlots$plot,
      shiftedPlot = plot_shifted_profiles(dt,plotTitle)$plot,
      avgPlot = profileAvgPlots$plot,
      avgHeight = profileAvgPlots$height,
      tb = profileAvgPlots$tb,
      title = paste0('MLS (wideband) calibration, N=',n_distinct(dt$label))
    )
  )
}


get_profile_table <- function(json, transducerType) {
  df <- data.frame(
    createDate = json$createDates,
    jsonFileName = json$jsonFileName,
    OEM = json$OEMs,
    modelName = json$modelNames,
    modelNumber = unlist(json$modelNumbers),
    componentCorrectionSD = json$SDs,
    maxAbsFilteredMLS = json$FilteredMLSRange,
    isDefault = json$isDefault,
    `T` = json$`T`,
    W = json$W,
    Q = json$Q,
    gainDBSPL = json$gainDBSPL,
    speakerGain_dB = json$speakerGain_dB,
    micGain_dB = json$micGain_dB,
    # backgroundDBSPL = json$backgroundDBSPL,
    RMSError = json$RMSError,
    fs2 = json$fs2
  ) %>%
    arrange(desc(isDefault)) %>%
    mutate(
      `T` = round(`T`, 1),
      W = round(W, 1),
      Q = round(Q, 1),
      gainDBSPL = round(gainDBSPL, 1),
      speakerGain_dB = round(speakerGain_dB, 1),
      micGain_dB = round(micGain_dB, 1),
      # backgroundDBSPL = round(backgroundDBSPL, 1),
      RMSError = round(RMSError, 1),
      fs2 = fs2
    ) %>% 
    mutate(label = ifelse(isDefault, paste0("default/", modelNumber), createDate)) %>% 
    select(-isDefault)
  return(df)
}

get_profile_summary <- function(df) {
  print('inside get_profile_summary')
  displayDf <- tibble()
  t <- df %>% select(componentCorrectionSD, maxAbsFilteredMLS, `T`, W, Q, gainDBSPL, speakerGain_dB, micGain_dB, RMSError, fs2) %>% 
    summarize(across(everything(), ~ mean(.x, na.rm = TRUE)))
  displayDf <- rbind(displayDf,t)
  t <- df %>% select(componentCorrectionSD, maxAbsFilteredMLS, `T`, W, Q, gainDBSPL, speakerGain_dB, micGain_dB, RMSError, fs2) %>% 
    summarize(across(everything(), ~ sd(.x, na.rm = TRUE)))
  displayDf <- rbind(displayDf,t)
  t <- df %>% select(componentCorrectionSD, maxAbsFilteredMLS, `T`, W, Q, gainDBSPL, speakerGain_dB, micGain_dB, RMSError, fs2)
  t <- colSums(!is.na(t))
  displayDf <- rbind(displayDf,t)
  # colnames(displayDf) <- c("componentCorrectionSD", "maxAbsFilteredMLS", "T","W","Q","gainDBSPL", "speakerGain_dB", "micGain_dB", "backgroundDBSPL", "RMSError", "fs2")
  rownames(displayDf) <- c("mean", "sd", "N")
  return(displayDf)
}

