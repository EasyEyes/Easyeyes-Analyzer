preprocessProfiles <- function(transducerType, t) {
  dt <- tibble()
  if (transducerType == "Microphones" && ("linear" %in% names(t))) {
    for (i in 1:length(t$linear$Freq)) {
      tmp <- tibble(
        freq = t$linear$Freq[[i]],
        gain = t$linear$Gain[[i]],
        label = ifelse(t$isDefault[i],
                       paste0("default/", t$ID[i]),
                       t$DateText[i])
      ) %>%
        filter(!is.na(label))
      dt <- dt %>% rbind(tmp)
    }
  } else if ("ir" %in% names(t)) {
    for (i in 1:length(t$ir$Freq)) {
      tmp <- tibble(
        freq = t$ir$Freq[[i]],
        gain = t$ir$Gain[[i]],
        label = t$CalibrationDate[i]
      ) %>%
        filter(!is.na(label))
      
      if (ncol(tmp) == 3) {
        dt <- dt %>% rbind(tmp)
      }
    }
  }
  return(dt)
}

plot_profiles <- function(dt, plotTitle) {
  dt <- dt %>% filter(is.finite(gain), freq >= 20, freq <= 20000)
  tmp <- dt %>% group_by(label) %>% summarize(gain1000 = approx(x = freq, y = gain, xout = 1000)$y)
  t <- tmp %>% ungroup() %>% summarize(sd = format(round(sd(gain1000) ,1), nsmall = 1))
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
  p <- ggplot() +
    geom_line(data = dt, aes(x = freq, y = gain, color = label, linetype = label), linewidth = 0.6) +                  
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
            l = .4,
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
  height = ceiling((maxY - minY) / 14) + ceiling(legendRows/2)*0.015 + 1
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
  t <- tmp %>% ungroup() %>% summarize(avg = mean(gain1000))
  tmp$diff <- tmp$gain1000 - t$avg
  tmp <- tmp %>% select(label, diff)
  shifted_dt <- dt %>% left_join(tmp, by = "label") %>% mutate(gain = gain - diff)
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
  p <- ggplot() +
    geom_line(data = shifted_dt, aes(x = freq, y = gain, color = label, linetype = label), linewidth = 0.6) +                  
    scale_y_continuous(limits = c(minY ,maxY), breaks = seq(minY,maxY,10), expand = c(0,0)) + 
    scale_x_log10(limits = c(20, 20000), 
                  breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
                  expand = c(0, 0)) +
    scale_color_manual(values = colors) + 
    scale_linetype_manual(values = linetypes) +
    theme_bw() +
    theme(legend.position="top",
          plot.margin = margin(
            t = 0,
            r = .4,
            b = 0,
            l = .4,
            "inch"
          ),
          legend.text = element_text(margin = margin(t = -5, b = -5, unit = "pt"))) +
    sound_theme_display +
    guides(color=guide_legend(byrow=TRUE, ncol = 2,
                              keyheight=0.3, unit = "inch")) +
    labs(title = paste(plotTitle, ", shifted"),
         x = "Frequency (Hz)",
         y = "Gain (dB)",
         color = "",
         linetype = "")
  height = ceiling((maxY - minY) / 14) + ceiling(legendRows/2)*0.015 + 1
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
                        expand = c(0, 0)) + 
          sound_theme_display,
        height = 5
      )
    )
  }
  profilePlots <- plot_profiles(dt, plotTitle)
  return (
    list(
      height = profilePlots$height,
      plot = profilePlots$plot,
      shiftedPlot = plot_shifted_profiles(dt,plotTitle)$plot
    )
  )
}

getFilteredProfilePlots <- function(transducerType, t, plotTitle, options) {
  dt <- preprocessProfiles(transducerType,t)
  if (nrow(dt) > 0) {
    dt <- dt %>% filter(label %in% options)
  }
  profilePlots <- plot_profiles(dt, plotTitle)
  return (
    list(
      height = profilePlots$height,
      plot = profilePlots$plot,
      shiftedPlot = plot_shifted_profiles(dt,plotTitle)$plot
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
    backgroundDBSPL = json$backgroundDBSPL,
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
      backgroundDBSPL = round(backgroundDBSPL, 1),
      RMSError = round(RMSError, 1),
      fs2 = fs2
    ) %>% 
    mutate(label = ifelse(isDefault, paste0("default/", modelNumber), createDate)) %>% 
    select(-isDefault)
  return(df)
}

get_profile_summary <- function(df) {
  t <- df %>% select(componentCorrectionSD, maxAbsFilteredMLS, `T`, W,Q,gainDBSPL,speakerGain_dB, micGain_dB, backgroundDBSPL, RMSError, fs2) %>% 
    get_summary_stats() %>% 
    select(mean,sd)
  displayDf <- t(t)
  
  colnames(displayDf) <- c("componentCorrectionSD", "maxAbsFilteredMLS", "T", "W","Q","gainDBSPL", "speakerGain_dB", "micGain_dB", "backgroundDBSPL", "RMSError", "fs2")
  rownames(displayDf) <- colnames(t)
  return(displayDf)
}

