require(jsonlite)
require(dplyr)

# jsonFile <- fromJSON("BoldBrownDog864_testSoundCalibration288_0001_2023-11-09_02h00.24.551_sound.json", simplifyDataFrame = F)

preprocessJSON <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
  
  
  #### volume ####
  volume_task <- tibble(
    `1000 Hz in (dB)` = jsonFile$Cal1000HzInDb,
    `1000 Hz out (dB SPL)` = jsonFile$Cal1000HzOutDb,
    THD = jsonFile$THD * 100,
    `All Hz out (dB SPL)` = jsonFile$Cal1000HzOutDb
  )
  volume_task <- volume_task %>%
    mutate(`out - in (dB SPL)` = `1000 Hz out (dB SPL)` - `1000 Hz in (dB)`,
           THD = round(THD, 2)) %>%
    select(`1000 Hz in (dB)`,
           `1000 Hz out (dB SPL)`,
           `out - in (dB SPL)`,
           THD,
           `All Hz out (dB SPL)`) %>%
    rename(
      "in (dB)" = "1000 Hz in (dB)",
      "out (dB SPL)" = "1000 Hz out (dB SPL)",
      "THD (%)" = "THD",
      "out @all Hz (dB SPL)" = "All Hz out (dB SPL)"
    )
  
  
  
  #### sound gain parameters ####
  dynamic_range_compression_model <-
    data.frame(jsonFile$SoundGainParameters) %>%
    mutate(`1/R` = as.character(round(1 / R, 1)))
  model <-
    get_sound_model(volume_task, dynamic_range_compression_model)
  DRCMforDisplay <- dynamic_range_compression_model %>%
    select(`T`, W, `1/R`, gainDBSPL, backgroundDBSPL, RMSError) %>%
    mutate_if(is.numeric, round, digits = 1)
  names = colnames(DRCMforDisplay)
  DRCMforDisplay <- t(DRCMforDisplay)
  
  
  
  #### spectrum plot ####
  recording_vs_freq <- get_recording_vs_frequency(jsonFile)
  #### transducer table ####
  transducerTable <- get_transducer_table(jsonFile)
  
  
  
  #### convolution ####
  
  
  
  #### IR ####
  loudspeaker_component_ir <-
    data.frame(jsonFile$`Loudspeaker Component IR`)
  loudspeaker_system_ir <-
    data.frame(Gain = jsonFile$`Loudspeaker system IR`) %>%
    mutate(Freq = row_number())
  
  inputParameters <-
    tibble(
      calibrateSoundBurstDb = jsonFile$calibrateSoundBurstDb,
      calibrateSoundBurstSec = jsonFile$calibrateSoundBurstSec,
      calibrateSoundBurstRepeats = jsonFile$calibrateSoundBurstRepeats,
      calibrateSoundIIRSec = jsonFile$calibrateSoundIIRSec,
      calibrateSoundMinHz = jsonFile$calibrateSoundMinHz,
      calibrateSoundMaxHz = jsonFile$calibrateSoundMaxHz,
      calibrateSoundHz = jsonFile$calibrateSoundHz,
      filteredRangeMinSys = round(jsonFile$filteredMLSRange$system$Min, 1),
      filteredRangeMaxSys = round(jsonFile$filteredMLSRange$system$Max, 1),
      filteredRangeMinComp = round(jsonFile$filteredMLSRange$component$Min, 1),
      filteredRangeMaxComp = round(jsonFile$filteredMLSRange$component$Max, 1)
    )
  #### noise ####
  
  noise <- get_bg_recording_vs_frequency(jsonFile)
  
  #### mls ####
  mlsPSD <- get_mls_psd(jsonFile)
  
  testConv <- tibble()
  if ("microphoneGain" %in% names(jsonFile)) {
    knownGain <- tibble(
      freq = unlist(jsonFile$microphoneGain$Freq),
      gain = unlist(jsonFile$microphoneGain$Gain),
      label = "microphone gain"
    )
  } else {
    knownGain <- tibble(
      freq = unlist(jsonFile$loudspeakerGain$Freq),
      gain = unlist(jsonFile$loudspeakerGain$Gain),
      label = "loudspeaker gain"
    )
  }
  
  
  return(
    list(
      volume_task,
      #1
      model,
      #2
      DRCMforDisplay,
      #3
      names,
      #4
      dynamic_range_compression_model,
      #5
      recording_vs_freq,
      #6
      transducerTable,
      #7
      loudspeaker_component_ir,
      #8
      loudspeaker_system_ir,
      #9
      noise,
      #10
      mlsPSD,
      #11
      inputParameters,
      #12
      testConv,
      #13
      knownGain
    )
  ) #14
}

plotComponentIRTime <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1])
  t <- tibble(IR = jsonFile$`Loudspeaker Component IR Time Domain`)
  defaultF <- 48
  time <- 1 / defaultF
  t$time <- seq(0, (nrow(t) - 1) * time, time)
  p <- ggplot(t, aes(x = time, y = IR)) +
    geom_line(size = 0.8) +
    labs(title = "Loudspeaker IR", x = "Time (ms)") +
    theme_bw()
  return(p)
}

plotComponentIIR <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1])
  defaultF <- 48
  time <- 1 / defaultF
  
  if ("Loudspeaker Component IR" %in% names(jsonFile)) {
    t <- tibble(IIR = jsonFile$`Loudspeaker Component IIR`)
    t$time <- seq(0, (nrow(t) - 1) * time, time)
    p <- ggplot(t, aes(x = time, y = IIR)) +
      geom_line(size = 0.8) +
      labs(title = "Loudspeaker IIR", x = "Time (ms)") +
      theme_bw()
  } else {
    t <- tibble(IIR = jsonFile$`Microphone Component IIR`)
    t$time <- seq(0, (nrow(t) - 1) * time, time)
    p <- ggplot(t, aes(x = time, y = IIR)) +
      geom_line(size = 0.8) +
      labs(title = "Microphone IIR", x = "Time (ms)") +
      theme_bw()
  }
  
  return(p)
}

plotComponetIRPSD <-
  function(fileJSON,
           sound_data,
           subtitleOne,
           subtitleTwo,
           subtitleThree) {
    file_list <- fileJSON$data
    jsonFile <- fromJSON(file_list[1])
    
    if ("Loudspeaker Component IR" %in% names(jsonFile)) {
      t <- tibble(
        Freq = jsonFile$`Loudspeaker Component IR`$Freq,
        Gain = jsonFile$`Loudspeaker Component IR`$Gain
      )
      t <- t %>% filter(Freq >= 20,  Freq <= 16000, is.finite(Gain))
      minY =  floor(min(t$Gain) / 10) * 10 - 40
      maxY =  ceiling(max(t$Gain) / 10) * 10 + 40
      
      p <- ggplot(t, aes(x = Freq, y = Gain)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(minY,maxY),breaks = seq(minY,maxY,10)) + 
        scale_x_log10(limits = c(20, 16000), 
                      breaks = c(20, 100, 200, 1000, 2000, 10000, 16000),
                      expand = c(0, 0)) +
        theme_bw() +
        labs(title = "Loudspeaker profile") +
        add_transducerTable_component(
          transducerTable = sound_data[[7]],
          position = c("left", "bottom"),
          title_text = "",
          subtitleOne = subtitleOne,
          subtitleTwo = subtitleTwo,
          subtitleThree = subtitleThree
        )
      
    } else {
      t <- tibble(
        Freq = jsonFile$`Microphone Component IR`$Freq,
        Gain = jsonFile$`Microphone Component IR`$Gain
      )
      t <- t %>% filter(Freq >= 20,Freq <= 16000, is.finite(Gain))
      minY =  floor(min(t$Gain) / 10) * 10 - 40
      maxY =  ceiling(max(t$Gain) / 10) * 10 + 40
      p <- ggplot(t, aes(x = Freq, y = Gain)) +
        geom_line(size = 0.8) +
        scale_y_continuous(limits = c(minY,maxY),breaks = seq(minY,maxY,10)) + 
        scale_x_log10(limits = c(20, 16000) , 
                      breaks = c(20, 100, 200, 1000, 2000, 10000, 16000),
                      expand = c(0, 0)) +
        theme_bw() +
        labs(title = "Microphone profile") +
        add_transducerTable_loudspeaker(
          transducerTable = sound_data[[7]],
          position = c("left", "bottom"),
          title_text = "",
          subtitleOne = subtitleOne,
          subtitleTwo = subtitleTwo,
          subtitleThree = subtitleThree
        )
    }
    
    
    return(p)
  }

plot_power_variations <- function(fileJSON,
                                  sound_data,
                                  subtitleOne,
                                  subtitleTwo,
                                  subtitleThree) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
  transducer <- ifelse("Loudspeaker Component IR" %in% names(jsonFile), "Loudspeaker", "Microphone")
  t <- tibble(time = jsonFile$recordingChecks$unfiltered[[1]]$warmupT,
              power = jsonFile$recordingChecks$unfiltered[[1]]$warmupDb,
              label = "MLS Warmup") %>% 
    rbind(tibble(time = jsonFile$recordingChecks$unfiltered[[1]]$recT,
                 power = jsonFile$recordingChecks$unfiltered[[1]]$recDb,
                 label = paste0("MLS Data, SD = ", jsonFile$recordingChecks$unfiltered[[1]]$sd, " dB"))) %>% 
    rbind(tibble(time = jsonFile$recordingChecks$system[[1]]$recT,
                 power = jsonFile$recordingChecks$system[[1]]$recDb,
                 label = paste0("Loudspeaker+Microphone Data, SD = ", jsonFile$recordingChecks$system[[1]]$sd, " dB"))) %>%
    rbind(tibble(time = jsonFile$recordingChecks$system[[1]]$warmupT,
                 power = jsonFile$recordingChecks$system[[1]]$warmupDb,
                 label = "Loudspeaker+Microphone Warmup")) %>%
    rbind(tibble(time = jsonFile$recordingChecks$component[[1]]$recT,
                 power = jsonFile$recordingChecks$component[[1]]$recDb,
                 label = paste0(transducer, " Data, SD = ", jsonFile$recordingChecks$component[[1]]$sd, " dB"))) %>%
    rbind(tibble(time = jsonFile$recordingChecks$component[[1]]$warmupT,
                 power = jsonFile$recordingChecks$component[[1]]$warmupDb,
                 label = paste(transducer,"Warmup")))
  t$label <- factor(t$label,
                    levels = c("MLS Warmup",
                               "Loudspeaker+Microphone Warmup",
                               paste(transducer,"Warmup"),
                               paste0("MLS Data, SD = ", jsonFile$recordingChecks$unfiltered[[1]]$sd, " dB"), 
                               paste0("Loudspeaker+Microphone Data, SD = ", jsonFile$recordingChecks$system[[1]]$sd, " dB"),
                               paste0(transducer, " Data, SD = ", jsonFile$recordingChecks$component[[1]]$sd, " dB")))
  maxY <- ceiling(max(t$power/10)) * 10 + 20
  minY <- floor(min(t$power/10)) * 10 - 50
  p <- ggplot(t, aes(x = time, y = power, color = label, linetype = label)) + 
    geom_line(size = 0.8) +
    scale_x_continuous(limits = c(0,3.5),
                       breaks = seq(0,3.5,0.5),
                       expand = c(0,0)) + 
    scale_y_continuous(limits=c(minY,maxY),
                       breaks = seq(minY,maxY,10),
                       expand = c(0,0)) +
    scale_color_manual(values = c("red","#3366FF","#33FF00","red","#3366FF","#33FF00")) +
    scale_linetype_manual(values = c(2,2,2,1,1,1)) +
    theme_bw() + 
    theme(
      legend.position = c(0.50, 0.93),
      legend.box = "horizontal",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.height = unit(1, "mm"),
      panel.background = element_blank(),
      legend.background = element_rect(fill='transparent',color = 'transparent'),
      axis.line = element_line(colour = "black"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(
        t = .5,
        r = 0,
        b = .5,
        l = 0,
        "inch"
      )
    ) +
    guides(color = guide_legend(keywidth = unit(1, "cm"),
                                fill = NULL,
                                color = NULL,
                                ncol = 2),
           guide_legend(keywidth = unit(1, "cm"),
                        fill = NULL,
                        color = NULL,
                        ncol = 3)) +
    labs(color = NULL, 
         linetype = NULL,
         title = "Power Variation in Recordings",
         y = "Power (dB)",
         x = "Time (s)")
  if ("Loudspeaker Component IR" %in% names(jsonFile)) {
    p <- p + add_transducerTable_component(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = "",
      subtitleOne = subtitleOne,
      subtitleTwo = subtitleTwo,
      subtitleThree = subtitleThree
    )
  } else {
   p <- p + add_transducerTable_loudspeaker(
       transducerTable = sound_data[[7]],
       position = c("left", "bottom"),
       title_text = "",
       subtitleOne = subtitleOne,
       subtitleTwo = subtitleTwo,
       subtitleThree = subtitleThree
     )
  } 
  return(p)
}

read_iir_JSON <- function(fileJSON) {
  file_list <- fileJSON$data
  all_result <- list()
  for (i in 1:length(file_list)) {
    result <- fromJSON(file_list[i])
    component_iir <-
      data.frame(
        dB_component_iir = result$dB_component_iir,
        Hz_component_iir = result$Hz_component_iir
      ) %>%
      mutate(dB_component_iir = 10 * log10(dB_component_iir))
    component_iir_no_bandpass <-
      data.frame(
        dB_component_iir_no_bandpass = result$dB_component_iir_no_bandpass,
        Hz_component_iir_no_bandpass = result$Hz_component_iir_no_bandpass
      ) %>%
      mutate(dB_component_iir_no_bandpass = 10 * log10(dB_component_iir_no_bandpass))
    system_iir <- data.frame(
      dB_system_iir = result$dB_system_iir,
      Hz_system_iir = result$Hz_system_iir
    ) %>%
      mutate(dB_system_iir = 10 * log10(dB_system_iir))
    system_iir_no_bandpass <-
      data.frame(
        dB_system_iir_no_bandpass = result$dB_system_iir_no_bandpass,
        Hz_system_iir_no_bandpass = result$Hz_system_iir_no_bandpass
      ) %>%
      mutate(dB_system_iir_no_bandpass = 10 * log10(dB_system_iir_no_bandpass))
  }
  return(
    list(
      component_iir,
      component_iir_no_bandpass,
      system_iir,
      system_iir_no_bandpass
    )
  )
}
get_recording_vs_frequency <- function(jsonFile) {
  record_freq_system <- rbind(
    tibble(
      freq = jsonFile$MlsSpectrumHz_system,
      gain = jsonFile$MlsSpectrumFilteredDb_system,
      label = "Recording of filtered MLS"
    ),
    tibble(
      freq = jsonFile$MlsSpectrumUnfilteredHz_system,
      gain = jsonFile$MlsSpectrumUnfilteredDb_system,
      label = "Recording of MLS"
    )
  )
  record_freq_component <- rbind(
    tibble(
      freq = jsonFile$MlsSpectrumHz_component,
      gain = jsonFile$MlsSpectrumFilteredDb_component,
      label = "Recording of filtered MLS"
    ),
    tibble(
      freq = jsonFile$MlsSpectrumUnfilteredHz_component,
      gain = jsonFile$MlsSpectrumUnfilteredDb_component,
      label = "Recording of MLS"
    )
  )
  return(list(system = record_freq_system,
              component = record_freq_component))
}

get_bg_recording_vs_frequency <- function(jsonFile) {
  if ("Hz_BackgroundNoise" %in% names(jsonFile)) {
    bg_recording_vs_freq <-
      tibble(freq = jsonFile$db_BackgroundNoise,
             gain = jsonFile$Hz_BackgroundNoise)
    return(bg_recording_vs_freq)
  }
  return(tibble(freq = 0,
                gain = 0))
}

get_autocorrelation_plot <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1])
  
  t <- tibble(acf = jsonFile$autocorrelations[[1]]) %>%
    mutate(lag = row_number())
  
  q <- ggplot(data = t, mapping = aes(x = lag, y = acf)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = lag, yend = 0)) +
    scale_x_continuous(breaks = c(50000, 100000, 150000, 200000, 250000, 300000)) +
    theme_bw()
  return(q)
}

plot_record_freq_system <- function(sound_data,
                                    convolutions,
                                    subtitleOne,
                                    subtitleTwo,
                                    subtitleThree) {
  noise <- sound_data[[10]]
  noise <- noise %>%
    mutate(gain = 10 * log10(gain),
           label = "Recording of background")
  
  t <- sound_data[[6]]$system
  
  t <- t %>%
    mutate(gain = 10 * log10(gain))
  
  tmp <- t %>%
    filter(
      label == "Recording of filtered MLS",
      freq >= sound_data[[12]]$calibrateSoundMinHz,
      freq <= sound_data[[12]]$calibrateSoundMaxHz
    ) %>%
    group_by(label) %>%
    summarize(SD = round(sd(gain), 1))
  
  if (nrow(noise) > 1) {
    t <- t %>% rbind(noise)
  }
  t <- t %>%
    select(freq,
           gain,
           label) %>%
    rbind(convolutions$system) %>%
    rbind(sound_data[[11]])
  
  range <- paste(sound_data[[12]]$calibrateSoundMinHz,
                 "to",
                 sound_data[[12]]$calibrateSoundMaxHz,
                 "Hz")
  tt <- paste("SD: actual", tmp$SD, "dB")
  
  tmp <- t %>%
    filter(label == "Recording of MLS") %>%
    left_join(t %>% filter(label == "Filtered MLS") %>% select(freq, gain),
              by = c("freq")) %>%
    mutate(label = "Expected corr",
           gain = gain.x + gain.y) %>%
    select(freq, gain, label)
  
  t <- rbind(t, tmp)
  
  tmp <- t %>%
    filter(
      label == "Expected corr",
      freq >= sound_data[[12]]$calibrateSoundMinHz,
      freq <= sound_data[[12]]$calibrateSoundMaxHz
    ) %>%
    group_by(label) %>%
    summarize(SD = round(sd(gain), 1))
  
  tt <- paste(tt, "and expected", tmp$SD, "dB over", range)
  
  t$label <- factor(
    t$label,
    levels = c(
      "Recording of background",
      "Recording of MLS",
      "Recording of filtered MLS",
      "MLS",
      "Filtered MLS",
      "Expected corr"
    )
  )
  
  tmp <- t %>% filter(is.finite(gain))
  maxY <- ceiling(max(tmp$gain) / 10) * 10 + 20
  minY <- floor(min(tmp$gain) / 10) * 10
  p1 <- ggplot() +
    geom_line(data = t,
              aes(
                x = freq,
                y = gain,
                color = label,
                linetype = label
              ), size = 0.8) +
    scale_x_log10(
      breaks = c(20, 100, 200, 1000, 2000, 10000, 16000),
      limits = c(20, 16000),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(minY, maxY, 10),
      limits = c(minY, maxY),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = c("#CCCCCC", "red", "#3366FF", "red", "#3366FF", "#9900CC")) +
    scale_linetype_manual(values = c(1, 1, 1, 2, 2, 2)) +
    theme_bw() +
    theme(
      legend.position = c(0.45, 0.97),
      legend.box = "horizontal",
      legend.key.height = unit(1, "mm"),
      legend.margin = margin(
        t = 0,
        b = -.1,
        l = -2.5,
        r = -3,
        unit = "in"
      ),
      panel.background = element_blank(),
      axis.text.x = element_text(
        angle = 30,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.line = element_line(colour = "black"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(
        t = 2,
        r = .5,
        b = .5,
        l = .5,
        "inch"
      )
    ) +
    guides(color = guide_legend(keywidth = unit(1, "cm"),
                                fill = NULL,
                                ncol = 3),
           guide_legend(keywidth = unit(1, "cm"),
                        fill = NULL,
                        ncol = 3)) +
    # coord_fixed(ratio = 0.02, clip = "off") +
    labs(
      x = "Frequency (Hz)",
      y = "Power spectral density (dB)",
      color = NULL,
      linetype = NULL,
      title = "Loudspeaker + Microphone"
    ) +
    add_transducerTable_system(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = tt,
      subtitleOne = subtitleOne,
      subtitleTwo = subtitleTwo,
      subtitleThree = subtitleThree$system,
      leftShift = 0.015
    )
  
  
  return(p1)
}

plot_record_freq_component <- function(sound_data,
                                       convolutions,
                                       subtitleOne,
                                       subtitleTwo,
                                       subtitleThree) {
  noise <- sound_data[[10]]
  noise <- noise %>%
    mutate(gain = 10 * log10(gain),
           label = "Recording of background")
  t <- sound_data[[6]]$component
  
  t <- t %>%
    mutate(gain = 10 * log10(gain))
  
  if (nrow(noise) > 1) {
    t <- t %>% rbind(noise)
  }
  
  t <- t %>%
    select(freq,
           gain,
           label) %>%
    rbind(convolutions$component) %>%
    rbind(sound_data[[11]]) %>%
    rbind(sound_data[[14]])
  
  tmp <- t %>%
    filter(label == "Recording of MLS") %>%
    left_join(t %>% filter(label == "Filtered MLS") %>% select(freq, gain),
              by = c("freq")) %>%
    mutate(label = "Expected corr",
           gain = gain.x + gain.y) %>%
    select(freq, gain, label)
  
  t <- rbind(t, tmp)
  transducerLabel <- unique(sound_data[[14]]$label)
  t$label <- factor(
    t$label,
    levels = c(
      "Recording of background",
      "Recording of MLS",
      "Recording of filtered MLS",
      "MLS",
      "Filtered MLS",
      "Expected corr",
      transducerLabel
    )
  )
  
  tmp <- t %>%
    filter(label == "Recording of filtered MLS")
  interpolated_values <-
    approx(sound_data[[14]]$freq, sound_data[[14]]$gain, xout = tmp$freq)$y
  
  tmp$transducergain = interpolated_values
  tmp <- tmp %>% mutate(gain = gain - transducergain) %>%
    select(freq,
           gain,
           label)
  t <- t %>% filter(label != "Recording of filtered MLS") %>%
    rbind(tmp)
  
  tmp <- t %>%
    filter(label == "Recording of MLS")
  
  tmp$transducergain = interpolated_values
  tmp <- tmp %>% mutate(gain = gain - transducergain) %>%
    select(freq,
           gain,
           label)
  t <- t %>% filter(label != "Recording of MLS") %>%
    rbind(tmp)
  
  tmp <- t %>%
    filter(
      label == "Recording of filtered MLS",
      freq >= sound_data[[12]]$calibrateSoundMinHz,
      freq <= sound_data[[12]]$calibrateSoundMaxHz
    ) %>%
    group_by(label) %>%
    summarize(SD = round(sd(gain), 1))
  
  range <- paste(sound_data[[12]]$calibrateSoundMinHz,
                 "to",
                 sound_data[[12]]$calibrateSoundMaxHz,
                 "Hz")
  
  tt <- paste("SD: actual", tmp$SD, "dB")
  
  tmp <- t %>%
    filter(
      label == "Expected corr",
      freq >= sound_data[[12]]$calibrateSoundMinHz,
      freq <= sound_data[[12]]$calibrateSoundMaxHz
    ) %>%
    group_by(label) %>%
    summarize(SD = round(sd(gain), 1))
  
  tt <- paste(tt, "and expected", tmp$SD, "dB over", range)
  
  tmp <- t %>% filter(freq <= 16000, is.finite(gain))
  maxY <- ceiling(max(tmp$gain) / 10) * 10 + 20
  minY <- floor(min(tmp$gain) / 10) * 10
  p1 <- ggplot(t) +
    geom_line(aes(
      x = freq,
      y = gain,
      color = label,
      linetype = label
    ), size = 0.8) +
    scale_x_log10(
      breaks = c(20, 100, 200, 1000, 2000, 10000, 16000),
      limits = c(20, 16000),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      breaks = seq(minY, maxY, 10),
      limits = c(minY, maxY),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = c(
      "#CCCCCC",
      "red",
      "#3366FF",
      "red",
      "#3366FF",
      "#9900CC",
      "black"
    )) +
    scale_linetype_manual(values = c(1, 1, 1, 2, 2, 2, 2)) +
    theme_bw() +
    theme(
      legend.position = c(0.4, 0.99),
      legend.box = "horizontal",
      legend.margin = margin(
        t = 0,
        b = -.3,
        l = -2.5,
        r = -3,
        unit = "in"
      ),
      legend.key = element_rect(color = NA,fill = NA),
      legend.key.height = unit(1, "mm"),
      panel.background = element_blank(),
      axis.text.x = element_text(
        angle = 30,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.line = element_line(colour = "black")
    ) +
    guides(color = guide_legend(keywidth = unit(1, "cm"),
                                fill = NULL,
                                ncol = 3),
           linetype = guide_legend(keywidth = unit(1, "cm"),
                                   fill = NULL,
                                   ncol = 3)) +
    # coord_fixed(ratio = 0.02, clip = "off") +
    labs(
      x = "Frequency (Hz)",
      y = "Power spectral density (dB)",
      title = "Loudspeaker",
      color = NULL,
      linetype = NULL
    )
  if (transducerLabel == "microphone gain") {
    p1 <-
      p1 + add_transducerTable_component(
        transducerTable = sound_data[[7]],
        position = c("left", "bottom"),
        title_text = tt,
        subtitleOne,
        subtitleTwo,
        subtitleThree$component
      )
  } else {
    p1 <-
      p1 + add_transducerTable_loudspeaker(
        transducerTable = sound_data[[7]],
        position = c("left", "bottom"),
        title_text = tt,
        subtitleOne,
        subtitleTwo,
        subtitleThree$component
      )
  }
  return(list(p1, (maxY- minY)/20))
}



SoundLevelModel <- function(inDb, dynamic_range_compression_model) {
  R = dynamic_range_compression_model$R
  `T` = dynamic_range_compression_model$`T`
  W = dynamic_range_compression_model$W
  backgroundDBSPL = dynamic_range_compression_model$backgroundDBSPL
  gainDBSPL = dynamic_range_compression_model$gainDBSPL
  totalDbSpl = 10 * log10(10 ^ (backgroundDBSPL / 10) +
                            10 ^ ((gainDBSPL + inDb) / 10))
  measuredDbSpl <- CompressorDb(totalDbSpl, `T`, R, W)
  return(measuredDbSpl)
  
}

CompressorDb <- function(inDb, `T`, R, W) {
  outDb = 0
  WFinal = ifelse(W >= 0, W, 0)
  if (inDb > `T` + WFinal / 2) {
    outDb = `T` + (inDb - `T`) / R
  } else if (inDb > (`T` - WFinal / 2)) {
    outDb = inDb + ((1 / R - 1) * (inDb - (`T` - WFinal / 2)) ^ 2) / (2 * WFinal)
  } else {
    outDb = inDb
  }
  return (outDb)
}

get_sound_model <-
  function(volume_task,
           dynamic_range_compression_model) {
    minX = min(volume_task$`in (dB)`)
    maxX = max(volume_task$`in (dB)`)
    x = seq(minX, maxX, 0.1)
    y = sapply(x, SoundLevelModel, dynamic_range_compression_model = dynamic_range_compression_model)
    t <- tibble(x, y)
    return(t)
  }

plot_sound_level <- function(sound_data) {
  volume_task <- sound_data[[1]]
  dynamic_range_compression_model <- sound_data[[5]] %>%
    select(`T`, W, `1/R`, gainDBSPL, backgroundDBSPL, RMSError)
  model <- sound_data[[2]]
  volume_task <-
    cbind(volume_task, dynamic_range_compression_model) %>%
    mutate(RMSError = round(RMSError, 2)) %>%
    mutate(label = paste("RMSError =", RMSError))
  labels <- unique(volume_task$label)
  minY = plyr::round_any(min(volume_task$`out (dB SPL)`), 10, floor)
  maxY = plyr::round_any(max(volume_task$`out (dB SPL)`), 10, ceiling)
  minX = plyr::round_any(min(volume_task$`in (dB)`), 10, floor)
  maxX = plyr::round_any(max(volume_task$`in (dB)`), 10, ceiling)
  p <-
    ggplot(data = volume_task, aes(x = `in (dB)`, y = `out (dB SPL)`)) +
    geom_point(size = 3) +
    geom_line(data = model, aes(x = x, y = y), size = 0.8) +
    ggpp::geom_text_npc(aes(
      npcx = "left",
      npcy = "top",
      label = paste(labels, collapse = "\n")
    )) +
    scale_y_continuous(breaks = seq(minY, maxY, 10)) +
    scale_x_continuous(
      breaks = unique(volume_task$`in (dB)`),
      limits = c(minX, maxX),
      expand = c(0, 0)
    ) +
    coord_fixed(ratio = 1, clip = "off") +
    guides(color = guide_legend(
      order = 1,
      nrow = 2,
      title = ""
    )) +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(
        t = 0,
        b = -.3,
        l = 0,
        r = 0,
        unit = "cm"
      ),
      legend.key = element_rect(fill = NA, color = NA),
      plot.title = element_text(size = 12, hjust = 0.5)
    ) +
    ggtitle("Sound Level at 1000 Hz") +
    ylab("out (dB)")
  return(p)
}

get_json_plots <- function(result, selected) {
  if (is.null(selected)) {
    return(list(ggplot(),
                ggplot(),
                ggplot()))
  }
  
  t <- result %>% filter(name == selected)
  
  defaultF <- 48
  time <- 1 / defaultF
  t$time <- seq(0, (nrow(t) - 1) * time, time)
  
  IR_0to6 <- t %>% filter(time >= 0,
                          time <= 6)
  IR_0to50 <- t %>% filter(time >= 0,
                           time <= 50)
  IR_0to400 <- t %>%
    filter(time >= 0, time <= 400) %>%
    mutate(db = cumsum((IR) ^ 2))
  
  p1 <- ggplot(IR_0to6, aes(x = time, y = IR)) +
    geom_line(size = 0.8) +
    xlab("Time (ms)") +
    ylab("v/v.s") +
    theme_bw() +
    ggtitle("0 to 6 ms")
  
  p2 <- ggplot(IR_0to50, aes(x = time, y = IR)) +
    geom_line(size = 0.8) +
    xlab("Time (ms)") +
    ylab("v/v.s") +
    theme_bw() +
    ggtitle("0 to 50 ms")
  
  p3 <- ggplot(IR_0to400, aes(x = time, y = db)) +
    geom_line(size = 0.8) +
    xlab("Time (ms)") +
    ylab("dB") +
    theme_bw() +
    ggtitle("schroeder plot, 0 to 400 ms")
  return(list(p1, p2, p3))
}

get_transducer_table <- function(jsonFile) {
  loudspeaker <-
    tibble(
      fullLoudspeakerModelName = jsonFile$`Loudspeaker model`$fullLoudspeakerModelName,
      fullLoudspeakerModelNumber = jsonFile$`Loudspeaker model`$fullLoudspeakerModelNumber,
      gainDBSPL = ifelse(
        is.null(jsonFile$`Loudspeaker model`$gainDBSPL),
        "",
        jsonFile$`Loudspeaker model`$gainDBSPL
      ),
      OEM = ifelse(
        is.null(jsonFile$`Loudspeaker model`$OEM),
        "",
        jsonFile$`Loudspeaker model`$OEM
      ),
      CalibrationDate = ifelse(
        is.null(jsonFile$`Loudspeaker model`$CalibrationDate),
        "",
        jsonFile$`Loudspeaker model`$CalibrationDate
      )
    )
  micInfo <- tibble(
    micModelName = jsonFile$`micInfo`$micModelName,
    OEM = jsonFile$`micInfo`$OEM,
    ID = jsonFile$`micInfo`$ID,
    gainDBSPL = ifelse(
      is.numeric(jsonFile$`micInfo`$gainDBSPL),
      jsonFile$`micInfo`$gainDBSPL,
      NA
    )
  )
  micGainDBSPL <-
    paste(round(micInfo$gainDBSPL, 1), "dB SPL at 1kHz                                             ")
  loudspeakerAudioDevice <- jsonFile$webAudioDeviceNames$loudspeaker
  micAudioDevice <- jsonFile$webAudioDeviceNames$microphone
  if (nchar(loudspeakerAudioDevice) > 0 && nchar(micAudioDevice) > 0) {
    if (nchar(loudspeakerAudioDevice) > 0) {
      indexLoudspeakers <- unlist(gregexpr(' ', loudspeakerAudioDevice))
      if (length(indexLoudspeakers) > 1) {
        indexLoudspeaker <- indexLoudspeakers[length(indexLoudspeakers) - 1]
        loudspeakerAudioDeviceOne <- substr(loudspeakerAudioDevice, 1, indexLoudspeaker)
        loudspeakerAudioDeviceTwo <- substr(loudspeakerAudioDevice, indexLoudspeaker+1, nchar(loudspeakerAudioDevice))
      } else {
        micAudioDeviceOne = micAudioDevice
        micAudioDeviceTwo = ""
      }
    } else {
      loudspeakerAudioDeviceOne = loudspeakerAudioDevice
      loudspeakerAudioDeviceTwo = ""
    }
    
    if (nchar(micAudioDevice) > 0) {
      indexMics <- unlist(gregexpr(' ', micAudioDevice))
      if (length(indexMics) > 1) {
        indexMic <- indexMics[length(indexMics)-1]
        micAudioDeviceOne <- substr(micAudioDevice, 1, indexMic)
        micAudioDeviceTwo <- substr(micAudioDevice, indexMic+1, nchar(micAudioDevice))
      } else {
        micAudioDeviceOne = micAudioDevice
        micAudioDeviceTwo = ""
      }
    } else {
      micAudioDeviceOne = micAudioDevice
      micAudioDeviceTwo = ""
    }
    transducerTable <- data.frame(rbind(
      c(paste(loudspeakerAudioDeviceOne, " "), micAudioDeviceOne),
      c(loudspeakerAudioDeviceTwo, micAudioDeviceTwo),
      c(paste(jsonFile$sampleRate$loudspeaker, "Hz"),
        paste(jsonFile$sampleRate$microphone, "Hz,", jsonFile$sampleSize, "bits")),
      c(loudspeaker$gainDBSPL, micGainDBSPL),
      c(loudspeaker$OEM, micInfo$OEM),
      c(
        paste(loudspeaker$fullLoudspeakerModelName, "  "),
        micInfo$micModelName
      ),
      c(
        paste(loudspeaker$fullLoudspeakerModelNumber, "  "),
        micInfo$ID
      ),
      c(loudspeaker$CalibrationDate, NA)
    ))
  } else {
    transducerTable <- data.frame(rbind(
      c(paste(jsonFile$sampleRate$loudspeaker, "Hz"),
        paste(jsonFile$sampleRate$microphone, "Hz,", jsonFile$sampleSize, "bits")),
      c(loudspeaker$gainDBSPL, micGainDBSPL),
      c(loudspeaker$OEM, micInfo$OEM),
      c(
        paste(loudspeaker$fullLoudspeakerModelName, "  "),
        micInfo$micModelName
      ),
      c(
        paste(loudspeaker$fullLoudspeakerModelNumber, "  "),
        micInfo$ID
      ),
      c(loudspeaker$CalibrationDate, NA)
    ))
  }
  
  colnames(transducerTable) <- c("Loudspeaker    ", "Microphone")
  return(transducerTable)
}

get_iir_plot <- function(iir) {
  component_iir <- iir[[1]]
  component_iir_no_bandpass <- iir[[2]]
  system_iir <- iir[[3]]
  system_iir_no_bandpass <- iir[[4]]
  # Plot Power Spectral Density of component iir
  t <- component_iir %>%
    filter(Hz_component_iir > 500) %>%
    filter(Hz_component_iir < 2000)
  
  p1 <- ggplot() +
    geom_line(
      data = component_iir,
      aes(x = Hz_component_iir,
          y = dB_component_iir,
          color = "Loudspeaker IIR"),
      size = .8
    ) +
    geom_line(
      data = component_iir_no_bandpass,
      aes(x = Hz_component_iir_no_bandpass,
          y = dB_component_iir_no_bandpass,
          color = "Loudspeaker IIR (No Bandpass)"),
      size = .8
    ) +
    scale_x_log10(limits = c(20, 20000), n.breaks = 15) +
    scale_y_continuous(limits = c(
      median(t$dB_component_iir) - 60,
      max(component_iir_no_bandpass$dB_component_iir) +
        5
    )) +
    ylab("dB") +
    xlab("Frequency (Hz)") +
    ggtitle("Power Spectral Density of Loudspeaker IIR") +
    scale_color_manual(values = c("#3366FF", "red")) +
    labs(color = "") +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(
        t = 0,
        b = -.3,
        l = 0,
        r = 0,
        unit = "cm"
      ),
      axis.line = element_line(colour = "black")
    )
  
  t <- system_iir %>%
    filter(Hz_system_iir > 500, Hz_system_iir < 2000)
  # Plot Power Spectral Density of system IIR
  p2 <- ggplot() +
    geom_line(
      data = system_iir,
      aes(x = Hz_system_iir,
          y = dB_system_iir,
          color = "System IIR"),
      size = .8
    ) +
    geom_line(
      data = system_iir_no_bandpass,
      aes(x = Hz_system_iir_no_bandpass,
          y = dB_system_iir_no_bandpass,
          color = "System IIR (No Bandpass)"),
      size = .8
    ) +
    scale_x_log10(limits = c(20, 20000), n.breaks = 15) +
    scale_y_continuous(limits = c(
      median(t$dB_system_iir) - 60,
      max(system_iir_no_bandpass$dB_system_iir_no_bandpass) + 5
    )) +
    ylab("dB") +
    xlab("Frequency (Hz)") +
    ggtitle("Power Spectral Density of System IIR") +
    scale_color_manual(values = c("#3366FF", "red")) +
    labs(color = "") +
    theme_bw() +
    theme(
      legend.position = "top",
      legend.box = "horizontal",
      legend.margin = margin(
        t = 0,
        b = -.3,
        l = 0,
        r = 0,
        unit = "cm"
      ),
      axis.line = element_line(colour = "black")
    )
  return(list(p1, p2))
}

get_convolutions <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1])
  systemConv <- tibble(
    freq = jsonFile$Hz_system_convolution,
    gain = 10 * log10(jsonFile$db_system_convolution),
    label = "Filtered MLS"
  )
  if (is.null(jsonFile$db_component_convolution)) {
    componentConv <- tibble(freq = c(),
                            gain = c(),
                            label = c())
  } else {
    componentConv <- tibble(
      freq = jsonFile$Hz_component_convolution,
      gain = 10 * log10(jsonFile$db_component_convolution),
      label = "Filtered MLS"
    )
  }
  
  
  return(list(system = systemConv,
              component = componentConv))
}

get_convolution_plot <- function(sound_data) {
  convolutions <- sound_data[[11]]
  systemConv <- convolutions$system
  componentConv <- convolutions$component
  p1 <- ggplot(systemConv, aes(x = freq, y = gain)) +
    geom_line(size = 0.8) +
    scale_x_log10(breaks = c(20, 100, 200, 1000, 2000, 10000, 16000)) +
    coord_fixed(0.05) +
    theme_bw() +
    labs(y = "Gain (dB)",
         x = "Frequency (Hz)",
         title = "Power Spetral Density of system convolution (filtered mls)")
  p2 <- ggplot(componentConv, aes(x = freq, y = gain)) +
    geom_line(size = 0.8) +
    scale_x_log10(breaks = c(20, 100, 200, 1000, 2000, 10000, 16000)) +
    coord_fixed(0.05) +
    theme_bw() +
    labs(y = "Gain (dB)",
         x = "Frequency (Hz)",
         title = "Power Spetral Density of component convolution (filtered mls)")
  return(list(system = p1,
              component = p2))
}

get_mls_psd <- function(jsonFile) {
  mlsPSD <- tibble(
    freq = jsonFile$Hz_mls,
    gain = 10 * log10(jsonFile$db_mls),
    label = "MLS"
  )
  return(mlsPSD)
}

add_transducerTable_system <- function(transducerTable,
                                       position,
                                       title_text = "",
                                       subtitleOne = "",
                                       subtitleTwo = "",
                                       subtitleThree = "",
                                       leftShift = 0.015) {
  geom_table_costumized(
    data = transducerTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(transducerTable %>% head(nrow(transducerTable) - 1)),
      text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
    ),
    title_text = title_text,
    subtitleTOne = subtitleOne,
    subtitleTTwo = subtitleTwo,
    subtitleTThree = subtitleThree,
    leftShift = leftShift,
    table.theme = ttheme_default(
      base_size = 12,
      padding = unit(c(1, 2), "mm"),
      colhead = list(
        fg_params = list(hjust = 0,
                         x = 0.1),
        bg_params = list(fill = NULL,
                         alpha = 0)
      ),
      core = list(
        fg_params = list(
          hjust = 0,
          x = 0.1,
          fontface = 1
        ),
        bg_params = list(fill = NULL,
                         alpha = 0)
      )
    )
  )
}
## when component is loudspeaker
add_transducerTable_component <- function(transducerTable,
                                          position,
                                          title_text = "",
                                          subtitleOne = "",
                                          subtitleTwo = "",
                                          subtitleThree = "",
                                          leftShift = 0.015) {
  geom_table_costumized(
    data = transducerTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(transducerTable %>% head(nrow(transducerTable) - 1)),
      text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
    ),
    title_text = title_text,
    subtitleTOne = subtitleOne,
    subtitleTTwo = subtitleTwo,
    subtitleTThree = subtitleThree,
    leftShift = leftShift,
    table.theme = ttheme_default(
      base_size = 12,
      padding = unit(c(1, 2), "mm"),
      colhead = list(
        fg_params = list(
          hjust = 0,
          x = 0.1,
          fontface = matrix(
            c(2, 1),
            ncol = 2,
            nrow = 1,
            byrow = TRUE
          )
        ),
        bg_params = list(fill = NULL,
                         alpha = 0)
      ),
      core = list(
        fg_params = list(
          hjust = 0,
          x = 0.1,
          fontface = 1
        ),
        bg_params = list(fill = NULL,
                         alpha = 0)
      )
    )
  )
}

add_transducerTable_loudspeaker <- function(transducerTable,
                                            position,
                                            title_text = "",
                                            subtitleOne = "",
                                            subtitleTwo = "",
                                            subtitleThree = "",
                                            leftShift = 0.015) {
  geom_table_costumized(
    data = transducerTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(transducerTable %>% head(nrow(transducerTable) - 1)),
      text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
    ),
    title_text = title_text,
    subtitleTOne = subtitleOne,
    subtitleTTwo = subtitleTwo,
    subtitleTThree = subtitleThree,
    leftShift = leftShift,
    table.theme = ttheme_default(
      base_size = 12,
      padding = unit(c(1, 2), "mm"),
      colhead = list(
        fg_params = list(
          hjust = 0,
          x = 0.1,
          fontface = matrix(
            c(1, 2),
            ncol = 2,
            nrow = 1,
            byrow = TRUE
          )
        ),
        bg_params = list(fill = NULL,
                         alpha = 0)
      ),
      core = list(
        fg_params = list(
          hjust = 0,
          x = 0.1,
          fontface = 1
        ),
        bg_params = list(fill = NULL,
                         alpha = 0)
      )
    )
  )
}

sound_theme_download <- theme(
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 12),
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 12)
)


sound_theme_display <- theme(
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 12),
  plot.title = element_text(size = 16),
  plot.subtitle = element_text(size = 12),
  plot.margin = margin(1.5, .5, .5, .5, "inch")
)
