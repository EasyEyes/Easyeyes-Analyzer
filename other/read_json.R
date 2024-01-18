require(jsonlite)
require(dplyr)

# jsonFile <- fromJSON("HappyBronzeLemon961_SoundCalibrationScientist86_0001_January 9, 2024 9_47 PM GMT-05_00_sound.json", simplifyVector = T)

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
      calibrateSoundIRSec = jsonFile$calibrateSoundIRSec,
      calibrateSoundIIRSec = jsonFile$calibrateSoundIIRSec,
      calibrateSoundMinHz = jsonFile$calibrateSoundMinHz,
      calibrateSoundMaxHz = jsonFile$calibrateSoundMaxHz,
      calibrateSoundHz = jsonFile$calibrateSoundHz,
      filteredRangeMinSys = round(jsonFile$filteredMLSRange$system$Min, 1),
      filteredRangeMaxSys = round(jsonFile$filteredMLSRange$system$Max, 1),
      filteredRangeMinComp = round(jsonFile$filteredMLSRange$component$Min, 1),
      filteredRangeMaxComp = round(jsonFile$filteredMLSRange$component$Max, 1),
      systemCorrectionSD = jsonFile$systemCorrectionSD,
      componentCorrectionSD = jsonFile$componentCorrectionSD,
      mlsSD = jsonFile$mlsSD,
      calibrateSoundAttenuationSpeakerAndMicDb = jsonFile$calibrateSoundAttenuationSpeakerAndMicDb,
      calibrateSoundAttenuationSpeakerAndMicGain = jsonFile$calibrateSoundAttenuationSpeakerAndMicGain,
      filteredMLSMaxAbsSystem = jsonFile$filteredMLSMaxAbsSystem,
      filteredMLSMaxAbsComponent = jsonFile$filteredMLSMaxAbsComponent,
      fMaxHzComponent = round(jsonFile$fMaxHz$component / 100) * 100,
      fMaxHzSystem = round(jsonFile$fMaxHz$system / 100) * 100,
      attenuatorDBSystem = jsonFile$attenuatorGainDB$system,
      attenuatorDBComponent = jsonFile$attenuatorGainDB$component,
      transducerTypeF = ifelse(
        "calibrateSoundAttenuationLoudspeakerGain" %in% names(jsonFile),
        "Loudspeaker",
        "Microphone"
      ),
      fs2 = jsonFile$fs2
    )
  if ("calibrateSoundAttenuationLoudspeakerGain" %in% names(jsonFile)) {
    inputParameters <- inputParameters %>% mutate(
      transducerType = "Speak",
      calibrateSoundAttenuationComponentGain = jsonFile$calibrateSoundAttenuationLoudspeakerGain,
      calibrateSoundAttenuationComponentDb = jsonFile$calibrateSoundAttenuationLoudspeakerDb
    )
  } else {
    inputParameters <- inputParameters %>% mutate(
      transducerType = "Mic",
      calibrateSoundAttenuationComponentGain = jsonFile$calibrateSoundAttenuationMicrophoneGain,
      calibrateSoundAttenuationComponentDb = jsonFile$calibrateSoundAttenuationMicrophoneDb
    )
  }
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
  
  subtitle <- get_subtitle(inputParameters)
  componentIIRPlots <-
    plotComponentIIR(jsonFile, subtitle$system, transducerTable)
  
  sound_data <- list(
    volume_task = volume_task,
    #1
    model = model,
    #2
    DRCMforDisplay = DRCMforDisplay,
    #3
    names = names,
    #4
    dynamic_range_compression_model = dynamic_range_compression_model,
    #5
    recording_vs_freq = recording_vs_freq,
    #6
    transducerTable = transducerTable,
    #7
    loudspeaker_component_ir = loudspeaker_component_ir,
    #8
    loudspeaker_system_ir = loudspeaker_system_ir,
    #9
    noise = noise,
    #10
    mlsPSD = mlsPSD,
    #11
    inputParameters = inputParameters,
    #12
    testConv = testConv,
    #13
    knownGain = knownGain,
    #14
    subtitle = subtitle,
    #15
    componentIIRPlots = componentIIRPlots
    #16
  )
  return(sound_data)
}

get_subtitle <- function(inputParameters) {
  subtitleOne <- paste0(
    "MLS: ",
    (round(inputParameters$calibrateSoundBurstDb, 3)),
    " dB, ampl. ",
    round(10 ^ (
      inputParameters$calibrateSoundBurstDb / 20
    ), 3),
    ", ",
    inputParameters$calibrateSoundBurstSec,
    " s, ",
    inputParameters$calibrateSoundBurstRepeats,
    "×",
    ", ",
    inputParameters$calibrateSoundHz,
    " Hz (",
    inputParameters$fs2,
    " Hz)"
  )
  
  subtitleTwo <- list(
    system =  paste0(
      "Filtered MLS: ",
      format(round(
        inputParameters$calibrateSoundAttenuationSpeakerAndMicDb,
        1
      ), nsmall = 1
      ),
      " dB, ampl. ",
      format(round(inputParameters$filteredMLSMaxAbsSystem, 1), nsmall = 1),
      ", ",
      inputParameters$calibrateSoundMinHz,
      " – ",
      inputParameters$fMaxHzSystem,
      " Hz, ",
      inputParameters$attenuatorDBSystem,
      " dB atten."
    ),
    component = paste0(
      "Filtered MLS: ",
      format(
        round(
          inputParameters$calibrateSoundAttenuationComponentDb,
          1
        ),
        nsmall = 1
      ),
      " dB, ampl. ",
      format(round(inputParameters$filteredMLSMaxAbsComponent, 1), nsmall = 1),
      ", ",
      inputParameters$calibrateSoundMinHz,
      " – ",
      inputParameters$fMaxHzComponent,
      " Hz, ",
      inputParameters$attenuatorDBComponent,
      " dB atten."
    )
  )
  
  subtitleThree <- list(
    system = paste0(
      "IR: ",
      inputParameters$calibrateSoundIRSec,
      " s, IIR: ",
      inputParameters$calibrateSoundIIRSec,
      " s, ",
      inputParameters$calibrateSoundMinHz,
      " – ",
      inputParameters$fMaxHzSystem,
      " Hz"
    ),
    component = paste0(
      "IR: ",
      inputParameters$calibrateSoundIRSec,
      " s, IIR: ",
      inputParameters$calibrateSoundIIRSec,
      " s, ",
      inputParameters$calibrateSoundMinHz,
      "–",
      inputParameters$fMaxHzComponent,
      " Hz"
    )
  )
  
  
  
  
  return(list(system = list(
    c(subtitleOne,
      subtitleTwo$system,
      subtitleThree$system)
  ),
  component = list(
    c(
      subtitleOne,
      subtitleTwo$component,
      subtitleThree$component
    )
  )))
}


plotComponentIIR <- function(jsonFile, subtitle, transducerTable) {
  defaultF <- jsonFile$sampleRate$loudspeaker / 1000
  time <- 1 / defaultF
  if ("Loudspeaker Component IR" %in% names(jsonFile)) {
    t <- tibble(IIR = jsonFile$`Loudspeaker Component IIR`)
  } else {
    t <- tibble(IIR = jsonFile$`Microphone Component IIR`)
  }
  
  t$time <- seq(0, (nrow(t) - 1) * time, time)
  
  peak <- t$time[t$IIR == max(t$IIR)]
  IIR_0to6 <- t %>% filter(time >= peak - 3,
                            time <= peak + 3)
  IIR_0to30 <- t %>% filter(time >= peak - 25,
                            time <= peak + 25)
  
  
  IIR_0to400 <- t %>%
    arrange(desc(time)) %>% 
    mutate(db = cumsum(IIR ^ 2)) %>% 
    mutate(db = db/max(db)) %>% 
    mutate(db = 10 * log10(db))
  
  minX <- ifelse(peak > 50, peak - 50, 0)
  maxX <- ifelse(max(t$time) < peak + 50, max(t$time), peak + 50)
  
  ten <- ggplot(IIR_0to6, aes(x = time, y = IIR)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0.1, 0.1),
                       limits = c(-max(IIR_0to6$IIR),max(IIR_0to6$IIR))
                       ) +
    labs(
      title = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "6 ms of Loudspeaker Inverse Impulse Response",
        "6 ms of Microphone Inverse Impulse Response"
      ),
      x = "Time (ms)",
      y = "Amplitude"
    ) +
    theme_bw() +
    coord_cartesian(clip = 'off') +
    margin_theme +
    add_transducerTable_component(
      transducerTable = transducerTable,
      position = c("left", "bottom"),
      subtitle = subtitle,
      leftShift = 0.015,
      transducerType = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "Loudspeaker",
        "Microphone"
      )
    )
  
  fifty <- ggplot(IIR_0to30, aes(x = time, y = IIR)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0.1, 0.1),
                       limits = c(-max(IIR_0to6$IIR)/10, max(IIR_0to6$IIR)/10),
                       oob = function(x, ...) x) +
    coord_cartesian(clip = 'on') +
    labs(
      title = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "50 ms of Loudspeaker Inverse Impulse Response",
        "50 ms of Microphone Inverse Impulse Response"
      ),
      x = "Time (ms)",
      y = "Amplitude"
    ) +
    theme_bw() +
    margin_theme +
    add_transducerTable_component(
      transducerTable = transducerTable,
      position = c("left", "bottom"),
      subtitle = subtitle,
      leftShift = 0.015,
      transducerType = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "Loudspeaker",
        "Microphone"
      )
    )
  
  maxY <- ceiling(max(IIR_0to400$db)/10) * 10
  schroeder <- ggplot(IIR_0to400, aes(x = time, y = db)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(minX, maxX)) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(maxY - 70, maxY),
      breaks = seq(maxY - 70, maxY, 10)
    ) +
    labs(
      title = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "Schroeder plot of loudspeaker inverse impulse response",
        "Schroeder plot of microphone inverse impulse response"
      ),
      x = "Time (ms)",
      y = "Cumulative power"
    ) +
    theme_bw() +
    margin_theme +
    add_transducerTable_component(
      transducerTable = transducerTable,
      position = c("left", "bottom"),
      subtitle = subtitle,
      leftShift = 0.015,
      transducerType = ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "Loudspeaker",
        "Microphone"
      )
    )
  return(list(
    ten = ten,
    fifty = fifty,
    schroeder = schroeder
  ))
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
      t <- t %>% filter(Freq >= 20,  Freq <= 20000, is.finite(Gain))
      minY =  floor((min(t$Gain) - 30) / 10) * 10
      maxY =  ceiling(max(t$Gain) / 10) * 10
      
      p <- ggplot(t, aes(x = Freq, y = Gain)) +
        geom_line(size = 0.8) +
        scale_y_continuous(
          limits = c(minY, maxY),
          breaks = seq(minY, maxY, 10),
          expand = c(0, 0)
        ) +
        scale_x_log10(
          limits = c(20, 20000),
          breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
          expand = c(0, 0)
        ) +
        theme_bw() +
        margin_theme +
        labs(title = "Loudspeaker Profile",
             x = "Frequency (Hz)",
             y = "Gain (dB)") +
        add_transducerTable_component(
          transducerTable = sound_data[[7]],
          position = c("left", "bottom"),
          title_text = "",
          subtitle = list(c(
            subtitleOne, subtitleTwo, subtitleThree
          ))
        )
      
    } else {
      t <- tibble(
        Freq = jsonFile$`Microphone Component IR`$Freq,
        Gain = jsonFile$`Microphone Component IR`$Gain
      )
      t <- t %>% filter(Freq >= 20, Freq <= 20000, is.finite(Gain))
      minY =  floor((min(t$Gain) - 30) / 10) * 10
      maxY =  ceiling(max(t$Gain) / 10) * 10
      p <- ggplot(t, aes(x = Freq, y = Gain)) +
        geom_line(size = 0.8) +
        scale_y_continuous(
          limits = c(minY, maxY),
          breaks = seq(minY, maxY, 10),
          expand = c(0, 0)
        ) +
        scale_x_log10(
          limits = c(20, 20000) ,
          breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
          expand = c(0, 0)
        ) +
        theme_bw() +
        margin_theme +
        labs(title = "Microphone Profile",
             x = "Frequency (Hz)",
             y = "Gain (dB)") +
        add_transducerTable_loudspeaker(
          transducerTable = sound_data[[7]],
          position = c("left", "bottom"),
          title_text = "",
          subtitle = list(c(
            subtitleOne, subtitleTwo, subtitleThree
          ))
        )
    }
    
    height = (maxY - minY) / 14 + 1
    return(list(plot = p, height = height))
  }

plot_power_variations <- function(fileJSON,
                                  sound_data,
                                  subtitleOne,
                                  subtitleTwo,
                                  subtitleThree) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
  transducer <-
    ifelse("Loudspeaker Component IR" %in% names(jsonFile),
           "Loudspeaker",
           "Microphone")
  
  pre <- tibble()
  rec <- tibble()
  post <- tibble()
  
  pre <- pre %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$unfiltered[[1]]$warmupT,
        power = jsonFile$recordingChecks$unfiltered[[1]]$warmupDb,
        label = paste0("MLS, SD = ",
                       format(jsonFile$recordingChecks$unfiltered[[1]]$sd, nsmall = 1),
                       " dB")
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$system[[1]]$warmupT,
        power = jsonFile$recordingChecks$system[[1]]$warmupDb,
        label = paste0(
          "Loudspeaker+Microphone corrected MLS, SD = ",
          format(jsonFile$recordingChecks$system[[1]]$sd, nsmall = 1),
          " dB"
        )
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$component[[1]]$warmupT,
        power = jsonFile$recordingChecks$component[[1]]$warmupDb,
        label = paste0(
          transducer,
          "-corrected MLS, SD = ",
          format(jsonFile$recordingChecks$component[[1]]$sd, nsmall = 1),
          " dB"
        )
      )
    )
  
  rec <- rec %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$unfiltered[[1]]$recT,
        power = jsonFile$recordingChecks$unfiltered[[1]]$recDb,
        label = paste0("MLS, SD = ",
                       format(jsonFile$recordingChecks$unfiltered[[1]]$sd, nsmall = 1),
                       " dB")
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$system[[1]]$recT,
        power = jsonFile$recordingChecks$system[[1]]$recDb,
        label = paste0(
          "Loudspeaker+Microphone corrected MLS, SD = ",
          format(jsonFile$recordingChecks$system[[1]]$sd, nsmall = 1),
          " dB"
        )
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$component[[1]]$recT,
        power = jsonFile$recordingChecks$component[[1]]$recDb,
        label = paste0(
          transducer,
          "-corrected MLS, SD = ",
          format(jsonFile$recordingChecks$component[[1]]$sd, nsmall = 1),
          " dB"
        )
      )
    )
  
  post <- post %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$unfiltered[[1]]$postT,
        power = jsonFile$recordingChecks$unfiltered[[1]]$postDb,
        label = paste0("MLS, SD = ",
                       format(jsonFile$recordingChecks$unfiltered[[1]]$sd, nsmall = 1),
                       " dB")
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$system[[1]]$postT,
        power = jsonFile$recordingChecks$system[[1]]$postDb,
        label = paste0(
          "Loudspeaker+Microphone corrected MLS, SD = ",
          format(jsonFile$recordingChecks$system[[1]]$sd,nsmall = 1),
          " dB"
        )
      )
    ) %>%
    rbind(
      tibble(
        time = jsonFile$recordingChecks$component[[1]]$postT,
        power = jsonFile$recordingChecks$component[[1]]$postDb,
        label = paste0(
          transducer,
          "-corrected MLS, SD = ",
          format(jsonFile$recordingChecks$component[[1]]$sd, nsmall = 1),
          " dB"
        )
      )
    )
  
  medi = jsonFile$recordingChecks$component[[1]]$recT[length(jsonFile$recordingChecks$component[[1]]$recT) %/%
                                                        2]
  t <- rec %>% filter(time == medi,
                      is.finite(power))
  maxY <-
    round(max(rec$power / 10, post$power / 10, pre$power / 10)) * 10 + 15
  minY <- floor(min(t$power / 10)) * 10 - 35
  maxX <- ceiling(max(post$time / 0.5)) * 0.5
  p <-
    ggplot() +
    geom_line(
      data = pre,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "dashed"
    ) +
    geom_line(
      data = rec,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "solid"
    ) +
    geom_line(
      data = post,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "dashed"
    ) +
    scale_x_continuous(
      limits = c(0, maxX),
      breaks = seq(0, maxX, 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(minY, maxY),
      breaks = seq(minY, maxY, 10),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = c("red", "#3366FF", "#33FF00")) +
    theme_bw() +
    theme(
      legend.position = c(0.50, 0.93),
      legend.box = "horizontal",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.height = unit(1, "mm"),
      panel.background = element_blank(),
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      axis.line = element_line(colour = "black"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(
        t = .5,
        r = 0.5,
        b = .5,
        l = .5,
        "cm"
      )
    ) +
    guides(
      color = guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        color = NULL,
        ncol = 2
      ),
      guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        color = NULL,
        ncol = 3
      )
    ) +
    labs(
      color = NULL,
      linetype = NULL,
      title = "Power Variation in Wideband Recordings",
      y = "Power (dB)",
      x = "Time (s)"
    )
  if ("Loudspeaker Component IR" %in% names(jsonFile)) {
    p <- p + add_transducerTable_component(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = "",
      subtitle = list(c(
        subtitleOne, subtitleTwo, subtitleThree
      ))
    )
  } else {
    p <- p + add_transducerTable_loudspeaker(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = "",
      subtitle = list(c(
        subtitleOne, subtitleTwo, subtitleThree
      ))
    )
    
  }
  height = (maxY - minY) / 16 + 1
  return(list(plot = p, height = height))
}


plot_volume_power_variations <- function(fileJSON,
                                         sound_data,
                                         subtitleOne,
                                         subtitleTwo,
                                         subtitleThree) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
  transducer <-
    ifelse("Loudspeaker Component IR" %in% names(jsonFile),
           "Loudspeaker",
           "Microphone")
  volumeData <- jsonFile$recordingChecks$volume
  pre <- tibble()
  rec <- tibble()
  post <- tibble()
  
  for (name in names(volumeData)) {
    pre <- pre %>% rbind(tibble(
      time = volumeData[[name]]$preT,
      power = volumeData[[name]]$preDb,
      label = paste0(name, " dB, SD=", format(volumeData[[name]]$sd, nsmall = 1), " dB")
    ))
    rec <- rec %>% rbind(tibble(
      time = volumeData[[name]]$recT,
      power = volumeData[[name]]$recDb,
      label = paste0(name, " dB, SD=", format(volumeData[[name]]$sd, nsmall = 1), " dB")
    ))
    post <- post %>% rbind(tibble(
      time = volumeData[[name]]$postT,
      power = volumeData[[name]]$postDb,
      label = paste0(name, " dB, SD=", format(volumeData[[name]]$sd, nsmall = 1), " dB")
    ))
  }
  medi = volumeData[[1]]$recT[length(volumeData[[1]]$recT) %/% 2]
  t <- rec %>% filter(time == medi)
  
  colorOptions <- c(
    "#3366CC",
    "#DC3912",
    "#FF9900",
    "#109618",
    "#990099",
    "#0099C6",
    "#DD4477",
    "#66AA00",
    "#B82E2E",
    "#316395",
    "#994499",
    "#22AA99",
    "#AAAA11",
    "#6633CC",
    "#E67300",
    "#8B0707",
    "#329262",
    "#5574A6",
    "#651067"
  )
  colors <-
    colorOptions[(1:n_distinct(rec$label)) %% length(colorOptions)]
  maxY <- ceiling(max(rec$power / 10)) * 10 + 15
  maxX <- ceiling(max(post$time / 0.5)) * 0.5
  minY <- floor(min(t$power / 10)) * 10 - 35
  p <- ggplot() +
    geom_line(
      data = pre,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "dashed"
    ) +
    geom_line(
      data = rec,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "solid"
    ) +
    geom_line(
      data = post,
      aes(x = time, y = power, color = label),
      size = 0.8,
      linetype = "dashed"
    ) +
    scale_x_continuous(
      limits = c(0, maxX),
      breaks = seq(0, maxX, 0.5),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(minY, maxY),
      breaks = seq(minY, maxY, 10),
      expand = c(0, 0)
    ) +
    scale_color_manual(values = colors) +
    theme_bw() +
    theme(
      legend.position = c(0.30, 0.94),
      legend.box = "horizontal",
      legend.key = element_rect(colour = NA, fill = NA),
      legend.key.height = unit(1, "mm"),
      panel.background = element_blank(),
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      axis.line = element_line(colour = "black"),
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(
        t = .5,
        r = .5,
        b = .5,
        l = .5,
        "cm"
      )
    ) +
    guides(
      color = guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        color = NULL,
        ncol = 2
      ),
      guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        color = NULL,
        ncol = 3
      )
    ) +
    labs(
      color = NULL,
      linetype = NULL,
      title = "Power Variation in 1000 Hz Recordings",
      y = "Power (dB)",
      x = "Time (s)"
    )
  if ("Loudspeaker Component IR" %in% names(jsonFile)) {
    p <- p + add_transducerTable_component(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = "",
      subtitle = list(c(
        subtitleOne, subtitleTwo, subtitleThree
      ))
    )
  } else {
    p <- p + add_transducerTable_loudspeaker(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = "",
      subtitle = list(c(
        subtitleOne, subtitleTwo, subtitleThree
      ))
    )
  }
  height = (maxY - minY) / 16 + 1
  return(list(plot = p, height = height))
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

get_autocorrelation_plot <- function(fileJSON, sound_data) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyVector = F)
  defaultF <- jsonFile$sampleRate$loudspeaker
  time <- 1 / defaultF
  t <- tibble(Autocorrelation = unlist(jsonFile$autocorrelations))
  t$time <- seq(0, (nrow(t) - 1) * time, time)
  
  q <-
    ggplot(data = t,
           mapping = aes(x = time, y = Autocorrelation)) +
    geom_hline(aes(yintercept = 0)) +
    geom_segment(mapping = aes(xend = time, yend = 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(x = "Lag (s)") +
    theme_bw() +
    add_transducerTable_component(
      transducerTable = sound_data[[7]],
      position = c(2, 1.2),
      title_text = "",
      leftShift = -0.14,
      subtitle = sound_data$subtitle$component,
      transducerType = sound_data$inputParameters$transducerTypeF
    ) +
    sound_theme_display
  return(q)
}

plot_record_freq_system <- function(sound_data,
                                    convolutions,
                                    subtitle) {
  noise <- sound_data[[10]]
  noise <- noise %>%
    mutate(gain = 10 * log10(gain),
           label = "Recording of background")
  
  t <- sound_data[[6]]$system
  
  t <- t %>%
    mutate(gain = 10 * log10(gain))
  
  if (nrow(noise) > 1) {
    t <- t %>% rbind(noise)
  }
  t <- t %>%
    select(freq,
           gain,
           label) %>%
    rbind(convolutions$system) %>%
    rbind(sound_data[[11]])
  
  tmp <- t %>%
    filter(label == "Recording of MLS") %>%
    left_join(t %>% filter(label == "Recording of background") %>% select(freq, gain),
              by = "freq") %>%
    mutate(corrected_p = 10 ^ (gain.x / 10) - 10 ^ (gain.y / 10)) %>%
    mutate(corrected_p = ifelse(corrected_p > 0, corrected_p, NA)) %>%
    select(freq, corrected_p, label) %>%
    left_join(t %>% filter(label == "Filtered MLS") %>% select(freq, gain), by = "freq") %>%
    mutate(label = "Expected corr",
           gain = 10 * log10(corrected_p) + gain) %>%
    select(freq, gain, label)
  
  
  t <- rbind(t, tmp)
  
  tmp <- t %>%
    filter(freq >= sound_data[[12]]$calibrateSoundMinHz,
           freq <= sound_data[[12]]$fMaxHzSystem) %>%
    group_by(label) %>%
    filter(is.finite(gain)) %>%
    summarize(SD = format(round(sd(gain), 1), nsmall = 1))
  
  range <- paste0(
    " ",
    sound_data[[12]]$calibrateSoundMinHz,
    "<span> – </span>",
    sound_data[[12]]$fMaxHzSystem,
    " Hz"
  )
  
  #  <-  <- paste0('SD (dB): <span style="color:red;">**- -**</span>',
  #              tmp[tmp$label == "MLS",]$SD,
  #              ',<span style="color:red;"> **—**</span>',
  #              tmp[tmp$label == "Recording of filtered MLS",]$SD,
  #              ', <span style="color:#9900CC;">**—** ',
  #              tmp[tmp$label == "Expected corr",]$SD,
  #              "</span>",
  #              ', <span style="color:#3366FF;">**—** </span>',
  #              tmp[tmp$label == "Recording of MLS",]$SD,
  #              ", "
  # )
  tt <- paste0(
    'SD (dB): <span style="color:red;">**- -** ',
    tmp[tmp$label == "MLS", ]$SD,
    '</span>',
    ', <span style="color:red;">**—** ',
    tmp[tmp$label == "Recording of MLS", ]$SD,
    '</span>',
    ', <span style="color:#9900CC;">**—** ',
    tmp[tmp$label == "Expected corr", ]$SD,
    "</span>",
    ', <span style="color:#3366FF;">**—** ',
    tmp[tmp$label == "Recording of filtered MLS", ]$SD,
    "</span>,"
  )
  
  
  tt <- paste(tt, range)
  
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
  maxY <- round(max(tmp$gain) / 10) * 10 + 10
  tmp <- t %>% filter(freq >= 950,
                      freq <= 1050,
                      is.finite(gain))
  minY <- floor(min(tmp$gain) / 10) * 10 - 50
  p1 <- ggplot() +
    geom_line(
      data = t,
      aes(
        x = freq,
        y = gain,
        color = label,
        linetype = label
      ),
      size = 0.8
    ) +
    scale_x_log10(
      breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
      limits = c(20, 20000),
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
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 12),
      plot.title = element_text(size = 16),
      plot.subtitle = element_text(size = 12),
      plot.margin = margin(
        t = 2,
        r = .5,
        b = 1,
        l = .5,
        "inch"
      )
    ) +
    guides(
      color = guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        ncol = 3
      ),
      guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        ncol = 3
      )
    ) +
    # coord_fixed(ratio = 0.02, clip = "off") +
    labs(
      x = "Frequency (Hz)",
      y = "Power spectral density (dB)",
      color = NULL,
      linetype = NULL,
      title = "Loudspeaker + Microphone Correction"
    ) +
    add_transducerTable_system(
      transducerTable = sound_data[[7]],
      position = c("left", "bottom"),
      title_text = tt,
      subtitle = subtitle,
      leftShift = 0.015
    )
  
  
  return(list(
    plot = p1 + sound_theme_display,
    height = (maxY - minY) / 14 + 1
  ))
}

plot_record_freq_component <- function(sound_data,
                                       convolutions,
                                       subtitle) {
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
    left_join(t %>% filter(label == "Recording of background") %>% select(freq, gain),
              by = "freq") %>%
    mutate(corrected_p = 10 ^ (gain.x / 10) - 10 ^ (gain.y / 10)) %>%
    mutate(corrected_p = ifelse(corrected_p > 0, corrected_p, NA)) %>%
    select(freq, corrected_p, label) %>%
    left_join(t %>% filter(label == "Filtered MLS") %>% select(freq, gain), by = "freq") %>%
    mutate(label = "Expected corr",
           gain = 10 * log10(corrected_p) + gain) %>%
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
  
  #subtract component gain Recording of filtered MLS
  
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
  
  
  #subtract component gain Recording of MLS
  tmp <- t %>%
    filter(label == "Recording of MLS")
  
  tmp$transducergain = interpolated_values
  tmp <- tmp %>% mutate(gain = gain - transducergain) %>%
    select(freq,
           gain,
           label)
  t <- t %>% filter(label != "Recording of MLS") %>%
    rbind(tmp)
  
  
  #subtract component gain Recording of background
  tmp <- t %>%
    filter(label == "Recording of background")
  
  tmp$transducergain = interpolated_values
  tmp <- tmp %>% mutate(gain = gain - transducergain) %>%
    select(freq,
           gain,
           label)
  t <- t %>% filter(label != "Recording of background") %>%
    rbind(tmp)
  
  #subtract component gain Expectation
  tmp <- t %>%
    filter(label == "Expected corr")
  
  tmp$transducergain = interpolated_values
  tmp <- tmp %>% mutate(gain = gain - transducergain) %>%
    select(freq,
           gain,
           label)
  t <- t %>% filter(label != "Expected corr") %>%
    rbind(tmp)
  
  tmp <- t %>%
    filter(freq >= sound_data[[12]]$calibrateSoundMinHz,
           freq <= sound_data[[12]]$fMaxHzComponent) %>%
    group_by(label) %>%
    filter(is.finite(gain)) %>%
    summarize(SD = format(round(sd(gain), 1), nsmall = 1))
  
  range <- paste0(sound_data[[12]]$calibrateSoundMinHz,
                  " – ",
                  sound_data[[12]]$fMaxHzComponent,
                  " Hz")
  
  # tt <- paste0('SD(dB): <span style="color:red;">**- -**',
  #              tmp[tmp$label == "MLS",]$SD,
  #              '</span>',
  #              ', <span style="color:red;">**—**',
  #              tmp[tmp$label == "Recording of filtered MLS",]$SD,
  #              '</span>',
  #              ', <span style="color:#9900CC;">**—** ',
  #              tmp[tmp$label == "Expected corr",]$SD,
  #              "</span>",
  #              ', <span style="color:#3366FF;">**—** ',
  #              tmp[tmp$label == "Recording of MLS",]$SD,
  #              "</span>, "
  # )
  
  tt <- paste0(
    'SD (dB): <span style="color:red;">**- -** ',
    tmp[tmp$label == "MLS", ]$SD,
    '</span>',
    ', <span style="color:red;">**—** ',
    tmp[tmp$label == "Recording of MLS", ]$SD,
    '</span>',
    ', <span style="color:#9900CC;">**—** ',
    tmp[tmp$label == "Expected corr", ]$SD,
    "</span>",
    ', <span style="color:#3366FF;">**—** ',
    tmp[tmp$label == "Recording of filtered MLS", ]$SD,
    "</span>, "
  )
  
  tt <- paste(tt, range)
  tmp <- t %>% filter(freq <= 20000, is.finite(gain))
  maxY <- round(max(tmp$gain) / 10) * 10 + 10
  tmp <- t %>% filter(freq <= 1050, freq >= 950, is.finite(gain))
  minY <- floor(min(tmp$gain) / 10) * 10 - 50
  p1 <- ggplot(t) +
    geom_line(aes(
      x = freq,
      y = gain,
      color = label,
      linetype = label
    ), size = 0.8) +
    scale_x_log10(
      breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
      limits = c(20, 20000),
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
      legend.key = element_rect(color = NA, fill = NA),
      legend.key.height = unit(1, "mm"),
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      panel.background = element_blank(),
      axis.text.x = element_text(
        angle = 30,
        vjust = 0.5,
        hjust = 0.5
      ),
      axis.line = element_line(colour = "black"),
      plot.margin = margin(
        t = 2,
        r = .5,
        b = 1,
        l = .5,
        "inch"
      )
    ) +
    guides(
      color = guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        ncol = 3
      ),
      linetype = guide_legend(
        keywidth = unit(1, "cm"),
        fill = NULL,
        ncol = 3
      )
    ) +
    # coord_fixed(ratio = 0.02, clip = "off") +
    labs(
      x = "Frequency (Hz)",
      y = "Power spectral density (dB)",
      title = ifelse(
        transducerLabel == "microphone gain",
        "Loudspeaker Correction",
        "Microphone Correction"
      ),
      color = NULL,
      linetype = NULL
    )
  if (transducerLabel == "microphone gain") {
    p1 <-
      p1 + add_transducerTable_component(
        transducerTable = sound_data[[7]],
        position = c("left", "bottom"),
        title_text = tt,
        subtitle = subtitle
      )
  } else {
    p1 <-
      p1 + add_transducerTable_loudspeaker(
        transducerTable = sound_data[[7]],
        position = c("left", "bottom"),
        title_text = tt,
        subtitle = subtitle
      )
  }
  return(list(
    plot = p1 + sound_theme_display,
    height = (maxY - minY) / 14 + 1
  ))
}

grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <-
    ggplotGrob(plots[[1]] + theme(legend.position = "bottom"))$grobs
  legend <- g[[which(sapply(g, function(x)
    x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  arrangeGrob(
    # change here
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position = "none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight)
  )
}

SoundLevelModel <- function(inDb, dynamic_range_compression_model) {
  R = dynamic_range_compression_model$R
  `T` = dynamic_range_compression_model$`T`
  W = dynamic_range_compression_model$W
  backgroundDBSPL = dynamic_range_compression_model$backgroundDBSPL
  gainDBSPL = dynamic_range_compression_model$gainDBSPL
  # totalDbSpl = 10 * log10(10 ^ (backgroundDBSPL / 10) +
  #                           10 ^ ((gainDBSPL + inDb) / 10))
  # updated Jan 18th, 2024
  compressorDb <- CompressorDb(inDb, `T`, R, W)
  outDb <- compressorDb + gainDBSPL
  return(outDb)
  
}

CompressorDb <- function(inDb, `T`, R, W) {
  outDb = 0
  Q = 1/R
  WFinal = ifelse(W >= 0, W, 0)
  if (inDb > `T` + WFinal / 2) {
    outDb = `T` + Q* (inDb - `T`)
  } else if (inDb > (`T` - WFinal / 2)) {
    outDb = inDb + ((1 - Q) * (inDb - (`T` - WFinal / 2)) ^ 2) / (2 * WFinal)
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

plot_sound_level <-
  function(sound_data,
           subtitleOne,
           subtitleTwo,
           subtitleThree) {
    # add <- plot here
    volume_task <- sound_data[[1]]
    
    DRCMforDisplay <- data.frame(sound_data[[3]])
    
    DRCMforDisplay <- cbind(rownames(DRCMforDisplay), DRCMforDisplay)
    colnames(DRCMforDisplay) <- NULL
    rownames(DRCMforDisplay) <- NULL
    DRCMforDisplay[3, 1] = "Q = 1/R"
    DRCMforDisplay[1, 2] = paste(DRCMforDisplay[1, 2] , "dB                                    ")
    DRCMforDisplay[2, 2] = paste(DRCMforDisplay[2, 2] , "dB")
    DRCMforDisplay[4, 2] = paste(DRCMforDisplay[4, 2] , "dB")
    DRCMforDisplay[5, 2] = paste(DRCMforDisplay[5, 2] , "dB")
    DRCMforDisplay[6, 2] = paste(DRCMforDisplay[6, 2] , "dB")
    dynamic_range_compression_model <- sound_data[[5]] %>%
      select(`T`, W, `1/R`, gainDBSPL, backgroundDBSPL, RMSError)
    threshold <-
      dynamic_range_compression_model$`T` - dynamic_range_compression_model$gainDBSPL
    model <- sound_data[[2]]
    micGainDBSPL <-
      as.numeric(unlist(strsplit(sound_data[[7]][1, 2], " "))[1])
    
    minY = floor(min(volume_task$`out (dB SPL)`) / 10) * 10
    maxY = ceiling(max(volume_task$`out (dB SPL)`) / 10) * 10
    minX = plyr::round_any(min(volume_task$`in (dB)`), 10, floor)
    maxX = max(volume_task$`in (dB)`)
    p <-
      ggplot() +
      geom_point(data = volume_task, aes(x = `in (dB)`, y = `out (dB SPL)`), size = 3) +
      geom_vline(xintercept = threshold, color = "red") +
      geom_line(data = model, aes(x = x, y = y), size = 0.8) +
      geom_rect(aes(xmin = threshold, xmax = maxX, 
                    ymin = minY, ymax = maxY), alpha = 0.1, fill = "red") +
      add_parameters_table(
        parametersTable = DRCMforDisplay,
        position = c("left", "top"),
        title_text = " Dynamic Range Compression Model",
        leftShift = 0.02,
        baseSize = 8,
        fs = 8,
        shrinkPadding = 0.8
      ) +
      scale_y_continuous(
        limits = c(minY, maxY),
        breaks = seq(minY, maxY - 10, 10),
        expand = c(0, 0),
        sec.axis = sec_axis(
          ~ . - micGainDBSPL,
          breaks = seq(minY - micGainDBSPL, maxY - micGainDBSPL, 10),
          labels = scales::number_format(accuracy = 0.1),
          name = "Sound level (dB SPL)"
        )
      ) +
      scale_x_continuous(
        breaks = seq(minX, maxX, 10),
        limits = c(minX, maxX),
        expand = c(0, 0)
      ) +
      coord_fixed(ratio = 1, clip = "off") +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        axis.ticks.length = unit(-0.2, "line"),
        legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.box = "horizontal",
        plot.margin = margin(
          t = 0,
          b = 0,
          l = 0,
          r = 0,
          unit = "inch"
        ),
        
        axis.text.x = element_text(vjust = 0.5,
                                   hjust = 0.5),
        plot.title = element_text(size = 12, hjust = 0.5)
      ) +
      add_transducerTable_system(
        transducerTable = sound_data[[7]],
        position = c("left", "bottom"),
        title_text = "",
        subtitle = list(c(
          subtitleOne, subtitleTwo$system, subtitleThree$system
        )),
        leftShift = 0.03,
        baseSize = 8,
        fs = 8,
        shrinkPadding = 0.8
      ) +
      sound_theme_soundLevel +
      ylab("Output level (dB)") +
      xlab("Input level (dB)")
    
    thd_data <- filter(volume_task, `in (dB)` > -45)
    thdMax <- ceiling(max(thd_data$`THD (%)`) / 0.5) * 0.5
    thd <- ggplot() +
      geom_line(data = thd_data, aes(y = `THD (%)`, x = `in (dB)`), size = 0.8) +
      geom_point(data = thd_data, aes(y = `THD (%)`, x = `in (dB)`), size = 3) +
      geom_vline(xintercept = threshold, color = "red") +
      geom_rect(aes(xmin = threshold, xmax = maxX, 
                    ymin = 0, ymax = ceiling(max(
                      thd_data$`THD (%)`
                    ) / 0.5) * 0.5), alpha = 0.1, fill = "red") +
      scale_y_continuous(
        limits = c(0, thdMax),
        breaks = seq(0, thdMax, 0.5),
        expand = c(0, 0)
      ) +
      scale_x_continuous(
        limits = c(minX, maxX),
        breaks = seq(minX, maxX, 10),
        expand = c(0, 0)
      ) +
      coord_fixed(ratio = 6, clip = "on") +
      guides(color = guide_legend(
        order = 1,
        nrow = 2,
        title = ""
      )) +
      theme_bw() +
      theme(
        plot.background = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(),
        legend.position = "none",
        axis.ticks.length = unit(-0.2, "line"),
        plot.margin = margin(
          t = 0,
          b = -4.4 + thdMax * 0.4,
          l = 1.7,
          r = 2.3,
          unit = "inch"
        ),
        # -4 when max= 1.0%
        # -3.8 when max= 1.5%
        plot.title = element_text(size = 12, hjust = 0.5)
      ) +
      sound_theme_soundLevel +
      ggtitle("Sound Level at 1000 Hz") +
      labs(x = NULL, y = "THD (%)         ")
    height = (maxY - minY) / 10 + 2 + ceiling(max(volume_task$`THD (%)`) /
                                                6)
    width = (maxX - minX) / 8 + 2
    g = ggarrange(
      thd,
      p,
      nrow = 2,
      widths = width,
      align = "v"
    )
    return(list(
      # plot = cowplot::ggdraw(g) +
      #   theme(plot.background = element_rect(fill="white", color = NA)),
      plot = g,
      width = width,
      height = height,
      thd = thd
    ))
  }

get_ir_plots <- function(fileJSON) {
  file_list <- fileJSON$data
  jsonFile <- fromJSON(file_list[1], simplifyDataFrame = F)
  
  t <- tibble(IR = jsonFile$`Loudspeaker Component IR Time Domain`)
  defaultF <- jsonFile$sampleRate$loudspeaker / 1000
  time <- 1 / defaultF
  t$time <- seq(0, (nrow(t) - 1) * time, time)
  
  peak <- t$time[t$IR == max(t$IR)]
  
  IR_0to6 <- t %>% filter(time >= peak - 0.6,
                          time <= peak + 5.4)
  IR_0to30 <- t %>% filter(time >= peak - 5,
                           time <= peak + 45)

  IR_0to400 <- t %>%
    arrange(desc(time)) %>% 
    mutate(db = cumsum((IR) ^ 2)) %>% 
    mutate(db = db/max(db)) %>% 
    mutate(db = 10 * log10(db))
  
  minX <- ifelse(peak-75 > 0, peak - 75, 0)
  maxX <- ifelse(peak + 75 < max(t$time), peak + 75, max(t$time))
  
  p1 <- ggplot(IR_0to6, aes(x = time, y = IR)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-max(IR_0to6$IR), max(IR_0to6$IR))) +
    coord_cartesian(clip = 'off') +
    xlab("Time (ms)") +
    ylab("Amplitude") +
    theme_bw() +
    margin_theme +
    ggtitle(
      ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "6 ms of Loudspeaker Impulse Response",
        "6 ms of Microphone Impulse Response"
      )
    )
  
  p2 <- ggplot(IR_0to30, aes(x = time, y = IR)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0,0),
                       limits = c(-max(IR_0to30$IR),max(IR_0to30$IR)),
                       oob = function(x, ...) x) +
    coord_cartesian(clip = 'on') +
    xlab("Time (ms)") +
    ylab("Amplitude") +
    theme_bw() +
    margin_theme +
    ggtitle(
      ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "50 ms of Loudspeaker Impulse Response",
        "50 ms of Microphone Impulse Response"
      )
    )
  
  tmp <- filter(IR_0to400, time >= minX, time <= maxX)
  
  minY <- min(tmp$db)
  maxY <- ceiling(max(tmp$db)/10) * 10
  
  p3 <- ggplot(IR_0to400, aes(x = time, y = db)) +
    geom_line(size = 0.8) +
    scale_x_continuous(expand = c(0, 0),
                       limits = c(peak-40, peak + 360)) +
    scale_y_continuous(
      expand = c(0, 0),
      limits = c(maxY- 70, maxY),
      breaks = seq(maxY- 70,maxY, 10)
    ) +
    xlab("Time (ms)") +
    ylab("Cumulative power") +
    theme_bw() +
    margin_theme +
    ggtitle(
      ifelse(
        "Loudspeaker Component IR" %in% names(jsonFile),
        "Schroeder plot of loudspeaker impulse response",
        "Schroeder plot of microphone impulse response"
      )
    )
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
        "CalibrationDate" %in% names(jsonFile),
        jsonFile$CalibrationDate,
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
    paste(format(round(micInfo$gainDBSPL, 1), nsmall = 1), "dB gain at 1 kHz                                             ")
  loudspeakerAudioDevice <- jsonFile$webAudioDeviceNames$loudspeaker
  micAudioDevice <- jsonFile$webAudioDeviceNames$microphone
  if (nchar(loudspeakerAudioDevice) > 0 &&
      nchar(micAudioDevice) > 0) {
    if (nchar(loudspeakerAudioDevice) > 0) {
      indexLoudspeakers <- unlist(gregexpr(' ', loudspeakerAudioDevice))
      if (length(indexLoudspeakers) > 1) {
        indexLoudspeaker <- indexLoudspeakers[length(indexLoudspeakers) - 1]
        loudspeakerAudioDeviceOne <-
          paste0('"',
                 substr(loudspeakerAudioDevice, 1, indexLoudspeaker))
        loudspeakerAudioDeviceTwo <-
          paste0(substr(
            loudspeakerAudioDevice,
            indexLoudspeaker + 1,
            nchar(loudspeakerAudioDevice)
          ),
          '"')
      } else {
        micAudioDeviceOne = paste0('"', micAudioDevice, '"')
        micAudioDeviceTwo = ""
      }
    } else {
      loudspeakerAudioDeviceOne = loudspeakerAudioDevice
      loudspeakerAudioDeviceTwo = ""
    }
    
    
    
    if (nchar(micAudioDevice) > 0) {
      indexMics <- unlist(gregexpr(' ', micAudioDevice))
      if (length(indexMics) > 1) {
        indexMic <- indexMics[length(indexMics)]
        micAudioDeviceOne <-
          paste0('"', substr(micAudioDevice, 1, indexMic))
        micAudioDeviceTwo <-
          paste0(substr(micAudioDevice, indexMic + 1, nchar(micAudioDevice)), '"')
      } else {
        micAudioDeviceOne = paste0('"', micAudioDevice, '"')
        micAudioDeviceTwo = ""
      }
    } else {
      micAudioDeviceOne = micAudioDevice
      micAudioDeviceTwo = ""
    }
    
    transducerTable <- data.frame(rbind(
      c(paste(format(loudspeaker$gainDBSPL, nsmall = 1), "dB"), micGainDBSPL),
      c(
        paste(jsonFile$sampleRate$loudspeaker, "Hz"),
        paste(
          jsonFile$sampleRate$microphone,
          "Hz,",
          ifelse(jsonFile$sampleSize == 16, 24, jsonFile$sampleSize),
          "bit sampling"
        )
      ),
      c(loudspeaker$OEM, micInfo$OEM),
      c(
        paste(loudspeaker$fullLoudspeakerModelName, "  "),
        micInfo$micModelName
      ),
      c(
        paste(loudspeaker$fullLoudspeakerModelNumber, "  "),
        micInfo$ID
      ),
      c(paste(loudspeakerAudioDeviceOne, " "), micAudioDeviceOne),
      c(loudspeakerAudioDeviceTwo, micAudioDeviceTwo),
      c(loudspeaker$CalibrationDate, NA)
    ))
  } else {
    transducerTable <- data.frame(rbind(
      c(paste(format(loudspeaker$gainDBSPL, nsmall = 1), "dB"), micGainDBSPL),
      c(
        paste(jsonFile$sampleRate$loudspeaker, "Hz"),
        paste(
          jsonFile$sampleRate$microphone,
          "Hz,",
          ifelse(jsonFile$sampleSize == 16, 24, jsonFile$sampleSize),
          "bit sampling"
        )
      ),
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
  t1 <- component_iir %>%
    filter(Hz_component_iir > 20,
           Hz_component_iir < 20000,
           is.finite(dB_component_iir))
  t2 <- component_iir_no_bandpass %>%
    filter(
      Hz_component_iir_no_bandpass > 20,
      Hz_component_iir_no_bandpass < 20000,
      is.finite(dB_component_iir_no_bandpass)
    )
  
  t1000 <- t1 %>% filter(Hz_component_iir > 950,
                         Hz_component_iir < 1050)
  
  minY = floor(min(t1000$dB_component_iir) / 10) * 10 - 40
  maxY = ceiling(max(t1$dB_component_iir, t2$dB_component_iir_no_bandpass) /
                   10) * 10  + 10
  
  heightOne = (maxY - minY) / 13
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
    scale_x_log10(
      limits = c(20, 20000),
      breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(minY, maxY),
      breaks = seq(minY, maxY, 10),
      expand = c(0, 0)
    ) +
    ylab("dB") +
    xlab("Frequency (Hz)") +
    ggtitle("Power Spectral Density of Loudspeaker IIR") +
    scale_color_manual(values = c("#3366FF", "red")) +
    labs(color = "") +
    theme_bw() +
    theme(
      legend.position = c(0.5, 0.9),
      legend.box = "horizontal",
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      legend.margin = margin(
        t = 0,
        b = 0,
        l = .5,
        r = .5,
        unit = "inch"
      ),
      plot.margin = margin(
        t = .5,
        b = .5,
        l = .5,
        r = 1,
        unit = "inch"
      ),
      axis.line = element_line(colour = "black")
    )
  
  t1 <- system_iir %>%
    filter(Hz_system_iir > 20,
           Hz_system_iir < 20000,
           is.finite(dB_system_iir))
  
  t2 <- system_iir_no_bandpass %>%
    filter(
      Hz_system_iir_no_bandpass > 20,
      Hz_system_iir_no_bandpass < 20000,
      is.finite(dB_system_iir_no_bandpass)
    )
  
  # Plot Power Spectral Density of system IIR
  
  system1000 <- t1 %>% filter(Hz_system_iir > 950,
                              Hz_system_iir < 1050)
  
  minY = floor(min(system1000$dB_system_iir) / 10) * 10 - 40
  
  maxY = ceiling(max(c(
    t1$dB_system_iir, t2$dB_system_iir_no_bandpass
  )) / 10) * 10 + 10
  heightTwo = (maxY - minY) / 13
  
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
    scale_x_log10(
      limits = c(20, 20000),
      breaks = c(20, 100, 200, 1000, 2000, 10000, 20000),
      expand = c(0, 0)
    ) +
    scale_y_continuous(
      limits = c(minY, maxY),
      breaks = seq(minY, maxY, 10),
      expand = c(0, 0)
    ) +
    ylab("dB") +
    xlab("Frequency (Hz)") +
    ggtitle("Power Spectral Density of System IIR") +
    scale_color_manual(values = c("#3366FF", "red")) +
    labs(color = "") +
    theme_bw() +
    theme(
      legend.position = c(0.5, 0.9),
      legend.box = "horizontal",
      legend.margin = margin(
        t = 0,
        b = .0,
        l = .5,
        r = .5,
        unit = "inch"
      ),
      legend.background = element_rect(fill = 'transparent', color = 'transparent'),
      plot.margin = margin(
        t = .5,
        b = .5,
        l = .5,
        r = 1,
        unit = "inch"
      ),
      axis.line = element_line(colour = "black")
    )
  return(list(p1, p2, heightOne, heightTwo))
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
    scale_x_log10(breaks = c(20, 100, 200, 1000, 2000, 10000, 20000)) +
    coord_fixed(0.05) +
    theme_bw() +
    labs(y = "Gain (dB)",
         x = "Frequency (Hz)",
         title = "Power Spetral Density of system convolution (filtered mls)")
  p2 <- ggplot(componentConv, aes(x = freq, y = gain)) +
    geom_line(size = 0.8) +
    scale_x_log10(breaks = c(20, 100, 200, 1000, 2000, 10000, 20000)) +
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
                                       subtitle = list(),
                                       leftShift = 0.015,
                                       baseSize = 12,
                                       fs = 12,
                                       shrinkPadding = 1) {
  geom_table_costumized(
    data = transducerTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(transducerTable %>% head(nrow(transducerTable) - 1)),
      text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
    ),
    title_text = title_text,
    titleFont = 1,
    subtitle = subtitle,
    leftShift = leftShift,
    fs = fs,
    shrinkPadding = shrinkPadding,
    table.theme = ttheme_default(
      base_size = baseSize,
      padding = unit(c(0.2, 0.3), "line"),
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
                                          subtitle = list(),
                                          leftShift = 0.015,
                                          baseSize = 12,
                                          fs = 12,
                                          transducerType = "Loudspeaker") {
  if (transducerType == "Loudspeaker") {
    geom_table_costumized(
      data = transducerTable,
      aes(
        npcx = position[1],
        npcy = position[2],
        label = list(transducerTable %>% head(nrow(
          transducerTable
        ) - 1)),
        text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
      ),
      title_text = title_text,
      titleFont = 1,
      subtitle = subtitle,
      leftShift = leftShift,
      shrinkPadding = 1,
      fs = fs,
      table.theme = ttheme_default(
        base_size = baseSize,
        padding = unit(c(0.2, 0.3), "line"),
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
  } else {
    geom_table_costumized(
      data = transducerTable,
      aes(
        npcx = position[1],
        npcy = position[2],
        label = list(transducerTable %>% head(nrow(
          transducerTable
        ) - 1)),
        text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
      ),
      title_text = title_text,
      titleFont = 1,
      subtitle = subtitle,
      leftShift = leftShift,
      fs = fs,
      shrinkPadding = 1,
      table.theme = ttheme_default(
        base_size = baseSize,
        padding = unit(c(0.2, 0.3), "line"),
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
}

add_transducerTable_loudspeaker <- function(transducerTable,
                                            position,
                                            title_text = "",
                                            subtitle = list(),
                                            leftShift = 0.015,
                                            baseSize = 12,
                                            fs = 12) {
  geom_table_costumized(
    data = transducerTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(transducerTable %>% head(nrow(transducerTable) - 1)),
      text = transducerTable$`Loudspeaker`[nrow(transducerTable)]
    ),
    title_text = title_text,
    titleFont = 1,
    subtitle = subtitle,
    leftShift = leftShift,
    fs = fs,
    shrinkPadding = 1,
    table.theme = ttheme_default(
      base_size = baseSize,
      padding = unit(c(0.2, 0.3), "line"),
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


add_parameters_table <- function(parametersTable,
                                 position,
                                 title_text = "",
                                 titleFont = 1,
                                 subtitle = "",
                                 leftShift = 0.015,
                                 baseSize = 12,
                                 shrinkPadding = 1,
                                 fs = 12) {
  geom_table_costumized(
    data = parametersTable,
    aes(
      npcx = position[1],
      npcy = position[2],
      label = list(parametersTable),
      text = ""
    ),
    titleFont = titleFont,
    title_text = title_text,
    subtitle = subtitle,
    leftShift = leftShift,
    fs = fs,
    shrinkPadding = shrinkPadding,
    table.theme = ttheme_default(
      base_size = baseSize,
      padding = unit(c(0.2, 0.3), "line"),
      colhead = list(
        fg_params = NULL,
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
  legend.title = element_text(size = 20),
  legend.text = element_text(size = 20),
  axis.title = element_text(size = 20),
  axis.text = element_text(size = 20),
  plot.title = element_text(size = 20),
  plot.subtitle = element_text(size = 20)
)


sound_theme_display <- theme(
  legend.title = element_text(size = 12),
  legend.text = element_text(size = 12),
  axis.title = element_text(size = 12),
  axis.text = element_text(size = 12),
  plot.title = element_text(size = 12),
  plot.subtitle = element_text(size = 12)
)

sound_theme_soundLevel <-  theme(
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10),
  axis.title = element_text(size = 10),
  axis.text = element_text(size = 10),
  plot.title = element_text(size = 10),
  plot.subtitle = element_text(size = 10)
)

margin_theme <-
  theme(plot.margin = unit(c(5.5,16,5.5,5.5), "pt"))
