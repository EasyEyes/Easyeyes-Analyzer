# this is the package to parse json data from csv
library(jsonlite)
# plot packages
preprocess_sound_data <- function(data_list) {
  sound_list <- list()
  foreach(i = 1 : length(data_list)) %do% {
    t <- data_list[[i]]
    if (!('1000 Hz in (dB)' %in% colnames(t))) {
      t$`1000 Hz in (dB)` <- ""
    }
    if (!('THD' %in% colnames(t))) {
      t$THD <- ""
    }
    if (!('All Hz out (dB SPL)' %in% colnames(t))) {
      t$`All Hz out (dB SPL)` <- ""
    }
    if (!('1000 Hz out (dB SPL)' %in% colnames(t))) {
      t$`1000 Hz out (dB SPL)` <- ""
    }
    if (!('Recording with filter (Hz)' %in% colnames(t))) {
      t$`Recording with filter (Hz)` <- ""
    }
    if (!('Recording with filter (dB)' %in% colnames(t))) {
      t$`Recording with filter (dB)` <- ""
    }
    if (!('Recording (Hz)' %in% colnames(t))) {
      t$`Recording (Hz)` <- ""
    }
    if (!('Recording (dB)' %in% colnames(t))) {
      t$`Recording (dB)` <- ""
    }
    if (!('Sound gain parameters' %in% colnames(t))) {
      t$`Sound gain parameters` <- ""
    }
    if (!('Microphone calibration results' %in% colnames(t))) {
      t$`Microphone calibration results` <- ""
    }
    sound_list[[i]] <- t %>% 
      rename("Pavlovia Session ID" = "participant") %>% 
      select(`Pavlovia Session ID`,
             ProlificParticipantID,
             `1000 Hz in (dB)`,
             `1000 Hz out (dB SPL)`,
             THD,
             `All Hz out (dB SPL)`,
             `Sound gain parameters`,
             `Recording with filter (Hz)`,
             `Recording with filter (dB)`,
             `Recording (Hz)`,
             `Recording (dB)`,
             `Microphone calibration results`
             )
  }
  return(sound_list)
}

get_all_sound_data <- function(sound_list) {
  dynamic_range_compression_model <- tibble()
  sound_data <- tibble()
  model <- tibble()
  recording <- tibble()
  result1000 <- tibble()
  IR <- list()
  for (i in 1 : length(sound_list)) {
    if (sound_list[[i]]$`1000 Hz in (dB)`[1] == "" ||
        sound_list[[i]]$`Microphone calibration results`[1] == "") {
      return(list(sound_data, 
                  dynamic_range_compression_model, 
                  tibble(), 
                  tibble(), 
                  tibble(),
                  c(""),
                  tibble()))
    } else {
      t <- get_sound_calibration_table(sound_list[[i]])
      dynamic_range_compression_model <- rbind(dynamic_range_compression_model, t[[2]]) 
      sound_data <- rbind(sound_data,t[[1]])
      model <- rbind(model, t[[3]])
      mr <- process_micro_cali_result(sound_list[[i]])
      recording <- rbind(recording,mr[[1]])
      result1000 <- rbind(result1000,mr[[2]])
      IR[[i]] <- mr[[3]]
    }
  }
  record_freq <- get_recording_vs_frequency_all(sound_list)
  DRCMforDisplay <- dynamic_range_compression_model %>% 
    select(`T`, W, `1/R`, gainDBSPL, backgroundDBSPL, RMSError) %>% 
    mutate_if(is.numeric,round,digits = 1)
  names = colnames(DRCMforDisplay)
  DRCMforDisplay <- t(DRCMforDisplay)
  return(list(sound_data, 
              dynamic_range_compression_model, 
              record_freq, 
              model, 
              DRCMforDisplay,
              names,
              recording,
              result1000,
              IR))
}


get_sound_calibration_table <- function(csvFile) {
  sound_calibration <- tibble(`1000 Hz in (dB)` = fromJSON(csvFile$`1000 Hz in (dB)`[1]),
                              `1000 Hz out (dB SPL)` = fromJSON(csvFile$`1000 Hz out (dB SPL)`[1]),
                              THD = fromJSON(csvFile$THD[1]) * 100,
                              `All Hz out (dB SPL)` = fromJSON(csvFile$`All Hz out (dB SPL)`[1]))
  sound_calibration <- sound_calibration %>% 
    mutate(`Pavlovia Session ID` = csvFile$`Pavlovia Session ID`[1],
           ProlificParticipantID = csvFile$ProlificParticipantID[1],
           `out - in (dB SPL)` = `1000 Hz out (dB SPL)` - `1000 Hz in (dB)`,
           THD = round(THD, 2)) %>% 
    select(ProlificParticipantID,
           `Pavlovia Session ID`,
           `1000 Hz in (dB)`,
           `1000 Hz out (dB SPL)`,
           `out - in (dB SPL)`,
           THD,
           `All Hz out (dB SPL)`) %>% 
    rename(
           "in (dB)" = "1000 Hz in (dB)",
           "out (dB SPL)" = "1000 Hz out (dB SPL)",
           "THD (%)" = "THD",
           "out @all Hz (dB SPL)" = "All Hz out (dB SPL)",)
  dynamic_range_compression_model <- data.frame(fromJSON(csvFile$`Sound gain parameters`[1])) %>% 
    mutate(`Pavlovia Session ID` = csvFile$`Pavlovia Session ID`[1],
           ProlificParticipantID = csvFile$ProlificParticipantID[1],
           `1/R` = as.character(round(1/R,1)))
  model <- get_sound_model(sound_calibration,dynamic_range_compression_model)
  return(list(sound_calibration, dynamic_range_compression_model, model))
}


get_recording_vs_frequency <- function(csvFile){
  recording_vs_freq <- rbind(
    tibble(`Pavlovia Session ID` = csvFile$`Pavlovia Session ID`[1],
           ProlificParticipantID = csvFile$ProlificParticipantID[1],
           `Recording (Hz)` = fromJSON(csvFile$`Recording with filter (Hz)`[1]),
           `Recording (dB)` = fromJSON(csvFile$`Recording with filter (dB)`[1]),
           filter = "Filtered"),
    tibble(`Pavlovia Session ID` = csvFile$`Pavlovia Session ID`[1],
           ProlificParticipantID = csvFile$ProlificParticipantID[1],
           `Recording (Hz)` = fromJSON(csvFile$`Recording (Hz)`[1]),
           `Recording (dB)` = fromJSON(csvFile$`Recording (dB)`[1]),
           filter = "Unfiltered")
    )
  return(recording_vs_freq)
}

get_recording_vs_frequency_all <- function(sound_list) {
  sound_data <- foreach(i = 1 : length(sound_list), .combine = "rbind") %do% {
    t <- get_recording_vs_frequency(sound_list[[i]])
  }
  return(sound_data)
}

SoundLevelModel <- function(inDb,dynamic_range_compression_model) {
  R = dynamic_range_compression_model$R
  `T` = dynamic_range_compression_model$`T`
  W = dynamic_range_compression_model$W
  backgroundDBSPL = dynamic_range_compression_model$backgroundDBSPL
  gainDBSPL = dynamic_range_compression_model$gainDBSPL
  totalDbSpl = 10 * log10(10 ^ (backgroundDBSPL / 10) +
                             10 ^ ((gainDBSPL + inDb) / 10))
  measuredDbSpl <- CompressorDb(totalDbSpl,`T`,R,W)
  return(measuredDbSpl)
  
}

CompressorDb <- function(inDb,`T`,R,W) {
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

get_sound_model <- function(sound_calibration,dynamic_range_compression_model) {
  minX = min(sound_calibration$`in (dB)`)
  maxX = max(sound_calibration$`in (dB)`)
  x = seq(minX, maxX, 0.1)
  y = sapply(x, SoundLevelModel, dynamic_range_compression_model = dynamic_range_compression_model)
  t <- tibble(x, y) %>% 
    mutate(`Pavlovia Session ID` = sound_calibration$`Pavlovia Session ID`[1])
  return(t)
}

plot_sound_level <- function(all_sound_data){
  sound_data <- all_sound_data[[1]]
  dynamic_range_compression_model <- all_sound_data[[2]]
  model <- all_sound_data[[4]]
  sound_data <- sound_data %>% 
    left_join(dynamic_range_compression_model, by = c("Pavlovia Session ID","ProlificParticipantID")) %>% 
    mutate(RMSError = round(RMSError,2)) %>% 
    mutate(label = paste("RMSError =", RMSError))
  labels <- unique(sound_data$label)
  minY = plyr::round_any(min(sound_data$`out (dB SPL)`),10,floor)
  maxY = plyr::round_any(max(sound_data$`out (dB SPL)`),10,ceiling)
  p <- ggplot(data = sound_data, aes(x = `in (dB)`, y = `out (dB SPL)`, color = `Pavlovia Session ID`)) + 
    # scale_shape_manual(values = rep(16,n_distinct(sound_data$`Pavlovia Session ID`))) +
    # geom_point(aes(shape = label), color = "white") + 
    geom_point(aes(color = `Pavlovia Session ID`), size = 3) + 
    geom_line(data = model, aes(x=x, y=y,color = `Pavlovia Session ID`)) + 
    scale_y_continuous(breaks = seq(minY, maxY, 10)) + 
    scale_x_continuous(n.breaks = 7, 
                       limits = c(-60, 0),
                       expand = c(0,0)) + 
    coord_fixed(ratio = 1, clip = "off") + 
    guides(color = guide_legend(order=1, nrow = 2, title = "")) + 
    ggpp::geom_text_npc(
      aes(npcx = "left",
          npcy = "top",
          label = paste(labels, collapse = "\n"))) +
    theme_bw() + 
    theme(
          legend.box = "horizontal",
          legend.position = "top", 
          legend.margin = margin(0,-.5,0,0, unit="cm"),
          legend.key = element_rect(fill = "white"),
          legend.title = element_text(size=12),
          legend.text = element_text(size=12),
          axis.title = element_text(size = 12),
          axis.text = element_text(size = 12),
          plot.title = element_text(size=14, hjust = 0.5),
          plot.subtitle = element_text(size=12)) + 
    ggtitle("Sound Level at 1000 Hz") +
    ylab("out (dB)")
  p
}

plot_record_freq <- function(all_sound_data) {
  record_freq <- all_sound_data[[3]] %>% 
    mutate(`Recording (dB)` = 10*log10(`Recording (dB)`))
           
  record_freq_filter <- record_freq %>% 
    filter(filter == "Filtered",
           `Recording (Hz)` >= 20,
           `Recording (Hz)` <= 10000) %>% 
    group_by(`Pavlovia Session ID`,filter) %>% 
    summarize(SD = sd(`Recording (dB)`))
  record_freq <- record_freq %>% 
    left_join(record_freq_filter, by = c("Pavlovia Session ID", "filter")) %>% 
    mutate(label = ifelse(filter == "Filtered", 
                          paste(`Pavlovia Session ID`,
                                "Filtered, SD =", 
                                round(SD,2)), 
                          paste(`Pavlovia Session ID`,
                                filter)))
  record_freq$`Pavlovia Session ID` <- factor(record_freq$`Pavlovia Session ID`)
  record_freq$filter <- factor( record_freq$filter)
  n <- n_distinct(record_freq$`Pavlovia Session ID`)
  ggplot(record_freq) +
   geom_line(aes(x = `Recording (Hz)`, 
                 y = `Recording (dB)`, 
                 linetype = label, 
                 color = label)) +
    scale_color_manual(values = rep(1:n,each = 2)) +
    scale_linetype_manual(values = rep(1:2,n)) + 
    scale_x_log10(breaks = c(20, 100, 200, 1000, 2000, 10000, 16000),
                 limits = c(20,16000)) +
    scale_y_continuous(n.breaks = 7, limits = c(-115, -50)) + 
    coord_fixed(0.05) + 
   theme_bw() +
    theme(legend.direction = "vertical", 
          legend.box = "horizontal",
          legend.position = "top", 
          legend.margin = margin(0,-1,0,0, unit="cm"),
          legend.key = element_rect(fill = "white"),
          legend.key.size = unit(4.5, "mm"),
          legend.title = element_text(size=14),
          legend.text = element_text(size=14),
          panel.background = element_blank(), 
          axis.title = element_text(size = 14),
          axis.text = element_text(size = 14),
          axis.text.x = element_text(angle = 55, vjust = 0.5, hjust=0.5),
          axis.line = element_line(colour = "black"),
          plot.title = element_text(size=11),
          plot.subtitle = element_text(size=14)) + 
    guides(color = guide_legend(title = "", keywidth = unit(1, "cm")),
           linetype = guide_legend(title = "")) + 
   labs(x = "Frequency (Hz)",
        y = "Recording (dB)")
}


process_micro_cali_result <- function(data){
  if (data$`Microphone calibration results`[1] == "") {
    return(tibble())
  }
  t <- fromJSON(data$`Microphone calibration results`[1])
  recording_result <- tibble()
  dB_1000Hz_result <- tibble()
  ir_result <- tibble()
  j = 1
  for (i in 1 : length(t$name)) {
    unfiltered <- dplyr::tibble(name = t$name[i],
                              filter = "unfiltered",
                              `Recording (Hz)` = unlist(t$Recording_Hz[i]),
                              `Recording (dB)` = unlist(t$Recording_dB[i]))
    filtered <- dplyr::tibble(name = t$name[i],
                            filter = "filtered",
                            `Recording (Hz)` = unlist(t$Recording_with_Filter_Hz[i]),
                            `Recording (dB)` = unlist(t$Recording_with_Filter_dB[i]))
    dB_1000Hz <- dplyr::tibble(
      name = t$name[i],
      in_dB_1000Hz = unlist(t$in_dB_1000Hz[i]), 
      out_dBSPL_1000Hz = unlist(t$out_dBSPL_1000Hz[i]))
    
    ir <- dplyr::tibble(
      name = t$name[i],
      Gain = unlist(t$ir$Gain[i])
      )
    impulse_response <- rbind(unfiltered,filtered)
    impulse_response$`Pavlovia Session ID` <- data$`Pavlovia Session ID`[1]
    impulse_response$id <- j
    dB_1000Hz$id <- j
    ir$id = j
    dB_1000Hz_result <- rbind(dB_1000Hz_result, dB_1000Hz)
    j = j + 1
    impulse_response$`Recording (dB)` <- 10*log10(impulse_response$`Recording (dB)`)
    impulse_response <- impulse_response %>% mutate(Time = 1000 / `Recording (Hz)`)
    recording_result <- rbind(recording_result, impulse_response)
    ir_result <- rbind(ir_result, ir)
  }
  return(list(recording_result,dB_1000Hz_result,ir_result))
}
