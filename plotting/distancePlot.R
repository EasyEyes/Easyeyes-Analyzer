get_measured_distance_data <- function(data_list) {
  df <- tibble()
  for (i in 1:length(data_list)) {
    t <- data_list[[i]] %>%
      select(participant,
             calibrateTrackDistanceMeasuredCm,
             calibrateTrackDistanceRequestedCm) %>% 
      distinct() %>% 
      filter(!is.na(calibrateTrackDistanceMeasuredCm),
             !is.na(calibrateTrackDistanceRequestedCm),
             calibrateTrackDistanceMeasuredCm != '',
             calibrateTrackDistanceRequestedCm != '')
    print(t)
    if (nrow(t) > 0) {
      tmp <- tibble(participant = t$participant[1], 
                    calibrateTrackDistanceMeasuredCm = jsonlite::fromJSON(t$calibrateTrackDistanceMeasuredCm),
                    calibrateTrackDistanceRequestedCm = jsonlite::fromJSON(t$calibrateTrackDistanceRequestedCm))
      df <- rbind(df, tmp)
    }
  }
  return(df)
}


plot_distance <- function(data_list) {
  print('inside plot_distance')
  distance <- get_measured_distance_data(data_list)
  print(distance)
  fit <- lm(calibrateTrackDistanceMeasuredCm~calibrateTrackDistanceRequestedCm, data = distance)
  print(coef(fit))
  slope <- coef(fit)
  slope <- format(round(slope[['calibrateTrackDistanceRequestedCm']],2),nsmall=2)
  corr <- cor(distance$calibrateTrackDistanceRequestedCm, distance$calibrateTrackDistanceMeasuredCm)
  corr <- format(round(corr,2),nsmall=2)
  p <- ggplot() + 
    geom_point(data=distance, aes(x = calibrateTrackDistanceRequestedCm, y = calibrateTrackDistanceMeasuredCm)) + 
    ggpp::geom_text_npc(aes(npcx="left",
                             npcy="top"),
                         label = paste0('N=',nrow(distance),'\n',
                                        'R=',corr,'\n',
                                        'slope=',slope)) + 
    geom_smooth(method='lm',
                se=F)
  return(p)
}