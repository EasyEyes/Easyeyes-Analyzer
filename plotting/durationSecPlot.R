plot_duraction_sec <- function(data_list) {
  print('inside plot_duraction_sec')
  df <- foreach(i=1:length(data_list), .combine = 'rbind') %do% {
    data_list[[i]] %>%
      select(font, targetMeasuredDurationSec,fontNominalSizePt)
  }
  if (nrow(df) == 0) {
    return(NULL)
  }
  df <- df %>% mutate(fontNominalSizePt = as.numeric(fontNominalSizePt),
                      targetMeasuredDurationSec = as.numeric(targetMeasuredDurationSec)) %>%
    distinct() %>%
    filter(!is.na(font))
  p <- ggplot() +
    geom_point(data=df,
               aes(x=fontNominalSizePt, 
                   y = targetMeasuredDurationSec,
                   color = font)) +
    scale_x_log10(expand=c(0,1)) +
    scale_y_log10(expand=c(0,1)) + 
    labs(title = 'targetMeasuredDurationSec')
 return(p)
}
