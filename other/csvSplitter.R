library(readr)
library(dplyr)

splitCSVfunction <- function(file){
  dt <- read_csv(file)
  dt$fileNames = ifelse(is.na(dt$ProlificParticipantID), 
                        paste(dt$participant, dt$experiment, dt$date, sep = '-'),
                        paste(dt$participant, dt$ProlificParticipantID, dt$experiment, dt$date, sep = '-'))
  dt$fileNames = stringr::str_replace_all(dt$fileNames, "/",".")
  data_list <- list()
  i = 1
  for (name in unique(dt$participant)) {
    tmp <- dt[dt$participant == name,]
    data_list[[i]] = tmp
    i = i + 1
  }
  return(data_list)
}



# split multiple db zip file
# setwd("~/Downloads/dbs")
# file_list <- list.files("~/Downloads/dbs")
# csvRepo = '~/Downloads/csv files/'
# for (i in 1:length(file_list)){
#   if (grepl(".zip", file_list[i])) {
#     file_names <- unzip(file_list[i], list = TRUE)$Name
#     all_csv <- file_names[grepl(".csv", file_names)]
#     for (j in 1 : length(all_csv)) {
#       dt <- read_csv(unzip(file_list[i], all_csv[j]))
#       dt$fileNames = ifelse(is.na(dt$ProlificParticipantID), 
#                             paste(dt$participant, dt$experiment, dt$date, sep = '-'),
#                             paste(dt$participant, dt$ProlificParticipantID, dt$experiment, dt$date, sep = '-'))
#       for (name in unique(dt$participant)) {
#         tmp <- dt[dt$participant == name,]
#         fileName = unique(tmp$fileNames)
#         tmp <- tmp %>% select(-fileNames)
#         write.csv(tmp, file = paste0(csvRepo,fileName, '.csv'))
#       }
#     }
#   }
# }


