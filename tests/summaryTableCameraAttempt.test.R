# Run: Rscript tests/summaryTableCameraAttempt.test.R
# Regression: generate_summary_table crashed ("Must use existing variables —
# cameraAttempt") when no uploaded CSV has a cameraAttemptJSON column, because
# the pre-join distinct() listed cameraAttempt before the left_join that
# creates it. Runs the real read_files + generate_summary_table pipeline on a
# minimal CSV without any camera data.
suppressPackageStartupMessages({
  library(dplyr); library(tibble); library(lubridate); library(DT)
  library(readr); library(stringr); library(tidyr); library(emojifont)
})
source("R/load_app.R")

csv <- file.path(tempdir(), "abc123_exp_0001_2026-01-01_10h00.00.000_UTC.csv")
writeLines("participant,date\nabc123,2026-01-01_10h00.00.000", csv)
file <- list(name = basename(csv), data = csv, datapath = csv)

parsed <- read_files(file, progress = NULL)
summary <- generate_summary_table(
  parsed$data_list, parsed$stairs, parsed$pretest, parsed$prolific
)

stopifnot(
  nrow(summary) == 1L,
  "cameraAttempt" %in% names(summary),
  "Pavlovia session ID" %in% names(summary)
)

cat("summaryTableCameraAttempt tests passed\n")
