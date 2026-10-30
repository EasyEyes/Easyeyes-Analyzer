# Run: Rscript tests/cameraAttemptSummary.test.R
source("R/report/summary_table.R")

latest <- summarize_camera_attempts(list(
  tibble::tibble(
    participant = "P",
    date = "D",
    cameraAttemptJSON = '{"phase":"permission","outcome":null}'
  ),
  tibble::tibble(
    participant = "P",
    date = "D",
    cameraAttemptJSON = '{"phase":"completed","outcome":{"type":"selected"}}'
  )
))
stopifnot(
  nrow(latest) == 1L,
  latest$cameraAttempt[[1]] == '{"phase":"completed","outcome":{"type":"selected"}}'
)

empty <- summarize_camera_attempts(list())
stopifnot(
  nrow(empty) == 0L,
  identical(names(empty), c("participant", "date", "cameraAttempt"))
)

# The production join must also work for archives with no camera attempts.
empty <- empty %>% rename("Pavlovia session ID" = "participant")
joined <- tryCatch(
  tibble::tibble(`Pavlovia session ID` = "P", date = "D") %>%
    left_join(empty, by = c("Pavlovia session ID", "date")) %>%
    mutate(cameraAttempt = ifelse(is.na(cameraAttempt), "", cameraAttempt)),
  error = function(e) e
)
stopifnot(!inherits(joined, "error"), joined$cameraAttempt[[1]] == "")

without_column <- collect_camera_attempt(
  tibble::tibble(participant = "P", date = "D")
)
stopifnot(is.null(without_column))

# Versioned RC fixture remains intact through the Analyzer aggregation path.
fixture <- read.csv(
  "../remote-calibrator/test/fixtures/camera-attempt-v1.csv",
  stringsAsFactors = FALSE
)
attempt <- collect_camera_attempt(fixture)
summary <- summarize_camera_attempts(list(attempt))
parsed <- jsonlite::fromJSON(summary$cameraAttempt)
stopifnot(
  parsed$version == 1,
  parsed$phase == "permission",
  is.null(parsed$outcome),
  parsed$cameras$name[[1]] == "HP True Vision FHD Camera"
)
