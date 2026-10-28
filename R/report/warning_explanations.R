# Warning-name → explanation map from the checked-in CSV.
# Same name/explanation format as the Error Table; seeded from Threshold's
# easyeyes-warning-table.tsv.

WARNING_EXPLANATIONS_PATH <- "data/warning_explanations.csv"

load_warning_explanations <- function(path = WARNING_EXPLANATIONS_PATH) {
  load_error_explanations(path)
}

warning_explanations_json <- function(path = WARNING_EXPLANATIONS_PATH) {
  jsonlite::toJSON(load_warning_explanations(path), auto_unbox = TRUE, null = "null")
}
