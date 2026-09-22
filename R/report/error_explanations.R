# Error-name → explanation map from the checked-in CSV.
# Source sheet: https://docs.google.com/spreadsheets/d/1x65NjykMm-XUOz98Eu_oo6ON2xspm_h0Q0M2u6UGtug
# Tab gid=472289738 (name, explanation)
# Kept in sync by .github/workflows/sync-error-explanations.yml

ERROR_EXPLANATIONS_PATH <- "data/error_explanations.csv"

load_error_explanations <- function(path = ERROR_EXPLANATIONS_PATH) {
  if (!file.exists(path)) {
    return(list())
  }
  df <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(df) || nrow(df) == 0) {
    return(list())
  }
  names(df) <- tolower(names(df))
  if (!all(c("name", "explanation") %in% names(df))) {
    return(list())
  }
  df$name <- trimws(as.character(df$name))
  df$explanation <- as.character(df$explanation)
  keep <- !is.na(df$name) & df$name != "" & !is.na(df$explanation) & df$explanation != ""
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0) {
    return(list())
  }
  stats::setNames(as.list(df$explanation), df$name)
}

error_explanations_json <- function(path = ERROR_EXPLANATIONS_PATH) {
  jsonlite::toJSON(load_error_explanations(path), auto_unbox = TRUE, null = "null")
}
