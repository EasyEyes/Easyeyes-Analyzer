# unmetNeeds → explanation map from the Parameter Glossary Google Sheet.
# Source sheet: https://docs.google.com/spreadsheets/d/1x65NjykMm-XUOz98Eu_oo6ON2xspm_h0Q0M2u6UGtug
# Tab gid=1287694458 (_s, EXPLANATION)
# Kept in sync by .github/workflows/sync-error-explanations.yml

UNMET_NEEDS_EXPLANATIONS_PATH <- "data/unmet_needs_explanations.csv"

load_unmet_needs_explanations <- function(path = UNMET_NEEDS_EXPLANATIONS_PATH) {
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
  # Accept either "_s" or "name", and "explanation" / "EXPLANATION"
  names_lower <- tolower(names(df))
  name_col <- if ("_s" %in% names(df)) {
    "_s"
  } else if ("name" %in% names_lower) {
    names(df)[match("name", names_lower)]
  } else {
    return(list())
  }
  expl_col <- if ("explanation" %in% names_lower) {
    names(df)[match("explanation", names_lower)]
  } else {
    return(list())
  }

  key <- trimws(as.character(df[[name_col]]))
  expl <- as.character(df[[expl_col]])
  keep <- !is.na(key) & key != "" & !startsWith(key, "__") &
    !is.na(expl) & expl != "" &
    # unmetNeeds cells list failed `_need*` requirements only
    grepl("^_need", key)
  key <- key[keep]
  expl <- expl[keep]
  if (length(key) == 0) {
    return(list())
  }
  stats::setNames(as.list(expl), key)
}

unmet_needs_explanations_json <- function(path = UNMET_NEEDS_EXPLANATIONS_PATH) {
  jsonlite::toJSON(load_unmet_needs_explanations(path), auto_unbox = TRUE, null = "null")
}
