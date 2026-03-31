.easyeyes_log <- new.env(parent = emptyenv())
.easyeyes_log$file <- NULL
.easyeyes_log$enabled <- FALSE

init_logger <- function(enabled = FALSE, app_dir = getwd()) {
  .easyeyes_log$enabled <- enabled
  if (enabled) {
    log_dir <- file.path(app_dir, ".logs")
    dir.create(log_dir, showWarnings = FALSE, recursive = TRUE)
    .easyeyes_log$file <- file.path(log_dir, "easyeyes.log")
    cat(paste0("=== EasyEyes Analyzer Log ===\nStarted: ", Sys.time(), "\n\n"),
        file = .easyeyes_log$file)
    message("Logger writing to: ", .easyeyes_log$file)
  }
}

log_info <- function(...) {
  if (!.easyeyes_log$enabled) return(invisible(NULL))
  msg <- paste0("[INFO  ", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n", file = .easyeyes_log$file, append = TRUE)
}

log_debug <- function(...) {
  if (!.easyeyes_log$enabled) return(invisible(NULL))
  msg <- paste0("[DEBUG ", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n", file = .easyeyes_log$file, append = TRUE)
}

log_warn <- function(...) {
  if (!.easyeyes_log$enabled) return(invisible(NULL))
  msg <- paste0("[WARN  ", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n", file = .easyeyes_log$file, append = TRUE)
}

log_error <- function(...) {
  if (!.easyeyes_log$enabled) return(invisible(NULL))
  msg <- paste0("[ERROR ", format(Sys.time(), "%H:%M:%S"), "] ", paste(..., collapse = ""))
  cat(msg, "\n", file = .easyeyes_log$file, append = TRUE)
}
