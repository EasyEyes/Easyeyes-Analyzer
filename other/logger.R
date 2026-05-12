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

create_app_profiler <- function(name = "session", enabled = TRUE) {
  state <- new.env(parent = emptyenv())
  state$name <- name
  state$enabled <- enabled
  state$start <- NULL
  state$last <- NULL

  now <- function() proc.time()[["elapsed"]]

  emit <- function(message_text) {
    if (!isTRUE(state$enabled)) return(invisible(NULL))
    msg <- paste0("[PROFILE ", format(Sys.time(), "%H:%M:%S"), "] ", message_text)
    message(msg)
    if (.easyeyes_log$enabled && !is.null(.easyeyes_log$file)) {
      cat(msg, "\n", file = .easyeyes_log$file, append = TRUE)
    }
    invisible(NULL)
  }

  list(
    reset = function(label = "start") {
      t <- now()
      state$start <- t
      state$last <- t
      emit(paste0(state$name, " | START | ", label))
      invisible(NULL)
    },
    mark = function(label, detail = NULL) {
      if (is.null(state$start)) {
        state$start <- now()
        state$last <- state$start
      }
      t <- now()
      total <- t - state$start
      delta <- t - state$last
      state$last <- t
      detail_text <- if (is.null(detail) || detail == "") "" else paste0(" | ", detail)
      emit(sprintf(
        "%s | +%.3fs total %.3fs | %s%s",
        state$name,
        delta,
        total,
        label,
        detail_text
      ))
      invisible(NULL)
    },
    is_enabled = function() isTRUE(state$enabled)
  )
}

app_profile_time <- function(profiler, label, expr) {
  if (is.null(profiler) || !isTRUE(profiler$is_enabled())) {
    return(eval(substitute(expr), parent.frame()))
  }

  start <- proc.time()[["elapsed"]]
  ok <- FALSE
  on.exit({
    duration <- proc.time()[["elapsed"]] - start
    status <- if (ok) "complete" else "failed"
    profiler$mark(paste0(label, " ", status), sprintf("duration %.3fs", duration))
  }, add = TRUE)

  value <- eval(substitute(expr), parent.frame())
  ok <- TRUE
  value
}
