# Headless check that each local-test-data archive survives the same
# preprocess + summary + threshold path the Shiny upload handler uses.
#
# Invoked by scripts/test_local_data.sh (do not run with Rscript --vanilla;
# renv needs .Rprofile).

options(dplyr.summarise.inform = FALSE)

# Progress goes to stderr so it shows up immediately under Rscript (stdout is fully buffered).
say <- function(...) {
  cat(..., file = stderr(), sep = "")
}

load_pipeline <- function() {
  say("Loading packages and R modules...\n")
  suppressPackageStartupMessages({
    library(dplyr)
    library(stringr)
    library(readr)
    library(tibble)
    library(tidyr)
    library(data.table)
    library(lubridate)
    library(readxl)
    library(foreach)
    library(ggplot2)
    library(DT)
  })
  source("R/utils/logger.R")
  source("R/constant.R")
  source("R/plot/simulatedRSVP.R")
  source("R/utils/utility.R")
  source("R/preprocess.R")
  source("R/threshold_and_warning.R")
  source("R/report/random_rgb.R")
  source("R/report/summary_table.R")
  source("R/report/participant_info.R")
  source("R/report/prolific.R")
  init_logger(enabled = FALSE)
  say("Pipeline loaded.\n")
  invisible(NULL)
}

shiny_like_file <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  list(
    name = basename(path),
    data = path,
    datapath = path
  )
}

strip_html <- function(x) {
  x <- gsub("<[^>]+>", " ", x)
  x <- gsub("&nbsp;", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

timed <- function(label, expr) {
  t0 <- proc.time()[["elapsed"]]
  value <- force(expr)
  elapsed <- proc.time()[["elapsed"]] - t0
  list(value = value, elapsed = elapsed, label = label)
}

fmt_secs <- function(s) sprintf("%.1fs", s)

test_one <- function(path) {
  file <- shiny_like_file(path)
  out <- list(
    file = basename(path),
    ok = FALSE,
    sessions = NA_integer_,
    quest = NA_integer_,
    crowding = NA_integer_,
    reading = NA_integer_,
    qa = NA_integer_,
    step = "check_file_names",
    error = NULL,
    elapsed = 0
  )
  t_all <- proc.time()[["elapsed"]]

  check <- check_file_names(file)
  if (!is.null(check)) {
    out$error <- paste("check_file_names:", strip_html(check))
    out$elapsed <- proc.time()[["elapsed"]] - t_all
    return(out)
  }

  out$step <- "read_files"
  say("    read_files...\n")
  parsed_t <- timed("read_files", read_files(file, progress = function(value, message, detail) {
    pct <- if (is.null(value) || !is.finite(value)) "" else sprintf(" %d%%", round(100 * value))
    msg <- paste(c(message, detail), collapse = " — ")
    say(sprintf("      %s%s\n", msg, pct))
  }))
  parsed <- parsed_t$value
  say(sprintf("    read_files: OK (%d session(s), %s)\n",
              length(parsed$data_list), fmt_secs(parsed_t$elapsed)))

  if (length(parsed$data_list) == 0) {
    out$ok <- TRUE
    out$sessions <- 0L
    out$quest <- 0L
    out$crowding <- 0L
    out$reading <- 0L
    out$qa <- 0L
    out$step <- "done"
    out$elapsed <- proc.time()[["elapsed"]] - t_all
    say("    generate_summary_table: skipped (no experiment sessions)\n")
    say("    generate_threshold: skipped (no experiment sessions)\n")
    return(out)
  }

  out$step <- "generate_summary_table"
  say("    generate_summary_table...\n")
  summary_t <- timed(
    "generate_summary_table",
    generate_summary_table(
      parsed$data_list,
      parsed$stairs,
      parsed$pretest,
      parsed$prolific
    )
  )
  sessions_summary <- summary_t$value
  say(sprintf("    generate_summary_table: OK (%d row(s), %s)\n",
              NROW(sessions_summary), fmt_secs(summary_t$elapsed)))

  short_ids <- short_ruler_participant_ids(sessions_summary, 0)

  out$step <- "generate_threshold"
  say("    generate_threshold...\n")
  th_t <- timed(
    "generate_threshold",
    generate_threshold(
      parsed$data_list,
      parsed$summary_list,
      parsed$df,
      parsed$pretest,
      parsed$stairs,
      parsed$prolific,
      "all",
      "all",
      10,
      0,
      0.2,
      NULL,
      10000,
      80,
      sessions_summary = sessions_summary,
      shortRulerParticipantIDs = short_ids
    )
  )
  th <- th_t$value
  out$sessions <- length(parsed$data_list)
  out$quest <- if (!is.null(th$quest)) NROW(th$quest) else 0L
  out$crowding <- if (!is.null(th$crowding)) NROW(th$crowding) else 0L
  out$reading <- if (!is.null(th$reading)) NROW(th$reading) else 0L
  out$qa <- if (!is.null(th$QA)) NROW(th$QA) else 0L
  say(sprintf(
    "    generate_threshold: OK (quest=%d crowding=%d reading=%d QA=%d, %s)\n",
    out$quest, out$crowding, out$reading, out$qa, fmt_secs(th_t$elapsed)
  ))

  out$ok <- TRUE
  out$step <- "done"
  out$elapsed <- proc.time()[["elapsed"]] - t_all
  out
}

run_tests <- function(paths) {
  load_pipeline()
  say("\n")

  results <- vector("list", length(paths))
  for (i in seq_along(paths)) {
    path <- paths[[i]]
    say(sprintf("==> [%d/%d] %s\n", i, length(paths), basename(path)))
    results[[i]] <- tryCatch(
      test_one(path),
      error = function(e) {
        list(
          file = basename(path),
          ok = FALSE,
          sessions = NA_integer_,
          quest = NA_integer_,
          crowding = NA_integer_,
          reading = NA_integer_,
          qa = NA_integer_,
          step = "error",
          error = conditionMessage(e),
          elapsed = NA_real_
        )
      }
    )
    r <- results[[i]]
    if (isTRUE(r$ok)) {
      say(sprintf("PASS %s (%s)\n\n", r$file, fmt_secs(r$elapsed)))
    } else {
      say(sprintf("FAIL %s at %s: %s\n\n", r$file, r$step, r$error))
    }
  }

  n_ok <- sum(vapply(results, function(r) isTRUE(r$ok), logical(1)))
  n_fail <- length(results) - n_ok
  say("----------------------------------------\n")
  say(sprintf("%d passed, %d failed, %d total\n", n_ok, n_fail, length(results)))
  if (n_fail > 0) {
    say("\nFailures:\n")
    for (r in results) {
      if (!isTRUE(r$ok)) {
        say(sprintf("  - %s (%s): %s\n", r$file, r$step, r$error))
      }
    }
  }
  invisible(n_fail == 0)
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Pass one or more file paths to test.")
}
missing <- args[!file.exists(args)]
if (length(missing) > 0) {
  stop("File(s) not found:\n  ", paste(missing, collapse = "\n  "))
}
ok <- run_tests(args)
quit(save = "no", status = if (isTRUE(ok)) 0L else 1L)
