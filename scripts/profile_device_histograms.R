#!/usr/bin/env Rscript
# Profile why Plots-tab device histograms (screenWidthPx, …) were 10–20s
# while other histograms were <1s.
#
# Root cause: ggplot objects built inside the Shiny `histograms()` reactive
# capture that frame as plot_env (includes files/data_list). PNG theming
# calls unserialize(serialize(plot)), which copies the fat env.
#
# Run: Rscript --vanilla scripts/profile_device_histograms.R

suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
  library(tibble)
  library(ggpp)
  library(ragg)
})

args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args_all, value = TRUE)
root <- if (length(file_arg) >= 1) {
  normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), ".."))
} else {
  normalizePath(".")
}
setwd(root)
source("R/constant.R")
source("R/plot/histogram.R")
get_short_experiment_name <- function(x) "exp"
add_experiment_title <- function(plot, experiment_name) {
  plot + labs(title = get_short_experiment_name(experiment_name))
}
source("R/utils/utility.R")

set.seed(1)
# Fake archive tables hanging off a reactive-like environment.
fat_archives <- lapply(seq_len(40), function(i) {
  data.frame(
    participant = paste0("p", i),
    level = rnorm(8000),
    questMeanAtEndOfTrialsLoop = rnorm(8000),
    stringsAsFactors = FALSE
  )
})

summary_like <- tibble(
  `Pavlovia session ID` = rep(paste0("p", seq_len(120)), each = 8),
  screenWidthPx = rep(runif(120, 1000, 3500), each = 8),
  screenWidthCm = rep(runif(120, 20, 40), each = 8) + rnorm(120 * 8, 0, 0.05),
  GB = rep(sample(c(4, 8, 16, 32), 120, TRUE), each = 8),
  devicePixelRatio = rep(sample(c(1, 2, 3), 120, TRUE), each = 8),
  cores = rep(sample(c(4, 8, 12), 120, TRUE), each = 8)
)

bench_serialize_render <- function(p, label) {
  t0 <- proc.time()[["elapsed"]]
  raw <- serialize(p, NULL)
  t1 <- proc.time()[["elapsed"]]
  invisible(unserialize(raw))
  t2 <- proc.time()[["elapsed"]]
  invisible(render_plots_display_png(
    p + hist_theme,
    width_in = 3.5,
    height_in = 3.5,
    disp_w = 280,
    disp_h = 280,
    png_theme_profile = "histogram",
    limitsize = FALSE
  ))
  t3 <- proc.time()[["elapsed"]]
  env_objs <- tryCatch(ls(p$plot_env), error = function(e) character())
  cat(sprintf(
    "%s\n  plot_env objects: %s\n  serialize: %.3fs (%.1f MB)\n  unserialize: %.3fs\n  render_png: %.3fs\n  total: %.3fs\n",
    label,
    paste(env_objs, collapse = ", "),
    t1 - t0,
    length(raw) / 1e6,
    t2 - t1,
    t3 - t2,
    t3 - t0
  ))
}

cat("=== AFTER FIX: append_hist_list builds plots in child functions ===\n")
# Mimic reactive frame that holds fat archives, then call append_hist_list.
out <- local({
  files_data_list <- fat_archives
  df_list_big <- list(acuity = fat_archives[[1]])
  pd <- participant_device_for_hists(summary_like)
  cat("participant_device rows:", nrow(pd),
      "(unique participants expected ~120)\n")
  append_hist_list(pd, NULL, list(), list(), "exp")
})

for (i in seq_along(out$plotList)) {
  suppressWarnings(bench_serialize_render(out$plotList[[i]], out$fileNames[[i]]))
}

cat("\n=== ANTI-PATTERN: ggplot built directly in fat frame ===\n")
p_bad <- local({
  files_data_list <- fat_archives
  data <- tibble(participant = paste0("p", 1:100), screenWidthPx = runif(100, 1000, 3000))
  ggplot(data, aes(x = screenWidthPx)) +
    geom_histogram(color = NA, fill = "gray80") +
    histogram_stats_text_npc(1, 1, 100) +
    labs(subtitle = "built in fat frame")
})
suppressWarnings(bench_serialize_render(p_bad, "inline-in-fat-frame"))
