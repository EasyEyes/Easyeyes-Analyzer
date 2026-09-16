# Run from the repo root: Rscript scripts/test_languages.R [archive1.zip ...]
# Without archives, runs synthetic regressions. With archives, also exercises
# the same combined-upload pipeline and Languages Shiny module as the app.
options(dplyr.summarise.inform = FALSE)
suppressPackageStartupMessages({
  library(shiny); library(dplyr); library(stringr); library(readr)
  library(tidyr); library(ggplot2); library(lubridate); library(emojifont)
})
source("R/utils/logger.R")
source("R/constant.R")
source("R/plot/simulatedRSVP.R")
source("R/preprocess.R")
source("R/utils/utility.R")
source("R/threshold_and_warning.R")
source("R/report/random_rgb.R")
source("R/report/summary_table.R")
source("R/report/participant_info.R")
source("R/report/prolific.R")
source("R/utils/language.R")
source("R/plot/language_plots.R")
source("R/server/languages_server.R")
init_logger(enabled = FALSE)

stopifnot(identical(detect_language(c(" AR ", "fa", "ur", "study_ar", "en", NA)),
                    c("ar", "fa", "ur", NA_character_, NA_character_, NA_character_)))
make_session <- function(experiment, participant, id, code) {
  tibble(experiment, participant, ProlificParticipantID = id,
         `_language` = c(NA_character_, code, NA_character_))
}
raw <- list(make_session("study1", "s1", "person1", "ar"),
            make_session("study2", "s2", "person1", "ar"),
            make_session("study1", "s3", "person2", "ar"),
            make_session("study1", "s4", "person3", "fa"),
            make_session("misleading_ur", "s5", "person4", NA_character_))
meta <- language_session_metadata(raw)
stopifnot(nrow(meta) == 5L, sum(meta$language == "ar", na.rm = TRUE) == 3L,
          is.na(meta$language[meta$participant == "s5"]),
          nrow(language_session_metadata(c(raw, raw))) == 5L)
reading <- tibble(experiment = c("study1", "study2", "study1", "study1", "study1"),
                  participant = c("s1", "s2", "s3", "s4", "s1"),
                  font = c("A", "A", "A", "A", "B"),
                  wordPerMin = c(100, 400, 800, 100, 50), CQAccuracy = 80)
d <- list(reading = add_language_column(reading, meta))
prepared <- prepare_language_reading_speed(d)
stopifnot(nrow(prepared) == 4L,
          abs(prepared$value[prepared$participant == "prolific:person1" & prepared$font == "A"] - 200) < 1e-8)
p <- plot_language_reading_speed(d)
a <- p$data %>% filter(language == "ar", font == "A")
stopifnot(a$n == 2L, abs(a$mean - 400) < 1e-8,
          abs(a$lower - 200) < 1e-8, abs(a$upper - 800) < 1e-8,
          all(is.na(p$data$lower[p$data$n == 1L])),
          identical(p$scales$get_scales("colour")$labels,
                    c("ar (Arabic), Average N=1.5", "fa (Persian), Average N=0.5", "ur (Urdu), Average N=0")),
          identical(unname(p$scales$get_scales("colour")$map(SUPPORTED_LANGUAGES)), unname(LANGUAGE_COLORS)))
stopifnot(identical(levels(p$data$language), SUPPORTED_LANGUAGES))
geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
stopifnot("GeomLinerange" %in% geoms, !"GeomErrorbar" %in% geoms)
dodge_widths <- vapply(p$layers, function(l) {
  if (inherits(l$position, "PositionDodge")) l$position$width else NA_real_
}, numeric(1))
stopifnot(sum(!is.na(dodge_widths)) == 3L, unique(dodge_widths[!is.na(dodge_widths)]) == 0.1)
stopifnot(all(prepare_language_reading_proportion_correct(d)$value == 0.8))
# Invalid values and unsupported languages yield an explicit empty-data state.
bad <- reading
bad$wordPerMin <- c(NA, Inf, 0, -1, NaN)
stopifnot(is.null(plot_language_reading_speed(list(reading = add_language_column(bad, meta)))))
stopifnot(is.null(plot_language_reading_speed(list(reading = reading))))
# Read both question layouts, deduplicate identical exported questions, and
# preserve the correct-answer field for comprehension scoring.
qa <- tibble(experiment = "study", participant = "s1", block = 1,
             block_condition = "1_1", conditionName = "beauty-Naskh", blockShuffleGroups2 = "",
             questionAndAnswerNickname = "BTY", questionAndAnswerQuestion = "Beauty?",
             questionAndAnswerResponse = "5", questionAndAnswerCorrectAnswer = "",
             questionAndAnswerNickname01 = "BTY", questionAndAnswerQuestion01 = "Beauty?",
             questionAndAnswerResponse01 = "5", questionAndAnswerCorrectAnswer01 = "",
             questionAndAnswerNickname02 = "FMLRTY", questionAndAnswerQuestion02 = "Familiarity?",
             questionAndAnswerResponse02 = "6", questionAndAnswerCorrectAnswer02 = "")
expanded <- session_question_answers(qa)
stopifnot(nrow(expanded) == 2L, setequal(expanded$questionAndAnswerNickname, c("BTY", "FMLRTY")),
          nrow(score_reading_comprehension(expanded)) == 0L,
          identical(comparison_rating_font(c("CMFRT-Naskhl", "beauty-Naskh", "CMFRT-NotoNastaliqUrdu")),
                    c("NotoNaskhArabic-Regular.ttf", "NotoNaskhArabic-Regular.ttf", "NotoNastaliqUrdu-Regular.woff2")))
cat("PASS language metadata, repeat participants, average N, geometric SE, missing data, numbered Q&A\n")

paths <- commandArgs(trailingOnly = TRUE)
if (length(paths)) {
  paths <- normalizePath(paths, mustWork = TRUE)
  # Shiny stores uploads under temporary paths without browser filename suffixes.
  upload_dir <- tempfile("languages-upload-")
  dir.create(upload_dir)
  uploaded <- file.path(upload_dir, paste0(seq_along(paths), ".zip"))
  stopifnot(all(file.copy(paths, uploaded)))
  files <- data.frame(name = basename(paths), datapath = uploaded)
  stopifnot(is.null(check_file_names(files)))
  parsed <- read_files(files)
  sessions <- generate_summary_table(parsed$data_list, parsed$stairs, parsed$pretest, parsed$prolific)
  thresholds <- function(cq = 80, speed = 10000, conditions = NULL, short = character()) {
    generate_threshold(parsed$data_list, parsed$summary_list, parsed$df,
                       parsed$pretest, parsed$stairs, parsed$prolific, "all", "all", 10, 0, 0.2,
                       conditions, speed, cq, sessions_summary = sessions,
                       shortRulerParticipantIDs = short)
  }
  th <- thresholds()
  metadata <- language_session_metadata(parsed$data_list)
  annotated <- lapply(th, add_language_column, metadata = metadata)
  names <- c("reading_speed", "reading_proportion_correct", "rsvp_speed", "crowding",
             "comfort", "beauty", "familiarity")
  output_dir <- Sys.getenv("LANGUAGES_TEST_PLOT_DIR", tempfile("languages-plots-"))
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  for (name in names) {
    p <- get(paste0("plot_language_", name))(annotated)
    stopifnot(inherits(p, "ggplot"), setequal(as.character(p$data$language), SUPPORTED_LANGUAGES),
              all(p$data$n > 0), all(is.finite(p$data$mean)),
              identical(unname(p$scales$get_scales("colour")$map(SUPPORTED_LANGUAGES)), unname(LANGUAGE_COLORS)))
    log_axis <- name %in% c("reading_speed", "rsvp_speed", "crowding")
    stopifnot(identical(!is.null(p$scales$get_scales("y")), log_axis))
    ggsave(file.path(output_dir, paste0(name, ".png")), p, width = 8, height = 6, dpi = 120)
    cat("PASS", name, "|", paste(p$scales$get_scales("colour")$labels, collapse = "; "), "\n")
  }
  # Comprehension and speed filters change the same data consumed by Languages.
  relaxed <- thresholds(cq = 0)
  stopifnot(nrow(relaxed$reading) > nrow(th$reading))
  capped <- thresholds(cq = 0, speed = 150)
  stopifnot(nrow(capped$reading) < nrow(relaxed$reading), all(capped$reading$wordPerMin <= 150))
  # Reactive renderers and Download All use the annotated data, including after
  # a filter invalidates the shared threshold reactive.
  testServer(languagesTabServer, args = list(df_list = reactiveVal(th),
             experiment_names = reactive(parsed$experiment), fileType = reactive("png"),
             data_list = reactive(parsed$data_list)), {
    stopifnot(length(downloadSpecs()) == 7L)
    for (id in c("readingSpeedPlot", "readingProportionCorrectPlot", "rsvpSpeedPlot",
                 "crowdingPlot", "comfortPlot", "beautyPlot", "familiarityPlot")) {
      stopifnot(!is.null(output[[id]]))
    }
    before <- readingSpeedPlot()$data
    df_list(relaxed)
    session$flushReact()
    stopifnot(!identical(before, readingSpeedPlot()$data))
    df_list(list(reading = tibble()))
    session$flushReact()
    stopifnot(length(downloadSpecs()) == 0L)
  })
  unlink(upload_dir, recursive = TRUE)
  cat("PASS combined archive upload, all 7 Shiny outputs, Download All, and filter reactivity\n")
  cat("Plot previews:", normalizePath(output_dir), "\n")
}
