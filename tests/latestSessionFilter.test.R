# Run: Rscript tests/latestSessionFilter.test.R
source("R/report/summary_table.R")

# --- Contract: each Prolific participant ID collapses to that participant's
# latest session; rows without a Prolific ID (participant unknown) all stay.
df <- tibble::tibble(
  `Prolific participant ID` = c("A", "A", "B", "", NA, "B"),
  date = c(
    "2026-01-01", "2026-01-02", "2026-01-02",
    "2026-01-03", "2026-01-04", "2026-01-01"
  ),
  `Pavlovia session ID` = c("s1", "s2", "s3", "s4", "s5", "s0")
)

out <- latest_sessions_per_participant(df)

stopifnot(
  nrow(out) == 4L,
  # Output stays in table order and keeps exactly the input columns.
  identical(out$`Pavlovia session ID`, c("s4", "s2", "s3", "s5")),
  identical(names(out), names(df))
)

# --- A session occupies one row per block/condition: the latest session must
# keep ALL its rows, not just one.
df_multi <- tibble::tibble(
  `Prolific participant ID` = c("A", "A", "A"),
  date = c("2026-01-01", "2026-01-02", "2026-01-02"),
  `Pavlovia session ID` = c("s1", "s2", "s2"),
  condition = c("c1", "c1", "c2")
)
out_multi <- latest_sessions_per_participant(df_multi)
stopifnot(
  nrow(out_multi) == 2L,
  setequal(out_multi$condition, c("c1", "c2"))
)

# --- Missing key columns pass through unchanged instead of erroring.
df_partial <- tibble::tibble(
  `Prolific participant ID` = c("A", "A"),
  x = 1:2
)
stopifnot(identical(latest_sessions_per_participant(df_partial), df_partial))

# --- Same-date tie: the higher Pavlovia session ID counts as "latest".
df_tie <- tibble::tibble(
  `Prolific participant ID` = c("A", "A"),
  date = c("2026-01-01", "2026-01-01"),
  `Pavlovia session ID` = c("s6", "s7")
)
stopifnot(
  identical(
    latest_sessions_per_participant(df_tie)$`Pavlovia session ID`,
    "s7"
  )
)

# --- "Latest" is chronological, not alphabetical: the table displays dates
# as "Jun 01, 2026, 09:00:00", and %b months (Apr, Aug, Dec, …) do not sort
# chronologically as strings.
df_months <- tibble::tibble(
  `Prolific participant ID` = c("A", "A"),
  date = c("May 31, 2026, 20:00:00", "Jun 01, 2026, 09:00:00"),
  `Pavlovia session ID` = c("s1", "s2")
)
stopifnot(
  identical(
    latest_sessions_per_participant(df_months)$`Pavlovia session ID`,
    "s2"
  )
)

# --- Year dominates month: Sep 2025 is older than Mar 2026.
df_years <- tibble::tibble(
  `Prolific participant ID` = c("A", "A"),
  date = c("Sep 08, 2025, 10:00:00", "Mar 10, 2026, 09:00:00"),
  `Pavlovia session ID` = c("s1", "s2")
)
stopifnot(
  identical(
    latest_sessions_per_participant(df_years)$`Pavlovia session ID`,
    "s2"
  )
)

# --- No Prolific column at all: pass through unchanged.
df_no_id <- tibble::tibble(x = 1:3)
stopifnot(identical(latest_sessions_per_participant(df_no_id), df_no_id))

cat("latestSessionFilter tests passed\n")
