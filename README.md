# Easyeyes-Analyzer

Dashboard for monitoring EasyEyes experiments and analyzing data.

Live app: https://easyeyes.shinyapps.io/easyeyes_app/

## Project layout

```
server.R, ui.R          # Shiny entry points
R/
  load_app.R            # server-side module loader
  load_ui.R             # UI tab loader
  constant.R            # shared constants
  preprocess.R          # file ingest
  threshold_and_warning.R
  utils/                # helpers (logger, formSpree, utility, …)
  report/               # session summary tables
  plot/                 # plot builders
  server/               # tab server modules
  ui/                   # tab UI definitions
www/                    # static assets (JS, CSS)
dev/
  notebooks/            # analysis notebooks
  reports/              # R Markdown reports
local-test-data/        # local test zips (gitignored)
renv.lock               # pinned package versions
```

## Environment (renv)

This project uses [renv](https://rstudio.github.io/renv/) for reproducible R package management. Opening the project in RStudio (or starting R from the repo root) activates renv automatically via `.Rprofile`.

First-time setup:

```r
install.packages("renv")   # if needed
renv::restore()            # install packages from renv.lock
```

After pulling changes that update `renv.lock`, run `renv::restore()` again. To record new or upgraded packages after installing them:

```r
renv::snapshot()
```

## Run locally

Open the project in RStudio and run the app, or:

```r
renv::restore()   # ensure dependencies are installed
shiny::runApp()
```
