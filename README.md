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

## How progress is reported

The app reports progress in two places.

**1. Upload window** (`www/fileUploadProgress.js`). This is a modal that opens when you choose files.
- *Compressing…* (optional) recompresses STORE zips in the browser before upload.
- *Uploading…* shows the browser sending the files to the server, with a percentage and bytes sent.
- *Reading…* is shown while R unzips and reads the files. During this step R reports its own progress.
- *Processing…* replaces Reading once reading is essentially done. An animated bar runs while R builds the Sessions summary table, so the UI does not look frozen at “Reading 100%”. The modal closes when that table is ready.

**2. Plots progress window** (`www/plotsPageProgress.js` and `R/server/plots_tab_server.R`). This is a small floating panel on the Plots tab. You can drag it or close it, and it never blocks the page. Plots that have already finished can be viewed and downloaded while later ones are still being made.

Display while running:

```
Plots 0:38
Plotting histograms …
22/30
```

- **When it opens.** The browser opens it as soon as you switch to the Plots tab after an upload. The timer in the title starts at that click, so *Done. m:ss* is the total wait you saw.
- **Stage / count.** R reports the current plotting stage and `N / M` progress (correlation matrices, histograms, violins, font comparisons, scatters, age plots). If R has not answered yet, the panel shows a short “Waiting for R …” / “Starting plots …” line.
- **Done.** Shown only when R has finished every plot and the browser has finished loading every image. Going back to the Plots tab later reuses cached images. A new upload starts a new run.

**Why the first Plots visit can take a minute or two (especially on shinyapps.io).** Each user session runs in a single R process. After the session table appears, R is still computing the threshold data (`generate_threshold`, run once per upload or filter change). Only after that can R start the plots, and then it renders each PNG. Network speed and the shinyapps.io instance size add to the time. The window cannot make R faster, but it shows which stage you are in.

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
