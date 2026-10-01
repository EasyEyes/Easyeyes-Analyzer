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
- *Uploading…* shows the browser sending the files to the server, with a percentage and the number of bytes sent.
- *Reading…* is shown while R unzips and reads the files. During this step R reports its own progress.

**2. Plots progress window** (`www/plotsPageProgress.js` and `R/server/plots_tab_server.R`). This is a small floating panel on the Plots tab. You can drag it or close it, and it never blocks the page. Plots that have already finished can be viewed and downloaded while later ones are still being made.

- **When it opens.** The browser opens it as soon as you switch to the Plots tab after an upload. It does not wait for R to respond. The timer also starts at the moment of the click, so the final *Done. m:ss* is the total wait you saw: R computing, network transfer, and the browser loading images.
- **"R server" line.** This is what R reports it is working on:
  - *Waiting for R server …* means R has not answered yet. If the note says R is still finishing an earlier step, R is busy with work that started before you switched tabs, such as computing thresholds after the upload. R runs one step at a time, so it can only start the Plots tab once that step ends.
  - *Preparing plots … / Waiting for data …* means R is still building the analysis data.
  - *Plotting correlation matrices / histograms / violins / font comparisons / scatter diagrams / age diagrams … N / M* means R is making plot images one at a time, in page order.
  - *Paused until you return to the Plots tab* means R only makes Plots-tab images while that tab is open, so leaving the tab pauses it.
- **"Browser" line.** This is what your browser is doing:
  - *Waiting for R …* means the browser has nothing to show yet.
  - *Waiting for R to send N plots …* means R is still making those plots.
  - *Loading N plot images …* means R has finished those images and they are downloading or being drawn in your browser.
  - *All received plots are shown.* means the browser has caught up with R.
- **Done.** The window shows *Done* only when R has finished every plot and the browser has finished loading every image. Going back to the Plots tab later reuses the cached images and does not restart the window. A new upload starts a new run.

**Why the first Plots visit can take a minute or two (especially on shinyapps.io).** Each user session runs in a single R process. After the session table appears, R is still computing the threshold data (`generate_threshold`, run once per upload or filter change). Only after that can R start the plots, and then it renders each PNG. Network speed and the shinyapps.io instance size add to the time. The window cannot make R faster, but it shows which of these steps you are waiting on.

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
