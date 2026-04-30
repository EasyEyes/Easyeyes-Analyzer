#!/usr/bin/env Rscript
# Reproducible Shinylive export for Easyeyes-Analyzer.
#
# Why this script exists:
# - Exporting from the full repo root pulls in renv/library (PDFs, binaries, etc.),
#   which can produce a huge, invalid UTF-8 app.json and blank pages in the browser.
# - Re-exporting into an existing shinylive-site can fail if shinylive assets drift;
#   we remove the destination first.
# - Loads renv from the real project so dependency discovery matches your lockfile.
# - Staging uses tempdir(), not a folder under the repo: if the staging path is
#   listed in .gitignore, renv::dependencies() skips it and shinylive bundles zero
#   wasm packages (empty shinylive/webr/packages/*.tgz).
#
# Usage (recommended: run from project root):
#   cd /path/to/Easyeyes-Analyzer
#   Rscript export_shinylive.R
#
# Or pass the project root explicitly:
#   Rscript export_shinylive.R /path/to/Easyeyes-Analyzer
#
# Or set an environment variable:
#   EASYEYES_PROJECT=/path/to/Easyeyes-Analyzer Rscript export_shinylive.R
#
# Requirements:
# - macOS/Linux: `rsync` on PATH (standard on macOS).
# - R packages: renv, shinylive, jsonlite (and everything shinylive needs to scan the app).
# - Network on first export: Shinylive web assets (~hundreds of MB) and wasm .tgz packages
#   are downloaded from GitHub / repo.r-wasm.org unless already cached.
#
# Shinylive web assets vs wasm bundling:
# - CRAN shinylive 0.4.x pins *legacy* assets (<= 0.7.0) that cannot bundle wasm packages.
#   export() then writes an almost-empty shinylive/webr/packages/ and the browser preload
#   tries to download every dependency (often failing with "preload error: Downloading webR package: …").
# - If SHINYLIVE_ASSETS_VERSION is unset, this script sets it to a current assets line (>= 0.8)
#   so wasm binaries are included in the static site. Override by exporting the variable yourself.

resolve_project_root <- function() {
  # 1) First CLI argument
  trailing <- commandArgs(trailingOnly = TRUE)
  if (length(trailing) >= 1L && nzchar(trailing[[1L]])) {
    p <- normalizePath(trailing[[1L]], winslash = "/", mustWork = FALSE)
    if (dir.exists(p) && file.exists(file.path(p, "server.R"))) {
      return(p)
    }
    stop("First argument must be an existing directory containing server.R: ", trailing[[1L]])
  }

  # 2) Environment override
  env <- Sys.getenv("EASYEYES_PROJECT", unset = "")
  if (nzchar(env)) {
    p <- normalizePath(env, winslash = "/", mustWork = FALSE)
    if (dir.exists(p) && file.exists(file.path(p, "server.R"))) {
      return(p)
    }
    stop("EASYEYES_PROJECT is set but is not a directory with server.R: ", env)
  }

  # 3) Directory containing this script, or its parent if script lives under other/
  ca <- commandArgs(FALSE)
  f <- grep("^--file=", ca, value = TRUE)
  if (length(f)) {
    script_path <- normalizePath(sub("^--file=", "", f[[1L]]), winslash = "/", mustWork = TRUE)
    candidates <- unique(c(
      dirname(script_path),
      dirname(dirname(script_path))
    ))
    for (p in candidates) {
      if (file.exists(file.path(p, "server.R")) && file.exists(file.path(p, "ui.R"))) {
        return(p)
      }
    }
  }

  # 4) Current working directory
  wd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  if (file.exists(file.path(wd, "server.R"))) {
    return(wd)
  }

  stop(
    "Cannot find project root (need server.R).\n",
    "  cd into the Easyeyes-Analyzer repo, or run:\n",
    "  Rscript export_shinylive.R /path/to/Easyeyes-Analyzer\n",
    "  EASYEYES_PROJECT=/path/to/Easyeyes-Analyzer Rscript export_shinylive.R"
  )
}

rsync_stage_app <- function(project_root, staging_dir) {
  if (!nzchar(Sys.which("rsync"))) {
    stop(
      "`rsync` not found on PATH. Install it or copy the exclusions logic into another tool.\n",
      "  On macOS, rsync is preinstalled."
    )
  }

  if (dir.exists(staging_dir)) {
    unlink(staging_dir, recursive = TRUE, force = TRUE)
  }
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)

  # Trailing slash on source: copy directory *contents* into staging_dir/
  src <- paste0(normalizePath(project_root, winslash = "/", mustWork = TRUE), "/")
  dst <- paste0(normalizePath(staging_dir, winslash = "/", mustWork = TRUE), "/")

  exclusions <- c(
    ".git/",
    ".Rproj.user/",
    "renv/",
    "shinylive-site/",
    "shinylive-appsrc/",
    ".logs/",
    "notebooks/",
    ".DS_Store",
    "*.Rproj"
  )

  args <- c(
    "-a",
    "--delete",
    unlist(lapply(exclusions, function(x) c("--exclude", x))),
    src,
    dst
  )

  status <- system2("rsync", args, stdout = TRUE, stderr = TRUE)
  exit_code <- attr(status, "status")
  if (is.null(exit_code)) exit_code <- 0L
  if (!identical(as.integer(exit_code), 0L)) {
    message(paste(status, collapse = "\n"))
    stop("rsync failed with exit code: ", exit_code)
  }

  if (!file.exists(file.path(staging_dir, "server.R"))) {
    stop("Staging directory is missing server.R after rsync: ", staging_dir)
  }
}

validate_app_json <- function(dest_dir) {
  app_json <- file.path(dest_dir, "app.json")
  if (!file.exists(app_json)) {
    stop("Export did not write app.json: ", app_json)
  }
  # Must be valid UTF-8 JSON (invalid bundle was the root cause of blank UI + JSON parse errors).
  jsonlite::fromJSON(app_json, simplifyVector = FALSE)
  message("OK: app.json parses as JSON (", file.info(app_json)$size, " bytes).")
}

# Default Shinylive *web assets* (not the R package version). Assets <= 0.7.0 skip wasm bundling.
DEFAULT_SHINYLIVE_ASSETS_VERSION <- "0.10.8"

ensure_shinylive_assets_for_wasm_bundle <- function() {
  cur <- Sys.getenv("SHINYLIVE_ASSETS_VERSION", unset = "")
  if (nzchar(cur)) {
    message("Using SHINYLIVE_ASSETS_VERSION=", cur, " (from environment).")
    return(invisible(NULL))
  }
  Sys.setenv(SHINYLIVE_ASSETS_VERSION = DEFAULT_SHINYLIVE_ASSETS_VERSION)
  message(
    "SHINYLIVE_ASSETS_VERSION was unset; using ", DEFAULT_SHINYLIVE_ASSETS_VERSION,
    " so WebAssembly R packages are bundled under shinylive/webr/packages/.\n",
    "  (Legacy assets <= 0.7.0 skip this step and the browser then fails preload package downloads.)\n",
    "  To use a different assets release, set SHINYLIVE_ASSETS_VERSION before running this script."
  )
  invisible(NULL)
}

validate_webr_packages_bundled <- function(dest_dir) {
  wasm_off <- Sys.getenv("SHINYLIVE_WASM_PACKAGES", unset = "")
  if (nzchar(wasm_off) && wasm_off %in% c("0", "FALSE", "false")) {
    message("Skipping webr/packages check (SHINYLIVE_WASM_PACKAGES disables wasm bundling).")
    return(invisible(NULL))
  }
  pkg_dir <- file.path(dest_dir, "shinylive", "webr", "packages")
  if (!dir.exists(pkg_dir)) {
    stop("Export is missing webr package dir (expected wasm bundle): ", pkg_dir)
  }
  tgz <- list.files(pkg_dir, pattern = "\\.tgz$", full.names = FALSE, recursive = TRUE)
  if (length(tgz) < 1L) {
    stop(
      "No .tgz under shinylive/webr/packages — wasm R packages were not bundled.\n",
      "  Without them, the app tries to download packages at preload and usually errors.\n",
      "  Fix: use Shinylive web assets >= 0.8.0 (this script sets SHINYLIVE_ASSETS_VERSION when unset),\n",
      "  ensure network access for export, and do not set SHINYLIVE_WASM_PACKAGES=0 unless intentional.\n",
      "  Also ensure export staging is not under a path ignored by renv (see script header)."
    )
  }
  message(
    "OK: ", length(tgz), " WebAssembly package archive(s) under shinylive/webr/packages/."
  )
  invisible(NULL)
}

main <- function() {
  project_root <- resolve_project_root()
  message("Project root: ", project_root)

  # Must not live under a path ignored by renv (e.g. repo-root shinylive-appsrc/ in .gitignore),
  # or renv::dependencies() returns zero rows and wasm packages are not bundled.
  staging_dir <- tempfile("easyeyes-shinylive-app-", tmpdir = tempdir())
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dest_dir <- file.path(project_root, "shinylive-site")

  message("Staging app (excluding renv, .git, prior export dirs, ...) -> ", staging_dir)
  rsync_stage_app(project_root, staging_dir)

  if (dir.exists(dest_dir)) {
    message("Removing previous export: ", dest_dir)
    unlink(dest_dir, recursive = TRUE, force = TRUE)
  }

  if (dir.exists(file.path(project_root, "renv"))) {
    message("Loading renv from project (for consistent dependency metadata) ...")
    renv::load(project_root)
  } else {
    message("Note: no renv/ directory; continuing without renv::load().")
  }

  if (!requireNamespace("shinylive", quietly = TRUE)) {
    stop("Package 'shinylive' is required. Install it in this project, e.g. renv::install('shinylive').")
  }
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for post-export validation.")
  }

  options(shiny.port = NA)

  ensure_shinylive_assets_for_wasm_bundle()

  if (requireNamespace("renv", quietly = TRUE)) {
    nd <- nrow(renv::dependencies(staging_dir, quiet = TRUE))
    if (nd < 1L) {
      stop(
        "renv::dependencies() found no R packages under the staging directory.\n",
        "  Shinylive would export with zero wasm bundles. Use a staging path outside .gitignore."
      )
    }
    message("Preflight: renv::dependencies() sees ", nd, " dependency record(s) under staging.")
  }

  message("Running shinylive::export() ...")
  shinylive::export(
    appdir = staging_dir,
    destdir = dest_dir,
    quiet = FALSE
  )

  validate_app_json(dest_dir)
  validate_webr_packages_bundled(dest_dir)

  message("\nDone.")
  message("Serve locally with:")
  message('  httpuv::runStaticServer("', dest_dir, '")')
  invisible(dest_dir)
}

main()
