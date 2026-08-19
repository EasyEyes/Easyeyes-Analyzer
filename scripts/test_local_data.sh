#!/usr/bin/env bash
# Run preprocess (read_files) and generate_threshold on every archive in
# local-test-data/, using the same defaults as the Shiny upload path.
#
# Usage:
#   ./scripts/test_local_data.sh
#   ./scripts/test_local_data.sh /path/to/other-data-dir
#   ./scripts/test_local_data.sh file1.results.zip file2.results.zip
#
# Exits 0 if every file completes with no R error; 1 if a file fails;
# 2 if setup is wrong (no Rscript, empty data dir, etc.).

set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

if ! command -v Rscript >/dev/null 2>&1; then
  echo "Rscript not found on PATH." >&2
  exit 2
fi

collect_from_dir() {
  local dir="$1"
  if [[ ! -d "$dir" ]]; then
    echo "Data directory not found: $dir" >&2
    exit 2
  fi
  # Only top-level files (skip extracted folders). Match what the app accepts.
  find "$dir" -maxdepth 1 -type f \( \
    -name '*.results.zip' -o \
    -name '*.zip' -o \
    -name '*.csv' -o \
    -name '*.xlsx' \
  \) ! -name '.DS_Store' ! -name '.gitkeep' | LC_ALL=C sort
}

FILES=()
if [[ $# -eq 0 ]]; then
  while IFS= read -r f; do
    FILES+=("$f")
  done < <(collect_from_dir "local-test-data")
elif [[ $# -eq 1 && -d "$1" ]]; then
  while IFS= read -r f; do
    FILES+=("$f")
  done < <(collect_from_dir "$1")
else
  FILES=("$@")
fi

if [[ ${#FILES[@]} -eq 0 ]]; then
  echo "No test files found. Put .results.zip / .csv / .xlsx under local-test-data/" >&2
  exit 2
fi

echo "Testing ${#FILES[@]} file(s) from $(pwd)"
echo

# Do not use --vanilla: this project activates renv from .Rprofile.
exec Rscript "$ROOT/scripts/test_local_data.R" "${FILES[@]}"
