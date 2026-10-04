#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
  echo "This workspace requires Linux x86_64 (linux-64)." >&2
  exit 1
fi

if [[ ! -f pixi.toml || ! -f pixi.lock ]]; then
  echo "Restore pixi.toml, pixi.lock, and vendor/ from the same revision." >&2
  exit 1
fi

mkdir -p .pixi
if [[ ! -w .pixi ]]; then
  sudo chown "$(id -u):$(id -g)" .pixi
fi

# Fail on a stale lock; never resolve newer versions during setup.
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked

# Check the installed runtimes and every vendored R package.
pixi run --as-is Rscript -e '
  descriptions <- Sys.glob("vendor/*/DESCRIPTION")
  packages <- vapply(descriptions, function(path) read.dcf(path)[1, "Package"], character(1))
  for (package in packages) {
    loadNamespace(package)
    cat(package, as.character(packageVersion(package)), "OK\n")
  }
'
pixi run --as-is python -c 'import numpy, pandas, scipy; print("Python scientific packages: OK")'
pixi run --as-is quarto --version
