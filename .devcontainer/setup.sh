#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

if [[ "$(uname -s)" != "Linux" || "$(uname -m)" != "x86_64" ]]; then
  echo "This workspace requires Linux x86_64 (linux-64)." >&2
  exit 1
fi

if [[ ! -f pixi.toml || ! -f pixi.lock || ! -f renv.lock || ! -f renv/activate.R ]]; then
  echo "Restore pixi.toml, pixi.lock, renv.lock, and renv/ from the same revision." >&2
  exit 1
fi

mkdir -p .pixi
if [[ ! -w .pixi ]]; then
  sudo chown "$(id -u):$(id -g)" .pixi
fi

# Fail on a stale lock; never resolve newer versions during setup.
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked

# Restore the R library from its lock, then verify both layers together.
pixi run --as-is restore
pixi run --as-is check
pixi run --as-is python -c 'import numpy, pandas, scipy; print("Python scientific packages: OK")'
pixi run --as-is quarto --version
