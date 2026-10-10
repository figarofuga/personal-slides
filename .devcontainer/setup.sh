#!/usr/bin/env bash
set -Eeuo pipefail

base_only=false
full_check=false
for argument in "$@"; do
  case "$argument" in
    --base-only) base_only=true ;;
    --check) full_check=true ;;
    *) echo "Usage: bash .devcontainer/setup.sh [--base-only | --check]" >&2; exit 2 ;;
  esac
done
if $base_only && $full_check; then
  echo "Choose either --base-only or --check." >&2
  exit 2
fi

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

# Keep diagnostics outside the environment volume, including failed restores.
log_file="$project_root/.devcontainer/setup.log"
exec > >(tee -a "$log_file") 2>&1
step="preflight"
trap 'status=$?; echo "[$(date -Is)] FAILED: $step (exit $status). Log: $log_file"; exit "$status"' ERR
echo "[$(date -Is)] Starting setup: ${*:-restore}"

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
step="pixi install --locked"
echo "[$(date -Is)] $step"
env -u PIXI_NO_INSTALL -u PIXI_FROZEN pixi install --locked

if $base_only; then
  echo "[$(date -Is)] Pixi base ready."
  echo "To restore R packages, run: bash .devcontainer/setup.sh"
  echo "R analysis and rendering require that restore to finish."
  exit 0
fi

# Re-running restore reuses installed packages and the persistent renv cache.
step="R package restore"
echo "[$(date -Is)] $step (first restore can take hours)"
pixi run --as-is restore
step="Python package check"
echo "[$(date -Is)] $step"
pixi run --as-is python -c 'import numpy, pandas, scipy; print("Python scientific packages: OK")'
step="Quarto version check"
echo "[$(date -Is)] $step"
pixi run --as-is quarto --version

if $full_check; then
  step="full integration check (Stan compilation and sampling, Quarto rendering)"
  echo "[$(date -Is)] $step"
  pixi run --as-is check
fi
echo "[$(date -Is)] Setup completed. Log: $log_file"
if ! $full_check; then
  echo "For full integration checks, run: pixi run --as-is check"
fi
