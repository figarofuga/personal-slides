#!/usr/bin/env bash
set -euo pipefail

# R packages
echo "Installing R packages..."
Rscript .devcontainer/r-packages.R

# Python packages, if you choose uv + .venv
if [ -f "pyproject.toml" ]; then
  if [ -f "uv.lock" ]; then
    uv sync --frozen
  else
    uv sync
  fi
fi

quarto --version
typst --version
R --version