#!/usr/bin/env bash
set -euo pipefail

echo "Installing R packages from .devcontainer/r-packages.R ..."

Rscript .devcontainer/r-packages.R