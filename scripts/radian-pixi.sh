#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

export R_HOME="$project_root/.pixi/envs/default/lib/R"
export R_LIBS_USER="$project_root/.pixi/envs/default/lib/R/library"
export RSTUDIO_WHICH_R="$project_root/.pixi/envs/default/bin/R"

exec "$project_root/.pixi/envs/default/bin/radian" "$@"
