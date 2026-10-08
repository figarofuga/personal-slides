#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

export R_HOME="$project_root/.pixi/envs/default/lib/R"
export RSTUDIO_WHICH_R="$project_root/.pixi/envs/default/bin/R"

pixi_executable="${PIXI_EXE:-}"

if [[ -z "$pixi_executable" ]]; then
  pixi_executable="$(command -v pixi || true)"
fi

if [[ -z "$pixi_executable" && -x "${HOME}/.pixi/bin/pixi" ]]; then
  pixi_executable="${HOME}/.pixi/bin/pixi"
fi

if [[ -z "$pixi_executable" ]]; then
  echo "pixi executable was not found." >&2
  exit 127
fi

# `pixi run` applies the environment's activation scripts.  In particular,
# Quarto needs QUARTO_DENO, QUARTO_SHARE_PATH, and QUARTO_PANDOC when it is
# called from R packages such as sessioninfo.
# Do not install/rebuild local packages when starting an editor console.
exec "$pixi_executable" run --as-is radian "$@"
