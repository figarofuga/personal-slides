#!/usr/bin/env bash
set -euo pipefail

project_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$project_root"

export R_HOME="$project_root/.pixi/envs/default/lib/R"
export ARF_R_HOME="$R_HOME"

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

# Keep the R_PROFILE_USER and SESS_* variables supplied by vscode-R.
# --as-is activates the installed environment without installing/building packages
# or changing pixi.lock. --frozen alone still permits installation and builds.
exec "$pixi_executable" run --as-is arf --r-home "$R_HOME" "$@"
