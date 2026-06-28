#!/usr/bin/env bash
set -u

targets=("index.qmd")
while IFS= read -r target; do
  targets+=("$target")
done < <(find statistics medicine -name "*.qmd" | sort)

failures=()

for target in "${targets[@]}"; do
  printf '\nRendering %s\n' "$target"
  if ! quarto render "$target" --no-clean; then
    failures+=("$target")
  fi
done

if (( ${#failures[@]} > 0 )); then
  printf '\nRender completed with failures:\n'
  printf '  %s\n' "${failures[@]}"
  exit 1
fi

printf '\nRender completed successfully.\n'
