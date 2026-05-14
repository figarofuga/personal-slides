#!/usr/bin/env bash
set -euo pipefail

# Quarto website / reveal.js slides
quarto render

# Typst / Touying PDF slides
mkdir -p docs/typst/amyloidosis
mkdir -p docs/typst/PEG
mkdir -p docs/typst/sepsis-fluid

typst compile typst/amyloidosis/main.typ docs/typst/amyloidosis/slides.pdf
typst compile typst/PEG/main.typ docs/typst/PEG/slides.pdf
typst compile typst/sepsis-fluid/main.typ docs/typst/sepsis-fluid/slides.pdf

# GitHub PagesでJekyll処理を避ける
touch docs/.nojekyll