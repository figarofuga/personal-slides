# personal-slides

Make my own study homepage.

**https://figarofuga.github.io/personal-slides/**

The structure of this repository is as follows:

```text
personal-slides/
├── .devcontainer/
│   ├── devcontainer.json
│   └── Dockerfile
├── pixi.toml
├── pixi.lock
├── _quarto.yml
├── index.qmd
├── statistics/
├── medicine/
└── docs/
```

## Development

Open the repository in its Dev Container or GitHub Codespaces. The environment is
installed automatically from `pixi.lock`. To install it manually, run:

```bash
pixi install --locked
```

Preview or render the site with:

```bash
pixi run preview
pixi run render

# Base Folder（pixi.tomlがある場所）からの相対パスで1つだけ
pixi run render statistics/RMST_mi/index.qmd
pixi run render medicine/antithrombotic_etc/index.qmd

git add .
# git commit -m "Update rendered slides"
git commit -m "Add antithrombotic-etc slides"
git push

```

The rendered site is written to `docs/` for GitHub Pages.

TODO: complete mlcausal, sepsis hydration.
TODO: PROのordinalの図のFactorを直す

## Plans

- Medicine: Rickettsia, Eosinophilia, hypokalemia, treatment of acute severe hyponatremia, Whipple disease, thrombophlic test for VTE, and antithrombosis drugs.
- Stats: Clustering, DAG, Model performance, visualizations, Advanced survival analysis including time-varying cox, splines, propensity score analysis, and multiple imputations
