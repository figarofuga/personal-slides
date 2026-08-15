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

Open the repository in its Dev Container or GitHub Codespaces. Environment
installation is intentionally not run from `postCreateCommand`, because installing
the environment can rebuild the local `pixi-build-r` packages. On first setup,
make the mounted `.pixi` volume writable inside the Dev Container:

```bash
sudo chown -R "$(id -u):$(id -g)" .pixi
```

Then, or whenever `pixi.lock` changes, install the environment explicitly:

```bash
pixi install --locked
direnv reload  # Only when direnv is available and in use
```

If `direnv` is not installed, reopen the terminal instead of running
`direnv reload` below.

Entering the repository activates the existing environment with
`pixi shell-hook --as-is`; it does not install packages or rebuild local packages.

Add a package explicitly with:

```bash
pixi add パッケージ名
direnv reload
```

If `pixi.toml` was edited directly, including when adding a local `pixi-build-r`
package, update the lock file and environment without `--locked`:

```bash
pixi install
direnv reload
```

To remove the local build cache manually, run:

```bash
pixi clean --build
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
git commit -m "Add progress model performance"
git push

```

The rendered site is written to `docs/` for GitHub Pages.

TODO: complete mlcausal, sepsis hydration.
TODO: PROのordinalの図のFactorを直す

## Plans

- Medicine: HF in elderly, Rickettsia, Eosinophilia, hypokalemia, treatment of acute severe hyponatremia, Whipple disease, thrombophlic test for VTE, and antithrombosis drugs.
- Stats: Clustering, DAG, Model performance, visualizations, Advanced survival analysis including time-varying cox, splines, propensity score analysis, and multiple imputations
