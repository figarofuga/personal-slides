# personal-slides

Make my own study homepage.

**https://figarofuga.github.io/personal-slides/**

The structure of this repositry was as follows: 

``` text
slides/
├── .devcontainer/
│   ├── devcontainer.json
│   ├── Dockerfile
├── _quarto.yml
├── index.qmd
├── statistics/
│   ├── index.qmd
│   └── regression-intro/
│       └── index.qmd
├── medicine/
│   ├── index.qmd
│   └── clinical-trial/
│       └── index.qmd
└── docs/
```

If we made qmd file for slides, then do as follows.

```bash

source .venv/bin/activate

# quarto render
# touch docs/.nojekyll
./scripts/render-site.sh
git add .
# git commit -m "Update rendered slides"
git commit -m "Progress mlcausal slides"
git push

```

TODO: complete mlcausal, FMF, and systemic amyloidosis.

# Plans: 

- Medicine: Eosinophilia, hypokalemia, treatment of acute severe hyponatremia, sepsis hydration, Whipple disease, thrombophlic test for VTE, and antithrombosis drugs.
- Stats: Clustering, DAG, Model performance, visualizations, Advanced survival analysis including time-varying cox, splines, propensity score analysis, and multiple imputations