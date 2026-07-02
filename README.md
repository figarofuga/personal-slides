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

If make slides, do as follows.

```bash

# quarto render
# touch docs/.nojekyll
./scripts/render-site.sh
git add .
# git commit -m "Update rendered slides"
git commit -m "Complete ESUS slides"
git push

```

TODO: mlcausal and pituitary_incidentaloma.

# Plans: 

- Medicine: Eosinophilia, Systemic Amyloidosis, hypokalemia, treatment of acute severe hyponatremia, sepsis hydration, Whipple disease, thrombotic test, and antithrombosis drugs.
- Stats: Clustering, DAG, Model performance, visualizations, Advanced survival analysis including time-varying cox, splines, propensity score analysis, and multiple imputations