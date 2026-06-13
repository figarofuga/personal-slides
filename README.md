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

quarto render
touch docs/.nojekyll
git add .
# git commit -m "Update rendered slides"
git commit -m "Change slide theme"
git push

```

TODO: complete PROanalysis, mlcausal, ESUS, and pituitary_incidentaloma. 