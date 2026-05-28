options(Ncpus = max(1, parallel::detectCores() - 1))

# options(repos = c(
#   CRAN = "https://cloud.r-project.org",
#   rlib = "https://r-lib.r-universe.dev",
#   tidyverse = "https://tidyverse.r-universe.dev",
#   tidymodels = "https://tidymodels.r-universe.dev",
#   easystats = "https://easystats.r-universe.dev",
#   ropensci = "https://ropensci.r-universe.dev"
# ))

if (requireNamespace("bspm", quietly = TRUE)) {
  bspm::enable()
}

pkgs <- c(
  "here",
  "glue",
  "knitr",
  "rmarkdown",
  "reticulate",
  "tidyverse",
  "easystats",
  "tidymodels",
  "bonsai",
  "lightgbm",
  "scales",
  "tinytable",
  "tinyplot",
  "survival",
  "plotly",
  "xgboost",
  "marginaleffects",
  "MatchIt",
  "rootSolve",
  "WeightIt",
  "modelsummary",
  "rms",
  "Hmisc",
  "grf",
  "dagitty",
  "ggdag",
  "sessioninfo"
)

to_install <- setdiff(pkgs, rownames(installed.packages()))

if (length(to_install) > 0) {
  install.packages(to_install, dependencies = TRUE)
}

missing <- setdiff(pkgs, rownames(installed.packages()))

if (length(missing) > 0) {
  stop(
    "The following packages could not be installed: ",
    paste(missing, collapse = ", ")
  )
}
