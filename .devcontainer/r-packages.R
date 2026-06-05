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
  "AER",
  "bonsai",
  "bootstrap",
  "cowplot",
  "dagitty",
  "DiagrammeR",
  "easystats",
  "ggcube",
  "ggdag",
  "ggsurvfit",
  "glmmTMB",
  "glue",
  "gt",
  "gtsummary", 
  "gtExtras",
  "grf",
  "here",
  "Hmisc",
  "kableExtra",
  "knitr",
  "lightgbm",
  "marginaleffects",
  "MatchIt",
  "mice", 
  "modelsummary",
  "naniar",
  "plotly",
  "policytree",
  "reticulate",
  "rmarkdown",
  "rms",
  "rootSolve",
  "scales",
  "sessioninfo",
  "simsurv",
  "skimr",
  "survival",
  "svglite",
  "tidymodels",
  "tidyverse",
  "tinyplot",
  "tinytable",
  "WeightIt",
  "xgboost"
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
