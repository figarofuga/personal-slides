options(
  repos = c(CRAN = "https://cloud.r-project.org"),
  Ncpus = max(1, parallel::detectCores() - 1)
)

packages <- c(
  # Quarto / R chunks
  "knitr",
  "rmarkdown",

  # Python bridge
  "reticulate",

  # よく使うもの
  "tidyverse",
  "here",
  "glue",
  "scales",
  "tinytable", 
  "tinyplot",

  # 表・図・HTMLまわり
  "gt",
  "htmltools",
  # statistics
  "rms", 
  "Hmisc", 
  "marginaleffects", 
  "modelsummary"
)

installed <- rownames(installed.packages())
missing <- setdiff(packages, installed)

if (length(missing) > 0) {
  install.packages(missing)
} else {
  message("All R packages are already installed.")
}