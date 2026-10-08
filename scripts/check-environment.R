# A small integration check, including actual compilation and execution.
stopifnot(renv::status()$synchronized)
lock <- renv::lockfile_read("renv.lock")
library <- normalizePath(renv::paths$library())
for (package in names(lock$Packages)) {
  # renv itself may be loaded through its cache symlink's resolved path.
  expected <- file.path(library, package)
  stopifnot(dir.exists(expected))
  stopifnot(normalizePath(find.package(package)) == normalizePath(expected))
  stopifnot(packageVersion(package) == package_version(lock$Packages[[package]]$Version))
}
packages <- c(
  "tidyverse", "tidymodels", "brms", "rstan", "rstanarm", "rmsb", "glmmTMB",
  "stochtree", "xgboost", "lightgbm", "httpgd", "languageserver", "sess",
  "reticulate", "IRkernel", "ExclusionTable", "forestploter", "ggcube", "clarify",
  "plotthis", "simsurv", "AIPW", "qreport", "PBSmodelling", "PredictABEL", "dcurves"
)
for (package in packages) {
  loadNamespace(package)
  cat(package, as.character(packageVersion(package)), "OK\n")
}
python <- reticulate::py_config()$python
stopifnot(normalizePath(python) == normalizePath(Sys.getenv("RETICULATE_PYTHON")))
reticulate::py_run_string("import numpy, pandas, scipy, sklearn")
stopifnot(dir.exists(cmdstanr::cmdstan_path()))
cat("CmdStan", as.character(cmdstanr::cmdstan_version()), "at", cmdstanr::cmdstan_path(), "\n")
model_dir <- tempfile("stan-check-")
dir.create(model_dir)
stan_file <- file.path(model_dir, "normal.stan")
writeLines("parameters { real mu; } model { mu ~ normal(0, 1); }", stan_file)
model <- cmdstanr::cmdstan_model(stan_file, quiet = TRUE)
fit <- model$sample(
  seed = 20261007, chains = 1, parallel_chains = 1,
  iter_warmup = 50, iter_sampling = 50, refresh = 0,
  show_messages = FALSE
)
stopifnot(all(is.finite(fit$draws("mu", format = "matrix"))))
rstan_model <- rstan::stan_model(file = stan_file, auto_write = FALSE)
rstan_fit <- rstan::sampling(rstan_model, seed = 20261007, chains = 1,
                             iter = 100, warmup = 50, refresh = 0)
stopifnot(all(is.finite(as.matrix(rstan_fit)[, "mu"])))
rstanarm_fit <- rstanarm::stan_glm(
  mpg ~ wt, data = mtcars, seed = 20261007, chains = 1,
  iter = 100, warmup = 50, refresh = 0
)
stopifnot(all(is.finite(as.matrix(rstanarm_fit))))
# Verify rmsb's regenerated model where the main and partial-PO widths differ.
rmsb_data <- transform(mtcars, outcome = ordered(rep(1:4, length.out = nrow(mtcars))))
rmsb_fit <- rmsb::blrm(
  outcome ~ mpg + wt, ppo = ~ mpg, cppo = function(y) as.numeric(y),
  iprior = 2, priorsdppo = 1, data = rmsb_data, backend = "rstan",
  seed = 20261008, chains = 1, iter = 100, warmup = 50,
  method = "sampling", loo = FALSE, refresh = 0
)
stopifnot(all(is.finite(coef(rmsb_fit))))
unlink(model_dir, recursive = TRUE)
cat("R packages, Python bridge, CmdStan, RStan, rstanarm and rmsb sampling: OK\n")
status <- system2("bash", "scripts/check-quarto.sh")
stopifnot(status == 0)
