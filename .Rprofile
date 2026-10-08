# Always use this project's Pixi Python and renv library.
local({
  project <- Sys.getenv("PIXI_PROJECT_ROOT", unset = getwd())
  Sys.setenv(RENV_PROJECT = project)
  prefix <- file.path(project, ".pixi", "envs", "default")
  if (file.exists(file.path(prefix, "bin", "python"))) {
    Sys.setenv(
      CONDA_PREFIX = prefix,
      RETICULATE_PYTHON = file.path(prefix, "bin", "python"),
      RETICULATE_USE_MANAGED_VENV = "no",
      CMDSTAN = file.path(prefix, "bin", "cmdstan"),
      PATH = paste(file.path(prefix, "bin"), Sys.getenv("PATH"), sep = .Platform$path.sep),
      RENV_PATHS_CACHE = file.path(project, ".pixi", "renv-cache"),
      RENV_PATHS_SANDBOX = file.path(project, ".pixi", "renv-sandbox")
    )
  }
  options(
    repos = c(CRAN = "https://cloud.r-project.org"),
    pkgType = "source",
    Ncpus = 1L,
    timeout = max(600, getOption("timeout")),
    # renv's parallel installer applies this deadline to the entire restore.
    renv.install.timeout = 24 * 60 * 60,
    renv.config.ppm.enabled = FALSE,
    renv.config.pak.enabled = FALSE,
    # System dependencies live in Pixi, not in the host's apt database.
    renv.config.sysreqs.check = FALSE,
    renv.config.install.jobs = 1L
  )
  source(file.path(project, "renv", "activate.R"))
})
