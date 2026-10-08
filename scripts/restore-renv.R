# Run from the repository root via `pixi run --as-is restore`.
if (!nzchar(Sys.getenv("PIXI_PROJECT_ROOT"))) {
  stop("Run this script through pixi so that R and the compiler are activated.")
}
lock <- renv::lockfile_read("renv.lock")
if (as.character(getRversion()) != lock$R$Version) {
  stop("R version differs from renv.lock; restore pixi.lock first.")
}
renv::restore(prompt = FALSE)
status <- IRkernel::installspec(
  user = FALSE,
  prefix = Sys.getenv("CONDA_PREFIX"),
  name = "ir",
  displayname = "R (pixi + renv)",
  rprofile = file.path(Sys.getenv("PIXI_PROJECT_ROOT"), ".Rprofile")
)
stopifnot(status == 0)
spec_path <- file.path(Sys.getenv("CONDA_PREFIX"), "share/jupyter/kernels/ir/kernel.json")
spec <- jsonlite::read_json(spec_path, simplifyVector = FALSE)
spec$env$PIXI_PROJECT_ROOT <- Sys.getenv("PIXI_PROJECT_ROOT")
jsonlite::write_json(spec, spec_path, auto_unbox = TRUE, pretty = TRUE)
