## reticulateでは常にこのプロジェクトのpixi Pythonを使う
local({
  pixi_prefix <- normalizePath(
    file.path(getwd(), ".pixi", "envs", "default"),
    mustWork = FALSE
  )

  pixi_python <- file.path(pixi_prefix, "bin", "python")

  if (file.exists(pixi_python)) {
    Sys.setenv(
      CONDA_PREFIX = pixi_prefix,
      RETICULATE_PYTHON = pixi_python,
      RETICULATE_USE_MANAGED_VENV = "no"
    )

    Sys.setenv(
      PATH = paste(
        file.path(pixi_prefix, "bin"),
        Sys.getenv("PATH"),
        sep = .Platform$path.sep
      )
    )
  }
})

## vscode-R 3.x loads its own profile and connects via sess.
## Do not source the removed ~/.vscode-R/init.R script here.
