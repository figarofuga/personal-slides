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

## VS Codeのみ
if (
  interactive() &&
  identical(Sys.getenv("TERM_PROGRAM"), "vscode")
) {

  vscode_init <- file.path(
    Sys.getenv("HOME"),
    ".vscode-R",
    "init.R"
  )

  if (file.exists(vscode_init)) {
    source(vscode_init)
  }
}
