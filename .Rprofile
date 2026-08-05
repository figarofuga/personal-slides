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