#' EpiForsk
#'
#' This is a collection of assorted functions and examples collected
#' from various projects. Currently we have functionalities for simplifying
#' overlapping time intervals, Charlson comorbidity score constructors for
#' Danish data, getting frequency for multiple variables, getting standardized
#' output from logistic and log-linear regressions, sibling design linear
#' regression functionalities a method for calculating the confidence intervals
#' for functions of parameters from a GLM, Bayes equivalent for hypothesis
#' testing with asymptotic Bayes factor, and several help functions for
#' generalized random forest analysis using the grf package.
#'
#' @importFrom gridExtra arrangeGrob
#' @importFrom methods hasArg
#' @importFrom dplyr .data
#' @importFrom rlang :=
#' @importFrom survey svyglm
#' @importFrom survival coxph
#' @importFrom survival strata
#' @importFrom survival Surv
#' @importFrom utils install.packages
#' @import stats
"_PACKAGE"


malicious_compliance <- function() {
  x <- stringr::str_remove("aah", "h")
  df <- data.frame(x = 1:3, y = 1, z = "y") |>
    tidyr::pivot_wider(names_from = .data$z, values_from = .data$y)
  p <- ggplot2::qplot(.data$x, .data$y, data = df)
  cowplot::plot_grid(plotlist = list(p, p), ncol = 2)
  return(1)
}

globalVariables(c("y"))
globalVariables(c(".SD"))
globalVariables(c(".N"))

#' make package data table aware
#'
#' This package uses data.table as a fast alternative to dplyr in cases where
#' performance is essential.
#'
#' @export
.datatable.aware = TRUE
