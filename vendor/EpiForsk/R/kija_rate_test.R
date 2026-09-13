#' wrapper for \link[grf]{rank_average_treatment_effect}
#'
#' Provides confidence interval and p-value together with the standard output
#' from \link[grf]{rank_average_treatment_effect}.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}().
#' @param priorities character, name of covariate to test for heterogeneity.
#' @param level numeric, level of RATE confidence interval.
#' @param cov_type character, either "continuous" or "discrete". If "discrete",
#'   and q is not manually set, TOC will be evaluated at the quantiles
#'   corresponding to transitions from one level to the next.
#' @param target character, see \link[grf]{rank_average_treatment_effect}.
#' @param q numeric, see \link[grf]{rank_average_treatment_effect}.
#' @param R integer, see \link[grf]{rank_average_treatment_effect}.
#' @param subset numeric, see \link[grf]{rank_average_treatment_effect}.
#' @param debiasing.weights numeric, see
#'   \link[grf]{rank_average_treatment_effect}.
#' @param compliance.score numeric, see
#'   \link[grf]{rank_average_treatment_effect}.
#' @param num.trees.for.weights integer, see
#'   \link[grf]{rank_average_treatment_effect}.
#'
#' @returns A list of class 'rank_average_treatment_effect' with elements
#' - estimate: the RATE estimate.
#' - std.err: bootstrapped standard error of RATE.
#' - target: the type of estimate.
#' - TOC: a data.frame with the Targeting Operator Characteristic curve
#'   estimated on grid q, along with bootstrapped SEs.
#' - confint: a data.frame with the lower and upper bounds of the RATE
#'   confidence interval.
#' - pval: the p-value for the test that RATE is non-positive.
#'
#' @author KIJA
#'
#' @examples
#' \donttest{
#' n <- 800
#' p <- 3
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' rate <- RATETest(cf, 1)
#' rate$pval
#' }
#'
#' @export

RATETest <- function(forest,
                     priorities,
                     level = 0.95,
                     cov_type = c("continuous", "discrete"),
                     target = c("AUTOC", "QINI"),
                     q = seq(0.1, 1, by = 0.1),
                     R = 500,
                     subset = NULL,
                     debiasing.weights = NULL,
                     compliance.score = NULL,
                     num.trees.for.weights = 500) {
  if (is.null(priorities)) return(NULL)
  cov_type <- match.arg(cov_type)
  target = match.arg(target)
  if (!(
    is.character(priorities) && length(priorities) == 1 && !is.null(names(forest$X.orig)) |
    is.numeric(priorities) && length(priorities) == 1 |
    is.numeric(priorities) && length(priorities) == length(forest$Y.orig)
  )) {
    stop(
      paste(
        "'priorities' must be a length one character vector, a length one",
        "integer vector,\nor a numeric vector with the same length as",
        "forest$Y.orig. If 'priorities' is a\ncharacter vector, forest$X.orig",
        "must have named columns."
      )
    )
  }
  if (length(priorities) == 1) {
    if (is.matrix(forest$X.orig)) {
      priorities <- forest[["X.orig"]][, priorities]
    } else {
      priorities <- forest[["X.orig"]][[priorities]]
    }
  }
  if (!hasArg(q) && cov_type[1] == "discrete") {
    q <- cumsum(rev(table(priorities))) /
        length(priorities)
    if (min(q) > 0.001) q <- c(0.001, q)
  }
  rate <- grf::rank_average_treatment_effect(
    forest = forest,
    priorities = priorities,
    target = target,
    q = q,
    R = R,
    subset = subset,
    debiasing.weights = debiasing.weights,
    compliance.score = compliance.score,
    num.trees.for.weights = num.trees.for.weights
  )
  confint <- rate$estimate +
    dplyr::tibble(
      estimate = 0,
      lower = qnorm(0.5 - level / 2) * rate$std.err,
      upper = qnorm(0.5 + level / 2) * rate$std.err
    )
  pval <- 2 * pnorm(-abs(rate$estimate) / rate$std.err)
  out <- c(
    rate,
    list(
      confint = confint,
      pval = pval
    )
  )
  class(out) <- "rank_average_treatment_effect"
  return(out)
}
