#' @title Asymptotic Bayes factors
#'
#' @description The Bayesian equivalent of a significance test for H1: an
#'   unrestricted parameter value versus H0: of a specific parameter value based
#'   only on data D can be obtained from Bayes factor (BF). Then `BF =
#'   Probability(H1|D) / Probability(H0|D)` and is a Bayesian equivalent of a
#'   likelihood ratio. It is based on the same asymptotics as the ubiqutous
#'   chi-square tests. This BF only depends on the difference in deviance
#'   between the models corresponding to H0 and H1 (chisquare) and the dimension
#'   d of H1. This BF is monotone in chisquare (and hence the p-value p) for
#'   fixed d. It is thus a tool to turn p-values into evidence, also
#'   retrospectively. The expression for BF depends on a parameter lambda which
#'   expresses the ratio between the information in the prior and the data
#'   (likelihood). By default `lambda = min(d / chisquare, lambdamax = 0.255)`.
#'   Thus, as chisquare goes to infinity we effectively maximize BF and hence
#'   the evidence favoring H1, and opposite for small chisquare has a
#'   well-defined watershed where we have equal preferences for H1 and H0. The
#'   value 0.255 corresponds to a watershed of 2, that is we prefer H1 when
#'   `chisquare > d * 2` and prefer H0 when `chisquare < d * 2`, similar to
#'   having a BF that is a continuous version of the Akaike Information
#'   Criterion for model selection. For derivations and details, see Rostgaard
#'   (2023).
#'
#' @param chisq a non-negative number denoting the difference in deviance
#'   between the statistical models corresponding to H0 and H1
#' @param p the p value corresponding to the test statistic chisq on d degrees
#'   of freedom
#' @param d the dimension of H1, `d => 1`
#' @param lambda a strictly positive number corresponding to the ratio between
#'   the information in the prior and the data
#' @param lambdamax an upper limit on lambda
#' @param bf Bayes factor, a strictly positive number
#' @param logasympBF log(bf)
#'
#' @details For fixed dimension d of the alternative hypothesis H1 `asympBF(.) =
#'   exp(logasympBF(.))` maps a test statistic (chisquare) or a p-value p into a
#'   Bayes factor (the ratio between the probabilities of the models
#'   corresponding to each hypothesis). `asympBF(.)` is monotonely increasing in
#'   chisquare, attaining the value 1 when `chisquare = d * watershed`. The
#'   watershed is thus a device to specify what the user considers a practical
#'   null result by choosing `lambdamax = watershed(watershed)`.
#'
#'   For sufficiently large values of chisquare, lambda is estimated as
#'   d/chisquare. This behavior can be overruled by specifying lambda. Using
#'   `invasympBF(.) = exp(invlogasympBF(.))` we can map a Bayes factor, bf to a
#'   value of chisquare.
#'
#'   Likewise, we can obtain the watershed corresponding to a given lambdamax as
#'   `invwatershed(lambdamax)`.
#'
#'   Generally in these functions we recode or ignore illegal values of
#'   parameters, rather than returning an error code. `chisquare` is always
#'   recoded as `abs(chisquare)`. `d` is set to `1` as default, and missing or
#'   entered values of `d < 1` are recoded as `d = 1`. Entered values of
#'   `lambdamax <= 0` or missing are ignored. Entered values of `lambda <= 0` or
#'   missing are ignored in `invwatershed(.)`. we use `abs(lambda)` as argument,
#'   `lambda = 0` results in an error.
#'
#' @references Klaus Rostgaard (2023): Simple nested Bayesian hypothesis testing
#' for meta-analysis, Cox, Poisson and logistic regression models. Scientific
#' Reports. https://doi.org/10.1038/s41598-023-31838-8
#'
#' @author
#' KLP & KIJA
#'
#' @examples
#' # example code
#'
#' # 1. the example(s) from Rostgaard (2023)
#' asympBF(p = 0.19, d = 8) # 0.148411
#' asympBF(p = 0.19, d = 8, lambdamax = 100) # 0.7922743
#' asympBF(p = 0.19, d = 8, lambda = 100 / 4442) # 5.648856e-05
#' # a maximal value of a parameter considered practically null
#' deltalogHR <- -0.2 * log(0.80)
#' sigma <- (log(1.19) - log(0.89)) / 3.92
#' chisq <- (deltalogHR / sigma) ** 2
#' chisq # 0.3626996
#' watershed(chisq)
#' # leads nowhere useful chisq=0.36<2
#'
#' # 2. tests for interaction/heterogeneity - real world examples
#' asympBF(p = 0.26, d = 24) # 0.00034645
#' asympBF(p = 0.06, d = 11) # 0.3101306
#' asympBF(p = 0.59, d = 7) # 0.034872
#'
#' # 3. other examples
#' asympBF(p = 0.05) # 2.082664
#' asympBF(p = 0.05, d = 8) # 0.8217683
#' chisq <- invasympBF(19)
#' chisq # 9.102697
#' pchisq(chisq, df = 1, lower.tail = FALSE) # 0.002552328
#' chisq <- invasympBF(19, d = 8)
#' chisq # 23.39056
#' pchisq(chisq, df = 8, lower.tail = FALSE) # 0.002897385

#' @rdname asympBF
#' @export
logasympBF <- function(chisq = NA,
                       p = NA,
                       d = 1,
                       lambda = NA,
                       lambdamax = 0.255) {
  chisq <- abs(chisq)
  d <- ifelse(is.na(d) | d < 1, 1, d)
  if (is.na(chisq)) chisq <- qchisq(p, d, lower.tail = FALSE)
  lambda <- ifelse(lambda <= 0 | is.na(lambda), d / max(chisq, 10E-6), lambda)
  lambdamax <- ifelse(lambdamax <= 0, 0.255, lambdamax)
  lambda0 <- min(lambda, lambdamax)
  psi <- lambda0 / (1 + lambda0)
  logasympBF <- d/2 * log(psi) + (1 - psi) * chisq/2
  return(logasympBF)
}

#' @rdname asympBF
#' @export
asympBF <- function(chisq = NA,
                    p = NA,
                    d = 1,
                    lambda = NA,
                    lambdamax = 0.255) {
  call <- match.call()
  call[[1]] <- logasympBF
  asympBF <- exp(eval(call))
  return(asympBF)
}

#' @rdname asympBF
#' @export
invlogasympBF <- function(logasympBF = NA,
                          d = 1,
                          lambda = NA,
                          lambdamax = 0.255) {
  d <- ifelse(is.na(d) | d < 1, 1, d)
  lambda <- ifelse(lambda <= 0, NA, lambda)
  lambdamax <- ifelse(lambdamax <= 0, 0.255, lambdamax)
  if (is.na(lambda)) {
    lambdaP <- lambdamax
    lambdaT <- NA
  } else {
    lambdaP <- min(lambda, lambdamax)
    lambdaT <- lambdaP
  }
  chisqT <- NA
  psiP <- lambdaP / (lambdaP + 1)
  chisqP <- (logasympBF - d * log(psiP)/2) * 2 / (1 - psiP)
  if (!is.na(lambdaT) || (0 <= chisqP && chisqP <= d / lambdaP)) {
    chisqT<-chisqP
    lambdaT<-lambdaP
  }
  epsilon <- 0.000005
  for (taeller in 1:20) {
    lambdaP <- min(lambdamax, d / chisqP)
    psiP <- lambdaP / (lambdaP + 1)
    chisqP <- (logasympBF - d * log(psiP)/2) * 2 / (1 - psiP)
    if (abs(lambdaP / min(lambdamax, d / chisqP) - 1) < epsilon) {
      chisqT <- chisqP
      break
    }
  }
  return(chisqT)
}

#' @rdname asympBF
#' @export
invasympBF <- function(bf,
                       d = 1,
                       lambda = NA,
                       lambdamax = 0.255) {
  invasympBF <- invlogasympBF(
    logasympBF = log(bf),
    d = d,
    lambda = lambda,
    lambdamax = lambdamax
  )
  return(invasympBF)
}

#' @rdname asympBF
#' @export
watershed <- function(chisq) {
  chisq <- abs(chisq)
  psi <- 0.5
  delta <- 0.25
  twologBF <- log(psi) + chisq * (1 - psi)
  for (taeller in 1:20) {
    if (twologBF < 0) psi <- psi + delta else psi <- psi - delta
    twologBF <- log(psi) + chisq * (1 - psi)
    if (abs(twologBF) <= 10e-6) break else delta <- delta / 2
  }
  lambda <- psi / (1 - psi)
  return(lambda)
}

#' @rdname asympBF
#' @export
invwatershed <- function(lambda) {
  lambda <- abs(lambda)
  psi <- lambda / (1 + lambda)
  chisq <- -log(psi) / (1 - psi)
  return(chisq)
}
