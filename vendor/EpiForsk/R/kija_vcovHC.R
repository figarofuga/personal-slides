#' Heteroscedasticity-Consistent Covariance Matrix
#'
#' Calculate Heteroscedasticity-Consistent Covariance Matrix from a linear
#' model using the HC3 method from sandwich.
#'
#' @param x lm object
#'
#' @return A matrix containing the covariance matrix estimate.
#'
#' @examples 1+1

vcovHC <- function (x)
{
  X <- model.matrix(x)
  attr(X, "assign") <- NULL
  n <- NROW(X)
  diaghat <- hatvalues(x)
  ef <- as.vector(residuals(x)) * X
  attr(ef, "contrasts") <- NULL
  res <- rowMeans(ef/X, na.rm = TRUE)
  all0 <- apply(abs(ef) < .Machine$double.eps, 1L, all)
  res[all0] <- 0
  omega <- res^2 / (1 - diaghat)^2
  rval <- sqrt(omega) * X
  meat <- crossprod(rval)/n
  if (!is.null(x$na.action))
    class(x$na.action) <- "omit"
  sx <- summary.lm(x)
  bread <- sx$cov.unscaled * as.vector(sum(sx$df[1L:2L]))
  return(1/n * (bread %*% meat %*% bread))
}
