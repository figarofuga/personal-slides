#' Confidence set for functions of model parameters
#'
#' Computes confidence sets of functions of model parameters by computing a
#' confidence set of the model parameters and returning the codomain of the
#' provided function given the confidence set of model parameters as domain.
#'
#' @param object A fitted model object.
#' @param f A function taking the parameter vector as its single argument, and
#'   returning a numeric vector.
#' @param which_parm Either a logical vector the same length as the coefficient
#'   vector, with `TRUE` indicating a coefficient is used by `f`, or an integer
#'   vector with the indices of the coefficients used by `f`.
#' @param level The confidence level required.
#' @param ... Additional argument(s) passed to methods.
#'
#' @returns A tibble with columns estimate, conf.low, and conf.high or if
#'   return_beta is `TRUE`, a list with the tibble and the beta values on the
#'   boundary used to calculate the confidence limits.
#'
#' @details Assume the response Y and predictors X are given by a generalized
#'   linear model, that is, they fulfill the assumptions
#'   \deqn{E(Y|X)=\mu(X^T\beta)}{E(Y|X)=\mu(X^T\beta)}
#'   \deqn{V(Y|X)=\psi \nu(\mu(X^T\beta))}{V(Y|X)=\psi \nu(\mu(X^T\beta))}
#'   \deqn{Y|X\sim\varepsilon(\theta,\nu_{\psi}).}{%
#'   Y|X ~ \varepsilon(\theta,\nu_(\psi)).}
#'   Here \eqn{\mu} is the mean value function, \eqn{\nu} is the variance
#'   function, and \eqn{\psi} is the dispersion parameter in the exponential
#'   dispersion model
#'   \eqn{\varepsilon(\theta,\nu_{\psi})}{\varepsilon(\theta,\nu_(\psi))}, where
#'   \eqn{\theta} is the canonical parameter and \eqn{\nu_{\psi}}{\nu_(\psi)} is
#'   the structure measure. Then it follows from the central limit theorem that
#'   \deqn{\hat\beta\sim N(\beta, (X^TWX)^{-1})}{%
#'   \hat\beta~N(\beta, (X^TWX)^(-1))}
#'   will be a good approximation in large samples, where \eqn{X^TWX} is the
#'   Fisher information of the exponential dispersion model.
#'
#'   From this, the combinant
#'   \deqn{(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)}{%
#'   (\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)}
#'   is an approximate pivot, with a \eqn{\chi_p^2} distribution. Then
#'   \deqn{C_{\beta}=\{\beta|(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)<\chi_p^2(1-\alpha)\}}{%
#'   C_(\beta)=\{\beta|(\hat\beta-\beta)^TX^TWX(\hat\beta-\beta)<\chi_p^2(1-\alpha)\}}
#'   is an approximate \eqn{(1-\alpha)}-confidence set for the parameter vector
#'   \eqn{\beta}. Similarly, confidence sets for sub-vectors of \eqn{\beta} can
#'   be obtained by the fact that marginal distributions of normal distributions
#'   are again normally distributed, where the mean vector and covariance matrix
#'   are appropriate subvectors and submatrices.
#'
#'   Finally, a confidence set for the transformed parameters \eqn{f(\beta)}
#'   is obtained as
#'   \deqn{\{f(\beta)|\beta\in C_{\beta}\}}{\{f(\beta)|\beta\in C_(\beta)\}.}
#'   Note this is a conservative confidence set, since parameters outside the
#'   confidence set of \eqn{\beta} can be mapped to the confidence set of the
#'   transformed parameter.
#'
#'   To determine \eqn{C_{\beta}}, `fct_confint()` uses a convex optimization
#'   program when f is follows DCP rules. Otherwise, it finds the boundary by
#'   taking a number of points around \eqn{\hat\beta} and projecting them onto
#'   the boundary. In this case, the confidence set of the transformed parameter
#'   will only be valid if the boundary of \eqn{C_{\beta}} is mapped to the
#'   boundary of the confidence set for the transformed parameter.
#'
#'   The points projected to the boundary are either laid out in a grid around
#'   \eqn{\hat\beta}, with the number of points in each direction determined
#'   by `n_grid`, or uniformly at random on a hypersphere, with the number of
#'   points determined by `k`. The radius of the grid/sphere is determined by
#'   `len`.
#'
#'  @section Progress bar:
#'
#'   To print a progress bar with information about the fitting process, wrap
#'   the call to fct_confint in with_progress, i.e.
#'   `progressr::with_progress({result <- fct_confint(object, f)})`
#'
#'  @section Specifying a plan to resolve futures:
#'
#'   `fct_confint()` uses futures to enable parallel processing. Use the
#'   `future::plan()` function to specify how futures are resolved.
#'
#' @author
#' KIJA
#'
#' @examples
#' data <- 1:5 |>
#'   purrr::map(
#'     \(x) {
#'       name = paste0("cov", x);
#'       dplyr::tibble("{name}" := rnorm(100, 1))
#'     }
#'   ) |>
#'   purrr::list_cbind() |>
#'   dplyr::mutate(
#'   y = rowSums(dplyr::across(dplyr::everything())) + rnorm(100)
#'   )
#' lm <- lm(
#'  as.formula(
#'   paste0("y ~ 0 + ", paste0(names(data)[names(data) != "y"], collapse = " + "))
#'  ),
#'  data
#' )
#' fct_confint(lm, sum)
#' fct_confint(lm, sum, which_parm = 1:3, level = 0.5)
#'
#' @export

fct_confint <- function(
    object,
    f,
    which_parm = rep(TRUE, length(coef(object))),
    level = 0.95,
    ...
) {
  stopifnot(
    "'fct_confint' requires package 'future' to be installed." =
      requireNamespace("future", quietly = TRUE)
  )
  stopifnot(
    "'fct_confint' requires package 'furrr' to be installed." =
      requireNamespace("furrr", quietly = TRUE)
  )
  UseMethod("fct_confint")
}


#' @rdname fct_confint
#' @param return_beta Logical, if `TRUE` returns both the confidence limits and
#'   the parameter values used from the boundary of the parameter confidence
#'   set.
#' @param n_grid Either `NULL` or an integer vector of length 1 or the number of
#'   `TRUE`/indices in which_parm. Specifies the number of grid points in each
#'   dimension of a grid with endpoints defined by len. If `NULL` or `0L`, will
#'   instead sample k points uniformly on a sphere.
#' @param k If n_grid is `NULL` or `0L`, the number of points to sample
#'   uniformly from a sphere.
#' @param len numeric, the radius of the sphere or box used to define directions
#'   in which to look for boundary points of the parameter confidence set.
#'
#' @export

fct_confint.lm <- function(
    object,
    f,
    which_parm = rep(TRUE, length(coef(object))),
    level = 0.95,
    return_beta = FALSE,
    n_grid = NULL,
    k = NULL,
    len = 0.1,
    ...
) {
  ### check input
  # check object class
  stopifnot("object must inherit from class 'lm'" = inherits(object, "lm"))
  # check f is a function
  stopifnot("f must be a function" = is.function(f))
  # convert which_parm to a logical vector
  if (!purrr::is_logical(which_parm)) {
    if (purrr::is_double(which_parm)) {
      which_parm_ind <- as.integer(trunc(which_parm))
      warning(
        "'which_parm' has type 'double', but type 'integer' is expected. ",
        "'which_parm' was\ntruncated and converted to an integer. ",
        "It is strongly recommended to use an integer\nvector with indices to ",
        "avoid unexpected behavior."
      )
    } else if (
      length(which_parm) <= length(object$coefficients) &&
      purrr::is_integer(which_parm) &&
      all(which_parm >= 1L) &&
      all(which_parm <= length(object$coefficients))
    ) {
      which_parm_ind <- which_parm
    } else {
      stop(
        "which_parm must either be a logical vector the same length as the ",
        "coefficients\nor an integer vector with indices of the coefficient ",
        "vector."
      )
    }
    which_parm <- rep(FALSE, length(object$coefficients))
    which_parm[which_parm_ind] <- TRUE
  }
  # check level is a double between 0 and 1
  stopifnot(
    "level must be a number between 0 and 1" =
      is.numeric(level) && length(level) == 1 && level > 0 && level < 1
  )
  # check return_beta is a Boolean
  stopifnot(
    "return_beta must be Boolean" =
      isTRUE(return_beta) | isFALSE(return_beta)
  )
  # check n_grid
  if (
    !(is.null(n_grid) ||
      is.integer(n_grid) &&
      match(length(n_grid), c(1, sum(which_parm))) &&
      all(n_grid > 0))
  ) {
    stop("n_grid must be either NULL, a positive integer, or a vector of ",
         "\npositive integers the number of `TRUE`/indices in which_parm")
  }
  # check k
  if (
    !(is.null(k) ||
      is.integer(k) && length(k) == 1 && k > 0)
  ) {
    if (is.numeric(k) && length(k) == 1 && k > 0 && round(k) == k) {
      k <- as.integer(k)
    } else {
      stop("k must be either NULL or a positive integer")
    }
  }
  # check len is a positive real
  stopifnot(
    "len must be a number greater than 0" =
      is.numeric(len) && length(len) == 1 && len > 0
  )
  # check if convex optimizer is needed
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    stopifnot(
      "'fct_confint.lm' requires package 'CVXR' to be installed unless 'n_grid' or 'k' are specified." =
        requireNamespace("CVXR", quietly = TRUE)
    )
  }

  ### extract MLE parameters
  beta_hat <- coef(object)

  ### design matrix
  X <- model.matrix(object)[, which_parm, drop = FALSE]

  ### weight matrix
  rdf <- object$df.residual
  if (is.null(object$weights)) {
    rss <- sum(object$residuals^2)
  } else {
    rss <- sum(object$weights * object$residuals^2)
  }
  W <- rss / rdf # equivalent to summary(object)$sigma^2

  ### create object X^TWX on reduced set of covariates
  xtx_inv <- W * solve(crossprod(X))
  xtx_red <- solve(xtx_inv)

  ### solve convex optimization problem (unless n_grid or k are specified)
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    # total number of output dimensions
    n_fout <- length(f(beta_hat))

    # create progress bar with progressr. To display a progress bar the user
    # wraps the call to fct_confint in with_progress()
    pb <- progressr::progressor(steps = n_fout)

    # run optimizer using the user specified future plan
    env <- parent.frame()
    tryCatch(
      {
        ci_data <- furrr::future_map(
          .x = seq_len(n_fout),
          .options = furrr::furrr_options(
            packages = c("CVXR", "dplyr"),
            globals = c("f", "xtx_red", "beta_hat", "which_parm", "level",
                        "pb", "n_grid", "k", "ci_fct"),
            seed = TRUE
          ),
          .f = \(i) {
            pb()
            ci_fct(i = i,
                   f = f,
                   xtx_red = xtx_red,
                   beta_hat = beta_hat,
                   which_parm = which_parm,
                   level = level,
                   n_grid = n_grid,
                   k = k)
          }
        ) |>
          purrr::list_rbind()
        # return results using convex optimization
        return(ci_data)
      },
      # If an error occurs, check if the problem is f not following DCP rules
      error = \(e) ci_fct_error_handler(e, which_parm, env)
    )
  }

  ### contruct points around the estimated parameter vector defining directions
  ### to look for points on the boundary of the confidence set if requested
  if (!((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L))) {
    if (is.null(n_grid) || any(n_grid) == 0L) {
      # create k point uniformly on a sum(which_parm)-dimensional sphere
      delta <- matrix(rnorm(sum(which_parm) * k), ncol = sum(which_parm))
      norm <- apply(delta, 1, function(y) sqrt(sum(y^2)))
      delta <- t(delta / rep(norm / len, ncol(delta)))
      rownames(delta) <- names(beta_hat)[which_parm]
    } else {
      # create sum(which_parm)-dimensional grid with n_grid points in each direction
      if (length(n_grid) == 1L) {
        delta <- as.matrix(expand.grid(
          rep(list(seq(-len, len, length.out = n_grid)), sum(which_parm))
        ))
      } else {
        delta <- as.matrix(expand.grid(
          lapply(n_grid, function(y) seq(-len, len, length.out = y))
        ))
      }
      delta <- t(delta)
      rownames(delta) <- names(beta_hat)[which_parm]
    }
  }

  # create progress bar with progressr. To display a progress bar the user
  # wraps the call to fct_confint in with_progress()
  pb <- progressr::progressor(steps = ncol(delta))

  ### solve equation for scaling points to end up on boundary of confidence set
  a <- furrr::future_map_dbl(
    .x = seq_len(ncol(delta)),
    .options = furrr::furrr_options(
      globals = c("delta", "xtx_red", "pb", "ci_fct")
    ),
    .f = \(i) {
      pb()
      t(delta[, i, drop = FALSE]) %*% xtx_red %*% delta[, i, drop = FALSE] |>
        as.numeric()
    }
  )
  c <- rep(
    qchisq(level, length(beta_hat[which_parm])),
    ncol(delta)
  )
  suppressWarnings({alpha1 <- -sqrt(c / a)})
  suppressWarnings({alpha2 <- sqrt(c / a)})
  # remove delta values that lead to complex solutions
  which_alpha <- which(!(is.nan(alpha1) | is.infinite(alpha1)))
  alpha1 <- alpha1[which_alpha]
  alpha2 <- alpha2[which_alpha]
  delta_red <- delta[, which_alpha, drop = FALSE]
  cat("points on the boundary:", 2 * length(which_alpha), "\n")
  # determine coefficient values on boundary of confidence set
  beta_1 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_1 <- beta_1 +
    t(
      matrix(
        rep(alpha1, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_1) <- names(beta_hat)[which_parm]
  beta_2 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_2 <- beta_2 +
    t(
      matrix(
        rep(alpha2, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_2) <- names(beta_hat)[which_parm]
  suppressMessages({
    ci_data_1 <- lapply(
      seq_len(nrow(beta_1)),
      function(x) f(beta_1[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_1)))
      ) |>
      dplyr::as_tibble() |>
      dplyr::bind_cols(
        dplyr::tibble(
          estimate = f(beta_hat[which_parm])
        )
      )
  })
  suppressMessages({
    ci_data_2 <- lapply(
      seq_len(nrow(beta_2)),
      function(x) f(beta_2[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_2)) + nrow(beta_1))
      ) |>
      dplyr::as_tibble()
  })
  # combine negative and positive solutions
  ci_data <- dplyr::bind_cols(ci_data_1, ci_data_2) |>
    dplyr::select("estimate", dplyr::everything())

  ci_data$conf.low <- do.call(pmin, ci_data[, -1])
  ci_data$conf.high <- do.call(pmax, ci_data[, -1])

  if (return_beta) {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high", dplyr::everything())
    return(list(ci_data = ci_data, beta = dplyr::bind_rows(beta_1, beta_2)))
  } else {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high")
    return(ci_data)
  }
}

#' @rdname fct_confint
#' @export

fct_confint.glm <- function(
    object,
    f,
    which_parm = rep(TRUE, length(coef(object))),
    level = 0.95,
    return_beta = FALSE,
    n_grid = NULL,
    k = NULL,
    len = 0.1,
    ...
) {
  ### check input
  # check object class
  stopifnot("object must inherit from class 'glm'" = inherits(object, "glm"))
  # check f is a function
  stopifnot("f must be a function" = is.function(f))
  # convert which_parm to a logical vector
  if (!purrr::is_logical(which_parm)) {
    if (purrr::is_double(which_parm)) {
      which_parm_ind <- as.integer(round(which_parm))
      warning(
        "'which_parm' has type 'double', but type 'integer' is expected. ",
        "'which_parm' was\ntruncated and converted to an integer. ",
        "It is strongly recommended to use an integer\nvector with indices to ",
        "avoid unexpected behavior."
      )
    } else if (
      length(which_parm) <= length(object$coefficients) &&
      purrr::is_integer(which_parm) &&
      all(which_parm >= 1L) &&
      all(which_parm <= length(object$coefficients))
    ) {
      which_parm_ind <- which_parm
    } else {
      stop(
        paste(
          "which_parm must either be a logical vector the same length as the",
          "coefficients or an integer vector with indices of the coefficient",
          "vector."
        )
      )
    }
    which_parm <- rep(FALSE, length(object$coefficients))
    which_parm[which_parm_ind] <- TRUE
  }
  # check level is a double between 0 and 1
  stopifnot(
    "level must be a number between 0 and 1" =
      is.numeric(level) && length(level) == 1 && level > 0 && level < 1
  )
  # check return_beta is a Boolean
  stopifnot(
    "return_beta must be Boolean" =
      isTRUE(return_beta) | isFALSE(return_beta)
  )
  # check n_grid
  if (
    !(is.null(n_grid) ||
      is.integer(n_grid) &&
      match(length(n_grid), c(1, sum(which_parm))) &&
      all(n_grid > 0))
  ) {
    stop("n_grid must be either NULL, a positive integer, or a vector of ",
         "\npositive integers the number of `TRUE`/indices in which_parm")
  }
  # check k
  if (
    !(is.null(k) ||
      is.integer(k) && length(k) == 1 && k > 0)
  ) {
    if (is.numeric(k) && length(k) == 1 && k > 0 && round(k) == k) {
      k <- as.integer(k)
    } else {
      stop("k must be either NULL or a positive integer")
    }
  }
  # check len is a positive real
  stopifnot(
    "len must be a number greater than 0" =
      is.numeric(len) && length(len) == 1 && len > 0
  )
  # check if convex optimizer is needed
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    stopifnot(
      "'fct_confint.lm' requires package 'CVXR' to be installed unless 'n_grid' or 'k' are specified." =
        requireNamespace("CVXR", quietly = TRUE)
    )
  }

  ### extract MLE parameters
  beta_hat <- coef(object)

  ### design matrix
  X <- model.matrix(object)[, which_parm, drop = FALSE]

  ### weight matrix
  W <- object$weights

  ### create object X^TWX on reduced set of covariates
  #xtx_inv <- solve(crossprod(X * sqrt(W)))
  xtx_inv <- W * solve(crossprod(X))
  xtx_red <- solve(xtx_inv)

  ### solve convex optimization problem (unless n_grid or k are specified)
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    # total number of output dimensions
    n_fout <- length(f(beta_hat))

    # create progress bar with progressr. To display a progress bar the user
    # wraps the call to fct_confint in with_progress()
    pb <- progressr::progressor(steps = n_fout)

    # run optimizer using the user specified future plan
    env <- parent.frame()
    tryCatch(
      {
        ci_data <- furrr::future_map(
          .x = seq_len(n_fout),
          .options = furrr::furrr_options(
            packages = c("CVXR", "dplyr"),
            globals = c("f", "xtx_red", "beta_hat", "which_parm", "level",
                        "n_grid", "k", "pb", "ci_fct"),
            seed = TRUE
          ),
          .f = \(i) {
            pb()
            ci_fct(i = i,
                   f = f,
                   xtx_red = xtx_red,
                   beta_hat = beta_hat,
                   which_parm = which_parm,
                   level = level,
                   n_grid = n_grid,
                   k = k)
          }
        ) |>
          purrr::list_rbind()
        # return results using convex optimization
        return(ci_data)
      },
      # If an error occurs, check if the problem is f not following DCP rules
      error = \(e) ci_fct_error_handler(e, which_parm, env)
    )
  }

  ### contruct points around the estimated parameter vector defining directions
  ### to look for points on the boundary of the confidence set if requested
  if (!((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L))) {
    if (is.null(n_grid) || any(n_grid) == 0L) {
      # create k point uniformly on a sum(which_parm)-dimensional sphere
      delta <- matrix(rnorm(sum(which_parm) * k), ncol = sum(which_parm))
      norm <- apply(delta, 1, function(y) sqrt(sum(y^2)))
      delta <- t(delta / rep(norm / len, ncol(delta)))
      rownames(delta) <- names(beta_hat)[which_parm]
    } else {
      # create sum(which_parm)-dimensional grid with n_grid points in each direction
      if (length(n_grid) == 1L) {
        delta <- as.matrix(expand.grid(
          rep(list(seq(-len, len, length.out = n_grid)), sum(which_parm))
        ))
      } else {
        delta <- as.matrix(expand.grid(
          lapply(n_grid, function(y) seq(-len, len, length.out = y))
        ))
      }
      delta <- t(delta)
      rownames(delta) <- names(beta_hat)[which_parm]
    }
  }

  # create progress bar with progressr. To display a progress bar the user
  # wraps the call to fct_confint in with_progress()
  pb <- progressr::progressor(steps = ncol(delta))

  ### solve equation for scaling points to end up on boundary of confidence set
  a <- furrr::future_map_dbl(
    .x = seq_len(ncol(delta)),
    .options = furrr::furrr_options(
      globals = c("delta", "xtx_red", "pb", "ci_fct")
    ),
    .f = \(i) {
      pb()
      t(delta[, i, drop = FALSE]) %*% xtx_red %*% delta[, i, drop = FALSE] |>
        as.numeric()
    }
  )
  c <- rep(
    qchisq(level, length(beta_hat[which_parm])),
    ncol(delta)
  )
  suppressWarnings({alpha1 <- -sqrt(c / a)})
  suppressWarnings({alpha2 <- sqrt(c / a)})
  # remove delta values that lead to complex solutions
  which_alpha <- which(!(is.nan(alpha1) | is.infinite(alpha1)))
  alpha1 <- alpha1[which_alpha]
  alpha2 <- alpha2[which_alpha]
  delta_red <- delta[, which_alpha, drop = FALSE]
  cat("points on the boundary:", 2 * length(which_alpha), "\n")

  # determine coefficient values on boundary of confidence set
  beta_1 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_1 <- beta_1 +
    t(
      matrix(
        rep(alpha1, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_1) <- names(beta_hat)[which_parm]
  beta_2 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_2 <- beta_2 +
    t(
      matrix(
        rep(alpha2, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_2) <- names(beta_hat)[which_parm]
  suppressMessages({
    ci_data_1 <- lapply(
      seq_len(nrow(beta_1)),
      function(x) f(beta_1[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_1)))
      ) |>
      dplyr::as_tibble() |>
      dplyr::bind_cols(
        dplyr::tibble(
          estimate = f(beta_hat[which_parm])
        )
      )
  })
  suppressMessages({
    ci_data_2 <- lapply(
      seq_len(nrow(beta_2)),
      function(x) f(beta_2[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_2)) + nrow(beta_1))
      ) |>
      dplyr::as_tibble()
  })
  # combine negative and positive solutions
  ci_data <- dplyr::bind_cols(ci_data_1, ci_data_2) |>
    dplyr::select("estimate", dplyr::everything())

  ci_data$conf.low <- do.call(pmin, ci_data[, -1])
  ci_data$conf.high <- do.call(pmax, ci_data[, -1])

  if (return_beta) {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high", dplyr::everything())
    return(list(ci_data = ci_data, beta = dplyr::bind_rows(beta_1, beta_2)))
  } else {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high")
    return(ci_data)
  }
}

#' @rdname fct_confint
#' @export
# NOTE: the lms method is currently a copy of the lm method.
fct_confint.lms <- function(
    object,
    f,
    which_parm = rep(TRUE, length(coef(object))),
    level = 0.95,
    return_beta = FALSE,
    n_grid = NULL,
    k = NULL,
    len = 0.1,
    ...
) {
  ### check input
  # check object class
  stopifnot("object must inherit from class 'lms'" = inherits(object, "lms"))
  # check f is a function
  stopifnot("f must be a function" = is.function(f))
  # convert which_parm to a logical vector
  if (!purrr::is_logical(which_parm)) {
    if (purrr::is_double(which_parm)) {
      which_parm_ind <- as.integer(trunc(which_parm))
      warning(
        "'which_parm' has type 'double', but type 'integer' is expected. ",
        "'which_parm' was\ntruncated and converted to an integer. ",
        "It is strongly recommended to use an integer\nvector with indices to ",
        "avoid unexpected behavior."
      )
    } else if (
      length(which_parm) <= length(object$coefficients) &&
      purrr::is_integer(which_parm) &&
      all(which_parm >= 1L) &&
      all(which_parm <= length(object$coefficients))
    ) {
      which_parm_ind <- which_parm
    } else {
      stop(
        "which_parm must either be a logical vector the same length as the ",
        "coefficients\nor an integer vector with indices of the coefficient ",
        "vector."
      )
    }
    which_parm <- rep(FALSE, length(object$coefficients))
    which_parm[which_parm_ind] <- TRUE
  }
  # check level is a double between 0 and 1
  stopifnot(
    "level must be a number between 0 and 1" =
      is.numeric(level) && length(level) == 1 && level > 0 && level < 1
  )
  # check return_beta is a Boolean
  stopifnot(
    "return_beta must be Boolean" =
      isTRUE(return_beta) | isFALSE(return_beta)
  )
  # check n_grid
  if (
    !(is.null(n_grid) ||
      is.integer(n_grid) &&
      match(length(n_grid), c(1, sum(which_parm))) &&
      all(n_grid > 0))
  ) {
    stop("n_grid must be either NULL, a positive integer, or a vector of ",
         "\npositive integers the number of `TRUE`/indices in which_parm")
  }
  # check k
  if (
    !(is.null(k) ||
      is.integer(k) && length(k) == 1 && k > 0)
  ) {
    if (is.numeric(k) && length(k) == 1 && k > 0 && round(k) == k) {
      k <- as.integer(k)
    } else {
      stop("k must be either NULL or a positive integer")
    }
  }
  # check len is a positive real
  stopifnot(
    "len must be a number greater than 0" =
      is.numeric(len) && length(len) == 1 && len > 0
  )
  # check if convex optimizer is needed
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    stopifnot(
      "'fct_confint.lm' requires package 'CVXR' to be installed unless 'n_grid' or 'k' are specified." =
        requireNamespace("CVXR", quietly = TRUE)
    )
  }

  ### extract MLE parameters
  beta_hat <- coef(object)

  ### design matrix
  X <- model.matrix(object)[, which_parm, drop = FALSE]

  ### weight matrix
  rdf <- object$df.residual
  if (is.null(object$weights)) {
    rss <- sum(object$residuals^2)
  } else {
    rss <- sum(object$weights * object$residuals^2)
  }
  W <- rss / rdf # equivalent to summary(object)$sigma^2

  ### create object X^TWX on reduced set of covariates
  xtx_inv <- W * solve(crossprod(X))
  xtx_red <- solve(xtx_inv)

  ### solve convex optimization problem (unless n_grid or k are specified)
  if ((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L)) {
    # total number of output dimensions
    n_fout <- length(f(beta_hat))

    # create progress bar with progressr. To display a progress bar the user
    # wraps the call to fct_confint in with_progress()
    pb <- progressr::progressor(steps = n_fout)

    # run optimizer using the user specified future plan
    env <- parent.frame()
    tryCatch(
      {
        ci_data <- furrr::future_map(
          .x = seq_len(n_fout),
          .options = furrr::furrr_options(
            packages = c("CVXR", "dplyr"),
            globals = c("f", "xtx_red", "beta_hat", "which_parm", "level",
                        "n_grid", "k", "pb", "ci_fct"),
            seed = TRUE
          ),
          .f = \(i) {
            pb()
            ci_fct(i = i,
                   f = f,
                   xtx_red = xtx_red,
                   beta_hat = beta_hat,
                   which_parm = which_parm,
                   level = level,
                   n_grid = n_grid,
                   k = k)
          }
        ) |>
          purrr::list_rbind()
        # return results using convex optimization
        return(ci_data)
      },
      # If an error occurs, check if the problem is f not following DCP rules
      error = \(e) ci_fct_error_handler(e, which_parm, env)
    )
  }

  ### contruct points around the estimated parameter vector defining directions
  ### to look for points on the boundary of the confidence set if requested
  if (!((is.null(n_grid) || any(n_grid) == 0L) && (is.null(k) || k == 0L))) {
    if (is.null(n_grid) || any(n_grid) == 0L) {
      # create k point uniformly on a sum(which_parm)-dimensional sphere
      delta <- matrix(rnorm(sum(which_parm) * k), ncol = sum(which_parm))
      norm <- apply(delta, 1, function(y) sqrt(sum(y^2)))
      delta <- t(delta / rep(norm / len, ncol(delta)))
      rownames(delta) <- names(beta_hat)[which_parm]
    } else {
      # create sum(which_parm)-dimensional grid with n_grid points in each direction
      if (length(n_grid) == 1L) {
        delta <- as.matrix(expand.grid(
          rep(list(seq(-len, len, length.out = n_grid)), sum(which_parm))
        ))
      } else {
        delta <- as.matrix(expand.grid(
          lapply(n_grid, function(y) seq(-len, len, length.out = y))
        ))
      }
      delta <- t(delta)
      rownames(delta) <- names(beta_hat)[which_parm]
    }
  }

  # create progress bar with progressr. To display a progress bar the user
  # wraps the call to fct_confint in with_progress()
  pb <- progressr::progressor(steps = ncol(delta))

  ### solve equation for scaling points to end up on boundary of confidence set
  a <- furrr::future_map_dbl(
    .x = seq_len(ncol(delta)),
    .options = furrr::furrr_options(
      globals = c("delta", "xtx_red", "pb", "ci_fct")
    ),
    .f = \(i) {
      pb()
      t(delta[, i, drop = FALSE]) %*% xtx_red %*% delta[, i, drop = FALSE] |>
        as.numeric()
    }
  )
  c <- rep(
    qchisq(level, length(beta_hat[which_parm])),
    ncol(delta)
  )
  suppressWarnings({alpha1 <- -sqrt(c / a)})
  suppressWarnings({alpha2 <- sqrt(c / a)})
  # remove delta values that lead to complex solutions
  which_alpha <- which(!(is.nan(alpha1) | is.infinite(alpha1)))
  alpha1 <- alpha1[which_alpha]
  alpha2 <- alpha2[which_alpha]
  delta_red <- delta[, which_alpha, drop = FALSE]
  cat("points on the boundary:", 2 * length(which_alpha), "\n")
  # determine coefficient values on boundary of confidence set
  beta_1 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_1 <- beta_1 +
    t(
      matrix(
        rep(alpha1, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_1) <- names(beta_hat)[which_parm]
  beta_2 <- matrix(
    rep(beta_hat[which_parm], length(which_alpha)),
    byrow = TRUE,
    ncol = length(beta_hat[which_parm])
  )
  beta_2 <- beta_2 +
    t(
      matrix(
        rep(alpha2, nrow(delta_red)),
        byrow = TRUE,
        nrow = nrow(delta_red)
      ) * delta_red
    )
  colnames(beta_2) <- names(beta_hat)[which_parm]
  suppressMessages({
    ci_data_1 <- lapply(
      seq_len(nrow(beta_1)),
      function(x) f(beta_1[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_1)))
      ) |>
      dplyr::as_tibble() |>
      dplyr::bind_cols(
        dplyr::tibble(
          estimate = f(beta_hat[which_parm])
        )
      )
  })
  suppressMessages({
    ci_data_2 <- lapply(
      seq_len(nrow(beta_2)),
      function(x) f(beta_2[x, ])
    ) |>
      structure(
        names = paste0("ci_bound_", seq_len(nrow(beta_2)) + nrow(beta_1))
      ) |>
      dplyr::as_tibble()
  })
  # combine negative and positive solutions
  ci_data <- dplyr::bind_cols(ci_data_1, ci_data_2) |>
    dplyr::select("estimate", dplyr::everything())

  ci_data$conf.low <- do.call(pmin, ci_data[, -1])
  ci_data$conf.high <- do.call(pmax, ci_data[, -1])

  if (return_beta) {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high", dplyr::everything())
    return(list(ci_data = ci_data, beta = dplyr::bind_rows(beta_1, beta_2)))
  } else {
    ci_data <- ci_data |>
      dplyr::select("estimate", "conf.low", "conf.high")
    return(ci_data)
  }
}

#' @rdname fct_confint
#' @export
fct_confint.default <- function(
    object,
    f,
    which_parm = rep(TRUE, length(coef(object))),
    level = 0.95,
    ...
) {
  stop("Class is not supported. Object must have class lm, glm or lms.")
}

#' solve optimization problem for  CI bounds
#'
#' solve optimization problem for each coordinate of f, to obtain the uniform
#' limit.
#'
#' @param i An index for the point at which to solve for confidence limits.
#' @param f A function taking the parameter vector as its single argument, and
#'   returning a numeric vector.
#' @param xtx_red Reduced form of matrix *X^TX*.
#' @param beta_hat Vector of parameter estimates.
#' @param which_parm Vector indicating which parameters to include.
#' @param level The confidence level required.
#' @param n_grid Either `NULL` or an integer vector of length 1 or the number of
#'   `TRUE`/indices in which_parm. Specifies the number of grid points in each
#'   dimension of a grid with endpoints defined by len. If `NULL` or `0L`, will
#'   instead sample k points uniformly on a sphere.
#' @param k If n_grid is `NULL` or `0L`, the number of points to sample
#'   uniformly from a sphere.
#'
#' @return One row tibble with estimate and confidence limits.
#'
#' @examples 1+1

ci_fct <- function(i,
                   f,
                   xtx_red,
                   beta_hat,
                   which_parm,
                   level,
                   n_grid,
                   k) {
  # Disable problematic solvers
  CVXR::exclude_solvers("MOSEK")
  on.exit(CVXR::include_solvers("MOSEK"))
  # find ci_lower
  beta <- CVXR::Variable(dim(xtx_red)[1])
  objective <- CVXR::Minimize(f(beta)[i])
  constraint <- CVXR::quad_form(beta_hat[which_parm] - beta, xtx_red) <=
    qchisq(level, length(beta_hat[which_parm]))
  problem <- CVXR::Problem(objective, constraints = list(constraint))
  result_lower <- CVXR::psolve(problem)
  # find ci_upper
  beta <- CVXR::Variable(dim(xtx_red)[1])
  objective <- CVXR::Maximize(f(beta)[i])
  constraint <- CVXR::quad_form(beta_hat[which_parm] - beta, xtx_red) <=
    qchisq(level, length(beta_hat[which_parm]))
  problem <- CVXR::Problem(objective, constraints = list(constraint))
  result_upper <- CVXR::psolve(problem)
  # collect results
  out <- dplyr::tibble(
    estimate = f(beta_hat[which_parm])[i],
    ci_lower = result_lower,
    ci_upper = result_upper
  )
  return(out)
}

#' Handle errors returned by ci_fct
#'
#' @param e error returned by ci_fct
#' @param which_parm Either a logical vector the same length as the coefficient
#'   vector, with `TRUE` indicating a coefficient is used by `f`, or an integer
#'   vector with the indices of the coefficients used by `f`.
#' @param env environment to assign n_grid and k
#'
#' @return returns NULL if no stop command is executed.
#'
#' @examples 1+1

ci_fct_error_handler <- function(e, which_parm, env) {
  if (
    grepl(
      'task 1 failed - "Problem does not follow DCP rules."',
      e$message
    ) &
    interactive()
  ) {
    # ask user if they want to continue with a grid search.
    input <- readline(
      "f does not follow DCP rules. Cannot find confidence limits using convex optimizer.\nDo you want to continue with a grid search? (y/n)"
    )
    for (i in seq_len(3)) {
      if (input %in% c("y", "n")) {
        if (input == "n") stop("f does not follow DCP rules.")
        break
      } else {
        if (i == 3) stop("Failed to answer 'y' or 'n' to many times.")
        input <- readline(
          "Please input either 'y' to continue or 'n' to stop execution: "
        )
      }
    }
    for (i in seq_len(3)) {
      input <- readline("Please provide either 'n_grid' or 'k': ")
      if (input == "n_grid") {
        n_grid <- c()
        for (l in seq_len(sum(which_parm))) {
          for (j in seq_len(3)) {
            input <- readline(
              glue::glue("Please provide a positive integer (as 1, not 1L) for entry {l} of n_grid: ")
            )
            input <- suppressWarnings(as.integer(input))
            if (!is.na(input) && is.integer(input) && as.integer(input) > 0L) {
              n_grid[l] <- as.integer(input)
              break
            }
            if (j == 3) stop("Failed to answer with a positive integer to many times.")
          }
          if (l == 1) {
            input <- readline(
              glue::glue("n_grid must have length 1 or {sum(which_parm)}. Should n_grid have length {sum(which_parm)}? (y/n): ")
            )
            if (input %in% c("y", "n")) {
              if (input == "n") break
            } else {
              input <- readline(
                "Please input either 'y' to add to n_grid or 'n' to continue: "
              )
              if (input %in% c("y", "n")) {
                if (input == "n") break
              } else {
                stop("Answer not 'y' or 'n'. Execution stopped.")
              }
            }
          }
        }
        assign("n_grid", n_grid, env)
        break
      } else if (input == "k") {
        for (j in 1:3) {
          input <- readline("Please provide a positive integer (as 1, not 1L) for k: ")
          input <- suppressWarnings(as.integer(input))
          if (!is.na(input) && is.integer(input) && as.integer(input) > 0L) {
            assign("k", input, env)
            break
          }
          if (j == 3) stop("Failed to answer with a positive integer to many times.")
        }
        break
      }
      if (i == 3) stop("Failed to answer 'n_grid' or 'k' to many times")
    }
  } else {
    stop(e)
  }

  return(invisible(NULL))
}
