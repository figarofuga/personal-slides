#' Calculate CATE on a surface in the covariate space
#'
#' Calculates CATE estimates from a causal forest object on a specified surface
#' within the covariate space.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}(). Alternatively, and object of class
#'   `regression_forest`, as returned by \link[grf]{regression_forest}().
#' @param continuous_covariates character, continuous covariates to use for the
#'   surface. Must match names in `forest$X.orig`.
#' @param discrete_covariates character, discrete covariates to use for the
#'   surface. Note that discrete covariates are currently assumed to be one-hot
#'   encoded with columns named `{fct_nm}_{lvl_nm}`. Names supplied to
#'   discrete_covariates should match `fct_nm`.
#' @param estimate_variance boolean, If `TRUE`, the variance of CATE estimates
#'   is computed.
#' @param grid list, points in which to predict CATE along continuous
#'   covariates. Index i in the list should contain a numeric vectors with
#'   either a single integer, specifying the number of equally spaced points
#'   within the range of the i'th continuous covariate in which to calculate the
#'   CATE, or a numeric vector with manually specified points in which to
#'   calculate the CATE along the i'th continuous covariate. If all elements of
#'   grid specify a number of points, this can be supplied using a numeric
#'   vector. If the list is named, the names must match the continuous
#'   covariates. grid will be reordered to match the order of
#'   continuous_covariates.
#' @param fixed_covariate_fct Function applied to covariates not in the
#'   sub-surface which returns the fixed value of the covariate used to
#'   calculate the CATE. Must be specified in one of the following ways:
#'   - A named function, e.g. `mean`.
#'   - An anonymous function, e.g. \code{\(x) x + 1} or \code{function(x) x + 1}.
#'   - A formula, e.g. \code{~ .x + 1}. You must use `.x` to refer to the
#'   first argument. Only recommended if you require backward compatibility with
#'   older versions of R.
#'   - A string, integer, or list, e.g. `"idx"`, `1`, or `list("idx", 1)` which
#'   are shorthand for \code{\(x) purrr::pluck(x, "idx")}, \code{\(x)
#'   purrr::pluck(x, 1)}, and \code{\(x) purrr::pluck(x, "idx", 1)}
#'   respectively. Optionally supply `.default` to set a default value if the
#'   indexed element is `NULL` or does not exist.
#' @param other_discrete A data frame, data frame extension (e.g. a tibble), or
#'   a lazy data frame (e.g. from dbplyr or dtplyr) with columns `covs` and
#'   `lvl`. Used to specify the level of each discrete covariate to use when
#'   calculating the CATE. assumes the use of one-hot encoding. `covs` must
#'   contain the name of discrete covariates, and `lvl` the level to use. Set to
#'   `NULL` if none of the fixed covariates are discrete using one-hot-encoding.
#' @param max_predict_size integer, maximum number of examples to predict at a
#'   time. If the surface has more points than max_predict_size, the prediction
#'   is split up into an appropriate number of chunks.
#' @param num_threads Number of threads used in training. If set to `NULL`, the
#'   software automatically selects an appropriate amount.
#'
#' @return Tibble with the predicted CATE's on the specified surface in the
#'   covariate space. The tibble has columns for each covariate used to train
#'   the input forest, as well as columns output from
#'   \link[grf]{predict.causal_forest}().
#'
#' @author KIJA
#'
#' @examples
#' \donttest{
#' n <- 1000
#' p <- 3
#' X <- matrix(rnorm(n * p), n, p) |> as.data.frame()
#' X_d <- data.frame(
#'   X_d1 = factor(sample(1:3, n, replace = TRUE)),
#'   X_d2 = factor(sample(1:3, n, replace = TRUE))
#' )
#' X_d <- DiscreteCovariatesToOneHot(X_d)
#' X <- cbind(X, X_d)
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' cate_surface <- CATESurface(
#'   cf,
#'   continuous_covariates = paste0("V", 1:2),
#'   discrete_covariates = "X_d1",
#'   grid = list(
#'     V1 = 10,
#'     V2 = -5:5
#'   ),
#'   other_discrete = data.frame(
#'     covs = "X_d2",
#'     lvl = "4"
#'   )
#' )
#' }
#'
#' @export

CATESurface <- function(forest,
                        continuous_covariates,
                        discrete_covariates,
                        estimate_variance = TRUE,
                        grid = 100,
                        fixed_covariate_fct = median,
                        other_discrete = NULL,
                        max_predict_size = 100000,
                        num_threads = 2) {
  # convert grid to list
  if (is.numeric(grid) && length(grid) == 1) {
    grid <- rep(grid, length(continuous_covariates))
  }
  grid <- as.list(grid)

  # check input
  stopifnot(
    "'forest' must be an object of class causal_forest or regression_forest" =
      any(c("causal_forest", "regression_forest") %in% class(forest))
  )
  stopifnot(
    "continuous_covariates must be a character vector" =
      is.character(continuous_covariates)
  )
  stopifnot(
    "discrete_covariates must be a character vector" =
      is.character(continuous_covariates)
  )
  stopifnot(
    "estimate_variance must be a boolean (TRUE or FALSE)" =
      isTRUE(estimate_variance) | isFALSE(estimate_variance)
  )
  stopifnot(
    "'grid' must have the same length as 'continuous_covariates'" =
      length(grid) == length(continuous_covariates)
  )
  lapply(
    grid,
    function(x) {
      if (length(x) == 1 && !(is.numeric(x) && trunc(x) > 0.9)) {
        stop("elements of 'grid' with length 1 must be positive integers.")
      }
      if (!(is.numeric(x))) {
        stop("elements of 'grid' with length >1 must be numeric vectors.")
      }
      invisible(return(NULL))
    }
  )
  if (!is.null(names(grid))) {
    stopifnot(
      "grid must be named after continuous_covariates." =
        all(names(grid) %in% continuous_covariates) &&
        all(continuous_covariates %in% names(grid))
    )
  }

  # if named, reorder grid according to continuous_covariates
  if (!is.null(names(grid))) {
    grid <- grid[continuous_covariates]
  }

  # covariates in causal forest
  if (is.null(colnames(forest$X.orig))) {
    warning(
      "Covariates used to train forest are unnamed. Names X_{colnr} are created.",
      immediate. = TRUE
    )
    colnames(forest$X.orig) <- paste0("X_", seq_len(ncol(forest$X.orig)))
  }
  covariates <- colnames(forest$X.orig)

  # formula with covariates
  fmla <- formula(
    paste0("~ 0 + ", paste0("`", covariates, "`", collapse = "+"))
  )

  # split covariates by use in sub-surface or not use in sub-surface
  discrete_covariates_input <- discrete_covariates
  discrete_covariates <- DiscreteCovariateNames(covariates, discrete_covariates)
  other_covariates <- stringr::str_subset(
    covariates,
    paste0(
      "^(",
      paste0(c(discrete_covariates, continuous_covariates), collapse = "|"),
      ")"
    ),
    negate = TRUE
  )

  # Generate grid of covariate values in which to evaluate CATE
  X_continuous <- dplyr::as_tibble(forest$X.orig) |>
    dplyr::select(dplyr::all_of(continuous_covariates))
  data_grid <- purrr::map2(
    X_continuous,
    grid,
    function(x, y) {
      if (length(y) == 1) {
        return(seq(range(x)[1], range(x)[2], length.out = y))
      } else {
        return(y)
      }
    }
  )
  discrete_values <- vector("list")
  for(i in seq_along(discrete_covariates_input)) {
    covariate_temp <- grep(
      paste0("^", discrete_covariates_input[i]),
      covariates,
      value = TRUE
    )
    tibble_temp <- diag(1, length(covariate_temp), length(covariate_temp)) |>
      dplyr::as_tibble(.name_repair = "minimal") |>
      purrr::set_names(nm = covariate_temp)
    discrete_values[[i]] <- tibble_temp
  }
  names(discrete_values) <- discrete_covariates_input
  data_grid <- c(data_grid, discrete_values)
  X_other <- dplyr::as_tibble(forest$X.orig) |>
    dplyr::select(dplyr::all_of(other_covariates))
  fixed_values <- purrr::map_dbl(X_other, fixed_covariate_fct)
  if(!is.null(other_discrete)) {
    fixed_values[
      grep(
        paste0(
          "^(",
          paste0(other_discrete$covs, collapse = "|"),
          ")"
        ),
        names(fixed_values),
        value = TRUE
      )
    ] <- 0
    fixed_values[
      grep(
        paste0(
          "^(",
          paste0(
            other_discrete$covs,
            "_",
            other_discrete$lvl,
            "$",
            collapse = "|"
          ),
          ")"
        ),
        names(fixed_values),
        value = TRUE
      )
    ] <- 1
  }
  fixed_values <- as.list(fixed_values)
  data_grid <- c(data_grid, fixed_values)
  X_grid <- purrr::exec(tidyr::expand_grid, !!!data_grid)
  X_grid <- tidyr::unnest(
    X_grid,
    cols = dplyr::all_of(discrete_covariates_input)
  )

  # predict CATE in grid of points on surface
  if(max_predict_size < nrow(X_grid)) {
    X_grid_split <-
      split(
        seq_len(nrow(X_grid)), ceiling(seq_len(nrow(X_grid)) / max_predict_size)
      )
    tau_hat <- X_grid_split |>
      purrr::map(
        function(x) {
          predict(
            object = forest,
            newdata = X_grid[x,],
            estimate.variance = estimate_variance,
            num.threads = num_threads)
        }
      ) |>
      purrr::list_rbind()
  } else {
    tau_hat <- predict(
      object = forest,
      newdata = X_grid,
      estimate.variance = estimate_variance,
      num.threads = num_threads)
  }

  # Return predicted CATE's on surface of covariate space
  return(
    X_grid |>
      dplyr::bind_cols(tau_hat)
  )
}
