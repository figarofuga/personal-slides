#' Wrapper around lm for sibling design
#'
#' Fits a linear model using demeaned data. Useful for sibling design.
#'
#' @param formula A formula, used to create a model matrix with demeaned columns.
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#' data frame (e.g. from dbplyr or dtplyr).
#' @param grp_id <[`data-masking`][dplyr::dplyr_data_masking]> One unquoted expression
#' naming the id variable in data defining the groups to demean,
#' e.g. sibling groups.
#' @param obs_id <[`data-masking`][dplyr::dplyr_data_masking]> Optional, One unquoted
#' expression naming an id variable to keep track of the input data order.
#' @param ... Additional arguments to be passed to [lm][stats::lm()]. In print,
#' additional arguments are ignored without warning.
#'
#' @returns
#' A list with class `c("lms", "lm")`. Contains the output from `lm` applied
#' to demeaned data according to `formula`, as well as the original data and the
#' provided formula.
#'
#' @details \code{lms} estimates parameters in the linear model
#' \deqn{y_{ij_i}=\alpha_i+x_{ij_i}^T\beta + \varepsilon_{ij_i}}{
#' y_(ij_i)=\alpha_i+x_(ij_i)^T\beta + \varepsilon_(ij_i)}
#' where \eqn{\alpha_i}{%\alpha_i} is a group (e.g. sibling group)
#' specific intercept and \eqn{x_{ij_i}}{%x_(ij_i)} are covariate values for
#' observation \eqn{j_i}{%j_i} in group i.
#' \eqn{\varepsilon_{ij_i}\sim N(0, \sigma^2)}{%\varepsilon_(ij_i)~N(0, \sigma^2)}
#' is a normally distributed error term. It is assumed that interest is in
#' estimating the vector \eqn{\beta}{%\beta} while \eqn{\alpha_{i}}{%\alpha_(i)}
#' are nuisance parameters. Estimation of \eqn{\beta} uses the mean deviation
#' method, where
#' \deqn{y_{ij_i}^{'}=y_{ij_i}-y_i}{y_(ij_i)^(')=y_(ij_i)-y_i}
#' is regressed on
#' \deqn{x_{ij_i}^{'}=x_{ij_i}-x_i.}{x_(ij_i)^(')=x_(ij_i)-x_i.}
#' Here \eqn{y_i} and \eqn{x_i} refers to the mean of y and x in group i.
#' \cr `lms` can keep track of observations by providing a unique identifier
#' column to `obs_id`. `lms` will return `obs_id` so it matches the order of
#' observations in model.\cr
#' `lms` only supports syntactic covariate names. Using a non-syntactic name
#' risks returning an error, e.g if names end in + or -.
#'
#' @author
#' KIJA
#'
#' @examples
#' \donttest{
#' sib_id <- sample(200, 1000, replace = TRUE)
#' sib_out <- rnorm(200)
#' x1 <- rnorm(1000)
#' x2 <- rnorm(1000) + sib_out[sib_id] + x1
#' y <- rnorm(1000, 1, 0.5) + 2 * sib_out[sib_id] - x1 + 2 * x2
#' data <- data.frame(
#'   x1 = x1,
#'   x2 = x2,
#'   y = y,
#'   sib_id = sib_id,
#'   obs_id = 1:1000
#' )
#' mod_lm <- lm(y ~ x1 + x2, data) # OLS model
#' mod_lm_grp <- lm(y ~ x1 + x2 + factor(sib_id), data) # OLS with grp
#' mod_lms <- lms(y ~ x1 + x2, data, sib_id, obs_id) # conditional model
#' summary(mod_lm)
#' coef(mod_lm_grp)[1:3]
#' summary(mod_lms)
#' print(mod_lms)
#' }
#' @export

lms <- function (formula, data, grp_id, obs_id = NULL, ...)
{
  grp_id <- dplyr::ensym(grp_id)
  tryCatch({obs_id <- dplyr::ensym(obs_id)}, error = function(e) e)
  if (length(formula) < 3) stop("'formula' must have a LHS")
  # remove intercept
  formula <- update.formula(formula, . ~ . - 1)
  # create model matrix from formula:
  model_matrix <- model.matrix(
    formula[-2],
    data = data
  ) |>
    dplyr::as_tibble() |>
    dplyr::bind_cols(
      data |> dplyr::select({{ grp_id }})
    )
  # add obs_id variable to model_matrix:
  if (!is.null(obs_id)) {
    model_matrix <- model_matrix |>
      dplyr::bind_cols(
        data |> dplyr::select({{ obs_id }})
      )
  }
  # demean data by grp_id
  model_matrix_trans <- data.table::as.data.table(model_matrix)
  nm <- names(model_matrix_trans)[
    seq_len(length(model_matrix_trans) - 1 - !is.null(obs_id))
  ]
  model_matrix_trans[
    ,
    (nm) := lapply(.SD, \(x) x - mean(x)),
    by = grp_id,
    .SDcols = nm]
  model_matrix_trans <- dplyr::as_tibble(model_matrix_trans)
  # demean outcome by grp_id
  outcome_trans <- data.table::as.data.table(
    data |>
      dplyr::select(
        !!formula[[2]],
        dplyr::all_of(grp_id),
        dplyr::all_of(obs_id)
      )
  )
  nm <- as.character(formula[[2]])
  outcome_trans[
    ,
    (nm) := lapply(.SD, \(x) x - mean(x)),
    by = grp_id,
    .SDcols = nm
  ]
  outcome_trans <- dplyr::as_tibble(outcome_trans)
  # combine data
  if (!is.null(obs_id)) {
    mod_data <- dplyr::left_join(
      model_matrix_trans,
      outcome_trans,
      by = c(paste0(obs_id), paste0(grp_id))
    )
  } else {
    mod_data <- dplyr::bind_cols(
      model_matrix_trans,
      outcome_trans |> dplyr::select(-dplyr::all_of(grp_id))
    )
  }
  # OLS model fitting demeaned data
  mod <- lm(
    formula = formula(paste0(formula[[2]], "~ . - 1")),
    data = mod_data |>
      dplyr::select(-c(dplyr::all_of(obs_id), dplyr::all_of(grp_id))),
    ...
  )
  # return enriched OLS model
  out <- c(
    unclass(mod),
    list(
      formula = formula,
      data = data,
      grp_id = model_matrix_trans[grp_id], # follows modified order in model
      obs_id = if (!is.null(obs_id)) model_matrix_trans[obs_id]
    )
  )
  class(out) <- c("lms", "lm")
  return(out)
}

#' @rdname lms
#' @param x An S3 object with class lms.
#' @param digits The number of significant digits to be passed to
#' \link[base]{format}(\link[stats]{coef}(x), .) when \link[base]{print}()ing.
#' @export

print.lms <- function (x, digits = max(3L, getOption("digits") - 3L), ...)
{
  cat("\nNumber of observations: ", nrow(x$data),
      "\nNumber of groups: ", nrow(x$grp_id |> dplyr::distinct()),
      "\n\n", sep = "")
  if (length(coef(x))) {
    cat("Coefficients:\n")
    print.default(format(coef(x), digits = digits), print.gap = 2L,
                  quote = FALSE)
  } else cat("No coefficients\n")
  cat("\n")
  invisible(x)
}
