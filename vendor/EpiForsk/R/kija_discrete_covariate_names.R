#' Extract discrete covariate names
#'
#' Detect elements in covariates which match a string from the discrete_covariates
#' argument.
#'
#' @param covariates character, names of covariates
#' @param discrete_covariates character, names of discrete covariates. Currently
#' it is assumed that discrete covariates are one-hot encoded with naming in
#' covariates following `{fct_nm}_{lvl_nm}`.
#'
#' @return A character vector with elements from covariates matching the names
#' supplied in discrete_covariates.
#'
#' @author KIJA
#'
#' @examples
#' one_hot_df <- mtcars |>
#'   dplyr::mutate(across(c(2, 8:11), factor)) |>
#'   as.data.frame() |>
#'   DiscreteCovariatesToOneHot(cyl)
#' EpiForsk:::DiscreteCovariateNames(colnames(one_hot_df), c("cyl"))

DiscreteCovariateNames <- function(covariates,
                                   discrete_covariates = NULL) {
  if(!is.character(covariates))  stop("covariates must be a character vector")

  if(is.null(discrete_covariates)) {
    return(discrete_covariates)
  } else {
    if(!is.character(discrete_covariates)) {
      stop("discrete_covariates must be NULL or a character vector")
    }

    return(
      grep(
        paste0("^(",
               paste0(discrete_covariates,
                      collapse = "|"),
               ")"), covariates,
        value = TRUE
      )
    )
  }
}
