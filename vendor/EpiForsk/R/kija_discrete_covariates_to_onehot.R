#' One-hot encode factors
#'
#' Convert factors in a data frame to one-hot encoding.
#'
#' @param df A data frame, data frame extension (e.g. a tibble), or a lazy data
#'   frame (e.g. from dbplyr or dtplyr).
#' @param factors <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions naming factors in df to one-hot encode.
#'
#' @returns Data frame with one-hot encoded factors. One-hot encoded columns
#'   have names `{fct_nm}_{lvl_nm}`.
#'
#' @author KIJA
#'
#' @examples
#' mtcars |>
#' dplyr::mutate(dplyr::across(c(2, 8:11), factor)) |>
#'  as.data.frame() |>
#'  DiscreteCovariatesToOneHot(cyl)
#' mtcars |>
#' dplyr::mutate(dplyr::across(c(2, 8:11), factor)) |>
#'  as.data.frame() |>
#'  DiscreteCovariatesToOneHot(c(2, 8:11))
#'
#' @export

DiscreteCovariatesToOneHot <- function(df,
                                       factors = dplyr::everything()) {
  if (!inherits(df, "data.frame")) {
    stop("df must be a data.frame or data.frame like object.")
  }
  df_f <- df |> dplyr::select({{ factors }})
  df_r <- df |> dplyr::select(!({{ factors }}))
  factor_id <- c()
  for (i in seq_along(df_f)) {
    if (!is.factor(df_f[[i]])) {
      factor_id <- c(factor_id, i)
    }
  }
  if (length(factor_id) > 0) {
    if (length(factor_id) == 1) {
      stop(
        glue::glue(
          "{names(df_f)[factor_id]} is not a factor.",
          "All covariates selected by 'factors' must be factors."
        )
      )
    } else {
      nf <- paste(names(df_f)[factor_id], collapse = ", ")
      stop(
        glue::glue(
          "{nf} are not factors. All covariates selected by 'factors' must be factors."
        )
      )
    }
  }
  df_o <-
    purrr::map(
      names(df_f),
      \(x) {
        out <- model.matrix(~ df_f[[x]] + 0)
        dimnames(out)[[2]] <- paste0(x, "_", levels(df_f[[x]]))
        return(out)
      }
    ) |>
    purrr::map(dplyr::as_tibble) |>
    purrr::list_cbind()
  return(dplyr::bind_cols(df_r, df_o))
}
