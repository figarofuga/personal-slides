#' Try Catch with Warning Handling
#'
#' @param expr 	An expression to be evaluated.
#' @param character A logical indicating if the returned error and warning
#'   should be characters (`character` = `TRUE`) or language
#'   (`character` = `FALSE`).
#'
#' @return
#' The `try_catch_warnings()` funciton returns a list with three elements
#' * `values` is the evaluated `expr` or `NULL` if the evaluations throws an
#'    error.
#' * `warning` is any warning given while evaluating `expr`. When `character` =
#'   `FALSE`, the default, `warning` is a \link[base]{simpleWarning}, otherwise
#'   it is a character.
#' * `error` is any error given while trying to evaluate `expr`. When
#'   `character` = `FALSE`, the default, `error` is a \link[base]{simpleError},
#'   otherwise it is a character.
#'
#' @examples
#' # No errors or warnings
#' try_catch_warnings(log(2))
#'
#' # Warnings
#' try_catch_warnings(log(-1))
#'
#' # Errors
#' try_catch_warnings(stop("Error Message"))
#' try_catch_warnings(stop("Error Message"), character = TRUE)
#'
#' @export
try_catch_warnings <- function(expr, character = FALSE){

  if (character) {
    warn <- err <- ""
    warning_handler <- function(w) {
      warn <<- w$message
      invokeRestart("muffleWarning")
    }
    error_handler <- function(e) {
      err <<- e$message
      NULL
    }
  } else {
    warn <- err <- NULL
    warning_handler <- function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    }
    error_handler <- function(e) {
      err <<- e
      NULL
    }
  }

  value <- tryCatch(
    withCallingHandlers(expr, warning = warning_handler),
    error = error_handler
  )
  return(
    list(
      value = value,
      warning = warn,
      error = err
    )
  )
}



