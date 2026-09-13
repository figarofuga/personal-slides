#' Bind lists of list of multiple data frames by row
#'
#' Row binds the matching innermost data frames in a list of lists. This is
#' essentially a list inversion [`purrr::list_transpose()`] with row-binding
#' [`dplyr::bind_rows()`]
#'
#' @param list A list of lists of `data.frame`s where matching innermost
#'   elements must be bound together row-wise.
#'
#' @return
#' A list of `data.frame`s with the combined information from the inputted list.
#'
#' @examples
#' # A simple example
#' lst <- lapply(1:5, function(x) {
#'   list(
#'     "A" = data.frame("first" = x, "second" = rnorm(x)),
#'     "B" = data.frame("info" = 1, "other" = 3)
#'   )
#' })
#' braid_rows(lst)
#'
#' # An example with an additional layer and jagged innermost info
#' lapply(c(28, 186, 35), function(len) {
#'   lapply(c("a", "b"), function(x) {
#'     res <- list(
#'       "descriptive" = data.frame(
#'          risk = len,
#'          event = x,
#'          var = 1,
#'          other = 2
#'        ),
#'       "results" = data.frame(
#'          risk = len,
#'          event = x,
#'          important = 4:7,
#'          new = 3:6
#'       )
#'     )
#'     if (len < 30) {
#'       res <- c(res, list("additional" = data.frame(helps = "extra data")))
#'     }
#'     return(res)
#'   }) |> braid_rows()
#' }) |> braid_rows()
#'
#'
#' @export
braid_rows <- function(list) {
  ulist <- unlist(list, recursive = FALSE)
  if (is.null(names(ulist))) {
    template <- seq_len(max(lengths(list)))
  } else if (any(names(ulist) == "")) {
    template <- seq_len(max(lengths(list)))
    message("Some data frames are unnamed. braiding by location in inner list.")
  } else {
    template <- unique(names(ulist))
  }

  # checking input is data frame based
  if (
    !all(
      sapply(
        ulist,
        function(ls) inherits(ls, "data.frame")
      )
    )
  ) {
    stop("All elements must be data frames")
  }

  return(
    purrr::list_transpose(
      list,
      template = template,
      simplify = FALSE
      ) |>
      lapply(function(ls) ls |> dplyr::bind_rows())
  )
}
