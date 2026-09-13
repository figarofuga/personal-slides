#' Join many data frames with name handling
#'
#' Function to join multiple data.frames with one or more common
#' variable names.
#'
#' @param ... Data frames to join. Each argument in `...` must either be
#' a data.frame or a list of data.frames.
#' @param .by  A character vector of variables to join by. The `.by` must be
#'   present in all data frames in `...`.
#'
#' @returns
#' The `multi_join()` function returns a data frame.
#'
#' @author KIJA
#'
#' @examples
#' # Create some dummy data
#' testdata_id <- c(1:10)
#' a1 <- 1:10; b1 <- rep(letters[1:5], times = 2); c1 <- runif(10)
#' a2 <- 11:20; b2 <- letters[1:10]
#' a3 <- 21:30; b3 <- rep(letters[11:12], times = 5)
#' a4 <- 31:40; b4 <- letters[13:22]
#' a5 <- 41:50; b5 <- letters[11:20]
#'
#' # Define data.frames with common key and shared column names
#' data1 <- data.frame(testdata_id, a = a1, b = b1, c = c1)
#' data2 <- data.frame(testdata_id, b = b2, a = a2)
#' data3 <- data.frame(testdata_id, a = a3, b = b3)
#' data4 <- data.frame(testdata_id, a = a4, b = b4)
#' data5 <- data.frame(testdata_id, a = a5, b = b5)
#'
#' # multi join
#' final_data <- multi_join(
#'   data1,
#'   data2,
#'   data3,
#'   data4,
#'   data5,
#'   .by = "testdata_id"
#' )
#'
#' @export

# assumes .by is a string naming a common key across all input data.frames
multi_join <- function(..., .by) {
  # build list of input data.frames
  x <- purrr::list_flatten(list(...))
  # test input
  if(!all(unlist(lapply(x, is.data.frame)))) {
    stop("'...' must contain data frames or lists of data frames")
  }
  common_names <- x |>
    lapply(names) |>
    unlist() |>
    table() |>
    (\(.x) dimnames(.x)[[1]][which(.x == length(x))])()
  if(!(is.character(.by) && all(.by %in% common_names))) {
    stop(
      paste0("'.by' must be a character vector with ",
             "names common to all input data frames"
      )
    )
  }
  # get repeated names from input data.frames
  x_names <- x |>
    lapply(names) |>
    unlist() |>
    table() |>
    (\(.x) dimnames(.x)[[1]][which(.x > 1)])() |>
    (\(.x) .x[-which(.x %in% .by)])()
  # contruct matrix with repeated column names from each data.frame
  names <- matrix(
    NA,
    length(x),
    length(x_names),
    dimnames = list(NULL, x_names))
  for (i in seq_along(x)) {
    i_names <- x_names[which(x_names %in% names(x[[i]]))]
    names[i, i_names] <- i_names
  }
  # add suffix to repeated names
  for (j in x_names) {
    new_names <- paste0(
      names[!is.na(names[, j]), j],
      paste0(".", seq_len(sum(!is.na(names[, j]))))
    )
    new_name_rows <- which(!is.na(names[, j]))
    for (k in seq_along(new_name_rows)) {
      names(x[[new_name_rows[k]]])[
        which(names(x[[new_name_rows[k]]]) == j)
      ] <- new_names[k]
    }
  }
  # full_join over each input data.frame
  purrr::reduce(.x = x, .f = dplyr::full_join, by = .by)
}
