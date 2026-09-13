#' Merging Many Data Frames with Name Handling
#'
#' Function to join/merge multiple data.frames with one or more common variable
#' names.
#'
#' @param by A join specification created with
#'   \code{\link[dplyr:join_by]{join_by()}}, or a character vector of variables
#'   to join by. The `by` must be present in all data frames `first_data` and
#'   `...`.
#' @param first_data A data frame (presented on the left in the final table).
#' @param ... Data frames to merge onto `first_data`.
#'
#' @return The `many_merge()` function returns a data frame.
#'
#' @author ASO
#'
#' @examples
#'
#' # Create some dummy data
#' testdata_id <- c(1:10)
#' var1 <- rep(letters[1:5], times = 2)
#' var2 <- letters[1:10]
#' var3 <- rep(letters[11:12], times = 5)
#' var4 <- letters[13:22]
#' var5 <- letters[11:20]
#'
#' # Rename alle the variables to "var"
#' data1 <- data.frame(testdata_id, var = var1)
#' data2 <- data.frame(testdata_id, var = var2)
#' data3 <- data.frame(testdata_id, var = var3)
#' data4 <- data.frame(testdata_id, var = var4)
#' data5 <- data.frame(testdata_id, var = var5)
#'
#' # Many merge
#' final_data <- many_merge(
#'   by = c("testdata_id"),
#'   data1,
#'   data2,
#'   data3,
#'   data4,
#'   data5
#' )
#'
#' @export

many_merge <- function(by, first_data, ...) {
  func_table <- first_data
  func_list <- list(...)
  j <- 1
  for (i in func_list) {
    j <- (j + 1)
    func_table <- dplyr::full_join(
      func_table,
      i,
      by = by,
      suffix = c("", paste(".", j, sep = "")),
      multiple = "all"
    )
  }
  return(func_table)
}
