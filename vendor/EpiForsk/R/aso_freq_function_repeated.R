#' Wrapper for `freq_function()` to get frequencies for many variables in one
#' go.
#'
#' A method for making multiple 1- and 2-way frequency tables with percentages
#' and odds ratios.
#'
#' @param normaldata A data frame or data frame extension (e.g. a tibble).
#' @param var1 A character vector with the names of the first variable to get
#'   frequencies from for each frequency table.
#' @param var2 An optional character naming the second variable to get
#'   frequencies. If `NULL` (standard) 1-way frequency tables of only variables
#'   in `var1` are created, and if `var2` is specified 2-way tables are
#'   returned.
#' @param by_vars An optional character vector naming variables in `normal_data`
#'   to stratify the calculations and output by. That is, ALL calculations will
#'   be made within the combinations of variables in the vector, hence it's
#'   possible to get N and % for many groups in one go.
#' @param include_NA A logical. If `FALSE` (standard) missing variables (`NA`'s)
#'   will be removed from `var1` and `var2`. Any missing values in `by_vars`
#'   will not be removed. If `TRUE` all missing values will be included in
#'   calculations and the output.
#' @param values_to_remove An optional character vector. When specified all
#'   values from `var1` and `var2` found in `values_to_remove` will be removed
#'   from the calculations and output.
#' @param weightvar An optional character naming a column in `normaldata` with
#'   numeric weights for each observation. If `NULL` (standard) all observations
#'   have weight 1.
#' @param textvar An optional character. When specified `textvar` is added to
#'   the resulting table as a comment. When `NULL` (standard) no such text
#'   addition is made.
#' @param number_decimals A numeric indicating the number of decimals to show on
#'   percentages and weighted frequencies in the combined frequency and percent
#'   variables.
#' @param output A character indicating the output type wanted:
#'   * `"all"` - will give ALL output from tables. In many cases unnecessary and
#'   hard to get an overview of. This is set as the standard.
#'   * `"numeric"` - will give frequencies and percents as numeric variables
#'   only, thus the number_decimals option is not in effect. This option might
#'   be useful when making figures/graphs.
#'   * "col" - will only give unweighted number of observations and weighted
#'   column percent (if weights are used, otherwise unweighted)
#'   * `"colw"` - will only give weighted number of observations and weighted
#'   column percent (if weights are used, otherwise unweighted)
#'   * `"row"`- will only give unweighted number of observations and weighted
#'   row percent (if weights are used, otherwise unweighted). Only works in
#'   two-way tables (`var2` is specified)
#'   * `"roww"` - will only give weighted number of oberservations and weighted
#'   column percent (if weights are used, otherwise unweighted). Only works in
#'   two-way tables (`var2` is specified)
#'   * `"total"` - will only give unweighted number of observations and
#'   weighted percent of the total (if weights are used, otherwise unweighted).
#'   Only works in two-way tables (`var2` is specified)
#'   * `"totalw"` - will only give weighted number of observations and
#'   weighted percent of the total (if weights are used, otherwise unweighted).
#'   Only works in two-way tables (`var2` is specified)
#'   * Any other text will give the default ("all")
#' @param chisquare A logical. `FALSE` (standard) will not calculate p-value for
#'   the chi-square test for two-way tables (`var2` is specified). If `TRUE`,
#'   the table will include the chi-square p-value as well as the chi-square
#'   statistic and the corresponding degrees of freedom. It will be included in
#'   the output whichever output option have been specified. No chi-square test
#'   is performed or included in one-way tables (`var2` is unspecified)
#'
#' @return Multiple frequency tables stored in a data frame object.
#'
#' @author ASO
#'
#' @seealso [freq_function()] for the function that creates frequency tables for
#' single variables.
#'
#' @examples
#' # Examples
#' data("starwars", package = "dplyr")
#' test_table1 <- freq_function_repeated(
#'   starwars,
#'   var1 = c("sex","homeworld","eye_color"),
#'   include_NA = TRUE
#' )
#' test_table2 <- freq_function_repeated(
#'   starwars,
#'   var1 = c("homeworld","eye_color","skin_color"),
#'   var2 = "sex",
#'   output = "col",
#'   number_decimals = 3
#' )
#' test_table3 <- freq_function_repeated(
#'   starwars,
#'   var1 = c("homeworld","eye_color","skin_color"),
#'   var2 = "sex",
#'   by_vars = c("gender"),
#'   output = "row"
#' )
#'
#' @export

freq_function_repeated <- function(
    normaldata,
    var1,
    var2 = NULL,
    by_vars = NULL,
    include_NA = FALSE,
    values_to_remove = NULL,
    weightvar = NULL,
    textvar = NULL,
    number_decimals = 2,
    output = c("all", "numeric", "col", "colw", "row", "roww", "total", "totalw"),
    chisquare = FALSE
) {
  output <- match.arg(output)
  func_table1 <- normaldata |>
    dplyr::select(
      dplyr::all_of({{ var1 }}),
      dplyr::all_of({{ var2 }}),
      dplyr::all_of({{ weightvar }}),
      dplyr::all_of({{ by_vars }})
    )

  var_count <- length(var1)

  for (i in seq_along(var1)) {
    func_var <- dplyr::nth(var1, n = i)
    func_freqs <- freq_function(
      normaldata = func_table1,
      var1 = func_var,
      var2 = var2,
      by_vars = by_vars,
      include_NA = include_NA,
      values_to_remove = values_to_remove,
      weightvar = weightvar,
      textvar = textvar,
      number_decimals = number_decimals,
      output = output,
      chisquare = chisquare
    )
    func_freqs2 <- func_freqs |>
      dplyr::mutate(var_name = func_var) |>
      dplyr::rename("Level" = dplyr::all_of(func_var))
    if (i == 1) {
      func_table2 <- func_freqs2
    } else {
      func_table2 <- dplyr::bind_rows(func_table2, func_freqs2)
    }
  }
  func_table3 <- func_table2 |>
    dplyr::relocate("var_name", .before = 1)
  return(func_table3)
}
