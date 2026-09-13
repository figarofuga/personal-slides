#' Flatten Date Intervals
#'
#' A tidyverse compatible function for simplifying time interval data
#'
#' @param data A data frame, data frame extension (e.g. a tibble), or a lazy
#'   data frame (e.g. from dbplyr or dtplyr).
#' @param id <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expression naming the id variables in data.
#' @param in_date <[`data-masking`][dplyr::dplyr_data_masking]> One unquoted
#'   expressions naming the start date variable in data.
#' @param out_date <[`data-masking`][dplyr::dplyr_data_masking]> One unquoted
#'   expression naming the end date variable in data.
#' @param status <[`tidy-select`][dplyr::dplyr_tidy_select]> One or more unquoted
#'   expressions naming a status variable in data, such as region or
#'   hospitalization reason.
#' @param overlap_handling A character naming the method for handling overlaps
#'   within an individuals time when `status` has been specified.
#'
#'   * "none": No special handling of the overlapping time intervals within
#'     person is done.
#'   * "first": The `status` mentioned first, that is, has the smallest
#'   `in_date`, dominates.
#'   * "most_recent" (default): The most recent `status`, that is, the one with
#'   the largest `in_date`, dominates. When the most recent `status` is fully
#'   contained within an older (and different) `status` then the `out_date`
#'   associated with the most recent `in_date` is kept, but the remaining time
#'   from the older `status` is removed. See examples below.
#'
#'   We currently don't have a method that lets the most recent status dominate
#'   and then potentially return to an older longer running status. If this is
#'   needed, please contact ADLS.
#'
#' @param lag A numeric, giving the number of days allowed between time
#'   intervals that should be collapsed into one.
#'
#' @returns
#' A data frame with the `id`, `status` if specified and simplified `in_date`
#' and `out_date`. The returned data is sorted by `id` and `in_date`.
#'
#' @details
#' This functions identifies overlapping time intervals within individual and
#' collapses them into distinct and disjoint intervals. When `status` is
#' specified these intervals are both individual and status specific.
#'
#' If `lag` is specified then intervals must be more then `lag` time units apart
#' to be considered distinct.
#'
#'
#' @author
#' ADLS, EMTH & ASO
#'
#' @examples
#'
#' ### The flatten function works with both dates and numeric
#'
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3, 4),
#'    START = c(1, 2, 5, 3, 6, 2, 3, 6),
#'    END   = c(3, 3, 7, 4, 9, 3, 5, 8))
#' dat |> flatten_date_intervals(ID, START, END)
#'
#' dat <- data.frame(
#'    ID    = c(1, 1, 1, 2, 2, 3, 3, 4, 4),
#'    START = as.Date(c("2012-02-15", "2005-12-13", "2006-01-24",
#'                      "2002-03-14", "1997-02-27",
#'                      "2008-08-13", "1998-09-23",
#'                      "2005-01-12", "2007-05-10")),
#'    END   = as.Date(c("2012-06-03", "2007-02-05", "2006-08-22",
#'                      "2005-02-26", "1999-04-16",
#'                      "2008-08-22", "2015-01-29",
#'                      "2007-05-07", "2008-12-12")))
#' dat |> flatten_date_intervals(ID, START, END)
#'
#'
#'
#' ###  Allow for a 5 days lag between
#'
#' dat |> flatten_date_intervals(ID, START, END, lag = 5)
#'
#'
#'
#' ### Adding status information
#'
#' dat <- data.frame(
#'    ID     = c(1, 1, 1, 2, 2, 3, 3, 4, 4),
#'    START  = as.Date(c("2012-02-15", "2005-12-13", "2006-01-24",
#'                       "2002-03-14", "1997-02-27",
#'                       "2008-08-13", "1998-09-23",
#'                       "2005-01-12", "2007-05-10")),
#'    END    = as.Date(c("2012-06-03", "2007-02-05", "2006-08-22",
#'                       "2005-02-26", "1999-04-16",
#'                       "2008-08-22", "2015-01-29",
#'                      "2007-05-07", "2008-12-12")),
#'    REGION = c("H", "H", "N", "S", "S", "M", "N", "S", "S"))
#'
#' # Note the difference between the the different overlap_handling methods
#' dat |> flatten_date_intervals(ID, START, END, REGION, "none")
#' dat |> flatten_date_intervals(ID, START, END, REGION, "first")
#' dat |> flatten_date_intervals(ID, START, END, REGION, "most_recent")
#'
#' @export
flatten_date_intervals <- function(
    data,
    id,
    in_date,
    out_date,
    status = NULL,
    overlap_handling = "most_recent",
    lag = 0
  ) {

  flat <- data |>
    dplyr::mutate(
      .numeric_in_date = as.numeric({{ in_date }}),
      .numeric_out_date = as.numeric({{ out_date }})
    ) |>
    dplyr::arrange(.data$.numeric_in_date, .data$.numeric_out_date) |>
    dplyr::group_by(dplyr::across(c({{ id }}, {{ status }}))) |>
    dplyr::mutate(
      .internal_running_index = c(
        0,
        cumsum(dplyr::lead(.data$.numeric_in_date) >
                 cummax(.data$.numeric_out_date + lag))[-dplyr::n()]
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::group_by(
      dplyr::across(c({{ id }}, {{ status }})),
      .data$.internal_running_index
    ) |>
    dplyr::summarize(
      .interval_IN_DATE = min({{ in_date }}),
      .interval_OUT_DATE = max({{ out_date }}),
      .groups = "drop"
    ) |>
    dplyr::select(-".internal_running_index") |>
    dplyr::rename(
      "{{ in_date }}" := ".interval_IN_DATE",
      "{{ out_date }}" := ".interval_OUT_DATE"
    ) |>
    dplyr::arrange({{ id }}, {{ in_date }})

  if (dplyr::select(data, {{ status }}) |> (\(x) ncol(x) == 0)()) {

    return(flat)

  } else if (overlap_handling == "none") {

    return(flat)

  } else if (overlap_handling == "first") {

    clean_flat <- flat |>
      dplyr::group_by(dplyr::across({{ id }})) |>
      dplyr::mutate(
        "{{ in_date }}" := pmax(
          {{ in_date }},
          dplyr::lag({{ out_date }}),
          na.rm = TRUE
        )
      ) |>
      dplyr::ungroup() |>
      dplyr::filter({{ in_date }} <= {{ out_date }})

  } else {

    clean_flat <- flat |>
      dplyr::group_by(dplyr::across({{ id }})) |>
      dplyr::mutate(
        "{{ out_date }}" := pmin(
          dplyr::lead({{ in_date }}),
          {{ out_date }},
          na.rm = TRUE
        )
      ) |>
      dplyr::ungroup()

  }

  return(clean_flat)
}
