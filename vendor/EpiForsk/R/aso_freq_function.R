#' Frequency Tables with Percentage and Odds Ratios
#'
#' A method for making 1- and 2-way frequency tables with percentages and odds
#' ratios.
#'
#' @param normaldata A data frame or data frame extension (e.g. a tibble).
#' @param var1 A character string naming the first variable to get frequencies.
#' @param var2 An optional character naming the second variable to get
#'   frequencies. If `NULL` (standard) a 1-way frequency table of only `var1` is
#'   created, and if `var2` is specified a 2-way table is returned.
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
#'   * `"col"` - will only give unweighted number of observations and weighted
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
#' @return A frequency table as a data frame object.
#'
#' @author ASO
#'
#' @seealso [freq_function_repeated()] to to get frequencies for multiple
#'   variables in one go.
#'
#' @examples
#' data("starwars", package = "dplyr")
#'
#' test_table1 <- freq_function(
#'   starwars,
#'   var1 = "homeworld"
#' )
#'
#' test_table2 <- freq_function(
#'   starwars,
#'   var1 = "sex",
#'   var2 = "eye_color",
#'   output = "total"
#' )
#'
#' test_table3 <- freq_function(
#'   starwars,
#'   var1 = "hair_color",
#'   var2 = "skin_color",
#'   by_vars = "gender",
#'   output = "col",
#'   number_decimals = 5
#' )
#'
#' @export

freq_function <- function(
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
  ### Begin input checks
  if (missing(normaldata)) {
    stop("'normaldata' must be a data frame.")
  }
  if (!inherits(normaldata, "data.frame")) {
    stop("'normaldata' must be a data frame.")
  }
  if (missing(var1) || !(is.character(var1) && length(var1) == 1)) {
    stop(
      "'var1' must be a length 1 character vector naming ",
      "the first variable to get frequencies."
    )
  }
  if (!(is.null(var2) || (is.character(var2) && length(var2) == 1))) {
    stop(
      "'var2' must be NULL or optionally a length 1 character ",
      "vector naming the second\nvariable to get frequencies."
    )
  }
  if (!is.null(by_vars)) {
    if (!inherits(by_vars, "character")) {
      stop(
        "by_vars must be NULL or optionally a character vector ",
        "naming variables in normal_data\n to stratify the ",
        "calculations and output by."
      )
    } else if (!all(by_vars %in% names(normaldata))) {
      stop(
        glue::glue(
          "by_vars must name variables in normaldata.\n",
          "The following are not names of normaldata columns:\n",
          "{paste(by_vars[!(by_vars %in% names(starwars))], collapse = ', ')}"
        )
      )
    }
  }
  if (!(isTRUE(include_NA) || isFALSE(include_NA))) {
    stop("'include_NA' must be a boolean.")
  }
  if (!is.null(values_to_remove)) {
    if (!inherits(values_to_remove, "character")) {
      stop(
        "'values_to_remove' must be NULL or optionally a character vector."
      )
    }
  }
  if (!is.null(weightvar)) {
    # The final weightvar check returns a warning if some weights in the column
    # specified by weightvar are negative or NA. This is done because these
    # observations are removed before creating the frequency tables - so it
    # will not result in an error - but the removal of these observations are
    # likely unexpected to the user. As a help, affected rows are printed.
    # To improve the readability of the warning, concecutive rows are grouped
    # together.
    if (!inherits(weightvar, "character")) {
      stop(
        "weightvar must be NULL or optionally a character of length 1 ",
        "naming a column\nin `normaldata` with numeric weights ",
        "for each observation."
      )
    } else if (!(length(weightvar == 1) && weightvar %in% names(normaldata))) {
      stop(
        "weightvar must name a single column in 'normaldata' with numeric",
        "weights\nfor each observation."
      )
    } else if (!is.numeric(normaldata[[weightvar]])) {
      stop(
        "The column named in weightvar must contain numeric weights",
        "for each observation."
      )
    }
    if (any(normaldata[[weightvar]] <= 0 | is.na(normaldata[[weightvar]]))) {
      warnindex <- which(
        normaldata[[weightvar]] <= 0 |
          is.na(normaldata[[weightvar]])
      )
      init <- TRUE
      warntext <- ""
      for (i in seq_len(length(warnindex) - 1)) {
        if (init) {
          warntext <- paste0(warntext, warnindex[i])
          if (warnindex[i + 1] == warnindex[i] + 1) {
            init <- FALSE
          } else {
            warntext <- paste0(warntext, ", ")
          }
        } else if (warnindex[i + 1] != warnindex[i] + 1) {
          warntext <- paste0(warntext, ":", warnindex[i], ", ")
          init <- TRUE
        }
      }
      paste0(warntext, warnindex[length(warnindex)])
      warning(glue::glue(
        "Non-positive weights or NAs detected in row(s)\n",
        "{warntext}\n",
        "Rows with non-positive weights or NAs are removed."
      ))
    }
  }
  if (!(is.null(textvar) || inherits(textvar, "character"))) {
    stop("When 'textvar' is specified it must be a character.")
  }
  if (!inherits(number_decimals, "numeric")) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  if (number_decimals < 0) {
    stop("'number_decimals' must be a non-negative integer.")
  }
  output <- match.arg(output)
  if (!(isTRUE(chisquare) || isFALSE(chisquare))) {
    stop("'chisquare' must be a boolean.")
  }
  # End input checks

  # Select only mentioned variables from called data (not using specifications)
  func_table <- normaldata |>
    dplyr::select(
      dplyr::all_of(var1),
      dplyr::all_of(var2),
      dplyr::all_of(weightvar),
      dplyr::all_of(by_vars)
    )

  # Encoding for weight variable - unless a precalculated weight should be used,
  # the weight will be 1 for all observations
  if (!is.null(weightvar)) {
    func_table <- func_table |>
      dplyr::mutate("weight_used" = .data[[weightvar]]) |>
      dplyr::filter(.data$weight_used > 0)
  }
  else {
    func_table <- dplyr::mutate(func_table, weight_used = as.numeric(1))
  }

  # Keeping/removing missing (=NA) in the data depending on user specification
  if (include_NA == FALSE & is.null(var2)) {
    func_table <- dplyr::filter(func_table, !is.na(.data[[var1]]))
  } else if (include_NA == FALSE & !is.null(var2)) {
    func_table <-
      dplyr::filter(
        func_table,
        !is.na(.data[[var1]]) & !is.na(.data[[var2]])
      )
  }

  # Remove specified values (if any) from the values used in tables before
  # calculations (by-variables NOT influenced)
  if (is.null(var2)) {
    func_table <- func_table |>
      dplyr::filter(!.data[[var1]] %in% values_to_remove)
  } else {
    func_table <- func_table |>
      dplyr::filter(
        !.data[[var1]] %in% values_to_remove &
          !.data[[var2]] %in% values_to_remove
      )
  }

  # Change all factor variables in data to character variables
  # (basically it will be by-variables who are changed)
  func_table <- func_table |>
    dplyr::mutate(dplyr::across(dplyr::where(is.factor), as.character))

  # Making summations and calculations (n, % etc.)
  if (is.null(by_vars) && is.null(var2)) {
    # Getting unweighted and weighted n for each group
    func_table <- func_table |>
      dplyr::summarise(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = dplyr::all_of(var1)
      ) |>
      dplyr::mutate(
        Func_var = var1
      ) |>
      dplyr::select("Func_var", dplyr::all_of(var1), "n", "n_weighted") |>
      # add summation within variables
      (\(x) {
        y <- x |>
          dplyr::summarise(
            n_total = sum(.data$n),
            n_weighted_total = sum(.data$n_weighted, na.rm = TRUE)
          ) |>
          dplyr::mutate(
            Func_var = var1,
            "{var1}" := "Total"
          ) |>
          dplyr::select(
            "Func_var",
            dplyr::all_of(var1),
            "n" = "n_total",
            "n_weighted" = "n_weighted_total"
          )
        multi_join(
          dplyr::bind_rows(x, y),
          y,
          .by = "Func_var"
        )
      })() |>
      dplyr::mutate(
        Column_pct = ((.data$n_weighted.1 / .data$n_weighted.2) * 100),
        Freq_col_pct = paste0(
          .data$n.1,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$n_weighted.1),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        "{var1}" := .data[[glue::glue("{var1}.1")]],
        N = .data$n.1,
        N_weighted = .data$n_weighted.1,
      ) |>
      dplyr::select(
        dplyr::all_of(var1),
        "N",
        "N_weighted",
        "Column_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )

    if (output == "numeric") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col") {
      func_table <- dplyr::select(
        func_table,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freqw_col_pct"
      )
    } else if (output == "colw") {
      func_table <- dplyr::select(
        func_table,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freq_col_pct"
      )
    }

    # Adding text-variable IF text have been added to textvar
    # at function call + return final table
    if (!is.null(textvar)) {
      # Adding text-variable
      func_table <- func_table |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
    }
    return(func_table)
  } else if (is.null(var2)) {
    # Getting unweighted and weighted n for each group
    func_table <- func_table |>
      dplyr::summarise(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c(dplyr::all_of(by_vars), dplyr::all_of(var1))
      ) |>
      dplyr::mutate(
        Func_var = var1
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var",
        dplyr::all_of(var1),
        "n",
        "n_weighted"
      ) |>
      # Summation within variables
      (\(x) {
        y <- x |>
          dplyr::summarise(
            n_total = sum(.data$n),
            n_weighted_total = sum(.data$n_weighted, na.rm = TRUE),
            .by = dplyr::all_of(by_vars)
          ) |>
          dplyr::mutate(
            Func_var = var1,
            "{var1}" := "Total",
            n = .data$n_total,
            n_weighted = .data$n_weighted_total
          ) |>
          dplyr::select(
            dplyr::all_of(by_vars),
            "Func_var",
            dplyr::all_of(var1),
            "n",
            "n_weighted"
          )
        multi_join(
          dplyr::bind_rows(x, y),
          y,
          .by = c(by_vars, "Func_var")
        )
      })() |>
      dplyr::mutate(
        Column_pct = ((.data$n_weighted.1 / .data$n_weighted.2) * 100),
        Freq_col_pct = paste0(
          .data$n.1,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$n_weighted.1),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        "{var1}" := .data[[glue::glue("{var1}.1")]],
        N = .data$n.1,
        N_weighted = .data$n_weighted.1
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        dplyr::all_of(var1),
        "N",
        "N_weighted",
        "Column_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      ) |>
      dplyr::arrange(
        dplyr::across(c(dplyr::all_of(by_vars), dplyr::all_of(var1)))
      )

    if (output == "numeric") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col") {
      func_table <- dplyr::select(
        func_table,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freqw_col_pct"
      )
    } else if (output == "colw") {
      func_table <- dplyr::select(
        func_table,
        -"N",
        -"N_weighted",
        -"Column_pct",
        -"Freq_col_pct"
      )
    }

    # Adding text if textvar is used + return final table
    if (!is.null(textvar)) {
      func_table <- func_table |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
    }
    return(func_table)
  }
  else if (is.null(by_vars)) {
    if (chisquare == TRUE) {
      # Degrees of freedom, chi-square test
      chi_degree1 <- length(unique(func_table[[var1]]))
      chi_degree2 <- length(unique(func_table[[var2]]))
      chi_degree_total <- (chi_degree1 - 1) * (chi_degree2 - 1)
      chi_degree_freedom <- as.data.frame(chi_degree_total)
    }

    # Getting unweighted and weighted n for each combination
    func_table <- func_table |>
      dplyr::summarise(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c(dplyr::all_of(var1), dplyr::all_of(var2))
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2
      ) |>
      dplyr::select(
        "Func_var1",
        "Func_var2",
        dplyr::all_of(var1),
        dplyr::all_of(var2),
        "n",
        "n_weighted"
      ) |>
      (\(x) {
        multi_join(
          # Summation within variable1
          x |>
            dplyr::summarise(
              n = sum(.data$n),
              n_weighted = sum(.data$n_weighted, na.rm = TRUE),
              .by = dplyr::all_of(var1)
            ) |>
            dplyr::mutate(
              Func_var1 = var1,
              "{var2}" := "Total"
            ) |>
            dplyr::select(
              "Func_var1",
              dplyr::all_of(var1),
              dplyr::all_of(var2),
              "n",
              "n_weighted"
            ),
          x,
          .by = c("Func_var1", var1)
        ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            dplyr::all_of(var1),
            "{var2}" := glue::glue("{var2}.2"),
            "{var1}_level_total" := "n.1",
            "{var1}_level_total_weighted" := "n_weighted.1",
            "n" = "n.2",
            "n_weighted" = "n_weighted.2"
          ) |>
          multi_join(
            # Summation totals
            x |>
              dplyr::summarise(
                n = sum(.data$n),
                n_weighted = sum(.data$n_weighted, na.rm = TRUE)
              ) |>
              dplyr::mutate(
                Func_var1 = var1,
                Func_var2 = var2,
                Var_tot = "Total"
              ) |>
              dplyr::select(
                "Func_var1",
                "Func_var2",
                "{var1}" := "Var_tot",
                "{var2}" := "Var_tot",
                "n",
                "n_weighted"
              ),
            .by = c("Func_var1","Func_var2")
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            "{var1}" := glue::glue("{var1}.1"),
            "{var2}" := glue::glue("{var2}.1"),
            "Total_n" = "n.2",
            "Total_n_weighted" = "n_weighted.2",
            glue::glue("{var1}_level_total"),
            glue::glue("{var1}_level_total_weighted"),
            "n" = "n.1",
            "n_weighted" = "n_weighted.1"
          ) |>
          multi_join(
            # Summation within variable2
            x |>
              dplyr::summarise(
                n = sum(.data$n),
                n_weighted = sum(.data$n_weighted, na.rm = TRUE),
                .by = dplyr::all_of(var2)
              ) |>
              dplyr::mutate(
                Func_var2 = var2,
                "{var1}" := "Total"
              ) |>
              dplyr::select(
                "Func_var2",
                dplyr::all_of(var1),
                dplyr::all_of(var2),
                "n",
                "n_weighted"
              ),
            .by = c("Func_var2", var2)
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            "{var1}" := glue::glue("{var1}.1"),
            dplyr::all_of(var2),
            "Total_n",
            "Total_n_weighted",
            glue::glue("{var1}_level_total"),
            glue::glue("{var1}_level_total_weighted"),
            "{var2}_level_total" := "n.2",
            "{var2}_level_total_weighted" := "n_weighted.2",
            "n" = "n.1",
            "n_weighted" = "n_weighted.1"
          )
      })()

    if (chisquare == TRUE) {
      # Calculating expected numbers for each cell
      chi_table <- func_table |>
        dplyr::mutate(
          expected =
            (.data[[glue::glue("{var1}_level_total_weighted")]] /
               .data$Total_n_weighted) *
            .data[[glue::glue("{var2}_level_total_weighted")]],
          observed = .data$n_weighted,
          cell_chi = (((.data$observed - .data$expected)^2) / .data$expected)
        ) |>
        dplyr::summarise(cell_chi_total = sum(.data$cell_chi, na.rm = TRUE))
    }

    func_table <-
      list(
        # Percent calculations for each Var1 and Var2 combination
        var1_var2_comb = func_table |>
          dplyr::mutate(
            Total_pct = (
              (.data$n_weighted / .data$Total_n_weighted) * 100
            ),
            Row_pct = (
              (.data$n_weighted /
                 .data[[glue::glue("{var1}_level_total_weighted")]]) * 100
            ),
            Column_pct = (
              (.data$n_weighted /
                 .data[[glue::glue("{var2}_level_total_weighted")]]) * 100
            )
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            dplyr::all_of(var1),
            dplyr::all_of(var2),
            "N" = "n",
            "Weighted_N" = "n_weighted",
            "Total_pct",
            "Row_pct",
            "Column_pct"
          ),
        # Percent calculations for each Var1 row totals (sum over var2)
        var1_comb =  func_table |>
          dplyr::mutate(
            "{var2}" := "Total",
            n = .data[[glue::glue("{var1}_level_total")]],
            Total_pct = (
              (.data[[glue::glue("{var1}_level_total_weighted")]] /
                 .data$Total_n_weighted) * 100
            ),
            Column_pct = (
              (.data[[glue::glue("{var1}_level_total_weighted")]] /
                 .data$Total_n_weighted) * 100
            ),
            Row_pct = (
              (.data[[glue::glue("{var1}_level_total_weighted")]] /
                 .data[[glue::glue("{var1}_level_total_weighted")]]) * 100
            )
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            dplyr::all_of(var1),
            dplyr::all_of(var2),
            "N" = "n",
            "Weighted_N" = glue::glue("{var1}_level_total_weighted"),
            "Total_pct",
            "Row_pct",
            "Column_pct"
          ) |>
          dplyr::distinct(),
        # Percent calculations for each Var2 row totals (sum over var1)
        var2_comb = func_table |>
          dplyr::mutate(
            "{var1}" := "Total",
            n = .data[[glue::glue("{var2}_level_total")]],
            Total_pct =
              (.data[[glue::glue("{var2}_level_total_weighted")]] /
                 .data$Total_n_weighted) * 100,
            Column_pct =
              (.data[[glue::glue("{var2}_level_total_weighted")]] /
                 .data[[glue::glue("{var2}_level_total_weighted")]]) * 100,
            Row_pct =
              (.data[[glue::glue("{var2}_level_total_weighted")]] /
                 .data$Total_n_weighted) * 100
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            dplyr::all_of(var1),
            dplyr::all_of(var2),
            "N" = "n",
            "Weighted_N" = glue::glue("{var2}_level_total_weighted"),
            "Total_pct",
            "Row_pct",
            "Column_pct"
          ) |>
          dplyr::distinct(),
        # Percent calculations for total (complete total)
        tot_comb = func_table |>
          dplyr::mutate(
            tot_var = "Total",
            n = .data$Total_n,
            Total_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
            Column_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
            Row_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100)
          ) |>
          dplyr::select(
            "Func_var1",
            "Func_var2",
            "{var1}" := "tot_var",
            "{var2}" := "tot_var",
            "N" = "n",
            "Weighted_N" = "Total_n_weighted",
            "Total_pct",
            "Row_pct",
            "Column_pct"
          ) |>
          dplyr::distinct()
      ) |>
      (\(x) {
        dplyr::bind_rows(x[[1]], x[[2]], x[[3]], x[[4]])
      })() |>
      dplyr::mutate(
        Freq_col_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freq_row_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freqw_row_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freq_total_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        ),
        Freqw_total_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        )
      ) |>
      dplyr::select(
        "Func_var2",
        dplyr::all_of(var1),
        dplyr::all_of(var2),
        "N",
        "Weighted_N",
        "Row_pct",
        "Column_pct",
        "Total_pct",
        "Freq_total_pct",
        "Freqw_total_pct",
        "Freq_row_pct",
        "Freqw_row_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )

    # Change rotation on table, so the rows and columns are easier to follow
    func_table <- dplyr::select(
      func_table,
      -"Freq_total_pct",
      -"Freqw_total_pct",
      -"Freq_row_pct",
      -"Freqw_row_pct",
      -"Freq_col_pct",
      -"Freqw_col_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", dplyr::all_of(var2)),
        values_from = c(
          "N",
          "Weighted_N",
          "Row_pct",
          "Column_pct",
          "Total_pct"
        ),
        values_fill = 0
      ) |>
      dplyr::full_join(
        dplyr::select(
          func_table,
          -"N",
          -"Weighted_N",
          -"Row_pct",
          -"Column_pct",
          -"Total_pct"
        ) |>
          tidyr::pivot_wider(
            names_from = c("Func_var2", dplyr::all_of(var2)),
            values_from = c(
              "Freq_total_pct",
              "Freqw_total_pct",
              "Freq_row_pct",
              "Freqw_row_pct",
              "Freq_col_pct",
              "Freqw_col_pct"
            ),
            values_fill = "0 (0%)"
          ),
        by = var1
      )

    if (chisquare == TRUE) {
      # Degrees of freedom, chi-square test
      chi_p_prp <- dplyr::bind_cols(chi_table, chi_degree_freedom)
      chi_p <- chi_p_prp |>
        dplyr::rowwise() |>
        dplyr::mutate(
          p_value = pchisq(
            .data$cell_chi_total,
            df = .data$chi_degree_total,
            lower.tail = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate("{var1}" := "Total")

      func_table <- dplyr::full_join(func_table, chi_p, by = var1)
    }

    if (output == "numeric") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "colw") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_")
      )
    } else if (output == "row") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "roww") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "total") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "totalw") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    }

    if (!is.null(textvar)) {
      # Adding text-variable
      func_table <- func_table |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
    }

    return(func_table)
  } else {
    if (chisquare == TRUE) {
      # Degrees of freedom, chi-square test
      chi_degree1 <- func_table |>
        dplyr::summarise(
          chi_degree1 = dplyr::n_distinct(.data[[var1]]),
          .by = dplyr::all_of(by_vars)
        )
      chi_degree2 <- func_table |>
        dplyr::summarise(
          chi_degree2 = dplyr::n_distinct(.data[[var2]]),
          .by = dplyr::all_of(by_vars)
        )
      chi_degree_freedom <- dplyr::full_join(
        chi_degree1,
        chi_degree2,
        by = by_vars
      ) |>
        dplyr::mutate(
          chi_degree_total = ((.data$chi_degree1 - 1) * (.data$chi_degree2 - 1))
        ) |>
        dplyr::select(-"chi_degree1", -"chi_degree2")
    }

    # Getting unweighted and weighted n for each combination
    func_table <- func_table |>
      dplyr::summarise(
        n = dplyr::n(),
        n_weighted = sum(.data$weight_used, na.rm = TRUE),
        .by = c(dplyr::all_of(by_vars), dplyr::all_of(var1), dplyr::all_of(var2))
      ) |>
      dplyr::mutate(
        Func_var1 = var1,
        Func_var2 = var2
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var1",
        "Func_var2",
        dplyr::all_of(var1),
        dplyr::all_of(var2),
        "n",
        "n_weighted"
      ) |>
      (\(x) {
        multi_join(
          # Summation within variable1
          x |>
            dplyr::summarise(
              n = sum(.data$n),
              n_weighted = sum(.data$n_weighted, na.rm = TRUE),
              .by = c(dplyr::all_of(by_vars), dplyr::all_of(var1))
            ) |>
            dplyr::mutate(
              Func_var1 = var1,
              "{var2}" := "Total"
            ) |>
            dplyr::select(
              dplyr::all_of(by_vars),
              "Func_var1",
              dplyr::all_of(var1),
              dplyr::all_of(var2),
              "n",
              "n_weighted"
            ),
          x,
          .by = c(by_vars, "Func_var1", var1)
        ) |>
          dplyr::select(
            dplyr::all_of(by_vars),
            "Func_var1",
            "Func_var2",
            dplyr::all_of(var1),
            "{var2}" := glue::glue("{var2}.2"),
            "{var1}_level_total" := "n.1",
            "{var1}_level_total_weighted" := "n_weighted.1",
            "n" = "n.2",
            "n_weighted" = "n_weighted.2"
          ) |>
          multi_join(
            # Summation totals
            x |>
              dplyr::summarise(
                n = sum(.data$n),
                n_weighted = sum(.data$n_weighted, na.rm = TRUE),
                .by = dplyr::all_of(by_vars)
              ) |>
              dplyr::mutate(
                Func_var1 = var1,
                Func_var2 = var2,
                tot_var = "Total"
              ) |>
              dplyr::select(
                dplyr::all_of(by_vars),
                "Func_var1",
                "Func_var2",
                "{var1}" := "tot_var",
                "{var2}" := "tot_var",
                "n",
                "n_weighted"
              ),
            .by = c(by_vars, "Func_var1","Func_var2")
          ) |>
          dplyr::select(
            dplyr::all_of(by_vars),
            "Func_var1",
            "Func_var2",
            "{var1}" := glue::glue("{var1}.1"),
            "{var2}" := glue::glue("{var2}.1"),
            "Total_n" = "n.2",
            "Total_n_weighted" = "n_weighted.2",
            glue::glue("{var1}_level_total"),
            glue::glue("{var1}_level_total_weighted"),
            "n" = "n.1",
            "n_weighted" = "n_weighted.1"
          ) |>
          multi_join(
            # Summation within variable2
            x |>
              dplyr::summarise(
                n = sum(.data$n),
                n_weighted = sum(.data$n_weighted, na.rm = TRUE),
                .by = c(dplyr::all_of(by_vars), dplyr::all_of(var2))
              ) |>
              dplyr::mutate(
                Func_var2 = var2,
                "{var1}" := "Total"
              ) |>
              dplyr::select(
                dplyr::all_of(by_vars),
                "Func_var2",
                dplyr::all_of(var1),
                dplyr::all_of(var2),
                "n",
                "n_weighted"
              ),
            .by = c(by_vars, "Func_var2", var2)
          ) |>
          dplyr::select(
            dplyr::all_of(by_vars),
            "Func_var1",
            "Func_var2",
            "{var1}" := glue::glue("{var1}.1"),
            dplyr::all_of(var2),
            "Total_n",
            "Total_n_weighted",
            glue::glue("{var1}_level_total"),
            glue::glue("{var1}_level_total_weighted"),
            "{var2}_level_total" := "n.2",
            "{var2}_level_total_weighted" := "n_weighted.2",
            "n" = "n.1",
            "n_weighted" = "n_weighted.1"
          )
      })()

    if (chisquare == TRUE) {
      # Calculating expected numbers for each cell
      chi_table <- func_table |>
        dplyr::mutate(
          expected = (
            (.data[[glue::glue("{var1}_level_total_weighted")]] / .data$Total_n_weighted) *
              .data[[glue::glue("{var2}_level_total_weighted")]]
          ),
          observed = .data$n_weighted,
          cell_chi = (((.data$observed - .data$expected)^2) / .data$expected)
        ) |>
        dplyr::summarise(
          cell_chi_total = sum(.data$cell_chi, na.rm = TRUE),
          .by = dplyr::all_of(by_vars)
        )
    }

    func_table <- list(
      # Percent calculations for each Var1 and Var2 combination
      var1_var2_comb = func_table |>
        dplyr::mutate(
          Total_pct =
            (.data$n_weighted / .data$Total_n_weighted) * 100,
          Row_pct =
            (.data$n_weighted /
               .data[[glue::glue("{var1}_level_total_weighted")]]) * 100,
          Column_pct =
            (.data$n_weighted /
               .data[[glue::glue("{var2}_level_total_weighted")]]) * 100
        ) |>
        dplyr::select(
          dplyr::all_of(by_vars),
          "Func_var1",
          "Func_var2",
          dplyr::all_of(var1),
          dplyr::all_of(var2),
          "N" = "n",
          "Weighted_N" = "n_weighted",
          "Total_pct",
          "Row_pct",
          "Column_pct"
        ),
      # Percent calculations for each Var1 row totals (sum over var2)
      var1_comb =  func_table |>
        dplyr::mutate(
          "{var2}" := "Total",
          n = .data[[glue::glue("{var1}_level_total")]],
          Total_pct =
            (.data[[glue::glue("{var1}_level_total_weighted")]] /
               .data$Total_n_weighted) * 100,
          Column_pct =
            (.data[[glue::glue("{var1}_level_total_weighted")]] /
               .data$Total_n_weighted) * 100,
          Row_pct =
            (.data[[glue::glue("{var1}_level_total_weighted")]] /
               .data[[glue::glue("{var1}_level_total_weighted")]]) * 100
        ) |>
        dplyr::select(
          dplyr::all_of(by_vars),
          "Func_var1",
          "Func_var2",
          dplyr::all_of(var1),
          dplyr::all_of(var2),
          "N" = "n",
          "Weighted_N" = glue::glue("{var1}_level_total_weighted"),
          "Total_pct",
          "Row_pct",
          "Column_pct"
        ) |>
        dplyr::distinct(),
      # Percent calculations for each Var2 row totals (sum over var1)
      var2_comb = func_table |>
        dplyr::mutate(
          "{var1}" := "Total",
          n = .data[[glue::glue("{var2}_level_total")]],
          Total_pct =
            (.data[[glue::glue("{var2}_level_total_weighted")]] /
               .data$Total_n_weighted) * 100,
          Column_pct =
            (.data[[glue::glue("{var2}_level_total_weighted")]] /
               .data[[glue::glue("{var2}_level_total_weighted")]]) * 100,
          Row_pct =
            (.data[[glue::glue("{var2}_level_total_weighted")]] /
               .data$Total_n_weighted) * 100
        ) |>
        dplyr::select(
          dplyr::all_of(by_vars),
          "Func_var1",
          "Func_var2",
          dplyr::all_of(var1),
          dplyr::all_of(var2),
          "N" = "n",
          "Weighted_N" = glue::glue("{var2}_level_total_weighted"),
          "Total_pct",
          "Row_pct",
          "Column_pct"
        ) |>
        dplyr::distinct(),
      # Percent calculations for total (complete total)
      tot_comb = func_table |>
        dplyr::mutate(
          tot_var = "Total",
          n = .data$Total_n,
          Total_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
          Column_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100),
          Row_pct = ((.data$Total_n_weighted / .data$Total_n_weighted) * 100)
        ) |>
        dplyr::select(
          dplyr::all_of(by_vars),
          "Func_var1",
          "Func_var2",
          "{var1}" := "tot_var",
          "{var2}" := "tot_var",
          "N" = "n",
          "Weighted_N" = "Total_n_weighted",
          "Total_pct",
          "Row_pct",
          "Column_pct"
        ) |>
        dplyr::distinct()
    ) |>
      (\(x) {
        dplyr::bind_rows(x[[1]], x[[2]], x[[3]], x[[4]])
      })() |>
      dplyr::mutate(
        Freq_col_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freqw_col_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Column_pct),
          "%)"
        ),
        Freq_row_pct = paste0(
          .data$N,
          " (",sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freqw_row_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Row_pct),
          "%)"
        ),
        Freq_total_pct = paste0(
          .data$N,
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        ),
        Freqw_total_pct = paste0(
          sprintf(paste0("%.",number_decimals,"f"), .data$Weighted_N),
          " (",
          sprintf(paste0("%.",number_decimals,"f"), .data$Total_pct),
          "%)"
        )
      ) |>
      dplyr::select(
        dplyr::all_of(by_vars),
        "Func_var2",
        dplyr::all_of(var1),
        dplyr::all_of(var2),
        "N",
        "Weighted_N",
        "Row_pct",
        "Column_pct",
        "Total_pct",
        "Freq_total_pct",
        "Freqw_total_pct",
        "Freq_row_pct",
        "Freqw_row_pct",
        "Freq_col_pct",
        "Freqw_col_pct"
      )

    # Change rotation on table, so the rows and columns are easier to follow
    func_table <- dplyr::select(
      func_table,
      -"Freq_total_pct",
      -"Freqw_total_pct",
      -"Freq_row_pct",
      -"Freqw_row_pct",
      -"Freq_col_pct",
      -"Freqw_col_pct"
    ) |>
      tidyr::pivot_wider(
        names_from = c("Func_var2", dplyr::all_of(var2)),
        values_from = c(
          "N",
          "Weighted_N",
          "Row_pct",
          "Column_pct",
          "Total_pct"
        ),
        values_fill = 0
      ) |>
      dplyr::full_join(
        dplyr::select(
          func_table,
          -"N",
          -"Weighted_N",
          -"Row_pct",
          -"Column_pct",
          -"Total_pct"
        ) |>
          tidyr::pivot_wider(
            names_from = c("Func_var2", dplyr::all_of(var2)),
            values_from = c(
              "Freq_total_pct",
              "Freqw_total_pct",
              "Freq_row_pct",
              "Freqw_row_pct",
              "Freq_col_pct",
              "Freqw_col_pct"
            ),
            values_fill = "0 (0%)"
          ),
        by = c(by_vars , var1)
      ) |>
      dplyr::arrange(dplyr::across(dplyr::all_of(by_vars)))


    if (chisquare == TRUE) {
      # Degrees of freedom, chi-square test
      chi_p <- dplyr::full_join(
        chi_table,
        chi_degree_freedom,
        by = by_vars
      ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
          p_value = pchisq(
            .data$cell_chi_total,
            df = .data$chi_degree_total,
            lower.tail = FALSE
          )
        ) |>
        dplyr::ungroup() |>
        dplyr::mutate("{var1}" := "Total")

      func_table <- dplyr::full_join(
        func_table,
        chi_p,
        by = c(by_vars, var1)
      )
    }

    if (output == "numeric") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("Freq")
      )
    } else if (output == "col") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "colw") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_")
      )
    } else if (output == "row") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "roww") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "total") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freqw_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    } else if (output == "totalw") {
      func_table <- dplyr::select(
        func_table,
        -dplyr::starts_with("N_"),
        -dplyr::starts_with("Weighted_N_"),
        -dplyr::starts_with("Row_pct_"),
        -dplyr::starts_with("Column_pct_"),
        -dplyr::starts_with("Total_pct_"),
        -dplyr::starts_with("Freq_total_"),
        -dplyr::starts_with("Freq_row_"),
        -dplyr::starts_with("Freqw_row_"),
        -dplyr::starts_with("Freq_col_"),
        -dplyr::starts_with("Freqw_col_")
      )
    }

    if (!is.null(textvar)) {
      # Adding text-variable
      func_table <- func_table |>
        dplyr::mutate(Description = textvar) |>
        dplyr::relocate("Description")
    }
    return(func_table)
  }
}
