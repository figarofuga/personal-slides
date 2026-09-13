#' c-for-benefit
#'
#' Calculates the c-for-benefit, as proposed by D. van Klaveren et al. (2018),
#' by matching patients based on patient characteristics.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}().
#' @param match character, `"covariates"` to match on covariates or `"CATE"` to
#'   match on estimated CATE.
#' @param match_method see \link[MatchIt]{matchit}.
#' @param match_distance see \link[MatchIt]{matchit}.
#' @param tau_hat_method character, `"risk_diff"` to calculate the expected
#'   treatment effect in matched groups as the risk under treatment for the
#'   treated subject minus the risk under control for the untreated subject.
#'   `"tau_avg"` to calculate it as the average treatment effect of matched
#'   subject.
#' @param CI character, `"none"` for no confidence interval, `"simple"` to use a
#'   normal approximation, and `"bootstrap"` to use the bootstrap.
#' @param level numeric, confidence level of the confidence interval.
#' @param n_bootstraps numeric, number of bootstraps to use for the bootstrap
#'   confidence interval computation.
#' @param time_limit numeric, maximum allowed time to compute C-for-benefit. If
#'   limit is reached, execution stops.
#' @param time_limit_CI numeric, maximum time allowed to compute the bootstrap
#'   confidence interval. If limit is reached, the user is asked if execution
#'   should continue or be stopped.
#' @param verbose boolean, TRUE to display progress bar, FALSE to not display
#'   progress bar.
#' @param Y a vector of outcomes. If provided, replaces `forest$Y.orig`.
#' @param W a vector of treatment assignment; 1 for active treatment; 0 for
#'   control If provided, replaces `forest$W.orig`.
#' @param X a matrix of patient characteristics. If provided, replaces
#'   `forest$X.orig`.
#' @param p_0 a vector of outcome probabilities under control.
#' @param p_1 a vector of outcome probabilities under active treatment.
#' @param tau_hat a vector of individualized treatment effect predictions. If
#'   provided, replaces forest$predictions.
#' @param ... additional arguments for \link[MatchIt]{matchit}.
#'
#' @returns a list with the following components:
#'
#' - type: a list with the input provided to the function which determines
#'         how C-for-benefit is computed.
#' - matched_patients: a tibble containing the matched patients.
#' - c_for_benefit: the resulting C-for-benefit value.
#' - lower_CI: the lower bound of the confidence interval (if CI = TRUE).
#' - upper_CI: the upper bound of the confidence interval (if CI = TRUE).
#'
#' @details The c-for-benefit statistic is inspired by the c-statistic used with
#'   prediction models to measure discrimination. The c-statistic takes all
#'   pairs of observations discordant on the outcome, and calculates the
#'   proportion of these where the subject with the higher predicted probability
#'   was the one who observed the outcome. In order to extend this to treatment
#'   effects, van Klaveren et al. suggest matching a treated subject to a
#'   control subject on the predicted treatments effect (or alternatively the
#'   covariates) and defining the observed effect as the difference between the
#'   outcomes of the treated subject and the control subject. The c-for-benefit
#'   statistic is then defined as the proportion of matched pairs with unequal
#'   observed effect in which the subject pair receiving greater treatment
#'   effect also has the highest expected treatment effect. \cr When calculating
#'   the expected treatment effect, van Klaveren et al. use the average CATE
#'   from the matched subjects in a pair (tau_hat_method = "mean"). However,
#'   this doesn't match the observed effect used, unless the baseline risks are
#'   equal. The observed effect is the difference between the observed outcome
#'   for the subject receiving treatment and the observed outcome for the
#'   subject receiving control. Their outcomes are governed by the exposed risk
#'   and the baseline risk respectively. The baseline risks are ideally equal
#'   when covariate matching, although instability of the forest estimates can
#'   cause significantly different baseline risks due to non-exact matching.
#'   When matching on CATE, we should not expect baseline risks to be equal.
#'   Instead, we can more closely match the observed treatment effect by using
#'   the difference between the exposed risk for the subject receiving treatment
#'   and the baseline risk of the subject receiving control (tau_hat_method =
#'   "treatment").
#'
#' @author KIJA
#'
#' @examples
#' \donttest{
#' n <- 800
#' p <- 3
#' X <- matrix(rnorm(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' CB_out <- CForBenefit(
#' forest = cf, CI = "bootstrap", n_bootstraps = 20L, verbose = TRUE,
#' match_method = "nearest", match_distance = "mahalanobis"
#' )
#' }
#'
#' @export

CForBenefit <- function(forest,
                        match = c("covariates", "CATE"),
                        match_method = "nearest",
                        match_distance = "mahalanobis",
                        tau_hat_method = c("risk_diff", "tau_avg"),
                        CI = c("simple", "bootstrap", "none"),
                        level = 0.95,
                        n_bootstraps = 999L,
                        time_limit = Inf,
                        time_limit_CI = Inf,
                        verbose = TRUE,
                        Y = NULL,
                        W = NULL,
                        X = NULL,
                        p_0 = NULL,
                        p_1 = NULL,
                        tau_hat = NULL,
                        ...) {
  ### Check input
  # ensure correct type of inputs controlling options
  stopifnot(
    "level must be a scalar between 0 and 1" =
      is.numeric(level) && length(level) == 1 && 0 < level && level < 1
  )
  stopifnot(
    "n_bootstraps must be a scalar" =
      is.numeric(n_bootstraps) && length(n_bootstraps) == 1
  )
  stopifnot("match_method must be a character" = is.character(match_method))
  stopifnot("match_distance must be a character" = is.character(match_distance))
  stopifnot(
    "verbose must be a boolean (TRUE or FALSE)" =
      isTRUE(verbose) | isFALSE(verbose)
  )
  match <- match.arg(match)
  tau_hat_method <- match.arg(tau_hat_method)
  CI <- match.arg(CI)

  # Check that suggested package cli is installed when verbose=TRUE
  if (verbose) {
    cli <- requireNamespace("cli", quietly = TRUE)
    if (!cli) {
      if (interactive()) {
        for (i in 1:3) {
          input <- readline(glue::glue(
            "'verbose=TRUE' requires package 'cli' to be installed.\n",
            "Attempt to install package from CRAN? (y/n)"
          ))
          if (input == "y") {
            install.packages(
              pkgs = "cli",
              repos = "https://cloud.r-project.org"
            )
            cli <- requireNamespace("cli", quietly = TRUE)
            if (!cli) {
              stop("Failed to install package 'cli'")
            }
            break
          } else if (input == "n") {
            stop("When 'verbose=TRUE', package 'cli' is required.")
          }
          if (i == 3) stop("Failed to answer 'y' or 'n' to many times.")
        }
      } else {
        stop("When 'verbose=TRUE', package 'cli' is required.")
      }
    }
  }

  # check forest is a causal_forest object (or missing or NULL)
  stopifnot(
    "forest must be a causal_forest object" =
      missing(forest) || is.null(forest) || inherits(forest, "causal_forest")
  )

  # If forest is missing or NULL, check required inputs are provided
  if (missing(forest) || is.null(forest)) {
    if (is.null(Y) | is.null(W)) {
      stop("Y and W must be provided")
    }
    if (match == "covariates" && is.null(X)) {
      stop("X must be provided when match = 'covariates'")
    }
    if (tau_hat_method == "risk_diff" && (is.null(p_0) || is.null(p_1))) {
      stop("p_0 and p_1 must be provided when tau_hat_method = 'risk_diff'")
    }
    if ((match == "CATE" || tau_hat_method == "tau_avg") && is.null(tau_hat)) {
      stop("tau_hat must be provided when match = 'CATE' or tau_hat_method = 'tau_avg'")
    }
  }

  # Extract quantities from causal forest object
  if (is.null(Y)) Y <- forest$Y.orig
  if (is.null(W)) W <- forest$W.orig
  suppressMessages({
    if (is.null(X)) X <- dplyr::as_tibble(forest$X.orig, .name_repair = "unique")
  })
  if (is.null(p_0)) {
    p_0 <- as.numeric(forest$Y.hat - forest$W.hat * forest$predictions)
  }
  if (is.null(p_1)) {
    p_1 <- as.numeric(forest$Y.hat + (1 - forest$W.hat) * forest$predictions)
  }
  if (is.null(tau_hat)) tau_hat <- as.numeric(forest$predictions)
  subclass <- NULL

  # ensure correct data types of data inputs
  stopifnot("Y must be a numeric vector" = is.vector(Y) && is.numeric(Y))
  stopifnot("W must be a numeric vector" = is.vector(W) && is.numeric(W))
  if (match == "covariates") {
    stopifnot("X must be a data frame" = is.data.frame(X))
  }
  if (tau_hat_method == "risk_diff") {
    stopifnot("p_0 must be a numeric vector" = is.vector(p_0) && is.numeric(p_0))
    stopifnot("p_1 must be a numeric vector" = is.vector(p_1) && is.numeric(p_1))
  }
  if (match == "CATE" || tau_hat_method == "tau_avg") {
    stopifnot(
      "tau_hat must be a numeric vector" =
        is.vector(tau_hat) && is.numeric(tau_hat)
    )
  }

  # patient can only be matched to one other patient from other treatment arm
  if (!("estimand" %in% ...names())) {
    if (sum(W == 1) <= sum(W == 0)){
      # ATT: all treated patients get matched with control patient
      estimand_method <- "ATT"
    } else if (sum(W == 1) > sum(W == 0)){
      # ATC: all control patients get matched with treated patient
      estimand_method <- "ATC"
    }
  }

  # combine all data in one tibble
  data_tbl <- dplyr::tibble(
    match_id = seq_along(Y),
    W = W,
    Y = Y
  )
  if (match == "covariates") data_tbl <- dplyr::mutate(data_tbl, X = X)
  if (tau_hat_method == "risk_diff") {
    data_tbl <- dplyr::mutate(data_tbl, p_0 = p_0, p_1 = p_1)
  }
  if (match == "CATE" || tau_hat_method == "tau_avg") {
    data_tbl <- dplyr::mutate(data_tbl, tau_hat = tau_hat)
  }

  # set time limit on calculating C for benefit
  on.exit({
    setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE)
  })
  setTimeLimit(cpu = time_limit, elapsed = time_limit, transient = TRUE)

  if (match == "covariates") {
    # match on covariates
    if ("estimand" %in% ...names()) {
      matched <- MatchIt::matchit(
        DF2formula(tidyr::unnest(dplyr::select(data_tbl, W, X), "X")),
        data = tidyr::unnest(data_tbl, "X"),
        method = match_method,
        distance = match_distance,
        ...
      )
    } else {
      matched <- MatchIt::matchit(
        DF2formula(tidyr::unnest(dplyr::select(data_tbl, W, X), "X")),
        data = tidyr::unnest(data_tbl, "X"),
        method = match_method,
        distance = match_distance,
        estimand = estimand_method,
        ...
      )
    }
  } else if (match == "CATE") {
    # match on CATE
    if ("estimand" %in% ...names()) {
      matched <- MatchIt::matchit(
        W ~ tau_hat,
        data = data_tbl,
        method = match_method,
        distance = match_distance,
        ...
      )
    } else {
      matched <- MatchIt::matchit(
        W ~ tau_hat,
        data = data_tbl,
        method = match_method,
        distance = match_distance,
        estimand = estimand_method,
        ...
      )
    }
  }
  matched_patients <- MatchIt::match.data(matched)
  matched_patients$subclass <- as.numeric(matched_patients$subclass)
  suppressMessages({
    matched_patients <-
      dplyr::as_tibble(matched_patients, .name_repair = "unique")
  })

  # sort on subclass and W
  matched_patients <- matched_patients |>
    dplyr::arrange(.data$subclass, .data$W)

  # matched observed treatment effect
  observed_te <- matched_patients |>
    dplyr::select(subclass, Y) |>
    dplyr::summarise(
      Y = diff(.data$Y),
      .by = subclass
    ) |>
    dplyr::pull(.data$Y)
  matched_patients$matched_tau_obs <- rep(observed_te, each = 2)

  # matched predicted treatment effect
  if(tau_hat_method == "risk_diff") {
    # matched p_0 = P(Y = 1|W = 0)
    # matched p_1 = P(Y = 1|W = 1)
    matched_patients <- matched_patients |>
      dplyr::mutate(
        matched_p_0 = sum((1 - .data$W) * .data$p_0),
        matched_p_1 = sum(.data$W * .data$p_1),
        matched_tau_hat = .data$matched_p_1 - .data$matched_p_0,
        .by = subclass
      )
  } else if (tau_hat_method == "tau_avg") {
    # matched treatment effect (average CATE)
    matched_patients <- matched_patients |>
      dplyr::mutate(
        matched_tau_hat = mean(.data$tau_hat),
        .by = subclass
      )
  }

  # C-for-benefit
  cindex <- Hmisc::rcorr.cens(
    matched_patients$matched_tau_hat[seq(1, nrow(matched_patients), 2)],
    matched_patients$matched_tau_obs[seq(1, nrow(matched_patients), 2)]
  )
  c_for_benefit <- cindex["C Index"][[1]]
  c_for_benefit_se <- cindex["S.D."][[1]] / 2

  if (CI == "simple") {
    lower_CI <- c_for_benefit + qnorm(0.5 - level / 2) * c_for_benefit_se
    upper_CI <- c_for_benefit + qnorm(0.5 + level / 2) * c_for_benefit_se
  } else if (CI == "bootstrap") {
    CB_for_CI <- c()
    B <- 0
    if (verbose) cli::cli_progress_bar("Bootstrapping", total = n_bootstraps)
    setTimeLimit(cpu = time_limit_CI, elapsed = time_limit_CI, transient = TRUE)
    while (B < n_bootstraps) {
      tryCatch(
        {
          # bootstrap matched patient pairs
          subclass_IDs <- unique(matched_patients$subclass)
          sample_subclass <- sample(
            subclass_IDs,
            length(subclass_IDs),
            replace = TRUE
          )
          # matched_patients is ordered by subclass
          # (and each subclass has 2 members)
          duplicated_matched_patients <- matched_patients |>
            dplyr::slice({{ sample_subclass }} * 2)
          # calculate C-for-benefit for duplicated matched pairs
          duplicated_cindex <- Hmisc::rcorr.cens(
            duplicated_matched_patients$matched_tau_hat,
            duplicated_matched_patients$matched_tau_obs
          )
          CB_for_CI <- c(CB_for_CI, duplicated_cindex["C Index"][[1]])
          B <- B + 1
          if (verbose) cli::cli_progress_update()
        },
        error = function(e) {
          if (
            grepl(
              "reached elapsed time limit|reached CPU time limit",
              e$message
            )
          ) {
            for (i in 1:3) {
              input <- readline(
                "Time limit reached. Do you want execution to continue (y/n) "
              )
              if (input =="y") {
                break
              } else if (input == "n") {
                stop("Time limit reached")
              }
              if (i == 3) {
                stop("Failed to answer 'y' or 'n' to many times")
              }
            }
          } else {
            stop(e)
          }
        }
      )
    }
    lower_CI <- as.numeric(quantile(CB_for_CI, 0.5 - level / 2))
    upper_CI <- as.numeric(quantile(CB_for_CI, 0.5 + level / 2))
  } else if (CI == "none") {
    lower_CI <- NA
    upper_CI <- NA
  }

  return(
    list(
      type = list(
        match = match,
        match_method = match_method,
        match_distance = match_distance,
        tau_hat_method = tau_hat_method,
        CI = CI,
        level = level,
        n_bootstraps = n_bootstraps
      ),
      matched_patients = matched_patients,
      c_for_benefit = c_for_benefit,
      lower_CI = lower_CI,
      upper_CI = upper_CI,
      cfb_tbl = dplyr::tibble(
        "ci_{0.5 - level / 2}" := lower_CI,
        "estimate" = c_for_benefit,
        "ci_{0.5 + level / 2}" := upper_CI
      )
    )
  )
}
