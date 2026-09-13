#' Calculate CATE in dynamically determined subgroups
#'
#' Determines subgroups ranked by CATE estimates from a causal_forest object,
#' then calculates comparable CATE estimates in each subgroup and tests for
#' differences.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}().
#' @param n_rankings Integer, scalar with number of groups to rank CATE's into.
#' @param n_folds Integer, scalar with number of folds to split data into.
#' @param ... Additional arguments passed to \link[grf]{causal_forest}() and
#'   \link[grf]{regression_forest}().
#'
#' @return A list with elements
#'  - forest_subgroups: A tibble with CATE estimates, ranking, and AIPW-scores
#'   for each subject.
#'  - forest_rank_ate: A tibble with the ATE estimate and standard error of
#'   each subgroup.
#'  - forest_rank_diff_test: A tibble with estimates of the difference in ATE
#'   between subgroups and p-values for a formal test of no difference.
#'  - heatmap_data: A tibble with data used to draw a heatmap of covariate
#'   distribution in each subgroup.
#'  - forest_rank_ate_plot: ggplot with the ATE estimates in each subgroup.
#'  - heatmap: ggplot with heatmap of covariate distribution in each subgroup.
#'
#' @details To evaluate heterogeneity in treatment effect one can split data
#'   into groups by estimated CATE (for an alternative, see also
#'   \link[EpiForsk]{RATEOmnibusTest}). To compare estimates one must use a
#'   model which is not trained on the subjects we wish to compare. To achieve
#'   this, data is partitioned into n_folds folds and a causal forest is trained
#'   for each fold where the fold is left out. If the data has no existing
#'   clustering, one \link[grf]{causal_forest}() is trained with the folds as
#'   clustering structure. This enables predictions on each fold where trees
#'   using data from the fold are left out for the prediction. In the case of
#'   preexisting clustering in the data, folds are sampled within each cluster
#'   and combined across clusters afterwards.
#'
#' @author KIJA
#'
#' @examples
#' \donttest{
#' n <- 800
#' p <- 3
#' X <- matrix(rnorm(n * p), n, p) |> as.data.frame()
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' Y <- rbinom(n, 1, event_prob)
#' cf <- grf::causal_forest(X, Y, W)
#' cf_ds <- CausalForestDynamicSubgroups(cf, 2, 4)
#' }
#'
#' @export

CausalForestDynamicSubgroups <- function(forest,
                                         n_rankings = 3,
                                         n_folds = 5,
                                         ...) {
  # save dots arguments
  args <- list(...)

  # check arguments
  stopifnot(
    "'forest' must be an object of class causal_forest" =
      inherits(forest, "causal_forest")
  )
  stopifnot(
    "n_rankings must be a positive integer or double coercible to a positive integer" =
      is.numeric(n_rankings) && trunc(n_rankings > 0.9)
  )
  stopifnot(
    "n_folds must be a positive integer or double coercible to a positive integer" =
      is.numeric(n_folds) && trunc(n_folds > 0.9)
  )
  n_rankings <- trunc(n_rankings)
  n_folds <- trunc(n_folds)

  # add names to forest$X.orig if missing
  if (is.null(colnames(forest$X.orig))) {
    warning(
      "Covariates used to train forest are unnamed. Names X_{colnr} are created.",
      immediate. = TRUE
    )
    colnames(forest$X.orig) <- paste0("X_", seq_len(ncol(forest$X.orig)))
  }

  # Methods for forest with and without clustering
  if (length(forest$clusters) > 0) {
    # partition data
    folds <- rep(0, length(forest$clusters))
    for (i in unique(forest$clusters)) {
      folds[forest$clusters == i] <- sample(
        sort(seq_along(folds[forest$clusters == i]) %% n_folds) + 1
      )
    }
    n <- length(forest$Y.orig)
    indices <- split(seq_len(n), folds)
    result <- purrr::map(
      indices,
      \(idx, args) {
        # Fit outcome model and predict on held-out data. Note fitting a causal
        # forest only gives outcome predictions on data included in training data.
        forest_m <- do.call(
          grf::regression_forest,
          c(
            list(
              X = forest$X.orig[-idx,],
              Y = forest$Y.orig[-idx],
              clusters = forest$clusters[-idx]
            ),
            args
          )
        )
        m_hat <- predict(forest_m, forest$X.orig[idx,])$predictions
        # Fit exposure model and predict on held-out data. Note fitting a causal
        # forest only gives exposure predictions on data included in training data.
        forest_e <- do.call(
          grf::regression_forest,
          c(
            list(
              X = forest$X.orig[-idx,],
              Y = forest$W.orig[-idx],
              clusters = forest$clusters[-idx]
            ),
            args
          )
        )
        e_hat <- predict(forest_e, forest$X.orig[idx,])$predictions
        # train forest without held-out fold and original clustering
        forest_rank <- do.call(
          grf::causal_forest,
          c(
            list(
              X = forest$X.orig[-idx,],
              Y = forest$Y.orig[-idx],
              W = forest$W.orig[-idx],
              Y.hat = forest_m$predictions,
              W.hat = forest_e$predictions,
              clusters = forest$clusters[-idx]
            ),
            args
          )
        )

        # Estimate cate's in held-out fold
        tau_hat <- predict(
          object = forest_rank,
          newdata = forest$X.orig[idx,]
        )$predictions

        # aipw scores in held-out fold
        mu_hat_0 <- m_hat - e_hat * tau_hat
        mu_hat_1 <- m_hat + (1 - e_hat) * tau_hat

        aipw_scores <-
          tau_hat +
          forest$W.orig[idx] / e_hat * (forest$Y.orig[idx] -  mu_hat_1) -
          (1 - forest$W.orig[idx]) / (1 - e_hat) * (forest$Y.orig[idx] - mu_hat_0)

        # rank observations by cate in held-out fold
        tau_hat_quantiles <- quantile(
          x = tau_hat,
          probs = seq(0, 1, by = 1 / n_rankings)
        )
        # if quantiles are not unique, manually sort and cut into appropriate
        # groups
        if (length(tau_hat_quantiles) == length(unique(tau_hat_quantiles))) {
          ranking <- cut(
            x = tau_hat,
            breaks = tau_hat_quantiles,
            include.lowest = TRUE,
            labels = seq_len(n_rankings)
          )
        } else {
          len <- length(tau_hat)
          ranking <- tau_hat |>
            (\(x) {
              dplyr::tibble(
                id = seq_along(x),
                tau_hat = x
              )
            }
            )() |>
            dplyr::arrange(.data$tau_hat) |>
            dplyr::mutate(
              id_2 = seq_along(.data$tau_hat),
              rank = cut(
                x = .data$id_2,
                breaks = c(
                  seq(0, len %% n_rankings, by = 1) *
                    (len %/% n_rankings + 1),
                  seq(
                    len %% n_rankings + 1,
                    n_rankings,
                    length.out = n_rankings - len %% n_rankings
                  ) *
                    len %/% n_rankings + len %% n_rankings
                ),
                include.lowest = TRUE,
                labels = seq_len(n_rankings)
              )
            ) |>
            dplyr::arrange(.data$id) |>
            dplyr::pull(.data$rank)
        }

        # collect ranking and aipw scores
        return(
          dplyr::tibble(
            id = idx,
            tau_hat = tau_hat,
            ranking = ranking,
            aipw_scores = aipw_scores
          )
        )
      },
      args
    ) |>
      purrr::list_rbind()
    result <- dplyr::arrange(result, .data$id)
  } else {
    # partition data into folds
    folds <- sort(seq_along(forest$Y.orig) %% n_folds) + 1

    # train forest using folds as clusters
    forest_rank <- do.call(
      grf::causal_forest,
      c(
        list(
          X = forest$X.orig,
          Y = forest$Y.orig,
          W = forest$W.orig,
          clusters = folds
        ),
        args
      )
    )

    # estimate cate's. Note that the cluster containing the sample to predict is
    # left out for prediction.
    tau_hat <- predict(object = forest_rank)$predictions

    # rank observations within folds
    ranking <- rep(NA, length(forest$Y.orig))
    for (fold in seq_len(n_folds)) {
      tau_hat_quantiles <- quantile(
        x = tau_hat[folds == fold],
        probs = seq(0, 1, by = 1 / n_rankings)
      )
      # if quantiles are not unique, manually sort and cut into appropriate
      # groups
      if (length(tau_hat_quantiles) == length(unique(tau_hat_quantiles))) {
        ranking[folds == fold] <- cut(
          x = tau_hat[folds == fold],
          breaks = tau_hat_quantiles,
          include.lowest = TRUE,
          labels = seq_len(n_rankings)
        )
      } else {
        len <- length(tau_hat[folds == fold])
        ranking[folds == fold] <- tau_hat[folds == fold] |>
          (\(x) {
            dplyr::tibble(
              id = seq_along(x),
              tau_hat = x
            )
          }
          )() |>
          dplyr::arrange(.data$tau_hat) |>
          dplyr::mutate(
            id_2 = seq_along(.data$tau_hat),
            rank = cut(
              x = .data$id_2,
              breaks = c(
                seq(0, len %% n_rankings, by = 1) *
                  (len %/% n_rankings + 1),
                seq(
                  len %% n_rankings + 1,
                  n_rankings,
                  length.out = n_rankings - len %% n_rankings
                ) *
                  len %/% n_rankings + len %% n_rankings
              ),
              include.lowest = TRUE,
              labels = seq_len(n_rankings)
            )
          ) |>
          dplyr::arrange(.data$id) |>
          dplyr::pull(.data$rank)
      }
    }

    # aipw scores
    mu_hat_0 <- forest_rank$Y.hat - forest_rank$W.hat * tau_hat
    mu_hat_1 <- forest_rank$Y.hat + (1 - forest_rank$W.hat) * tau_hat

    aipw_scores <-
      tau_hat +
      forest$W.orig / forest_rank$W.hat * (forest$Y.orig -  mu_hat_1) -
      (1 - forest$W.orig) / (1 - forest_rank$W.hat) * (forest$Y.orig - mu_hat_0)

    # collect ranking and aipw scores
    result <- dplyr::tibble(
      id = seq_along(forest$Y.orig),
      tau_hat = tau_hat,
      ranking = ranking,
      aipw_scores = aipw_scores
    )
  }

  # fit linear model of aipw scores to find average in each rank
  ols <- lm(result$aipw_scores ~ 0 + factor(result$ranking))
  forest_rank_ate <- dplyr::tibble(
    method = "aipw",
    ranking = paste0("Q", seq_len(n_rankings)),
    estimate = coef(ols),
    std_err = sqrt(diag(vcovHC(ols)))
  )

  # plot with estimates and 95 % confidence intervals within each ranking:
  forest_rank_ate_plot <- ggplot2::ggplot(
    forest_rank_ate,
    ggplot2::aes(x = .data$ranking, y = .data$estimate)
  ) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$estimate + qnorm(0.025) * .data$std_err,
        ymax = .data$estimate + qnorm(0.975) * .data$std_err
      ),
      width = 0.2
    ) +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::ggtitle(
      "AIPW score within each ranking (as defined by predicted CATE)"
    ) +
    ggplot2::theme_bw()

  # table with tests for differences between ranking groups
  forest_rank_diff_test <- dplyr::tibble()
  for (i in seq_len(n_rankings - 1)) {
    lev <- seq_len(n_rankings)
    ols <- lm(
      result$aipw_scores ~
        1 + factor(result$ranking, levels = c(lev[i], lev[-i]))
    )
    forest_rank_diff_test <- coef(summary(ols))[
      seq(i + 1, n_rankings),
      c(1, 2, 4),
      drop = FALSE
    ] |>
      dplyr::as_tibble() |>
      dplyr::mutate(
        id = paste("Rank",  seq(i + 1, n_rankings), "- Rank ", i)
      ) |>
      (\(x) rbind(forest_rank_diff_test, x))()
  }

  # Adjust for multiple testing using the Benjamini-Hockberg procedure
  forest_rank_diff_test <- forest_rank_diff_test |>
    dplyr::rename("Orig. p-value" = "Pr(>|t|)") |>
    dplyr::mutate(
      `95% CI` = paste0(
        "(",
        sprintf(
          fmt = "%.3f",
          .data$Estimate + qnorm(0.025) * .data$`Std. Error`
        ),
        ", ",
        sprintf(
          fmt = "%.3f",
          .data$Estimate + qnorm(0.975) * .data$`Std. Error`
        ),
        ")"
      ),
      `Orig. p-value` = sprintf(
        fmt = "%.3f",
        .data$`Orig. p-value`
      ),
      `Adj. p-value` = sprintf(
        fmt = "%.3f",
        p.adjust(.data$`Orig. p-value`, method = "BH")
      )
    ) |>
    dplyr::select(
      "id",
      "Estimate",
      "Std. Error",
      "95% CI",
      dplyr::everything()
    )

  # Plot heatmap with average of covariates in each group
  heatmap_data <- purrr::map(
    colnames(forest$X.orig),
    \(covariate) {
      # calculate average and standard error of each covariate
      fmla <- formula(paste0("`", covariate, "`", "~ 0 + ranking"))
      data_forest <- forest$X.orig |>
        dplyr::as_tibble() |>
        dplyr::mutate(
          Y = forest$Y.orig,
          W = forest$W.orig,
          ranking = factor(result$ranking)
        )
      ols <- lm(formula = fmla, data = data_forest)

      # results
      avg <- coef(ols)
      stderr <- sqrt(diag(vcovHC(ols)))

      # collect results in table
      dplyr::tibble(
        covariate = covariate,
        avg = avg,
        stderr = stderr,
        ranking = paste0("Q", seq_len(n_rankings)),
        scaling = pnorm((avg - mean(avg)) / sd(avg)),
        # standard error between groups normalized by
        # standard error between all observations:
        variation = sd(avg) / sd(data_forest[[{{ covariate }}]]),
        labels = paste0(
          sprintf("%.3f", avg),
          "\n",
          "(",
          sprintf("%.3f", stderr),
          ")"
        )
      )
    }
  ) |>
    purrr::list_rbind() |>
    # ensure the heatmap will be in descending order of variation, defined
    # as the standard error of the average within groups normalized by the
    # standard error of all values of the covariate; i.e. the amount of
    # variation explained by the averages relative to the total variation of
    # the covariate:
    dplyr::mutate(
      covariate = forcats::fct_reorder(.data$covariate, .data$variation)
    )

  # plot heatmap
  heatmap <- ggplot2::ggplot(heatmap_data) +
    ggplot2::aes(.data$ranking, .data$covariate) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$scaling)) +
    ggplot2::geom_text(ggplot2::aes(label = labels)) +
    ggplot2::scale_fill_gradient(low = "#0000FF", high = "#FF8000") +
    ggplot2::ggtitle(
      "Average covariate values within group (based on CATE estimate ranking)"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::xlab("CATE estimate ranking") +
    ggplot2::ylab("") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 11, face = "bold"),
      axis.text = ggplot2::element_text(size = 11)
    )

  return(
    list(
      forest_subgroups = result,
      forest_rank_ate = forest_rank_ate,
      forest_rank_ate_plot = forest_rank_ate_plot,
      forest_rank_diff_test = forest_rank_diff_test,
      heatmap_data = heatmap_data,
      heatmap = heatmap
    )
  )
}
