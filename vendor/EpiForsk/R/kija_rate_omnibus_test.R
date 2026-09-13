#' RATE based omnibus test of heterogeneity
#'
#' Provides the P-value for a formal test of heterogeneity based on the RATE
#' statistic by Yadlowsky et al.
#'
#' @param forest An object of class `causal_forest`, as returned by
#'   \link[grf]{causal_forest}(), with binary treatment.
#' @param ... Additional argument(s) passed to methods.
#'
#' @returns A list of class `rank_average_treatment_effect` with elements
#' - estimate: the RATE estimate.
#' - std.err: bootstrapped standard error of RATE.
#' - target: the type of estimate.
#' - TOC: a data.frame with the Targeting Operator Characteristic curve
#'   estimated on grid q, along with bootstrapped SEs.
#' - confint: a data.frame with the lower and upper bounds of the RATE
#'   confidence interval.
#' - pval: the p-value for the test that RATE is non-positive.
#'
#' @details RATE evaluates the ability of a provided prioritization rule to
#'   prioritize treatment to subjects with a large benefit. In order to test for
#'   heterogeneity, we want estimated CATE's to define the prioritization rule.
#'   However, to obtain valid inference the prioritization scores must be
#'   constructed independently of the evaluating forest training data. To
#'   accomplice this, we split the data and train separate forests on each part.
#'   Then we estimate double robust scores on the observations used to train
#'   each forest, and obtain prioritization scores by predicting CATE's with
#'   each forest on the samples not used for training.
#'
#'
#' @references Yadlowsky S, Fleming S, Shah N, Brunskill E, Wager S. Evaluating
#'   Treatment Prioritization Rules via Rank-Weighted Average Treatment Effects.
#'   2021. http://arxiv.org/abs/2111.07966.
#'
#' @author
#' KIJA
#'
#' @examples
#' \donttest{
#' n <- 2000
#' p <- 5
#' X <- matrix(rnorm(n * p), n, p)
#' X_surv <- matrix(runif(n * p), n, p)
#' W <- rbinom(n, 1, 0.5)
#' event_prob <- 1 / (1 + exp(2 * (pmax(2 * X[, 1], 0) * W - X[, 2])))
#' horizon <- 1
#' failure.time <- pmin(rexp(n) * X_surv[, 1] + W, horizon)
#' censor.time <- 2 * runif(n)
#' D <- as.integer(failure.time <= censor.time)
#' Y <- rbinom(n, 1, event_prob)
#' Y_surv <- pmin(failure.time, censor.time)
#' clusters <- sample(1:4, n, replace = TRUE)
#' cf <- grf::causal_forest(X, Y, W, clusters = clusters)
#' csf <- grf::causal_survival_forest(X, round(Y_surv, 2), W, D, horizon = horizon)
#' rate_cf <- RATEOmnibusTest(cf, target = "QINI")
#' rate_csf <- RATEOmnibusTest(csf, target = "QINI")
#' rate_cf
#' rate_csf
#' }
#'
#' @export

RATEOmnibusTest <- function(forest,
                            ...) {
  UseMethod("RATEOmnibusTest")
}

#' @rdname RATEOmnibusTest
#' @param level numeric, level of RATE confidence interval.
#' @param target character, see \link[grf]{rank_average_treatment_effect}.
#' @param q numeric, see \link[grf]{rank_average_treatment_effect}.
#' @param R integer, see \link[grf]{rank_average_treatment_effect}
#' @param num.threads passed to \link[grf]{causal_forest}. Number of threads used in
#'   training. Default value is 1.
#' @param seed numeric, either length 1, in which case the same seed is used for
#' both new forests, or length 2, to train each forest with a different seed.
#' Default is `NULL`, in which case two seeds are randomly sampled.
#' @param honesty Boolean, `TRUE` if forest was trained using honesty. Otherwise `FALSE`.
#'   Argument controls if honesty is used to train the new forests on the random
#'   half-samples, so misspecification will lead to invalid results. Default is
#'   `TRUE`, the default in \link[grf]{causal_forest}.
#' @param stabilize.splits Boolean, `TRUE` if forest was trained taking treatment into
#'   account when determining the imbalance of a split. Otherwise `FALSE`.
#'   Argument controls if treatment is taken into account when determining the
#'   imbalance of a split during training of the new forests on the random
#'   half-samples, so misspecification will lead to invalid results. Default is
#'   `TRUE`, the default in \link[grf]{causal_forest}.
#' @param ... additional arguments passed to \link[grf]{causal_forest}. By default, the
#' arguments used by forest will be used to train new forests on the random
#' half-samples. Arguments provided through `...` will override these. Note that
#' sample.weights and clusters are passed to both \link[grf]{causal_forest} and
#' \link[grf]{rank_average_treatment_effect.fit}.
#'
#' @export

RATEOmnibusTest.causal_forest <- function(forest,
                                          level = 0.95,
                                          target = c("AUTOC", "QINI"),
                                          q = seq(0.1, 1, 0.1),
                                          R = 500,
                                          num.threads = 1,
                                          seed = NULL,
                                          honesty = TRUE,
                                          stabilize.splits = TRUE,
                                          ...) {
  target <- match.arg(target)
  clusters <- forest$clusters
  if (is.numeric(clusters) && length(clusters) == 0) clusters <- NULL
  id <- NULL

  # collect ... arguments in list
  cf_args <- list(...)

  # Argument checks (TODO)
  if (!all(forest$W.orig %in% c(0, 1))) {
    stop("RATEOmnibusTest only supports causal forest with binary treatment.")
  }
  stopifnot(
    "num.treads must be a positive integer" =
      is.numeric(num.threads) && num.threads >= 1
  )
  if (
    !(
      is.null(seed) ||
      (is.numeric(seed) && all(seed > 0) && length(seed) %in% c(1, 2))
    )
  ) {
    stop(
      "seed must be a numeric vector of length 1 or 2 with values > 0, or NULL"
    )
  }
  stopifnot(
    "honesty must be a boolean (TRUE or FALSE)" =
      isTRUE(honesty) | isFALSE(honesty)
  )

  # Prepare arguments for grf::causal_forest()
  if(is.null(cf_args$Y.hat)) cf_args$Y.hat <- forest$Y.hat
  if(is.null(cf_args$W.hat)) cf_args$W.hat <- forest$W.hat
  if(is.null(cf_args$`_num_trees`)) cf_args$`_num_trees` <- forest$`_num_trees`
  if(is.null(cf_args$sample.weights)) {
    cf_args$sample.weights <- forest$sample.weights
  }
  if(is.null(cf_args$clusters)) cf_args$clusters <- forest$clusters
  if(is.null(cf_args$equalize.cluster.weights)) {
    cf_args$equalize.cluster.weights <- forest$equalize.cluster.weights
  }
  if(is.null(cf_args$sample.fraction)) {
    cf_args$sample.fraction <- forest$tunable.params$sample.fraction
  }
  if(is.null(cf_args$mtry)) cf_args$mtry <- forest$tunable.params$mtry
  if(is.null(cf_args$min.node.size)) {
    cf_args$min.node.size <- forest$tunable.params$min.node.size
  }
  if(is.null(cf_args$honesty.fraction)) {
    cf_args$honesty.fraction <- forest$tunable.params$honesty.fraction
  }
  if(is.null(cf_args$honesty.prune.leaves)) {
    cf_args$honesty.prune.leaves <- forest$tunable.params$honesty.prune.leaves
  }
  if(is.null(cf_args$alpha)) cf_args$alpha <- forest$tunable.params$alpha
  if(is.null(cf_args$imbalance.penalty)) {
    cf_args$imbalance.penalty <- forest$tunable.params$imbalance.penalty
  }
  if(is.null(cf_args$ci.group.size)) {
    cf_args$ci.group.size <- forest$ci.group.size
  }
  if(is.null(cf_args$tune.parameters)) cf_args$tune.parameters <- "none"
  if(is.null(cf_args$tune.num.trees)) cf_args$tune.num.trees <- 200
  if(is.null(cf_args$tune.num.reps)) cf_args$tune.num.reps <- 50
  if(is.null(cf_args$tune.num.draws)) cf_args$tune.num.draws <- 100
  if(is.null(seed)) {
    seed <- runif(2, 0, .Machine$integer.max)
  } else if (length(seed) == 1) {
    seed <- rep(seed, 2)
  }

  # Slit data in two random half-samples. Clustering is respected.
  if (is.null(clusters)) {
    sample <- sample(
      seq_along(forest$Y.orig), as.integer(length(forest$Y.orig) / 2),
      replace = FALSE
    )
  } else {
    dt <- data.table::data.table(
      clusters = clusters,
      id = seq_along(forest$Y.orig)
    )
    sample <- dt[
      ,
      .SD[sample(x = .N, size = as.integer(.N / 2))],
      by = clusters
    ]
    sample <- sample[, id]
  }
  # Train causal forests on each half-sample
  if (length(cf_args$clusters) == 0) {
    clust_arg_sample <- cf_args$clusters
    clust_arg_nsample <- cf_args$clusters
  } else {
    clust_arg_sample <- cf_args$clusters[sample]
    clust_arg_nsample <- cf_args$clusters[-sample]
  }
  forest_1 <- grf::causal_forest(
    X = forest$X.orig[sample,],
    Y = forest$Y.orig[sample],
    W = forest$W.orig[sample],
    Y.hat = cf_args$Y.hat[sample],
    W.hat = cf_args$W.hat[sample],
    num.trees  = cf_args$`_num_trees`,
    sample.weights = cf_args$sample.weights[sample],
    clusters = clust_arg_sample,
    equalize.cluster.weights = cf_args$equalize.cluster.weights,
    sample.fraction = cf_args$sample.fraction,
    mtry = cf_args$mtry,
    min.node.size = cf_args$min.node.size,
    honesty = honesty,
    honesty.fraction = cf_args$honesty.fraction,
    honesty.prune.leaves = cf_args$honesty.prune.leaves,
    alpha = cf_args$alpha,
    imbalance.penalty = cf_args$imbalance.penalty,
    stabilize.splits = stabilize.splits,
    ci.group.size = cf_args$ci.group.size,
    tune.parameters = cf_args$tune.parameters,
    tune.num.trees = cf_args$tune.num.trees,
    tune.num.reps = cf_args$tune.num.reps,
    tune.num.draws = cf_args$tune.num.draws,
    compute.oob.predictions = FALSE,
    num.threads = num.threads,
    seed = seed[1]
  )
  forest_2 <- grf::causal_forest(
    X = forest$X.orig[-sample,],
    Y = forest$Y.orig[-sample],
    W = forest$W.orig[-sample],
    Y.hat = cf_args$Y.hat[-sample],
    W.hat = cf_args$W.hat[-sample],
    num.trees  = cf_args$`_num_trees`,
    sample.weights = cf_args$sample.weights[-sample],
    clusters = clust_arg_nsample,
    equalize.cluster.weights = cf_args$equalize.cluster.weights,
    sample.fraction = cf_args$sample.fraction,
    mtry = cf_args$mtry,
    min.node.size = cf_args$min.node.size,
    honesty = honesty,
    honesty.fraction = cf_args$honesty.fraction,
    honesty.prune.leaves = cf_args$honesty.prune.leaves,
    alpha = cf_args$alpha,
    imbalance.penalty = cf_args$imbalance.penalty,
    stabilize.splits = stabilize.splits,
    ci.group.size = cf_args$ci.group.size,
    tune.parameters = cf_args$tune.parameters,
    tune.num.trees = cf_args$tune.num.trees,
    tune.num.reps = cf_args$tune.num.reps,
    tune.num.draws = cf_args$tune.num.draws,
    compute.oob.predictions = FALSE,
    num.threads = num.threads,
    seed = seed[2]
  )

  # Obtain prioritization scores by predicting CATE from the opposite forest
  tau_1 <- predict(forest_2, newdata = forest_1$X.orig)$predictions
  tau_2 <- predict(forest_1, newdata = forest_2$X.orig)$predictions
  tau <- vector(mode = "double", length = length(forest$Y.orig))
  tau[sample] <- tau_1
  tau[-sample] <- tau_2

  # Obtain double robust scores
  tmp <- policytree::double_robust_scores(forest_1)
  dr_1 <- vector(mode = "double", length = length(forest_1$W.orig))
  for (i in seq_along(forest_1$W.orig)) {
    dr_1[i] <- tmp[i, forest_1$W.orig[i] + 1]
  }
  tmp <- policytree::double_robust_scores(forest_2)
  dr_2 <- vector(mode = "double", length = length(forest_2$W.orig))
  for (i in seq_along(forest_2$W.orig)) {
    dr_2[i] <- tmp[i, forest_2$W.orig[i] + 1]
  }
  dr <- vector(mode = "double", length = length(forest$Y.orig))
  dr[sample] <- dr_1
  dr[-sample] <- dr_2
  # Compute RATE
  if(length(cf_args$clusters) > 0) clusters <- cf_args$clusters
  rate <- grf::rank_average_treatment_effect.fit(
    DR.scores = dr,
    priorities = tau,
    target = target,
    q = q,
    R = R,
    sample.weights = cf_args$sample.weights,
    clusters = clusters
  )

  # Confidence interval and p-value
  confint <- rate$estimate +
    dplyr::tibble(
      estimate = 0,
      lower = -qnorm(1 - (1 - level) / 2) * rate$std.err,
      upper =  qnorm(1 - (1 - level) / 2) * rate$std.err
    )
  pval <- 2 * pnorm(-abs(rate$estimate) / rate$std.err)

  # Output
  out <- c(
    rate,
    list(
      confint = confint,
      pval = pval
    )
  )
  class(out) <- "rank_average_treatment_effect"
  return(out)
}

#' @rdname RATEOmnibusTest
#'
#' @param failure.times For valid results, specify the value used to train `forest`.
#' @param sample.fraction For valid results, specify the value used to train `forest`.
#' @param mtry For valid results, specify the value used to train `forest`.
#' @param min.node.size For valid results, specify the value used to train `forest`.
#' @param honesty.fraction For valid results, specify the value used to train `forest`.
#' @param honesty.prune.leaves For valid results, specify the value used to train `forest`.
#' @param alpha For valid results, specify the value used to train `forest`.
#' @param imbalance.penalty For valid results, specify the value used to train `forest`.
#'
#' @export

RATEOmnibusTest.causal_survival_forest <- function(forest,
                                          level = 0.95,
                                          target = c("AUTOC", "QINI"),
                                          q = seq(0.1, 1, 0.1),
                                          R = 500,
                                          num.threads = 1,
                                          seed = NULL,
                                          failure.times = NULL,
                                          honesty = TRUE,
                                          stabilize.splits = TRUE,
                                          sample.fraction = 0.5,
                                          mtry = min(ceiling(sqrt(ncol(forest$X.orig)) + 20), ncol(forest$X.orig)),
                                          min.node.size = 5,
                                          honesty.fraction = 0.5,
                                          honesty.prune.leaves = TRUE,
                                          alpha = 0.05,
                                          imbalance.penalty = 0,
                                          ...) {
  target <- match.arg(target)
  clusters <- forest$clusters
  if (is.numeric(clusters) && length(clusters) == 0) clusters <- NULL
  id <- NULL

  # collect ... arguments in list
  csf_args <- list(...)

  # Argument checks (TODO)
  if (!all(forest$W.orig %in% c(0, 1))) {
    stop("RATEOmnibusTest only supports causal survival forest with binary treatment.")
  }
  stopifnot(
    "num.treads must be a positive integer" =
      is.numeric(num.threads) && num.threads >= 1
  )
  if (
    !(
      is.null(seed) ||
      (is.numeric(seed) && all(seed > 0) && length(seed) %in% c(1, 2))
    )
  ) {
    stop(
      "seed must be a numeric vector of length 1 or 2 with values > 0, or NULL"
    )
  }
  stopifnot(
    "honesty must be a boolean (TRUE or FALSE)" =
      isTRUE(honesty) | isFALSE(honesty)
  )

  # Prepare arguments for grf::causal_survival_forest()
  if(is.null(csf_args$W.hat)) csf_args$W.hat <- forest$W.hat
  if(is.null(csf_args$target)) csf_args$target <- forest$target
  if(is.null(csf_args$horizon)) csf_args$horizon <- forest$horizon
  if(is.null(csf_args$failure.times)) csf_args$failure.times <- forest$failure.times
  if(is.null(csf_args$`_num_trees`)) csf_args$`_num_trees` <- forest$`_num_trees`
  if(is.null(csf_args$sample.weights)) {
    csf_args$sample.weights <- forest$sample.weights
  }
  if(is.null(csf_args$clusters)) csf_args$clusters <- forest$clusters
  if(is.null(csf_args$equalize.cluster.weights)) {
    csf_args$equalize.cluster.weights <- forest$equalize.cluster.weights
  }
  if(is.null(csf_args$ci.group.size)) {
    csf_args$ci.group.size <- forest$`_ci_group_size`
  }
  if(is.null(seed)) {
    seed <- runif(2, 0, .Machine$integer.max)
  } else if (length(seed) == 1) {
    seed <- rep(seed, 2)
  }

  # Slit data in two random half-samples. Clustering is respected.
  if (is.null(clusters)) {
    sample <- sample(
      seq_along(forest$Y.orig), as.integer(length(forest$Y.orig) / 2),
      replace = FALSE
    )
  } else {
    dt <- data.table::data.table(
      clusters = clusters,
      id = seq_along(forest$Y.orig)
    )
    sample <- dt[
      ,
      .SD[sample(x = .N, size = as.integer(.N / 2))],
      by = clusters
    ]
    sample <- sample[, id]
  }
  # Train causal forests on each half-sample
  if (length(csf_args$clusters) == 0) {
    clust_arg_sample <- csf_args$clusters
    clust_arg_nsample <- csf_args$clusters
  } else {
    clust_arg_sample <- csf_args$clusters[sample]
    clust_arg_nsample <- csf_args$clusters[-sample]
  }
  forest_1 <- grf::causal_survival_forest(
    X = forest$X.orig[sample, , drop = FALSE],
    Y = forest$Y.orig[sample],
    W = forest$W.orig[sample],
    D = forest$D.orig[sample],
    W.hat = csf_args$W.hat[sample],
    target = csf_args$target,
    horizon = csf_args$horizon,
    failure.times = failure.times,
    num.trees  = csf_args$`_num_trees`,
    sample.weights = csf_args$sample.weights[sample],
    clusters = clust_arg_sample,
    equalize.cluster.weights = csf_args$equalize.cluster.weights,
    sample.fraction = sample.fraction,
    mtry = mtry,
    min.node.size = min.node.size,
    honesty = honesty,
    honesty.fraction = honesty.fraction,
    honesty.prune.leaves = honesty.prune.leaves,
    alpha = alpha,
    imbalance.penalty = imbalance.penalty,
    stabilize.splits = stabilize.splits,
    ci.group.size = csf_args$ci.group.size,
    compute.oob.predictions = FALSE,
    num.threads = num.threads,
    seed = seed[1]
  )
  forest_2 <- grf::causal_survival_forest(
    X = forest$X.orig[-sample, , drop = FALSE],
    Y = forest$Y.orig[-sample],
    W = forest$W.orig[-sample],
    D = forest$D.orig[-sample],
    W.hat = csf_args$W.hat[-sample],
    target = csf_args$target,
    horizon = csf_args$horizon,
    failure.times = failure.times,
    num.trees  = csf_args$`_num_trees`,
    sample.weights = csf_args$sample.weights[-sample],
    clusters = clust_arg_nsample,
    equalize.cluster.weights = csf_args$equalize.cluster.weights,
    sample.fraction = sample.fraction,
    mtry = mtry,
    min.node.size = min.node.size,
    honesty = honesty,
    honesty.fraction = honesty.fraction,
    honesty.prune.leaves = honesty.prune.leaves,
    alpha = alpha,
    imbalance.penalty = imbalance.penalty,
    stabilize.splits = stabilize.splits,
    ci.group.size = csf_args$ci.group.size,
    compute.oob.predictions = FALSE,
    num.threads = num.threads,
    seed = seed[2]
  )

  # Obtain prioritization scores by predicting CATE from the opposite forest
  tau_1 <- predict(forest_2, newdata = forest_1$X.orig)$predictions
  tau_2 <- predict(forest_1, newdata = forest_2$X.orig)$predictions
  tau <- vector(mode = "double", length = length(forest$Y.orig))
  tau[sample] <- tau_1
  tau[-sample] <- tau_2

  # Obtain double robust scores
  tmp <- policytree::double_robust_scores(forest_1)
  dr_1 <- vector(mode = "double", length = length(forest_1$W.orig))
  for (i in seq_along(forest_1$W.orig)) {
    dr_1[i] <- tmp[i, forest_1$W.orig[i] + 1]
  }
  tmp <- policytree::double_robust_scores(forest_2)
  dr_2 <- vector(mode = "double", length = length(forest_2$W.orig))
  for (i in seq_along(forest_2$W.orig)) {
    dr_2[i] <- tmp[i, forest_2$W.orig[i] + 1]
  }
  dr <- vector(mode = "double", length = length(forest$Y.orig))
  dr[sample] <- dr_1
  dr[-sample] <- dr_2
  # Compute RATE
  if(length(csf_args$clusters) > 0) clusters <- csf_args$clusters
  rate <- grf::rank_average_treatment_effect.fit(
    DR.scores = dr,
    priorities = tau,
    target = target,
    q = q,
    R = R,
    sample.weights = csf_args$sample.weights,
    clusters = clusters
  )

  # Confidence interval and p-value
  confint <- rate$estimate +
    dplyr::tibble(
      estimate = 0,
      lower = -qnorm(1 - (1 - level) / 2) * rate$std.err,
      upper =  qnorm(1 - (1 - level) / 2) * rate$std.err
    )
  pval <- 2 * pnorm(-abs(rate$estimate) / rate$std.err)

  # Output
  out <- c(
    rate,
    list(
      confint = confint,
      pval = pval
    )
  )
  class(out) <- "rank_average_treatment_effect"
  return(out)
}
