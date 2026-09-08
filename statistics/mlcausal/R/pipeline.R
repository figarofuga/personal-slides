ml_feature_names <- function() {
  c("age", "sexm1", "bmi", "hf", "bnp", "lvef")
}

make_subclassification_fit <- function(lalonde_data) {
  MatchIt::matchit(
    treat ~ age + educ + nodegree + married + re74 + re75,
    data = lalonde_data,
    method = "subclass",
    subclass = 5,
    estimand = "ATT"
  )
}

make_subclassification_result <- function(subclassification_fit) {
  matched_data <- MatchIt::match_data(subclassification_fit)

  fit <- stats::lm(
    re78 ~ treat * (age + educ + nodegree + married + re74 + re75),
    data = matched_data
  )

  marginaleffects::avg_comparisons(
    fit,
    variables = "treat",
    vcov = "HC3",
    newdata = subset(treat == 1)
  )
}

make_nearest_matching_fit <- function(lalonde_data) {
  MatchIt::matchit(
    treat ~ age + educ + race + nodegree + married + re74 + re75,
    data = lalonde_data,
    method = "nearest",
    estimand = "ATT"
  )
}

make_matching_result <- function(nearest_matching_fit) {
  # Preserve the current slide calculation exactly: the slide fits
  # nearest_matching_fit but then calls match_data() on the earlier
  # subclassification fit. See the final implementation report.
  
  matched_data <- MatchIt::match_data(nearest_matching_fit)

  fit <- stats::lm(
    re78 ~ treat * (age + educ + nodegree + married + re74 + re75),
    data = matched_data,
    weights = weights
  )

  marginaleffects::avg_comparisons(
    fit,
    variables = "treat",
    vcov = ~subclass,
    newdata = subset(treat == 1)
  )
}

make_weighting_result <- function(lalonde_data) {
  weighting_fit <- WeightIt::weightit(
    treat ~ age + educ + race + nodegree + married + re74 + re75,
    data = lalonde_data,
    method = "glm"
  )

  outcome_fit <- WeightIt::lm_weightit(
    re78 ~ treat * (age + educ + race + married + nodegree + re74 + re75),
    data = lalonde_data,
    weightit = weighting_fit
  )

  marginaleffects::avg_comparisons(
    outcome_fit,
    variables = "treat",
    newdata = subset(treat == 1)
  )
}

make_aipw_result <- function(lalonde_data) {
  covariates <- lalonde_data[c(
    "age", "educ", "race", "married", "nodegree", "re74", "re75"
  )]

  set.seed(123)
  fit <- AIPW::AIPW$new(
    Y = lalonde_data$re78,
    A = lalonde_data$treat,
    W = covariates,
    Q.SL.library = "SL.glm",
    g.SL.library = "SL.glm",
    k_split = 5L,
    verbose = FALSE
  )
  fit$fit()
  fit$summary()

  result <- fit$result["Mean Difference", ]

  tibble::tibble(
    method = "AIPW",
    estimate = unname(result["Estimate"]),
    conf_low = unname(result["95% LCL"]),
    conf_high = unname(result["95% UCL"])
  )
}

make_tmle_result <- function(lalonde_data) {
  covariates <- lalonde_data[c(
    "age", "educ", "race", "married", "nodegree", "re74", "re75"
  )]

  set.seed(123)
  fit <- tmle::tmle(
    Y = lalonde_data$re78,
    A = lalonde_data$treat,
    W = covariates,
    Q.SL.library = "SL.glm",
    g.SL.library = "SL.glm",
    family = "gaussian",
    verbose = FALSE
  )

  tibble::tibble(
    method = "TMLE",
    estimate = fit$estimates$ATE$psi,
    conf_low = fit$estimates$ATE$CI[1],
    conf_high = fit$estimates$ATE$CI[2]
  )
}

make_orthogonalization_result <- function(lalonde_data) {
  outcome_fit <- stats::glm(
    re78 ~ age + educ + race + married + nodegree + re74 + re75,
    data = lalonde_data,
    family = stats::gaussian()
  )
  treatment_fit <- stats::glm(
    treat ~ age + educ + race + married + nodegree + re74 + re75,
    data = lalonde_data,
    family = stats::binomial()
  )
  residual_data <- lalonde_data |>
    dplyr::mutate(
      y_resid = stats::residuals(outcome_fit, type = "response"),
      a_resid = treat - stats::fitted(treatment_fit)
    )
  orthogonal_fit <- stats::glm(
    y_resid ~ 0 + a_resid,
    data = residual_data,
    family = stats::gaussian()
  )
  coefficient <- summary(orthogonal_fit)$coefficients["a_resid", ]

  tibble::tibble(
    method = "Orthogonalization",
    estimate = unname(coefficient["Estimate"]),
    conf_low = estimate - 1.96 * unname(coefficient["Std. Error"]),
    conf_high = estimate + 1.96 * unname(coefficient["Std. Error"])
  )
}

make_ate_rd_summary <- function(full_toy_data) {
  full_toy_data |>
    dplyr::summarise(
      n = sum(!is.na(ite_rd_bin)),
      ate_rd = mean(ite_rd_bin, na.rm = TRUE),
      ite_rd_sd = stats::sd(ite_rd_bin, na.rm = TRUE)
    ) |>
    dplyr::mutate(
      ate_rd_se = ite_rd_sd / sqrt(n),
      critical_value = stats::qt(0.975, df = n - 1),
      ci_lower = ate_rd - critical_value * ate_rd_se,
      ci_upper = ate_rd + critical_value * ate_rd_se
    ) |>
    dplyr::select(
      n,
      ate_rd,
      ite_rd_sd,
      ate_rd_se,
      ci_lower,
      ci_upper
    )
}

make_policy_features <- function(toy_data) {
  as.data.frame(toy_data[, ml_feature_names(), drop = FALSE])
}

fit_causal_forest_bin <- function(policy_features, toy_data) {
  grf::causal_forest(
    X = policy_features,
    Y = toy_data$bin_outcome,
    W = toy_data$ca,
    honesty = TRUE,
    seed = 42
  )
}

make_causal_forest_predictions <- function(causal_forest_bin, toy_data) {
  tibble::tibble(
    id = toy_data$id,
    cate_rd = as.numeric(stats::predict(causal_forest_bin)$predictions)
  )
}

make_causal_forest_dr_scores <- function(causal_forest_bin) {
  policytree::double_robust_scores(causal_forest_bin)
}

make_causal_forest_shap <- function(causal_forest_bin, policy_features) {
  prediction_function <- function(object, newdata) {
    stats::predict(
      object,
      newdata = as.matrix(newdata)
    )$predictions
  }

  set.seed(42)
  background_features <- dplyr::slice_sample(policy_features, n = 50)
  explained_features <- dplyr::slice_sample(policy_features, n = 200)

  kernelshap::kernelshap(
    object = causal_forest_bin,
    X = explained_features,
    bg_X = background_features,
    pred_fun = prediction_function
  ) |>
    shapviz::shapviz()
}

make_causal_forest_surrogate_tree <- function(
  causal_forest_bin,
  toy_data
) {
  interpretation_data <- toy_data |>
    dplyr::mutate(
      tau_hat = stats::predict(causal_forest_bin)$predictions
    ) |>
    tibble::as_tibble()

  partykit::ctree(
    tau_hat ~ age + sexm1 + bmi + hf + bnp + lvef,
    data = interpretation_data,
    control = partykit::ctree_control(maxdepth = 3)
  )
}

fit_causal_forest_policy <- function(policy_features, toy_data) {
  grf::causal_forest(
    X = policy_features,
    Y = toy_data$bin_event_free,
    W = toy_data$ca,
    honesty = TRUE,
    seed = 42
  )
}

make_policy_scores <- function(causal_forest_policy) {
  policytree::double_robust_scores(causal_forest_policy)
}

fit_policy_tree <- function(policy_features, policy_scores) {
  policytree::policy_tree(
    policy_features,
    policy_scores,
    depth = 2
  )
}

make_rate_results <- function(causal_forest_bin) {
  cate_predictions <- stats::predict(causal_forest_bin)$predictions

  list(
    autoc = grf::rank_average_treatment_effect(
      causal_forest_bin,
      cate_predictions,
      target = "AUTOC"
    ),
    qini = grf::rank_average_treatment_effect(
      causal_forest_bin,
      cate_predictions,
      target = "QINI"
    )
  )
}

make_grf_validation <- function(
  causal_forest_bin,
  rate_results,
  n_groups = 5L
) {
  cate_predictions <- as.numeric(
    stats::predict(causal_forest_bin)$predictions
  )
  dr_scores <- as.numeric(grf::get_scores(causal_forest_bin))
  calibration_test <- grf::test_calibration(causal_forest_bin)

  # Equal-sized groups ordered from the lowest to the highest OOB CATE.
  order_index <- order(cate_predictions)
  gate_group <- integer(length(cate_predictions))
  gate_group[order_index] <- pmin(
    n_groups,
    ceiling(seq_along(order_index) / length(order_index) * n_groups)
  )

  gate_data <- lapply(seq_len(n_groups), function(group) {
    selected <- gate_group == group
    tibble::tibble(
      group = group,
      n = sum(selected),
      predicted_gate = mean(cate_predictions[selected]),
      gate = mean(dr_scores[selected]),
      gate_se = stats::sd(dr_scores[selected]) / sqrt(sum(selected))
    )
  }) |>
    dplyr::bind_rows()

  group_probability <- gate_data$n / sum(gate_data$n)
  calibration_error <- sum(
    abs(gate_data$gate - gate_data$predicted_gate) * group_probability
  )
  overall_error <- sum(
    abs(gate_data$gate - mean(dr_scores)) * group_probability
  )
  calibration_r2 <- if (overall_error > 0) {
    1 - calibration_error / overall_error
  } else {
    NA_real_
  }

  calibration_rows <- tibble::tibble(
    metric = c(
      "Mean forest calibration",
      "Differential forest calibration"
    ),
    estimate = calibration_test[, "Estimate"],
    std_error = calibration_test[, "Std. Error"],
    p_value = calibration_test[, "Pr(>t)"]
  )
  discrimination_rows <- tibble::tibble(
    metric = c("AUTOC", "QINI"),
    estimate = c(
      rate_results$autoc$estimate,
      rate_results$qini$estimate
    ),
    std_error = c(
      rate_results$autoc$std.err,
      rate_results$qini$std.err
    ),
    p_value = stats::pnorm(
      estimate / std_error,
      lower.tail = FALSE
    )
  )

  list(
    gates = gate_data,
    summary = dplyr::bind_rows(
      calibration_rows,
      tibble::tibble(
        metric = "Grouped calibration R2",
        estimate = calibration_r2,
        std_error = NA_real_,
        p_value = NA_real_
      ),
      discrimination_rows
    ),
    calibration_test = calibration_test
  )
}

make_explanation_samples <- function(policy_features) {
  explain_features <- c("age", "lvef", "bnp")
  explain_data <- as.data.frame(policy_features)

  set.seed(42)
  reference_features <- explain_data |>
    dplyr::slice_sample(n = min(500L, nrow(explain_data)))
  ice_features <- reference_features |>
    dplyr::slice_sample(n = min(20L, nrow(reference_features)))
  shap_features <- explain_data |>
    dplyr::slice_sample(n = min(40L, nrow(explain_data)))
  background_features <- explain_data |>
    dplyr::slice_sample(n = min(30L, nrow(explain_data)))

  feature_grids <- lapply(explain_features, function(feature) {
    seq(
      stats::quantile(
        reference_features[[feature]],
        0.05,
        na.rm = TRUE
      ),
      stats::quantile(
        reference_features[[feature]],
        0.95,
        na.rm = TRUE
      ),
      length.out = 25L
    )
  })
  names(feature_grids) <- explain_features

  list(
    reference = reference_features,
    ice = ice_features,
    shap = shap_features,
    background = background_features,
    feature_grids = feature_grids
  )
}

make_grf_effect_curves <- function(
  causal_forest_policy,
  explanation_samples
) {
  explain_features <- c("age", "lvef", "bnp")
  feature_labels <- c(
    age = "Age",
    lvef = "LVEF (%)",
    bnp = "BNP"
  )
  reference_features <- explanation_samples$reference
  ice_features <- explanation_samples$ice

  predict_benefit <- function(newdata) {
    as.numeric(
      stats::predict(
        causal_forest_policy,
        newdata = as.data.frame(newdata)
      )$predictions
    )
  }

  pdp_list <- vector("list", length(explain_features))
  ice_list <- vector("list", length(explain_features))

  for (index in seq_along(explain_features)) {
    feature <- explain_features[[index]]
    feature_grid <- explanation_samples$feature_grids[[feature]]

    pdp_newdata <- reference_features[
      rep(seq_len(nrow(reference_features)), times = length(feature_grid)),
      ,
      drop = FALSE
    ]
    pdp_newdata[[feature]] <- rep(
      feature_grid,
      each = nrow(reference_features)
    )
    pdp_prediction <- predict_benefit(pdp_newdata)

    pdp_list[[index]] <- tibble::tibble(
      model = "GRF causal forest",
      feature = feature_labels[[feature]],
      value = feature_grid,
      hte = vapply(
        split(
          pdp_prediction,
          rep(seq_along(feature_grid), each = nrow(reference_features))
        ),
        mean,
        numeric(1)
      )
    )

    ice_newdata <- ice_features[
      rep(seq_len(nrow(ice_features)), times = length(feature_grid)),
      ,
      drop = FALSE
    ]
    ice_newdata[[feature]] <- rep(
      feature_grid,
      each = nrow(ice_features)
    )

    ice_list[[index]] <- tibble::tibble(
      model = "GRF causal forest",
      feature = feature_labels[[feature]],
      id = rep(seq_len(nrow(ice_features)), times = length(feature_grid)),
      value = rep(feature_grid, each = nrow(ice_features)),
      hte = predict_benefit(ice_newdata)
    )
  }

  list(
    pdp = dplyr::bind_rows(pdp_list),
    ice = dplyr::bind_rows(ice_list)
  )
}

predict_grf_benefit <- function(object, newdata) {
  as.numeric(
    stats::predict(
      object,
      newdata = as.data.frame(newdata)
    )$predictions
  )
}

relabel_cate_shap <- function(shap_object) {
  feature_labels <- c(
    age = "Age",
    sexm1 = "Sex (male)",
    bmi = "BMI",
    hf = "Heart failure",
    bnp = "BNP",
    lvef = "LVEF"
  )

  colnames(shap_object$S) <- unname(
    feature_labels[colnames(shap_object$S)]
  )
  names(shap_object$X) <- unname(feature_labels[names(shap_object$X)])
  shap_object
}

make_grf_cate_shap <- function(causal_forest_policy, explanation_samples) {
  kernelshap::kernelshap(
    causal_forest_policy,
    X = explanation_samples$shap,
    bg_X = explanation_samples$background,
    pred_fun = predict_grf_benefit,
    exact = TRUE,
    verbose = FALSE
  ) |>
    shapviz::shapviz() |>
    relabel_cate_shap()
}
