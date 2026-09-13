# R と Python の学習・予測で共通に使う共変量と、その列順を定義する。
ml_feature_names <- function() {
  c("age", "sexm1", "bmi", "hf", "bnp", "lvef")
}

# 傾向スコアで 5 層に分け、治療群を対象とする ATT の比較集団を作る。
make_subclassification_fit <- function(lalonde_data) {
  MatchIt::matchit(
    treat ~ age + educ + nodegree + married + re74 + re75,
    data = lalonde_data,
    method = "subclass",
    subclass = 5,
    estimand = "ATT"
  )
}

# 治療と共変量の交互作用を許した回帰から、治療群で平均した効果を求める。
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
    newdata = dplyr::filter(matched_data, treat == 1)
  )
}

# 各治療例に近い傾向スコアの対照例を対応させる。
make_nearest_matching_fit <- function(lalonde_data) {
  MatchIt::matchit(
    treat ~ age + educ + race + nodegree + married + re74 + re75,
    data = lalonde_data,
    method = "nearest",
    estimand = "ATT"
  )
}

make_matching_result <- function(nearest_matching_fit) {
  # 最近傍マッチングで得た重みを回帰に使い、マッチ集合内の相関を
  # subclass によるクラスタ頑健分散で考慮する。
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
    newdata = dplyr::filter(matched_data, treat == 1)
  )
}

# 傾向スコアの重みを推定し、その推定を考慮した回帰から治療群での平均効果を求める。
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
    newdata = dplyr::filter(lalonde_data, treat == 1)
  )
}

# アウトカム回帰と傾向スコアを組み合わせ、5 分割の cross-fitting で ATE を推定する。
make_aipw_result <- function(lalonde_data) {
  covariates <- lalonde_data |>
    dplyr::select(age, educ, race, married, nodegree, re74, re75)

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

# アウトカムの初期予測を傾向スコアで更新し、ATE と信頼区間を取り出す。
make_tmle_result <- function(lalonde_data) {
  covariates <- lalonde_data |>
    dplyr::select(age, educ, race, married, nodegree, re74, re75)

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

# 共変量で説明される成分を取り除き、残差同士の回帰で治療効果を示す教材例。
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

# シミュレーションで既知の個体別リスク差を平均し、標本内のばらつきも要約する。
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

# 開発・外部検証コホート間の、意図したcase-mix shiftを数値で確認する。
make_external_shift_summary <- function(toy_data, test_toy_data) {
  summarise_cohort <- function(data, cohort) {
    tibble::tibble(
      cohort = cohort,
      n = nrow(data),
      mean_age = mean(data$age),
      mean_bmi = mean(data$bmi),
      female_fraction = mean(data$sexm1 == 0)
    )
  }

  cohort_summary <- dplyr::bind_rows(
    summarise_cohort(toy_data, "Development"),
    summarise_cohort(test_toy_data, "External validation")
  )

  development <- dplyr::filter(cohort_summary, cohort == "Development")
  external <- dplyr::filter(
    cohort_summary,
    cohort == "External validation"
  )

  list(
    cohorts = cohort_summary,
    differences = tibble::tibble(
      contrast = "External validation - Development",
      age_difference = external$mean_age - development$mean_age,
      bmi_difference = external$mean_bmi - development$mean_bmi,
      female_fraction_difference =
        external$female_fraction - development$female_fraction
    )
  )
}

make_policy_features <- function(toy_data) {
  # 列順は Python 側の FEATURE_NAMES と揃える。GRF との境界で data.frame にする。
  toy_data |>
    dplyr::select(dplyr::all_of(ml_feature_names())) |>
    as.data.frame()
}

# イベント発生をアウトカムにするため、CATE が負なら治療によるリスク低下を表す。
fit_causal_forest_bin <- function(policy_features, toy_data) {
  grf::causal_forest(
    X = policy_features,
    Y = toy_data$bin_outcome,
    W = toy_data$ca,
    honesty = TRUE,
    seed = 42
  )
}

# 学習データに対する OOB 予測を ID に対応付ける。入力データの行順を維持する。
make_causal_forest_predictions <- function(causal_forest_bin, toy_data) {
  tibble::tibble(
    id = toy_data$id,
    cate_rd = as.numeric(stats::predict(causal_forest_bin)$predictions)
  )
}

# 各治療選択の価値を評価するための二重頑健スコアを取り出す。
make_causal_forest_dr_scores <- function(causal_forest_bin) {
  policytree::double_robust_scores(causal_forest_bin)
}

# 背景集団を基準に、イベントリスク差の予測への各共変量の寄与を説明する。
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

# 予測 CATE を深さ 3 の木で近似し、効果の異質性を読み取りやすくする。
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

# 非発生を効用にすることで、正の CATE が利益となり、方策の最大化と向きが揃う。
fit_causal_forest_policy <- function(policy_features, toy_data) {
  grf::causal_forest(
    X = policy_features,
    Y = toy_data$bin_event_free,
    W = toy_data$ca,
    honesty = TRUE,
    seed = 42
  )
}

# 非発生確率を効用とした、治療選択ごとの二重頑健スコアを得る。
make_policy_scores <- function(causal_forest_policy) {
  policytree::double_robust_scores(causal_forest_policy)
}

# 期待効用が高くなる治療選択を、深さ 2 の簡潔なルールとして学習する。
fit_policy_tree <- function(policy_features, policy_scores) {
  policytree::policy_tree(
    policy_features,
    policy_scores,
    depth = 2
  )
}

# OOB CATE による順位付けを AUTOC と QINI で評価する。符号はイベントリスク差のまま。
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

  # OOB（各個体を学習に使わなかった木）の予測で、小さい CATE から群分けする。
  # ceiling による従来の境界を保つ。ntile() は端数の配分が異なるため使わない。
  gate_data <- tibble::tibble(
    cate_predictions = cate_predictions,
    dr_scores = dr_scores
  ) |>
    dplyr::mutate(
      group = pmin(
        n_groups,
        ceiling(dplyr::row_number(cate_predictions) / dplyr::n() * n_groups)
      ),
      # 個体数より群数が多い場合も、空の群を結果に残す。
      group = factor(group, levels = seq_len(n_groups))
    ) |>
    dplyr::group_by(group, .drop = FALSE) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted_gate = mean(cate_predictions),
      gate = mean(dr_scores),
      gate_se = stats::sd(dr_scores) / sqrt(dplyr::n()),
      .groups = "drop"
    ) |>
    dplyr::mutate(group = as.integer(group))

  # 群の大きさで誤差を重み付けし、全員に平均効果を予測する基準と比較する。
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

# 開発コホートで学習済みのforestを固定し、外部コホートだけに予測する。
make_external_causal_forest_predictions <- function(
  causal_forest_bin,
  test_policy_features,
  test_toy_data
) {
  tibble::tibble(
    id = test_toy_data$id,
    cate_rd = as.numeric(
      stats::predict(
        causal_forest_bin,
        newdata = as.data.frame(test_policy_features)
      )$predictions
    )
  )
}

# 外部検証コホート内でcross-fittingされたnuisance推定からDR scoreを作る。
# このforestのCATE予測は使わず、開発コホート由来の固定予測を評価する。
fit_external_evaluation_forest <- function(
  test_policy_features,
  test_toy_data
) {
  grf::causal_forest(
    X = test_policy_features,
    Y = test_toy_data$bin_outcome,
    W = test_toy_data$ca,
    honesty = TRUE,
    seed = 43
  )
}

make_external_rate_results <- function(
  external_evaluation_forest,
  external_causal_forest_predictions
) {
  priorities <- external_causal_forest_predictions$cate_rd

  list(
    autoc = grf::rank_average_treatment_effect(
      external_evaluation_forest,
      priorities,
      target = "AUTOC"
    ),
    qini = grf::rank_average_treatment_effect(
      external_evaluation_forest,
      priorities,
      target = "QINI"
    )
  )
}

# 固定された外部CATE予測を、DR scoreに対するBLPとGATEで校正評価する。
make_external_grf_validation <- function(
  external_causal_forest_predictions,
  external_evaluation_forest,
  external_rate_results,
  n_groups = 5L
) {
  cate_predictions <- external_causal_forest_predictions$cate_rd
  dr_scores <- as.numeric(grf::get_scores(external_evaluation_forest))
  stopifnot(length(cate_predictions) == length(dr_scores))

  mean_prediction <- rep(mean(cate_predictions), length(cate_predictions))
  calibration_data <- tibble::tibble(
    dr_score = dr_scores,
    mean_prediction = mean_prediction,
    differential_prediction = cate_predictions - mean(cate_predictions)
  )
  calibration_fit <- stats::lm(
    dr_score ~ 0 + mean_prediction + differential_prediction,
    data = calibration_data
  )
  calibration_vcov <- sandwich::vcovHC(calibration_fit, type = "HC3")
  calibration_estimate <- stats::coef(calibration_fit)
  calibration_se <- sqrt(diag(calibration_vcov))
  calibration_df <- stats::df.residual(calibration_fit)

  order_index <- order(cate_predictions)
  gate_group <- integer(length(cate_predictions))
  gate_group[order_index] <- pmin(
    n_groups,
    ceiling(seq_along(order_index) / length(order_index) * n_groups)
  )

  gate_data <- purrr::map_dfr(seq_len(n_groups), function(group) {
    selected <- gate_group == group
    tibble::tibble(
      group = group,
      n = sum(selected),
      predicted_gate = mean(cate_predictions[selected]),
      gate = mean(dr_scores[selected]),
      gate_se = stats::sd(dr_scores[selected]) / sqrt(sum(selected))
    )
  })

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
    estimate = unname(calibration_estimate),
    std_error = unname(calibration_se),
    p_value = 2 * stats::pt(
      -abs(calibration_estimate / calibration_se),
      df = calibration_df
    )
  )
  discrimination_rows <- tibble::tibble(
    metric = c("AUTOC", "QINI"),
    estimate = c(
      external_rate_results$autoc$estimate,
      external_rate_results$qini$estimate
    ),
    std_error = c(
      external_rate_results$autoc$std.err,
      external_rate_results$qini$std.err
    ),
    p_value = 2 * stats::pnorm(-abs(estimate / std_error))
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
    calibration_fit = calibration_fit,
    dr_scores = dr_scores
  )
}

# ageだけで治療・対照を1:1対応させ、全HTEモデルに共通の評価ペアを作る。
make_external_benefit_pairs <- function(test_toy_data) {
  set.seed(44)
  estimand <- if (sum(test_toy_data$ca == 1) <= sum(test_toy_data$ca == 0)) {
    "ATT"
  } else {
    "ATC"
  }
  matching_fit <- MatchIt::matchit(
    ca ~ age,
    data = test_toy_data,
    method = "nearest",
    distance = "mahalanobis",
    caliper = c(age = 0.1),
    estimand = estimand,
    ratio = 1,
    replace = FALSE
  )

  MatchIt::match_data(matching_fit) |>
    dplyr::group_by(subclass) |>
    dplyr::filter(dplyr::n() == 2L, dplyr::n_distinct(ca) == 2L) |>
    dplyr::summarise(
      treated_id = id[ca == 1][1],
      control_id = id[ca == 0][1],
      # 有害イベントなので、対照−治療が正なら観察上のbenefitが大きい。
      observed_benefit =
        bin_outcome[ca == 0][1] - bin_outcome[ca == 1][1],
      age_difference = age[ca == 1][1] - age[ca == 0][1],
      .groups = "drop"
    )
}

# 異なるobserved benefitを持つ二つのmatched pairを比較し、予測順位の
# concordance（予測tieは0.5）をc-for-benefitとして計算する。
calculate_c_for_benefit <- function(
  predicted_benefit,
  observed_benefit
) {
  keep <- stats::complete.cases(predicted_benefit, observed_benefit)
  predicted_benefit <- predicted_benefit[keep]
  observed_benefit <- observed_benefit[keep]

  observed_levels <- sort(unique(observed_benefit))
  if (length(observed_levels) < 2L) {
    return(NA_real_)
  }

  concordant_pairs <- 0
  comparable_pairs <- 0
  for (high_index in 2:length(observed_levels)) {
    for (low_index in seq_len(high_index - 1L)) {
      high_predictions <- predicted_benefit[
        observed_benefit == observed_levels[high_index]
      ]
      low_predictions <- predicted_benefit[
        observed_benefit == observed_levels[low_index]
      ]
      n_high <- length(high_predictions)
      n_low <- length(low_predictions)
      pooled_ranks <- rank(
        c(high_predictions, low_predictions),
        ties.method = "average"
      )
      # Wilcoxon rank-sum identity: prediction ties contribute 0.5.
      concordant_pairs <- concordant_pairs +
        sum(pooled_ranks[seq_len(n_high)]) - n_high * (n_high + 1) / 2
      comparable_pairs <- comparable_pairs + n_high * n_low
    }
  }
  concordant_pairs / comparable_pairs
}

make_external_c_for_benefit <- function(
  external_benefit_pairs,
  external_meta_learner_effects_file,
  external_causal_forest_predictions,
  n_bootstrap = 500L
) {
  meta_predictions <- utils::read.csv(
    external_meta_learner_effects_file
  ) |>
    tibble::as_tibble()
  predictions <- dplyr::bind_rows(
    meta_predictions,
    external_causal_forest_predictions |>
      dplyr::mutate(learner = "Causal forest", .before = cate_rd)
  )

  pair_predictions <- predictions |>
    dplyr::inner_join(
      external_benefit_pairs,
      by = c("id" = "treated_id")
    ) |>
    dplyr::rename(treated_cate_rd = cate_rd) |>
    dplyr::select(-id) |>
    dplyr::inner_join(
      predictions |>
        dplyr::select(id, learner, control_cate_rd = cate_rd),
      by = c("control_id" = "id", "learner")
    ) |>
    dplyr::mutate(
      # CATEはevent risk differenceなので、符号を反転してbenefit尺度にする。
      predicted_benefit = -(treated_cate_rd + control_cate_rd) / 2
    )

  set.seed(44)
  summary <- pair_predictions |>
    dplyr::group_by(learner) |>
    dplyr::group_modify(function(data, key) {
      estimate <- calculate_c_for_benefit(
        data$predicted_benefit,
        data$observed_benefit
      )
      bootstrap_estimates <- replicate(n_bootstrap, {
        index <- sample.int(nrow(data), replace = TRUE)
        calculate_c_for_benefit(
          data$predicted_benefit[index],
          data$observed_benefit[index]
        )
      })

      tibble::tibble(
        n_pairs = nrow(data),
        c_for_benefit = estimate,
        std_error = stats::sd(bootstrap_estimates, na.rm = TRUE),
        ci_lower = stats::quantile(
          bootstrap_estimates,
          0.025,
          na.rm = TRUE,
          names = FALSE
        ),
        ci_upper = stats::quantile(
          bootstrap_estimates,
          0.975,
          na.rm = TRUE,
          names = FALSE
        )
      )
    }) |>
    dplyr::ungroup()

  list(
    pairs = external_benefit_pairs,
    pair_predictions = pair_predictions,
    summary = summary
  )
}

# 計算量を抑えつつ再現可能にするため、PDP・ICE・SHAP の対象と背景集団を固定する。
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

  # 外れ値の影響を避け、参照集団の中央 90% で特徴量を動かす。
  feature_grids <- purrr::map(explain_features, function(feature) {
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
  }) |>
    purrr::set_names(explain_features)

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

  # 一つの特徴量だけを置き換え、他の特徴量は各個体の観測値に固定する。
  # グリッドごとにまとめて予測し、PDP は集団平均、ICE は個体別の値を残す。
  effect_curves <- purrr::map(explain_features, function(feature) {
    feature_grid <- explanation_samples$feature_grids[[feature]]

    make_curve_data <- function(features) {
      features |>
        dplyr::slice(rep(seq_len(nrow(features)), times = length(feature_grid))) |>
        dplyr::mutate(
          !!feature := rep(feature_grid, each = nrow(features))
        )
    }

    pdp_predictions <- predict_benefit(make_curve_data(reference_features))
    pdp <- tibble::tibble(
      grid_id = rep(seq_along(feature_grid), each = nrow(reference_features)),
      value = rep(feature_grid, each = nrow(reference_features)),
      hte = pdp_predictions
    ) |>
      # 値が同じグリッドも別々に保持し、元の 25 点と順序を維持する。
      dplyr::group_by(grid_id) |>
      dplyr::summarise(
        value = dplyr::first(value),
        hte = mean(hte),
        .groups = "drop"
      ) |>
      dplyr::transmute(
        model = "GRF causal forest",
        feature = feature_labels[[feature]],
        value = value,
        # 従来の集計が付けていたグリッド番号の名前属性も維持する。
        hte = purrr::set_names(hte, as.character(grid_id))
      )

    ice <- tibble::tibble(
      model = "GRF causal forest",
      feature = feature_labels[[feature]],
      id = rep(seq_len(nrow(ice_features)), times = length(feature_grid)),
      value = rep(feature_grid, each = nrow(ice_features)),
      hte = predict_benefit(make_curve_data(ice_features))
    )

    list(pdp = pdp, ice = ice)
  })

  list(
    pdp = effect_curves |> purrr::map("pdp") |> dplyr::bind_rows(),
    ice = effect_curves |> purrr::map("ice") |> dplyr::bind_rows()
  )
}

# 方策用 GRF の予測は非発生確率の差なので、そのままリスク低下量として扱える。
predict_grf_benefit <- function(object, newdata) {
  as.numeric(
    stats::predict(
      object,
      newdata = as.data.frame(newdata)
    )$predictions
  )
}

# SHAP 値と説明対象データの列名を同時に変え、表示ラベルの対応を保つ。
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

# 方策用 GRF の利益予測を、共通の背景集団に対する SHAP 値に分解する。
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
