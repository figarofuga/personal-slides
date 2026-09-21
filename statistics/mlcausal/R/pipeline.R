# R と Python の学習・予測で共通に使う共変量と、その列順を定義する。
ml_feature_names <- function() {
  c("age", "sexm1", "bmi", "hf", "bnp", "lvef")
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

# 学習データの診断表示に使う値を一度だけ計算する。
make_grf_diagnostics <- function(causal_forest_bin, toy_data) {
  tau_hat <- as.numeric(stats::predict(causal_forest_bin)$predictions)
  propensity_score <- as.numeric(causal_forest_bin$W.hat)

  stopifnot(
    length(tau_hat) == nrow(toy_data),
    length(propensity_score) == nrow(toy_data)
  )

  toy_data |>
    dplyr::mutate(
      tau_hat = tau_hat,
      tau_hat_rev = -tau_hat,
      pscore = propensity_score,
      ipw = dplyr::if_else(
        ca == 1,
        1 / propensity_score,
        1 / (1 - propensity_score)
      )
    ) |>
    tibble::as_tibble()
}

make_causal_forest_calibration <- function(causal_forest_bin) {
  grf::test_calibration(causal_forest_bin)
}

# 予測 CATE を深さ 3 の木で近似し、効果の異質性を読み取りやすくする。
make_causal_forest_surrogate_tree <- function(
  causal_forest_bin,
  toy_data
) {
  interpretation_data <- toy_data |>
    dplyr::mutate(
      predicted_benefit = -as.numeric(
        stats::predict(causal_forest_bin)$predictions
      )
    ) |>
    tibble::as_tibble()

  partykit::ctree(
    predicted_benefit ~ age + sexm1 + bmi + hf + bnp + lvef,
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

# 期待効用が高くなる治療選択を、深さ 2 の簡潔なルールとして学習する。
fit_policy_tree <- function(causal_forest_policy, policy_features) {
  policy_scores <- policytree::double_robust_scores(causal_forest_policy)

  policytree::policy_tree(
    policy_features,
    policy_scores,
    depth = 2
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
    Y = 1 - test_toy_data$bin_outcome,
    W = test_toy_data$ca,
    honesty = TRUE,
    seed = 43
  )
}

# 開発コホートで固定したforestを外部コホートへ適用する。
make_external_causal_forest_predictions <- function(
  causal_forest_bin,
  test_policy_features,
  test_toy_data
) {
  cate_rd <- as.numeric(
    stats::predict(
      causal_forest_bin,
      newdata = as.matrix(test_policy_features)
    )$predictions
  )

  stopifnot(length(cate_rd) == nrow(test_toy_data))

  tibble::tibble(
    id = test_toy_data$id,
    cate_rd = cate_rd,
    predicted_benefit = -cate_rd
  )
}

# 固定予測による順位付けを、外部コホートのDR scoreで評価する。
make_external_rate_results <- function(
  external_evaluation_forest,
  external_causal_forest_predictions,
  n_bootstrap = 500L
) {
  priorities <- external_causal_forest_predictions$predicted_benefit
  fractions <- seq(0.1, 1, by = 0.1)

  set.seed(123)
  autoc <- grf::rank_average_treatment_effect(
    external_evaluation_forest,
    priorities = priorities,
    target = "AUTOC",
    q = fractions,
    R = n_bootstrap
  )
  set.seed(123)
  qini <- grf::rank_average_treatment_effect(
    external_evaluation_forest,
    priorities = priorities,
    target = "QINI",
    q = fractions,
    R = n_bootstrap
  )

  summary <- tibble::tibble(
    metric = c("AUTOC", "QINI"),
    estimate = c(autoc$estimate, qini$estimate),
    se = c(autoc$std.err, qini$std.err)
  ) |>
    dplyr::mutate(
      lower = estimate - 1.96 * se,
      upper = estimate + 1.96 * se
    )

  # grf::plot() displays TOC for both targets. Construct Qini explicitly.
  toc <- autoc$TOC |>
    dplyr::filter(priority == dplyr::first(priority))
  curves <- dplyr::bind_rows(
    dplyr::transmute(
      toc,
      q,
      curve = "TOC",
      value = estimate,
      se = std.err
    ),
    dplyr::transmute(
      toc,
      q,
      curve = "Qini",
      value = q * estimate,
      se = q * std.err
    )
  )

  list(autoc = autoc, qini = qini, summary = summary, curves = curves)
}

c_for_benefit <- function(observed_benefit, predicted_benefit) {
  stopifnot(
    length(observed_benefit) == length(predicted_benefit),
    length(observed_benefit) >= 2L,
    all(is.finite(observed_benefit)),
    all(is.finite(predicted_benefit))
  )

  observed_levels <- sort(unique(observed_benefit))
  if (length(observed_levels) < 2L) {
    return(NA_real_)
  }

  total_score <- 0
  number_of_comparisons <- 0

  for (higher_level in observed_levels[-1]) {
    predictions_in_higher_group <- predicted_benefit[
      observed_benefit == higher_level
    ]
    predictions_in_lower_groups <- predicted_benefit[
      observed_benefit < higher_level
    ]

    score_for_each_pair <- vapply(
      predictions_in_higher_group,
      function(higher_prediction) {
        sum(higher_prediction > predictions_in_lower_groups) +
          0.5 * sum(higher_prediction == predictions_in_lower_groups)
      },
      numeric(1)
    )

    total_score <- total_score + sum(score_for_each_pair)
    number_of_comparisons <- number_of_comparisons +
      length(predictions_in_higher_group) *
        length(predictions_in_lower_groups)
  }

  total_score / number_of_comparisons
}

# 予測利益で治療例と対照例を対応させ、matched pair単位でbootstrapする。
make_external_c_for_benefit <- function(
  test_toy_data,
  external_causal_forest_predictions,
  n_bootstrap = 500L
) {
  cfb_data <- test_toy_data |>
    dplyr::left_join(
      dplyr::select(
        external_causal_forest_predictions,
        id,
        predicted_benefit
      ),
      by = "id"
    )
  stopifnot(!anyNA(cfb_data$predicted_benefit))

  matching_estimand <- if (
    sum(cfb_data$ca == 1) <= sum(cfb_data$ca == 0)
  ) {
    "ATT"
  } else {
    "ATC"
  }

  set.seed(42)
  matching_fit <- MatchIt::matchit(
    ca ~ predicted_benefit,
    data = cfb_data,
    method = "nearest",
    distance = "mahalanobis",
    estimand = matching_estimand,
    ratio = 1,
    replace = FALSE,
    m.order = "closest"
  )

  pairs <- MatchIt::match_data(matching_fit) |>
    dplyr::group_by(subclass) |>
    dplyr::filter(dplyr::n() == 2L, dplyr::n_distinct(ca) == 2L) |>
    dplyr::summarise(
      treated_id = id[ca == 1][1],
      control_id = id[ca == 0][1],
      observed_benefit =
        bin_outcome[ca == 0][1] - bin_outcome[ca == 1][1],
      predicted_benefit = mean(predicted_benefit),
      .groups = "drop"
    )

  bootstrap_statistic <- function(data, indices) {
    sampled_pairs <- data[indices, , drop = FALSE]
    c_for_benefit(
      sampled_pairs$observed_benefit,
      sampled_pairs$predicted_benefit
    )
  }

  set.seed(123)
  bootstrap <- boot::boot(
    data = pairs,
    statistic = bootstrap_statistic,
    R = n_bootstrap
  )
  confidence_interval <- boot::boot.ci(
    bootstrap,
    conf = 0.95,
    type = "bca"
  )$bca[4:5]

  list(
    pairs = pairs,
    summary = tibble::tibble(
      c_for_benefit = as.numeric(bootstrap$t0),
      lower_95 = unname(confidence_interval[1]),
      upper_95 = unname(confidence_interval[2])
    )
  )
}

# GATEの境界は開発コホートだけで定め、外部コホートのDR scoreで評価する。
make_external_grf_gates <- function(
  causal_forest_bin,
  external_evaluation_forest,
  external_causal_forest_predictions,
  n_groups = 5L
) {
  predicted_benefit <-
    external_causal_forest_predictions$predicted_benefit
  dr_benefit <- as.numeric(grf::get_scores(external_evaluation_forest))
  stopifnot(length(predicted_benefit) == length(dr_benefit))

  development_benefit <- -as.numeric(
    stats::predict(causal_forest_bin)$predictions
  )
  gate_breaks <- c(
    -Inf,
    unique(as.numeric(stats::quantile(
      development_benefit,
      probs = seq_len(n_groups - 1L) / n_groups
    ))),
    Inf
  )

  tibble::tibble(
    group = cut(predicted_benefit, breaks = gate_breaks, labels = FALSE),
    predicted_benefit = predicted_benefit,
    dr_benefit = dr_benefit
  ) |>
    dplyr::group_by(group) |>
    dplyr::summarise(
      n = dplyr::n(),
      predicted_gate = mean(predicted_benefit),
      gate = mean(dr_benefit),
      gate_se = stats::sd(dr_benefit) / sqrt(n),
      .groups = "drop"
    )
}

# 計算量を抑えつつ再現可能にするため、PDP・SHAP の対象と背景集団を固定する。
make_explanation_samples <- function(policy_features) {
  explain_features <- c("age", "bmi", "bnp", "lvef")
  explain_data <- as.data.frame(policy_features)

  set.seed(42)
  reference_features <- explain_data |>
    dplyr::slice_sample(n = min(500L, nrow(explain_data)))
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
    shap = shap_features,
    background = background_features,
    feature_grids = feature_grids
  )
}

make_grf_pdp_data <- function(
  causal_forest_policy,
  explanation_samples
) {
  explain_features <- c("age", "bmi", "bnp", "lvef")
  feature_labels <- c(
    age = "Age",
    bmi = "BMI",
    lvef = "LVEF (%)",
    bnp = "BNP"
  )
  reference_features <- explanation_samples$reference

  predict_benefit <- function(newdata) {
    as.numeric(
      stats::predict(
        causal_forest_policy,
        newdata = as.data.frame(newdata)
      )$predictions
    )
  }

  # 一つの特徴量だけを置き換え、他の特徴量は各個体の観測値に固定する。
  # グリッドごとにまとめて予測し、集団平均をPDPとして返す。
  purrr::map(explain_features, function(feature) {
    feature_grid <- explanation_samples$feature_grids[[feature]]

    make_curve_data <- function(features) {
      features |>
        dplyr::slice(rep(seq_len(nrow(features)), times = length(feature_grid))) |>
        dplyr::mutate(
          !!feature := rep(feature_grid, each = nrow(features))
        )
    }

    pdp_predictions <- predict_benefit(make_curve_data(reference_features))
    tibble::tibble(
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
  }) |>
    dplyr::bind_rows()
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
