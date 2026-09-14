# 観測データと、真の潜在アウトカム・効果を含む検証用データを同時に生成する。
make_toy_data <- function(
  n = 3000L,
  seed = 123L,
  base_age = 60,
  female_age_difference = 5,
  base_log_bmi = log(22) - 0.20^2 / 2,
  female_frac = 0.40,
  base_lvef = 50,
  base_ses_probability = 0.625,
  base_log_bnp = log(150) - 0.90^2 / 2,
  target_ca_prevalence = 0.52,
  target_ate_rd = -0.05
) {
  stopifnot(
    n >= 2L,
    female_frac > 0,
    female_frac < 1,
    base_ses_probability > 0,
    base_ses_probability < 1,
    target_ca_prevalence > 0,
    target_ca_prevalence < 1,
    target_ate_rd > -1,
    target_ate_rd < 1
  )
  set.seed(seed)

  inv_logit <- function(x) {
    1 / (1 + exp(-x))
  }

  # 範囲外の乱数は捨て、必要な個数になるまで補充する（端に丸める処理ではない）。
  rtruncnorm_simple <- function(n, mean, sd, lower, upper) {
    out <- numeric(0)

    while (length(out) < n) {
      x <- rnorm(n, mean = mean, sd = sd)
      x <- x[x >= lower & x <= upper]
      out <- c(out, x)
    }

    out[seq_len(n)]
  }

  # 個体ごとの平均を保ち、範囲外だった個体だけ再抽出する。
  rtruncnorm_vec <- function(mean, sd, lower, upper) {
    out <- rnorm(length(mean), mean = mean, sd = sd)
    bad <- out < lower | out > upper

    while (any(bad)) {
      out[bad] <- rnorm(sum(bad), mean = mean[bad], sd = sd)
      bad <- out < lower | out > upper
    }

    out
  }

  # BMI などの右に裾が長い分布も、同じ棄却抽出で範囲を制限する。
  rtrunclnorm_simple <- function(n, meanlog, sdlog, lower, upper) {
    out <- numeric(0)

    while (length(out) < n) {
      x <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
      x <- x[x >= lower & x <= upper]
      out <- c(out, x)
    }

    out[seq_len(n)]
  }

  # Baseline variables
  # sexm1 = 1 は男性。female_frac は女性（sexm1 = 0）の割合を指定する。
  sexm1 <- rbinom(n, size = 1, prob = 1 - female_frac)

  # 既存データとの再現性のため、群別の乱数生成順と ifelse のリサイクルを維持する。
  age <- ifelse(
    sexm1 == 0,
    rtruncnorm_simple(
      n = sum(sexm1 == 0),
      mean = base_age + female_age_difference,
      sd = 8,
      lower = 20,
      upper = 99
    ),
    rtruncnorm_simple(
      n = sum(sexm1 == 1),
      mean = base_age,
      sd = 8,
      lower = 20,
      upper = 95
    )
  ) |>
    as.integer()

  sdlog_bmi <- 0.20
  meanlog_bmi <- base_log_bmi

  bmi <- rtrunclnorm_simple(
    n = n,
    meanlog = meanlog_bmi,
    sdlog = sdlog_bmi,
    lower = 12,
    upper = 40
  )

  lvef <- rtruncnorm_simple(
    n = n,
    mean = base_lvef,
    sd = 8,
    lower = 15,
    upper = 70
  ) |>
    as.integer()

  age_z <- as.numeric(scale(age))
  bmi_z <- as.numeric(scale(bmi))
  lvef_z <- as.numeric(scale(lvef))

  p_SES <- base_ses_probability
  SES <- rbinom(n, size = 4, prob = p_SES)
  SES_z <- as.numeric(scale(SES))

  # Comorbidities
  p_hf <- inv_logit(
    -1.5 +
      0.95 * age_z +
      0.15 * sexm1 -
      0.45 * bmi_z -
      0.3 * lvef_z +
      0.2 * SES_z
  )

  hf <- rbinom(n, size = 1, prob = p_hf)

  # BNP
  sdlog_bnp <- 0.90
  meanlog_bnp <- base_log_bnp

  bnp_base <- rtrunclnorm_simple(
    n = n,
    meanlog = meanlog_bnp,
    sdlog = sdlog_bnp,
    lower = 0,
    upper = 2000
  )

  bnp <- bnp_base *
    exp(
      0.3 * age_z +
        0.10 * sexm1 -
        0.15 * bmi_z -
        0.4 * lvef_z
    )

  bnp <- pmin(pmax(bnp, 0), 2000) |>
    as.integer()

  bnp_z <- as.numeric(scale(log1p(bnp)))

  # Palpitation
  p_palpitation <- inv_logit(
    -1.0 +
      0.10 * age_z +
      0.35 * sexm1 -
      0.40 * SES_z
  )

  palpitation <- rbinom(n, size = 1, prob = p_palpitation)

  # Treatment / intervention
  # Age has the strongest negative association with CA, especially after 70.
  # Low LVEF increases CA use, while high LVEF has only a mild penalty.
  ca_score <-
    -0.085 * (age - 60) -
    0.010 * pmax(age - 70, 0)^2 +
    0.065 * pmax(50 - lvef, 0) -
    0.012 * pmax(lvef - 50, 0) +
    1.70 * hf -
    2.20 * (bmi < 18) -
    0.25 * pmax(18 - bmi, 0) +
    2.10 * palpitation +
    0.35 * sexm1 +
    0.55 * SES_z -
    0.20 * bnp_z +
    0.10 * bmi_z

  # Keep the overall CA prevalence stable across seeds and sample sizes.
  ca_intercept <- uniroot(
    function(intercept) {
      mean(inv_logit(intercept + ca_score)) - target_ca_prevalence
    },
    interval = c(-10, 10)
  )$root

  p_ca <- inv_logit(
    ca_intercept + ca_score
  )

  ca <- rbinom(n, size = 1, prob = p_ca)

  # Nonlinear prognostic effects
  bmi_risk <-
    pmax(25 - bmi, 0)^2 +
    0.3 * pmax(bmi - 30, 0)^2

  lvef_risk <-
    pmax(40 - lvef, 0)^2 +
    0.3 * pmax(60 - lvef, 0)^2

  bmi_risk_z <- as.numeric(scale(bmi_risk))
  lvef_risk_z <- as.numeric(scale(lvef_risk))
  age_risk <- pmax(age - 65, 0) / 10

  bnp_risk_raw <- dplyr::case_when(
    bnp <= 350 ~ 0.05 * (bnp / 350)^2,
    bnp <= 1500 ~
      0.05 +
        0.75 * ((bnp - 350) / (1500 - 350))^2,
    TRUE ~
      0.80 +
        0.20 * ((bnp - 1500) / (2000 - 1500))
  )

  bnp_risk <- as.numeric(scale(bnp_risk_raw))

  # Outcome risk under no treatment
  linear_predictor_outcome_a0_raw <-
    -2.0 +
    0.70 * age_risk +
    0.90 * hf -
    0.85 * SES_z +
    0.55 * bnp_risk +
    0.04 * bmi_risk +
    0.1 * lvef_risk

  # Avoid probabilities numerically equal to 0 or 1. This also leaves room
  # for both benefit and harm on the risk-difference scale.
  outcome_logit_bounds <- qlogis(c(0.005, 0.995))
  linear_predictor_outcome_a0 <- pmin(
    pmax(linear_predictor_outcome_a0_raw, outcome_logit_bounds[1]),
    outcome_logit_bounds[2]
  )

  p_bin_outcome_a0 <- inv_logit(linear_predictor_outcome_a0)

  # Heterogeneous treatment effect on the risk-difference scale.
  # A larger benefit score produces a more negative ite_rd_bin:
  # younger age, lower LVEF, higher BNP, and higher BMI imply more benefit.
  age_benefit_modifier <-
    -age_z +
    0.5 * pmax(55 - age, 0) / 10

  lvef_benefit_modifier <-
    -lvef_z +
    0.5 * pmax(45 - lvef, 0) / 10

  bmi_benefit_modifier <-
    bmi_z -
    0.5 * (bmi < 18)

  benefit_score <-
    1.00 * age_benefit_modifier +
    1.00 * lvef_benefit_modifier +
    0.80 * bnp_z +
    1.30 * bmi_benefit_modifier

  benefit_score_z <- as.numeric(scale(benefit_score))
  ite_rd_bin_raw <- -0.12 * benefit_score_z

  rd_lower_bound <- 0.005 - p_bin_outcome_a0
  rd_upper_bound <- 0.995 - p_bin_outcome_a0

  # Preserve the heterogeneous effect pattern while shifting the population
  # average toward a clinically visible 5 percentage-point risk reduction.
  ate_calibration_shift <- uniroot(
    function(shift) {
      mean(
        pmin(
          pmax(ite_rd_bin_raw + shift, rd_lower_bound),
          rd_upper_bound
        )
      ) - target_ate_rd
    },
    interval = c(-1, 1)
  )$root

  ite_rd_bin <- pmin(
    pmax(ite_rd_bin_raw + ate_calibration_shift, rd_lower_bound),
    rd_upper_bound
  )

  p_bin_outcome_a1 <- p_bin_outcome_a0 + ite_rd_bin
  linear_predictor_outcome_a1 <- qlogis(p_bin_outcome_a1)

  # Positive values denote a reduction in the log odds of the outcome.
  treatment_effect <-
    linear_predictor_outcome_a0 - linear_predictor_outcome_a1

  linear_predictor_outcome <- dplyr::if_else(
    ca == 1,
    linear_predictor_outcome_a1,
    linear_predictor_outcome_a0
  )

  p_bin_outcome <- dplyr::if_else(
    ca == 1,
    p_bin_outcome_a1,
    p_bin_outcome_a0
  )
  bin_outcome <- rbinom(n, size = 1, prob = p_bin_outcome)

  # Utility outcome for policy learning: 1 means that the adverse event
  # represented by bin_outcome did not occur. Keeping this as a separate
  # column preserves the usual event-risk interpretation of bin_outcome.
  bin_event_free <- 1L - bin_outcome

  younger_age_benefit <- pmax(65 - age, 0) / 10

  afeqt_treatment_effect <-
    4.0 +
    2.5 * (1 - sexm1) +
    1.5 * SES_z +
    1.2 * younger_age_benefit

  afeqt_treatment_effect <- pmax(1.0, afeqt_treatment_effect)

  afeqt_mean_a0 <-
    75 -
    4 * age_z -
    7.0 * sexm1 -
    2.0 * bmi_z -
    8.0 * hf +
    3.0 * SES_z -
    5.0 * bnp_z +
    3.0 * lvef_z -
    10.0 * palpitation

  afeqt_mean_a1 <- afeqt_mean_a0 + afeqt_treatment_effect
  afeqt_mean <- dplyr::if_else(ca == 1, afeqt_mean_a1, afeqt_mean_a0)

  afeqt_os <- rtruncnorm_vec(
    mean = afeqt_mean,
    sd = 8,
    lower = 0,
    upper = 100
  )

  surv_lambda <- 0.08
  surv_gamma <- 1.5
  time_5y <- 5

  ite_surv_hr <- exp(-treatment_effect)

  surv_5y_a0 <- exp(
    -surv_lambda *
      time_5y^surv_gamma *
      exp(linear_predictor_outcome_a0)
  )
  surv_5y_a1 <- exp(
    -surv_lambda *
      time_5y^surv_gamma *
      exp(linear_predictor_outcome_a1)
  )

  risk_5y_a0 <- 1 - surv_5y_a0
  risk_5y_a1 <- 1 - surv_5y_a1
  ite_surv_rd_5y <- risk_5y_a1 - risk_5y_a0

  # Final datasets
  pre_toy_data <- tibble::tibble(
    age = age,
    sexm1 = sexm1,
    bmi = bmi,
    hf = hf,
    SES = SES,
    bnp = bnp,
    lvef = lvef,
    palpitation = palpitation,
    p_ca = p_ca,
    ca = ca,
    age_risk = age_risk,
    bnp_risk = bnp_risk,
    bmi_risk = bmi_risk,
    lvef_risk = lvef_risk,
    treatment_effect = treatment_effect,
    linear_predictor_outcome_a0 = linear_predictor_outcome_a0,
    linear_predictor_outcome_a1 = linear_predictor_outcome_a1,
    linear_predictor_outcome = linear_predictor_outcome,
    p_bin_outcome_a0 = p_bin_outcome_a0,
    p_bin_outcome_a1 = p_bin_outcome_a1,
    p_bin_outcome = p_bin_outcome,
    bin_outcome = bin_outcome,
    bin_event_free = bin_event_free,
    afeqt_treatment_effect = afeqt_treatment_effect,
    afeqt_mean_a0 = afeqt_mean_a0,
    afeqt_mean_a1 = afeqt_mean_a1,
    afeqt_os = afeqt_os,
    ite_surv_hr = ite_surv_hr,
    ite_surv_rd_5y = ite_surv_rd_5y
  ) |>
    dplyr::mutate(
      id = dplyr::row_number(),
      .before = age
    )

  surv_dat <- simsurv::simsurv(
    dist = "weibull",
    lambdas = surv_lambda,
    gammas = surv_gamma,
    x = pre_toy_data,
    betas = c(linear_predictor_outcome = 1),
    maxt = time_5y
  )

  # 比較教材用：対数オッズ尺度の治療効果を共変量に依存させない。
  # リスク差尺度では、ベースラインリスクによる異質性は残る。
  treatment_effect_no_hte <- rnorm(
    n = n,
    mean = 4,
    sd = 0.2
  )

  linear_predictor_outcome_no_hte_a0 <- linear_predictor_outcome_a0

  linear_predictor_outcome_no_hte_a1 <-
    linear_predictor_outcome_no_hte_a0 - treatment_effect_no_hte

  linear_predictor_outcome_no_hte <- dplyr::if_else(
    ca == 1,
    linear_predictor_outcome_no_hte_a1,
    linear_predictor_outcome_no_hte_a0
  )

  p_bin_outcome_no_hte_a0 <- inv_logit(
    linear_predictor_outcome_no_hte_a0
  )

  p_bin_outcome_no_hte_a1 <- inv_logit(
    linear_predictor_outcome_no_hte_a1
  )

  p_bin_outcome_no_hte <- inv_logit(
    linear_predictor_outcome_no_hte
  )

  bin_outcome_no_hte <- rbinom(
    n = n,
    size = 1,
    prob = p_bin_outcome_no_hte
  )

  full_toy_data <- tibble::tibble(
    pre_toy_data,
    treatment_effect_no_hte = treatment_effect_no_hte,
    linear_predictor_outcome_no_hte_a0 =
      linear_predictor_outcome_no_hte_a0,
    linear_predictor_outcome_no_hte_a1 =
      linear_predictor_outcome_no_hte_a1,
    linear_predictor_outcome_no_hte = linear_predictor_outcome_no_hte,
    p_bin_outcome_no_hte_a0 = p_bin_outcome_no_hte_a0,
    p_bin_outcome_no_hte_a1 = p_bin_outcome_no_hte_a1,
    p_bin_outcome_no_hte = p_bin_outcome_no_hte,
    bin_outcome_no_hte = bin_outcome_no_hte
  ) |>
    dplyr::left_join(
      surv_dat,
      by = "id"
    ) |>
    dplyr::mutate(
      # 真の個体別効果は「治療あり − 治療なし」の向きに統一する。
      ite_rd_bin = p_bin_outcome_a1 - p_bin_outcome_a0,
      ite_log_or_bin = linear_predictor_outcome_a1 - linear_predictor_outcome_a0,
      ite_or_bin = exp(-treatment_effect)
    )

  # 学習用には観測項目を渡し、潜在アウトカムや真の効果は full_toy_data に残す。
  toy_data <- full_toy_data |>
    dplyr::select(
      id, age, sexm1, bmi, hf, bnp, lvef, palpitation, ca,
      bin_outcome, bin_event_free
    )

  list(
    toy_data = toy_data,
    full_toy_data = full_toy_data
  )
}
