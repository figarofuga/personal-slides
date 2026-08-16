make_toy_data <- function(n = 3000L, seed = 123L) {
  set.seed(seed)

  inv_logit <- function(x) {
    1 / (1 + exp(-x))
  }

  rtruncnorm_simple <- function(n, mean, sd, lower, upper) {
    out <- numeric(0)

    while (length(out) < n) {
      x <- rnorm(n, mean = mean, sd = sd)
      x <- x[x >= lower & x <= upper]
      out <- c(out, x)
    }

    out[seq_len(n)]
  }

  rtruncnorm_vec <- function(mean, sd, lower, upper) {
    out <- rnorm(length(mean), mean = mean, sd = sd)
    bad <- out < lower | out > upper

    while (any(bad)) {
      out[bad] <- rnorm(sum(bad), mean = mean[bad], sd = sd)
      bad <- out < lower | out > upper
    }

    out
  }

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
  sexm1 <- rbinom(n, size = 1, prob = 0.60)

  age <- ifelse(
    sexm1 == 0,
    rtruncnorm_simple(
      n = sum(sexm1 == 0),
      mean = 65,
      sd = 8,
      lower = 20,
      upper = 99
    ),
    rtruncnorm_simple(
      n = sum(sexm1 == 1),
      mean = 60,
      sd = 8,
      lower = 20,
      upper = 95
    )
  ) |>
    as.integer()

  sdlog_bmi <- 0.20
  meanlog_bmi <- log(22) - sdlog_bmi^2 / 2

  bmi <- rtrunclnorm_simple(
    n = n,
    meanlog = meanlog_bmi,
    sdlog = sdlog_bmi,
    lower = 12,
    upper = 40
  )

  lvef <- rtruncnorm_simple(
    n = n,
    mean = 52,
    sd = 8,
    lower = 15,
    upper = 75
  ) |>
    as.integer()

  age_z <- as.numeric(scale(age))
  bmi_z <- as.numeric(scale(bmi))
  lvef_z <- as.numeric(scale(lvef))

  p_SES <- 0.625
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
  meanlog_bnp <- log(150) - sdlog_bnp^2 / 2

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
  p_ca <- inv_logit(
    -1.00 -
      1.05 * age_z +
      0.60 * sexm1 +
      0.35 * bmi_z +
      0.75 * SES_z -
      0.85 * bnp_z +
      1.70 * palpitation -
      0.70 * lvef_z +
      1.25 * hf
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

  # Heterogeneous treatment effect
  treatment_effect <- ifelse(
    sexm1 == 1,
    -0.20 -
      0.45 * age_risk -
      0.55 * bnp_risk -
      0.20 * bmi_risk_z -
      0.25 * lvef_risk_z,
    -0.20 -
      0.25 * age_risk -
      0.55 * bnp_risk -
      0.45 * bmi_risk_z -
      0.25 * lvef_risk_z
  ) * (-1)

  # Outcomes
  linear_predictor_outcome_a0 <-
    -2.0 +
    0.70 * age_risk +
    0.90 * hf -
    0.85 * SES_z +
    0.55 * bnp_risk +
    0.04 * bmi_risk +
    0.1 * lvef_risk

  linear_predictor_outcome_a1 <-
    linear_predictor_outcome_a0 - treatment_effect

  linear_predictor_outcome <- ifelse(
    ca == 1,
    linear_predictor_outcome_a1,
    linear_predictor_outcome_a0
  )

  p_bin_outcome_a0 <- inv_logit(linear_predictor_outcome_a0)
  p_bin_outcome_a1 <- inv_logit(linear_predictor_outcome_a1)
  p_bin_outcome <- inv_logit(linear_predictor_outcome)
  bin_outcome <- rbinom(n, size = 1, prob = p_bin_outcome)

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
  afeqt_mean <- ifelse(ca == 1, afeqt_mean_a1, afeqt_mean_a0)

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

  full_toy_data <- dplyr::left_join(
    pre_toy_data,
    surv_dat,
    by = "id"
  )

  toy_data <- dplyr::select(
    full_toy_data,
    id,
    age,
    sexm1,
    bmi,
    hf,
    bnp,
    lvef,
    palpitation,
    ca,
    bin_outcome,
    afeqt_treatment_effect,
    afeqt_os,
    eventtime,
    status
  )

  list(
    toy_data = toy_data,
    full_toy_data = full_toy_data
  )
}
