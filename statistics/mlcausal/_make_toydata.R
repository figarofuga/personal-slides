
# ============================================================
# Toy data generation according to DAG
# with BNP, nonlinear BMI effect, and heterogeneous treatment effect
# ============================================================

# Required packages for DAG plot:
# install.packages(c("dagitty", "ggdag", "ggplot2"))
library(tinyplot)
library(dplyr)
library(simsurv)

set.seed(123)

n <- 6000

# ------------------------------------------------------------
# Helper functions
# ------------------------------------------------------------

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

rtrunclnorm_simple <- function(n, meanlog, sdlog, lower, upper) {
  out <- numeric(0)

  while (length(out) < n) {
    x <- rlnorm(n, meanlog = meanlog, sdlog = sdlog)
    x <- x[x >= lower & x <= upper]
    out <- c(out, x)
  }

  out[seq_len(n)]
}

# ------------------------------------------------------------
# 1. Baseline variables
# ------------------------------------------------------------

# Sex: 0/1, P(Sex = 0) = 0.40, P(Sex = 1) = 0.60
sex <- rbinom(n, size = 1, prob = 0.60)

# Age: Normal(mean 60 for male, mean 65 for female, sd 8), truncated at 20 and 95


age <- ifelse(
  sex == 0,
  rtruncnorm_simple(
    n = sum(sex == 0),
    mean = 65,
    sd = 8,
    lower = 20,
    upper = 99
  ),
  rtruncnorm_simple(
    n = sum(sex == 1),
    mean = 60,
    sd = 8,
    lower = 20,
    upper = 95
  )
) |> as.integer()


# BMI: lognormal with expected value approximately 25 before truncation
# truncated at 10 and 35
sdlog_bmi <- 0.20
meanlog_bmi <- log(22) - sdlog_bmi^2 / 2

bmi <- rtrunclnorm_simple(
  n = n,
  meanlog = meanlog_bmi,
  sdlog = sdlog_bmi,
  lower = 12,
  upper = 40
)

# LVEF: normal with expected value for 55%

lvef <- rtruncnorm_simple(
  n = n,
  mean = 52,
  sd = 8,
  lower = 15,
  upper = 75
) |> as.integer()




# Standardized variables for model coefficients
age_z <- as.numeric(scale(age))
bmi_z <- as.numeric(scale(bmi))
lvef_z <- as.numeric(scale(lvef))

p_SES <- 0.625

SES <- rbinom(n, size = 4, prob = p_SES)

SES_z <- as.numeric(scale(SES))

# ------------------------------------------------------------
# 2. Comorbidities
#    Age, Sex, SES -> HF, HTN, Stroke, SES
# ------------------------------------------------------------

p_hf <- inv_logit(
  -1.5 +
    0.95 * age_z +
    0.15 * sex -
    0.45 * bmi_z -
    0.3 * lvef_z +
    0.2 * SES_z
)

hf <- rbinom(n, size = 1, prob = p_hf)


# ------------------------------------------------------------
# 3. BNP
#    Lognormal, lower bound 0, upper bound 2000,
#    expected value approximately 150 before truncation.
#
#    BNP is affected by:
#    Age, Sex, BMI, SES, HF, HTN, Stroke, SES
# ------------------------------------------------------------

sdlog_bnp <- 0.90
meanlog_bnp <- log(150) - sdlog_bnp^2 / 2

bnp_base <- rtrunclnorm_simple(
  n = n,
  meanlog = meanlog_bnp,
  sdlog = sdlog_bnp,
  lower = 0,
  upper = 2000
)

# Disease-related multiplicative structure for BNP
bnp <- bnp_base *
  exp(
    0.3 * age_z +
      0.10 * sex -
      0.15 * bmi_z -
      0.4 * lvef_z
  )

# Truncate again at 0 and 2000
bnp <- pmin(pmax(bnp, 0), 2000) |>
  as.integer()

bnp_z <- as.numeric(scale(log1p(bnp)))

# ------------------------------------------------------------
# 4. Palpitation
#    Age, Sex, HF, SES, SES -> Palpitation
# ------------------------------------------------------------

p_palpitation <- inv_logit(
  -1.0 +
    0.10 * age_z +
    0.35 * sex -
    0.40 * SES_z
)

palpitation <- rbinom(n, size = 1, prob = p_palpitation)

# ------------------------------------------------------------
# 5. Treatment / intervention
#    CA is more likely in lower-risk patients:
#    younger age, male sex, higher BMI, higher SES, and lower BNP.
#    Palpitation, lower LVEF, and HF also increase CA use.
# ------------------------------------------------------------

p_ca <- inv_logit(
    -1.00 +
    -1.05 * age_z +
    0.60 * sex +
    0.35 * bmi_z +
    0.75 * SES_z +
    -0.85 * bnp_z +
    1.70 * palpitation -
    0.70 * lvef_z +
    1.25 * hf
)

ca <- rbinom(n, size = 1, prob = p_ca)

# ------------------------------------------------------------
# 6. Nonlinear BMI prognostic effect
#
#    BMI 25-30: lowest risk
#    BMI < 25: lower BMI increases risk strongly
#    BMI > 30: higher BMI increases risk mildly
#
#    This creates an asymmetric convex / U-shaped risk function.
# ------------------------------------------------------------

bmi_risk <-
  pmax(25 - bmi, 0)^2 +
  0.3 * pmax(bmi - 30, 0)^2

lvef_risk <-
  pmax(40 - lvef, 0)^2 +
  0.3 * pmax(60 - lvef, 0)^2

bmi_risk_z <- as.numeric(scale(bmi_risk))
lvef_risk_z <- as.numeric(scale(lvef_risk))

age_risk <- pmax(age - 65, 0) / 10

bnp_risk <- log1p(bnp) / log1p(2000)

# ------------------------------------------------------------
# 7. Heterogeneous treatment effect
#
#    Higher risk patients have a larger treatment benefit:
#    older age, higher BNP, higher BMI risk, and higher LVEF risk.
#
#    Female patients have a smaller age-related increase in treatment
#    benefit and a larger BMI-risk-related increase.
#
#    The effect is coded on the log-odds scale.
#    More negative = stronger risk reduction by CA.
# ------------------------------------------------------------


treatment_effect <- ifelse(
  sex == 1,
  -0.20 - 0.45 * age_risk - 0.55 * bnp_risk - 0.20 * bmi_risk_z - 0.25 * lvef_risk_z,
  -0.20 - 0.25 * age_risk - 0.55 * bnp_risk - 0.45 * bmi_risk_z - 0.25 * lvef_risk_z
  )

# ------------------------------------------------------------
# 8. Outcome
#    HF + death composite outcome
#
#    Affected by everything except palpitation:
#    Age, Sex, BMI, SES, HF, HTN, Stroke, SES, BNP, CA -> Outcome
#
#    Palpitation does not directly affect Outcome.
# ------------------------------------------------------------

linear_predictor_outcome <-
  -2.0 +
  0.70 * age_risk +
  0.90 * hf +
  -0.85 * SES_z +
  0.55 * bnp_risk +
  0.04 * bmi_risk +
  0.1 * lvef_risk +
  ca * treatment_effect

p_bin_outcome <- inv_logit(linear_predictor_outcome)

bin_outcome <- rbinom(n, size = 1, prob = p_bin_outcome)

afeqt_mean <- (
  75 -
  4 * age_z -
  7.0 * sex -
  2.0 * bmi_z -
  8.0 * hf +
  3.0 * SES_z -
  5.0 * bnp_z +
  3.0 * lvef_z -
  10.0 * palpitation +
  6.0 * ca
)

afeqt_os <- rtruncnorm_simple(
  n = n,
  mean = afeqt_mean,
  sd = 12,
  lower = 0,
  upper = 100
)


# ------------------------------------------------------------
# 9. Final dataset
# ------------------------------------------------------------

pre_toy_data <- tibble::tibble(
  age = age,
  sex = sex,
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
  linear_predictor_outcome = linear_predictor_outcome,
  p_bin_outcome = p_bin_outcome,
  bin_outcome = bin_outcome,
  afeqt_os = afeqt_os
)  |>
dplyr::mutate(
  id = dplyr::row_number(),
  .before = age
  )

library(simsurv)

surv_dat <- simsurv::simsurv(
  dist = "weibull",
  lambdas = 0.08,
  gammas = 1.5,
  x = pre_toy_data,
  betas = c(linear_predictor_outcome = 1),
  maxt = 5
  )

full_toy_data <-
  dplyr::left_join(
  pre_toy_data,
  surv_dat,
  by = "id"
  )

toy_data <- dplyr::select(
  full_toy_data,
  id, age, sex, bmi, hf, bnp, lvef, palpitation, ca, bin_outcome, afeqt_os, eventtime, status
)
# ------------------------------------------------------------
# 10. Quick checks
# ------------------------------------------------------------
