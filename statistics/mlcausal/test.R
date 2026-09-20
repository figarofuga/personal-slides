library(tidyverse)
library(rsample)
library(grf)
library(MatchIt)

data("lalonde", package = "MatchIt")

lalonde <- model.matrix(~ ., data = lalonde)[, -1] |> 
  tibble::as_tibble() |>
  mutate(
    outcome = as.integer(re78 > mean(re78))
  )

set.seed(123)

split <- initial_split(
  lalonde,
  prop = 0.7,
  strata = treat
)

train <- training(split)

valid <- testing(split)

xvars <- c(
  "age",
  "educ",
  "racewhite",
  "racehispan",
  "married",
  "nodegree",
  "re74",
  "re75"
)

X_train <- train |>
  dplyr::select(all_of(xvars)) |>
  as.matrix()

Y_train <- train$outcome

W_train <- train$treat

set.seed(123)

cf <- causal_forest(
  X = X_train,
  Y = Y_train,
  W = W_train,
  num.trees = 2000,
  seed = 123
)

X_valid <- valid |>
  select(all_of(xvars)) |>
  as.matrix()

valid <- valid |>
  mutate(
    pred_benefit =
      predict(
        cf,
        newdata = X_valid
      )$predictions
  )

set.seed(123)

m <- matchit(
  treat ~ pred_benefit,
  data = valid,
  method = "nearest",
  # distance = valid$pred_benefit,
  ratio = 1,
  replace = FALSE
)

matched <- match_data(m) |>
  as_tibble()

matched |>
  select(
    subclass,
    treat,
    outcome,
    pred_benefit
  ) |>
  arrange(subclass)

pair_df <- matched |>
  group_by(subclass) |>
  summarise(

    outcome_treated =
      outcome[treat == 1],

    outcome_control =
      outcome[treat == 0],

    observed_benefit =
      outcome_treated -
      outcome_control,

    predicted_benefit =
      mean(pred_benefit),

    .groups = "drop"
  )

pair_comparison <- tidyr::crossing(
  pair_i = seq_len(nrow(pair_df)),
  pair_j = seq_len(nrow(pair_df))
) |>
  filter(
    pair_i < pair_j
  ) |>
  mutate(

    observed_i =
      pair_df$observed_benefit[pair_i],

    observed_j =
      pair_df$observed_benefit[pair_j],

    predicted_i =
      pair_df$predicted_benefit[pair_i],

    predicted_j =
      pair_df$predicted_benefit[pair_j]
  )

pair_comparison <- pair_comparison |>
  filter(
    observed_i != observed_j
  ) |>
  mutate(

    observed_diff =
      observed_i - observed_j,

    predicted_diff =
      predicted_i - predicted_j,

    concordance = case_when(

      observed_diff * predicted_diff > 0
        ~ 1,

      predicted_diff == 0
        ~ 0.5,

      TRUE
        ~ 0
    )
  )
