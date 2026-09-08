## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "../man/figures/README-"
  )

library(dplyr)
library(evalITR)

load("../data/star.rda")

# specifying the outcome
outcomes <- "g3tlangss"

# specifying the data (remove other outcomes)
star_data <- star %>% 
  dplyr::select(-c(g3treadss,g3tmathss)) %>%
  mutate(SCHLURBN = as.numeric(SCHLURBN)) %>%
  rename(T = treatment)

star_data = star_data %>% mutate(
  cov1 = GKWHITE,
  cov2 = GKBUSED,
  cov3 = GKFRLNCH,
  school_urban = SCHLURBN
)

# specifying the formula
user_formula <- as.formula(
  "g3tlangss ~ T + gender + race + birthmonth + 
  birthyear + SCHLURBN + GRDRANGE + GKENRMNT + cov3 + 
  cov2 + cov1 ")


## ----compare_itr_summary, warning = FALSE, message = FALSE--------------------

# estimate ITR 
fit <- estimate_itr(
  treatment = "T",
  form = user_formula,
  data = star_data,
  algorithms = c("causal_forest"),
  budget = 0.2,
  split_ratio = 0.7)

# user's own ITR
score_function <- function(data){
  data %>% 
    mutate(score = case_when(
      school_urban == 1 ~ 0.1, # inner-city
      school_urban == 2 ~ 0.2, # suburban
      school_urban == 3 ~ 0.4, # rural
      school_urban == 4 ~ 0.3, # urban
    )) %>%
    pull(score) -> score
    
  return(score)
}

# evalutate ITR
compare_itr <- evaluate_itr(
  fit = fit,
  user_itr = score_function,
  data = star_data,
  treatment = "T",
  outcome = outcomes,
  budget = 0.2)

# summarize estimates
summary(compare_itr)

## ----compare_itr_aupec, fig.width = 6, fig.height = 4-------------------------
# plot the AUPEC 
plot(compare_itr)

## ----compare_itr_model, warning = FALSE, message = FALSE----------------------
# user-defined model
user_model <- function(training_data, test_data){

  # model fit on training data
  fit <- train_model(training_data)
  
  # estimate CATE on test data
  compute_hatf <- function(fit, test_data){

    score <- fit_predict(fit, test_data)  
    itr   <- score_function(score)
    
    return(list(itr = itr, score = score))
  }

  hatf <- compute_hatf(fit, test_data)
  
  return(list(
    itr = hatf$itr, 
    fit = fit, 
    score = hatf$score))
}

## ----compare_itr_model_train, warning = FALSE, message = FALSE----------------
# train model
train_model <- function(data){
  fit <- lm(
    Y ~ T*(cov1 + cov1 + cov3), 
    data = data)
  return(fit)
}

# predict function
fit_predict <- function(fit, data){
  # need to change this function if 
  # the model does not have a default predict function
  score <- predict(fit, data) 
  return(score)
}

## ----compare_itr_model_score, warning = FALSE, message = FALSE----------------
# score function
score_function <- function(score){
  itr <- (score >= 0) * 1
  return(itr)
}

## ----compare_itr_model_summary, warning = FALSE, message = FALSE--------------
# estimate ITR
compare_fit <- estimate_itr(
  treatment = "T",
  form = user_formula,
  data = star_data,
  algorithms = c("causal_forest"),
  budget = 0.2,
  split_ratio = 0.7,
  user_model = "user_model")


# evaluate ITR 
compare_est <- evaluate_itr(compare_fit)

# summarize estimates
summary(compare_est)
plot(compare_est)

