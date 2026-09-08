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

# specifying the treatment
treatment <- "treatment"

# specifying the data (remove other outcomes)
star_data <- star %>% dplyr::select(-c(g3treadss,g3tmathss))

star_data = star_data %>% mutate(
  school_urban = SCHLURBN
)

# specifying the formula
user_formula <- as.formula(
  "g3tlangss ~ treatment + gender + race + birthmonth + 
  birthyear + SCHLURBN + GRDRANGE + GKENRMNT + GKFRLNCH + 
  GKBUSED + GKWHITE ")


## ----user_itr_summary, warning = FALSE, message = FALSE-----------------------
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
user_itr <- evaluate_itr(
  user_itr = score_function,
  data = star_data,
  treatment = treatment,
  outcome = outcomes,
  budget = 0.2)

# summarize estimates
summary(user_itr)

## ----user_itr_gate, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4----
# plot GATE estimates
library(ggplot2)
gate_est <- summary(user_itr)$GATE

plot_estimate(gate_est, type = "GATE") +
  scale_color_manual(values = c("#0072B2", "#D55E00"))

## ----user_itr_aupec, fig.width = 6, fig.height = 4----------------------------
# plot the AUPEC 
plot(user_itr)

