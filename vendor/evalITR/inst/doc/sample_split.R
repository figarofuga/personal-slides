## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "../man/figures/README-"
  )
  
load("../data/star.rda")


## ----sample_split, warning = FALSE, message = FALSE---------------------------
library(dplyr)
library(evalITR)

# specifying the outcome
outcomes <- "g3tlangss"

# specifying the treatment
treatment <- "treatment"

# specifying the data (remove other outcomes)
star_data <- star %>% dplyr::select(-c(g3treadss,g3tmathss))

# specifying the formula
user_formula <- as.formula(
  "g3tlangss ~ treatment + gender + race + birthmonth + 
  birthyear + SCHLURBN + GRDRANGE + GKENRMNT + GKFRLNCH + 
  GKBUSED + GKWHITE ")


# estimate ITR 
fit <- estimate_itr(
  treatment = treatment,
  form = user_formula,
  data = star_data,
  algorithms = c("causal_forest"),
  budget = 0.2,
  split_ratio = 0.7)


# evaluate ITR 
est <- evaluate_itr(fit)

## ----sp_summary---------------------------------------------------------------
# summarize estimates
summary(est)

## ----est_extract, warning = FALSE, message = FALSE, fig.width = 6, fig.height = 4----
# plot GATE estimates
library(ggplot2)
gate_est <- summary(est)$GATE

plot_estimate(gate_est, type = "GATE") +
  scale_color_manual(values = c("#0072B2", "#D55E00"))

## ----sp_plot, fig.width = 6, fig.height = 4-----------------------------------
# plot the AUPEC 
plot(est)

