## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "../man/figures/README-"
  )

library(dplyr)
load("../data/star.rda")

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

## ----parallel, message = FALSE, eval = FALSE----------------------------------
# # parallel computing
# library(doParallel)
# cl <- makePSOCKcluster(2)
# registerDoParallel(cl)
# 
# # stop after finishing the computation
# stopCluster(cl)

## ----caret estimate, message = FALSE------------------------------------------
library(evalITR)
library(caret)

# specify the trainControl method
fitControl <- caret::trainControl(
  method = "repeatedcv", # 3-fold CV
  number = 3, # repeated 3 times
  repeats = 3,
  search='grid',
  allowParallel = TRUE) # grid search

# specify the tuning grid
gbmGrid <- expand.grid(
  interaction.depth = c(5,9), 
  n.trees = (5:10)*100, 
  shrinkage = 0.1,
  n.minobsinnode = 20)

# estimate ITR
fit_caret <- estimate_itr(
  treatment = "treatment",
  form = user_formula,
  trControl = fitControl,
  data = star_data,
  algorithms = c("gbm"),
  budget = 0.2,
  split_ratio = 0.7,
  tuneGrid = gbmGrid,
  verbose = FALSE)

# evaluate ITR
est_caret <- evaluate_itr(fit_caret)


## ----caret_model, message = FALSE, warning = FALSE, fig.width = 6, fig.height = 4----
# extract the final model
caret_model <- fit_caret$estimates$models$gbm
print(caret_model$finalModel)

# check model performance
trellis.par.set(caretTheme()) # theme
plot(caret_model) 
# heatmap 
plot(
  caret_model, 
  plotType = "level",
  scales = list(x = list(rot = 90)))

## ----sl_summary, message = FALSE, warning = FALSE-----------------------------
library(SuperLearner)

fit_sl <- estimate_itr(
  treatment = "treatment",
  form = user_formula,
  data = star_data,
  algorithms = c("causal_forest","SuperLearner"),
  budget = 0.2,
  split_ratio = 0.7,
  SL_library = c("SL.ranger", "SL.glmnet"))

est_sl <- evaluate_itr(fit_sl)

# summarize estimates
summary(est_sl)

## ----sl_plot, fig.width=6, fig.height=4,fig.align = "center"------------------
# plot the AUPEC 
plot(est_sl)

