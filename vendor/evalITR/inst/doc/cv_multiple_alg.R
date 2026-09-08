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

## ----multiple, message=TRUE, warning=TRUE-------------------------------------
library(evalITR)

# specify the trainControl method
fitControl <- caret::trainControl(
                           method = "repeatedcv",
                           number = 2,
                           repeats = 2)
# estimate ITR
set.seed(2021)
fit_cv <- estimate_itr(
               treatment = "treatment",
               form = user_formula,
               data = star_data,
               trControl = fitControl,
               algorithms = c(
                  "causal_forest", 
                  # "bartc",
                  # "rlasso", # from rlearner 
                  # "ulasso", # from rlearner 
                  "lasso" # from caret package
                  # "rf" # from caret package
                  ), # from caret package
               budget = 0.2,
               n_folds = 2)

# evaluate ITR
est_cv <- evaluate_itr(fit_cv)

# summarize estimates
summary(est_cv)

## ----multiple_plot, fig.width=8, fig.height=6,fig.align = "center"------------
# plot the AUPEC with different ML algorithms
plot(est_cv)

