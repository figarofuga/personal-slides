## ----include = FALSE, messsage = FALSE, warning = FALSE-----------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

options(rmarkdown.html_vignette.check_title = FALSE)  


## ----messsage = FALSE, warning = FALSE, eval=FALSE----------------------------
# # Install release version from CRAN (updating evalITR is the same command)
# install.packages("evalITR")

## ----messsage = FALSE, warning = FALSE, eval = FALSE--------------------------
# # install.packages("devtools")
# devtools::install_github("MichaelLLi/evalITR", ref = "causal-ml")

## ----messsage = FALSE, warning = FALSE, eval=FALSE----------------------------
# library(furrr)
# library(future.apply)
# 
# # check the number of cores
# parallel::detectCores()
# 
# # set the number of cores
# nworkers <- 4
# plan(multisession, workers =nworkers)

