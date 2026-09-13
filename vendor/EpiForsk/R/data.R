#' Example Data for Husby's Forest Plot Vignette
#'
#' A data example for the construction of a multi faceted forest plot.
#'
#' @format ## `andh_forest_data`
#' A data frame with 18 rows and 12 columns:
#' \describe{
#'   \item{type}{text formattaing, bold/plain}
#'   \item{indent}{number of indents in final formatting}
#'   \item{text}{description text}
#'   \item{A_est}{point estimate in first figure column}
#'   \item{A_l}{lower limit of confidence interval in first figure column}
#'   \item{A_u}{upper limit of confidence interval in first figure column}
#'   \item{B_est}{point estimate in second figure column}
#'   \item{B_l}{lower limit of confidence interval in second figure column}
#'   \item{B_u}{upper limit of confidence interval in second figure column}
#'   \item{C_est}{point estimate in third figure column}
#'   \item{C_l}{lower limit of confidence interval in third figure column}
#'   \item{C_u}{upper limit of confidence interval in third figure column}
#' }
"andh_forest_data"



#' Simulated Time-Varying Residence Data
#'
#' A dataset of simulated time-varying residence and gender data.
#'
#' @format ## `andh_forest_data`
#' A data frame with 546 rows and 7 columns describing 100 people:
#' \describe{
#'   \item{id}{an id number}
#'   \item{dob}{date of birth}
#'   \item{region}{region of Denmark}
#'   \item{move_in}{date of moving to region}
#'   \item{move_out}{date of moving away from region}
#'   \item{gender}{gender of the person}
#'   \item{claim}{whether or not the person made a claim here}
#' }
"adls_timevarying_region_data"
