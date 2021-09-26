
#' Scale Parameter for Integer Modelling and Forecast
#'
#' @param x The parameter to be scaled, which should normally be the fitted values from supported models. The scaled parameter is used mainly for constrained forecasting of a response variable >> positive {0 - inf} or << negative {-inf - 0}). The scaling also involves log transformation of the parameter
#' @param lower Integer or variable representing the lower limits for the scaling (-inf or 0)
#' @param upper Integer or variable representing the upper limits for the scaling (0 or inf)
#'
#' @return
#' @export scaled_logit
#'
#' @examples
#' X <- 0:35
#' lower <- 0
#' upper <- 35
#' scaled_logit(x = X, lower = lower, upper = upper)
#'
scaled_logit <- function(x, lower, upper) {
  log((x - lower) / (upper - x))
}
