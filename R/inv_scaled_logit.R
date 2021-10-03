
#' Exponential Values after One-Sided Response Integer Variable Forecasting
#'
#' This package is used to estimate exponential lower (80% and 95%) and upper (80% and 95%) values from the outcome of the `scaled_logit` package. The exponentiation ensures that the forecast does not go beyond the upper and lower limits of the base data.
#'
#' @param x The forecast values from constrained forecast package. Please specify the appropriate column containing the forecast values.
#' @param lower Lower limits of the forecast values
#' @param upper Upper limits of the forecast values
#'
#' @export inv_scaled_logit
#'
#' @examples
#' x <- 1:35
#' lower <- 1
#' upper <- 35
#' inv_scaled_logit(x = x, lower = lower, upper = upper)

inv_scaled_logit <- function(x, lower, upper) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

