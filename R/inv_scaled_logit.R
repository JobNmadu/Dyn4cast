
#' Exponential Values  after Integer Forecasting
#'
#' @param x The forecast values from constrained forecast package. Please specify the appropriate column containing the forecast values.
#' @param lower Lower limits of the forecast values
#' @param upper Upper limits of the forecast values
#'
#' @export inv_scaled_logit
#'
#' @examples
#' x <- 0:35
#' lower <- 0
#' upper <- 35
#' inv_scaled_logit(x = x, lower = lower, upper = upper)

inv_scaled_logit <- function(x, lower, upper) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

