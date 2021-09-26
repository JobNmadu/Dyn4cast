
#' Exponential Values  after Integer Forecasting
#'
#' @param x The forecast values from constrained forecast package. Please specify the appropriate column (x[ , 1]) containing the forecast values.
#' @param lower Lower limits of the forecast values
#' @param upper Upper limits of the forecast values
#'
#' @return
#' @export inv_scaled_logit
#'
#' @examples
# inv_scaled_logit(x = x, lower = lower, upper = upper)
inv_scaled_logit <- function(x, lower, upper) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}

