#' Exponential Values after One-Sided Response Integer Variable Forecasting
#'
#' @description
#' This function is used to estimate exponential lower (80% and 95%) and upper
#'  (80% and 95%) values from the outcome of the `scaledlogit` function.
#'  The exponentiation ensures that the forecast does not go beyond the upper
#'  and lower limits of the base data.
#'
#' @param x `r lifecycle::badge("deprecated")`.
#' @param x3 The forecast values from constrained forecast package. Please
#' specify the appropriate column containing the forecast values.
#' @param lower Lower limits of the forecast values
#' @param upper Upper limits of the forecast values
#'
#' @export invscaledlogit
#' @name invscaledlogit
#'
#' @examples
#' x3 <- 1:35
#' lower <- 1
#' upper <- 35
#' invscaledlogit(x3 = x3, lower = lower, upper = upper)
invscaledlogit <- function(x, x3, lower, upper) {
  (upper - lower) * exp(x3) / (1 + exp(x3)) + lower
}
