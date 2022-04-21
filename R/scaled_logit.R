
#' Scale Parameter for Integer Modelling and Forecast
#'
#' `scaled_logit()` This function is a wrapper for scaling the fitted (predicted) values of a one-sided (positive or negative only) integer response variable of supported models. The scaling involves some log transformation of the fitted (predicted) values.
#'
#' @param x The parameter to be scaled, which is the fitted values from supported models. The scaled parameter is used mainly for constrained forecasting of a response variable >> positive {0 - inf} or << negative {-inf - 0}). The scaling involves log transformation of the parameter
#' @param lower Integer or variable representing the lower limit for the scaling (-inf or 0)
#' @param upper Integer or variable representing the upper limit for the scaling (0 or inf)
#'
#' @export scaled_logit
#'
#' @examples
#' library(splines)
#' lower <- 1
#' upper <- 37
#' Model   <- lm(states ~ bs(sequence, knots = c(30, 115)),
#' data = StatesAffected)
#' scaled_logit(x = fitted.values(Model), lower = lower,
#'  upper = upper)
#'
scaled_logit <- function(x, lower, upper) {
  log((x - lower) / (upper - x))
}
