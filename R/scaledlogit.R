#' Scale Parameter for Integer Modelling and Forecast
#'
#' This function is a wrapper for scaling the fitted (predicted) values of a one-sided (positive or negative only) integer response variable of supported models. The scaling involves some log transformation of the fitted (predicted) values.
#'
#' @param x The parameter to be scaled, which is the fitted values from supported models. The scaled parameter is used mainly for constrained forecasting of a response variable >> positive {0 - inf} or << negative {-inf - 0}). The scaling involves log transformation of the parameter
#' @param lower Integer or variable representing the lower limit for the scaling (-inf or 0)
#' @param upper Integer or variable representing the upper limit for the scaling (0 or inf)
#'
#' @export scaledlogit
#' @return
#' @name scaledlogit
#'
#' @examples
#' library(Dyn4cast)
#' library(splines)
#' lower <- 1
#' upper <- 37
#' Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
#' scaledlogit(x = fitted.values(Model), lower = lower,
#'  upper = upper)
scaledlogit <- function(x, lower, upper) {
  log((x - lower) / (upper - x))
}
