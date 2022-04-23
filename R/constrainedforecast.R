#' Constrained Forecast of One-sided Integer Response Model
#'
#' This function estimates the lower and upper 80% and 95% forecasts of the Model. The final values are within the lower and upper limits of the base data. Used in conjunction with <scaled_logit> and <inv_scaled_logit> functions, they are adapted from Hyndman & Athanasopoulos (2021) and modified for independent use rather than be restricted to be used with a particular package.
#'
#' @param Model This is the exponential values from the `invscaledlogit` function.
#' @param lower The lower limit of the forecast
#' @param upper The upper limit of the forecast
#'
#' @return A list of forecast values within 80% and 95% confidence band. The values are:
#' \item{\code{Lower 80%}}{Forecast at lower 80% confidence level.}
#' \item{\code{Upper 80%}}{Forecast at upper 80% confidence level.}
#' \item{\code{Lower 95%}}{Forecast at lower 95% confidence level.}
#' \item{\code{Upper 95%}}{Forecast at upper 95% confidence level.}
#'
#' @export constrainedforecast
#'
#' @name constrainedforecast
#'
#' @docType package
#'
#' @aliases StatesAffected
#'
#' @return A list with the following components:
#' \item{\code{Lower80}}{The lower 80% confindence band of the forecast.}
#' \item{\code{Lower95}}{The lower 95% confindence band of the forecast..}
#' \item{\code{Upper80}}{The upper 80% confindence band of the forecast.}
#' \item{\code{upper95}}{The upper 95% confindence band of the forecast..}
#'
#' @examples
#' library(splines)
#' library(forecast)
#' lower <- 1
#' upper <- 37
#' Model   <- lm(states ~ bs(sequence, knots = c(30, 115)),
#' data = StatesAffected)
#' FitModel <- scaledlogit(x = fitted.values(Model), lower = lower,
#'  upper = upper)
#' ForecastModel <- forecast(FitModel, h = length(200))
#' ForecastValues <- constrainedforecast(Model = ForecastModel, lower, upper)

#' @description
#' Obtain the constrained forecast of the model
constrainedforecast <- function(Model, lower, upper) {
  F2 <- Model$upper
  F1 <- Model$lower
  F1l1 <- Dyn4cast::invscaledlogit(x = F1[, 1], lower = lower, upper = upper)
  F1l2 <- Dyn4cast::invscaledlogit(x = F1[, 2], lower = lower, upper = upper)
  F2u1 <- Dyn4cast::invscaledlogit(x = F2[, 1], lower = lower, upper = upper)
  F2u2 <- Dyn4cast::invscaledlogit(x = F2[, 2], lower = lower, upper = upper)

  results <- list("Lower80" = F1l1,
                  "Lower95" = F1l2,
                  "Upper80" = F2u1,
                  "Upper95" = F2u2)
  return(results)
}
