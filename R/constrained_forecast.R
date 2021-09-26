
#' Constrained Forecast of One-sided Integer Response Model
#'
#' @param Model This is the forecast of the scaled fitted values of supported models.
#' @param lower The lower limit of the forecast
#' @param upper The upper limit of the forecast
#'
#' @return A list of forecast values within 80% and 95% confidence band. The values are:
#' \item{\code{Lower 80%}}{Forecast at lower 80% confidence level.}
#' \item{\code{Upper 80%}}{Forecast at upper 80% confidence level.}
#' \item{\code{Lower 95%}}{Forecast at lower 95% confidence level.}
#' \item{\code{Upper 95%}}{Forecast at upper 95% confidence level.}
#'
#' @importFrom inv_scaled_logit inv_scaled_logit
#' @export constrained_forecast
#'
#' @examples
# constrained_forecast(Model = Model, lower, upper)

constrained_forecast <- function(Model, lower, upper) {
  F2 <- Model$upper
  F1 <- Model$lower
  F1l1 <- inv_scaled_logit(x = F1[, 1], lower = lower, upper = upper)
  F1l2 <- inv_scaled_logit(x = F1[, 2], lower = lower, upper = upper)
  F2u1 <- inv_scaled_logit(x = F2[, 1], lower = lower, upper = upper)
  F2u2 <- inv_scaled_logit(x = F2[, 2], lower = lower, upper = upper)

  results <- list("Lower80" = F1l1,
                  "Lower95" = F1l2,
                  "Upper80" = F2u1,
                  "Upper95" = F2u2)
  return(results)
}
