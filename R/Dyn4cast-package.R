#' @keywords internal
"_PACKAGE"

# The following block is used by usethis to automatically manage
# roxygen namespace tags. Modify with care!
## usethis namespace: start
## usethis namespace: end
NULL
#'Dynamic Forecast of five models and thier Essembles
#'
#' This is a package to dynamically estimate and forecast a model.
#'
#' @format The package presently covers five models and their Ensembles:
#' \describe{
#'   \item{Spline without knots}{The estimated spline model without the breaks (knots)}
#'   \item{Spline with knots}{The estimated spline model without the breaks (knots)}
#'   \item{Smooth Spline}{The smooth spline estimates}
#'   \item{ARIMA}{Estimated Auto Regressive Integrated Moving Average model}
#'   \item{Quadratic}{The estimated quadratic polynomial model}
#'   \item{Ensembled with equal weight}{Estimated Essemble model with equal weight given to each of the models. To get this, the fitted values of each of the models is divided by the number of models and summed together}
#'   \item{Ensembled based on weight}{Estimated Essemble model based on weight of each model. To do this, the fitted values of each model is multiplied and regressed agaisnt the trend}
#'   \item{Ensembled based on weight of fit}{Estimated Essemble model. The fit of each model is measured by the rmse}
#' }
#'
