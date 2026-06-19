#' Deprecated functions in Dyn4cast
#'
#' These functions have been removed (defunct) as indicated against their names.
#'
#' @param ... Not used.
#'
#' @returns None, function now defunct.
#' @name Dyn4cast-deprecated
#'
#' @examples
#' # Non
NULL
#' @export
#' @rdname Dyn4cast-deprecated
Model_factors <- function(...) {
  lifecycle::deprecate_warn(
    "11.11.26",
    "Model_factors()",
    "model_factors()",
    "Please use `model_factors()` instead"
  )
}

mdpi <- function(...) {
  lifecycle::deprecate_warn(
    "11.11.28",
    "mdpi()",
    "mdi()",
    "Please use `mdi()` instead"
  )
}

plot_mdpi <- function(...) {
  lifecycle::deprecate_warn(
    "11.11.28",
    "plot_mdpi()",
    "plot_mdi()",
    "Please use `plot_mdi()` instead"
  )
}
