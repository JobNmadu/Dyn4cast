
#' Computation of Mallows_Cp
#'
#' `Mallows_Cp()` This function is one of the numerous metrics used to assess and compare linear-based models. The measure gives an approximate number of explanatory variables that should be in the model.
#'
#' @param Model The estimated **model** from which the Mallow's Cp would be computed
#' @param y The vector of the **LHS** variable of the estimated model
#' @param x The matrix of the **RHS** variable of the estimated model
#'
#' @return
#'
#' @export Mallows_Cp
#'
#' @importFrom stats anova
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' x <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' y <- c(ctl, trt)
#' Model <- lm(y ~ x)
#' Mallows_Cp(Model = Model, y = y, x = x)
Mallows_Cp <- function(Model, y, x) {
  Anova <- anova(Model)
  AAAV <- Anova["Residuals", ]
  RSSp <- AAAV$`Sum Sq`
  MSEp <- AAAV$`Mean Sq`
  size <- length(y)
  if (is.null(ncol(x))) {
    nvars <- 1
  } else {
    nvars <- ncol(x)
  }
  Cp <- RSSp/MSEp-size+2*(nvars+1)

  results <- list("Mallow's Cp" = Cp)
  return(results)
}
