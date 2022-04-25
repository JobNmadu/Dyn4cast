#' Computation of MallowsCp
#'
#' This function is one of the numerous metrics used to assess and compare linear based models. The measure gives an approximate number of explanatory variables that should be in the model.
#'
#' @param Model The estimated **model** from which the Mallows Cp would be computed
#' @param y The vector of the **LHS** variable of the estimated model
#' @param x The matrix of the **RHS** variable of the estimated model. Note that if the model adds additional factor variables into the output, then the number of additional factors `Nlevels` is required otherwise the computed Cp would be biased.
#' @param Nlevels Optional number of additional variables created if the model has categorical variables that generates additional dummy variables during estimation
#' @param type The type of model for which Cp would be computed broadly divided in to linear and non-linear. If type is non-linear, specify the name of the model. Supported models include `linear`, `smooth.spline`, `ARIMA`, `ALM`, `plm` and `GLM` for binary based models
#'
#' @export MallowsCp
#' @name MallowsCp
#' @docType package
#'
#' @return A list with the following components
#' \item{\code{MallowsCp}}{of the Model.}
#'
#' @examples
#' ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
#' trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
#' x <- gl(2, 10, 20, labels = c("Ctl","Trt"))
#' y <- c(ctl, trt)
#' Model <- lm(y ~ x)
#' MallowsCp(Model = Model, y = y, x = x, type, Nlevels = NULL)
MallowsCp <- function(Model, y, x, type, Nlevels = NULL){
  size <- length(y)
  if (is.null(ncol(x))){
    nvars <- 1
  }else{
    nvars <- ncol(x)
  }
  DFF <- size - nvars + Nlevels - 1
 if(type == "linear"){
   RSSp <- sum(Model[["residuals"]]^2)
   MSEp <- RSSp/DFF
 }else if (type == "smooth.spline"){
   RSSp <- sum(y - fitted.values(fit11))^2
   MSEp <- RSSp/DFF
 }else if (type == "ALM" | type == "ARIMA" | type == "plm"){
   RSSp <- sum(Model[["residuals"]]^2)
   MSEp <- RSSp/DFF
 }else{
   RSSp <- sum(Model[["residuals"]]^2)
   MSEp <- RSSp/DFF
 }
  Cp <- RSSp/MSEp-size+2*(nvars+Nlevels+1)
  results <- list("MallowsCp" = Cp)
  return(results)
}
