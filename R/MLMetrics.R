#' Collection of Machine Learning Model Metrics for Easy Reference
#'
#' @description
#' This function estimates over 40 Metrics for assessing the quality of Machine
#'  Learning Models. The purpose is to provide a wrapper which brings all the
#'  metrics on the table and makes it easier to use them to select a model.
#'
#' @usage MLMetrics(Observed, yvalue, modeli, K, Name, Form, kutuf, TTy)
#'
#' @param Observed The Observed data in a data frame format
#' @param yvalue The Response variable of the estimated Model
#' @param modeli The Estimated Model (*Model* = a + bx)
#' @param K The number of variables in the estimated Model to consider
#' @param Name The Name of the Models that need to be specified. They are ARIMA,
#'  Values if the model computes the fitted value without estimation like
#'  Essembles, SMOOTH (smooth.spline), Logit, Ensembles based on weight -
#'  EssemWet, QUADRATIC polynomial, SPLINE polynomial.
#' @param Form Form of the Model Estimated (LM, ALM, GLM, N-LM, ARDL)
#' @param kutuf Cutoff for the Estimated values (defaults to 0.5 if not specified)
#' @param TTy Type of response variable (Numeric or Response - like *binary*)
#'
#' @export MLMetrics
#' @name MLMetrics
#'
#' @importFrom stats AIC
#' @importFrom generics augment
#' @importFrom stats BIC
#' @importFrom stats na.omit
#' @importFrom Metrics accuracy
#' @importFrom Metrics ae
#' @importFrom Metrics ape
#' @importFrom Metrics apk
#' @importFrom Metrics bias
#' @importFrom Metrics f1
#' @importFrom Metrics ll
#' @importFrom Metrics mape
#' @importFrom Metrics mapk
#' @importFrom Metrics mase
#' @importFrom Metrics mdae
#' @importFrom Metrics msle
#' @importFrom Metrics percent_bias
#' @importFrom Metrics rae
#' @importFrom Metrics rmsle
#' @importFrom Metrics rrse
#' @importFrom Metrics rse
#' @importFrom Metrics se
#' @importFrom Metrics sle
#' @importFrom Metrics smape
#' @importFrom Metrics sse
#' @importFrom ModelMetrics auc
#' @importFrom ModelMetrics ce
#' @importFrom ModelMetrics f1Score
#' @importFrom ModelMetrics logLoss
#' @importFrom ModelMetrics mae
#' @importFrom ModelMetrics mse
#' @importFrom ModelMetrics precision
#' @importFrom ModelMetrics recall
#' @importFrom ModelMetrics rmse
#' @importFrom ModelMetrics brier
#' @importFrom ModelMetrics gini
#' @importFrom ModelMetrics kappa
#' @importFrom ModelMetrics sensitivity
#' @importFrom ModelMetrics specificity
#' @importFrom ModelMetrics fScore
#' @importFrom ModelMetrics mcc
#' @importFrom ModelMetrics tnr
#' @importFrom ModelMetrics tpr
#' @importFrom ModelMetrics ppv
#' @importFrom ModelMetrics npv
#' @importFrom stats predict
#' @importFrom stats fitted
#' @importFrom stats residuals
#' @importFrom stats update
#' @importFrom utils flush.console
#'
#' @return A list with the following components:
#' \item{\code{Absolute Error}}{of the Model.}
#' \item{\code{Absolute Percent Error}}{of the Model.}
#' \item{\code{Accuracy}}{of the Model.}
#' \item{\code{Adjusted R Square}}{of the Model.}
#' \item{\code{`Akaike's` Information Criterion AIC}}{of the Model.}
#' \item{\code{Area under the ROC curve (AUC)}}{of the Model.}
#' \item{\code{Average Precision at k}}{of the Model.}
#' \item{\code{Bias}}{of the Model.}
#' \item{\code{Brier score}}{of the Model.}
#' \item{\code{Classification Error}}{of the Model.}
#' \item{\code{F1 Score}}{of the Model.}
#' \item{\code{fScore}}{of the Model.}
#' \item{\code{GINI Coefficient}}{of the Model.}
#' \item{\code{kappa statistic}}{of the Model.}
#' \item{\code{Log Loss}}{of the Model.}
#' \item{\code{`Mallow's` cp}}{of the Model.}
#' \item{\code{Matthews Correlation Coefficient}}{of the Model.}
#' \item{\code{Mean Log Loss}}{of the Model.}
#' \item{\code{Mean Absolute Error}}{of the Model.}
#' \item{\code{Mean Absolute Percent Error}}{of the Model.}
#' \item{\code{Mean Average Precision at k}}{of the Model.}
#' \item{\code{Mean Absolute Scaled Error}}{of the Model.}
#' \item{\code{Median Absolute Error}}{of the Model.}
#' \item{\code{Mean Squared Error}}{of the Model.}
#' \item{\code{Mean Squared Log Error}}{of the Model.}
#' \item{\code{Model turning point error}}{of the Model.}
#' \item{\code{Negative Predictive Value}}{of the Model.}
#' \item{\code{Percent Bias}}{of the Model.}
#' \item{\code{Positive Predictive Value}}{of the Model.}
#' \item{\code{Precision}}{of the Model.}
#' \item{\code{Predictive Residual Sum of Squares}}{of the Model.}
#' \item{\code{R Square}}{of the Model.}
#' \item{\code{Relative Absolute Error}}{of the Model.}
#' \item{\code{Recall}}{of the Model.}
#' \item{\code{Root Mean Squared Error}}{of the Model.}
#' \item{\code{Root Mean Squared Log Error}}{of the Model.}
#' \item{\code{Root Relative Squared Error}}{of the Model.}
#' \item{\code{Relative Squared Error}}{of the Model.}
#' \item{\code{`Schwarz's` Bayesian criterion BIC}}{of the Model.}
#' \item{\code{Sensitivity}}{of the Model.}
#' \item{\code{specificity}}{of the Model.}
#' \item{\code{Squared Error}}{of the Model.}
#' \item{\code{Squared Log Error}}{of the Model.}
#' \item{\code{Symmetric Mean Absolute Percentage Error}}{of the Model.}
#' \item{\code{Sum of Squared Errors}}{of the Model.}
#' \item{\code{True negative rate}}{of the Model.}
#' \item{\code{True positive rate}}{of the Model.}
#'
#' @examples
#' library(splines)
#' library(readr)
#' Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
#' MLMetrics(Observed = Data, yvalue = Data$states, modeli = Model, K = 2,
#'  Name = "Linear", Form = "LM", kutuf = 0, TTy = "Number")
utils::globalVariables("pcrfit")
MLMetrics <- function(Observed, yvalue, modeli, K, Name, Form, kutuf, TTy){
  Predy = 0
  Preds = 0
  Probable = 0
  Predy <- if(Name == "ARIMA"){
    modeli[["fitted"]]
  }else if (Form == "ALM"){
    modeli[["fitted"]]
  }else if(Form == "ARDL"){
    c(0, 0, 0, modeli[["fitted.values"]])
  }else if(Name == "Values"){
    modeli
  }else{
    fitted.values(modeli)
  }

  if(Form == "LM"){
    Preds = fitted.values(modeli)
  }else if(Form == "ALM"){
    Preds = modeli[["fitted"]]
  }else if(Form == "N-LM"){
    Preds = 0
  }else if(Form == "GLM" & Name != "Log"){
    KK <- generics::augment(modeli, data = Observed)
    KK$Probable =  1/(1 + exp(-KK$.fitted))
    KK$Predicted =  ifelse(Probable > 0.5, 1, 0)
    Preds = KK$Predicted
  }else{
    Preds = stats::predict(modeli, type = "response")
  }

  ppk <- if(sum(Preds) == 0) 1 else 2
  kutuf <- if (kutuf != 0) kutuf else .5

  RD01 <- signif(ifelse(Name == "ARIMA",  modeli$aic,
                        ifelse(Name == "SMOOTH"| Name == "Values", 0,
                               stats::AIC(modeli))), 2)
  RD02 <- signif(ifelse(Name == "ARIMA",  modeli$bic,
                        ifelse(Name == "SMOOTH"| Name == "Values", 0,
                               stats::BIC(modeli))), 2)
  RD03 <- if(Name == "ARIMA" | Name == "SMOOTH"| Form == "GLM"|
             Name == "Values"| Name == "Logit"){
    0
  }else if(Name == "ALM"){
    signif(summary(modeli[["r.squared"]]), 2)
  }else{
    signif(summary(modeli)$r.squared, 2)
  }
  RD04 <- if(Name == "ARIMA" | Name == "SMOOTH"| Form == "GLM"|
             Name == "Values"| Name == "Logit"){
    0
  } else if (Name == "ALM"){
    signif(summary(modeli[["adj.r.squared"]]), 2)
  } else {
    signif(summary(modeli)$adj.r.squared, 2)
  }
  RD05 = signif(Metrics::accuracy(yvalue, Preds), 2)
  RD06 = signif(sum(Metrics::ae(yvalue, Preds)), 2)
  RD07 = signif(sum(Metrics::ape(yvalue, Predy)), 2)
  RD08 = signif(Metrics::apk(actual = yvalue, predicted = Preds, k = K), 2)
  RD09 = signif(ifelse(Form == "LM"| Form == "ARDL" |
                         TTy == "Number" | Name == "nil" & ppk == 1, 0,
                       ModelMetrics::auc(yvalue, Preds)), 2)
  RD10 = signif(Metrics::bias(yvalue, Preds), 2)
  RD11 = signif(ifelse(Form == "LM" | TTy == "Number"| Form == "ALM",
                       ModelMetrics::ce(yvalue, Predy),
                       ModelMetrics::ce(modeli)), 2)
  RD12 = signif(ifelse(Form == "LM" | Form == "ALM",
                       Metrics::f1(yvalue, Predy),
                       ModelMetrics::f1Score(yvalue, Preds,
                                             cutoff = kutuf)), 2)
  RD13 = signif(sum(na.omit((Metrics::ll(yvalue, Preds))), 2))
  RD14 = signif(ifelse(Form == "LM" | Form == "ALM",
                       ModelMetrics::logLoss(yvalue, Predy),
                       ModelMetrics::logLoss(yvalue, Preds)), 2)
  RD15 = if(Form == "GLM"){
    signif(ModelMetrics::mae(actual = yvalue, predicted = Preds), 2)
  }else if (Form == "LM"| TTy == "Number"| Form == "ALM"){
    signif(ModelMetrics::mae(actual = yvalue, predicted = Preds), 2)
  }else{
    signif(ModelMetrics::mae(actual = yvalue, predicted = Predy), 2)
  }
  RD16 = signif(Metrics::mape(yvalue, Predy), 2)
  RD17 = signif(ifelse(Name != "Values",
                       Metrics::mapk(actual = yvalue, predicted = Predy,
                                     k = K), 0), 2)
  RD18 = signif(Metrics::mase(yvalue, Preds), 2)
  RD19 = signif(Metrics::mdae(yvalue, Predy), 2)
  RD20 = signif(ifelse(Form == "LM" | Form == "ALM",
                       ModelMetrics::mse(yvalue, Predy),
                       ModelMetrics::mse(yvalue, Preds)), 2)
  RD21 = signif(Metrics::msle(yvalue, Preds), 2)
  RD22 = signif(Metrics::percent_bias(yvalue, Predy), 2)
  RD23 = signif(ifelse(Form == "LM" | Form == "ALM",
                       ModelMetrics::precision(yvalue, Predy),
                       ModelMetrics::precision(yvalue, Preds,
                                               cutoff = kutuf)), 2)
  RD24 = signif(Metrics::rae(yvalue, Predy), 2)
  RD25 = signif((ifelse(Form == "LM" | Form == "ALM",
                        ModelMetrics::recall(yvalue, Predy),
                        ModelMetrics::recall(yvalue, Preds,
                                             cutoff = kutuf))), 2)
  RD26 = signif(ifelse(Form == "LM" | Form == "ALM",
                       ModelMetrics::rmse(yvalue, Predy),
                       ModelMetrics::rmse(yvalue, Preds)), 2)
  RD27 = signif(Metrics::rmsle(yvalue, Preds), 2)
  RD28 = signif(Metrics::rrse(yvalue, Preds), 2)
  RD29 = signif(Metrics::rse(yvalue, Preds), 2)
  RD30 = signif(sum(Metrics::se(yvalue, Preds)), 2)
  RD31 = signif(sum(Metrics::sle(yvalue, Preds)), 2)
  RD32 = signif(Metrics::smape(yvalue, Predy), 2)
  RD33 = signif(Metrics::sse(yvalue, Preds), 2)
  ptp  = diff(yvalue, lag = 1) / diff(Predy, lag = 1)
  ptpe = ifelse(ptp > 0, 0, 1)
  RD34 = sum(ptpe)

  if(Name == "QUADRATIC"){
    Nlevels = 1
  }else if(Name == "SPLINE"){
    Nlevels = modeli[["rank"]] - 4
  }else{
    Nlevels = 0
  }

  if(Name != "SPLINE"){
    Type = Form
  }else{
    Type = "SPLINE"
  }

  RD37 = MallowsCp(model2 = modeli, y = yvalue, x = Observed[, -1],
                             type = Type, Nlevels = Nlevels)
  RD38 <- ifelse(ppk == 1 & Name == "QUADRATIC",
                 signif(predict_square(modeli, verbose = FALSE)$P.square, 2), 0)
  RD39 = signif(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM",
                       ModelMetrics::brier(yvalue, Preds),
                       ModelMetrics::brier(modeli)), 0)
  RD40 = signif(ifelse(Form == "LM"| TTy == "Number" | Form == "ALM",
                       ModelMetrics::gini(yvalue, Predy),
                       ModelMetrics::gini(modeli)), 0)
  RD41 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::kappa(yvalue, Preds,
                                           cutoff = kutuf)), 0)
  RD42 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::sensitivity(yvalue, Predy,
                                                 cutoff = kutuf)), 0)
  RD43 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::specificity(yvalue, Preds,
                                                 cutoff = kutuf)), 0)
  RD44 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::fScore(yvalue, Preds,
                                            cutoff = kutuf, beta = 1)), 0)
  RD45 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::mcc(yvalue, Preds, cutoff = kutuf)), 0)
  RD46 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::tnr(yvalue, Preds, cutoff = kutuf)), 0)
  RD47 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::tpr(yvalue, Preds, cutoff = kutuf)), 0)
  RD48 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::ppv(yvalue, Preds, cutoff = kutuf)), 0)
  RD49 = signif(ifelse(Form == "LM" | Form == "ALM", 0,
                       ModelMetrics::npv(yvalue, Preds, cutoff = kutuf)), 0)
  results <- list(
    "Absolute Error" = RD06,
    "Absolute Percent Error" = RD07,
    "Accuracy" = RD05,
    "Adjusted R Square" = RD04,
    "Akaike's Information Criterion AIC" = RD01,
    "Area under the ROC curve (AUC)" = RD09,
    "Average Precision at k" = RD08,
    "Bias" = RD10,
    "Brier score" = RD39,
    "Classification Error" = RD11,
    "F1 Score" = RD12,
    "fScore" = RD44,
    "GINI Coefficient" = RD40,
    "kappa statistic" = RD41,
    "Log Loss" = RD13,
    "Mallow's cp" = RD37,
    "Matthews Correlation Coefficient" = RD45,
    "Mean Log Loss" = RD14,
    "Mean Absolute Error" = RD15,
    "Mean Absolute Percent Error" = RD16,
    "Mean Average Precision at k" = RD17,
    "Mean Absolute Scaled Error" = RD18,
    "Median Absolute Error" = RD19,
    "Mean Squared Error" = RD20,
    "Mean Squared Log Error" = RD21,
    "Model turning point error" = RD34,
    "Negative Predictive Value" = RD49,
    #    "Observed turning point error" = RD35$tp,
    "Percent Bias" = RD22,
    "Positive Predictive Value" = RD48,
    "Precision" = RD23,
    "Predictive Residual Sum of Squares" = RD38,
    "R Square" = RD03,
    "Relative Absolute Error" = RD24,
    "Recall" = RD25,
    "Root Mean Squared Error" = RD26,
    "Root Mean Squared Log Error" = RD27,
    "Root Relative Squared Error" = RD28,
    "Relative Squared Error" = RD29,
    "Schwarz's Bayesian criterion BIC" = RD02,
    "Sensitivity" = RD42,
    "specificity" = RD43,
    "Squared Error" = RD30,
    "Squared Log Error" = RD31,
    "Symmetric Mean Absolute Percentage Error" = RD32,
    "Sum of Squared Errors" = RD33,
    "True negative rate" = RD46,
    "True positive rate" = RD47
    #   "Turning point error using random tests" = RD36$tp
  )
  return(results)
}

predict_square <- function(object, verbose = TRUE) {
  fetcheddata <- fetchdata(object)
  data <- fetcheddata$data
  pred.pos <- fetcheddata$pred.pos
  resp.pos <- fetcheddata$resp.pos
  pred.name <- fetcheddata$pred.name
  press.res <- vector("numeric", nrow(data))

  for (i in 1:nrow(data)) {
    if (verbose) {
      counter(i)
      flush.console()
    }

    newdata <- data[-i, ]

    if (class(object)[1] == "pcrfit")
      newmod <- pcrfit(newdata, cyc = 1, fluo = 2, model = object$MODEL,
                       verbose = FALSE)
    else newmod <- update(object, data = newdata)

    newpred <- as.data.frame(data[i, pred.pos])
    colnames(newpred) <- pred.name
    y.hat <- as.numeric(predict(newmod, newdata = newpred))
    press.res[i] <- data[i, resp.pos] - y.hat
  }

  if (verbose) cat("\n")

  yi <- residuals(object) - fitted(object)
  tss <- sum((yi - mean(yi))^2)
  rss <- sum(press.res^2)
  P.square <- 1 - (rss / tss)

  return(list(stat = sum(press.res^2), residuals = press.res,
              P.square = P.square))
}

fetchdata <- function(object) {
  if (class(object)[1] == "pcrfit") data <- object$data

  iicc <- inherits(object$call$data,"class")

  if (iicc == "name") data <- eval(object$call$data)
  else if (iicc == "data.frame" || iicc == "matrix")
    data <- object$call$data
  else if (is.null(object$call$data))
    data <- as.data.frame(sapply(all.vars(object$call$formula),
                                 function(a) get(a, envir = .GlobalEnv)))

  vars <- all.vars(object$call$formula)
  lhs <- vars[1]
  rhs <- vars[-1]

  pred.pos <- match(rhs, colnames(data))
  pred.name <- rhs[which(!is.na(pred.pos))]
  pred.pos <- as.numeric(na.omit(pred.pos))
  resp.pos <- match(lhs, colnames(data))

  return(list(data = data, pred.pos = pred.pos, resp.pos = resp.pos,
              pred.name = pred.name))
}

counter <- function(i) {
  if (i %% 10 == 0) cat(i) else cat(".")
  if (i %% 50 == 0) cat("\n")
  flush.console()
}
