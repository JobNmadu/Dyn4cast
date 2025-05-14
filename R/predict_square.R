#' Predictive Residual Sum of Squares
#'
#' @param object Model Estimated
#' @param verbose Outputs details argument
#'
#' @importFrom stats fitted
#' @importFrom stats residuals
#' @importFrom stats update
#' @importFrom utils flush.console
#'
#' @name predict_square
#' @keywords internal
#' @export
#'
utils::globalVariables("pcrfit")
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
