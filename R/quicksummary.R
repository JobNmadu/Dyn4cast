#' Quick Formatted Summary of Machine Learning Data
#'
#' @description
#' There is increasing need to make user-friendly and production ready Tables
#'  for machine learning data. This function is a simplified quick summary and
#'  the output is a formatted table. This is very handy for those who do not
#'  have the time to write codes for user-friendly summaries.
#'
#' @param x The data to be summarised. Only numeric data is allowed.
#' @param Type The type of data to be summarised. There are two options here 1
#' or 2, 1 = `Continuous` and 2 = `Likert-type`
#' @param Cut The cut-off point for Likert-type data
#' @param Up The top Likert-type scale, for example, `Agree`, `Constraints` etc
#'  which would appear in the remark column.
#' @param Down The lower Likert-type scale, for example, `Disagree`,
#' `Not a Constraint` etc which would appear in the remark column.
#' @param ci Confidence interval which is defaults to 0.95.
#'
#' @return The function returns formatted tables of the Quick summary
#' \item{\code{Summary}}{List of two `data.frames`}
#'
#' @export quicksummary
#'
#' @importFrom stats var
#' @importFrom stats quantile
#' @importFrom stats qt
#' @importFrom stats median
#'
#' @aliases Quicksummary
#'
#' @examples
#' # Likert-type data
#' Up <- "Constraint"
#' Down <- "Not a constraint"
#' quicksummary(x = Quicksummary, Type = 2, Cut = 2.60, Up = Up, Down = Down)
#'
#' # Continuous data
#' x <- select(linearsystems, 1:6)
#' quicksummary(x = x, Type = 1)
quicksummary <- function(x, Type, Cut, Up, Down, ci = 0.95) {
  y  <-  as.matrix(x)
  if (is.null(colnames(y))) {
    Dim <- dim(y)[2]
    if (Dim == 1) {
      colnames(y) <- paste(substitute(x), collapse = ".")
    } else if (Dim > 1) {
      colnames(y)  <-  paste(paste(substitute(x), collapse = ""),
                             1 : Dim, sep = "")
    }
  }
  cl.vals  <-  function(x, ci) {
    x  <-  x[!is.na(x)]
    n  <-  length(x)
    if (n <= 1)
      return(c(NA, NA))
    se.mean  <-  sqrt(stats::var(x) / n)
    t.val  <-  stats::qt((1 - ci) / 2, n - 1)
    mn  <-  mean(x)
    lcl  <-  mn + se.mean * t.val
    ucl  <-  mn - se.mean * t.val
    c(lcl, ucl)
  }
  nColumns  <-  dim(y)[2]
  ans <-NULL
  ank <- NULL
  for (i in 1:nColumns) {
    X  <-  y[, i]
    X.length  <-  length(X)
    X  <-  X[!is.na(X)]
    X.na  <-  X.length - length(X)
    sod <- X - mean(X)
    SD <- sd(X)
    n <- length(X)
    NNS <- n / ((n - 1) * (n - 2))
    skewness <- NNS * sum((sod / SD)^3)
    kurtosis <- ((sum(sod^4) / n) / SD^4) - 3

    if (Type == 1) {
      z  <-  c(mean(X), sqrt(stats::var(X)),
               sqrt(stats::var(X) / length(X)), min(X), median(X), max(X),
               as.numeric(stats::quantile(X, prob = 0.25, na.rm = TRUE)),
               as.numeric(stats::quantile(X, prob = 0.75, na.rm = TRUE)),
               skewness, kurtosis, X.length)
      znames  <-  c("Mean", "SD", "SE Mean", "Min", "Median", "Max", "Q1",
                    "Q3",  "Skewness", "Kurtosis", "Nobs")
      me_n <- c("Arithmetic", "Geometric", "Quadratic", "Harmonic", "Cubic")
      me <- c(amean(X), gmean(X), qmean(X), hmean(X), cmean(X))
    } else {
      z  <-  c(mean(X), sqrt(var(X)), sqrt(var(X) / length(X)),
               X.length)
      znames  <-  c("Mean", "SD", "SE Mean", "Nobs")
      me_n <- c("Arithmetic", "Geometric", "Quadratic", "Harmonic", "Cubic")
      me <- c(amean(X), gmean(X), qmean(X), hmean(X), cmean(X))
    }
    result <- matrix(z, ncol = 1)
    risult <- matrix(me, ncol = 1)
    row.names(result)  <-  znames
    row.names(risult)  <-  me_n
    ans  <-  cbind(ans, result)
    ank  <-  cbind(ank, risult)
  }
  colnames(ans)  <-  colnames(y)
  ans  <-  data.frame(round(t(ans), digits = 2))

  colnames(ank)  <-  colnames(y)
  ank  <-  data.frame(round(t(ank), digits = 2))

  if (Type != 1) {
    ans        <-  ans[order(-ans$Mean), ]
    ans$Rank    <-  1:length(ans$Mean)
    ans$Remark  <-  ifelse(ans$Mean < Cut, Down, Up)
  } else {
    ans  <-  ans
  }

  if (ncol(ans) > nrow(ans)) {
    ans  <-  t(ans)
    ank  <-  t(ank)
  } else {
    ans  <-  ans
    ank  <-  ank
  }
  list(Summary = ans, Means = ank)
}

amean <- function(x) sum(x, na.rm = TRUE) / length(x[!is.na(x)])
gmean <- function(x) prod(x, na.rm = TRUE) ^ (1 / length(x[!is.na(x)]))
qmean <- function(x) sqrt(sum(x ^ 2, na.rm = TRUE) / length(x[!is.na(x)]))
hmean <- function(x) length(x[!is.na(x)]) / sum(1 / x, na.rm = TRUE)
cmean <- function(x) (sum(x ^ 3, na.rm = TRUE) /
                        length(x[!is.na(x)])) ^ 0.3333333
