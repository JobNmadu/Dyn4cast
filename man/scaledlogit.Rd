% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaledlogit.R
\name{scaledlogit}
\alias{scaledlogit}
\title{Scale Parameter for Integer Modeling and Forecast}
\usage{
scaledlogit(x, lower, upper)
}
\arguments{
\item{x}{The parameter to be scaled, which is the fitted values from
supported models. The scaled parameter is used mainly for constrained
forecasting of a response variable \emph{positive (0 - inf) or negative
(-inf - 0)}. The scaling involves log transformation of the parameter}

\item{lower}{Integer or variable representing the lower limit for the
scaling (-inf or 0)}

\item{upper}{Integer or variable representing the upper limit for the
scaling (0 or inf)}
}
\description{
This function is a wrapper for scaling the fitted (predicted) values of a
one-sided (positive or negative only) integer response variable of supported
models. The scaling involves some log transformation of the fitted
(predicted) values.
}
\examples{
library(Dyn4cast)
library(splines)
lower <- 1
upper <- 37
Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
scaledlogit(x = fitted.values(Model), lower = lower,
 upper = upper)
}
