% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/treatmemt_model.R
\name{treatment_model}
\alias{treatment_model}
\alias{treatments}
\title{Enhanced Estimation of Treatment Effects of Binary Data from Randomized
Experiments}
\usage{
treatment_model(Treatment, x_data)
}
\arguments{
\item{Treatment}{Vector of binary data (0 = control population,
1 = treated population) LHS for the treatment effects estimation}

\item{x_data}{Data frame of explanatory variables for the RHS of the
estimation}
}
\value{
A list with the following components:
\item{\code{Model}}{Estimated treatment effects model.}
\item{\code{Effect}}{Data frame of the estimated various treatment effects.}
\item{\code{P_score}}{Vector of estimated propensity scores from the model}
\item{\code{Fitted_estimate}}{Vector of fitted values from the model}
\item{\code{Residuals}}{Residuals of the estimated model}
\item{\code{`Experiment plot`}}{Plot of the propensity scores from the model
faceted into Treated and control populations}
\item{\code{`ATE plot`}}{Plot of the average treatment effect for the
\strong{entire} population}
\item{\code{`ATT plot`}}{Plot of the average treatment effect for the
\strong{treated} population}
\item{\code{`ATC plot`}}{Plot of the average treatment effect for the
\strong{controlled} population}
\item{\code{`ATM plot`}}{Plot of the average Treatment effect for the
\strong{evenly} population}
\item{\code{`ATO plot`}}{Plot of the average Treatment effect for the
\strong{overlap} population}
\item{\code{weights}}{Estimated weights for each of the treatment effects}
}
\description{
Observational study involves the evaluation of outcomes of participants not
randomly assigned treatments or exposures. To be able to assess the effects
of the outcome, the participants are matched using propensity scores (PSM).
This then enables the determination of the effects of the treatments on
those treated against those who were not treated. Most of the earlier
functions available for this analysis only enables the determination of
the average treatments effects on the treated (ATT) while the other
treatment effects are optional. This is where this functions is unique
because five different average treatment effects are estimated
simultaneously, in spite of the \strong{one line code arguments}. The five
treatment effects are:
\enumerate{
\item Average treatment effect  for the entire (ATE)  population
\item Average treatment effect  for the treated (ATT)  population
\item Average treatment effect  for the controlled (ATC)  population
\item Average treatment effect  for the evenly matched (ATM)  population
\item Average treatment effect  for the overlap (ATO) population.
}

There are excellent materials dealing with each of the treatment effects,
please see \href{https://livefreeordichotomize.com/posts/2019-01-17-understanding-propensity-score-weighting/}{Understanding propensity score weighting}
}
\examples{
library(readr)
Treatment = treatments$treatment
data = treatments[, c(2:3)]
treatment_model(Treatment, data)

}
