% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/formattedcut.R
\name{formattedcut}
\alias{formattedcut}
\title{Convert continuous vector variable to formatted factors}
\usage{
formattedcut(data, breaks, cut = FALSE)
}
\arguments{
\item{data}{A vector of the data to be converted to factors if not cut
already or the vector of a cut data}

\item{breaks}{Number of classes to break the data into}

\item{cut}{\code{Logical} to indicate if the \code{cut} function has already being
applied to the data, defaults to \code{FALSE}.}
}
\value{
The function returns a \verb{data frame} with three or four columns
i.e \verb{Lower class}, \verb{Upper class}, \verb{Class interval} and \code{Frequency} (if the
cut is \code{FALSE}).
\item{\code{Cut}}{The \verb{data frame}}
}
\description{
Often, when a continuous data is converted to factors using the \verb{base R}
cut function, the resultant \verb{Class Interval} column provide data with
scientific notation which normally appears confusing to interpret,
especially to casual data scientist. This function provide a more
user-friendly output and is provided in a formatted manner. It is a easy to
implement function.
}
\examples{
library(tidyverse)
DD <- rnorm(100000)
formattedcut(DD, 12, FALSE)
DD1 <- cut(DD, 12)
DDK <- formattedcut(DD1, 12, TRUE)
DDK
# if data is not from a data frame, the frequency distribution is required.
as.data.frame(DDK \%>\%
group_by(`Lower class`, `Upper class`, `Class interval`) \%>\%
tally())
}
