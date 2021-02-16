
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Dyn4cast

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/JobNmadu/Dyn4cast.svg?branch=master)](https://travis-ci.org/JobNmadu/Dyn4cast)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/JobNmadu/Dyn4cast?branch=master&svg=true)](https://ci.appveyor.com/project/JobNmadu/Dyn4cast)
[![Coverage
status](https://codecov.io/gh/JobNmadu/Dyn4cast/branch/master/graph/badge.svg)](https://codecov.io/github/JobNmadu/Dyn4cast?branch=master)
[![CRAN
status](https://www.r-pkg.org/badges/version/Dyn4cast)](https://cran.r-project.org/package=Dyn4cast)
<!-- badges: end -->

The **Dyn4cast** package is designed to be a lightweight package, with
the aim of simplifying the estimation, prediction and forecast of
time-varying dataset, especially where the data is continuously
collected on routine and regular basis. The package takes away the
efforts of loading the libraries of more than 10 packages which have
been used to develop the functions for forecasting the data.

## Installation

Althouth it would be possible to install the released version of
Dyn4cast from [CRAN](https://CRAN.R-project.org) in future, presently,
only the development version is available. The canonical form to do this
is:

``` r
install.packages("Dyn4cast")
```

The development version is the only one available now and can be
installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JobNmadu/Dyn4cast")
```

## Basic usage

At present, the package exports a single function, `DynamicForecast`,
which takes two required arguments: the `Data` of any recognised format
but should be a **dataframe** containing two columns `Date` and `Case`.
The Date is the *day* the data is collected while Case is the variable
for forecasting. The other arguements parsed to the function are
`MaximumDate`, which is the last date Data was collected and `BREAKS`,
which is a vector of numbers and used as `knots` in estimating spline
polynomials.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(Dyn4cast)
## basic example code
niz2 <- readxl::read_excel("~/Data.xlsx")
niz2$Date <- as.Date(niz2$Date, format = '%m/%d/%Y')

Dss <- seq(niz2$Date[1], by = "day", length.out = length(niz2$Case))
lastdayfo21 <- Dss[length(Dss)]
BREAKS = c(70, 131, 173, 228, 274)
KK_28 <- niz2[niz2$Date <= lastdayfo21 - 28, ]
Days_28 <- DynamicForecast(Data = KK_28, BREAKS = BREAKS, MaximumDate = "2021-02-10")
summary(Days_28$`Ensembled based on summed weight`)

knitr::kable(as.data.frame(Days_28$Forecast), row.names = FALSE, "html")
knitr::kable(as.data.frame(Days_28$RMSE), row.names = FALSE, "html")
Days_28$Plot

KK_14 <- niz2[niz2$Date <= lastdayfo21 - 14, ]
Days_14 <- DynamicForecast(Data = KK_28, BREAKS = BREAKS, MaximumDate = "2021-02-10")

summary(Days_14$`Ensembled based on weight`)

knitr::kable(as.data.frame(Days_14$Forecast), row.names = FALSE, "html")
knitr::kable(as.data.frame(Days_14$RMSE), row.names = FALSE, "html")
Days_14$Plot
```

![](docs/README.html)

## Suggested packages

Although not a dependency, the package derives functionally from a
number of other packages ans so may require you to install such packages
if they are not yet installed. the packages are listed below:

``` r
install.packages(c("forecast", "lubridate", "Metrics", "tidyr", "ggplot2", "magrittr", "formattable", "xlsx", "readxl"))
```

## Things the package can do

The package is capable of estimation, prediction and forecasting of the
following models.  
- Spline without knots  
- Spline with knots  
- Smooth Spline  
- ARIMA  
- Quadratic  
- Ensembled with equal weight  
- Ensembled based on weight  
- Ensembled based on summed weight  
- Ensembled based on weight of fit

Note that a *warning* (not *error*) is thrown up while estimating the
RMSE for the `Ensembled with equal weight` model. It was thoroughly
investigated and causes no harm. Efforts are still on to silence the
warning, which I will soon. The warning is one of such issues that is
general to R. If you set your *chunk option* to `warning = FALSE` you
will not notice the warning.

## Other suggestions?

The package is still very much in progress as such feedback,
particularly at this developmental stage, would be greatly welcome and
appreciated.
