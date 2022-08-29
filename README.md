
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Dyn4cast <img src="man/figures/logo.png" align="right" alt="" width="150" height="150" />

<!-- badges -->

[![R build
status](https://github.com/JobNmadu/Dyn4cast/workflows/R-CMD-check/badge.svg)](https://github.com/JobNmadu/Dyn4cast/actions)

[![Build
Status](https://travis-ci.org/JobNmadu/Dyn4cast.svg?branch=master)](https://travis-ci.org/JobNmadu/Dyn4cast)

[![codecov](https://codecov.io/gh/JobNmadu/Dyn4cast/branch/master/graph/badge.svg)](https://app.codecov.io/gh/JobNmadu/Dyn4cast)

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

<!-- end -->

The **Dyn4cast** package is designed to be a lightweight package, with
the aim of simplifying the estimation, prediction and forecast of
time-varying dataset, especially where the data is continuously
collected on routine and regular basis. The package takes away the
efforts of loading the libraries of more than 10 packages which have
been used to develop the functions for forecasting the data.

## Installation

Although it would be possible to install the released version of
*Dyn4cast* from [CRAN](https://CRAN.R-project.org) in future, presently,
only the development version is available. The canonical form for
[CRAN](https://CRAN.R-project.org) is:

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

At present, the package exports the following functions:

1.  `DynamicForecast` which takes two required arguments: the `Data` of
    any recognized format but should be a **dataframe** containing two
    columns `Date` and `Case`. The Date is the *day/month/year* the data
    is collected while Case is the variable for forecasting. The Date
    must be in the recognized format i.e. ‘YYYY-MM-DD’. The other
    arguments parsed to the function are `MaximumDate`, which is the
    last date Data was collected and `BREAKS`, which is a vector of
    numbers and is used as `knots` in estimating spline polynomials.

2.  `constrainedforecast` which constrain forecast of one-sided integer
    forecast to lie between the lower and upper limits of the base data.
    The function estimates the lower and upper 80% and 95% forecasts of
    the estimated model. This function works with two other functions,
    that is, `invscaledligit` and `scaledlogit` which are adapted from
    Hyndman & Athanasopoulos (2021) and both of which are adopted.

3.  `Percent` which affix the % sign on a value or a vector or data
    frame of values.

4.  `MachineLearningMetrics` which collects more than 40 metrics that
    are useful in model selection. The beauty of this function is the
    simplicity with which these metrics are collected from difference
    packages and saves the user the need to load more than 10 libraries
    in order to get these metrics.

5.  `MallowsCP` for determining the the Mallows CP.

6.  `Linearsystems` for linear regression model with some
    transformation.

7.  `quicksummary` which outputs a formatted table of useful summary
    statistics of machine learning data.

8.  `formattedcut` is a wrapper for providing publication ready
    frequency tables for continuous variable.

## Things the package can do

The package is capable of

-   computing, estimating, predicting and forecasting of the following
    models.

    -   Spline without knots

    -   Spline with knots

    -   Smooth Spline

    -   ARIMA

    -   Quadratic

    -   Ensembled with equal weight

    -   Ensembled based on weight

    -   Ensembled based on summed weight

    -   Ensembled based on weight of fit

-   Unconstrained forecasts

-   Constrained forecast

-   Machine Learning Metrics

-   Mallow’s CP

-   Per cent sign

    -   Rate

    -   percent

-   Scaled logit for constrained forecast

-   Inverse scaled logit for constrained forecast

-   Linear regression and transformations which consists of

    -   Linear model

    -   Semilog model

    -   Growth model

    -   Double Log model

    -   Quadratic model

    -   Inverse of y model

    -   Square root of y model

    -   formatted Model Table

    -   Prediction plots

    -   Summary of numeric variables

    -   Summary of character variables

-   Convert a continuous vector to a data frame

## Citation

The citation information for this package can be obtained easily when
you run `citation("Dyn4cast")` in your `R` console.

``` r

citation("Dyn4cast")

To cite package 'Dyn4cast' in publications use:

  Nmadu J (????). _Dyn4cast: Dynamic Modeling and Machine Learning
  Environment_. R package version 11.11.1.9000,
  <https://github.com/JobNmadu/Dyn4cast>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {Dyn4cast: Dynamic Modeling and Machine Learning Environment},
    author = {Job Nmadu},
    note = {R package version 11.11.1.9000},
    url = {https://github.com/JobNmadu/Dyn4cast},
  }
```

## Suggested packages

Although not a dependency, the package derives functionally from a
number of other packages and so may require you to install such packages
if they are not yet installed. The packages are listed below:

``` r
install.packages(c("forecast", "lubridate", "Metrics", "tidyr", "ggplot2", "magrittr", "formattable", "xlsx", "readxl"))
```

Note that a *warning* (not *error*) is thrown up while estimating the
RMSE for the `Ensembled with equal weight` model. It was thoroughly
investigated and causes no harm. Efforts are still on to silence the
warning, which I will soon. The warning is one of such issues that is
general to R. If you set your *chunk option* to `warning = FALSE` you
will not notice the warning.

## Other suggestions?

The package is still very much in progress as such feedback,
particularly at this developmental stage, would be greatly welcome and
appreciated. Please fork your feedback at GitHub.

## Bibliography

Mahoney, M. (2021). [Model averaging methods: how and why to build
ensemble models.](https://www.mm218.dev/posts/2021/01/model-averaging/)

Hyndman, R. J. (2020). Quantile forecasting with ensembles and
combinations in *Business Forecasting: The Emerging Role of Artificial
Intelligence and Machine Learning*, eds. Gilliland, Tashman & Sglavo.
John Wiley & Sons.

Hyndman, R.J., & Athanasopoulos, G. (2021). *Forecasting: principles and
practice*, 3rd edition, OTexts: Melbourne, Australia. OTexts.com/fpp3.
Accessed on July 30, 2021.

## Code of Conduct

Please note that the Dyn4cast project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
