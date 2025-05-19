
<!-- README.md is generated from README.Rmd. Please edit that file -->

<!--
[![Build Status](https://travis-ci.org/JobNmadu/Dyn4cast.svg?branch=master)](https://travis-ci.org/JobNmadu/Dyn4cast)
&#10;[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
&#10;[![codecov](https://codecov.io/gh/JobNmadu/Dyn4cast/branch/master/graph/badge.svg)](https://codecov.io/gh/JobNmadu/Dyn4cast)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
-->

# Dyn4cast <img src="man/figures/logo.png" align="right" alt="" width="150" height="150" />

<!-- badges: start -->

[![](https://img.shields.io/badge/Made%20With-R-9cf)](https://github.com/JobNmadu/Dyn4cast)

[![R-CMD-check](https://github.com/JobNmadu/Dyn4cast/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JobNmadu/Dyn4cast/actions/workflows/R-CMD-check.yaml)

[![GitHub release (latest by
date)](https://img.shields.io/github/v/release/JobNmadu/Dyn4cast?color=green)](https://github.com/JobNmadu/Dyn4cast/releases)

[![codecov](https://codecov.io/gh/JobNmadu/Dyn4cast/graph/badge.svg?token=RYV9KWHBN5)](https://codecov.io/gh/JobNmadu/Dyn4cast)

[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)

<!--
&#10;[![Build Status](https://travis-ci.org/JobNmadu/Dyn4cast.svg?branch=master)](https://travis-ci.org/JobNmadu/Dyn4cast)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
&#10;[![codecov](https://codecov.io/gh/JobNmadu/Dyn4cast/branch/master/graph/badge.svg)](https://codecov.io/gh/JobNmadu/Dyn4cast)
&#10;[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
-->

<!-- badges: end -->

The **Dyn4cast** package is designed to be a lightweight package. The
philosophy behind it was the need to provide quick updates and
visualization of Nigerian *COVID-19* cases during the pandemic in 2020,
hence had only one function, **DynamicForecast**. The aim was to
simplify the estimation, prediction and forecast of time-varying
*COVID-19* dataset, based on daily update of incidences. There was need
to have a function which is able to handle continuous data collection,
estimation and forecast. That was what led to the name of the package.
From that single function, the number of functions have grown to more
than 10 because there were need to have supporting functions to make the
forecasting easy. However, the package has pride itself of the *line
line technology* by providing various **machine learning functions**
that have the functionality of revealing the facts behind your data. The
functions are working optimally and efforts are continuously being made
to improve on them and ensuring that dependencies are reduced to the
barest minimum. The unique selling point of this package is that it
takes away the need to load multiple libraries to perform the various
machine learning tasks.

## Installation

Although it would be possible to install the released version of
*Dyn4cast* from [CRAN](https://CRAN.R-project.org) in future, presently,
only the development version is available. The canonical form for
[CRAN](https://CRAN.R-project.org) is:

    install.packages("Dyn4cast")

The development version is the only one available now and can be
installed from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("JobNmadu/Dyn4cast")
```

The development version can also be installed through
[r-universe](https://jobnmadu.r-universe.dev/Dyn4cast). Use the form:

``` r
install.packages("Dyn4cast", repos = c("https://jobnmadu.r-universe.dev", "https://cloud.r-project.org"))
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

4.  `MLMetrics` which collects more than 40 metrics that are useful in
    model selection. The beauty of this function is the simplicity with
    which these metrics are collected from difference packages and saves
    the user the need to load more than 10 libraries in order to get
    these metrics.

5.  `MallowsCP` for determining the the Mallows CP.

6.  `Linearsystems` for linear regression model with some
    transformation.

7.  `quicksummary` which outputs a formatted table of useful summary
    statistics of machine learning data.

8.  `formattedcut` is a wrapper for providing publication ready
    frequency tables for continuous variable.

9.  `data_transform` is a wrapper for standardizing `data.frame` to make
    the values comparable for estimation and/or visualization.

10. `estimate_plot` is a function for plotting estimated coefficients of
    a model in their order of significance.

11. `corplot` is for plotting the correlation matrix.

12. `garrett_ranking` is for ranking Likert-type data.

13. `Model_factors` is for determining and retrieving latent factors
    from Likert-type data for estimation and Machine Learning.

14. `treatment_model` is for propensity matching treatments effects and
    other metrics in the Machine Learning Environment.

15. `mdpi` is for computation of multidimensional poverty indicators and
    indices

16. `plot_mdpi` is to plot the indices

17. `gender` is to convert age and sex to gender

## Things the package can do

The package is capable of

- computing, estimating, predicting and forecasting of the following
  models.

  - Spline without knots

  - Spline with knots

  - Smooth Spline

  - ARIMA

  - Quadratic

  - Ensembled with equal weight

  - Ensembled based on weight

  - Ensembled based on summed weight

  - Ensembled based on weight of fit

- Unconstrained forecasts

- Constrained forecast

- Machine Learning Metrics

- Mallow’s CP

- Per cent sign

  - Rate

  - percent

- Scaled logit for constrained forecast

- Inverse scaled logit for constrained forecast

- Linear regression and functional forms which consists of

  - Linear model

  - Linear model with interactions

  - Semilog model

  - Growth model

  - Double Log model

  - Mixed-power model

  - Translog model

  - Quadratic model

  - Cubic model

  - Inverse of y model

  - Inverse of x model

  - Inverse of y & x model

  - Square root model

  - Cubic root model

  - formatted Model Table

  - Prediction plots

  - Fitted plots

  - Naive effects plots

  - Summary of numeric variables

  - Summary of character variables

- Convert a continuous vector to a data frame

- Convert a raw data frame to a uniform data frame

- Plot of correlation matrix

- Plot of the order of significance of estimates coefficients

- Rank Likert-type data using Garrett ranking technique

- Determine and retrieve the latent factors in Likert-type variables

- *Treatment model* which is for propensity matching and treatment
  effects. It has the capacity to provide:

  - Estimated treatment effects model

  - Data frame of the estimated various treatment effects

  - Vector of estimated propensity scores from the model

  - Vector of fitted values from the model

  - Residuals of the estimated model

  - Plot of the propensity scores from the model faceted into Treated
    and control populations

  - Plot of the average treatment effect for the **entire** population

  - Plot of the average treatment effect for the **treated** population

  - Plot of the average treatment effect for the **controlled**
    population

  - Plot of the average Treatment effect for the **evenly** population

  - Plot of the average Treatment effect for the **overlap** population

  - Estimated weights for each of the treatment effects

- *mdpi* computes MDPI on various dimensions to give:

  - Deprivation scores

  - Incidence of poverty

  - Adjusted incidence of poverty

  - Contribution in % of each of the dimensions to MDPI

  - Multidimensional poverty index (MDPI)

  - Average deprivation among the deprived

- *plot_mdpi* produces the plots of all the indices

- *gender* converts age and sex to gender

## Citation

The citation information for this package can be obtained easily when
you run `citation("Dyn4cast")` in your `R` console.

``` r

citation("Dyn4cast")
To cite package 'Dyn4cast' in publications use:

  Nmadu J (2025). _Dyn4cast: Dynamic Modeling and Machine Learning
  Environment_. R package version 11.11.24, commit
  23e0180d4ac8942ab1e8d96bfbc7fdb0a34ddb16,
  <https://github.com/JobNmadu/Dyn4cast>.

A BibTeX entry for LaTeX users is

  @Manual{,
    title = {Dyn4cast: Dynamic Modeling and Machine Learning Environment},
    author = {Job Nmadu},
    year = {2025},
    note = {R package version 11.11.24, commit 23e0180d4ac8942ab1e8d96bfbc7fdb0a34ddb16},
    url = {https://github.com/JobNmadu/Dyn4cast},
  }
```

## Suggested packages

Although not dependencies, the package derives functionally from a
number of other packages and so may require you to install them if not
already installed on your machine. Some of the packages are listed
below:

``` r
install.packages(c("lubridate", "tidyverse", "xlsx", "readxl", "rmarkdown", "covr",
                   "qpdf", "caret", "kableExtra", "knitr", "spelling", "psych", 
                   "lifecycle", "MetBrewer"))
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

Lucy D’Agostino McGowan (2019). Understanding propensity score
weighting. Available at:
<https://livefreeordichotomize.com/posts/2019-01-17-understanding-propensity-score-weighting/>

## Code of Conduct

Please note that the Dyn4cast project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
