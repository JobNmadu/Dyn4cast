# Getting started

## Installation

`Dyn4cast` is on CRAN and the development version is available. The
package is very functional and stable and is actively being watched for
any issue. Presently, it has passed all the tests it was subjected to.

[![R-CMD-check](https://github.com/JobNmadu/Dyn4cast/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JobNmadu/Dyn4cast/actions/workflows/R-CMD-check.yaml)
[![registry status
badge](https://jobnmadu.r-universe.dev/badges/:registry)](https://jobnmadu.r-universe.dev/)
[![name status
badge](https://jobnmadu.r-universe.dev/badges/:name)](https://jobnmadu.r-universe.dev/)
[![GitHub release (latest by
date)](https://img.shields.io/github/v/release/JobNmadu/Dyn4cast?color=green)](https://github.com/JobNmadu/Dyn4cast/releases)
[![codecov](https://codecov.io/gh/JobNmadu/Dyn4cast/graph/badge.svg?token=RYV9KWHBN5)](https://app.codecov.io/gh/JobNmadu/Dyn4cast)
[![lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![Project Status:
Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![downloads](http://cranlogs.r-pkg.org/badges/grand-total/Dyn4cast)](https://cran.r-project.org/package=Dyn4cast)

The released version of *Dyn4cast* can be installed from
[CRAN](https://cran.r-project.org/web/packages/Dyn4cast/index.html). The
canonical form for
[CRAN](https://cran.r-project.org/web/packages/Dyn4cast/index.html) is:

    install.packages("Dyn4cast")

To install the development version of `Dyn4cast` from
[GitHub](https://github.com/JobNmadu/Dyn4cast), use the following
canonical form:

``` r


# install.packages("devtools")
pak::pak("JobNmadu/Dyn4cast")
```

The development version can also be installed through
[r-universe](https://jobnmadu.r-universe.dev/Dyn4cast). Use the form:

``` r


install.packages("Dyn4cast", repos = c("https://jobnmadu.r-universe.dev", "https://cloud.r-project.org"))
```

## Suggested packages

A number of other packages are required for the smooth running of the
package and may need to be installed if not already installed on your
machine. Some of the packages are listed below:

``` r

install.packages(c("lubridate", "tidyverse", "xlsx", "readxl", "rmarkdown",
                   "covr", "caret", "kableExtra", "knitr", "spelling",
                   "psych", "lifecycle", "MetBrewer", "data.table", "ggtext", 
                   "lubridate", "forecast", "MASS", "mlogit", "nnet", "betareg",
                   "mvProbit", "miscTools"))
```

## Citation

The citation information for this package can be obtained easily when
you run `citation("Dyn4cast")` in your `R` console.

    To cite package 'Dyn4cast' in publications use:

      Nmadu J (2026). _Dyn4cast: Dynamic Modeling and Machine Learning
      Environment_. doi:10.32614/CRAN.package.Dyn4cast
      <https://doi.org/10.32614/CRAN.package.Dyn4cast>. R package version
      11.11.26, <https://jobnmadu.github.io/Dyn4cast/>.

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {Dyn4cast: Dynamic Modeling and Machine Learning Environment},
        note = {R package version 11.11.26},
        author = {Job Nmadu},
        year = {2026},
        doi = {10.32614/CRAN.package.Dyn4cast},
        url = {https://jobnmadu.github.io/Dyn4cast/},
      }
