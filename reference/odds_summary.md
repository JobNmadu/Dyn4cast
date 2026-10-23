# Odds-Based Measures for Binary and Categorical Models

This function computes odds ratios, percentage changes, and confidence
intervals from fitted binary and categorical regression models. It
standardizes statistical inference outputs and highlights significant
predictors for rapid interpretation. It is a *one-line*, *one-argument*
code!

## Usage

``` r
odds_summary(model)
```

## Arguments

- model:

  An `R` object of estimates from models covered. For now only `glm`,
  `betareg`, `mlogit`, `multimon`, `mvProbit` and `polr` models are
  covered.

## Value

A `list` or a `data.frame` depending on which model. The model must
converged otherwise there will be no any return and an error is thrown
up

## Examples

``` r
library(Dyn4cast)
library(tidyverse)

counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
ddc <- data.frame(treatment, outcome, counts) # showing data
glm.D93 <- glm(counts ~ ., data = ddc, family = poisson())
odds_summary(glm.D93)
#> Waiting for profiling to be done...
#>               Variables           Coefficient         Std Error
#> (Intercept) (Intercept)      3.04452243772342 0.170898651504024
#> treatment2   treatment2 -1.63256614336998e-17 0.199999997948297
#> treatment3   treatment3 -2.02944179046695e-16 0.199999998490874
#> outcome2       outcome2    -0.454255272277596 0.202170756683482
#> outcome3       outcome3    -0.292987124681474 0.192742343532216
#> 6                                                              
#>                           t value              p value
#> (Intercept)      17.8147832702574 5.42676746190795e-71
#> treatment2  -8.16283080058842e-17                    1
#> treatment3  -1.01472090289019e-15    0.999999999999999
#> outcome2        -2.24688911358618   0.0246471146278086
#> outcome3        -1.52009734504708    0.128486511787877
#> 6                                                     
#>                                                      Coef Sig        Odds_ratio
#> (Intercept)                                          3.045***                21
#> treatment2                                                  0                 1
#> treatment3                                                  0                 1
#> outcome2                                              -0.454* 0.634920634920635
#> outcome3                                               -0.293 0.746031746031746
#> 6           + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001                  
#>                                 % Odds Sig          CI_lower          CI_upper
#> (Intercept)                  2000    21***  14.8176865102766  28.9785474580355
#> treatment2                      0        1 0.674856752244523  1.48179594658285
#> treatment3  -2.22044604925031e-14        1 0.674856752244533  1.48179594658283
#> outcome2        -36.5079365079365   0.635* 0.424135695597257 0.939358202591257
#> outcome3        -25.3968253968254    0.746  0.50896827248296  1.08593448574167
#> 6                                                                             
```
