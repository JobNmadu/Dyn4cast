# Computation of MallowsCp

Mallow's Cp is one of the very useful metrics and selection criteria for
machine learning algorithms (models). It is used to estimate the closest
number to the number of predictors and the intercept (approximate number
of explanatory variables) of linear and non-linear based models. The
function inherits `residuals` from the estimated model. The uniqueness
of this function compared to other procedures for computing Mallow's Cp
is that it does not require nested models for computation and it is not
limited to `lm` based models only.

## Usage

``` r
MallowsCp(model2, y, x, type, Nlevels = 0)
```

## Arguments

- model2:

  The estimated **model** from which the Mallows Cp would be computed

- y:

  The vector of the **LHS** variable of the estimated model

- x:

  The matrix of the **RHS** variable of the estimated model. Note that
  if the model adds additional factor variables into the output, then
  the number of additional factors `Nlevels` is required otherwise the
  computed Cp would be biased.

- type:

  The type of model (`LM`, `ALM`, `GLM`,`N-LM`, `nls`, `ARDL`, `SMOOTH`,
  `SPLINE`, `ARIMA`, `plm`) for which Cp would be computed broadly
  divided in to linear (`LM`, `ALM`, `GLM`, `ARDL`, `SMOOTH`, `SPLINE`,
  `ARIMA`, `plm`) and non-linear (`GLM`,`N-LM`, `nls`). The type of
  model must be specified as indicated. Supported models are `LM`,
  `ALM`, `GLM` (for binary based models), `N-LM` (not linear for models
  not clearly defined as linear or non-linear especially some of the
  essemble models that are merely **computed** not **estimated**) or
  `nls` for other non linear models, `ARDL`, `SMOOTH` for
  **smooth.spline**, `SPLINE` for bs spline models, `ARIMA` and `plm`.

- Nlevels:

  Optional number of additional variables created if the model has
  categorical variables that generates additional dummy variables during
  estimation or the number of additional variables created if the model
  involves interaction terms.

## Value

A list with the following components

- `MallowsCp`:

  of the Model.

## Examples

``` r
library(Dyn4cast)
ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
x <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
y <- c(ctl, trt)
Model <- lm(y ~ x)
Type <- "LM"
MallowsCp(model2 = Model, y = y, x = x, type = Type, Nlevels = 0)
#> [1] 2
```
