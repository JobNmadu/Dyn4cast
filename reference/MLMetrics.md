# Collection of Machine Learning Model Metrics for Easy Reference

This function estimates over 40 Metrics for assessing the quality of
Machine Learning Models. The purpose is to provide a wrapper which
brings all the metrics on the table and makes it easier to use them to
select a model.

## Usage

``` r
MLMetrics(Observed, yvalue, modeli, K, Name, Form, kutuf, TTy)
```

## Arguments

- Observed:

  The Observed data in a data frame format

- yvalue:

  The Response variable of the estimated Model

- modeli:

  The Estimated Model (*Model* = a + bx)

- K:

  The number of variables in the estimated Model to consider

- Name:

  The Name of the Models that need to be specified. They are *ARIMA*,
  *Values* if the model computes the fitted value without estimation
  like *Essembles*, *SMOOTH* (smooth.spline), *Logit*, Ensembles based
  on weight - *EssemWet*, *QUADRATIC* polynomial, *SPLINE* polynomial.

- Form:

  Form of the Model Estimated (LM, ALM, GLM, N-LM, ARDL)

- kutuf:

  Cutoff for the Estimated values (defaults to 0.5 if not specified)

- TTy:

  Type of response variable (Numeric or Response - like *binary*)

## Value

A list with the following components:

- `Absolute Error`:

  of the Model.

- `Absolute Percent Error`:

  of the Model.

- `Accuracy`:

  of the Model.

- `Adjusted R Square`:

  of the Model.

- `` `Akaike's` Information Criterion AIC ``:

  of the Model.

- `Area under the ROC curve (AUC)`:

  of the Model.

- `Average Precision at k`:

  of the Model.

- `Bias`:

  of the Model.

- `Brier score`:

  of the Model.

- `Classification Error`:

  of the Model.

- `F1 Score`:

  of the Model.

- `fScore`:

  of the Model.

- `GINI Coefficient`:

  of the Model.

- `kappa statistic`:

  of the Model.

- `Log Loss`:

  of the Model.

- `` `Mallow's` cp ``:

  of the Model.

- `Matthews Correlation Coefficient`:

  of the Model.

- `Mean Log Loss`:

  of the Model.

- `Mean Absolute Error`:

  of the Model.

- `Mean Absolute Percent Error`:

  of the Model.

- `Mean Average Precision at k`:

  of the Model.

- `Mean Absolute Scaled Error`:

  of the Model.

- `Median Absolute Error`:

  of the Model.

- `Mean Squared Error`:

  of the Model.

- `Mean Squared Log Error`:

  of the Model.

- `Model turning point error`:

  of the Model.

- `Negative Predictive Value`:

  of the Model.

- `Percent Bias`:

  of the Model.

- `Positive Predictive Value`:

  of the Model.

- `Precision`:

  of the Model.

- `Predictive Residual Sum of Squares`:

  of the Model.

- `R Square`:

  of the Model.

- `Relative Absolute Error`:

  of the Model.

- `Recall`:

  of the Model.

- `Root Mean Squared Error`:

  of the Model.

- `Root Mean Squared Log Error`:

  of the Model.

- `Root Relative Squared Error`:

  of the Model.

- `Relative Squared Error`:

  of the Model.

- `` `Schwarz's` Bayesian criterion BIC ``:

  of the Model.

- `Sensitivity`:

  of the Model.

- `specificity`:

  of the Model.

- `Squared Error`:

  of the Model.

- `Squared Log Error`:

  of the Model.

- `Symmetric Mean Absolute Percentage Error`:

  of the Model.

- `Sum of Squared Errors`:

  of the Model.

- `True negative rate`:

  of the Model.

- `True positive rate`:

  of the Model.

## Examples

``` r
library(splines)
library(readr)
Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
MLMetrics(Observed = Data, yvalue = Data$states, modeli = Model, K = 2,
 Name = "Linear", Form = "LM", kutuf = 0, TTy = "Number")
#> Warning: NaNs produced
#> Warning: actual should be a list of vectors. Converting to a list.
#> Warning: predicted should be a list of vectors. Converting to a list.
#> $`Absolute Error`
#> [1] 460
#> 
#> $`Absolute Percent Error`
#> [1] 51
#> 
#> $Accuracy
#> [1] 0
#> 
#> $`Adjusted R Square`
#> [1] 0.77
#> 
#> $`Akaike's Information Criterion AIC`
#> [1] 1000
#> 
#> $`Area under the ROC curve (AUC)`
#> [1] 0
#> 
#> $`Average Precision at k`
#> [1] 0
#> 
#> $Bias
#> [1] 2.4e-17
#> 
#> $`Brier score`
#> [1] 8
#> 
#> $`Classification Error`
#> [1] 1
#> 
#> $`F1 Score`
#> [1] 0
#> 
#> $fScore
#> [1] 0
#> 
#> $`GINI Coefficient`
#> [1] 0.8
#> 
#> $`kappa statistic`
#> [1] 0
#> 
#> $`Log Loss`
#> [1] Inf
#> 
#> $`Mallow's cp`
#> [1] 3
#> 
#> $`Matthews Correlation Coefficient`
#> [1] 0
#> 
#> $`Mean Log Loss`
#> [1] -480
#> 
#> $`Mean Absolute Error`
#> [1] 2.3
#> 
#> $`Mean Absolute Percent Error`
#> [1] 0.25
#> 
#> $`Mean Average Precision at k`
#> [1] 0
#> 
#> $`Mean Absolute Scaled Error`
#> [1] 0.74
#> 
#> $`Median Absolute Error`
#> [1] 1.9
#> 
#> $`Mean Squared Error`
#> [1] 8.4
#> 
#> $`Mean Squared Log Error`
#> [1] 0.072
#> 
#> $`Model turning point error`
#> [1] 110
#> 
#> $`Negative Predictive Value`
#> [1] 0
#> 
#> $`Percent Bias`
#> [1] -0.1
#> 
#> $`Positive Predictive Value`
#> [1] 0
#> 
#> $Precision
#> [1] 1
#> 
#> $`Predictive Residual Sum of Squares`
#> [1] 0
#> 
#> $`R Square`
#> [1] 0.78
#> 
#> $`Relative Absolute Error`
#> [1] 0.47
#> 
#> $Recall
#> [1] 1
#> 
#> $`Root Mean Squared Error`
#> [1] 2.9
#> 
#> $`Root Mean Squared Log Error`
#> [1] 0.27
#> 
#> $`Root Relative Squared Error`
#> [1] 0.47
#> 
#> $`Relative Squared Error`
#> [1] 0.22
#> 
#> $`Schwarz's Bayesian criterion BIC`
#> [1] 1000
#> 
#> $Sensitivity
#> [1] 0
#> 
#> $specificity
#> [1] 0
#> 
#> $`Squared Error`
#> [1] 1700
#> 
#> $`Squared Log Error`
#> [1] 14
#> 
#> $`Symmetric Mean Absolute Percentage Error`
#> [1] 0.21
#> 
#> $`Sum of Squared Errors`
#> [1] 1700
#> 
#> $`True negative rate`
#> [1] 0
#> 
#> $`True positive rate`
#> [1] 0
#> 
```
