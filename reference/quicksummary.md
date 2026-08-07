# Quick Formatted Summary of Machine Learning Data

There is increasing need to make user-friendly and production ready
Tables for machine learning data. This function is simplified and quick
summary; and the output is a formatted table. This is very handy for
those who do not have the time to write codes for user-friendly
summaries.

## Usage

``` r
quicksummary(
  x,
  Type,
  Cut = deprecated(),
  Up = deprecated(),
  Down = deprecated(),
  Dig = 2,
  ci = 0.95
)
```

## Arguments

- x:

  The data to be summarised. Only numeric data is allowed.

- Type:

  The type of data to be summarised. There are two options here 1 or 2,
  1 = `Continuous` and 2 = `Likert-type`

- Cut:

  **\[deprecated\]**

- Up:

  **\[deprecated\]**

- Down:

  **\[deprecated\]**

- Dig:

  Number of significant digits which is defaults to 2.

- ci:

  Confidence interval which is defaults to 0.95.

## Value

The function returns formatted tables of the Quick summary

- `Summary`:

  List of two `data.frames`

## Examples

``` r
library(tidyverse)
# Likert-type data
quicksummary(x = Quicksummary, Type = 2)
#> $Summary
#>                  Mean   SD SE.Mean Nobs Rank
#> Likert scores 1  4.30 1.10   0.110  103    1
#> Likert scores 14 3.90 1.40   0.130  103    2
#> Likert scores 3  3.50 1.40   0.130  103    3
#> Likert scores 10 3.50 1.50   0.150  103    4
#> Likert scores 15 3.40 1.40   0.140  103    5
#> Likert scores 17 3.40 1.20   0.120  103    6
#> Likert scores 19 3.40 1.20   0.120  103    7
#> Likert scores 2  3.20 1.60   0.150  103    8
#> Likert scores 4  3.20 1.30   0.130  103    9
#> Likert scores 18 3.20 1.20   0.120  103   10
#> Likert scores 7  3.10 1.30   0.130  103   11
#> Likert scores 21 3.10 1.30   0.130  103   12
#> Likert scores 20 3.00 1.20   0.120  103   13
#> Likert scores 26 3.00 1.20   0.120  103   14
#> Likert scores 11 2.90 1.20   0.120  103   15
#> Likert scores 13 2.90 1.40   0.140  103   16
#> Likert scores 16 2.90 1.50   0.140  103   17
#> Likert scores 22 2.90 1.30   0.130  103   18
#> Likert scores 25 2.90 1.30   0.130  103   19
#> Likert scores 6  2.80 1.40   0.140  103   20
#> Likert scores 8  2.80 1.30   0.130  103   21
#> Likert scores 23 2.80 1.50   0.150  103   22
#> Likert scores 5  2.70 1.30   0.130  103   23
#> Likert scores 24 2.70 1.30   0.130  103   24
#> Likert scores 9  2.60 1.30   0.130  103   25
#> Likert scores 12 2.40 1.30   0.120  103   26
#> Likert scores 27 2.40 1.30   0.130  103   27
#> Likert scores 29 0.89 1.80   0.180  103   28
#> Likert scores 28 0.26 0.83   0.082  103   29
#> 
#> $Means
#>                  Arithmetic Geometric Quadratic Harmonic Cubic Nobs
#> Likert scores 1        4.30       4.1      4.50      3.7     1  103
#> Likert scores 2        3.20       2.7      3.60      2.2     1  103
#> Likert scores 3        3.50       3.1      3.70      2.7     1  103
#> Likert scores 4        3.20       2.8      3.40      2.5     1  103
#> Likert scores 5        2.70       2.3      3.00      2.0     1  103
#> Likert scores 6        2.80       2.4      3.10      2.0     1  103
#> Likert scores 7        3.10       2.7      3.30      2.3     1  103
#> Likert scores 8        2.80       2.5      3.10      2.1     1  103
#> Likert scores 9        2.60       2.3      2.90      2.0     1  103
#> Likert scores 10       3.50       3.0      3.80      2.5     1  103
#> Likert scores 11       2.90       2.6      3.10      2.3     1  103
#> Likert scores 12       2.40       2.1      2.70      1.8     1  103
#> Likert scores 13       2.90       2.5      3.20      2.1     1  103
#> Likert scores 14       3.90       3.5      4.10      3.0     1  103
#> Likert scores 15       3.40       3.1      3.70      2.6     1  103
#> Likert scores 16       2.90       2.6      3.30      2.2     1  103
#> Likert scores 17       3.40       3.1      3.60      2.7     1  103
#> Likert scores 18       3.20       2.9      3.50      2.5     1  103
#> Likert scores 19       3.40       3.1      3.60      2.8     1  103
#> Likert scores 20       3.00       2.7      3.20      2.4     1  103
#> Likert scores 21       3.10       2.7      3.30      2.3     1  103
#> Likert scores 22       2.90       2.6      3.20      2.2     1  103
#> Likert scores 23       2.80       2.4      3.20      2.0     1  103
#> Likert scores 24       2.70       2.4      3.00      2.0     1  103
#> Likert scores 25       2.90       2.5      3.20      2.1     1  103
#> Likert scores 26       3.00       2.7      3.30      2.4     1  103
#> Likert scores 27       2.40       0.0      2.80      0.0     1  103
#> Likert scores 28       0.26       0.0      0.86      0.0     1  103
#> Likert scores 29       0.89       0.0      2.00      0.0     1  103
#> 

# Continuous data
x <- select(linearsystems, 1:6)
quicksummary(x = x, Type = 1)
#> $Summary
#>          MKTcost     Age Experience Years spent in formal education
#> Mean      3900.0 3.8e+01      12.00                           10.00
#> SD        2800.0 1.1e+01       4.60                            5.20
#> SE.Mean    280.0 1.1e+00       0.46                            0.52
#> Min          0.0 2.0e+01       2.00                            0.00
#> Q1        1800.0 3.0e+01       8.80                            7.00
#> Median    3000.0 3.6e+01      11.00                           12.00
#> Q3        5800.0 4.5e+01      15.00                           14.00
#> Max      14000.0 6.8e+01      20.00                           20.00
#> IQR       3900.0 1.5e+01       6.20                            7.00
#> Skewness     1.2 8.3e-01       0.38                           -0.72
#> Kurtosis     1.3 7.2e-03      -0.77                           -0.42
#> Nobs       100.0 1.0e+02     100.00                          100.00
#>          Household size Years as a cooperative member
#> Mean               8.30                         10.00
#> SD                 3.60                          3.80
#> SE.Mean            0.36                          0.38
#> Min                0.00                          2.00
#> Q1                 5.00                          7.80
#> Median             8.00                         10.00
#> Q3                11.00                         12.00
#> Max               17.00                         20.00
#> IQR                6.00                          4.20
#> Skewness           0.18                          0.64
#> Kurtosis          -0.37                         -0.20
#> Nobs             100.00                        100.00
#> 
#> $Means
#>            MKTcost Age Experience Years spent in formal education
#> Arithmetic    3900  38       12.0                              10
#> Geometric        0  37       11.0                               0
#> Quadratic     4800  40       13.0                              12
#> Harmonic         0  35        9.8                               0
#> Cubic            1   1        1.0                               1
#> Nobs           100 100      100.0                             100
#>            Household size Years as a cooperative member
#> Arithmetic            8.3                          10.0
#> Geometric             0.0                           9.5
#> Quadratic             9.0                          11.0
#> Harmonic              0.0                           8.7
#> Cubic                 1.0                           1.0
#> Nobs                100.0                         100.0
#> 
```
