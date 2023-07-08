
# Dyn4cast 11.11.23 2023-07-07

The new version is coming with a huge improvement. The number of functional forms of the linear regression has increased from seven to 14 based on the Taylor Series expansion of the Box-Cox transformations. In addition, two new plots are added to the suite i.e. fitted and effects plots. Also, Tables of marginal effects are now produced for fast inference from the estimated coefficients. This new version also comes with 47 metrics for assessing model performance and carry out model diagnostics. This new version also comes with the capability of making prediction about each of the functional forms.

There are two new functions coming along with this new version. `corplot` for plotting the correlation matrix of the variables included in the model. `estimate_plot` is for plotting the estimated coefficients in their order of significance in the estimated model. In addition to a new plot, `visual means` it has become lots easier to understand the nature of the distribution of the variables, digest and make inference about the model.

Even with this huge improvements, the principle of one line of code is maintained. It is easy and can been implemented with less headaches once the data has been cleaned. The structure is `Linearsystems(y, x, mod, limit, Test)`. The argument `Test` is optional, if omitted, the prediction is made with `NULL` data.

As always, comments, questions and suggestions are welcome.

# Dyn4cast 11.11.10-kappa 2023-04-02

`quicksummary` now returns a data frame rather a `kable` Table.

The internal argument `digits` in `Percent` is removed due to observed exaggeration of the final rates

Final formatting of data frames in the `Linearsystems` package was removed. Now, the estimates can display in the `console`.

# Dyn4cast 11.11.10-beta 2023-03-12

A new function `data_transform` added and is working perfectly. The addition of this function only makes the package more versatile for `Machine Learning` operations and estimations.

# Dyn4cast 11.11.10-alpha 2022-08-20

A new function `formattedcut` added and is working perfectly. The addition of this function is providing more clarity to the package. Quite a number of editorial fixes were undertaken and the codes are working superbly.

# Dyn4cast 11.11.1.9000 2022-04-25

Mallow's Cp successfully added and documentation improved sufficiently. The codes are working better now and the check results are encouraging.

# Dyn4cast 11.11.01.9000 2022-03-15

Documentation improved and new functions added to the library. Editorial fixes were made.

# Dyn4cast 11.11.00 2021-02-12

A small experimental library with only one function, i.e., _DynamicForecast_ and is perfectly working although the check result looks funny.

* Added a `NEWS.md` file to track changes to the package.
