# Dyn4cast 11.11.26-delta 2025-05-03

`quicksummary` now have additional output of five types of mean, which can be useful when working on various modelling tasks.

Some editing on the various functions to make them more readable.

# Dyn4cast 11.11.26-kappa 2025-04-22

More efficient estimation procedure and better logic in selecting the option to estimate. Some options will not output because of complexity.

Some editing on the various functions to make them more readable.

# Dyn4cast 11.11.26-beta 2025-03-03

Huge improvements and new additions to the package. A lot of editorial corrections observed in the codes have been carried out. Tree new functions, `mdpi`, `plot_mdpi` and `gender` are successfully tested and now operational. The functions provides for awesome resources for simple, precise and empirical policy making processes and procedures.

The functions are for computing and visualizing multidimensional poverty indices. There is also more clarity of the difference between `sex` and `gender`.

# Dyn4cast 11.11.26-alpha 2024-10-14

roxygen 2 misleading internal keyword demobilised to index all the help pages.

modelsummary's datasummary_df insistence on tinytable object for some summary data addressed, The datasummary_df was demobilised.

# Dyn4cast 11.11.24 2024-07-28

treatment_model introduced for propensity score matching and treatment effects

# Dyn4cast 11.11.24-kappa 2024-07-02

PRESS suspended in `MlMetrics`

minor bugs discovered in the garret ranking function modules fixed.

# Dyn4cast 11.11.24-kappa 2024-06-24

minor bugs fixed probably due to changes in dependencies following new R release.

# Dyn4cast 11.11.24-kappa 2023-10-22

`timeDate` dependency in `quicksummary` is removed.

# Dyn4cast 11.11.24-beta 2023-09-29

Huge improvements and new additions to the package. A lot of editorial corrections observed in the codes have been carried out. Two new functions, `Model_factors` and `garrett_ranking` are successfully tested and now operational. The two functions provides for gaining deeper insights into the meaning behind Likert-type variables collected from respondents. Garrett ranking provides the ranks of the observations of the variables based on the level of seriousness attached to it by the respondents. On the other hand, Model factors determines and retrieve the latent factors inherent in such data which now becomes continuous data. The factors or data frame retrieved from the variables can be used in other analysis like regression and machine learning. 

The two functions are part of factor analysis, essentially, exploratory factor analysis (EFA), used to unravel the underlying structure of the observed variables. The analysis also helps to reduce the complex structure by determining a smaller number of latent factors that sufficiently represent the variation in the observed variables. With EFA, no prior knowledge or hypothesis about the number or nature of the factors is assumed. These are great tools to help tell the story behind your data. The data used for `Model_factors` is prepared using `fa.parallel` and `fa` functions in the `psych` package.

# Dyn4cast 11.11.24-alpha 2023-09-25

The **Data** argument for `DynamicForecast` function has been modified. Rather than the previous argument were a data frame, containing the two required  vectors, was supplied, now data argument is in three vectors `date, series, x`. The `x` argument is new but optional and would now allow additional data apart from the two mandatory ones to be added to the model. The `origin` argument which was previously embedded in `...` has now being defaulted to **1970-01-01**. Two objects have been added to the results, i.e., `table of estimates` and the `plots of the fitted values`. The function, which was the first in the package has now be promoted to `stable`.

# Dyn4cast 11.11.23

The new version is coming with a huge improvement. The number of functional forms of the linear regression has increased from seven to 14 based on the Taylor Series expansion of the Box-Cox transformations. In addition, two new plots are added to the suite i.e. fitted and effects plots. Also, Tables of marginal effects are now produced for fast inference from the estimated coefficients. This new version also comes with 47 metrics for assessing model performance and carry out model diagnostics. This new version also comes with the capability of making prediction about each of the functional forms.

There are two new functions coming along with this new version. `corplot` for plotting the correlation matrix of the variables included in the model. `estimate_plot` is for plotting the estimated coefficients in their order of significance in the estimated model. In addition to a new plot, `visual means` it has become lots easier to understand the nature of the distribution of the variables, digest and make inference about the model.

Even with this huge improvements, the principle of one line of code is maintained. It is easy and can be implemented with less headaches once the data has been cleaned. The structure is `Linearsystems(y, x, mod, limit, Test)`. The argument `Test` is optional, if omitted, the prediction is made with `NULL` data.

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
