# Dynamic Forecast of Five Models and their Ensembles

The function estimates, predict and forecast time series data with
models, and also make subset forecasts within the length of the entire
trend of the data. However, the forecast is constrained to lower and
upper 80% and 95% forecasts of the of the data for `integer series` in
line with Hyndman & Athanasopoulos (2021). The recognized models are lm,
smooth spline, polynomial splines with or without knots, quadratic
polynomial, and ARIMA. The robust output include the models' estimates,
time-varying forecasts and plots based on themes from ggplot. The main
attraction of this function is the use of the newly introduced *equal
number of trend to forecast from the model*. The function takes
`daily, monthly and yearly data sets for now`.

## Usage

``` r
DynamicForecast(Data, date, series, dyrima, Trend, Type, MaximumDate, x = 0,
x100 = 0, BREAKS = 0, ORIGIN = NULL, origin = "1970-01-01", Length = 0, ...)
```

## Arguments

- date:

  A vector containing the dates for which the data is collected. Must be
  the same length with `series`. The date must be in 'YYYY-MM-DD'. If
  the data is monthly series, the recognized date format is the last day
  of the month of the dataset e.g. 2021-02-28. If the data is a yearly
  series, the recognized date format is the last day of the year of the
  data set e.g. 2020-12-31. There is no format for Quarterly data for
  now.

- x:

  **\[deprecated\]**

- series:

  A vector containing observations for estimation and forecasting. Must
  be the same length with `date`.

- dyrima:

  **ARIMA** object of the `series` obtained from `auto.rima` in forecast
  package.

- x100:

  vector of optional dataset that is to be added to the model for
  forecasting. The modeling and forecasting is still done if not
  provided. Must be the same length with `series`.

- BREAKS:

  A vector of numbers indicating points of breaks for estimation of the
  spline models.

- MaximumDate:

  **\[deprecated\]**. The date indicating the maximum date (last date)
  in the data frame, meaning that forecasting starts the next date
  following it. The date must be a recognized date format. Note that for
  forecasting, the date origin is set to 1970-01-01.

- Trend:

  The type of trend. There are three options **Day, Month and Year**.

- Type:

  The type of response variable. There are two options **Continuous and
  Integer**. For integer variable, the forecasts are constrained between
  the minimum and maximum value of the response variable.

- Length:

  The length for which the forecast would be made. If not given, would
  default to the length of the dataset i.e. sample size.

- origin:

  default date origin which is **1970-01-01** used to position the date
  of data so that the forecasts are in tandem with the period of the
  observations.

- ORIGIN:

  date origin of the dataset and if different from **origin** must be in
  the format `"YYYY-MM-DD"`. This is used to position the date of the
  data to properly `date` the forecasts.

- Data:

  **\[deprecated\]**. Now broken into three vectors `date`, `series` and
  `x100`.

- ...:

  Additional arguments that may be passed to the function.

## Value

A list with the following components:

- `Spline without knots`:

  The estimated spline model without the breaks (knots).

- `Spline with knots`:

  The estimated spline model with the breaks (knots).

- `Smooth Spline`:

  The smooth spline estimates.

- `ARIMA`:

  Estimated Auto Regressive Integrated Moving Average model.

- `Quadratic`:

  The estimated quadratic polynomial model.

- `Ensembled with equal weight`:

  Estimated Ensemble model with equal weight given to each of the
  models. To get this, the fitted values of each of the models is
  divided by the number of models and summed together.

- `Ensembled based on weight`:

  Estimated Ensemble model based on weight of each model. To do this,
  the fitted values of each model served as independent variable and
  regressed against the trend with interaction among the variables.

- `Ensembled based on summed weight`:

  Estimated Ensemble model based on summed weight of each model. To do
  this, the fitted values of each model served as independent variable
  and is regressed against the trend.

- `Ensembled based on weight of fit`:

  Estimated Ensemble model. The fit of each model is measured by the
  rmse.

- `Unconstrained Forecast`:

  The forecast if the response variable is continuous. The number of
  forecasts is equivalent to the length of the dataset (equal days
  forecast).

- `Constrained Forecast`:

  The forecast if the response variable is integer. The number of
  forecasts is equivalent to the length of the dataset (equal days
  forecast).

- `RMSE`:

  Root Mean Square Error (rmse) for each forecast.

- `Unconstrained forecast Plot`:

  The combined plots of the unconstrained forecasts using ggplot.

- `Constrained forecast Plot`:

  The combined plots of the constrained forecasts using ggplot.

- `Date`:

  This is the date range for the forecast.

- `Fitted plot`:

  This is the plot of the fitted models.

- `Estimated coefficients`:

  This is the estimated coefficients of the various models in the
  forecast.

## Examples

``` r
 library(readr)
 library(forecast)
 COVID19$Date <- zoo::as.Date(COVID19$Date, format = '%m/%d/%Y')
  #The date is formatted to R format
 LEN <- length(COVID19$Case)
 Dss <- seq(COVID19$Date[1], by = "day", length.out = LEN)
  #data length for forecast
 ORIGIN = "2020-02-29"
 lastdayfo21 <- Dss[length(Dss)] # The maximum length # uncomment to run
 Data <- COVID19[COVID19$Date <= lastdayfo21 - 28, ]
 # desired length of forecast
 BREAKS <- c(70, 131, 173, 228, 274) # The default breaks for the data
 dyrima <- auto.arima(Data$Case)
 DynamicForecast(date = Data$Date, series = Data$Case, dyrima = dyrima,
 BREAKS = BREAKS, Trend = "Day", Length = 0, Type = "Integer", x100 = 0)
#> Warning: Coercing LHS to a list
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> Warning: NaNs produced
#> $`Spline without knots`
#> 
#> Call:
#> stats::lm(formula = series ~ splines::bs(Series, knots = NULL))
#> 
#> Coefficients:
#>                        (Intercept)  splines::bs(Series, knots = NULL)1  
#>                             -396.5                              2212.6  
#> splines::bs(Series, knots = NULL)2  splines::bs(Series, knots = NULL)3  
#>                             -912.1                              1621.2  
#> 
#> 
#> $`Spline with knots`
#> 
#> Call:
#> stats::lm(formula = series ~ splines::bs(Series, knots = BREAKS))
#> 
#> Coefficients:
#>                          (Intercept)  splines::bs(Series, knots = BREAKS)1  
#>                                190.1                                -634.7  
#> splines::bs(Series, knots = BREAKS)2  splines::bs(Series, knots = BREAKS)3  
#>                               1742.0                               -1477.3  
#> splines::bs(Series, knots = BREAKS)4  
#>                               1199.9  
#> 
#> 
#> $`Smooth Spline`
#> Call:
#> stats::smooth.spline(x = Series, y = series)
#> 
#> Smoothing Parameter  spar= 0.4790742  lambda= 5.206569e-06 (12 iterations)
#> Equivalent Degrees of Freedom (Df): 32.10921
#> Penalized Criterion (RSS): 7579996
#> GCV: 29266
#> 
#> $ARIMA
#> Time Series:
#> Start = 1 
#> End = 320 
#> Frequency = 1 
#>   [1]    0.99900000    0.84528948    0.23242234    0.19426066    0.23530389
#>   [6]    0.18708366    0.20645071    0.09392626   -0.05029039   -0.12751880
#>  [11]   -0.09508467    0.46504846    0.12239000    0.14548861    0.20729516
#>  [16]    0.08546458    0.10439869    0.05184082   -0.02306526   -0.05671365
#>  [21]    2.82802596    2.35418907    5.57152616    3.52213383    7.02699363
#>  [26]    8.18974689    7.33454460   13.04482986   18.93331641   13.42789704
#>  [31]   16.73706721   11.56629015   27.70845053   17.36076948   22.02026457
#>  [36]   15.13295123   15.08353256   11.95120672   14.01732590   29.72495102
#>  [41]   19.16495321   15.76480194   19.10329764   13.67793324   18.35549181
#>  [46]   14.57291651   16.88749994   18.96793027   19.23755469   23.57464546
#>  [51]   35.49894393   59.90850008   44.76916782   84.95613619   80.04915644
#>  [56]   86.76356517  101.98862342   90.91460794   99.02262416   88.23105898
#>  [61]  145.59796785  150.60421503  154.52368562  187.64195367  191.43146550
#>  [66]  190.09834215  243.00601377  201.72881635  208.39385336  288.70681193
#>  [71]  282.49818424  230.78804155  259.64196433  273.87659408  238.73752450
#>  [76]  256.08056296  228.28610073  226.67871401  159.36620385  232.94066634
#>  [81]  213.03084814  237.13647050  300.86417459  315.43530480  265.57250383
#>  [86]  258.29599042  275.34920247  240.01517161  281.66646401  347.85779215
#>  [91]  245.48719089  331.73071905  416.77976221  322.31902239  389.26287681
#>  [96]  332.11617117  372.70080701  394.25839547  354.21033868  363.78953368
#> [101]  274.24946690  289.41558654  480.02191952  396.30190817  551.69228445
#> [106]  570.97712445  497.71786106  497.40000084  575.40905043  539.34952603
#> [111]  566.93276948  631.60522920  573.29293903  593.39316916  532.77037276
#> [116]  666.17839729  592.38582758  646.66889702  605.16286479  579.12335338
#> [121]  640.84172837  529.23394052  608.99965448  643.49167033  732.00676152
#> [126]  654.41231894  522.99023142  570.87053287  535.40644280  576.17524301
#> [131]  578.62219750  531.20590822  519.26387006  524.08878948  562.19156012
#> [136]  538.12598299  570.61603561  533.91485307  620.63549290  613.17827696
#> [141]  596.69908573  616.18007719  551.48003872  562.38882391  591.65510906
#> [146]  583.20969192  612.75219503  591.09678438  494.90732111  536.71880960
#> [151]  584.38416520  583.19361950  503.82932047  528.73980185  506.11203767
#> [156]  448.04403350  403.71674706  359.04013504  331.27550007  382.91747471
#> [161]  336.00606824  378.33311297  401.86602629  407.37011537  370.39093072
#> [166]  468.16921168  417.38872010  388.14616651  367.81361127  335.42499378
#> [171]  325.15498520  382.09614995  385.37022332  468.01781911  431.46645076
#> [176]  372.36965178  525.45899741  415.79972151  414.09932639  383.96878188
#> [181]  295.73338550  301.13900684  205.51646870  225.91423502  178.55935487
#> [186]  169.58991049  229.57139296  208.77196905  160.02360708  165.03499862
#> [191]  157.20925222  128.14111654  162.80347023  227.95674232  167.94033105
#> [196]  177.67765611  184.48135296  170.87919338  152.33212245  172.71690972
#> [201]  135.56846047  122.04732303  111.30930055  142.30424489  145.73504847
#> [206]  121.73954336  188.51488855  187.46584108  151.39917761  152.80497228
#> [211]  172.55221920  129.17259956  129.23021921  145.14873964  167.85613232
#> [216]  183.92759868  162.22400941  144.61878956  154.72792301  106.85464470
#> [221]  135.04631390  134.57630232  133.47075972  107.50797845  120.07927499
#> [226]  109.62956728  140.46851998  159.86280830  189.49693411  174.33170093
#> [231]  225.36258131  200.35053496  158.62453022  184.17536367  169.39294337
#> [236]  134.09652740   96.92174162  110.59602125   92.57339537   49.76618208
#> [241]   71.70005198  108.33012862  113.00172536  134.78096219  131.13037961
#> [246]  129.98770066  134.02812607  124.46015053  121.21974798  220.03979828
#> [251]  165.28185388  162.68323775  188.32528871   99.69249400  229.18205838
#> [256]  158.67648312  170.14396194  195.24157908  173.32460200  153.43000269
#> [261]  131.91940098  155.57072589  162.68808740  163.57955745  200.97807628
#> [266]  150.09933218  140.54555316  199.91071148  165.61015497  130.35327723
#> [271]  182.88419865  178.77739929  153.16700120  192.38042601  128.38426664
#> [276]  117.62437134  165.75952493  227.75615894  157.62623637  246.33450167
#> [281]  242.45601936  235.86261688  294.21846852  353.84868769  457.79175055
#> [286]  442.39559357  768.28265853 1588.96845988  745.26562871  753.13203313
#> [291]  814.94237712  939.24280630 1092.11260196 1011.68845314  649.60974152
#> [296]  560.70516352  483.86504471  611.77867970 1104.43324249 1152.92374466
#> [301] 1008.14941332  675.06258902  634.58241723  749.67656294  705.70389089
#> [306]  975.96296794 1042.91314029  869.68620342  811.23913958  561.45880246
#> [311]  798.60501240 1107.98586199 1298.37506527 1467.19527431 1368.61156068
#> [316] 1327.56147075 1426.11011520 1289.77484996 1483.67880299 1503.24004691
#> 
#> $Quadratic
#> 
#> Call:
#> stats::lm(formula = series ~ Series + I(Series^2))
#> 
#> Coefficients:
#> (Intercept)       Series  I(Series^2)  
#>   1.479e+02    1.504e-01    4.727e-03  
#> 
#> 
#> $`Ensembled with equal weight`
#> Time Series:
#> Start = 1 
#> End = 320 
#> Frequency = 1 
#>            1            2            3            4            5            6 
#>  -11.3989579  -12.6647237  -13.7978782  -14.5958942  -15.1621183  -15.5344936 
#>            7            8            9           10           11           12 
#>  -15.6859007  -15.6603546  -15.4414091  -15.0127902  -14.3690701  -13.4283285 
#>           13           14           15           16           17           18 
#>  -12.4779949  -11.2655119   -9.8567243   -8.2963564   -6.5205721   -4.5760214 
#>           19           20           21           22           23           24 
#>   -2.4578630   -0.1592426    2.8858808    5.4118496    8.8181602   11.3020224 
#>           25           26           27           28           29           30 
#>   15.0162336   18.3688704   21.4070989   25.8286905   30.3398404   32.6200564 
#>           31           32           33           34           35           36 
#>   36.7075364   39.1411015   45.8819752   47.3734777   51.9193157   54.2147032 
#>           37           38           39           40           41           42 
#>   57.9430188   61.1258291   65.4222870   72.5231764   74.4549730   77.9359912 
#>           43           44           45           46           47           48 
#>   82.9209414   86.3431780   91.9895347   96.1557826  101.7569108  107.5107792 
#>           49           50           51           52           53           54 
#>  113.0813273  119.6221586  127.8098860  138.5954630  141.5478807  155.6359053 
#>           55           56           57           58           59           60 
#>  160.7736920  168.3079848  177.6421981  181.8454208  190.0203033  194.4585354 
#>           61           62           63           64           65           66 
#>  212.4551362  219.8033232  226.7094642  239.1959015  245.5268953  250.5316152 
#>           67           68           69           70           71           72 
#>  266.0752364  262.4494099  267.9935194  287.7469601  289.6251508  281.9856768 
#>           73           74           75           76           77           78 
#>  290.2462877  295.5524678  291.0455804  297.1665539  294.4413748  297.1054310 
#>           79           80           81           82           83           84 
#>  286.7308970  304.5915760  303.7876813  311.7983909  327.7242670  333.7965786 
#>           85           86           87           88           89           90 
#>  326.9779189  328.6981626  335.3229593  331.4904017  343.0476208  359.4648882 
#>           91           92           93           94           95           96 
#>  342.0279863  362.0650139  381.5554949  364.9710558  380.6776181  371.7471481 
#>           97           98           99          100          101          102 
#>  382.6668281  390.1957336  385.8931274  391.9374873  378.4756379  386.1441211 
#>          103          104          105          106          107          108 
#>  428.8624937  416.4255203  451.3314367  458.5992016  447.0660584  449.9165353 
#>          109          110          111          112          113          114 
#>  468.1650422  463.2322606  470.5817281  484.9099651  474.2308995  478.8656741 
#>          115          116          117          118          119          120 
#>  467.0807369  493.9333823  479.2466377  489.9962028  481.2923679  475.2986347 
#>          121          122          123          124          125          126 
#>  486.5022082  462.7443489  477.0184207  482.0298565  497.6682443  479.9557807 
#>          127          128          129          130          131          132 
#>  451.4715028  458.9825616  450.0863285  456.7673196  456.1692305  446.0056216 
#>          133          134          135          136          137          138 
#>  443.2346207  443.9725950  451.3975648  446.3681942  452.5951981  444.8857169 
#>          139          140          141          142          143          144 
#>  461.6812508  459.3713469  454.9144130  457.3051086  442.5311681  442.5620655 
#>          145          146          147          148          149          150 
#>  445.9411443  441.4436987  444.1951487  436.3292674  413.1461276  417.1402016 
#>          151          152          153          154          155          156 
#>  421.9502247  416.7225664  395.7091867  395.6241545  386.3564343  370.5389326 
#>          157          158          159          160          161          162 
#>  358.0680883  346.1478755  338.2237178  346.6724254  335.7510644  342.8670651 
#>          163          164          165          166          167          168 
#>  346.2884162  346.1444494  337.5459862  355.9501219  344.7315008  337.9545933 
#>          169          170          171          172          173          174 
#>  333.1028930  325.8480838  322.8539495  332.9606305  331.7852121  345.7213333 
#>          175          176          177          178          179          180 
#>  334.9020759  318.7539794  344.3812546  316.9698230  310.8939758  299.1236767 
#>          181          182          183          184          185          186 
#>  275.9465442  271.8334687  247.9471206  247.7729617  234.5492234  229.4756069 
#>          187          188          189          190          191          192 
#>  238.6377847  232.0639227  220.2954367  219.6349011  216.6670295  209.6006773 
#>          193          194          195          196          197          198 
#>  215.3292425  227.1392980  213.8421269  214.3952378  214.3516662  210.3368961 
#>          199          200          201          202          203          204 
#>  205.5414973  208.7530086  200.6690468  197.5008558  195.0127235  200.9162459 
#>          205          206          207          208          209          210 
#>  201.2887414  196.1549746  209.1666896  208.6151003  201.0636235  201.0102851 
#>          211          212          213          214          215          216 
#>  204.6275696  195.6151968  195.2780459  198.0967743  202.2683710  205.1231378 
#>          217          218          219          220          221          222 
#>  200.4622510  196.7443591  198.7903630  189.5387261  195.7971885  196.5988051 
#>          223          224          225          226          227          228 
#>  197.5191522  193.6520961  197.6060738  196.9877004  204.5263729  209.5303485 
#>          229          230          231          232          233          234 
#>  216.2186355  213.5742456  223.8143100  218.5397452  209.7878218  214.5588643 
#>          235          236          237          238          239          240 
#>  211.5107976  204.6985303  197.9180228  201.7685570  199.7345704  193.1771179 
#>          241          242          243          244          245          246 
#>  199.9799246  210.0985073  214.1100244  221.7216966  224.3364645  227.5180488 
#>          247          248          249          250          251          252 
#>  231.7941019  233.3902512  236.2428857  259.4390919  251.8058321  254.5017863 
#>          253          254          255          256          257          258 
#>  262.7685787  248.1280430  277.0828764  266.0324152  271.3929670  279.5179499 
#>          259          260          261          262          263          264 
#>  278.3002510  277.5657327  276.5917219  284.7354412  289.6547368  293.3897964 
#>          265          266          267          268          269          270 
#>  304.4611690  297.9074838  299.7308013  315.5530616  312.9605542  310.5986742 
#>          271          272          273          274          275          276 
#>  326.3198364  331.3614663  332.9486738  348.5641676  344.8047974  353.1084915 
#>          277          278          279          280          281          282 
#>  374.7420722  400.7940902  402.0029979  436.4580027  453.7359608  471.5354186 
#>          283          284          285          286          287          288 
#>  503.0789765  535.1551709  575.2302046  589.2659210  668.4291491  843.8656718 
#>          289          290          291          292          293          294 
#>  684.5307801  694.3226320  714.0780302  745.7818857  782.9192597  773.3607881 
#>          295          296          297          298          299          300 
#>  707.6711685  697.0152828  689.2196223  722.8327933  829.8662208  848.5658690 
#>          301          302          303          304          305          306 
#>  829.1417881  772.6884855  775.7891676  811.5105791  817.2812117  887.6515257 
#>          307          308          309          310          311          312 
#>  918.8876439  903.3891201  911.9163096  883.0144827  951.9989884 1035.1767707 
#>          313          314          315          316          317          318 
#> 1093.4433832 1145.6228474 1142.6823890 1149.9424089 1184.1562352 1170.7973009 
#>          319          320 
#> 1223.2676078 1241.0281464 
#> 
#> $`Ensembled based on weight`
#> 
#> Call:
#> stats::lm(formula = Series ~ Without.knots * With.knots * Smooth * 
#>     Quadratic * ARIMA)
#> 
#> Coefficients:
#>                                     (Intercept)  
#>                                      -1.041e+02  
#>                                   Without.knots  
#>                                       2.955e-01  
#>                                      With.knots  
#>                                      -1.379e-01  
#>                                          Smooth  
#>                                      -7.582e-02  
#>                                       Quadratic  
#>                                       8.501e-01  
#>                                           ARIMA  
#>                                      -3.154e-02  
#>                        Without.knots:With.knots  
#>                                      -1.730e-05  
#>                            Without.knots:Smooth  
#>                                       3.781e-04  
#>                               With.knots:Smooth  
#>                                      -2.171e-04  
#>                         Without.knots:Quadratic  
#>                                      -1.569e-03  
#>                            With.knots:Quadratic  
#>                                       1.052e-03  
#>                                Smooth:Quadratic  
#>                                       2.033e-04  
#>                             Without.knots:ARIMA  
#>                                       1.379e-04  
#>                                With.knots:ARIMA  
#>                                      -6.588e-05  
#>                                    Smooth:ARIMA  
#>                                       4.391e-04  
#>                                 Quadratic:ARIMA  
#>                                       8.397e-05  
#>                 Without.knots:With.knots:Smooth  
#>                                       5.938e-09  
#>              Without.knots:With.knots:Quadratic  
#>                                       1.307e-08  
#>                  Without.knots:Smooth:Quadratic  
#>                                      -1.249e-06  
#>                     With.knots:Smooth:Quadratic  
#>                                       8.810e-07  
#>                  Without.knots:With.knots:ARIMA  
#>                                      -2.892e-09  
#>                      Without.knots:Smooth:ARIMA  
#>                                      -2.085e-06  
#>                         With.knots:Smooth:ARIMA  
#>                                       1.123e-06  
#>                   Without.knots:Quadratic:ARIMA  
#>                                      -4.872e-07  
#>                      With.knots:Quadratic:ARIMA  
#>                                       3.261e-07  
#>                          Smooth:Quadratic:ARIMA  
#>                                      -1.140e-06  
#>       Without.knots:With.knots:Smooth:Quadratic  
#>                                      -2.182e-11  
#>           Without.knots:With.knots:Smooth:ARIMA  
#>                                       7.294e-11  
#>        Without.knots:With.knots:Quadratic:ARIMA  
#>                                       8.872e-13  
#>            Without.knots:Smooth:Quadratic:ARIMA  
#>                                       6.714e-09  
#>               With.knots:Smooth:Quadratic:ARIMA  
#>                                      -4.587e-09  
#> Without.knots:With.knots:Smooth:Quadratic:ARIMA  
#>                                      -4.477e-14  
#> 
#> 
#> $`Ensembled based on summed weight`
#> 
#> Call:
#> stats::lm(formula = Series ~ Without.knots + With.knots + Smooth + 
#>     Quadratic + ARIMA)
#> 
#> Coefficients:
#>   (Intercept)  Without.knots     With.knots         Smooth      Quadratic  
#>    -29.792396       0.001736       0.001500      -0.028442       0.568967  
#>         ARIMA  
#>      0.026470  
#> 
#> 
#> $`Ensembled based on weight of fit`
#> Time Series:
#> Start = 1 
#> End = 320 
#> Frequency = 1 
#>            1            2            3            4            5            6 
#>   -6.0247547   -6.3547327   -6.5850396   -6.5737599   -6.3937495   -6.0719406 
#>            7            8            9           10           11           12 
#>   -5.5895815   -4.9778759   -4.2256078   -3.3216903   -2.2626748   -0.9914928 
#>           13           14           15           16           17           18 
#>    0.2895600    1.7573324    3.3650593    5.0807941    6.9488668    8.9362281 
#>           19           20           21           22           23           24 
#>   11.0460341   13.2829096   16.0443395   18.4417233   21.4578305   23.8269686 
#>           25           26           27           28           29           30 
#>   27.0609031   30.0420945   32.8042197   36.5400203   40.3415168   42.5791831 
#>           31           32           33           34           35           36 
#>   46.0881330   48.4376580   53.8127498   55.5023082   59.3360127   61.5881275 
#>           37           38           39           40           41           42 
#>   64.8439983   67.7135312   71.3609714   76.9728191   78.9493186   82.0051487 
#>           43           44           45           46           47           48 
#>   86.1054842   89.0943541   93.6293003   97.1082804  101.5772934  106.1365207 
#>           49           50           51           52           53           54 
#>  110.5507480  115.6308235  121.8530071  129.8862730  132.4073799  142.7352298 
#>           55           56           57           58           59           60 
#>  146.7659476  152.4659445  159.4141459  162.7414360  168.8383440  172.2976848 
#>           61           62           63           64           65           66 
#>  185.2701095  190.7645989  195.9501374  205.0570004  209.8462030  213.7087874 
#>           67           68           69           70           71           72 
#>  224.9758931  222.7884865  227.0501694  241.3052728  243.0281387  238.0774623 
#>           73           74           75           76           77           78 
#>  244.2877583  248.4088547  245.6192692  250.2675407  248.6768835  250.8448545 
#>           79           80           81           82           83           84 
#>  243.8355423  256.6326888  256.3070679  262.1549102  273.5461842  278.0054637 
#>           85           86           87           88           89           90 
#>  273.3990680  274.7727132  279.5736400  277.0167408  285.2527105  296.8907957 
#>           91           92           93           94           95           96 
#>  284.7536531  298.9318337  312.7328903  301.2019791  312.3313682  306.1379805 
#>           97           98           99          100          101          102 
#>  313.8512584  319.1455996  316.0897304  320.2609506  310.7044016  315.9616801 
#>          103          104          105          106          107          108 
#>  345.8210957  336.9605017  361.3619896  366.3733156  358.1919090  360.1132067 
#>          109          110          111          112          113          114 
#>  372.8536536  369.3310323  374.4515214  384.4909855  376.9885431  380.2535544 
#>          115          116          117          118          119          120 
#>  371.9983424  390.8752649  380.5832617  388.1550686  382.0786255  377.9219521 
#>          121          122          123          124          125          126 
#>  385.8557198  369.2527964  379.3628489  382.9756292  394.0550508  381.7188804 
#>          127          128          129          130          131          132 
#>  361.8135977  367.1683817  360.9805054  365.7039705  365.2867906  358.1223432 
#>          133          134          135          136          137          138 
#>  356.1250318  356.5771351  361.7181848  358.1114446  362.4079328  356.9212443 
#>          139          140          141          142          143          144 
#>  368.6487079  366.9748419  363.8119295  365.4759529  355.1057061  355.1482724 
#>          145          146          147          148          149          150 
#>  357.5598788  354.4599011  356.4698881  351.0469725  334.8933358  337.8477904 
#>          151          152          153          154          155          156 
#>  341.3964601  337.9129952  323.3542535  323.4852975  317.1477099  306.1769046 
#>          157          158          159          160          161          162 
#>  297.5181290  289.2069419  283.6632076  289.5853295  281.8858889  286.8405546 
#>          163          164          165          166          167          168 
#>  289.1981741  289.0512618  282.9669366  295.8413034  287.9122388  283.0957152 
#>          169          170          171          172          173          174 
#>  279.6243748  274.4678108  272.3160852  279.3878626  278.5758144  288.4262142 
#>          175          176          177          178          179          180 
#>  280.9560506  269.7999513  288.0227038  269.0391366  265.0571913  257.0820382 
#>          181          182          183          184          185          186 
#>  241.0887979  238.4653349  221.9358139  222.0292887  212.9338583  209.5370230 
#>          187          188          189          190          191          192 
#>  216.1138215  211.6211149  203.4624211  203.0873974  201.0826704  196.1977446 
#>          193          194          195          196          197          198 
#>  200.3010542  208.6830635  199.4481603  199.9525784  200.0467430  197.3538213 
#>          199          200          201          202          203          204 
#>  194.1080697  196.4791555  190.9145569  188.7986445  187.1617150  191.4236869 
#>          205          206          207          208          209          210 
#>  191.8128486  188.3469672  197.6327172  197.4050839  192.2726909  192.4156385 
#>          211          212          213          214          215          216 
#>  195.1466918  189.0211044  188.9989639  191.2052809  194.3735371  196.6284188 
#>          217          218          219          220          221          222 
#>  193.6157396  191.2696905  192.9693199  186.7300149  191.3753458  192.1847815 
#>          223          224          225          226          227          228 
#>  193.0753338  190.6062363  193.6352204  193.4655916  199.0439841  202.8724939 
#>          229          230          231          232          233          234 
#>  207.9211939  206.4552018  214.0735054  210.8325881  205.1738577  209.0216255 
#>          235          236          237          238          239          240 
#>  207.3792009  203.0882060  198.8098472  201.9832870  201.0126013  196.8550214 
#>          241          242          243          244          245          246 
#>  202.0692982  209.6050696  212.8520345  218.6331034  220.9176561  223.6140111 
#>          247          248          249          250          251          252 
#>  227.0933322  228.7066333  231.2212973  248.0434349  243.2445783  245.7241004 
#>          253          254          255          256          257          258 
#>  252.1394672  242.4931395  263.4792229  256.3956284  260.8542637  267.2718852 
#>          259          260          261          262          263          264 
#>  267.1462103  267.3755753  267.4524343  273.9471302  278.1939147  281.6268790 
#>          265          266          267          268          269          270 
#>  290.2306409  286.4788699  288.6241460  300.6076293  299.6634525  298.8772705 
#>          271          272          273          274          275          276 
#>  310.7779364  315.1620336  317.0903774  328.8250357  326.8990487  333.3775822 
#>          277          278          279          280          281          282 
#>  349.1417965  367.9286199  369.1960822  393.7387321  406.1605208  418.9062788 
#>          283          284          285          286          287          288 
#>  441.2801957  464.0361049  492.4889178  502.8214664  559.1080591  683.1901238 
#>          289          290          291          292          293          294 
#>  572.3563430  580.3781304  595.4751261  619.0238001  646.4327985  641.0838898 
#>          295          296          297          298          299          300 
#>  596.3372144  590.2356383  586.1422694  611.1238270  687.6566932  702.1625305 
#>          301          302          303          304          305          306 
#>  689.8949983  651.6162286  655.1177773  681.4584995  686.6809527  737.1815820 
#>          307          308          309          310          311          312 
#>  760.1373443  750.2260868  757.1475599  737.7657492  787.1148702  846.4771779 
#>          313          314          315          316          317          318 
#>  888.4497761  926.2921077  925.5671412  932.1192235  957.6904907  949.9283267 
#>          319          320 
#>  988.4351256 1002.5929666 
#> 
#> $`Unconstrained Forecast`
#>                                                             DDf91    Case
#> Linear                                                     Linear  277722
#> Semilog                                                   Semilog  179540
#> Growth                                                     Growth    2721
#> Without Knots                                       Without knots 1606956
#> Smooth spline                                       Smooth Spline 2082046
#> With Knots                                             With knots  991316
#> Polynomial                                   Quadratic Polynomial  380508
#> Lower ARIMA                                           Lower ARIMA -119303
#> Upper ARIMA                                           Upper ARIMA 1074901
#> Ensembled with equal weight           Essembled with equal weight 1403498
#> Ensembled based on weight               Essembled based on weight  154152
#> Ensembled based on summed weight Essembled based on summed weight  211499
#> Ensembled based on weight of fit Essembled based on weight of fit 1099890
#> 
#> $RMSE
#>                                  DDf91                              RMSE_f91
#> Linear                           "Linear"                           "307.33"
#> Semilog                          "Semilog"                          "310.78"
#> Growrh                           "Growth"                           "475.6" 
#> Without knots                    "Without knots"                    "221.72"
#> Smooth Spline                    "Smooth Spline"                    "185.4" 
#> With knots                       "With knots"                       "153.91"
#> Polynomial                       "Quadratic Polynomial"             "305.21"
#> Lower ARIMA                      "Lower ARIMA"                      "169.16"
#> Upper ARIMA                      "Upper ARIMA"                      "169.16"
#> Ensembled with equal weight      "Essembled with equal weight"      "178.28"
#> Ensembled based on weight        "Essembled based on weight"        "358.35"
#> Ensembled based on weight        "Essembled based on summed weight" "357.6" 
#> Ensembled based on weight of fit "Essembled based on weight of fit" "193.29"
#> 
#> $`Unconstrained forecast Plot`

#> 
#> $Date
#> [1] "Jan 14, 21 - Nov 29, 21"
#> 
#> $`Constrained Forecast`
#>                                                                     Model
#> Linear                                                             Linear
#> Semilog                                                           Semilog
#> Growth                                                             Growth
#> Smooth spline 80%                                       Without knots 80%
#> Smooth spline 95%                                       Without knots 95%
#> Without knots 80%                                       Smooth Spline 80%
#> Without knots 95%                                       Smooth Spline 95%
#> With knots 80%                                             With knots 80%
#> With knots 95%                                             With knots 95%
#> Polynomial 80%                                   Quadratic Polynomial 80%
#> Polynomial 95%                                   Quadratic Polynomial 95%
#> ARIMA 80%                                                       ARIMA 80%
#> ARIMA 95%                                                       ARIMA 95%
#> Essembled with equal weight 80%           Essembled with equal weight 80%
#> Essembled with equal weight 95%           Essembled with equal weight 95%
#> Essembled based on weight 80%               Essembled based on weight 80%
#> Essembled based on weight 95%               Essembled based on weight 95%
#> Essembled based on summed weight 80% Essembled based on summed weight 80%
#> Essembled based on summed weight 95% Essembled based on summed weight 95%
#> Essembled based on weight of fit 80% Essembled based on weight of fit 80%
#> Essembled based on weight of fit 95% Essembled based on weight of fit 95%
#>                                      Confirmed cases
#> Linear                                        277722
#> Semilog                                       179540
#> Growth                                          2721
#> Smooth spline 80%                               4723
#> Smooth spline 95%                             875749
#> Without knots 80%                              15471
#> Without knots 95%                             871333
#> With knots 80%                                 25306
#> With knots 95%                                861881
#> Polynomial 80%                                406348
#> Polynomial 95%                                430240
#> ARIMA 80%                                       2356
#> ARIMA 95%                                     876588
#> Essembled with equal weight 80%                17358
#> Essembled with equal weight 95%               867253
#> Essembled based on weight 80%                  75091
#> Essembled based on weight 95%                 168687
#> Essembled based on summed weight 80%          220605
#> Essembled based on summed weight 95%          357114
#> Essembled based on weight of fit 80%            8815
#> Essembled based on weight of fit 95%          868036
#> 
#> $`Constrained forecast Plot`

#> 
#> $`Fitted plot`

#> 
#> $`Estimated coefficients`
#> 
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       | Linear without knots | Linear with knots | ARIMA            | Quadratic polynomial | Linear    | Semilog     | Growth   |
#> +=======================================+======================+===================+==================+======================+===========+=============+==========+
#> | (Intercept)                           | -396.533***          | 190.135**         |                  | 147.912**            | 66.474+   | -393.533*** | 3.323*** |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       | (49.313)             | (65.159)          |                  | (51.749)             | (34.549)  | (88.025)    | (0.155)  |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = NULL)1   | 2212.594***          |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       | (142.576)            |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = NULL)2   | -912.054***          |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       | (90.701)             |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = NULL)3   | 1621.249***          |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       | (77.788)             |                   |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = BREAKS)1 |                      | -634.663***       |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      | (99.062)          |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = BREAKS)2 |                      | 1741.962***       |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      | (90.134)          |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = BREAKS)3 |                      | -1477.319***      |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      | (110.314)         |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | splines = bs(Series, knots = BREAKS)4 |                      | 1199.904***       |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      | (74.529)          |                  |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ar1                                   |                      |                   | 0.620***         |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.112)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ar2                                   |                      |                   | -0.642***        |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.083)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ar3                                   |                      |                   | -0.272***        |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.075)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ar4                                   |                      |                   | -0.134+          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.068)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ar5                                   |                      |                   | -0.170**         |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.065)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ma1                                   |                      |                   | -1.141***        |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.096)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | ma2                                   |                      |                   | 0.791***         |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   | (0.100)          |                      |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | Series                                |                      |                   |                  | 0.150                | 1.668***  |             | 0.011*** |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   |                  | (0.744)              | (0.187)   |             | (0.001)  |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | I(Series^2)                           |                      |                   |                  | 0.005*               |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   |                  | (0.002)              |           |             |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | log(Series)                           |                      |                   |                  |                      |           | 152.232***  |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> |                                       |                      |                   |                  |                      |           | (18.050)    |          |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | Num.Obs.                              | 320                  | 320               | 319              | 320                  | 320       | 320         | 320      |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | R2                                    | 0.584                | 0.709             |                  | 0.212                | 0.201     | 0.183       | 0.342    |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | R2 Adj.                               | 0.580                | 0.705             |                  | 0.207                | 0.198     | 0.180       | 0.340    |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | AIC                                   | 4375.0               | 4262.5            | 4197.1           | 4577.6               | 4580.0    | 4587.1      | 1121.6   |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | BIC                                   | 4393.9               | 4285.2            | 4227.3           | 4592.6               | 4591.3    | 4598.4      | 1132.9   |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | Log.Lik.                              | -2182.518            | -2125.272         |                  | -2284.776            | -2286.996 | -2290.570   | -557.783 |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | F                                     | 147.900              | 192.015           |                  | 42.606               | 79.919    | 71.130      | 164.982  |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | RMSE                                  | 221.72               | 185.40            | 169.16           | 305.21               | 307.33    | 310.78      | 1.38     |
#> +---------------------------------------+----------------------+-------------------+------------------+----------------------+-----------+-------------+----------+
#> | x                                     |                      |                   | 0.75932820959595 |                      |           |             |          |
#> +=======================================+======================+===================+==================+======================+===========+=============+==========+
#> | + p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001                                                                                                               |
#> +=======================================+======================+===================+==================+======================+===========+=============+==========+ 
#> 
```
