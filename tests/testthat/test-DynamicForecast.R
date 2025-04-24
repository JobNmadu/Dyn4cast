test_that("DynamicForecast works", {
  DDD <- as.data.frame(readr::read_csv("data/day_14.csv"))
  DDD$Date <- as.Date(DDD$Date)
  BREAKS = c(70, 131, 173, 228, 274)

  test_model <- DynamicForecast(date = DDD$Date, series = DDD$Case,
                                BREAKS = BREAKS,
                                MaximumDate = "2021-02-08", Trend = "Day", Length = 0,
                                Type = "Integer", origin = "2020-02-29")

  y = linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  test_model1 <- Linearsystems(y, x, 6, 7)

  Test_01 <- test_model$Plot
  Test_02 <- test_model[["Spline without knots"]][["coefficients"]]
  Test_03 <- test_model[["Spline with knots"]][["coefficients"]]
  Test_04 <- test_model[["Smooth Spline"]][["tol"]]
  Test_05 <- test_model[["ARIMA"]][["coef"]]
  Test_06 <- test_model[["Quadratic"]][["coefficients"]]
  Test_07 <- test_model[["Ensembled with equal weight"]]
  Test_08 <- test_model[["Ensembled based on summed weight"]][["coefficients"]]
  Test_09 <- test_model[["Ensembled based on weight of fit"]]
  Test_10 <- test_model[["Forecast"]][["Case"]]
  Test_11 <- test_model[["RMSE"]]
  Test_12 <- test_model[["Date"]]
  Test_13 <- test_model1[["Significant plot of Cubic root"]]
  Test_14 <- test_model1[["Model Table"]]
  Test_15 <- test_model1[["Machine Learning Metrics"]]
  Test_16 <- test_model1[["Correlation plot"]]$plot()
  Test_17 <- test_model1[["Summary of numeric variables"]]
  Test_18 <- test_model1[["Fitted plots wide format"]]
  Test_19 <- test_model1[["Visual means of the numeric variable"]]
  Test_20 <- test_model1[["Mixed-power model"]]

  expect_identical(test_model$Plot, Test_01)
  expect_identical(test_model[["Spline without knots"]][["coefficients"]], Test_02)
  expect_identical(test_model[["Spline with knots"]][["coefficients"]], Test_03)
  expect_identical(test_model[["Smooth Spline"]][["tol"]], Test_04)
  expect_identical(test_model[["ARIMA"]][["coef"]], Test_05)
  expect_identical(test_model[["Quadratic"]][["coefficients"]], Test_06)
  expect_identical(test_model[["Ensembled with equal weight"]], Test_07)
  expect_identical(test_model[["Ensembled based on summed weight"]][["coefficients"]], Test_08)
  expect_identical(test_model[["Ensembled based on weight of fit"]], Test_09)
  expect_identical(test_model[["Forecast"]][["Case"]], Test_10)
  expect_identical(test_model[["RMSE"]], Test_11)
  expect_identical(test_model[["Date"]], Test_12)
  expect_identical(test_model1[["Significant plot of Cubic root"]], Test_13)
  expect_identical(test_model1[["Model Table"]], Test_14)
  expect_identical(test_model1[["Machine Learning Metrics"]], Test_15)
  expect_identical(test_model1[["Correlation plot"]]$plot(), Test_16)
  expect_identical(test_model1[["Summary of numeric variables"]], Test_17)
  expect_identical(test_model1[["Fitted plots wide format"]], Test_18)
  expect_identical(test_model1[["Visual means of the numeric variable"]], Test_19)
  expect_identical(test_model1[["Mixed-power model"]], Test_20)
}
)
