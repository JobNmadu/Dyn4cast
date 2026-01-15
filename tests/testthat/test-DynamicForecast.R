
test_that("DynamicForecast works", {
  library(forecast)
  library(readr)
  DDD <- as.data.frame(read_csv("data/day_14.csv"))
  DDD$Date <- as.Date(DDD$Date)
  BREAKS <- c(70, 131, 173, 228, 274)
  dyrima <- auto.arima( DDD$Case)

  test_model <- DynamicForecast(date = DDD$Date, series = DDD$Case,
                                BREAKS = BREAKS, dyrima = dyrima,
                                MaximumDate = "2021-02-08", x100 = 0,
                                Trend = "Day", Length = 0, Type = "Integer",
                                origin = "2020-02-29")

  Test_01 <- test_model[["Fitted plot"]]
  Test_02 <- test_model[["Spline without knots"]][["coefficients"]]
  Test_03 <- test_model[["Spline with knots"]][["coefficients"]]
  Test_04 <- test_model[["Smooth Spline"]][["tol"]]
  Test_05 <- test_model[["ARIMA"]]
  Test_06 <- test_model[["Quadratic"]][["coefficients"]]
  Test_07 <- test_model[["Ensembled with equal weight"]]
  Test_08 <- test_model[["Ensembled based on summed weight"]][["coefficients"]]
  Test_09 <- test_model[["Ensembled based on weight of fit"]]
  Test_10 <- test_model[["Constrained Forecast"]]
  Test_11 <- test_model[["RMSE"]]
  Test_12 <- test_model[["Date"]]

  expect_identical(test_model[["Fitted plot"]], Test_01)
  expect_identical(test_model[["Spline without knots"]][["coefficients"]], Test_02)
  expect_identical(test_model[["Spline with knots"]][["coefficients"]], Test_03)
  expect_identical(test_model[["Smooth Spline"]][["tol"]], Test_04)
  expect_identical(test_model[["ARIMA"]], Test_05)
  expect_identical(test_model[["Quadratic"]][["coefficients"]], Test_06)
  expect_identical(test_model[["Ensembled with equal weight"]], Test_07)
  expect_identical(test_model[["Ensembled based on summed weight"]][["coefficients"]], Test_08)
  expect_identical(test_model[["Ensembled based on weight of fit"]], Test_09)
  expect_identical(test_model[["Constrained Forecast"]], Test_10)
  expect_identical(test_model[["RMSE"]], Test_11)
  expect_identical(test_model[["Date"]], Test_12)
}
)
