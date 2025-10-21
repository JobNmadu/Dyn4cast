
test_that("MLMetrics works", {
  library(readr)
  library(splines)
  library(Dyn4cast)
  Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
  test <- suppressWarnings(MLMetrics(Observed = Data, yvalue = Data$states, modeli = Model,
                    K = 2, Name = "Linear", Form = "LM", kutuf = 0,
                    TTy = "Number"))

  expect_identical(test, suppressWarnings(MLMetrics(Observed = Data, yvalue = Data$states,
                                   modeli = Model, K = 2, Name = "Linear",
                                   Form = "LM", kutuf = 0, TTy = "Number")))
})
