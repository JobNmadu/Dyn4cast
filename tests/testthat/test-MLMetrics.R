test_that("MLMetrics works", {
  library(splines)
  Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
  test <- MLMetrics(Observed = Data, yvalue = Data$states, Model = Model,
                    K = 2, Name = "Linear", Form = "LM", kutuf = 0,
                    TTy = "Number")

  expect_identical(test, MLMetrics(Observed = Data, yvalue = Data$states,
                                   Model = Model, K = 2, Name = "Linear",
                                   Form = "LM", kutuf = 0, TTy = "Number"))
})
