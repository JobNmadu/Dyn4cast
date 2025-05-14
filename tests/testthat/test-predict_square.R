test_that("predict_square works", {
  library(Dyn4cast)
  library(readr)
  library(splines)
  Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
  test <- suppressWarnings(MLMetrics(Observed = Data, yvalue = Data$states,
                                     modeli = Model,
                                     K = 2, Name = "Linear", Form = "LM",
                                     kutuf = 0,
                                     TTy = "Number"))
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  weight <- c(ctl, trt)
  Data1 <- data.frame(weight, group)
  lm.D9 <- lm(weight ~ group, data = Data1)
  lm.D90 <- lm(weight ~ group - 1, data = Data1)
  test1 <- suppressWarnings(MLMetrics(Observed = Data1, yvalue = Data1$weight,
                                      modeli = lm.D9,
                                      K = 2, Name = "Linear", Form = "LM",
                                      kutuf = 0,
                                      TTy = "Number"))
  test_01 <- test$`Predictive Residual Sum of Squares`
  test_02 <- test1$`Predictive Residual Sum of Squares`

  expect_identical(test_01, test$`Predictive Residual Sum of Squares`)
  expect_identical(test_02, test1$`Predictive Residual Sum of Squares`)
})
