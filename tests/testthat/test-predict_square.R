test_that("predict_square works", {
  library(Dyn4cast)
  library(readr)
  library(splines)
  library(tidyverse)
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2, 10, 20, labels = c("Ctl","Trt"))
  wate <- c(ctl, trt)
  eData1 <- data.frame(weight = wate, group = group)
  lm.D9 <- lm(weight ~ group, data = eData1)
  lm.D90 <- lm(weight ~ group - 1, data = eData1)

  Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
  test <- suppressWarnings(MLMetrics(Observed = Data, yvalue = Data$states,
                                     modeli = Model,
                                     K = 2, Name = "Linear", Form = "LM",
                                     kutuf = 0,
                                     TTy = "Number"))

  test1 <- suppressWarnings(MLMetrics(Observed = eData1, yvalue = eData1$weight,
                                      modeli = lm.D9,
                                      K = 2, Name = "Linear", Form = "LM",
                                      kutuf = 0,
                                      TTy = "Number"))
  test0 <- suppressWarnings(MLMetrics(Observed = eData1, yvalue = eData1$weight,
                                      modeli = lm.D90,
                                      K = 2, Name = "Linear", Form = "LM",
                                      kutuf = 0,
                                      TTy = "Number"))
  test_01 <- test$`Predictive Residual Sum of Squares`
  test_02 <- test1$`Predictive Residual Sum of Squares`
  test5 <- test0$`Predictive Residual Sum of Squares`

  y <- linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  x1 <- names(x)
  dda <- cbind(Tcost = y, x = x)
  names(dda) <- c("Tcost", x1)
  ddb <- lm(Tcost ~ ., data = dda)
  test7 <- suppressWarnings(MLMetrics(Observed = dda, yvalue = dda$Tcost,
                                      modeli = ddb,
                                      K = 2, Name = "Linear", Form = "LM",
                                      kutuf = 0,
                                      TTy = "Number"))
  test8 <- test7$`Predictive Residual Sum of Squares`

  expect_identical(test_01, test$`Predictive Residual Sum of Squares`)
  expect_identical(test_02, test1$`Predictive Residual Sum of Squares`)
  expect_identical(test5, test0$`Predictive Residual Sum of Squares`)
  expect_identical(test8, test7$`Predictive Residual Sum of Squares`)
})
