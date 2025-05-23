test_that("estimate_plot works", {
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
  test3 <- estimate_plot(lm.D9, 2)
  lm.D90 <- lm(weight ~ group - 1, data = eData1)
  test4 <- estimate_plot(lm.D90, 2)
  Model   <- lm(states ~ bs(sequence, knots = c(30, 115)), data = Data)
  test1 <- estimate_plot(Model, 6)
  y <- linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  x1 <- names(x)
  dda <- cbind(Tcost = y, x = x)
  names(dda) <- c("Tcost", x1)
  ddb <- lm(Tcost ~ ., data = dda)
  test2 <- estimate_plot(ddb, 7)

  expect_identical(test1, estimate_plot(Model, 6))
  expect_identical(test2, estimate_plot(ddb, 7))
  expect_identical(test3, estimate_plot(lm.D9, 2))
  expect_identical(test4, estimate_plot(lm.D90, 2))
})
