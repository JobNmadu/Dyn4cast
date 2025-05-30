test_that("MallowsCp works", {
  ctl <- c(4.17, 5.58, 5.18, 6.11, 4.50, 4.61, 5.17, 4.53, 5.33, 5.14)
  trt <- c(4.81, 4.17, 4.41, 3.59, 5.87, 3.83, 6.03, 4.89, 4.32, 4.69)
  x <- gl(2, 10, 20, labels = c("Ctl", "Trt"))
  y <- c(ctl, trt)
  Model <- lm(y ~ x)
  Type <- "LM"
  test <- MallowsCp(model2 = Model, y = y, x = x, type = Type, Nlevels = 0)

  expect_identical(test, MallowsCp(model2 = Model, y = y, x = x, type = Type,
                                   Nlevels = 0))

})
