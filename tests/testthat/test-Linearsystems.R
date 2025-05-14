test_that("Linearsystems works", {
  library(tidyverse)
  library(Dyn4cast)
  y <- linearsystems$MKTcost # to run all the exercises, uncomment.
  x <- select(linearsystems, -MKTcost)
  test <- suppressWarnings(Linearsystems(y, x, 6, 15))
  Test01 <- test[-33]
  Test02 <- test[33]
  x1 <- sampling[, -1]
  y1 <- sampling$qOutput
  limit <- 20
  mod <-3
  Test <- NA
  test1 <- suppressWarnings(Linearsystems(y1, x1, 3, 15))
  Test03 <- test1[-11]
  Test04 <- test1[11]
  x2 <- sampling[, -1]
  y2 <- sampling$qOutput
  Data <- cbind(y2, x2)
  sampling <- sample(1 : nrow(Data), 0.8 * nrow(Data))
  train <- Data[sampling, ]
  Test  <- Data[-sampling, ]
  y2 <- train$y
  x2 <- train[, -1]
  mod <- 4
  test2 <- suppressWarnings(Linearsystems(y2, x2, 4, 15, Test))
  Test05 <- test2[-11]
  Test06 <- test2[11]

  expect_identical(test[-33],  Test01)
  expect_identical(test[33],   Test02)
  expect_identical(test1[-11], Test03)
  expect_identical(test1[11],  Test04)
  expect_identical(test2[-11], Test05)
  expect_identical(test2[11],  Test06)
}
)
