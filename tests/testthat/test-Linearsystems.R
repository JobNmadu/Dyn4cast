
test_that("Linearsystems works", {
  library(tidyverse)
  library(Dyn4cast)
  library(ggtext)

  y <- linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  test <- suppressWarnings(Linearsystems(y, x, 6, 15))
  Test01 <- test[-33]
  Test02 <- test[33]

  test3 <- suppressWarnings(Linearsystems(y, x, 5, 15))
  Test07 <- test3[-13]
  Test08 <- test3[13]

  x1 <- sampling[, -1]
  y1 <- sampling$qOutput

  mod <- 2
  test4 <- suppressWarnings(Linearsystems(y1, x1, 2, 15))
  Test09 <- test4[-17]
  Test10 <- test4[17]

  mod <- 1
  test5 <- suppressWarnings(Linearsystems(y1, x1, 1, 15))
  Test11 <- test5[-9]
  Test12 <- test5[9]
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
  expect_identical(test3[-13], Test07)
  expect_identical(test3[13],  Test08)
  expect_identical(test4[-17], Test09)
  expect_identical(test4[17],  Test10)
  expect_identical(test5[-9],  Test11)
  expect_identical(test5[9],   Test12)
}
)
