
test_that("quicksummary works", {
  library(tidyverse)
  test1 <- quicksummary(x = Quicksummary, Type = 2)
  Test_01 <- test1$Summary
  Test_02 <- test1$Means
  x <- select(linearsystems, 1:6)
  test2 <- quicksummary(x = x, Type = 1)
  Test_03 <- test2$Summary
  Test_04 <- test2$Means

  expect_identical(test1$Summary, Test_01)
  expect_identical(test1$Means, Test_02)
  expect_identical(test2$Summary, Test_03)
  expect_identical(test2$Means, Test_04)
})
