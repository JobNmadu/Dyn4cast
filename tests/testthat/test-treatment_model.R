test_that("treatment_model works", {
  library(readr)
  Treatment  <- treatments$treatment
  data  <- treatments[, c(2:3)]
  test_model <- treatment_model(Treatment, data)

  Test_13 <- test_model[["Model"]]
  Test_14 <- test_model[["Residuals"]]
  Test_15 <- test_model[["ATO plot"]]
  Test_16 <- test_model[["ATM plot"]]
  Test_17 <- test_model[["ATC plot"]]
  Test_18 <- test_model[["ATT plot"]]
  Test_19 <- test_model[["ATE plot"]]

  expect_identical(test_model[["Model"]], Test_13)
  expect_identical(test_model[["Residuals"]], Test_14)
  expect_identical(test_model[["ATO plot"]], Test_15)
  expect_identical(test_model[["ATM plot"]], Test_16)
  expect_identical(test_model[["ATC plot"]], Test_17)
  expect_identical(test_model[["ATT plot"]], Test_18)
  expect_identical(test_model[["ATE plot"]], Test_19)
})
