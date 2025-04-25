test_that("Linearsystems works", {
  y = linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  test_model1 <- Linearsystems(y, x, 6, 7)

  Test_13 <- test_model1[["Significant plot of Cubic root"]]
  Test_14 <- test_model1[["Model Table"]]
  Test_15 <- test_model1[["Machine Learning Metrics"]]
  Test_16 <- test_model1[["Correlation plot"]]$plot()
  Test_17 <- test_model1[["Summary of numeric variables"]]
  Test_18 <- test_model1[["Fitted plots wide format"]]
  Test_19 <- test_model1[["Visual means of the numeric variable"]]
  Test_20 <- test_model1[["Mixed-power model"]]

  expect_identical(test_model1[["Significant plot of Cubic root"]], Test_13)
  expect_identical(test_model1[["Model Table"]], Test_14)
  expect_identical(test_model1[["Machine Learning Metrics"]], Test_15)
  expect_identical(test_model1[["Correlation plot"]]$plot(), Test_16)
  expect_identical(test_model1[["Summary of numeric variables"]], Test_17)
  expect_identical(test_model1[["Fitted plots wide format"]], Test_18)
  expect_identical(test_model1[["Visual means of the numeric variable"]], Test_19)
  expect_identical(test_model1[["Mixed-power model"]], Test_20)
}
)
