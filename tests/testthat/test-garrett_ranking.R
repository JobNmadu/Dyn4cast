test_that("garrett_ranking works", {
  garrett_data <- data.frame(garrett_data)
  ranking <- c("Serious constraint", "Constraint",
               "Not certain it is a constraint", "Not a constraint",
               "Not a serious constraint")
  test_model1 <- garrett_ranking(garrett_data, 5, ranking)

  Test_13 <- test_model1[["Garrett value"]]
  Test_14 <- test_model1[["Garrett ranked data"]]
  Test_15 <- test_model1[["Data mean table"]]

  expect_identical(test_model1[["Garrett value"]], Test_13)
  expect_identical(test_model1[["Garrett ranked data"]], Test_14)
  expect_identical(test_model1[["Data mean table"]], Test_15)
}
)
