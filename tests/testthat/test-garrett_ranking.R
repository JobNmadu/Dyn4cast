test_that("garrett_ranking works", {
  library(readr)
  garrett_data <- data.frame(garrett_data)
  ranking <- c("Serious constraint", "Constraint",
               "Not certain it is a constraint", "Not a constraint",
               "Not a serious constraint")

  test1 <- garrett_ranking(garrett_data, 5, ranking)
  test2 <- garrett_ranking(garrett_data, 5)
  test3 <- garrett_ranking(garrett_data, 8)
  test4 <- garrett_ranking(garrett_data, 4)

  expect_identical(test1, garrett_ranking(garrett_data, 5, ranking))
  expect_identical(test2, garrett_ranking(garrett_data, 5))
  expect_identical(test3, garrett_ranking(garrett_data, 8))
  expect_identical(test4, garrett_ranking(garrett_data, 4))
}
)
