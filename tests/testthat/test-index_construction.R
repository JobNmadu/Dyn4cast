test_that("index_construction works", {
  library(readr)
  garrett_data <- data.frame(garrett_data)

  test <- index_construction(garrett_data)

  expect_identical(test, index_construction(garrett_data))
})
