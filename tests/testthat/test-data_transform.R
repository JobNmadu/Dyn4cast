
test_that("data_transform works", {
  library(Dyn4cast)
  library(tidyverse)

  data1 <- data_transform(Transform[, -1], 1)
  data1 <- cbind(Transform[, 1], data1)
  test1 <- data1 %>%
    pivot_longer(!X, names_to = "Factors", values_to = "Data")

  data2 <- data_transform(Transform[, -1], 2)
  data2 <- cbind(Transform[, 1], data2)
  test2 <- data2 %>%
    pivot_longer(!X, names_to = "Factors", values_to = "Data")

  data3 <- data_transform(Transform[, -1], 3)
  data3 <- cbind(Transform[, 1], data3)
  test3 <- data3 %>%
    pivot_longer(!X, names_to = "Factors", values_to = "Data")

  expect_identical(test1, data1 %>%
                     pivot_longer(!X, names_to = "Factors",
                                  values_to = "Data"))
  expect_identical(test2, data2 %>%
                     pivot_longer(!X, names_to = "Factors",
                                  values_to = "Data"))
  expect_identical(test3, data3 %>%
                     pivot_longer(!X, names_to = "Factors",
                                  values_to = "Data"))
}
)
