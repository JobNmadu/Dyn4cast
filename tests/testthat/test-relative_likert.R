test_that("relative_likert works", {
  library(readr)
  garrett_data <- data.frame(garrett_data)
  test1 <- relative_likert(garrett_data, Ranks = 3, Option = "sccore")
  test2 <- relative_likert(garrett_data, Ranks = 5, Option = "sccore")
  test3 <- relative_likert(garrett_data, Ranks = 7, Option = "sccore")
  test4 <- relative_likert(garrett_data, Ranks = 9, Option = "sccore")

  test5 <- relative_likert(Quicksummary, Ranks = 5, Option = "sccore")

  library(tidyverse)
  data_l <- garrett_data %>%
    pivot_longer(cols = everything()) %>%
    mutate(value = case_when(value == 5 ~ "Serious constraint",
                             value == 4 ~ "Constraint",
                             value == 3 ~ "Not certain it is a constraint",
                             value == 2 ~ "Not a constraint",
                             value == 1 ~ "Not a serious constraint",
                             .default = "None")) %>%
    group_by(name) %>%
    mutate(row = row_number()) %>%
    pivot_wider(names_from = name, values_from = value) %>%
    select(-row) %>%
    unnest(cols = everything())

  ranking <- c("Serious constraint", "Constraint",
               "Not certain it is a constraint", "Not a constraint",
               "Not a serious constraint")

  test6 <- relative_likert(data_l, Likert = ranking)

  expect_identical(relative_likert(garrett_data, Ranks = 3, Option = "sccore"), test1)
  expect_identical(relative_likert(garrett_data, Ranks = 5, Option = "sccore"), test2)
  expect_identical(relative_likert(garrett_data, Ranks = 7, Option = "sccore"), test3)
  expect_identical(relative_likert(garrett_data, Ranks = 9, Option = "sccore"), test4)
  expect_identical(relative_likert(Quicksummary, Ranks = 5, Option = "sccore"), test5)
  expect_identical(relative_likert(data_l, Likert = ranking), test6)
})
