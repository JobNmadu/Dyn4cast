test_that("formattedcut works", {
  library(tidyverse)
  DD <- rnorm(100000)
  test1 <- formattedcut(DD, 12, FALSE)
  test2 <- cut(DD, 12)
  test3 <- formattedcut(test2, 12, TRUE)
  test4 <- as.data.frame(test3 %>%
                           group_by(`Lower class`, `Upper class`,
                                    `Class interval`) %>%
                           tally())

  expect_identical(test1, formattedcut(DD, 12, FALSE))
  expect_identical(test2, cut(DD, 12))
  expect_identical(test3, formattedcut(test2, 12, TRUE))
  expect_identical(test4, as.data.frame(test3 %>%
                                          group_by(`Lower class`, `Upper class`,
                                                   `Class interval`) %>%
                                          tally()))

})
