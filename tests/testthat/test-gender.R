test_that("gender works", {
  df <- data.frame(Age = c(49, 30, 44, 37, 29, 56, 28, 26, 33, 45, 45, 19,
                           32, 22, 19, 28, 28, 36, 56, 34),
                   Sex = c("male", "female", "female", "male", "male",
                           "male", "female", "female", "Prefer not to say",
                           "male", "male", "female", "female", "male",
                           "Non-binary/third gender", "male", "female",
                           "female", "male", "male"))

  library(Dyn4cast)
  test <- gender(df)
  expect_identical(test, gender(df))
}
)
