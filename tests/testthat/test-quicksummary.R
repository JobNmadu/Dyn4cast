test_that("quicksummary works", {
  Up <- "Constraint"
  Down <- "Not a constraint"
  test1 <- quicksummary(x = Quicksummary, Type = 2, Cut = 2.60, Up = Up, Down = Down)
  x <- select(linearsystems, 1:6)
  test2 <- quicksummary(x = x, Type = 1)

  expect_identical(test1, quicksummary(x = Quicksummary, Type = 2,
                                       Cut = 2.60, Up = Up,
                                       Down = Down))
  expect_identical(test2,quicksummary(x = x, Type = 1))

})
