
test_that("Percent works", {
  library(Dyn4cast)
  Data1 <- c(1.2, 0.5, 0.103, 7, 0.1501)
  test1 <- Percent(Data = Data1, Type = "Frame")
  Data2 <- 1.2
  test2 <- Percent(Data = Data2, Type = "Value")
  df <- data.frame(c(A = 2320, 5760, 4800, 2600, 5700, 7800, 3000, 6300, 2400,
                     10000, 2220, 3740),
                   B = c(0, 0, 1620, 3600, 1200, 1200, 1200, 4250, 14000, 10000,
                         1850, 1850),
                   C = c(3000, 3000, 7800, 5400, 3900, 7800, 1950, 2400, 2400,
                         7000, 1850, 1850),
                   D = c(2900, 5760, 3750, 5400, 4095, 3150, 2080, 7800, 1920,
                         1200, 5000, 1950),
                   E = c(2900, 2030, 0, 5400, 5760, 1800, 2000, 1950, 1850,
                         3600, 5200, 5760),
                   F = c(2800, 5760, 1820, 4340, 7500, 2400, 2300, 1680, 1850,
                         0, 2800, 8000),
                   G = c(5760, 4600, 13000, 7800, 6270, 1200, 1440, 8000, 1200,
                         2025, 4800, 2600),
                   H = c(2100, 5760, 8250, 3900, 1800, 1200, 4800, 1800, 7800,
                         2035, 8000, 3000))
  test3 <- Percent(Data = df, Type = "Frame")

  expect_identical(test1, Percent(Data = Data1, Type = "Frame"))
  expect_identical(test2, Percent(Data = Data2, Type = "Value"))
  expect_identical(test3, Percent(Data = df, Type = "Frame"))
}
)
