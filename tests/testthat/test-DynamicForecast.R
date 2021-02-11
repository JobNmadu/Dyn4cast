test_that("DynamicForecast works", {
  #DDD <- as.data.frame(read.csv("data/day_14.csv"))
  DDD <-readxl::read_excel("data/day_14.xlsx")
  DDD$Date <- as.Date(DDD$Date)

  test_model <- DynamicForecast(Data = DDD,
                                BREAKS = c(70, 131, 173, 228, 274),
                                Date = "2021-02-08")
  expect_identical(test_model, DynamicForecast(Data = DDD, BREAKS = c(70, 131, 173, 228, 274), Date = "2021-02-08"))
})
