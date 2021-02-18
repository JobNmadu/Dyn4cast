
library(qpdf)
test_that("DynamicForecast works", {
  #DDD <- as.data.frame(read.csv("data/day_14.csv"))
  DDD <-readxl::read_excel("data/day_14.xlsx")
  DDD$Date <- as.Date(DDD$Date)
  BREAKS = c(70, 131, 173, 228, 274)

  test_model <- DynamicForecast(Data = DDD,
                                BREAKS = BREAKS,
                                MaximumDate = "2021-02-08", Trend = "Day")
  expect_identical(test_model, DynamicForecast(Data = DDD, BREAKS = BREAKS,
                                           MaximumDate = "2021-02-08",
                                           Trend = "Day"))
}
)
