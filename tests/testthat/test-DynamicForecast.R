test_that("DynamicForecast works", {
  DDD <- as.data.frame(read.csv("data/Data.csv"))
  test_model <- DynamicForecast(Data = DDD, Title = "14 days lag forecast")
  test_plot <- test_model$Plot

  expect_identical(test_plot, "figures/plot_14")
})
