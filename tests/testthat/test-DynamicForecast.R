test_that("DynamicForecast works", {
  DDD <- as.data.frame(read.csv("data/Data.csv"))
  test_model <- DynamicForecast(Data = DDD, Title = "14 days lag forecast")
  ggsave(filename = "figures/test_plot.png", test_model$Plot)
  expect_identical("figures/test_plot.png", "figures/plot_14.png")
})
