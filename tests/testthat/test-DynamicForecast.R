test_that("DynamicForecast works", {
  DDD <- as.data.frame(read.csv("data/Data.csv"))
  test_model <- DynamicForecast(Data = DDD, Title = "14 days lag forecast")
  ggsave(filename = "figures/test_plot.png", test_model$Plot)
  xlsx::write.xlsx2(test_model$Forecast, file = "doc/test_model.xlsx",
              col.names = TRUE, row.names = FALSE)
  xlsx::write.xlsx2(test_model$RMSE, file = "doc/test_rmse.xlsx",
                    col.names = TRUE, row.names = FALSE)
  expect_identical("doc/test_model.xlsx", "doc/Days_14.xlsx")
  expect_identical("doc/test_rmse.xlsx", "doc/Days_14_rmse.xlsx")
})
