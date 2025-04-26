test_that("mdpi works", {
  data <- mdpi1
  dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
             d2 = c("Years.of.education", "School.attendance", "School.lag"),
             d3 = c("Cooking.Fuel", "Access.to.clean.source.of.water",
                    "Access.to.an.improve.sanatation", "Electricity",
                    "Housing.Materials", "Asset.ownership"))
  test_model <- mdpi(data, dm, plots = "t", Factor = "Region")

  Test_01 <- test_model[["MDPI_p"]]
  Test_02 <- test_model[["MDPI"]]
  Test_03 <- test_model[["dimensions"]]
  Test_04 <- test_model[["MDPI mean"]]
  Test_05 <- test_model[["MDPI SD"]]
  Test_06 <- test_model[["plots"]]$`Multidimensional poverty index`
  Test_07 <- test_model[["plots"]]$`Deprivation Score`
  Test_08 <- test_model[["plots"]]$`Adjusted incidence of poverty`
  Test_09 <- test_model[["plots"]]$`Intensity of poverty`
  Test_10 <- test_model[["plots"]]$`Average deprivation among the deprived`
  Test_11 <- test_model[["plots"]]$`Contribution of each Dimension`
  Test_12 <- test_model[["plots"]]$combined_only
  Test_13 <- test_model[["plots"]]$national_only
  Test_14 <- test_model[["plots"]]$combined_national_only

  expect_identical(test_model[["MDPI_p"]], Test_01)
  expect_identical(test_model[["MDPI"]], Test_02)
  expect_identical(test_model[["dimensions"]], Test_03)
  expect_identical(test_model[["MDPI mean"]], Test_04)
  expect_identical(test_model[["MDPI SD"]], Test_05)
  expect_identical(test_model[["plots"]]$`Multidimensional poverty index`, Test_06)
  expect_identical(test_model[["plots"]]$`Deprivation Score`, Test_07)
  expect_identical(test_model[["plots"]]$`Adjusted incidence of poverty`, Test_08)
  expect_identical(test_model[["plots"]]$`Intensity of poverty`, Test_09)
  expect_identical(test_model[["plots"]]$`Average deprivation among the deprived`,
                   Test_10)
  expect_identical(test_model[["plots"]]$`Contribution of each Dimension`, Test_11)
  expect_identical(test_model[["plots"]]$combined_only, Test_12)
  expect_identical(test_model[["plots"]]$national_only, Test_13)
  expect_identical(test_model[["plots"]]$combined_national_only , Test_14)
})
