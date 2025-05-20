test_that("mdpi works", {
  library(Dyn4cast)
  data <- mdpi1
  dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
             d2 = c("Years.of.education", "School.attendance", "School.lag"),
             d3 = c("Cooking.Fuel", "Access.to.clean.source.of.water",
                    "Access.to.an.improve.sanatation", "Electricity",
                    "Housing.Materials", "Asset.ownership"))
  test1 <- mdpi(data, dm, plots = "t", Factor = "Region")
  Test01 <- test1[-1]
  Test02 <- test1[1]
  test2 <- mdpi(data, dm, plots = "t")
  Test04 <- test2

  dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
             d2 = c("Years.of.education", "School.attendance", "School.lag"),
             d3 = c("Cooking.Fuel", "Electricity", "Housing.Materials",
                    "Asset.ownership"),
             d4 = c("Access.to.clean.source.of.water",
                    "Access.to.an.improve.sanatation"))

  test5 <- mdpi(data, dm, id_add = c("Water and Sanitation"),
                plots = "t", Factor = "Region")
  Test17 <- test5[-1]
  Test08 <- test5[1]

  test6 <- mdpi(data, dm, id_add = c("Water and Sanitation"), plots = "t")
  Test09 <- test6

  data <- mdpi2
  dm <- list(d1 = c("d_nutr","d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))
  test3 <- mdpi(data, dm, plots = "t", Factor = "region")
  Test05 <- test3[-1]
  Test06 <- test3[1]
  test4 <- mdpi(data, dm, plots = "t")
  Test07 <- test4
  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_hsg","d_ckfl","d_asst"),
             d4 = c("d_sani","d_wtr"),
             d5 = c("d_nutr"))

  test7 <- mdpi(data, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
       plots = "t", Factor = "region")
  test8 <- mdpi(data, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
       plots = "t")
  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_hsg","d_ckfl","d_asst"),
             d4 = c("d_sani","d_wtr"),
             d5 = c("d_nutr"))

  test7 <- mdpi(data, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
                plots = "t", Factor = "region")
  test8 <- mdpi(data, dm, id_add = "Water and Sanitation", id_add1 = "Nutrition",
                plots = "t")
  Test10 <- test7[-1]
  Test11 <- test7[1]
  Test12 <- test8

  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_ckfl"),
             d4 = c("d_sani","d_wtr"),
             d5 = c("d_nutr"),
             d6 = c("d_hsg","d_asst"))

  test9 <- mdpi(data, dm, id_add = "Water and Sanitation",
                id_add1 = "Nutrition", id_addn = "Housing and Assets",
                plots = "t", Factor = "region")

  test10 <- mdpi(data, dm, id_add = "Water and Sanitation",
                 id_add1 = "Nutrition", id_addn = "Housing and Assets",
                 plots = "t")
  Test13 <- test9[-1]
  Test14 <- test9[1]
  Test15 <- test10

  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_ckfl"),
             d4 = c("d_sani","d_wtr"),
             d5 = c("d_nutr"),
             d6 = c("d_hsg"),
             d7 = c("d_asst"))

  test11 <- mdpi(data, dm, id_add = "Water and Sanitation",
                id_add1 = "Nutrition", id_addn = c("Housing", "Assets"),
                plots = "t", Factor = "region")

  test12 <- mdpi(data, dm, id_add = "Water and Sanitation",
                 id_add1 = "Nutrition", id_addn = c("Housing", "Assets"),
                 plots = "t")
  Test16 <- test11[-1]
  Test19 <- test11[1]
  Test18 <- test12

  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_ckfl"),
             d4 = c("d_sani"),
             d5 = c("d_nutr"),
             d6 = c("d_hsg"),
             d7 = c("d_asst"),
             d8 = c("d_wtr"))

  test13 <- mdpi(data, dm, id_add = "Sanitation",
                 id_add1 = "Nutrition",
                 id_addn = c("Housing", "Assets", "Water"),
                 plots = "t", Factor = "region")

  test14 <- mdpi(data, dm, id_add = "Sanitation",
                 id_add1 = "Nutrition",
                 id_addn = c("Housing", "Assets", "Water"), plots = "t")
  Test20 <- test13[-1]
  Test21 <- test13[1]
  Test22 <- test14

  dm <- list(d1 = c("d_cm"),
             d2 = c("d_satt","d_educ"),
             d3 = c("d_elct", "d_ckfl"),
             d4 = c("d_sani"),
             d5 = c("d_nutr"),
             d6 = c("d_hsg"),
             d7 = c("d_asst"),
             d8 = c("d_wtr"),
             d9 = c("d_elct"))

  test15 <- mdpi(data, dm, id_add = "Sanitation",
                 id_add1 = "Nutrition",
                 id_addn = c("Housing", "Assets", "Water", "Electricity"),
                 plots = "t", Factor = "region")

  test16 <- mdpi(data, dm, id_add = "Sanitation",
                 id_add1 = "Nutrition",
                 id_addn = c("Housing", "Assets", "Water", "Electricity"),
                 plots = "t")
  Test23 <- test15[-1]
  Test24 <- test15[1]
  Test25 <- test16

  expect_identical(test1[-1], Test01)
  expect_identical(test1[1],  Test02)
  expect_identical(test2,     Test04)
  expect_identical(test4,     Test07)
  expect_identical(test3[-1], Test05)
  expect_identical(test3[1],  Test06)
  expect_identical(test6,     Test09)
  expect_identical(test5[-1], Test17)
  expect_identical(test5[1],  Test08)
  expect_identical(test8,     Test12)
  expect_identical(test7[-1], Test10)
  expect_identical(test7[1],  Test11)
  expect_identical(test10,    Test15)
  expect_identical(test9[-1], Test13)
  expect_identical(test9[1],  Test14)
  expect_identical(test12,    Test18)
  expect_identical(test11[-1], Test16)
  expect_identical(test11[1],  Test19)
  expect_identical(test14,    Test22)
  expect_identical(test13[-1], Test20)
  expect_identical(test13[1],  Test21)
  expect_identical(test16,    Test25)
  expect_identical(test15[-1], Test23)
  expect_identical(test15[1],  Test24)
})
