test_that("model_factors works", {
  library(psych)
  Data <- Quicksummary
  GGn <- names(Data)
  GG <- ncol(Data)
  GGx <- c(paste0('x0', 1:9), paste("x", 10:ncol(Data), sep = ""))
  names(Data) <- GGx
  lll <- fa.parallel(Data, fm = 'minres', fa = 'fa')
  dat <- fa(Data, nfactors = lll[["nfact"]], rotate = "varimax", fm="minres")
  test <- model_factors(data = dat, DATA = Data)

  Test_01 <- test[["Latent_frame"]]
  Test_02 <- test[["Latent_1"]]
  Test_03 <- test[["Latent_2"]]
  Test_04 <- test[["Latent_3"]]
  Test_05 <- test[["Latent_4"]]
  Test_06 <- test[["Latent_5"]]

  expect_identical(test[["Latent_frame"]], Test_01)
  expect_identical(test[["Latent_1"]], Test_02)
  expect_identical(test[["Latent_2"]], Test_03)
  expect_identical(test[["Latent_3"]], Test_04)
  expect_identical(test[["Latent_4"]], Test_05)
  expect_identical(test[["Latent_5"]], Test_06)
})
