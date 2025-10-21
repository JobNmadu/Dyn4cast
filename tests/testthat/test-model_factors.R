
test_that("model_factors works", {
  library(psych)
  library(readr)
  Data <- Quicksummary
  GGn <- names(Data)
  GG <- ncol(Data)
  GGx <- c(paste0('x0', 1 : 9), paste("x", 10 : ncol(Data), sep = ""))
  names(Data) <- GGx
  lll <- fa.parallel(Data, fm = "minres", fa = "fa")
  dat <- fa(Data, nfactors = lll[["nfact"]], rotate = "varimax", fm = "minres")
  test <- model_factors(data = dat, DATA = Data)

  expect_identical(test, model_factors(data = dat, DATA = Data))
})
