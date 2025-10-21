
test_that("corplot works", {
  library(Dyn4cast)
  library(tidyverse)
  y <- linearsystems$MKTcost
  x <- select(linearsystems, -MKTcost)
  Data <- cbind(y, x)
  KNN <- Data %>% select_if(is.numeric)
  YYY <- paste(c("y", paste(c(paste("x", 1 : (length(KNN)-1), sep = "")))))
  colnames(KNN) <- YYY
  ddd <- corplot(cor(KNN))
  test <- ddd$plot()

  expect_identical(test, ddd$plot())
})
