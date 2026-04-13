
test_that("odds_summary works", {
  library(Dyn4cast)
  library(tidyverse)

  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  ddc <- data.frame(treatment, outcome, counts) # showing data
  glm.D93 <- glm(counts ~ ., data = ddc, family = poisson())
  ts <- odds_summary(glm.D93)

  library(MASS)

  options(contrasts = c("contr.treatment", "contr.poly"))
  house.plr <- polr(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  ts1 <- odds_summary(house.plr)

  anorexia

  anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
                  family = gaussian, data = anorexia)
  ts2 <- odds_summary(anorex.1)

  clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
  lot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
  ts3 <- odds_summary(lot1)

  lot2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
  ts4 <- odds_summary(lot2)

  x <- rnorm(100)
  y <- rpois(100, exp(1+x))

  lm2 <- glm(y ~ x, family = quasi(variance = "mu", link = "log"))
  ts5 <- odds_summary(lm2)

  lm3 <- glm(y ~ x, family = poisson)
  ts8 <- odds_summary(lm3)

  library(mlogit)
  data("Fishing", package = "mlogit")
  Fish <- dfidx(Fishing, varying = 2:9, shape = "wide", choice = "mode")

  ## a pure "conditional" model
  mml <- mlogit(mode ~ price + catch, data = Fish)
  ts6 <- odds_summary(mml)

  library(nnet)
  data(iris)

 tinom <- multinom(Species ~ Petal.Length + Petal.Width + Sepal.Length
                    + Sepal.Width, data = iris)
  ts7 <- odds_summary(tinom)

  library(betareg)
  data("GasolineYield")
  gy <- betareg(yield ~ batch + temp, data = GasolineYield)
  ts9 <- odds_summary(gy)

  expect_identical(ts,  odds_summary(glm.D93))
  expect_identical(ts1, odds_summary(house.plr))
  expect_identical(ts2, odds_summary(anorex.1))
  expect_identical(ts3, odds_summary(lot1))
  expect_identical(ts4, odds_summary(lot2))
  expect_identical(ts5, odds_summary(lm2))
  expect_identical(ts6, odds_summary(mml))
  expect_identical(ts7, odds_summary(tinom))
  expect_identical(ts8, odds_summary(lm3))
  expect_identical(ts9, odds_summary(gy))
})
