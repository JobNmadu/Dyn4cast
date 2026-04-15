#' Odds-Based Measures for Binary and Categorical Models

#' @description
#' This function computes odds ratios, percentage changes, and confidence
#' intervals from fitted binary and categorical regression models.
#' It standardizes statistical inference outputs and highlights significant
#' predictors for rapid interpretation. It is a _one-line_, _one-argument_ code!
#'
#' @param model An `R` object of estimates from models covered. For now only
#'  `glm`, `betareg`, `mlogit`, `multimon`, `mvProbit` and `polr` models are covered.
#'
#' @returns A `list` or a `data.frame` depending on which model. The model must
#'  converged otherwise there will be no any return and an error is thrown up
#'
#' @importFrom stats confint
#' @importFrom stats pnorm
#'
#' @export odds_summary
#'
#' @examples
#'
#' # library(Dyn4cast)  # uncomment to run
#' # library(tidyverse)
#'
#' # counts <- c(18,17,15,20,10,20,25,13,12)
#' # outcome <- gl(3,1,9)
#' # treatment <- gl(3,3)
#' # ddc <- data.frame(treatment, outcome, counts) # showing data
#' # glm.D93 <- glm(counts ~ ., data = ddc, family = poisson())
#' # odds_summary(glm.D93)
#'
#' # library(MASS)
#' # anorexia
#'
#' # anorex.1 <- glm(Postwt ~ Prewt + Treat + offset(Prewt),
#' #                 family = gaussian, data = anorexia)
#' # odds_summary(anorex.1)
#'
#' # clotting <- data.frame(
#' #   u = c(5,10,15,20,30,40,60,80,100),
#' #   lot1 = c(118,58,42,35,27,25,21,19,18),
#' #   lot2 = c(69,35,26,21,18,16,13,12,12))
#' # lot1 <- glm(lot1 ~ log(u), data = clotting, family = Gamma)
#' # odds_summary(lot1)
#'
#' # lot2 <- glm(lot2 ~ log(u), data = clotting, family = Gamma)
#' # odds_summary(lot2)
#'
#' # fS <- glm(lot2 ~ log(u) + log(u^2), data = clotting, family = Gamma)
#' # #odds_summary(fS) #error because there is no convergence
#'
#' # x <- rnorm(100)
#' # y <- rpois(100, exp(1+x))
#'
#' # lm2 <- glm(y ~ x, family = quasi(variance = "mu", link = "log"))
#' # odds_summary(lm2)
#'
#' # lm3 <- glm(y ~ x, family = poisson)
#' # odds_summary(lm3)
#'
#' # lm4 <- glm(y ~ x, family = quasi(variance = "mu^2", link = "log"))
#' # #odds_summary(lm4) #error
#'
#' # y <- rbinom(100, 1, plogis(x))
#'
#' # lm5 <- glm(y ~ x, family = quasi(variance = "mu(1-mu)", link = "logit"),
#' #            start = c(0,1))
#' # odds_summary(lm5)
#'
#' # library(betareg)
#' # data("GasolineYield")
#' # gy <- betareg(yield ~ batch + temp, data = GasolineYield)
#' # odds_summary(gy)
#'
#' # library(mvProbit)
#' # ## generate a simulated data set
#' # set.seed( 123 )
#' # # number of observations
#' # nObs <- 50
#'
#' # # generate explanatory variables
#' # xMat <- cbind(const = rep(1, nObs), x1 = as.numeric(rnorm(nObs) > 0),
#' # x2 = rnorm(nObs))
#'
#' # # model coefficients
#' # beta <- cbind(c(0.8,  1.2, -0.8), c(-0.6, 1.0, -1.6), c(0.5, -0.6, 1.2))
#'
#' # # covariance matrix of error terms
#' # library(miscTools)
#' # sigma <- symMatrix(c(1, 0.2, 0.4, 1, -0.1, 1))
#'
#' # # generate dependent variables
#' # yMatLin <- xMat %*% beta
#' # yMat <- (yMatLin + rmvnorm(nObs, sigma = sigma)) > 0
#' # colnames(yMat) <- paste("y", 1:3, sep = "")
#'
#' # estResultStart <- mvProbit(cbind(y1, y2, y3) ~ x1 + x2, start = c(beta),
#' # startSigma = sigma, data = as.data.frame(cbind(xMat, yMat)), iterlim = 1,
#' # nGHK = 50)
#' # odds_summary(estResultStart)
#' #
odds_summary <- function(model) {
  model <- model
  call <- model$call[[1]]
  Coefficient <- 0
  Odds_ratio <- 0
  dist <-  model$dist

# Get the coefficient table
  ctable <- data.frame(coef(summary(model))) %>%
    tibble::rownames_to_column(., var = "Variables")

  if (call == "polr") {
    names(ctable) <- c("Variables", "Coefficient", "Std Error", "t value")
    zeze <- data.frame(Coefficient = model[["zeta"]]) %>%
      tibble::rownames_to_column(., var = "Variables")

    zeze <- ctable[ctable$Variables %in% zeze$Variables, ]
    zeze$margin <- qt(0.975, df = model$n - 1) * zeze$`Std Error`
    zeze$CI_lower <- zeze$Coefficient - zeze$margin
    zeze$CI_upper <- zeze$Coefficient + zeze$margin
    #zeze$OD <- exp(zeze$Coefficient)

# Calculate p-values using the t-distribution
    p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
    ctable <- cbind(ctable, "p value" = p)

# Calculate confidence intervals
    ci <- data.frame(confint(model, type = "wald")) %>%
      tibble::rownames_to_column(., var = "Variables")
    or_ci <- exp(ci[, 2:3])
    names(or_ci) <- c("CI_lower", "CI_upper")
    or_ci <- rbind(or_ci, zeze[, 6:7])

    odds_ratios <- data.frame(Odds_ratio = exp(c(coef(model), model$zeta)))

    ctable <- ctable %>% mutate(`Coef Sig` =  p2(p, Coefficient))
    odds_ratios$`%` <- (odds_ratios$Odds_ratio - 1) * 100

    odds_ratios <- odds_ratios %>% mutate(`Odds Sig` = p2(p, Odds_ratio))

  } else if (call == "mlogit" | call == "glm") {
    names(ctable) <- c("Variables", "Coefficient", "Std Error", "t value",
                       "p value")

    p <- ctable$`p value`

    or_ci <- data.frame(exp(confint(model)))
    names(or_ci) <- c("CI_lower", "CI_upper")

    odds_ratios <- data.frame(Odds_ratio = exp(coef(model)))

    ctable <- ctable %>% mutate(`Coef Sig` =  p2(p, Coefficient))
    odds_ratios$`%` <- (odds_ratios$Odds_ratio - 1) * 100

    odds_ratios <- odds_ratios %>% mutate(`Odds Sig` = p2(p, Odds_ratio))
  } else if (call == "multinom") {
    coe <- data.frame(t(coef(summary(model)))) %>%
      tibble::rownames_to_column(., var = "Variables")

    cse <- data.frame(t((summary(model)$standard.errors))) %>%
      rownames_to_column(., var = "Variables")

    ctt <- data.frame(coe[, -1]/cse[, -1])

    odds_ratios <- data.frame(Variables = coe[, 1], exp(coe[, -1]))

    odds_per <- data.frame(Variables = coe[, 1], (odds_ratios[, -1] - 1) * 100)

    ccp <- 0
    Coef_sig <- Odds_sig <- vector("list", length = NCOL(ctt))
    oddss <- odds_ratios[, -1]
    coefs <- coe[, -1]

    for (i in 1 : NCOL(ctt)) {
      p1 <- pnorm(abs(ctt[, i]), lower.tail = FALSE) * 2
      ccp <- cbind(ccp, p1)

      ggt <- coefs[, i]
      Coef_sig[[i]] <- p2(p1, ggt)

      ggt <- oddss[, i]
      Odds_sig[[i]] <- p2(p1, ggt)
    }

    ccp <- data.frame(ccp[, -1])
    Coef_sig <- data.frame(Coef_sig)
    Odds_sig <- data.frame(Odds_sig)
    names(ccp)  <- names(Coef_sig)  <- names(Odds_sig)  <- names(ctt)

    cci <- data.frame(confint(model)) %>%
      tibble::rownames_to_column(., var = "Variables")
    names(cci) <- gsub("X2.5...", "Lower ", names(cci))
    names(cci) <- gsub(c("X97.5..."), "Upper ", names(cci))
  } else if (call == "betareg") {
    ctable <- data.frame(coef(summary(model)))
    phiy <- ctable[1, 5:8]

    if (dist != "beta") {
      nunu <- ctable[1, 9:12]
      names(phiy) <- names(nunu) <- names(ctable[, 1:4])
      cctable <- dplyr::bind_rows(ctable[, 1:4], phiy, nunu)
    } else{
      names(phiy) <- names(ctable[, 1:4])
      cctable <- dplyr::bind_rows(ctable[, 1:4], phiy)
    }

    names(cctable) <- c("Coefficient", "Std. Error", "z value", "p value")

    cctable <- cctable %>%
      tibble::rownames_to_column(., var = "Variables")

    or_ci <- data.frame(exp(confint(model)))
    names(or_ci) <- c("CI_lower", "CI_upper")

    or_ci <- or_ci %>%
      tibble::rownames_to_column(., var = "Variables")

    odds_ratios <- data.frame(Odds_ratio = exp(coef(model))) %>%
      tibble::rownames_to_column(., var = "Variables")
    p <- cctable$`p value`
    cctable <- cctable %>% mutate(`Coef Sig` =  p2(p, Coefficient))
    odds_ratios$`%` <- (odds_ratios$Odds_ratio - 1) * 100
    odds_ratios <- odds_ratios %>% mutate(`Odds Sig` = p2(p, Odds_ratio))

    ctable <- data.frame(Variables = or_ci[, 1], cctable[, -1])
  } else if (call == "mvProbit") {
    ctable <- data.frame(coef(summary(model)))
    names(ctable) <- c("Coefficient", "Std. Error", "z value", "p value")

    ctable <- ctable %>%
      tibble::rownames_to_column(., var = "Variables")

    odds_ratios <- data.frame(Odds_ratio = exp(c(coef(model))))

    or_ci <- data.frame(exp(confint(model)))
    names(or_ci) <- c("CI_lower", "CI_upper")

    p <- ctable$`p value`

    ctable <- ctable %>% mutate(`Coef Sig` =  p2(p, Coefficient))
    odds_ratios$`%` <- (odds_ratios$Odds_ratio - 1) * 100
    odds_ratios <- odds_ratios %>% mutate(`Odds Sig` = p2(p, Odds_ratio))
  } else {
    stop("Model type not supported. Suggest the model you are analysing for
         inclusion.")
  }

# Combine everything into a data frame

  if (call == "multinom") {
    return(list(coefficient = coe, t_value = ctt, Odds_ratio = odds_ratios,
                Percent_odds = odds_per, Coef_sig = Coef_sig,
                Odds_sig = Odds_sig,
                p_value = ccp, Confident_interval = cci))

  } else if (call == "betareg") {
    return(dplyr::bind_cols(ctable, odds_ratios[, -1], or_ci[, -1]))
  } else if (call == "mvProbit") {
    return(dplyr::bind_cols(ctable, odds_ratios, or_ci))
  } else {
    return(dplyr::bind_cols(ctable, Odds_ratio = odds_ratios, or_ci))
  }
}

p2 <- function(p1, ggt) {
  dplyr::case_when(p1 > 0.1    ~ paste0(round(ggt, 3), ""),
                   p1 <= 0.001 ~ paste0(round(ggt, 3), "***"),
                   p1 <= 0.01  ~ paste0(round(ggt, 3), "**"),
                   p1 <= 0.05  ~ paste0(round(ggt, 3), "*"),
                   p1 <= 0.1   ~ paste0(round(ggt, 3), "+"))
}
