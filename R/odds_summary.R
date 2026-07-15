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
#' library(Dyn4cast)
#' library(tidyverse)
#'
#' counts <- c(18,17,15,20,10,20,25,13,12)
#' outcome <- gl(3,1,9)
#' treatment <- gl(3,3)
#' ddc <- data.frame(treatment, outcome, counts) # showing data
#' glm.D93 <- glm(counts ~ ., data = ddc, family = poisson())
#' odds_summary(glm.D93)
#'
odds_summary <- function(model) {
  model <- model
  call <- model$call[[1]]
  Coefficient <- 0
  Odds_ratio <- 0
  dist <-  model$dist
  hhh <- c(" ", " " ," " ," " ," " ,
           "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001",
           " " ," " ," " ," " ," ")

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

    cdd <- dplyr::bind_cols(ctable, Odds_ratio = odds_ratios, or_ci)
    return(rbind(cdd, hhh))

  } else if (call == "mlogit" | call == "glm") {

    if (is.null(model[["family"]])) {
      oddsplyr <- 1
    } else if (model[["family"]][["link"]] == "probit") {
      oddsplyr <- 1.6
    } else {
      oddsplyr <- 1
    }

    names(ctable) <- c("Variables", "Coefficient", "Std Error", "t value",
                       "p value")

    p <- ctable$`p value`

    or_ci <- data.frame(exp(confint(model) * oddsplyr))
    colnames(or_ci) <- c("CI_lower", "CI_upper")

    odds_ratios <- data.frame(Odds_ratio = exp(coef(model) * oddsplyr))

    ctable <- ctable %>% mutate(`Coef Sig` =  p2(p, Coefficient))
    odds_ratios$`%` <- (odds_ratios$Odds_ratio - 1) * 100

    odds_ratios <- odds_ratios %>% mutate(`Odds Sig` = p2(p, Odds_ratio))

    cdd <- dplyr::bind_cols(ctable, Odds_ratio = odds_ratios, or_ci)
    return(rbind(cdd, hhh))

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
    hhm <- hhh[1 : NCOL(ctt)]

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

    return(list(coefficient = coe, t_value = ctt, Odds_ratio = odds_ratios,
                Percent_odds = odds_per, Coef_sig = rbind(Coef_sig, hhm),
                Odds_sig = rbind(Odds_sig, hhm),
                p_value = ccp, Confident_interval = cci))

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

    cdd <- dplyr::bind_cols(ctable, odds_ratios[, -1], or_ci[, -1])
    return(rbind(cdd, hhh))

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

    cdd <- dplyr::bind_cols(ctable, odds_ratios, or_ci)
    return(rbind(cdd, hhh))

  } else {
    stop("Model type not supported. Suggest the model you are analysing for
         inclusion.")
  }
}

p2 <- function(p1, ggt) {

  dplyr::case_when(p1 > 0.1    ~ ifelse(ggt > 9999,
                                        paste0(sprintf("%.2e", ggt) , ""),
                            paste0(round(ggt, 3), "")),
                   p1 <= 0.001 ~ ifelse(ggt > 9999,
                                        paste0(sprintf("%.2e", ggt) , "***"),
                            paste0(round(ggt, 3), "***")),
                   p1 <= 0.01  ~ ifelse(ggt > 9999,
                                        paste0(sprintf("%.2e", ggt) , "**"),
                            paste0(round(ggt, 3), "**")),
                   p1 <= 0.05  ~ ifelse(ggt > 9999,
                                        paste0(sprintf("%.2e", ggt) , "*"),
                            paste0(round(ggt, 3), "*")),
                   p1 <= 0.1   ~ ifelse(ggt > 9999,
                                        paste0(sprintf("%.2e", ggt) , "+"),
                            paste0(round(ggt, 3), "+")))
}
