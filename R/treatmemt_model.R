#' Enhanced Estimation of Treatment Effects of Binary Data from Randomized Experiments
#'
#' @description
#' Observational study involves the evaluation of outcomes of participants not randomly assigned treatments or exposures. To be able to assess the effects of the outcome, the participants are matched using propensity scores (PSM). This then enables the determination of the effects of the treatments on those treated against those who were not treated. Most of the earlier functions available for this analysis only enables the determination of the average treatments effects on the treated (ATT) while the other treatment effects are optional. This is where this functions is unique because five different average treatment effects are estimated simultaneously, in spite of the **one line code arguments**. The five treatment effects are:
#'
#' 1. Average treatment effect  for the entire (ATE)  population
#'
#' 2. Average treatment effect  for the treated (ATT)  population
#'
#' 3. Average treatment effect  for the controlled (ATC)  population
#'
#' 4. Average treatment effect  for the evenly matched (ATM)  population
#'
#' 5. Average treatment effect  for the overlap (ATO) population.
#'
#' There are excellent materials dealing with each of the treatment effects, please see [Understanding propensity score weightin](https://livefreeordichotomize.com/posts/2019-01-17-understanding-propensity-score-weighting/)
#'
#' @param Treatment Vector of binary data (0 = control population, 1 = treated population) LHS for the treatment effects estimation
#' @param x_data Data frame of explanatory variables for the RHS of the estimation
#'
#' @return A list with the following components:
#' \item{\code{Model}}{Estimated treatment effects model.}
#' \item{\code{Effect}}{Data frame of the estimated various treatment effects.}
#' \item{\code{P_score}}{Vector of estimated propensity scores from the model}
#' \item{\code{Fitted_estimate}}{Vector of fitted values from the model}
#' \item{\code{Residuals}}{Residuals of the estimated model}
#' \item{\code{`Experiment plot`}}{Plot of the propensity scores from the model faceted into Treated and control populations}
#' \item{\code{`ATE plot`}}{Plot of the average treatment effect for the **entire** population}
#' \item{\code{`ATT plot`}}{Plot of the average treatment effect for the **treated** population}
#' \item{\code{`ATC plot`}}{Plot of the average treatment effect for the **controlled** population}
#' \item{\code{`ATM plot`}}{Plot of the average Treatment effect for the **evenly** population}
#' \item{\code{`ATO plot`}}{Plot of the average Treatment effect for the **overlap** population}
#' \item{\code{weights}}{Estimated weights for each of the treatment effects}
#'
#' @export treatment_model
#'
#' @importFrom stats binomial
#' @importFrom stats glm
#' @importFrom ggplot2 geom_histogram
#' @importFrom dplyr case_match
#' @importFrom ggplot2 after_stat
#' @importFrom dplyr count
#' @importFrom ggplot2 geom_hline
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom dplyr mutate
#' @aliases treatments
#'
#' @examples
#' Treatment = treatments$treatment
#' data = treatments[, c(2:3)]
#' treatment_model(Treatment, data)
#'
#' @usage treatment_model(Treatment, x_data)
#'
treatment_model <- function(Treatment, x_data) {
  data = cbind(Treatment, x_data)
  MM = stats::glm(Treatment ~ 1 + ., data = data, family = stats::binomial(link = "logit"))
  propensity_score <- predict(MM, type = "response")

  w_ate = (Treatment / propensity_score) +  ((1 - Treatment) /
                                               (1 - propensity_score))

  w_att = ((propensity_score * Treatment) / propensity_score) +
    ((propensity_score * (1 - Treatment)) / (1 - propensity_score))

  w_atc = (((1 - propensity_score) * Treatment) / propensity_score) +
    (((1 - propensity_score) * (1 - Treatment)) / (1 - propensity_score))

  w_atm = pmin(propensity_score, 1 - propensity_score) /
    (Treatment * propensity_score + (1 - Treatment) *
       (1 - propensity_score))

  w_ato = (1 - propensity_score) * Treatment + propensity_score *
    (1 - Treatment)

  datw <- data.frame(cbind(ATE = w_ate, ATT = w_att, ATC = w_atc, ATM = w_atm,
                           ATO = w_ato))

  dat1 <- data.frame(cbind(data, propensity_score, w_ate, w_att,
                           w_atc, w_atm, w_ato))

  dat1$Status <- dplyr::case_match(dat1$Treatment,
                              1 ~ "Treated-actual",
                              0 ~ "Control-actual")

  Exp <- NULL

  Exp_p <- dat1 %>%
    dplyr::mutate(Exp = ifelse(Treatment == 1, "Treated", "Control")) %>%
    ggplot(aes(x = propensity_score, fill = Exp)) +
    ggplot2::geom_histogram(color = "white") +
    facet_wrap(~Exp) +
    labs(x = "Propensity scores",
         y = "Frequency",
         fill = "Experiment") +
    theme_bw()

  dat2 <- dat1 %>%
    tidyr::pivot_wider(names_from = Treatment, values_from = propensity_score,
                names_prefix= "treat_p")

  # ATE
  ATE_P <- plotu(dat2$treat_p1, dat2$treat_p0, dat2$w_ate)

  # ATT
  ATT_P <- plotu(dat2$treat_p1, dat2$treat_p0, dat2$w_att)

  # ATC
  ATC_P <- plotu(dat2$treat_p1, dat2$treat_p0, dat2$w_atc)

  # ATM
  ATM_P <- plotu(dat2$treat_p1, dat2$treat_p0, dat2$w_atm)

  # ATO
  ATO_P <- plotu(dat2$treat_p1, dat2$treat_p0, dat2$w_ato)

  x_data <- x_data %>%
    dplyr::select_if(is.numeric)

  Effect  <- data.frame(
    ATE = treatment_effect(Treatment, x_data, w_ate),
    ATT = treatment_effect(Treatment, x_data, w_att),
    ATC = treatment_effect(Treatment, x_data, w_atc),
    ATM = treatment_effect(Treatment, x_data, w_atm),
    ATO = treatment_effect(Treatment, x_data, w_ato))

  RR <- list(P_score = propensity_score,
             Effect = Effect,
             Fitted_estimate = MM[["fitted.values"]],
             Residuals = MM[["residuals"]],
             Model = MM,
             `Experiment plot` = Exp_p,
             `ATE plot` = ATE_P,
             `ATT plot` = ATT_P,
             `ATC plot` = ATC_P,
             `ATM plot` = ATM_P,
             `ATO plot` = ATO_P,
             weights = datw)

  return(RR)
}

treatment_effect <- function(treat, model, weight) {
  (sum(treat * model * weight) / sum(treat * weight)) +
    (sum((1 - treat) * model * weight) / sum((1 - treat) * weight))
}

plotu <- function(p1, p0, www) {
  ggplot() +
    ggplot2::geom_histogram(aes(x = p1, y = ggplot2::after_stat(!!str2lang("count")),
                                fill = "g"),
                   alpha = .5, binwidth = 0.05) +
    ggplot2::geom_histogram(aes(x = p1, weight = www, fill = "o"),
                   alpha = .5, binwidth = 0.05) +
    ggplot2::geom_histogram(aes(x = p0, y = -ggplot2::after_stat(!!str2lang("count")),
                                fill = "p"),
                   alpha = .5, binwidth = 0.05) +
    ggplot2::geom_histogram(aes(x = p0, weight = www,
                                y = -ggplot2::after_stat(!!str2lang("count")),
                                fill = "s"),
                   alpha = .5, binwidth = 0.05)  +
    labs(x = "Propensity scores", y = "Frequency") +
    ggplot2::geom_hline(yintercept = 0, lwd = 0.5) +
    ggplot2::scale_y_continuous(label = abs) +
    ggplot2::scale_fill_manual(name = "Scores",
                               values = c("g" = "green", "o" = "orangered",
                                          "p" = "purple", "s" = "skyblue2"),
                      labels = c("g" = "Treated - actual",
                                 "o" = "Treated - weighted",
                                 "p" = "Control - actual",
                                 "s" = "Control - weighted"))
}

