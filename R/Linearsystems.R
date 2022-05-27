#' Linear Model and various Transformations for Efficiency
#'
#' The linear model still remains a reference point towards advanced modeling of some datasets as foundation for **Machine Learning**, **Data Science** and **Artificial Intelligence** in spite of some of her weaknesses. The major task in **modeling** is to compare various models before a selection is made for one or for advanced modelling. Often, some trial and error methods are used to decide which model to select. This is where this function is unique. It helps to estimate seven different linear models and provide their coefficients in a Table for quick comparison so that time and energy are saved. The interesting thing about this function is the simplicity, and it is a _one line_ code.
#'
#' @param y Vector of the dependent variable. This must be numeric.
#' @param x Data frame of the explanatory variables.
#'
#' @return A list with the following components:
#' \item{\code{Linear}}{The full estimates of the Linear Model.}
#' \item{\code{Semilog}}{The full estimates of the Semilog Model. Here the independent variable(s) is/are log-transformed.}
#' \item{\code{Growth}}{The full estimates of the Growth Model. Here the dependent variable(s) is/are log-transformed.}
#' \item{\code{Double Log}}{The full estimates of the Semilog Model. Here the both the dependent and independent variables are log-transformed.}
#' \item{\code{Quardratic}}{The full estimates of the Quadratic Model. Here the square of _numeric_ independent variable(s) is/are included as independent variables.}
#' \item{\code{Inverse y}}{The full estimates of the Inverse Model. Here the dependent variable is inverse-transformed (1/y).}
#' \item{\code{Square root y}}{The full estimates of the Square root Model. Here the dependent variable is square root-transformed (y^1/2).}
#' \item{\code{Model Table}}{Formatted Table of the coefficient estimates of all the models}
#' \item{\code{Prediction plots}}{Plots o the prediction from each of the model.}
#' \item{\code{Summary of numeric variables}}{of the dataset.}
#' \item{\code{Summary of character variables}}{of the dataset.}
#'
#' @export Linearsystems
#'
#' @importFrom fBasics basicStats
#' @importFrom huxtable huxreg
#' @importFrom kableExtra kable_styling
#' @importFrom knitr kable
#' @importFrom dplyr select_if
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_fill_hue
#' @importFrom ggplot2 scale_color_hue
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#'
#' @name Linearsystems
#'
#' @aliases linearsystems
#'
#' @examples
#' y = linearsystems$MKTcost
#' x <- select(linearsystems, -MKTcost)
#' Linearsystems(y = y, x = x)
utils::globalVariables("Prediction")
Linearsystems <- function(y, x) {

  Data <- cbind(y, x)
  Names <- names(Data)

  KKN <- Data %>%
    dplyr::select_if(is.numeric) %>%
    fBasics::basicStats()

  KKC <- Data %>%
    dplyr::select_if(is.character) %>%
    summary()

  Ggk1 <- KKN[-c(1,2,9,13),]
  KKN1 <- kableExtra::kable_styling(knitr::kable(Ggk1, digits = 2), "striped",
                                    position = "center", font_size = 12)

  xnum <- x %>%
    dplyr::select_if(is.numeric)

  xcha <- x %>%
    dplyr::select_if(is.character)

  #Data <- as.data.frame(cbind(y, x))
  #DD0 <- cbind(y, xnum, xcha)
  #names(DD0) <- Names
  Linear   <- lm(y ~ .,       data = Data)
  Linear1  <- fitted.values(Linear)
  Linears <- MLMetrics(Observed = Data, yvalue = y, Model = Linear,
                       K = 2, Name = "Linear", Form = "LM", kutuf = 0,
                       TTy = "Number")

  Growth   <- lm(log(y+1) ~ .,  data = Data)
  Growth1  <- exp(fitted.values(Growth))-1
  Growths <- MLMetrics(Observed = Data, yvalue = y, Model = Growth,
                       K = 2, Name = "Growth", Form = "LM", kutuf = 0,
                       TTy = "Number")
  lxnum <- log(xnum+1)
  DD <- cbind(y, lxnum, xcha)
  Data <- DD

  Semilog  <- lm(y ~ .,  data = Data)
  Semilog1 <- fitted.values(Semilog)
  Semilogs <- MLMetrics(Observed = Data, yvalue = y, Model = Semilog,
                        K = 2, Name = "Semilog", Form = "LM", kutuf = 0,
                        TTy = "Number")

  Loglog   <- lm(log(y+1) ~ .,  data = Data)
  Loglog1  <- exp(fitted.values(Loglog))-1
  Loglogs <- MLMetrics(Observed = Data, yvalue = y, Model = Loglog,
                       K = 2, Name = "Loglog", Form = "LM", kutuf = 0,
                       TTy = "Number")

  Inverse  <- lm((1/(y+1)) ~ .,   data = Data)
  Inverse1  <- 1/fitted.values(Inverse)
  Inverses <- MLMetrics(Observed = Data, yvalue = y, Model = Inverse,
                       K = 2, Name = "Inverse", Form = "LM", kutuf = 0,
                       TTy = "Number")

  Sqrt     <- lm((y)^(1/2) ~ ., data = Data)
  Sqrt1    <- fitted.values(Sqrt)^2
  Sqrts <- MLMetrics(Observed = Data, yvalue = y, Model = Sqrt,
                     K = 2,  Name = "Square root", Form = "LM", kutuf = 0,
                     TTy = "Number")

  sqq <- xnum^2
  names(sqq) <- paste("S", names(xnum), sep = "")
  DD2 <- cbind(y, xnum, xcha, sqq)
  #XXX <- paste("x", 1:1:(length(xnum)+length(xcha)), sep = "")
  #YYY <- paste("x1", 1:length(xnum),  sep = "")
  #XXYY <- c("y", YYY, XXX)
  #names(DD2) <- XXYY
  #MOD <- formula(paste(c('y', paste(c(paste("x",
  #1:(length(xnum)+length(xcha)), sep = "", collapse = " + "),
  #                              paste("x1", 1:length(xnum),  sep = "",
  #                                    collapse = " + ")),
  #                            collapse = " + ")), collapse = " ~ "))
  Data <- DD2
  Quard    <- lm(y ~ ., data = Data)
  Quard1   <- fitted.values(Quard)
  Quards <- MLMetrics(Observed = Data, yvalue = y, Model = Quard,
                      K = 2,  Name = "QUADRATIC", Form = "LM", kutuf = 0,
                      TTy = "Number")

  SelectionCriteria <- as.data.frame(cbind("Linear" = Linears,
                                           "Semilog" = Semilogs,
                                           "Growth" = Growths,
                                           "Double log" = Loglogs,
                                           "Quadratic" = Quards,
                                           "Inverse y" = Inverses,
                                           "Square root y" = Sqrts))
  XYZ1 <- knitr::kable(SelectionCriteria, "html")

  ID <- paste("O", 1:length(Data$y), sep = "")
  ID <- seq(1:length(Data$y))
  ModelTable <- huxtable::huxreg("Linear" = Linear, "Semilog" = Semilog,
                   "Growth" = Growth, "Double Log" = Loglog,
                   "Quardratic" = Quard, "Inverse y" = Inverse,
                   "Square root y" = Sqrt, stars = c(`****` = 0.001,
                                                     `***` = 0.01, `**` = 0.05,
                                                     '*' = 0.1),
                   statistics = NULL)

  PredictTable <- as.data.frame(cbind("ID" = ID, "Observed" = y,
                                      "Linear" = Linear1,
                                       "Semilog" = Semilog1,
                                      "Growth" = Growth1,
                                       "Double Log" = Loglog1,
                                       "Quardratic" =Quard1,
                                       "Inverse y" = Inverse1,
                                       "Square root y" = Sqrt1))

  PredictTable <- PredictTable %>%
    tidyr::pivot_longer(-ID, names_to = "Models", values_to = "Prediction")

  #PredictTable$Prediction <- log(PredictTable$Prediction)
  PredictPlot <- ggplot2::ggplot(PredictTable) +
    aes(x = ID, y = Prediction, fill = Models, colour = Models) +
    ggplot2::geom_line(size = 0.5) +
    ggplot2::scale_fill_hue() +
    ggplot2::scale_color_hue() +
    ggplot2::theme_minimal() +
    ggplot2::facet_wrap(ggplot2::vars(Models))

  results <- list(
    "Linear" = Linear,
    "Semilog" = Semilog,
    "Growth" = Growth,
    "Double Log" = Loglog,
    "Quardratic" = Quard,
    "Inverse y" = Inverse,
    "Square root y" = Sqrt,
    "Model Table" = ModelTable,
    "Prediction plots" = PredictPlot,
    "Summary of numeric variables" = KKN1,
    "Summary of character variables" = KKC,
    "Machine Learning Metrics" =  XYZ1)
  return(results)
}
