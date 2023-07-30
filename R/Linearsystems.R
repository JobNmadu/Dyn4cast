#' Linear Model and various Transformations for Efficiency
#'
#' The linear model still remains a reference point towards advanced modeling of some datasets as foundation for **Machine Learning**, **Data Science** and **Artificial Intelligence** in spite of some of her weaknesses. The major task in **modeling** is to compare various models before a selection is made for one or for advanced modeling. Often, some trial and error methods are used to decide which model to select. This is where this function is unique. It helps to estimate 14 different linear models and provide their coefficients in a formatted Table for quick comparison so that time and energy are saved. The interesting thing about this function is the simplicity, and it is a _one line_ code.
#'
#' @param y Vector of the dependent variable. This must be numeric.
#' @param x Data frame of the explanatory variables.
#' @param mod The group of linear models to be estimated. It takes value from 0 to 6. 0 = EDA (correlation, summary tables, Visuals means); 1 = Linear systems, 2 = power models, 3 = polynomial models, 4 = root models, 5 = inverse models, 6 = all the 14 models
#' @param limit Number of variables to be included in the coefficients plots
#' @param Test test data to be used to predict y. If not supplied, the fitted y is used hence may be identical with the fitted value. It is important to be cautious if the data is to be divided between train and test subsets in order to train and test the model. If the sample size is not sufficient to have enough data for the test, errors are thrown up.
#'
#' @return A list with the following components:
#' \item{\code{Visual means of the numeric variable}}{Plot of the means of the _numeric_ variables.}
#' \item{\code{Correlation plot}}{Plot of the Correlation Matrix of the _numeric_ variables. To recover the plot, please use this canonical form `objectnale$e_corplot$plot()`.}
#' \item{\code{Linear}}{The full estimates of the Linear Model.}
#' \item{\code{Linear with interaction}}{The full estimates of the Linear Model with full interaction among the _numeric_ variables.}
#' \item{\code{Semilog}}{The full estimates of the Semilog Model. Here the independent variable(s) is/are log-transformed.}
#' \item{\code{Growth}}{The full estimates of the Growth Model. Here the dependent variable is log-transformed.}
#' \item{\code{Double Log}}{The full estimates of the double-log Model. Here the both the dependent and independent variables are log-transformed.}
#' \item{\code{Mixed-power model}}{The full estimates of the Mixed-power Model. This is a combination of linear and double log models. It has significant gains over the two models separately.}
#' \item{\code{Translog model}}{The full estimates of the double-log Model with full interaction of the _numeric_ variables.}
#' \item{\code{Quadratic}}{The full estimates of the Quadratic Model. Here the square of _numeric_ independent variable(s) is/are included as independent variables.}
#' \item{\code{Cubic model}}{The full estimates of the Cubic Model. Here the third-power (x^3) of _numeric_ independent variable(s) is/are included as independent variables.}
#' \item{\code{Inverse y}}{The full estimates of the Inverse Model. Here the dependent variable is inverse-transformed (1/y).}
#' \item{\code{Inverse x}}{The full estimates of the Inverse Model. Here the independent variable is inverse-transformed (1/x).}
#' \item{\code{Inverse y & x}}{The full estimates of the Inverse Model. Here the dependent and independent variables are inverse-transformed 1/y & 1/x).}
#' \item{\code{Square root}}{The full estimates of the Square root Model. Here the independent variable is square root-transformed (x^0.5).}
#' \item{\code{Cubic root}}{The full estimates of the cubic root Model. Here the independent variable is cubic root-transformed (x^1/3).}
#' \item{\code{Significant plot of Linear}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Linear with interaction}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Semilog}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Growth}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Double Log}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Mixed-power model}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Translog model}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Quadratic}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Cubic model}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Inverse y}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Inverse x}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Inverse y & x}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Square root}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Significant plot of Cubic root}}{Plots of order of importance and significance of estimates coefficients of the model.}
#' \item{\code{Model Table}}{Formatted Tables of the coefficient estimates of all the models}
#' \item{\code{Machine Learning Metrics}}{Metrics (47) for assessing model performance and metrics for diagnostic analysis of the error in estimation.}
#' \item{\code{Marginal effecrs Tables}}{Tables of marginal effects of each model. Because of computational limitations, if you choose to estimate all the 14 models, the Tables are produced separately for the major transformations. They can easily be compiled into one.}
#' \item{\code{Fitted plots long format}}{Plots of the fitted estimates from each of the model.}
#' \item{\code{Fitted plots wide format}}{Plots of the fitted estimates from each of the model.}
#' \item{\code{Prediction plots long format}}{Plots of the predicted estimates from each of the model.}
#' \item{\code{Prediction plots wide format}}{Plots of the predicted estimates from each of the model.}
#' \item{\code{Naive effects plots long format}}{Plots of the `lm` effects. May be identical with plots of marginal effects if performed.}
#' \item{\code{Naive effects plots wide format}}{Plots of the `lm` effects. May be identical with plots of marginal effects if performed.}
#' \item{\code{Summary of numeric variables}}{of the dataset.}
#' \item{\code{Summary of character variables}}{of the dataset.}
#'
#' @export Linearsystems
#'
#' @importFrom modelsummary modelsummary
#' @importFrom dplyr select_if
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 scale_fill_hue
#' @importFrom ggplot2 scale_color_hue
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 vars
#' @importFrom ggplot2 stat_summary
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 element_blank
#' @importFrom ggtext element_markdown
#' @importFrom marginaleffects avg_slopes
#' @importFrom tibble rownames_to_column
#'
#' @name Linearsystems
#'
#' @aliases linearsystems
#' @aliases sampling
#'
#' @examples
#' # Without test data (not run)
#' y = linearsystems$MKTcost
#' x <- select(linearsystems, -MKTcost)
#' Linearsystems(y, x, 6, 15)
#' # Without test data (not run)
#' x = sampling[, -1]
#' y = sampling$qOutput
#' limit = 20
#' mod <-3
#' Test <- NA
#' Linearsystems(y, x, 3, 15)
#' x = sampling[, -1]
#' y = sampling$qOutput
#' Data <- cbind(y, x)
#' sampling <- sample(1:nrow(Data), 0.8*nrow(Data)) # 80% of data is sampled for training the model
#' train <- Data[sampling, ]
#' Test  <- Data[-sampling, ] # 20% of data is reserved for testing (predicting) the model
#' y <- train$y
#' x <- train[, -1]
#' mod <- 4
#' Linearsystems(y, x, 4, 15, Test)
utils::globalVariables(c("Variables", "Model", "values", "Observed"))
Linearsystems <- function(y, x, mod, limit, Test = NA) {

  y1 <- y
  Data <- cbind(y, x)
  Names <- names(Data)

  KNN <- Data %>% dplyr::select_if(is.numeric)

  KNN1 <- quicksummary(x = KNN, Type = 1)

  KKC <- Data %>%
    dplyr::select_if(is.character) %>%
    summary()

  xnum <- x %>%
    dplyr::select_if(is.numeric)
  xnum_n <- names(xnum)

  xcha <- x %>%
    dplyr::select_if(is.character)
  xcha_n <- names(xcha)

  KNN <- Data %>% dplyr::select_if(is.numeric)
  YYY <- paste(c('y', paste(c(paste("x", 1 : (length(KNN)-1), sep = "")))))
  colnames(KNN) <- YYY

  e_corplot <- corplot(stats::cor(KNN))

  Data1 <- tidyr::pivot_longer(KNN, tidyr::everything(), names_to = "Variables",
                               values_to = "values")

  e_meanplot <- ggplot2::ggplot(Data1, ggplot2::aes(x = Variables, y = values)) +
    ggplot2::geom_jitter(linewidth = 3,
                         alpha = 0.7,
                         shape = 16,
                         width = 0.2,
                         color = "cadetblue") +
    ggplot2::geom_vline(
      xintercept = seq(.5, length(unique(Data1$Variables)), by = 1),
      color = "gray90",
      linewidth = 1) +
    ggplot2::stat_summary(fun = "mean",
                          geom = "point",
                          size = 5,
                          alpha = 0.6,
                          shape = 16,
                          color = "tomato") +
    ggplot2::stat_summary(
      ggplot2::aes(label = round(ggplot2::after_stat(y), 1), fontface = "bold"),
      fun = mean,
      geom = "label", #try text if you want
      linewidth = 4,
      alpha = 0.6,
      label.size = NA, # remove border around label
      vjust = -0.5,
      color = "tomato") +
    ggplot2::labs(
      title = "Visualised means of the variables in the model",
      subtitle = "",
      x = "Variables",
      y = "Values") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.subtitle = ggtext::element_markdown())

  y <- Data$y
  Linear <- lm(y ~ ., data = Data)
  e_Linear <- marginaleffects::avg_slopes(Linear, by = TRUE)
  Linears <- MLMetrics(Observed = Data, yvalue = y, Model = Linear,
                       K = 2, Name = "Linear", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_Linear <- estimate_plot(Model = Linear, limit = limit)

  DD0 <- cbind(xnum, xcha)
  DDn <- names(DD0)
  if(length(xcha) == 0){
    names(DD0) <- paste(c(paste(xnum_n, sep = ""),
                          paste(xcha_n,  sep = "")))
    MOD <- stats::formula(paste(c('y', paste(c(paste("`", xnum_n,"`", sep = "",
                                                     collapse = " * ")),
                                             collapse = "+")),
                                collapse = " ~ "))
  } else{
    names(DD0) <- paste(c(paste(xnum_n, sep = ""),
                          paste(xcha_n,  sep = "")))
    MOD <- stats::formula(paste(c('y', paste(c(paste("`", xnum_n,"`",
                                                     sep = "", collapse = " * "),
                                               paste("`", xcha_n, "`",  sep = "",
                                                     collapse = " + ")), collapse = "+")),
                                collapse = " ~ "))
  }

  Data <- cbind(y, DD0)

  LinearI <- lm(MOD, data = Data)
  e_LinearI <- marginaleffects::avg_slopes(LinearI, by = TRUE)
  LinearIs <- MLMetrics(Observed = Data, yvalue = y, Model = LinearI,
                        K = 2, Name = "Linear", Form = "LM", kutuf = 0,
                        TTy = "Number")
  v_LinearI <- estimate_plot(Model = LinearI, limit = limit)

  Data <- cbind(y, xnum, xcha)

  `reciprocal in Y` <- lm((1/(y + 1)) ~ ., data = Data)
  e_reciY <- marginaleffects::avg_slopes(`reciprocal in Y`, by = TRUE)
  reciY <- MLMetrics(Observed = Data, yvalue = y, Model = `reciprocal in Y`,
                     K = 2, Name = "Semilog in Y", Form = "LM", kutuf = 0,
                     TTy = "Number")
  v_reciY <- estimate_plot(Model = `reciprocal in Y`, limit = limit)

  Data <- cbind(y = log(y +1), xnum, xcha)
  y <- Data$y
  loglin <- lm(y ~ ., data = Data)
  e_loglin <- marginaleffects::avg_slopes(loglin, by = TRUE)
  loglins <- MLMetrics(Observed = Data, yvalue = y, Model = loglin,
                       K = 2, Name = "Semilog in X", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_loglin <- estimate_plot(Model = loglin, limit = limit)

  xnum1 <- xnum^2
  names(xnum1) <- paste("I", names(xnum1), sep = "")
  DD2 <- cbind(y, xnum, xcha, xnum1)
  Data <- DD2
  y <- Data$y

  quadratic <- lm(y ~ ., data = Data)
  e_quadratic <- marginaleffects::avg_slopes(quadratic, by = TRUE)
  quadratics <- MLMetrics(Observed = Data, yvalue = y, Model = quadratic,
                          K = 2, Name = "Quadratic", Form = "LM", kutuf = 0,
                          TTy = "Number")
  v_quadratic <- estimate_plot(Model = quadratic, limit = limit)

  xnum2 <- xnum^3
  names(xnum2) <- paste("IC", names(xnum), sep = "")
  Data <- cbind(DD2, xnum2)
  y <- Data$y

  cube <- lm(y ~ ., data = Data)
  e_cube <- marginaleffects::avg_slopes(cube, by = TRUE)
  cubes <- MLMetrics(Observed = Data, yvalue = y, Model = cube,
                     K = 2, Name = "Cube", Form = "LM", kutuf = 0,
                     TTy = "Number")
  v_cube <- estimate_plot(Model = cube, limit = limit)

  xnum3 <- xnum^.5
  names(xnum3) <- paste("I", names(xnum3), sep = "")
  DD3 <- cbind(y, xnum, xcha, xnum3)
  Data <- DD3
  y <- Data$y

  `square root` <- lm(y ~ ., data = Data)
  e_square <- marginaleffects::avg_slopes(`square root`, by = TRUE)
  squares <- MLMetrics(Observed = Data, yvalue = y, Model = `square root`,
                       K = 2, Name = "Square root", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_square <- estimate_plot(Model = `square root`, limit = limit)

  xnum4 <- xnum^(1/3)
  names(xnum4) <- paste("I", names(xnum4), sep = "")
  DD4 <- cbind(y, xnum, xcha, xnum4)
  Data <- DD4
  y <- Data$y

  `cubic root` <- lm(y ~ ., data = Data)
  e_cubic <- marginaleffects::avg_slopes(`cubic root`, by = TRUE)
  cubics <- MLMetrics(Observed = Data, yvalue = y, Model = `cubic root`,
                      K = 2, Name = "Cubic root", Form = "LM", kutuf = 0,
                      TTy = "Number")
  v_cubic <- estimate_plot(Model = `cubic root`, limit = limit)

  xnum5 <- log(xnum + 1)
  DD5 <- cbind(y, xnum5, xcha)
  Data <- DD5
  y <- Data$y

  linlog <- lm(y ~ ., data = Data)
  e_linlog <- marginaleffects::avg_slopes(linlog, by = TRUE)
  linlogs <- MLMetrics(Observed = Data, yvalue = y, Model = linlog,
                       K = 2, Name = "Semilog in X", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_linlog <- estimate_plot(Model = linlog, limit = limit)

  names(xnum5) <- paste("I", names(xnum5), sep = "")
  DD6 <- cbind(y, xnum, xcha, xnum5 )
  Data <- DD6
  y <- Data$y

  perlog <- lm(y ~ ., data = Data)
  e_perlog <- marginaleffects::avg_slopes(perlog, by = TRUE)
  perlogs <- MLMetrics(Observed = Data, yvalue = y, Model = perlog,
                       K = 2, Name = "Mixed-power", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_perlog <- estimate_plot(Model = perlog, limit = limit)

  DD7 <- cbind(y = y, xnum)
  DD7 <- log(DD7 + 1)
  Data <- cbind(DD7, xcha)
  y = Data$y

  loglog <- lm(y ~ ., data = Data)
  e_loglog <- marginaleffects::avg_slopes(loglog, by = TRUE)
  loglogs <- MLMetrics(Observed = Data, yvalue = y, Model = loglog,
                       K = 2, Name = "Cobb Douglas", Form = "LM", kutuf = 0,
                       TTy = "Number")
  v_loglog <- estimate_plot(Model = loglog, limit = limit)

  xnum6 <- log(xnum + 1)
  xnum6_n <- names(xnum6)
  xnum7 <- xnum6^2
  xnum7_n <- names(xnum7)

  DD7 <- cbind(xnum6, xnum7, xcha)
  if(length(xcha) == 0){
    names(DD7) <- paste(c(paste(xnum6_n, sep = ""),
                          paste("I", xnum7_n, sep = ""),
                          paste(xcha_n,  sep = "")))
    MOD <- stats::formula(paste(c("y", paste(c(paste("`", xnum6_n, "`",sep = "",
                                                     collapse = "*"),
                                               paste("`", "I", xnum7_n, "`", sep = "",
                                                     collapse = "+")), collapse = "+")),
                                collapse = " ~ "))
  } else{
    names(DD7) <- paste(c(paste(xnum6_n, sep = ""),
                          paste("I", xnum7_n, sep = ""), paste(xcha_n,  sep = "")))
    MOD <- stats::formula(paste(c("y", paste(c(paste("`", xnum6_n, "`",sep = "",
                                                     collapse = "*"),
                                               paste("`", "I", xnum7_n, "`", sep = "",
                                                     collapse = "+"),
                                               paste("`", xcha_n, "`",  sep = "",
                                                     collapse = " + ")), collapse = "+")),
                                collapse = " ~ "))
  }

  Data <- cbind(y = log(y + 1), DD7)
  y <- Data$y

  translog <- lm(MOD, data = Data)
  e_translog <- marginaleffects::avg_slopes(translog, by = TRUE)
  translogs <- MLMetrics(Observed = Data, yvalue = y, Model = translog,
                         K = 2, Name = "Translog", Form = "LM", kutuf = 0,
                         TTy = "Number")
  v_translog <- estimate_plot(Model = translog, limit = limit)

  xnum8 <- 1/(xnum + 1)
  Data <- cbind(y = y, xnum8, xcha)
  y <- Data$y

  `reciprocal in X` <- lm(y ~ ., data = Data)
  e_reciX <- marginaleffects::avg_slopes(`reciprocal in X`, by = TRUE)
  reciX <- MLMetrics(Observed = Data, yvalue = y, Model = `reciprocal in X`,
                     K = 2, Name = "Inverse in X", Form = "LM", kutuf = 0,
                     TTy = "Number")
  v_reciX <- estimate_plot(Model = `reciprocal in X`, limit = limit)

  Data$y <- y <- 1/(y + 1)

  `double reciprocal` <- lm(y ~ ., data = Data)
  e_reciD <- marginaleffects::avg_slopes(`double reciprocal`, by = TRUE)
  reciD <- MLMetrics(Observed = Data, yvalue = y, Model = `double reciprocal`,
                     K = 2, Name = "Inverse in Y & X", Form = "LM", kutuf = 0,
                     TTy = "Number")
  v_reciD <- estimate_plot(Model = `double reciprocal`, limit = limit)

  mod <- mod
  Test <- Test
  AA <- dim(Test)

  if(is.null(AA)){
    Test = Test
  } else {
    Test <- Test
    xt <- Test[, -1]

    xnum <- xt %>%
      dplyr::select_if(is.numeric)
    xnum_n <- names(xnum)

    xcha <- xt %>%
      dplyr::select_if(is.character)
    xcha_n <- names(xcha)

    xnum1 <- xnum^2
    names(xnum1) <- paste("I", names(xnum1), sep = "")
    Test_q <- cbind(xnum, xcha, xnum1)

    xnum2 <- xnum^3
    names(xnum2) <- paste("IC", names(xnum), sep = "")
    Test_c <- cbind(Test_q, xnum2)

    xnum3 <- xnum^.5
    names(xnum3) <- paste("I", names(xnum3), sep = "")
    Test_s <- cbind(xnum, xcha, xnum3)

    xnum4 <- xnum^(1/3)
    names(xnum4) <- paste("I", names(xnum4), sep = "")
    Test_cr <- cbind(xnum, xcha, xnum4)

    xnum5 <- log(xnum + 1)
    Test_l <- cbind(xnum5, xcha)

    names(xnum5) <- paste("I", names(xnum5), sep = "")
    Test_p <- cbind(xnum, xcha, xnum5)

    DD7 <- log(xnum + 1)
    Test_ll <- cbind(DD7, xcha)

    xnum6 <- log(xnum + 1)
    xnum6_n <- names(xnum6)
    xnum7 <- xnum6^2
    xnum7_n <- names(xnum7)
    Test_t <- cbind(xnum6, xnum7, xcha)
    names(Test_t) <- paste(c(paste(xnum6_n, sep = ""),
                             paste("I", xnum7_n, sep = ""), paste(xcha_n,  sep = "")))

    xnum8 <- 1/(xnum + 1)
    Test_X <- cbind(xnum8, xcha)
  }

  if (!mod %in% 0:6) {
    message("Module out of range")
    stop(message(paste("Choose between", 0,
                       "and", 6)))
  }
  if(mod == 5){
    e_list <- list(Linear                   = e_Linear,
                   `Reciprocal in X`         = e_reciX,
                   `Reciprocal in Y`         = e_reciY,
                   `Double reciprocal`       = e_reciD)
    e_table <- modelsummary::modelsummary(e_list,
                                          shape = term : contrast ~ model,
                                          stars = TRUE)

    m_list <- list(Linear                    = Linear,
                   `Reciprocal in X`         = `reciprocal in X`,
                   `Reciprocal in Y`         = `reciprocal in Y`,
                   `Double reciprocal`       = `double reciprocal`)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, `reciprocal in X`, `reciprocal in X`, `double reciprocal`)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <- modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         `Reciprocal in X`           = fitted.values(`reciprocal in X`),
                         `Reciprocal in Y`           = 1/fitted.values(`reciprocal in Y`),
                         `Double reciprocal`         = 1/fitted.values(`double reciprocal`))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              `Reciprocal in X`         = predict(`reciprocal in X`),
                              `Reciprocal in Y`         = 1/predict(`reciprocal in Y`),
                              `Double reciprocal`       = 1/predict(`double reciprocal`))
    }else{
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Reciprocal in X`         = predict(`reciprocal in X`, Test_X),
                              `Reciprocal in Y`         = 1/predict(`reciprocal in Y`, Test),
                              `Double reciprocal`       = 1/predict(`double reciprocal`, Test_X))
    }

    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               `Reciprocal in X`         = `reciprocal in X`[["effects"]],
                               `Reciprocal in Y`         = 1/`reciprocal in Y`[["effects"]],
                               `Double reciprocal`       = 1/`double reciprocal`[["effects"]])

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                    = Linears,
                        `Reciprocal in X`         = reciX,
                        `Reciprocal in Y`         = reciY,
                        `Double reciprocal`       = reciD)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Inverse y" = `reciprocal in Y`,
      "Inverse x" = `reciprocal in X`,
      "Inverse y & x" = `double reciprocal`,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Inverse y" = v_reciY,
      "Significant plot of Inverse x" = v_reciX,
      "Significant plot of Inverse y & x" = v_reciD,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Table" = e_table,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else if (mod == 4){
    e_list <- list(Linear                    = e_Linear,
                   `Square root`             = e_square,
                   `Cubic root`              = e_cubic)
    e_table <- modelsummary::modelsummary(e_list,
                                          shape = term : contrast ~ model,
                                          stars = TRUE)

    m_list <- list(Linear                    = Linear,
                   `Square root`             = `square root`,
                   `Cubic root`              = `cubic root`)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, `square root`, `cubic root`)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <-modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         `Square root`               = fitted.values(`square root`),
                         `Cubic root`                = fitted.values(`cubic root`))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              `Square root`             = predict(`square root`),
                              `Cubic root`              = predict(`cubic root`))
    }else{
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Square root`             = predict(`square root`, Test_s),
                              `Cubic root`              = predict(`cubic root`, Test_cr))
    }

    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               `Square root`             = `square root`[["effects"]],
                               `Cubic root`              = `cubic root`[["effects"]])

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                    = Linears,
                        `Square root`             = squares,
                        `Cubic root`              = cubics)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Square root" = `square root`,
      "Cubic root" = `cubic root`,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Square root" = v_square,
      "Significant plot of Cubic root" = v_cubic,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Table" = e_table,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else if(mod == 3){
    e_list <- list(Linear                    = e_Linear,
                   Quadratic                 = e_quadratic,
                   Cubic                     = e_cube)
    e_table <- modelsummary::modelsummary(e_list,
                                          shape = term : contrast ~ model,
                                          stars = TRUE)

    m_list <- list(Linear                    = Linear,
                   Quadratic                 = quadratic,
                   Cubic                     = cube)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, quadratic, cube)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <- modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         Quadratic                   = fitted.values(quadratic),
                         Cubic                       = fitted.values(cube))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              Quadratic                 = predict(quadratic),
                              Cubic                     = predict(cube))
    }else{
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Square root`             = predict(quadratic, Test_s),
                              `Cubic root`              = predict(cube, Test_c))
    }

    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               Quadratic                 = quadratic[["effects"]],
                               Cubic                     = cube[["effects"]])

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                    = Linears,
                        Quadratic                 = quadratics,
                        Cubic                     = cubes)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Quadratic" = quadratic,
      "Cubic model" = cube,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Quadratic" = v_quadratic,
      "Significant plot of Cubic model" = v_cube,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Table" = e_table,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else if(mod == 2){
    e_list <- list(Linear                    = e_Linear,
                   `Cobb Douglas`            = e_loglog,
                   Linlog                    = e_linlog,
                   Loglin                    = e_loglin,
                   `Mixed-power`             = e_perlog,
                   Translog                  = e_translog)
    e_table <- modelsummary::modelsummary(e_list,
                                          shape = term : contrast ~ model,
                                          stars = TRUE)

    m_list <- list(Linear                    = Linear,
                   `Cobb Douglas`            = loglog,
                   Linlog                    = linlog,
                   Loglin                    = loglin,
                   `Mixed-power`            = perlog,
                   Translog                  = translog)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, loglog, perlog, translog, linlog, loglin)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <- modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         `Cobb Douglas`             = exp(fitted.values(loglog)),
                         Translog                    = exp(fitted.values(translog)),
                         `Mixed-power`               = exp(fitted.values(perlog)),
                         Linlog                      = fitted.values(linlog),
                         Loglin                      = exp(fitted.values(loglin)))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              `Cobb Douglas`            = exp(predict(loglog)),
                              Translog                  = exp(predict(translog)),
                              `Mixed-power`             = exp(predict(perlog)),
                              Linlog                    = predict(linlog),
                              Loglin                    = exp(predict(loglin)))
    }else{
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Cobb Douglas`            = exp(predict(loglog, Test_ll)),
                              Translog                  = exp(predict(translog, Test_t)),
                              `Mixed-power`             = exp(predict(perlog, Test_p)),
                              Linlog                    = predict(linlog, Test_l),
                              Loglin                    = exp(predict(loglin, Test)))
    }



    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               `Cobb Douglas`            = exp(loglog[["effects"]]),
                               Translog                  = exp(translog[["effects"]]),
                               `Mixed-power`             = exp(perlog[["effects"]]),
                               Linlog                    = linlog[["effects"]],
                               Loglin                    = exp(loglin[["effects"]]))

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                    = Linears,
                        `Cobb Douglas`            = loglogs,
                        Linlog                    = linlogs,
                        Loglin                    = loglins,
                        `Mixed-power`             = perlogs,
                        Translog                  = translogs)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Semilog" = linlog,
      "Growth" = loglin,
      "Double Log" = loglog,
      "Mixed-power model" = perlog,
      "Translog model" = translog,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Semilog" = v_linlog,
      "Significant plot of Growth" = v_loglin,
      "Significant plot of Double Log" = v_loglog,
      "Significant plot of Mixed-power model" = v_perlog,
      "Significant plot of Translog model" = v_translog,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Table" = e_table,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else if(mod == 1){
    e_list <- list(Linear                    = e_Linear,
                   `Linear with interaction` = e_LinearI)
    e_table <- modelsummary::modelsummary(e_list,
                                          shape = term : contrast ~ model,
                                          stars = TRUE)

    m_list <- list(Linear                    = Linear,
                   `Linear with interaction` = LinearI)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, LinearI)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <- modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         `Linear with interaction`  = fitted.values(LinearI))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              `Linear with interaction` = predict(LinearI))
    }else{
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Linear with interaction` = predict(LinearI, Test))
    }

    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               `Linear with interaction` = LinearI[["effects"]])

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                   = Linears,
                        `Linear with interaction` = LinearIs)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Linear with interaction" = LinearI,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Linear with interaction" = v_LinearI,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Table" = e_table,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else if(mod == 0){
    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }else{
    m_list <- list(Linear                    = Linear,
                   `Cobb Douglas`            = loglog,
                   Linlog                    = linlog,
                   Loglin                    = loglin,
                   `Reciprocal in X`         = `reciprocal in X`,
                   `Reciprocal in Y`         = `reciprocal in Y`,
                   `Double reciprocal`       = `double reciprocal`,
                   Quadratic                 = quadratic,
                   `Square root`             = `square root`,
                   `Cubic root`              = `cubic root`,
                   Cubic                     = cube,
                   `Mixed-power`            = perlog,
                   Translog                  = translog,
                   `Linear with interaction` = LinearI)
    ModelTable <- modelsummary::modelsummary(m_list, stars = TRUE)

    Anova <- stats::anova(Linear, LinearI, loglog, perlog, translog, linlog, loglin, quadratic,
                          cube, `square root`, `cubic root`, `reciprocal in X`,
                          `reciprocal in X`, `double reciprocal`)
    Anova <- tibble::rownames_to_column(Anova, var = "Model")
    Anova <- modelsummary::datasummary_df(Anova, stars = TRUE)

    Fitted <- data.frame(Observed                   = y1,
                         Linear                     = fitted.values(Linear),
                         `Linear with interaction`  = fitted.values(LinearI),
                         `Cobb Douglas`             = exp(fitted.values(loglog)),
                         Translog                    = exp(fitted.values(translog)),
                         `Mixed-power`               = exp(fitted.values(perlog)),
                         Linlog                      = fitted.values(linlog),
                         Loglin                      = exp(fitted.values(loglin)),
                         Quadratic                   = fitted.values(quadratic),
                         Cubic                       = fitted.values(cube),
                         `Square root`               = fitted.values(`square root`),
                         `Cubic root`                = fitted.values(`cubic root`),
                         `Reciprocal in X`           = fitted.values(`reciprocal in X`),
                         `Reciprocal in Y`           = 1/fitted.values(`reciprocal in Y`),
                         `Double reciprocal`         = 1/fitted.values(`double reciprocal`))

    Fitted <- tidyr::pivot_longer(Fitted, -Observed, names_to = "Model",
                                  values_to = "Fitted")

    fitted_long  <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(x = Observed,
                   y = Fitted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    fitted_wide  <- ggplot2::ggplot(Fitted) +
      ggplot2::aes(
        x = Observed,
        y = Fitted,
        fill = Model,
        colour = Model,
        group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    if(is.null(AA)){
      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear),
                              `Linear with interaction` = predict(LinearI),
                              `Cobb Douglas`            = exp(predict(loglog)),
                              Translog                  = exp(predict(translog)),
                              `Mixed-power`             = exp(predict(perlog)),
                              Linlog                    = predict(linlog),
                              Loglin                    = exp(predict(loglin)),
                              Quadratic                 = predict(quadratic),
                              Cubic                     = predict(cube),
                              `Square root`             = predict(`square root`),
                              `Cubic root`              = predict(`cubic root`),
                              `Reciprocal in X`         = predict(`reciprocal in X`),
                              `Reciprocal in Y`         = 1/predict(`reciprocal in Y`),
                              `Double reciprocal`       = 1/predict(`double reciprocal`))
    }else{

      Predicted <- data.frame(Observed                  = y1,
                              Linear                    = predict(Linear, Test),
                              `Linear with interaction` = predict(LinearI, Test),
                              `Cobb Douglas`            = exp(predict(loglog, Test_ll)),
                              Translog                  = exp(predict(translog, Test_t)),
                              `Mixed-power`             = exp(predict(perlog, Test_p)),
                              Linlog                    = predict(linlog, Test_l),
                              Loglin                    = exp(predict(loglin, Test)),
                              Quadratic                 = predict(quadratic, Test_q),
                              Cubic                     = predict(cube, Test_c),
                              `Square root`             = predict(`square root`, Test_s),
                              `Cubic root`              = predict(`cubic root`, Test_cr),
                              `Reciprocal in X`         = predict(`reciprocal in X`, Test_X),
                              `Reciprocal in Y`         = 1/predict(`reciprocal in Y`, Test),
                              `Double reciprocal`       = 1/predict(`double reciprocal`, Test_X))
    }

    Predicted <- tidyr::pivot_longer(Predicted, -Observed, names_to = "Model", values_to = "Predicted")

    Predicted_long  <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Predicted_wide <- ggplot2::ggplot(Predicted) +
      ggplot2::aes(x = Observed,
                   y = Predicted,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    Effects <-      data.frame(Observed                  = y1,
                               Linear                    = Linear[["effects"]],
                               `Linear with interaction` = LinearI[["effects"]],
                               `Cobb Douglas`            = exp(loglog[["effects"]]),
                               Translog                  = exp(translog[["effects"]]),
                               `Mixed-power`             = exp(perlog[["effects"]]),
                               Linlog                    = linlog[["effects"]],
                               Loglin                    = exp(loglin[["effects"]]),
                               Quadratic                 = quadratic[["effects"]],
                               Cubic                     = cube[["effects"]],
                               `Square root`             = `square root`[["effects"]],
                               `Cubic root`              = `cubic root`[["effects"]],
                               `Reciprocal in X`         = `reciprocal in X`[["effects"]],
                               `Reciprocal in Y`         = 1/`reciprocal in Y`[["effects"]],
                               `Double reciprocal`       = 1/`double reciprocal`[["effects"]])

    Effects <- tidyr::pivot_longer(Effects, -Observed, names_to = "Model", values_to = "Effects")

    Effects_long  <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal()

    Effects_wide  <- ggplot2::ggplot(Effects) +
      ggplot2::aes(x = Observed,
                   y = Effects,
                   fill = Model,
                   colour = Model,
                   group = Model) +
      ggplot2::geom_line(linewidth = 1.5) +
      ggplot2::scale_fill_hue(direction = 1) +
      ggplot2::scale_color_hue(direction = 1) +
      ggplot2::theme_minimal() +
      ggplot2::facet_wrap(ggplot2::vars(Model))

    evaluation <- cbind(Linear                    = Linears,
                        `Cobb Douglas`            = loglogs,
                        Linlog                    = linlogs,
                        Loglin                    = loglins,
                        `Reciprocal in X`         = reciX,
                        `Reciprocal in Y`         = reciY,
                        `Double reciprocal`       = reciD,
                        Quadratic                 = quadratics,
                        `Square root`             = squares,
                        `Cubic root`              = cubics,
                        Cubic                     = cubes,
                        `Mixed-power`             = perlogs,
                        Translog                  = translogs,
                        `Linear with interaction` = LinearIs)
    evaluation <- data.frame(evaluation)
    evaluation <- tibble::rownames_to_column(evaluation, var = "Name")
    evaluation <- modelsummary::datasummary_df(evaluation)

    e_list <- list(Linear                      = e_Linear,
                   `Linear with interaction` = e_LinearI)
    e_table1 <- modelsummary::modelsummary(e_list,
                                           shape = term : contrast ~ model,
                                           stars = TRUE)

    e_list <- list(`Cobb Douglas`            = e_loglog,
                   Linlog                    = e_linlog,
                   Loglin                    = e_loglin,
                   `Mixed-power`             = e_perlog,
                   Translog                  = e_translog)
    e_table2 <- modelsummary::modelsummary(e_list,
                                           shape = term : contrast ~ model,
                                           stars = TRUE)

    e_list <- list(`Reciprocal in X`         = e_reciX,
                   `Reciprocal in Y`         = e_reciY,
                   `Double reciprocal`       = e_reciD)
    e_table3 <- modelsummary::modelsummary(e_list,
                                           shape = term : contrast ~ model,
                                           stars = TRUE)

    e_list <- list(Quadratic                 = e_quadratic,
                   Cubic                     = e_cube)
    e_table4 <- modelsummary::modelsummary(e_list,
                                           shape = term : contrast ~ model,
                                           stars = TRUE)

    e_list <- list(`Square root`             = e_square,
                   `Cubic root`              = e_cubic)
    e_table5 <- modelsummary::modelsummary(e_list,
                                           shape = term : contrast ~ model,
                                           stars = TRUE)
    e_tables <- list(e_table1, e_table2, e_table3, e_table4, e_table5)
    results <- list(
      "Visual means of the numeric variable" = e_meanplot,
      "Correlation plot" = e_corplot,
      "Linear" = Linear,
      "Linear with interaction" = LinearI,
      "Semilog" = linlog,
      "Growth" = loglin,
      "Double Log" = loglog,
      "Mixed-power model" = perlog,
      "Translog model" = translog,
      "Quadratic" = quadratic,
      "Cubic model" = cube,
      "Inverse y" = `reciprocal in Y`,
      "Inverse x" = `reciprocal in X`,
      "Inverse y & x" = `double reciprocal`,
      "Square root" = `square root`,
      "Cubic root" = `cubic root`,
      "Significant plot of Linear" = v_Linear,
      "Significant plot of Linear with interaction" = v_LinearI,
      "Significant plot of Semilog" = v_linlog,
      "Significant plot of Growth" = v_loglin,
      "Significant plot of Double Log" = v_loglog,
      "Significant plot of Mixed-power model" = v_perlog,
      "Significant plot of Translog model" = v_translog,
      "Significant plot of Quadratic" = v_quadratic,
      "Significant plot of Cubic model" = v_cube,
      "Significant plot of Inverse y" = v_reciY,
      "Significant plot of Inverse x" = v_reciX,
      "Significant plot of Inverse y & x" = v_reciD,
      "Significant plot of Square root" = v_square,
      "Significant plot of Cubic root" = v_cubic,
      "Model Table" = ModelTable,
      "Machine Learning Metrics" =  evaluation,
      "Marginal effecrs Tables" = e_tables,
      "Fitted plots long format" = fitted_long,
      "Fitted plots wide format" = fitted_wide,
      "Prediction plots long format" = Predicted_long,
      "Prediction plots wide format" = Predicted_wide,
      "Naive effects plots long format" = Effects_long,
      "Naive effects plots wide format" = Effects_wide,
      "Summary of numeric variables" = KNN1,
      "Summary of character variables" = KKC)
  }
  return(results)
}
