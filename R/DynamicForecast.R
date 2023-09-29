#' Dynamic Forecast of Five Models and their Ensembles
#'
#' @description
#' The function estimates and predict models using time series dataset and provide subset forecasts within the length of trend. The recognized models are lm, smooth spline, polynomial splines with or without knots, quadratic polynomial,  and ARIMA. The robust output include the models' estimates, time-varying forecasts and plots  based on themes from ggplot. The main attraction of this function is the use of the newly introduced _equal number of trend (days, months, years) to estimate forecast from the model_. The function takes `daily, monthly and yearly data sets for now`.
#'
#' @param date A vector containing the dates for which the data is collected. Must be the same length with `series`. The date must be in 'YYYY-MM-DD'. If the data is monthly series, the recognized date format is the last day of the month of the dataset e.g. 2021-02-28. If the data is a yearly series, the recognized date format is the last day of the year of the data set e.g. 2020-12-31. There is no format for Quarterly data for now.
#' @param series A vector containing data for estimation and forecasting. Must be the same length with `date`.
#' @param x vector of optional dataset that is to be added to the model for forecasting. The modeling and forecasting is still done if not provided. Must be the same length with `series`.
#' @param BREAKS A vector of numbers indicating points of breaks for estimation of the spline models.
#' @param MaximumDate The date indicating the maximum date (last date) in the data frame, meaning that forecasting starts the next date following it. The date must be a recognized date format. Note that for forecasting, the date origin is set to 1970-01-01.
#' @param Trend The type of trend. There are three options **Day, Month and Year**.
#' @param Type The type of response variable. There are two options **Continuous and Integer**. For integer variable, the forecasts are constrained between the minimum and maximum value of the response variable.
#' @param Lenght The length for which the forecast would be made. If not given, would default to the length of the dataset i.e. sample size.
#' @param ... Additional arguments that may be passed to the function. If the maximum date is NULL which is is the default, it is set to the last date of the `series`. In the same way, `origin (origin = "YYYY-MM-DD")` to be used to position the date of the data is set to **1970-01-01** if it is not supplied as argument in order to properly date the forecasts.
#'
#' @import tidyverse
#' @importFrom stats lm
#' @importFrom stats fitted.values
#' @importFrom stats smooth.spline
#' @importFrom ModelMetrics rmse
#' @importFrom splines bs
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggsave
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_color_hue
#' @importFrom magrittr %>%
#' @importFrom formattable comma
#' @importFrom forecast auto.arima
#' @importFrom forecast forecast
#' @importFrom utils globalVariables
#' @importFrom zoo yearmon
#' @importFrom zoo as.Date
#' @importFrom lifecycle badge
#'
#' @name DynamicForecast
#' @export DynamicForecast
#'
#' @return A list with the following components:
#' \item{\code{Spline without knots}}{The estimated spline model without the breaks (knots).}
#' \item{\code{Spline with knots}}{The estimated spline model with the breaks (knots).}
#' \item{\code{Smooth Spline}}{The smooth spline estimates.}
#' \item{\code{ARIMA}}{Estimated Auto Regressive Integrated Moving Average model.}
#' \item{\code{Quadratic}}{The estimated quadratic polynomial model.}
#' \item{\code{Ensembled with equal weight}}{Estimated Ensemble model with equal weight given to each of the models. To get this, the fitted values of each of the models is divided by the number of models and summed together.}
#' \item{\code{Ensembled based on weight}}{Estimated Ensemble model based on weight of each model. To do this, the fitted values of each model served as independent variable and regressed against the trend with interaction among the variables.}
#' \item{\code{Ensembled based on summed weight}}{Estimated Ensemble model based on summed weight of each model. To do this, the fitted values of each model served as independent variable and is regressed against the trend.}
#' \item{\code{Ensembled based on weight of fit}}{Estimated Ensemble model. The fit of each model is measured by the rmse.}
#' \item{\code{Unconstrained Forecast}}{The forecast if the response variable is continuous. The number of forecasts is equivalent to the length of the dataset (equal days forecast).}
#' \item{\code{Constrained Forecast}}{The forecast if the response variable is integer. The number of forecasts is equivalent to the length of the dataset (equal days forecast).}
#' \item{\code{RMSE}}{Root Mean Square Error (rmse) for each forecast.}
#' \item{\code{Unconstrained forecast Plot}}{The combined plots of the unconstrained forecasts using ggplot. }
#' \item{\code{Constrained forecast Plot}}{The combined plots of the constrained forecasts using ggplot. }
#' \item{\code{Date}}{This is the date range for the forecast.}
#' \item{\code{Fitted plot}}{This is the plot of the fitted models.}
#' \item{\code{Estimated coefficients}}{This is the estimated coefficients of the various models in the forecast.}
#'
#' @aliases COVID19
#'
#' @examples
#' COVID19$Date <- zoo::as.Date(COVID19$Date, format = '%m/%d/%Y')
#'  #The date is formatted to R format
#' LEN <- length(COVID19$Case)
#' Dss <- seq(COVID19$Date[1], by = "day", length.out = LEN)
#'  #data length for forecast
#' ORIGIN = "2020-02-29"
#' lastdayfo21 <- Dss[length(Dss)] # The maximum length
#' Data <- COVID19[COVID19$Date <= lastdayfo21 - 28, ]
#' # desired length of forecast
#' BREAKS <- c(70, 131, 173, 228, 274) # The default breaks for the data
#' DynamicForecast(date = Data$Date, series = Data$Case,
#' BREAKS = BREAKS, MaximumDate = "2021-02-10",
#'  Trend = "Day", Length = 0, Type = "Integer", origin = ORIGIN)
#'
#' lastdayfo21 <- Dss[length(Dss)]
#' Data <- COVID19[COVID19$Date <= lastdayfo21 - 14, ]
#' BREAKS = c(70, 131, 173, 228, 274)
#' DynamicForecast(date = Data$Date, series = Data$Case,
#' BREAKS = BREAKS , MaximumDate = "2021-02-10",
#'  Trend = "Day", Length = 0, Type = "Integer", origin = ORIGIN)
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
utils::globalVariables(c("Spline without knots",
                         "Spline with knots",
                         "Smooth Spline",
                         "ARIMA",
                         "Quadratic",
                         "Ensembled with equal weight",
                         "Ensembled based on weight",
                         "Ensembled based on summed weight",
                         "Ensembled based on weight of fit","Date", "Day",
                         "Forecast", "Models", "Fitted values"))

lifecycle::badge("stable")
DynamicForecast <- function(date, series, x = 0, BREAKS = 0, origin = 0, MaximumDate, Trend,
                            Type, Length = 0, ...) {

  origin <- ifelse(origin == 0, "1970-01-01", origin)
  date <- zoo::as.Date(date, origin = origin)
  Series <- ss <- seq(1:length(series))

  if (x == 0){

    Data <- cbind(Date = date, series, Series)

    fit01  <- stats::lm(series ~ splines::bs(Series, knots = NULL))
    fit10   <- stats::lm(series ~ splines::bs(Series, knots = BREAKS))
    fit11  <- stats::smooth.spline(Series, series)
    fita1  <- forecast::auto.arima(series)
    fitpi1 <- stats::lm(series ~ Series + I(Series^2))
    Linear <-  stats::lm(series ~      Series)
    Semilog <- stats::lm(series ~      log(Series))
    Growth <-  stats::lm(log(series+1) ~ Series)
    }else{

      Data <- cbind(Date = date, series, Series, x)

      fit01  <- stats::lm(series ~ splines::bs(Series, knots = NULL) + x)
      fit10   <- stats::lm(series ~ splines::bs(Series, knots = BREAKS) + x)
      fit11  <- stats::smooth.spline(Series, series)
      fita1  <- forecast::auto.arima(series)
      fitpi1 <- stats::lm(series ~ Series + I(Series^2) + x)
      Linear <-  stats::lm(series ~      Series + x)
      Semilog <- stats::lm(series ~      log(Series) + x)
      Growth <-  stats::lm(log(series+1) ~ Series + x)
    }

  Estimates <- modelsummary::modelsummary(list(`Linear without knots` = fit01,
                    `Linear with knots` = fit10,
                    ARIMA = fita1,
                    `Quadratic polynomial` = fitpi1,
                    Linear = Linear,
                    Semilog = Semilog,
                    Growth = Growth), stars = TRUE)
  MaxDayDat <- 0
  if (is.null(MaximumDate)){
    Dss19 <- seq(Data$Series[1], by = 1, length.out = length(Series))
    Dss191 <- seq(max(Dss19)+1, by = 1, length.out = length(Series))
    DayDat0 <- zoo::as.Date(Dss19[length(Dss19)], origin = origin)
    DayDat1 <- zoo::as.Date(Dss191[length(Dss191)], origin = origin)
    MaxDayDat <- zoo::as.Date(DayDat0)
  }else{
    MaximumDate <- zoo::as.Date(MaximumDate)
  }

  if (MaximumDate == MaxDayDat){
    MaximumDate <- MaxDayDat
  }else{
    MaximumDate <- MaximumDate
  }

  if (Trend == "Day") {
    Dsf19 <- seq(zoo::as.Date(MaximumDate + lubridate::days(1)),
                 by = "day", length.out = length(series))
    Dsf19day01 <- format(Dsf19[1], format = "%b %d, %y")
    Dsf19daylast <- format(Dsf19[length(Dsf19)], format = "%b %d, %y")
  } else if (Trend == "Month") {
    Dsf19 <- seq(zoo::as.Date(MaximumDate + lubridate::month(1)),
                 by = "month", length.out = length(series))
    Dsf19day01 <- zoo::as.yearmon(Dsf19[1], "%b %y")
    Dsf19daylast <- zoo::as.yearmon(Dsf19[length(Dsf19)], "%b %y")
  } else {
    Dsf19 <- seq(zoo::as.Date(MaximumDate + lubridate::years(1)),
                 by = "year", length.out = length(series))
    Dsf19day01 <- format(zoo::as.Date(Dsf19[1]), "%Y")
    Dsf19daylast <- format(zoo::as.Date(Dsf19[length(Dsf19)]), "%Y")
  }

  Title <- paste(Dsf19day01, "-", Dsf19daylast, collapse="")
  Without.knots <- fitted.values(fit01)
  With.knots <- fitted.values(fit10)
  Smooth <- fitted.values(fit11)
  ARIMA <- fita1[["fitted"]]
  Quadratic <- fitted.values(fitpi1)
  Linear1  <- fitted.values(Linear)
  Semilog1 <- fitted.values(Semilog)
  Growth1  <- fitted.values(Growth)
  Fitted <- data.frame(cbind(date, Observed = series, Linear_without_knots = Without.knots,
                  Linear_with_knots = With.knots,
                  Smooth_spline = Smooth,
                  ARIMA = ARIMA,
                  Quadratic = Quadratic,
                  Linear = Linear1,
                  Semilog = Semilog1,
                  Grwoth = Growth1))
  Fitted <- tidyr::pivot_longer(Fitted, -date, names_to = "Models",
                        values_to = "Fitted values")
  Fitted$date <- zoo::as.Date(Fitted$date, origin = origin)

  Fit_plot <- ggplot2::ggplot(Fitted) +
    ggplot2::aes(x = date, y = `Fitted values`, colour = Models) +
    ggplot2::geom_line() +
    ggplot2::scale_color_manual(
      values = c(ARIMA = "#F8766D",
                 Grwoth = "#D39200",
                 Linear = "#93AA00",
                 Linear_with_knots = "#00BA38",
                 Linear_without_knots = "#00C19F",
                 Observed = "#00B9E3",
                 Quadratic = "#619CFF",
                 Semilog = "#DB72FB",
                 Smooth_spline = "#FF61C3")) +
    ggplot2::theme_light()

if (Length != 0){
    H = Length
  }else{
    H = length(Dsf19)
  }
  kk91   <- forecast::forecast(Without.knots,  h = H)
  kk091  <- forecast::forecast(With.knots,  h = H)
  kk191  <- forecast::forecast(Smooth,  h = H)
  kk1091 <- forecast::forecast(Quadratic, h = H)
  kk291  <- forecast::forecast(fita1, h = H)
  LinearF  <- forecast::forecast(Linear1, h = H)
  SemilogF <- forecast::forecast(Semilog1, h = H)
  GrowthF  <- forecast::forecast(Growth1, h = H)

  kk3091 <- (Without.knots + With.knots +
               Smooth + Quadratic +
               ARIMA)/5
  kk3191 <- forecast::forecast(kk3091, h = H)
  kk4091 <- stats::lm(Series~Without.knots * With.knots * Smooth *
                        Quadratic * ARIMA)
  kk4191 <- forecast::forecast(fitted.values(kk4091), h = H)
  kk6091 <- stats::lm(Series~Without.knots + With.knots + Smooth +
                        Quadratic + ARIMA)
  kk6191 <- forecast::forecast(fitted.values(kk6091), h = H)

  KK91 <- data.frame(cbind("Date" = Dsf19,"Series" = ss,
                              "Linear" = LinearF[["mean"]],
                              "Semilog" = SemilogF[["mean"]],
                              "Growth" = GrowthF[["mean"]],
                              "Without Knots" = kk91[["mean"]],
                              "Smooth spline" = kk091[["mean"]],
                              "With Knots" = kk191[["mean"]],
                              "Polynomial" = kk1091[["mean"]],
                              "Lower ARIMA" = kk291[["lower"]],
                              "Upper ARIMA" = kk291[["upper"]]))
  KK91 <- KK91[,-c(10,12)]
  names(KK91) <- c("Date", "Series", "Linear", "Semilog", "Growth",
                   "Without Knots", "Smooth spline", "With Knots",
                   "Polynomial", "Lower ARIMA", "Upper ARIMA")
  RMSE91L <- as.data.frame(cbind(
    "Linear" = ModelMetrics::rmse(series, Linear1),
    "Semilog" = ModelMetrics::rmse(series, Semilog1),
    "Growth" = ModelMetrics::rmse(series, Growth1)))

  RMSE91 <- c("Without knots" = ModelMetrics::rmse(series,
                                                   Without.knots),
              "Smooth Spline" = ModelMetrics::rmse(series, With.knots),
              "With knots" = ModelMetrics::rmse(series, Smooth),
              "Polynomial" = ModelMetrics::rmse(series, Quadratic),
              "Lower ARIMA" = ModelMetrics::rmse(series, ARIMA),
              "Upper ARIMA" = ModelMetrics::rmse(series, ARIMA))

  RMSE_weight91 <- as.list(RMSE91 / sum(RMSE91))
  KK91$Date <- zoo::as.Date(KK91$Date, origin = origin)
  KK91$`Ensembled with equal weight` <- kk3191[["mean"]]
  KK91$`Ensembled based on weight` <- kk4191[["mean"]]
  KK91$`Ensembled based on summed weight` <- kk6191[["mean"]]
  P_weight91 <- (Without.knots * RMSE_weight91$`Without knots`) +
    (With.knots * RMSE_weight91$`Smooth Spline`) +
    (Smooth * RMSE_weight91$`With knots`) +
    (Quadratic * RMSE_weight91$Polynomial) +
    (ARIMA * RMSE_weight91$`Lower ARIMA`)

  kk5191 <- forecast::forecast(P_weight91, h = H)
  KK91$`Ensembled based on weight of fit` <- kk5191[["mean"]]
  RMSE91$`Ensembled with equal weight` <-
    ModelMetrics::rmse(series, kk3091)
  RMSE91$`Ensembled based on weight` <-
    ModelMetrics::rmse(series, fitted.values(kk4091))
  RMSE91$`Ensembled based on summed weight` <-
    ModelMetrics::rmse(series, fitted.values(kk6091))
  RMSE91$`Ensembled based on weight of fit` <- ModelMetrics::rmse(Series,
                                                                  P_weight91)
  DDf91 <- c("Linear", "Semilog", "Growth", "Without knots", "Smooth Spline",
             "With knots", "Quadratic Polynomial",
             "Lower ARIMA", "Upper ARIMA",
             "Essembled with equal weight",
             "Essembled based on weight",
             "Essembled based on summed weight",
             "Essembled based on weight of fit" )
  Forcasts91 <- colSums(KK91[,-c(1,2)])
  Fore_f91 <- as.data.frame(cbind(DDf91, "Case" =
                                    formattable::comma(round(Forcasts91, 0))))
  RMSE_f91 <- c("Linear" =  round(RMSE91L$Linear, 2),
                "Semilog" = round(RMSE91L$Semilog, 2),
                "Growrh" = round(RMSE91L$Growth, 2),
                "Without knots" = round(RMSE91$`Without knots`, 2),
                "Smooth Spline"  = round(RMSE91$`Smooth Spline`, 2),
                "With knots"  = round(RMSE91$`With knots`, 2),
                "Polynomial"  = round(RMSE91$Polynomial, 2),
                "Lower ARIMA"  = round(RMSE91$`Lower ARIMA`, 2),
                "Upper ARIMA"  = round(RMSE91$`Upper ARIMA`, 2),
                "Ensembled with equal weight"  =
                  round(RMSE91$`Ensembled with equal weight`, 2),
                "Ensembled based on weight"  =
                  round(RMSE91$`Ensembled based on weight`, 2),
                "Ensembled based on weight"  =
                  round(RMSE91$`Ensembled based on summed weight`, 2),
                "Ensembled based on weight of fit"  =
                  round(RMSE91$`Ensembled based on weight of fit`, 2))
  RMSE_f91 <- cbind(DDf91, RMSE_f91)

  KK91 <-  tidyr::pivot_longer(KK91, -c(Date, Series), names_to = "Models",
                        values_to = "Forecast")
  KK91$Date <- zoo::as.Date(KK91$Date)
  KK0091 <- ggplot2::ggplot(KK91) +
    aes(x = Date, y = Forecast, colour = Models, group = Models) +
    geom_line(linewidth = 1L) +
    scale_color_hue() +
    theme_bw() +
    theme() +
    labs(title = Title,
         subtitle = " ",
         caption = " ")

  if (Type == "Continuous") {
    Fore_f91c  = NULL
    KK0091c    = NULL
  } else {
    lower = min(series)
    upper = max(series)
    kkF  <- forecast::forecast( scaledlogit(x = Without.knots,
                                                     lower = lower,
                                                     upper = upper), h = H)
    kkc <-  constrainedforecast(Model = kkF, lower = lower,
                                         upper = upper)
    kk0F <- forecast::forecast( scaledlogit(x = With.knots,
                                                     lower = lower,
                                                     upper = upper), h = H)
    kk0c <-  constrainedforecast(Model = kk0F, lower = lower,
                                          upper = upper)
    kk1F <- forecast::forecast( scaledlogit(x = Smooth,
                                                     lower = lower,
                                                     upper = upper), h = H)
    kk1c <-  constrainedforecast(Model = kk1F, lower = lower,
                                          upper = upper)
    kk10F <- forecast::forecast( scaledlogit(x = Quadratic,
                                                      lower = lower,
                                                      upper = upper), h = H)
    kk10c <-  constrainedforecast(Model = kk10F, lower = lower,
                                           upper = upper)
    kk2F <- forecast::forecast( scaledlogit(x = ARIMA, lower = lower,
                                                     upper = upper), h = H)
    kk2c <-  constrainedforecast(Model = kk2F, lower = lower,
                                          upper = upper)
    kk31F <- forecast::forecast( scaledlogit(x = kk3091,
                                                      lower = lower,
                                                      upper = upper), h = H)
    kk31c <-  constrainedforecast(Model = kk31F, lower = lower,
                                           upper = upper)
    kk41F <-
      forecast::forecast( scaledlogit(x = fitted.values(kk4091),
                                               lower = lower, upper = upper),
                         h = H)
    kk41c <-  constrainedforecast(Model = kk41F, lower = lower,
                                           upper = upper)
    kk61F <-
      forecast::forecast( scaledlogit(x = fitted.values(kk6091),
                                               lower = lower, upper = upper),
                         h = H)
    kk61c <-  constrainedforecast(Model = kk61F, lower = lower,
                                           upper = upper)
    KK91c <- as.data.frame(cbind("Date" = Dsf19, "Series" = 1:length(Dsf19),
                                 "Linear" = LinearF[["mean"]],
                                 "Semilog" = SemilogF[["mean"]],
                                 "Growth" = GrowthF[["mean"]]))
    KK91c$`Smooth spline 80%` = kk1c$Lower80
    KK91c$`Smooth spline 95%` = kk1c$Upper95
    KK91c$`Without knots 80%` = kk0c$Lower80
    KK91c$`Without knots 95%` = kk0c$Upper95
    KK91c$`With knots 80%`    = kkc$Lower80
    KK91c$`With knots 95%`    = kkc$Upper95
    KK91c$`Polynomial 80%`    = kk10c$Lower80
    KK91c$`Polynomial 95%`    = kk10c$Upper95
    KK91c$`ARIMA 80%`         = kk2c$Lower80
    KK91c$`ARIMA 95%`         = kk2c$Upper95
    KK91c$Date <- zoo::as.Date(KK91c$Date, origin = origin)
    DDfc <- c("Linear", "Semilog", "Growth", "Without knots 80%",
              "Without knots 95%", "Smooth Spline 80%",
              "Smooth Spline 95%", "With knots 80%", "With knots 95%",
              "Quadratic Polynomial 80%", "Quadratic Polynomial 95%",
              "ARIMA 80%", "ARIMA 95%",
              "Essembled with equal weight 80%",
              "Essembled with equal weight 95%",
              "Essembled based on weight 80%",
              "Essembled based on weight 95%",
              "Essembled based on summed weight 80%",
              "Essembled based on summed weight 95%",
              "Essembled based on weight of fit 80%",
              "Essembled based on weight of fit 95%" )
    KK91c$`Essembled with equal weight 80%` <- kk31c$Lower80
    KK91c$`Essembled with equal weight 95%` <- kk31c$Upper95
    KK91c$`Essembled based on weight 80%` <- kk41c$Lower80
    KK91c$`Essembled based on weight 95%` <- kk41c$Upper95
    KK91c$`Essembled based on summed weight 80%` <- kk61c$Lower80
    KK91c$`Essembled based on summed weight 95%` <- kk61c$Upper95
    kk51F <- forecast::forecast( scaledlogit(x = P_weight91,
                                                      lower = lower,
                                                      upper = upper), h = H)
    kk51c <-  constrainedforecast(Model = kk51F, lower = lower,
                                           upper = upper)
    KK91c$`Essembled based on weight of fit 80%` <- kk51c$Lower80
    KK91c$`Essembled based on weight of fit 95%` <- kk51c$Upper95
    Forcasts91c <- colSums(KK91c[,-c(1,2)])
    Fore_f91c <- as.data.frame(cbind("Model" = DDfc,
                                     "Confirmed cases" =
                                       comma(round(Forcasts91c, 0))))
    KK191c <- KK91c %>%
      tidyr::pivot_longer(-c(Date, Series), names_to = "Models",
                          values_to = "Forecast")
    KK191c$Date <- zoo::as.Date(KK191c$Date)
    KK0091c <- ggplot2::ggplot(KK191c) +
      ggplot2::aes(x = Date, y = Forecast, colour = Models, group = Models) +
      ggplot2::geom_line(linewidth = 1L) +
      ggplot2::scale_color_hue() +
      ggplot2::theme_bw() +
      ggplot2::theme() +
      ggplot2::labs(title = Title,
           subtitle = " ",
           caption = " ")
  }

  results <- list(
    "Spline without knots" = fit01,
    "Spline with knots" = fit10,
    "Smooth Spline" = fit11,
    "ARIMA" = fita1,
    "Quadratic" = fitpi1,
    "Ensembled with equal weight" = kk3091,
    "Ensembled based on weight" = kk4091,
    "Ensembled based on summed weight" = kk6091,
    "Ensembled based on weight of fit" = P_weight91,
    "Unconstrained Forecast" = Fore_f91,
    "RMSE"     = RMSE_f91,
    "Unconstrained forecast Plot"     = KK0091,
    "Date"     = Title,
    "Constrained Forecast" = Fore_f91c,
    "Constrained forecast Plot"     = KK0091c,
    "Fitted plot" = Fit_plot,
    "Estimated coefficients" = Estimates)
  return(results)
}
