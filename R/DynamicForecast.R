#' Dynamic Forecast of Five Models and their Ensembles
#'
#' The function estimates and predict models using time series dataset and provide subset forecasts within the length of trend. The recognized models are lm, smooth spline, polynomial splines with or without knots, quadratic polynomial,  and ARIMA. The robust output include the models' estimates, time-varying forecasts and plots  based on themes from ggplot. The main attraction of this package is the use of the newly introduced _equal number days (time, trend) forecast_. The function takes `daily, monthly and yearly datasets for now`.
#'
#' @param Data A two column (Date, Variables) dataset for the estimation. The date must be in format recognized by R i.e. 'YYYY-MM-DD'. If the data is monthly series, the recognized date format is the last day of the maximum month of the dataset e.g. 2021-02-28. If the data is a yearly series, the recognized date format is the last day of the maximum year of the dataset e.g. 2020-12-31. Quarterly data is not available. The Response **y** variable must be specified. If there are other variables in the data, then the date must be in column one.
#' @param BREAKS A vector of numbers indicating points of breaks for estimation of the spline models.
#' @param MaximumDate The date indicating the maximum date (last date) in the data frame, meaning that forecasting starts the next date following it. The date must be a recognized date format. Note that for forecasting, the date origin is set to 1970-01-01.
#' @param Trend The type of trend. There are three options **Day, Month and Year**.
#' @param Type The type of response variable. There are two options **Continuous and Integer**. For integer variable, the forecasts are constrained between the minimum and maximum value of the response variable.
#' @param Lenght The length for which the forecast would be made. If not given, would default to the length of the dataset i.e. sample size.
#' @param ... Additional arguments that may be passed to the function if the maximum date is NULL which is advisable. For example, the date of origin (origin = "YYYY-MM-DD") of the data may be specified in order to properly date the forecast.
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
#' @importFrom xlsx write.xlsx2
#' @importFrom readxl read_excel
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
#' DynamicForecast(Data = Data, BREAKS = BREAKS, MaximumDate = "2021-02-10",
#'  Trend = "Day", Length = 0, Type = "Integer", origin = ORIGIN)
#'
#' lastdayfo21 <- Dss[length(Dss)]
#' Data <- COVID19[COVID19$Date <= lastdayfo21 - 14, ]
#' BREAKS = c(70, 131, 173, 228, 274)
#' DynamicForecast(Data = Data, BREAKS = BREAKS , MaximumDate = "2021-02-10",
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
                         "Forecast", "Models"))

lifecycle::badge('experimental')
DynamicForecast <- function(Data, BREAKS, MaximumDate, Trend, Type,
                            Length = 0, ...) {
  Data$Day <- ss <- seq(1:length(Data$Case))
  fit01  <- stats::lm(Case ~ splines::bs(Day, knots = NULL), data = Data)
  fit10   <- stats::lm(Case ~ splines::bs(Day, knots = BREAKS),
                       data = Data)
  fit11  <- stats::smooth.spline(Data$Day, Data$Case)
  fita1  <- forecast::auto.arima(Data$Case)
  fitpi1 <- stats::lm(Case ~ Day + I(Day^2), data = Data)
  Linear <-  stats::lm(Case ~      Day, data = Data)
  Semilog <- stats::lm(Case ~      log(Day), data = Data)
  Growth <-  stats::lm(log(Case+1) ~ Day, data = Data)

  MaxDayDat <- 0
  if (is.null(MaximumDate)){
    Dss19 <- seq(Data$Day[1], by = 1, length.out = length(Data$Day))
    Dss191 <- seq(max(Dss19)+1, by = 1, length.out = length(Data$Day))
    DayDat0 <- zoo::as.Date(Dss19[length(Dss19)], origin = ...)
    DayDat1 <- zoo::as.Date(Dss191[length(Dss191)], origin = ...)
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
                 by = "day", length.out = length(Data$Case))
    Dsf19day01 <- format(Dsf19[1], format = "%b %d, %y")
    Dsf19daylast <- format(Dsf19[length(Dsf19)], format = "%b %d, %y")
  } else if (Trend == "Month") {
    Dsf19 <- seq(zoo::as.Date(MaximumDate + lubridate::month(1)),
                 by = "month", length.out = length(Data$Case))
    Dsf19day01 <- zoo::as.yearmon(Dsf19[1], "%b %y")
    Dsf19daylast <- zoo::as.yearmon(Dsf19[length(Dsf19)], "%b %y")
  } else {
    Dsf19 <- seq(zoo::as.Date(MaximumDate + lubridate::years(1)),
                 by = "year", length.out = length(Data$Case))
    Dsf19day01 <- format(zoo::as.Date(Dsf19[1]), "%Y")
    Dsf19daylast <- format(zoo::as.Date(Dsf19[length(Dsf19)]), "%Y")
  }

  Title <- paste(Dsf19day01, "-", Dsf19daylast,
                 collapse="")
  Without.knots <- fitted.values(fit01)
  With.knots <- fitted.values(fit10)
  Smooth <- fitted.values(fit11)
  ARIMA <- fita1[["fitted"]]
  Quadratic <- fitted.values(fitpi1)
  Linear1  <- fitted.values(Linear)
  Semilog1 <- fitted.values(Semilog)
  Growth1  <- fitted.values(Growth)
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
  kk4091 <- stats::lm(Data$Day~Without.knots * With.knots * Smooth *
                        Quadratic * ARIMA)
  kk4191 <- forecast::forecast(fitted.values(kk4091), h = H)
  kk6091 <- stats::lm(Data$Day~Without.knots + With.knots + Smooth +
                        Quadratic + ARIMA)
  kk6191 <- forecast::forecast(fitted.values(kk6091), h = H)

  KK91 <- as.data.frame(cbind("Date" = Dsf19,"Day" = ss,
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
  names(KK91) <- c("Date", "Day", "Linear", "Semilog", "Growth",
                   "Without Knots", "Smooth spline", "With Knots",
                   "Polynomial", "Lower ARIMA", "Upper ARIMA")
  RMSE91L <- as.data.frame(cbind(
    "Linear" = ModelMetrics::rmse(Data$Case, Linear1),
    "Semilog" = ModelMetrics::rmse(Data$Case, Semilog1),
    "Growth" = ModelMetrics::rmse(Data$Case, Growth1)))

  RMSE91 <- c("Without knots" = ModelMetrics::rmse(Data$Case,
                                                   Without.knots),
              "Smooth Spline" = ModelMetrics::rmse(Data$Case, With.knots),
              "With knots" = ModelMetrics::rmse(Data$Case, Smooth),
              "Polynomial" = ModelMetrics::rmse(Data$Case, Quadratic),
              "Lower ARIMA" = ModelMetrics::rmse(Data$Case, ARIMA),
              "Upper ARIMA" = ModelMetrics::rmse(Data$Case, ARIMA))

  RMSE_weight91 <- as.list(RMSE91 / sum(RMSE91))
  KK91$Date <- zoo::as.Date(KK91$Date, origin = "1970-01-01")
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
    ModelMetrics::rmse(Data$Case, kk3091)
  RMSE91$`Ensembled based on weight` <-
    ModelMetrics::rmse(Data$Case, fitted.values(kk4091))
  RMSE91$`Ensembled based on summed weight` <-
    ModelMetrics::rmse(Data$Case, fitted.values(kk6091))
  RMSE91$`Ensembled based on weight of fit` <- ModelMetrics::rmse(Data$Day,
                                                                  P_weight91)
  DDf91 <- c("Linear", "Semilog", "Growth", "Without knots", "Smooth Spline",
             "With knots", "Quadratic Polynomial",
             "Lower ARIMA", "Upper ARIMA",
             "Essembled with equal weight",
             "Essembled based on weight",
             "Essembled based on summed weight",
             "Essembled based on weight of fit" )
  Forcasts91 <- colSums(KK91[,-c(1,2)])
  Fore_f91 <- as.data.frame(cbind("Model" = DDf91,
                                  "Case" =
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
  RMSE_f91 <- cbind("Models" = DDf91, "RMSE" = RMSE_f91)

  KK191 <- KK91 %>%
    tidyr::pivot_longer(-c(Date, Day), names_to = "Models",
                        values_to = "Forecast")
  KK191$Date <- zoo::as.Date(KK191$Date)
  KK0091 <- ggplot2::ggplot(KK191) +
    aes(x = Date, y = Forecast, colour = Models, group = Models) +
    geom_line(size = 1L) +
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
    lower = min(Data$Case)
    upper = max(Data$Case)
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
    KK91c <- as.data.frame(cbind("Date" = Dsf19, "Day" = 1:length(Dsf19),
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
    KK91c$Date <- zoo::as.Date(KK91c$Date, origin = "1970-01-01")
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
      tidyr::pivot_longer(-c(Date, Day), names_to = "Models",
                          values_to = "Forecast")
    KK191c$Date <- zoo::as.Date(KK191c$Date)
    KK0091c <- ggplot2::ggplot(KK191c) +
      aes(x = Date, y = Forecast, colour = Models, group = Models) +
      geom_line(size = 1L) +
      scale_color_hue() +
      theme_bw() +
      theme() +
      labs(title = Title,
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
    "Constrained forecast Plot"     = KK0091c
  )
  return(results)
}
