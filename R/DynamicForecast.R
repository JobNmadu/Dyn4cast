#' @name Dyn4cast
#'
#' @title Dynamic Forecast of five models and their Ensembles
#'
#' @description This function estimates, predict and forecast five models and their Ensembles. The recognised models are lm, smooth spline, polynomial splines with or without knots, quadratic polynomial,  and ARIMA. The robust output include the models' estimates, time-varying forecasts and plots  based on themes from ggplot. The main attraction of this package is the use of the newly introduced _equal number days (time, trend) forcast_
#'
#' @param Data A two column (Date, Case) DAILY dataset for the estimation. The date must be in format recognized by R. In future versions provisions shall be made of quarterly, monthly and yearly data
#'
#' @param BREAKS A vector of numbers indicating points of breaks for estimation of the spline models
#' @param MaximumDate The date indicating the maximum date (last date) in the data frame, meaning that forecasting starts the next date following it. The date must be a recognized date format. Note that for forecasting, the date origin is set to 1970-01-01
#'
#' @return A list with the following components:
#' \item{\code{Spline without knots}}{The estimated spline model without the breaks (knots).}
#' \item{\code{Spline with knots}}{The estimated spline model without the breaks (knots).}
#' \item{\code{Smooth Spline}}{The smooth spline estimates.}
#' \item{\code{ARIMA}}{Estimated Auto Regressive Integrated Moving Areage model.}
#' \item{\code{Quadratic}}{The estimated quadratic polynomial model.}
#' \item{\code{Essembled with equal weight}}{Estimated Essemble model with equal weight given to each of the models. To get this, the fitted values of each of the models is divided by the number of models and summed together.}
#' \item{\code{Essembled based on weight}}{Estimated Essemble model based on weight of each model. To do this, the fitted values of each model is multiplied and regressed agaisnt the trend.}
#' \item{\code{Essembled based on weight of fit}}{Estimated Essemble model. The fit of each model is measured by the rmse.}
#' \item{\code{Forecast}}{The forecast is equivalent to the length of the dataset (equal days forecast).}
#' \item{\code{RMSE}}{Root Mean Sqaure Error (rmse) for each forecast.}
#' \item{\code{Plot}}{The combined plots of the forecasts using ggplot. }
#' \item{\code{Date}}{This is the date range for the forecast.}
#'
#' @export DynamicForecast
#' @import tidyverse
#' @importFrom stats lm
#' @importFrom stats fitted.values
#' @importFrom stats smooth.spline
#' @importFrom Metrics rmse
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
#'
#' @examples
#' KK_28 <- readxl::read_excel("~/Data.xlsx") # Nigeria COVID-19 data
#' KK_28$Date <- as.Date(KK_28$Date, format = '%m/%d/%Y') # The date is reformatted
#' Dss <- seq(KK_28$Date[1], by = "day", length.out = length(KK_28$Case)) #data length for forecast
#' lastdayfo21 <- Dss[length(Dss)] # The maximum length
#' Data <- KK_28[KK_28$Date <= lastdayfo21 - 28, ] # desired length of forecast
#' BREAKS <- c(70, 131, 173, 228, 274) # The default breaks for the data
#' DynamicForecast(Data = Data, BREAKS = BREAKS, MaximumDate = "2021-02-10")
#'
#' KK_14 <- readxl::read_excel("~/Data.xlsx")
#' KK_14$Date <- as.Date(KK_14$Date, format = '%m/%d/%Y')
#' Dss <- seq(KK_14$Date[1], by = "day", length.out = length(KK_14$Case))
#' lastdayfo21 <- Dss[length(Dss)]
#' Data <- KK_14[KK_14$Date <= lastdayfo21 - 14, ]
#' BREAKS = c(70, 131, 173, 228, 274)
#' DynamicForecast(Data = Data, BREAKS = BREAKS , MaximumDate = "2021-02-10")
#'

if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))
utils::globalVariables(c("Spline without knots",
                         "Spline with knots",
                         "Smooth Spline",
                         "ARIMA",
                         "Quadratic",
                         "Essembled with equal weight",
                         "Essembled based on weight",
                         "Essembled based on weight of fit","Date", "Day",
                         "Forecast", "Models"))

DynamicForecast <- function(Data, BREAKS, MaximumDate) {
  Data$Day <- ss <- seq(1:length(Data$Case))
  fit01  <- lm(Case ~ splines::bs(Day, knots = NULL), data = Data)
  fit10   <- lm(Case ~ splines::bs(Day, knots = BREAKS),
                data = Data)
  fit11  <- stats::smooth.spline(Data$Day, Data$Case)
  fita1  <- forecast::auto.arima(Data$Case)
  fitpi1 <- stats::lm(Case ~ Day + I(Day^2), data = Data)
  Dss19 <- seq(Data$Day[1], by = 1, length.out = length(Data$Day))
  MaximumDate <- as.Date(MaximumDate)
  Dsf19 <- seq(as.Date(MaximumDate + lubridate::days(1)),
               by = "day", length.out = length(Data$Case))
  Dsf19day01 <- format(Dsf19[1], format = "%b %d, %y")
  Dsf19daylast <- format(Dsf19[length(Dsf19)], format = "%b %d, %y")
  Title <- paste(Dsf19day01, "-", Dsf19daylast,
                 collapse="")
  Without.knots <- fitted.values(fit01)
  With.knots <- fitted.values(fit10)
  Smooth <- fitted.values(fit11)
  ARIMA <- fita1[["fitted"]]
  Quadratic <- fitted.values(fitpi1)

  kk91   <- forecast::forecast(Without.knots,  h = length(Dsf19))
  kk091  <- forecast::forecast(With.knots,  h = length(Dsf19))
  kk191  <- forecast::forecast(Smooth,  h = length(Dsf19))
  kk1091 <- forecast::forecast(Quadratic, h = length(Dsf19))
  kk291  <- forecast::forecast(fita1, h = length(Dsf19))
  kk3091 <- (Without.knots + With.knots +
               Smooth + Quadratic +
               ARIMA)/5
  kk3191 <- forecast::forecast(kk3091, h = length(Dsf19))
  kk4091 <- lm(Data$Day~Without.knots*With.knots*
                 Smooth*Quadratic*
                 ARIMA)
  kk4191 <- forecast::forecast(fitted.values(kk4091), h = length(Dsf19))

  KK91 <- as.data.frame(cbind("Date" = Dsf19,"Day" = ss, "Without Knots" =
                                kk91[["mean"]], "Smooth spline" =
                                kk091[["mean"]], "With Knots" =
                                kk191[["mean"]], "Polynomial" =
                                kk1091[["mean"]], "Lower ARIMA" =
                                kk291[["lower"]], "Upper ARIMA" =
                                kk291[["upper"]]))
  KK91 <- KK91[,-c(7,9)]
  names(KK91) <- c("Date", "Day", "Without Knots", "Smooth spline",
                   "With Knots", "Polynomial", "Lower ARIMA", "Upper ARIMA")
  #KK91$Date <- as.character(KK91$Date)

  RMSE91 <- c("Without knots" = Metrics::rmse(Data$Case,
                                              Without.knots),
              "Smooth Spline" = Metrics::rmse(Data$Case, With.knots),
              "With knots" = Metrics::rmse(Data$Case, Smooth),
              "Polynomial" = Metrics::rmse(Data$Case, Quadratic),
              "Lower ARIMA" = Metrics::rmse(Data$Case, ARIMA),
              "Upper ARIMA" = Metrics::rmse(Data$Case, ARIMA))

  #RMSE <- 1/RMSE
  RMSE_weight91 <- as.list(RMSE91 / sum(RMSE91))
  KK91$Date <- as.Date(KK91$Date, origin = "1970-01-01")
  KK91$`Essembled with equal weight` <- kk3191[["mean"]]
  KK91$`Essembled based on weight of model` <- kk4191[["mean"]]
  P_weight91 <- (Without.knots * RMSE_weight91$`Without knots`) +
    (With.knots * RMSE_weight91$`Smooth Spline`) +
    (Smooth * RMSE_weight91$`With knots`) +
    (Quadratic * RMSE_weight91$Polynomial) +
    (ARIMA * RMSE_weight91$`Lower ARIMA`)

  kk5191 <- forecast::forecast(P_weight91, h = length(Dsf19))
  KK91$`Essembled based on weight of fit of each model` <- kk5191[["mean"]]
  RMSE91$`Essembled with equal weight` <- Metrics::rmse(Data$Case, kk3091)
  RMSE91$`Essembled based on weight of model` <- Metrics::rmse(Data$Case,
                                                               fitted.values(kk4091))
  RMSE91$`Essembled based on weight of fit of each model` <- Metrics::rmse(Data$Day, P_weight91)
  DDf91 <- c("Without knots", "Smooth Spline",
             "With knots", "Quadratic Polynomial",
             "Lower ARIMA", "Upper ARIMA",
             "Essembled with equal weight",
             "Essembled based on weight of model",
             "Essembled based on weight of fit of each model" )
  Forcasts91 <- colSums(KK91[,-c(1,2)])
  Fore_f91 <- as.data.frame(cbind("Model" = DDf91,
                                  "Confirmed cases" =
                                    formattable::comma(round(Forcasts91, 0))))
  RMSE_f91 <- c(
    "Without knots" =
      round(RMSE91$`Without knots`, 2),
    "Smooth Spline"  =
      round(RMSE91$`Smooth Spline`, 2),
    "With knots"  =
      round(RMSE91$`With knots`, 2),
    "Polynomial"  =
      round(RMSE91$Polynomial, 2),
    "Lower ARIMA"  =
      round(RMSE91$`Lower ARIMA`, 2),
    "Upper ARIMA"  =
      round(RMSE91$`Upper ARIMA`, 2),
    "Essembled with equal weight"  =
      round(RMSE91$`Essembled with equal weight`, 2),
    "Essembled based on weight of model"  =
      round(RMSE91$`Essembled based on weight of model`, 2),
    "Essembled based on weight of fit of each model"  =
      round(RMSE91$`Essembled based on weight of fit of each model`, 2)
  )
  RMSE_f91 <- cbind("Models" = DDf91, "RMSE" = RMSE_f91)

  KK191 <- KK91 %>%
    tidyr::pivot_longer(-c(Date, Day), names_to = "Models",
                        values_to = "Forecast")
  KK191$Date <- as.Date(KK191$Date)
  #KK911$Date <- as.character(as.Date(KK191$Date, origin = "2021-02-09"))
  KK0091 <- ggplot2::ggplot(KK191) +
    aes(x = Date, y = Forecast, colour = Models, group = Models) +
    geom_line(size = 1L) +
    scale_color_hue() +
    theme_bw() +
    theme() +
    labs(title = Title,
         subtitle = " ",
         caption = " ")
  results <- list(
    "Spline without knots" = fit01,
    "Spline with knots" = fit10,
    "Smooth Spline" = fit11,
    "ARIMA" = fita1,
    "Quadratic" = fitpi1,
    "Essembled with equal weight" = kk3091,
    "Essembled based on weight" = kk4091,
    "Essembled based on weight of fit" = P_weight91,
    "Forecast" = Fore_f91,
    "RMSE"     = RMSE_f91,
    "Plot"     = KK0091,
    "Date"     = Title
  )
  return(results)
}
