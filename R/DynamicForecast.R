#' @param DynamicForecast name
#'
#' @param Data A data.frame
#' @param Title A text
#'
#' @return results
#' @export
#' @importFrom stats lm
#' @importFrom stats fitted.values
#' @importFrom stats smooth.spline
#' @importFrom splines bs
#'
#' @examples
#' DynamicForecast(Data = Data, Title = "14 days lag forecast")

library(stats)
DynamicForecast <- function(Data, Title) {
  fit01  <- lm(Case ~ splines::bs(Day, knots = NULL), data = Data)
  fit10   <- lm(Case ~ splines::bs(Day, knots = BREAKS), data = Data)
  fit11  <- stats::smooth.spline(Data[, 2], Data[, 3])
  fita1  <- forecast::auto.arima(Data$Case)
  fitpi1 <- stats::lm(Case ~ Day + I(Day^2), data = Data)
  Dss19 <- seq(Data$Day[1], by = 1, length.out = length(Data$Day))
  Dsf19 <- seq(as.Date(MaximumDate + lubridate::days(1)),
             by = "day", length.out = length(Data$Case))
  Dsf19day01 <- format(Dsf19[1], format = "%b %d, %y")
  Dsf19daylast <- format(Dsf19[length(Dsf19)], format = "%b %d, %y")
  Title <- paste(Dsf19day01, "-", Dsf19daylast,
                       collapse="")

  kk91   <- forecast::forecast(fitted.values(fit01),  h = length(Dsf19))
  kk091  <- forecast::forecast(fitted.values(fit10),  h = length(Dsf19))
  kk191  <- forecast::forecast(fitted.values(fit11),  h = length(Dsf19))
  kk1091 <- forecast::forecast(fitted.values(fitpi1), h = length(Dsf19))
  kk291  <- forecast::forecast(fita1, h = length(Dsf19))
  kk3091 <- (fitted.values(fit01) + fitted.values(fit10) +
             fitted.values(fit11) + fitted.values(fitpi1) +
             fita1[["fitted"]])/5
  kk3191 <- forecast::forecast(kk3091, h = length(Dsf19))
  kk4091 <- lm(Data[,2]~fitted.values(fit01)*fitted.values(fit10)*
               fitted.values(fit11)*fitted.values(fitpi1)*
               fita1[["fitted"]])
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

  RMSE91 <- c("Without knots" = rmse(Data[, 3],
                                   fitted.values(fit01)),
            "Smooth Spline" = rmse(Data[, 3], fitted.values(fit10)),
            "With knots" = rmse(Data[, 3], fitted.values(fit11)),
            "Polynomial" = rmse(Data[, 3], fitted.values(fitpi1)),
            "Lower ARIMA" = rmse(Data[, 3], fita1[["fitted"]]),
            "Upper ARIMA" = rmse(Data[, 3], fita1[["fitted"]]))

  #RMSE <- 1/RMSE
  RMSE_weight91 <- as.list(RMSE91 / sum(RMSE91))
  KK91$Date <- as.Date(KK91$Date)
  KK91$`Essembled with equal weight` <- kk3191[["mean"]]
  KK91$`Essembled based on weight of model` <- kk4191[["mean"]]
  P_weight91 <- (fitted.values(fit01) * RMSE_weight91$`Without knots`) +
    (fitted.values(fit10) * RMSE_weight91$`Smooth Spline`) +
    (fitted.values(fit11) * RMSE_weight91$`With knots`) +
    (fitted.values(fitpi1) * RMSE_weight91$Polynomial) +
    (fita1[["fitted"]] * RMSE_weight91$`Lower ARIMA`)

  kk5191 <- forecast::forecast(P_weight91, h = length(Dsf19))
  KK91$`Essembled based on weight of fit of each model` <- kk5191[["mean"]]
  RMSE$`Essembled with equal weight` <- rmse(Data[, 3], kk3091)
  RMSE$`Essembled based on weight of model` <- rmse(Data[,2],
                                                    fitted.values(kk4091))
  RMSE$`Essembled based on weight of fit of each model` <- rmse(Data[,2],
                                                                P_weight91)
  DDf91 <- c("Without knots", "Smooth Spline",
           "With knots", "Quadratic Polynomial",
           "Lower ARIMA", "Upper ARIMA",
           "Essembled with equal weight",
           "Essembled based on weight of model",
           "Essembled based on weight of fit of each model" )
  Forcasts91 <- colSums(KK91[,-c(1,2)])
  Fore_f91 <- as.data.frame(cbind("Model" = DDf91,
                                "Confirmed cases" =
                                  comma(round(Forcasts91, 0))))
  RMSE_f91 <- c(
    "Without knots" =
      round(RMSE$`Without knots`, 2),
    "Smooth Spline"  =
      round(RMSE$`Smooth Spline`, 2),
    "With knots"  =
      round(RMSE$`With knots`, 2),
    "Polynomial"  =
      round(RMSE$Polynomial, 2),
    "Lower ARIMA"  =
      round(RMSE$`Lower ARIMA`, 2),
    "Upper ARIMA"  =
      round(RMSE$`Upper ARIMA`, 2),
    "Essembled with equal weight"  =
      round(RMSE$`Essembled with equal weight`, 2),
    "Essembled based on weight of model"  =
      round(RMSE$`Essembled based on weight of model`, 2),
    "Essembled based on weight of fit of each model"  =
      round(RMSE$`Essembled based on weight of fit of each model`, 2)
  )

  KK191 <- KK91 %>%
    pivot_longer(-c(Date, Day), names_to = "Models",
                 values_to = "Forecast")
  KK191$Date <- as.Date(KK191$Date)

  KK0091 <- ggplot(KK191) +
    aes(x = Date, y = Forecast, colour = Models, group = Models) +
    geom_line(size = 1L) +
    scale_color_hue() +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = Title,
         subtitle = " ",
         caption = " ")
  results <- list(
    "Forecast" = Fore_f91,
    "RMSE"     = RMSE_f91,
    "Plot"     = KK0091,
    "Date"     = Title
  )
  return(results)
}
