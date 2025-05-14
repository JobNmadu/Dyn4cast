#' Attach Per Cent Sign to Data
#'
#' @description
#' This function is a wrapper for easy affixing of the per cent sign (%) to a
#' value or a vector or a data frame of values.
#'
#' @param Data The Data which the percent sign is to be affixed. The data must
#'  be in the raw form because for frame argument, the per cent value of each
#'  cell is calculated before the sign is affixed.
#' @param Type The type of data. The default arguments are *Value* for single
#' numeric data of *Frame* for a numeric vector or data frame data. In the case
#'  of vector or data frame, the per cent value of each cell is calculated
#'  before the per cent sign is affixed.
#' @param format The format of the output which is internal and the default is
#'  a character factor
#' @param ... Additional arguments that may be passed to the function
#'
#' @return This function returns the result as
#' \item{\code{percent}}{ values with the percentage sign (%) affixed.}
#'
#' @export Percent
#' @name Percent
#'
#' @examples
#' Data <- c(1.2, 0.5, 0.103, 7, 0.1501)
#' Percent(Data = Data, Type = "Frame")  # Value, Frame
#' Data <- 1.2
#' Percent(Data = Data, Type = "Value")  # Value, Frame
#' df <- data.frame(c(A = 2320, 5760, 4800, 2600, 5700, 7800, 3000, 6300, 2400,
#' 10000, 2220, 3740),
#' B = c(0, 0, 1620, 3600, 1200, 1200, 1200, 4250, 14000, 10000, 1850, 1850),
#' C = c(3000, 3000, 7800, 5400, 3900, 7800, 1950, 2400, 2400, 7000, 1850, 1850),
#' D = c(2900, 5760, 3750, 5400, 4095, 3150, 2080, 7800, 1920, 1200, 5000, 1950),
#' E = c(2900, 2030, 0, 5400, 5760, 1800, 2000, 1950, 1850, 3600, 5200, 5760),
#' F = c(2800, 5760, 1820, 4340, 7500, 2400, 2300, 1680, 1850, 0, 2800, 8000),
#' G = c(5760, 4600, 13000, 7800, 6270, 1200, 1440, 8000, 1200, 2025, 4800, 2600),
#' H = c(2100, 5760, 8250, 3900, 1800, 1200, 4800, 1800, 7800, 2035, 8000, 3000))
#' Percent(Data = df, Type = "Frame")  # Value, Frame
Percent <- function(Data, Type, format = "f", ...) {
  options(scipen = 999, digits = 2)
  if (Type == "Value") {
    percent <- Data
    Rate <- (percent / percent) * 100
    percent <- paste0(formatC(percent / (1)), "%")
  } else {
    percent <- Data
    if (is.null(dim(percent))) {
      Rate <- percent / sum(percent) * 100
      percent <- paste0(formatC((percent / sum(percent)) * 100, format = format,
                                ...), "%")
    } else {
      Data <- sweep(Data, 2, colSums(Data), FUN = "/") * 100
      percent <- Rate <- as.data.frame(as.matrix(Data))
      HI <- nrow(percent)
      GI <- ncol(percent)
      for (i in 1:HI) {
        for (j in 1:GI) {
          percent[i, j] <- paste0(percent[i, j], "%")
        }
      }
    }
  }
  result <- list(percent = percent,
                 Rate = Rate)
  return(result)
}
