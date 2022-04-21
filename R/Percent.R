#' Attach Per Cent Sign to Data
#'
#' `Percent()` This function is a wrapper for easy affixing of the per cent sign (%) to a value or a vector of values
#'
#' @param Data The Data which the percent sign is to be affixed
#' @param Type The type of data. The default arguments are *Value* for single numeric data of *Frame* for a numeric vector data
#' @param digits Number of decimal points for the output
#' @param format The format of the output which is internal and the default is a character factor
#' @param ... Additional arguments that may be passed to the function
#'
#' @export Percent
#'
#' @examples
#' Data <- c(1.2, 0.5, 0.103, 7, 0.1501)
#' Percent(Data = Data, Type = "Frame")  # Value, Frame
#'
#' Data <- 1.2
#' Percent(Data = Data, Type = "Value")  # Value, Frame
#'
Percent <- function(Data, Type, digits = 2,
                    format = "f", ...) {
  # Create user-defined function
  if (Type == "Value") {
    paste0(formatC(Data/(1)), "%")
  } else {
    paste0(formatC((Data/sum(Data))*100, format = format,
                   digits = digits, ...), "%")
  }
}
