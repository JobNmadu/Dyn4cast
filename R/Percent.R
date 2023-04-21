#' Attach Per Cent Sign to Data
#'
#' This function is a wrapper for easy affixing of the per cent sign (%) to a value or a vector or a data frame of values.
#'
#' @param Data The Data which the percent sign is to be affixed. The data must be in the raw form because for frame argument, the per cent value of each cell is calculated before the sign is affixed.
#' @param Type The type of data. The default arguments are *Value* for single numeric data of *Frame* for a numeric vector or data frame data. In the case of vector or data frame, the per cent value of each cell is calculated before the per cent sign is affixed.
#' @param format The format of the output which is internal and the default is a character factor
#' @param ... Additional arguments that may be passed to the function
#'
#' @return This function returns the result as
#' \item{\code{percent}}{ values with the percentage sign (%) affixed.}
#'
#' @export Percent
#' @name Percent
#'
#' @aliases sample
#'
#' @examples
#' Data <- c(1.2, 0.5, 0.103, 7, 0.1501)
#' Percent(Data = Data, Type = "Frame")  # Value, Frame
#' Data <- 1.2
#' Percent(Data = Data, Type = "Value")  # Value, Frame
#' Percent(Data = sample, Type = "Frame")  # Value, Frame
Percent <- function(Data, Type, format = "f", ...){
  if (Type == "Value") {
    percent <- Data
    Rate <- (percent/percent)*100
    percent <- paste0(formatC(percent/(1)), "%")
  } else {
    percent <- Data
    if(is.null(dim(percent))){
      Rate <- percent/sum(percent)*100
      percent <- paste0(formatC((percent/sum(percent))*100, format = format,
                                digits = digits, ...), "%")
    }else{
      Data <- signif(sweep(Data, 2, colSums(Data), FUN = "/")*100, 2)
      percent <- Rate <- as.data.frame(as.matrix(Data))
      HI <- nrow(percent)
      GI <- ncol(percent)
      for (i in 1:HI){
        for (j in 1:GI){
          percent[i, j] <- paste0(percent[i, j], "%")
        }
      }
      #Data1 <- as.data.frame(lapply(Data0, as.numeric, digits = 2))
      #Data2 <- as.data.frame(Data0, digits = 2)
      #sapply(Data0, paste0(formatC(Data0, digits = 2), "%"))
      #paste0(formatC(Data1, digits = digits), "%", format = "f")
    }
  }
  result <- list(percent = percent,
                 Rate = Rate)
  return(result)
}

