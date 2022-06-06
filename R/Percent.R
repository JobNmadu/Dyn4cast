#' Attach Per Cent Sign to Data
#'
#' This function is a wrapper for easy affixing of the per cent sign (%) to a value or a vector or a data frame of values.
#'
#' @param Data The Data which the percent sign is to be affixed
#' @param Type The type of data. The default arguments are *Value* for single numeric data of *Frame* for a numeric vector data
#' @param digits Number of decimal points for the output
#' @param format The format of the output which is internal and the default is a character factor
#' @param ... Additional arguments that may be passed to the function
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
Percent <- function(Data, Type, digits = 2, format = "f", ...){
  Data1 <- as.data.frame(Data)
  if (Type == "Value") {
    Data1 <- paste0(formatC(Data/(1)), "%")
  } else {
    if(is.null(dim(Data))){
      Data1 <- paste0(formatC((Data/sum(Data))*100, format = format, digits = digits,
                              ...), "%")
    }else{
      Data <- signif(sweep(Data, 2, colSums(Data), FUN = "/")*100, 2)
      Data <- as.data.frame(as.matrix(Data))
      HI <- nrow(Data)
      GI <- ncol(Data)
      for (i in 1:HI){
        for (j in 1:GI){
          Data1[i, j] <- print(paste0(Data1[i, j], "%"))
        }
      }
      #Data1 <- as.data.frame(lapply(Data0, as.numeric, digits = 2))
      #Data2 <- as.data.frame(Data0, digits = 2)
      #sapply(Data0, paste0(formatC(Data0, digits = 2), "%"))
      #paste0(formatC(Data1, digits = digits), "%", format = "f")
    }
  }
  return(Data1)
}

