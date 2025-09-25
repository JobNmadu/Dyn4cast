#' Convert continuous vector variable to formatted factors
#'
#' @description
#' Often, when a continuous data is converted to factors using the `base R`
#' cut function, the resultant `Class Interval` column provide data with
#' scientific notation which normally appears confusing to interpret,
#' especially to casual data scientist. This function provide a more
#' user-friendly output and is provided in a formatted manner. It is a easy to
#'  implement function.
#'
#' @param data A vector of the data to be converted to factors if not cut
#' already or the vector of a cut data
#' @param breaks Number of classes to break the data into
#' @param cut `Logical` to indicate if the `cut` function has already being
#' applied to the data, defaults to `FALSE`.
#'
#' @return The function returns a `data frame` with three or four columns
#'  i.e `Lower class`, `Upper class`, `Class interval` and `Frequency` (if the
#'   cut is `FALSE`).
#' \item{\code{Cut}}{The `data frame`}
#'
#' @export formattedcut
#'
#' @examples
#' library(tidyverse)
#' DD <- rnorm(100000)
#' formattedcut(DD, 12, FALSE)
#' DD1 <- cut(DD, 12)
#' DDK <- formattedcut(DD1, 12, TRUE)
#' DDK
#' # if data is not from a data frame, the frequency distribution is required.
#' as.data.frame(DDK %>%
#' group_by(`Lower class`, `Upper class`, `Class interval`) %>%
#' tally())
formattedcut <- function(data, breaks, cut = FALSE) {
  options(scipen = 999, digits = 2)
  if (cut == FALSE) {
    tally <- as.data.frame(table(cut(data, breaks, include.lowest = FALSE)))
    group <- as.data.frame(cbind(`Lower class` = as.numeric(sub("\\((.+),.*",
                                                                "\\1",
                                                                tally$Var1)),
                                 `Upper class` =
                                   as.numeric(sub("[^,]*,([^]]*)\\]",
                                                  "\\1", tally$Var1))))
    group$`Class interval` <- paste(group$`Lower class`, "-",
                                    group$`Upper class`)
    cut <- cbind(tally, group)
  } else {
    group <- as.data.frame(cbind(`Lower class` = as.numeric(sub("\\((.+),.*",
                                                                "\\1", data)),
                                 `Upper class`
                                 = as.numeric(sub("[^,]*,([^]]*)\\]",
                                                  "\\1", data))))
    group$`Class interval` <- paste(group$`Lower class`, "-",
                                    group$`Upper class`)
    cut <- group
  }
  cut
}
