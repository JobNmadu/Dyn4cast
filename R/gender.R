#' Create Gender Variable
#'
#'@description
#'Often, there is need to differentiate between sex and gender. Many wonder if
#' there is any difference at all. This function will create clarity between
#'  them.
#'
#' @param data data frame containing __Age__ and __Sex__ variables
#'
#' @returns The `data.frame` with:
#' \item{\code{Gender}}{data frame with two additional variables.}
#'
#' @export gender
#'
#' @examples
#' # df <- data.frame(Age = c(49, 30, 44, 37, 29, 56, 28, 26, 33, 45, 45, 19,
#' #   32, 22, 19, 28, 28, 36, 56, 34),
#' #  Sex = c("male", "female", "female", "male", "male", "male", "female",
#' #  "female", "Prefer not to say", "male", "male", "female", "female", "male",
#' #  "Non-binary/third gender", "male", "female", "female", "male", "male"))
#' #  gender(df)
gender <- function(data) {
  data$Group <- dplyr::case_when(
    data$Age >= 60 ~ 3,
    data$Age >  25 ~ 2,
    data$Age <= 25 ~ 1)
  data$Gender <- dplyr::case_when(
    data$Group == 3 & tolower(data$Sex) == "male"   ~ "Elderly male",
    data$Group == 3 & tolower(data$Sex) == "female" ~ "Elderly female",
    data$Group == 2 & tolower(data$Sex) == "male"   ~ "Adult male",
    data$Group == 2 & tolower(data$Sex) == "female" ~ "Adult female",
    data$Group == 1                               ~ "Youth",
    .default   =  data$Sex)
  return(Gender = data)
}
