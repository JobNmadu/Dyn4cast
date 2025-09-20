
#' Index Construction for estimation of **Exposure** or **Sensitivity** 
#' 
#' @description
#' Vulnerability or to be vulnerable means the state or quality of being
#'  susceptible to physical or emotional harm, damage, or attack, usually
#'   indicating a lack of defense or protection, making someone or some systems 
#'   more likely to be affected by external factors or threats. Therefore, 
#'   vulnerability index is a quantitative or standardized framework of  such
#'    state or quality which then makes comparisons between households, 
#'    communities or systems possible. The index is made up of three main 
#'    components: exposure, sensitivity and adaptive capacity. Each component 
#'    has multiple indicators from wide ranges including social, medical, 
#'    psychological and various extreme events like floods, drought, earthquakes
#'     etc. This function is for conversion of indicators exposure and 
#'     sensitivity into a vector of index through normalization and weighting.
#'      The resulting index from each of the component is then combined via an
#'       appropriate model into vulnerability index.
#' 
#' @param data Data frame of indicators of Exposure or Sensitivity. The data
#'  frame must be numeric.
#'
#' @return A list with the following components:
#' \item{\code{Indexed data}}{`dataframe` of indices corresponding to the 
#' supplied data.}
#' \item{\code{Index}}{A vector of indices representing the variable of 
#' interest, either **Exposure** or **Sensitivity**.}
#' 
#' @export index_construction
#'
#' @examples
#' library(readr)
#' garrett_data <- data.frame(garrett_data)
#' index_construction(garrett_data)

index_construction <- function(data) {
  try1 <- data.frame(data_transform(data, 2))
  
  TR <- 0
  TRv <- 0
  for(i in 1 : NCOL(try1)){
    TR[i] <- sqrt(var(try1[, i]))
    TRv[i] <- ifelse(TR[i] == 0, 0, 1 / TR[i])
  }
  
  cc <- sum(TRv, na.rm = TRUE)
  try2 <- as.data.frame(matrix(NCOL(try1), 1))
  try2 <- cc / TR
  
  try3 <- (try1 * try2)
  try3[sapply(try3, is.infinite)] <- NA
  try3[is.na(try3)] <- 0
  try4 <- colSums(try3)
  try5 <- try3 / try4
  Index <- rowSums(try5) / NCOL(try5)
  
  return(load <- list(`Indexed data` = try5, `Index` = Index))
}
