#' Latent Factors Recovery from Variables Loadings
#'
#' @description
#' This function retrieves the latent factors and their variable loadings which
#'  can be used as `R` objects to perform other analysis.
#'
#' @param data An `R object` obtained from exploratory factor analysis (EFA)
#' using the `fa` function in `psych` package.
#' @param DATA A `data.frame`, the raw data used to carry out the parallel
#' analysis to obtain `data` object.
#' @param RC Optional factor indicating whether resilience capacity is to be
#'  estimated but defaults to `NULL` once the number of variables in the data
#'   is not sufficient, i.e. < 20. To estimate, turn it to "Yes".
#'
#' @return A list with the following components:
#' \item{\code{Loadings data}}{`dataframe` of the factor loadings from the
#'  data.}
#' \item{\code{Factors extracted}}{`dataframe` of retrieved latent factors.}
#' \item{\code{factored data}}{`dataframe` of latent data based the product of
#'  recovered latent factors and the on raw data.}
#' \item{\code{Factors list}}{A list of vectors of individual latent factors
#'  recovered from the data. However, to make it usable, the vector should
#'   be `bind` with the names of the variables in the data and the
#'    `NA` removed.}
#' \item{\code{Resilence capacity}}{A vector of the resilience capacity if the
#'  data is prepared for that otherwise NULL.}
#'
#' @name model_factors
#' @export model_factors
#'
#' @importFrom utils globalVariables
#'
#' @examples
#' library(psych)
#' library(readr)
#' Data <- Quicksummary
#' GGn <- names(Data)
#' GG <- ncol(Data)
#' GGx <- c(paste0('x0', 1 : 9), paste("x", 10 : ncol(Data), sep = ""))
#' names(Data) <- GGx
#' lll <- fa.parallel(Data, fm = "minres", fa = "fa")
#' dat <- fa(Data, nfactors = lll[["nfact"]], rotate = "varimax",fm = "minres")
#'
#' model_factors(data = dat, DATA = Data)
#'
# #' @keywords internal
utils::globalVariables(c("."))
name  <-  NULL
value <- 0
model_factors <- function(data, DATA, RC = "No") {
  llp <- printLoadings(data$loadings)

  #convert chr to num
  llp <- as.data.frame(apply(llp, 2, as.numeric))

  #format and sort data frame based on factors
  llp[is.na(llp)] <- 0
  llpp <- llp

  llpp <- rownames_to_column(llpp, "Factor")

  Factors <- llpp %>%
    tidyr::pivot_longer(cols = -1) %>%
    dplyr::mutate(value = dplyr::case_when(abs(value) > 0.39 ~ value,
                             .default = 0)) %>%
    tidyr::pivot_wider(names_from = name, values_from = value) %>%
    tidyr::unnest(cols = -1)
  Factors <- Factors[order(Factors$Factor),]

  llp <- rownames_to_column(llp, "Factor")
  llp <- llp[order(llp$Factor),]

  #data frame computed with factor loadings
  Load <- Factors[, -1]
  TR <- 0
  for(i in 1 : NCOL(Load)){
    dplyr::mutate(TR[i] <- dplyr::case_when(Load[, i] > 0 ~ Load[, i]))
  }
  names(TR) <- names(Load)

  z_bRC <- data.frame(as.matrix(DATA) %*% as.matrix(Load))
  z_bRC[is.na(z_bRC)] <- 0

  if (RC == "Yes" & NCOL(data) > 20) {
    Rc = rowSums(z_bRC) / NROW(z_bRC)
  }else{
    Rc = NULL
  }
  return(load <- list(`Loadings data` = llp, `Factors extracted` = Factors,
               `factored data` =  z_bRC, `Factors list` = TR,
               `Resilence capacity` = Rc))
}

printLoadings <- function(x, digits = 3, cutoff = 0.01, sort = TRUE, ...) {
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p), ]
  }
  cat("\nLoadings:\n")
  fx <- format(round(Lambda, digits))
  names(fx) <- NULL
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- paste(rep(" ", nc), collapse = "")
  newx <- print(fx, quote = FALSE, ...)
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  if (base::is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx / p)
    if (factors > 1)
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx / p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx)
}
