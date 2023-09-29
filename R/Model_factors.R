#' Latent Variables Recovery from Factor Loadings
#'
#' @description
#' This function retrieves the latent factors and their variable loadings which can be used as `R` objects to perform other analysis.
#'
#' @param data An `R object` obtained from exploratory factor analysis (EFA) using the `fa` function in `psych` package.
#' @param DATA A `data.frame`, the raw data used to carry out the parallel analysis to obtain `data` object.
#'
#' @return A list with the following components:
#' \item{\code{Load_numeric}}{Table of latent factors and the variables loadings.}
#' \item{\code{Load_char}}{Table of latent factors and their variables.}
#'
#' @name Model_factors
#'
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom purrr map2
#'
#' @export Model_factors
#'
#' @examples
#' library(psych)
#' Data <- Quicksummary
#' GGn <- names(Data)
#' GG <- ncol(Data)
#' GGx <- c(paste0('x0', 1:9), paste("x", 10:ncol(Data), sep = ""))
#' names(Data) <- GGx
#' lll <- fa.parallel(Data, fm = 'minres', fa = 'fa')
#' dat <- fa(Data, nfactors = lll[["nfact"]], rotate = "varimax",fm="minres")
#'
#' DD <- Model_factors(data = dat, DATA = Data)
#'
#' factorM <- DD$Load_numeric
#' factorC <- DD$Load_char
utils::globalVariables(c("Variable", "Latent", "Loading", "is_null", "list_c",
                         "rep_along", "stop_input_type"))

Model_factors <- function(data, DATA) {
  llp <- printLoadings(data$loadings)
  Factor_m <- Factors <- as.data.frame(llp)

  Factors1 <- Factors %>%
    rownames_to_column() %>%
    tidyr::pivot_longer(cols = tidyr::starts_with("M"))
  names(Factors1) <- c("Variable", "Latent", "Loading")

  Factors1 <-  Factors1 %>%
    tidyr::drop_na(Loading) %>%
    mutate(Loading = Variable) %>%
    tidyr::pivot_wider(names_from = Latent,
                values_from = Loading)

  Factors2 <- dplyr::select(Factors1, c(2:ncol(Factors1)))

  py <- nrow(Factors2)
  pz <- ncol(Factors2)
  Factors3 <- as.data.frame(matrix(nrow=py, ncol=pz))
  for(i in 1 : py) {
    for(j in 1 : pz){
      if (Factors[i, j] > is.na(Factors[i+1, j])){
        Factors3[i+1,j] = NA
      } else if (Factors[i, j] != "      ")  {
        Factors3[i,j] = Factors2[i, j]
      } else {
        Factors3[i,j] = NA
      }
    }
  }
  names(Factors3) <- names(Factors)

  Factor_m[Factor_m == "      "] = NA

  if (ncol(Factors3) < 2 | ncol(Factors3) > 7) {
    stop("Factors must be uqual or above 2 and less than or equal 7 to proceed")
  } else {
    for (i in 2 : ncol(Factors3)) {
      if (i == 7) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3), loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste(Mk333$MR3, " * ", Mk333$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4), loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste(Mk444$MR4, " * ", Mk444$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5), loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste(Mk555$MR5, " * ", Mk555$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi6 <- unique(intersect(kk(Factors3$MR6), kk(Factors3$MR7)))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6), loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste(Mk555$MR6, " * ", Mk666$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR777 <- kk(Factors3$MR7)
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7), loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17
        MR7 <- paste(Mk777$MR7, " * ", Mk777$loading, sep = "", collapse = " + ") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6", "MR7"))

      } else if (i == 6) {
        Mk777 <- NULL
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3), loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste(Mk333$MR3, " * ", Mk333$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4), loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste(Mk444$MR4, " * ", Mk444$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi5 <- unique(intersect(kk(Factors3$MR5), kk(Factors3$MR6)))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR55 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5), loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR5 <- paste(Mk555$MR5, " * ", Mk555$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR666 <- kk(Factors3$MR6)
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6), loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16
        MR6 <- paste(Mk666$MR6, " * ", Mk666$loading, sep = "", collapse = " + ") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6"))

      } else if (i == 5){

        Mk777 <- Mk666 <- NULL
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3), loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste(Mk333$MR3, " * ", Mk333$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi4 <- unique(intersect(kk(Factors3$MR4), kk(Factors3$MR5)))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4), loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste(Mk444$MR4, " * ", Mk444$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR555 <- kk(Factors3$MR5)
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5), loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15
        MR5 <- paste(Mk555$MR5, " * ", Mk555$loading, sep = "", collapse = " + ") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5), nm = c("MR1", "MR2", "MR3", "MR4", "MR5"))
      } else if (i == 4)  {

        Mk777 <- Mk666 <- Mk555 <- NULL
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3), loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste(Mk333$MR3, " * ", Mk333$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR444 <- kk(Factors3$MR4)
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4), loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14
        MR4 <- paste(Mk444$MR4, " * ", Mk444$loading, sep = "", collapse = " + ") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3, MR4), nm = c("MR1", "MR2", "MR3", "MR4"))
      } else if (i == 3){

        Mk777 <- Mk666 <- Mk555 <- Mk444 <- NULL
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR333 <- kk(Factors3$MR3)
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3), loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13
        MR3 <- paste(Mk333$MR3, " * ", Mk333$loading, sep = "", collapse = " + ") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3), nm = c("MR1", "MR2", "MR3"))
      } else{

        Mk777 <- Mk666 <- Mk555 <- Mk444 <- Mk333 <- NULL
        MRi1 <- unique(intersect(kk(Factors3$MR1), kk(Factors3$MR3)))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1), loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste(Mk111$MR1, " * ", Mk111$loading, sep = "", collapse = " + ") %>%
          eees(.)

        MR222 <- kk(Factors3$MR2)
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2), loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12
        MR2 <- paste(Mk222$MR2, " * ", Mk222$loading, sep = "", collapse = " + ") %>%
          eees(.)
      }
    }
  }


  MRi_data <- DATA %>%
    mutate(!!!expression, .keep = "none")

  results <- list(Latent_frame = MRi_data,
                  Latent_1 = Mk111,
                  Latent_2 = Mk222,
                  Latent_3 = Mk333,
                  Latent_4 = Mk444,
                  Latent_5 = Mk555,
                  Latent_6 = Mk666,
                  Latent_7 = Mk777)
  return(results)
}

printLoadings <- function (x, digits = 3, cutoff = 0.4, sort = TRUE, ...)
{
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
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1)
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  cat("\n")
  print(round(varex, digits))
  invisible(newx)
}

eees <- function (x)
{
  if (inherits(x, "connection")) {
    if (!isOpen(x)) {
      open(x)
      on.exit(close(x))
    }
    exprs <- parse(file = x, keep.source = FALSE)
  }
  else if (is.character(x)) {
    exprs <- cccs(x)
  }
  else {
    stop_input_type(x, "a character vector or an R connection")
  }
  as.list(exprs)
}

cccs <- function(x) {
  parsed <- purrr::map(x, function(elt) as.list(chrss(elt)))

  nms <- names(parsed)
  parsed <- unname(parsed)

  if (!is_null(nms)) {
    nms <- list_c(purrr::map2(parsed, nms, rep_along))
  }
  if (length(parsed)) {
    parsed <- list_c(parsed)
  }
}

chrss <- function(x) {
  parse(text = x, keep.source = FALSE)
}
