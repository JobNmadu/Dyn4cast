#' Latent Factors Recovery from Variables Loadings
#'
#' @description
#' This function retrieves the latent factors and their variable loadings which
#'  can be used as `R` objects to perform other analysis.
#'
#' @usage model_factors(data, DATA)
#'
#' @param data An `R object` obtained from exploratory factor analysis (EFA)
#' using the `fa` function in `psych` package.
#' @param DATA A `data.frame`, the raw data used to carry out the parallel
#' analysis to obtain `data` object.
#'
#' @return A list with the following components:
#' \item{\code{Latent_frame}}{`data.frame` of latent factors based on the
#' variables loadings.}
#' \item{\code{Latent_1}}{`data.frame` of variables in Latent factor 1 with
#'  their loadings.}
#' \item{\code{Latent_2}}{`data.frame` of variables in Latent factor 2 with
#' their loadings.}
#' \item{\code{Latent_3}}{`data.frame` of variables in Latent factor 3 with
#' their loadings.}
#' \item{\code{Latent_4}}{`data.frame` of variables in Latent factor 3 with
#' their loadings.}
#' \item{\code{Latent_5}}{`data.frame` of variables in Latent factor 5 with
#' their loadings.}
#' \item{\code{Latent_6}}{`data.frame` of variables in Latent factor 6 with
#' their loadings.}
#' \item{\code{Latent_7}}{`data.frame` of variables in Latent factor 7 with
#' their loadings.}
#' \item{\code{Latent_8}}{`data.frame` of variables in Latent factor 8 with
#' their loadings.}
#' \item{\code{Latent_9}}{`data.frame` of variables in Latent factor 9 with
#' their loadings.}
#' \item{\code{Latent_10}}{`data.frame` of variables in Latent factor 10 with
#' their loadings.}
#' \item{\code{Latent_11}}{`data.frame` of variables in Latent factor 11 with
#' their loadings.}
#' \item{\code{Latent_12}}{`data.frame` of variables in Latent factor 12 with
#' their loadings.}
#' \item{\code{Latent_13}}{`data.frame` of variables in Latent factor 13 with
#' their loadings.}
#' \item{\code{Latent_14}}{`data.frame` of variables in Latent factor 14 with
#' their loadings.}
#' \item{\code{Latent_15}}{`data.frame` of variables in Latent factor 15 with
#' their loadings.}
#'
#' @name model_factors
#' @export model_factors
#'
#' @importFrom stats setNames
#' @importFrom purrr map
#' @importFrom purrr map2
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
utils::globalVariables(c("Variable", "Latent", "Loading", "is_null", "list_c",
                         "rep_along", "stop_input_type", "."))

model_factors <- function(data, DATA) {
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
  Factors3 <- as.data.frame(matrix(nrow = py, ncol = pz))
  for (i in 1 : py) {
    for (j in 1 : pz) {
      if (Factors[i, j] > is.na(Factors[i + 1, j])) {
        Factors3[i + 1, j]  <-  NA
      } else if (Factors[i, j] != "      ")  {
        Factors3[i, j]  <-  Factors2[i, j]
      } else {
        Factors3[i, j] <- NA
      }
    }
  }
  names(Factors3) <- names(Factors)

  Factor_m[Factor_m == "      "]  <-  NA

  Mk222 <- Mk333 <- Mk444 <- Mk555 <- Mk666 <- Mk777 <- Mk888 <- Mk999 <-
    Mk101010 <- Mk111111 <- Mk121212 <- Mk131313 <- Mk141414 <- Mk151515 <- NULL

  if (ncol(Factors3) < 2 | ncol(Factors3) > 15) {
    stop("Factors must be uqual or above 2 and less than or equal 15 to proceed")
  } else {
    for (i in 2 : ncol(Factors3)) {
      if(i == 15){
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR15))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR15))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR15))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR15))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR15))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR15))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR14)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR15))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR14)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR15))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(c(intersect(kk(Factors3$MR9), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR14)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR15))))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi10 <- unique(c(intersect(kk(Factors3$MR10), kk(Factors3$MR11)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR13)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR14)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR15))))
        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi11 <- unique(c(intersect(kk(Factors3$MR11), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR13)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR14)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR15))))
        MR1111 <- kk(Factors3$MR11)
        MR111111 <- MR1111[!MR1111 %in% MRi11]
        Mk111 <- data.frame(MR11 = kk(Factors3$MR11),
                            loading = as.numeric(kk(Factor_m$MR11)))
        Mk111111 <- Mk111[Mk111$MR11 %in% MR111111, ]
        MR11 <- paste("(", Mk111111$MR11, ")*(", Mk111111$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi12<- unique(c(intersect(kk(Factors3$MR12), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR12), kk(Factors3$MR14)),
                       intersect(kk(Factors3$MR12), kk(Factors3$MR15))))
        MR1212 <- kk(Factors3$MR12)
        MR121212 <- MR1212[!MR1212 %in% MRi12]
        Mk112 <- data.frame(MR12 = kk(Factors3$MR12),
                            loading = as.numeric(kk(Factor_m$MR12)))
        Mk121212 <- Mk112[Mk112$MR12 %in% MR121212, ]
        MR12 <- paste("(", Mk121212$MR12, ")*(", Mk121212$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi13 <- unique(c(intersect(kk(Factors3$MR13), kk(Factors3$MR14)),
                        intersect(kk(Factors3$MR13), kk(Factors3$MR15))))
        MR1313 <- kk(Factors3$MR13)
        MR131313 <- MR1313[!MR1313 %in% MRi13]
        Mk113 <- data.frame(MR13 = kk(Factors3$MR13),
                            loading = as.numeric(kk(Factor_m$MR13)))
        Mk131313 <- Mk113[Mk113$MR13 %in% MR131313, ]
        MR13 <- paste("(", Mk131313$MR13, ")*(", Mk131313$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi14 <- unique(intersect(kk(Factors3$MR14), kk(Factors3$MR15)))
        MR1414 <- kk(Factors3$MR14)
        MR141414 <- MR1414[!MR1414 %in% MRi14]
        Mk114 <- data.frame(MR14 = kk(Factors3$MR14),
                            loading = as.numeric(kk(Factor_m$MR14)))
        Mk141414 <- Mk114[Mk114$MR14 %in% MR141414, ]
        MR14 <- paste("(", Mk141414$MR14, ")*(", Mk141414$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MR151515 <-  kk(Factors3$MR15)
        Mk115 <- data.frame(MR15 = kk(Factors3$MR15),
                            loading = as.numeric(kk(Factor_m$MR15)))
        Mk151515 <- Mk115[Mk115$MR15 %in% MR151515, ]
        MR15 <- paste("(", Mk151515$MR15, ")*(", Mk151515$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10, MR11, MR12, MR13, MR14, MR15),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10",
                                      "MR11", "MR12", "MR13", "MR14", "MR15"))
      }else if (i == 14) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR14))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR14))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR14))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR14))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR14))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR14))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR13)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR14))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR14))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(c(intersect(kk(Factors3$MR9), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR14))))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi10 <- unique(c(intersect(kk(Factors3$MR10), kk(Factors3$MR11)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR13)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR14))))
        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi11 <- unique(c(intersect(kk(Factors3$MR11), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR13)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR14))))
        MR1111 <- kk(Factors3$MR11)
        MR111111 <- MR1111[!MR1111 %in% MRi11]
        Mk111 <- data.frame(MR11 = kk(Factors3$MR11),
                            loading = as.numeric(kk(Factor_m$MR11)))
        Mk111111 <- Mk111[Mk111$MR11 %in% MR111111, ]
        MR11 <- paste("(", Mk111111$MR11, ")*(", Mk111111$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi12<- unique(c(intersect(kk(Factors3$MR12), kk(Factors3$MR13)),
                       intersect(kk(Factors3$MR12), kk(Factors3$MR14))))
        MR1212 <- kk(Factors3$MR12)
        MR121212 <- MR1212[!MR1212 %in% MRi12]
        Mk112 <- data.frame(MR12 = kk(Factors3$MR12),
                            loading = as.numeric(kk(Factor_m$MR12)))
        Mk121212 <- Mk112[Mk112$MR12 %in% MR121212, ]
        MR12 <- paste("(", Mk121212$MR12, ")*(", Mk121212$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi13 <- unique(intersect(kk(Factors3$MR13), kk(Factors3$MR14)))
        MR1313 <- kk(Factors3$MR13)
        MR131313 <- MR1313[!MR1313 %in% MRi13]
        Mk113 <- data.frame(MR13 = kk(Factors3$MR13),
                            loading = as.numeric(kk(Factor_m$MR13)))
        Mk131313 <- Mk113[Mk113$MR13 %in% MR131313, ]
        MR13 <- paste("(", Mk131313$MR13, ")*(", Mk131313$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MR1414 <- kk(Factors3$MR14)
        MR141414 <- MR1414[!MR1414 %in% MRi14]
        Mk114 <- data.frame(MR14 = kk(Factors3$MR14),
                            loading = as.numeric(kk(Factor_m$MR14)))
        Mk141414 <- Mk114[Mk114$MR14 %in% MR141414, ]
        MR14 <- paste("(", Mk141414$MR14, ")*(", Mk141414$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10, MR11, MR12, MR13, MR14),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10",
                                      "MR11", "MR12", "MR13", "MR14"))
      } else if (i == 13) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR13))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR13))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR13))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR13))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR13))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR13))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR12)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR13))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR8), kk(Factors3$MR13))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(c(intersect(kk(Factors3$MR9), kk(Factors3$MR10)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR11)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR12)),
                       intersect(kk(Factors3$MR9), kk(Factors3$MR13))))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi10 <- unique(c(intersect(kk(Factors3$MR10), kk(Factors3$MR11)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR10), kk(Factors3$MR13))))
        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi11 <- unique(c(intersect(kk(Factors3$MR11), kk(Factors3$MR12)),
                        intersect(kk(Factors3$MR11), kk(Factors3$MR13))))
        MR1111 <- kk(Factors3$MR11)
        MR111111 <- MR1111[!MR1111 %in% MRi11]
        Mk111 <- data.frame(MR11 = kk(Factors3$MR11),
                            loading = as.numeric(kk(Factor_m$MR11)))
        Mk111111 <- Mk111[Mk111$MR11 %in% MR111111, ]
        MR11 <- paste("(", Mk111111$MR11, ")*(", Mk111111$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi12<- unique(intersect(kk(Factors3$MR12), kk(Factors3$MR13)))
        MR1212 <- kk(Factors3$MR12)
        MR121212 <- MR1212[!MR1212 %in% MRi12]
        Mk112 <- data.frame(MR12 = kk(Factors3$MR12),
                            loading = as.numeric(kk(Factor_m$MR12)))
        Mk121212 <- Mk112[Mk112$MR12 %in% MR121212, ]
        MR12 <- paste("(", Mk121212$MR12, ")*(", Mk121212$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MR1313 <- kk(Factors3$MR13)
        MR131313 <- MR1313[!MR1313 %in% MRi13]
        Mk113 <- data.frame(MR13 = kk(Factors3$MR13),
                            loading = as.numeric(kk(Factor_m$MR13)))
        Mk131313 <- Mk113[Mk113$MR13 %in% MR131313, ]
        MR13 <- paste("(", Mk131313$MR13, ")*(", Mk131313$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10, MR11, MR12, MR13),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10",
                                      "MR11", "MR12", "MR13"))
      } else if (i == 12) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR12))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR12))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR12))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR12))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR12))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR12))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR12))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR12))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(c(intersect(kk(Factors3$MR9), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR9), kk(Factors3$MR11)),
                         intersect(kk(Factors3$MR9), kk(Factors3$MR12))))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi10 <- unique(c(intersect(kk(Factors3$MR10), kk(Factors3$MR11)),
                          intersect(kk(Factors3$MR10), kk(Factors3$MR12))))
        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MRi11 <- unique(intersect(kk(Factors3$MR11), kk(Factors3$MR12)))
        MR1111 <- kk(Factors3$MR11)
        MR111111 <- MR1111[!MR1111 %in% MRi11]
        Mk111 <- data.frame(MR11 = kk(Factors3$MR11),
                            loading = as.numeric(kk(Factor_m$MR11)))
        Mk111111 <- Mk111[Mk111$MR11 %in% MR111111, ]
        MR11 <- paste("(", Mk111111$MR11, ")*(", Mk111111$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MR1212 <- kk(Factors3$MR12)
        MR121212 <- MR1212[!MR1212 %in% MRi12]
        Mk112 <- data.frame(MR12 = kk(Factors3$MR12),
                            loading = as.numeric(kk(Factor_m$MR12)))
        Mk121212 <- Mk112[Mk112$MR12 %in% MR121212, ]
        MR12 <- paste("(", Mk121212$MR12, ")*(", Mk121212$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10, MR11, MR12),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10",
                                      "MR11", "MR12"))
      } else if (i == 11) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR11))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR11))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR11))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR11))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR11))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR11))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR11))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR11))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(c(intersect(kk(Factors3$MR9), kk(Factors3$MR10)),
                         intersect(kk(Factors3$MR9), kk(Factors3$MR11))))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi10 <- unique(intersect(kk(Factors3$MR10), kk(Factors3$MR11)))
        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)

        MR1111 <- kk(Factors3$MR11)
        MR111111 <- MR1111[!MR1111 %in% MRi11]
        Mk111 <- data.frame(MR11 = kk(Factors3$MR11),
                            loading = as.numeric(kk(Factor_m$MR11)))
        Mk111111 <- Mk111[Mk111$MR11 %in% MR111111, ]
        MR11 <- paste("(", Mk111111$MR11, ")*(", Mk111111$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10, MR11),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10",
                                      "MR11"))
      } else if (i == 10) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR10))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR10))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR10))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR10))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR10))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR10))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR10))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(c(intersect(kk(Factors3$MR8), kk(Factors3$MR9)),
                         intersect(kk(Factors3$MR8), kk(Factors3$MR10))))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi9 <- unique(intersect(kk(Factors3$MR9), kk(Factors3$MR10)))
        MR99 <- kk(Factors3$MR9)
        MR999 <- MR99[!MR99 %in% MRi9]
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19[Mk19$MR9 %in% MR999, ]
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR1010 <- kk(Factors3$MR10)
        MR101010 <- MR1010[!MR1010 %in% MRi10]
        Mk110 <- data.frame(MR10 = kk(Factors3$MR10),
                            loading = as.numeric(kk(Factor_m$MR10)))
        Mk101010 <- Mk110[Mk110$MR10 %in% MR101010, ]
        MR10 <- paste("(", Mk101010$MR10, ")*(", Mk101010$loading, "))", sep = "",
                      collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8,
                                 MR9, MR10),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9", "MR10"))
      } else if (i == 9) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR9))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR9))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR9))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR9))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR9))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR9))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(c(intersect(kk(Factors3$MR7), kk(Factors3$MR8)),
                         intersect(kk(Factors3$MR7), kk(Factors3$MR9))))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi8 <- unique(intersect(kk(Factors3$MR8), kk(Factors3$MR9)))
        MR88 <- kk(Factors3$MR8)
        MR888 <- MR88[!MR88 %in% MRi8]
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18[Mk18$MR8 %in% MR888, ]
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR999 <- kk(Factors3$MR9)
        Mk19 <- data.frame(MR9 = kk(Factors3$MR9),
                           loading = as.numeric(kk(Factor_m$MR9)))
        Mk999 <- Mk19
        MR9 <- paste("(", Mk999$MR9, ")*(", Mk999$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8, MR9),
                              nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8", "MR9"))
      } else if (i == 8) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR8))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR8))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR8))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR8))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR8))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(c(intersect(kk(Factors3$MR6), kk(Factors3$MR7)),
                         intersect(kk(Factors3$MR6), kk(Factors3$MR8))))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi7 <- unique(intersect(kk(Factors3$MR7), kk(Factors3$MR8)))
        MR77 <- kk(Factors3$MR7)
        MR777 <- MR77[!MR77 %in% MRi7]
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17[Mk17$MR7 %in% MR777, ]
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR888 <- kk(Factors3$MR8)
        Mk18 <- data.frame(MR8 = kk(Factors3$MR8),
                           loading = as.numeric(kk(Factor_m$MR8)))
        Mk888 <- Mk18
        MR8 <- paste("(", Mk888$MR8, ")*(", Mk888$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7, MR8),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5",
                                      "MR6", "MR7", "MR8"))
      } else if (i == 7) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR7))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR7))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR7))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR7))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(c(intersect(kk(Factors3$MR5), kk(Factors3$MR6)),
                         intersect(kk(Factors3$MR5), kk(Factors3$MR7))))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR33 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR4 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi6 <- unique(intersect(kk(Factors3$MR6), kk(Factors3$MR7)))
        MR66 <- kk(Factors3$MR6)
        MR666 <- MR66[!MR66 %in% MRi6]
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16[Mk16$MR6 %in% MR666, ]
        MR6 <- paste("(", Mk555$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR777 <- kk(Factors3$MR7)
        Mk17 <- data.frame(MR7 = kk(Factors3$MR7),
                           loading = as.numeric(kk(Factor_m$MR7)))
        Mk777 <- Mk17
        MR7 <- paste("(", Mk777$MR7, ")*(", Mk777$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6, MR7),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6",
                                      "MR7"))
      } else if (i == 6) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR6))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR6))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR6))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(c(intersect(kk(Factors3$MR4), kk(Factors3$MR5)),
                         intersect(kk(Factors3$MR4), kk(Factors3$MR6))))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi5 <- unique(intersect(kk(Factors3$MR5), kk(Factors3$MR6)))
        MR55 <- kk(Factors3$MR5)
        MR555 <- MR55[!MR55 %in% MRi5]
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15[Mk15$MR5 %in% MR555, ]
        MR5 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR666 <- kk(Factors3$MR6)
        Mk16 <- data.frame(MR6 = kk(Factors3$MR6),
                           loading = as.numeric(kk(Factor_m$MR6)))
        Mk666 <- Mk16
        MR6 <- paste("(", Mk666$MR6, ")*(", Mk666$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5, MR6),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5", "MR6"))
      } else if (i == 5) {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR5))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR5))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4)),
                         intersect(kk(Factors3$MR3), kk(Factors3$MR5))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi4 <- unique(intersect(kk(Factors3$MR4), kk(Factors3$MR5)))
        MR44 <- kk(Factors3$MR4)
        MR444 <- MR44[!MR44 %in% MRi4]
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14[Mk14$MR4 %in% MR444, ]
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR555 <- kk(Factors3$MR5)
        Mk15 <- data.frame(MR5 = kk(Factors3$MR5),
                           loading = as.numeric(kk(Factor_m$MR5)))
        Mk555 <- Mk15
        MR5 <- paste("(", Mk555$MR5, ")*(", Mk555$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2, MR3, MR4, MR5),
                               nm = c("MR1", "MR2", "MR3", "MR4", "MR5"))
      } else if (i == 4)  {
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR4))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3)),
                         intersect(kk(Factors3$MR2), kk(Factors3$MR4))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi3 <- unique(c(intersect(kk(Factors3$MR3), kk(Factors3$MR4))))
        MR33 <- kk(Factors3$MR3)
        MR333 <- MR33[!MR33 %in% MRi3]
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13[Mk13$MR3 %in% MR333, ]
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR444 <- kk(Factors3$MR4)
        Mk14 <- data.frame(MR4 = kk(Factors3$MR4),
                           loading = as.numeric(kk(Factor_m$MR4)))
        Mk444 <- Mk14
        MR4 <- paste("(", Mk444$MR4, ")*(", Mk444$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3, MR4),
                               nm = c("MR1", "MR2", "MR3", "MR4"))
      } else if (i == 3){
        Mk999 <- Mk888 <- Mk777 <- Mk666 <- Mk555 <- Mk444 <- NULL
        MRi1 <- unique(c(intersect(kk(Factors3$MR1), kk(Factors3$MR2)),
                         intersect(kk(Factors3$MR1), kk(Factors3$MR3))))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("(", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MRi2 <- unique(c(intersect(kk(Factors3$MR2), kk(Factors3$MR3))))
        MR22 <- kk(Factors3$MR2)
        MR222 <- MR22[!MR22 %in% MRi2]
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12[Mk12$MR2 %in% MR222, ]
        MR2 <- paste("(", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR333 <- kk(Factors3$MR3)
        Mk13 <- data.frame(MR3 = kk(Factors3$MR3),
                           loading = as.numeric(kk(Factor_m$MR3)))
        Mk333 <- Mk13
        MR3 <- paste("(", Mk333$MR3, ")*(", Mk333$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        expression <- setNames(c(MR1, MR2, MR3), nm = c("MR1", "MR2", "MR3"))
      } else{
        MRi1 <- unique(intersect(kk(Factors3$MR1), kk(Factors3$MR3)))
        MR11 <- kk(Factors3$MR1)
        MR111 <- MR11[!MR11 %in% MRi1]
        Mk11 <- data.frame(MR1 = kk(Factors3$MR1),
                           loading = as.numeric(kk(Factor_m$MR1)))
        Mk111 <- Mk11[Mk11$MR1 %in% MR111, ]
        MR1 <- paste("((", Mk111$MR1, ")*(", Mk111$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)

        MR222 <- kk(Factors3$MR2)
        Mk12 <- data.frame(MR2 = kk(Factors3$MR2),
                           loading = as.numeric(kk(Factor_m$MR2)))
        Mk222 <- Mk12
        MR2 <- paste("((", Mk222$MR2, ")*(", Mk222$loading, "))", sep = "",
                     collapse = "+") %>%
          eees(.)
        expression <- setNames(c(MR1, MR2), nm = c("MR1", "MR2"))
      }
    }
  }


  MRi_data <- DATA %>%
    mutate(!!!expression, .keep = "none")

  results <- list(Latent_frame = MRi_data,
                  Latent_1  = Mk111,
                  Latent_2  = Mk222,
                  Latent_3  = Mk333,
                  Latent_4  = Mk444,
                  Latent_5  = Mk555,
                  Latent_6  = Mk666,
                  Latent_7  = Mk777,
                  Latent_8  = Mk888,
                  Latent_9  = Mk999,
                  Latent_10 = Mk101010,
                  Latent_11 = Mk111111,
                  Latent_12 = Mk121212,
                  Latent_13 = Mk131313,
                  Latent_14 = Mk141414,
                  Latent_15 = Mk151515)
  return(results)
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

eees <- function(x) {
  if (inherits(x, "connection")) {
    if (!isOpen(x)) {
      open(x)
      on.exit(close(x))
    }
    exprs <- parse(file = x, keep.source = FALSE)
  } else if (is.character(x)) {
    exprs <- cccs(x)
  } else {
    stop_input_type(x, "a character vector or an R connection")
  }
  as.list(exprs)
}

cccs <- function(x) {
  parsed <- purrr::map(x, function(elt) as.list(chrss(elt)))

  nms <- names(parsed)
  parsed <- unname(parsed)

  if (!base::is.null(nms)) {
    nms <- purrr::list_c(purrr::map2(parsed, nms, rep_along))
  }
  if (length(parsed)) {
    parsed <- purrr::list_c(parsed)
  }
}

chrss <- function(x) {
  parse(text = x, keep.source = FALSE)
}
