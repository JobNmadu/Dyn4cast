#' Garrett Ranking of Categorical Data
#'
#' @description
#' There are three main types of ranking: Standard competition, Ordinal and
#' Fractional. Garrett's Ranking Technique is the application of fractional
#' ranking in which the data points are ordered and given an ordinal
#' number/rank. The ordering and ranking provide additional information which
#'  may not be available from frequency distribution. Again, the ordering is
#' based on the level of seriousness or severity of the data point from the view
#'  point of the respondent. Ranking enables ease of comparison and makes
#'  grouping more meaningful. It is used in social science, psychology and
#' other survey types of research. This functions performs Garrett Ranking
#' of up to 15 ranks.
#'
#' @param data The data for the Garrett Ranking, must be a `data.frame`.
#' @param num_rank A vector representing the number of ranks applied to the
#' data. If the data is a five-point Likert-type data, then number of ranks is
#'  5.
#' @param ranking A vector of list representing the ranks applied to the data.
#' If not available, positional ranks are applied.
#' @param m_rank The scope of the ranking methods which is between 2 and 15.
#'
#' @aliases garrett_table
#' @aliases garrett_data
#'
#' @return A list with the following components:
#' \item{\code{RII}}{Relative importance index.}
#' \item{\code{Garrett ranked data}}{Table of data ranked using Garrett
#' mean score.}
#' \item{\code{Garrett value}}{Table of ranking Garrett values}
#'
#' @export garrett_ranking
#'
#' @examples
#' library(readr)
#' garrett_data <- data.frame(garrett_data)
#' ranking <- c("Serious constraint", "Constraint",
#' "Not certain it is a constraint", "Not a constraint",
#' "Not a serious constraint")
#'
#' ## ranking is supplied
#' garrett_ranking(garrett_data, 5, ranking)
#'
#' # ranking not supplied
#' garrett_ranking(garrett_data, 5)
#'
#' # you can rank subset of the data
#' garrett_ranking(garrett_data, 8)
#'
#' garrett_ranking(garrett_data, 4)

garrett_ranking <- function(data, num_rank,
                            ranking = NULL,
                            m_rank = c(2:15)) {
  num_rank <- num_rank
  right <- intersect(m_rank, num_rank)
  ranking <- ranking
  garrett_table <- garrett_table
  Data <- data.frame(data)
  ggn <- names(Data)
  pr <- nrow(Data)
  pc <- ncol(Data)
  garrett <- as.data.frame(matrix(pc, num_rank))
  garrett_i <- as.data.frame(matrix(pc, num_rank))
  z_m <- as.data.frame(matrix(pc, 5))
  gv <- as.data.frame(matrix(num_rank, 3))
  cuty <- mean(1:num_rank)

  if (is.null(ranking)) {
    if (length(right) == 0) {
      num_rank <- 0
      stop("Number of ranks must be an integer between 2 and 15")
    } else {
      num_rank < num_rank
      if (num_rank == 2) {
        ranking <- paste0(1:num_rank, c("st", "nd"), " Rank")
      } else {
        ranking <- paste0(1:num_rank, c("st", "nd", "rd", rep("th",
                                                            num_rank - 3)),
                        " Rank")
        }
    }
    } else {
    ranking <- ranking
  }

  for (i in 1: pc) {
    X <- Data[, i]
    z_m[i, 1] <- mean(X)
    for (p in 1: num_rank) {
      gv[p, 1] <- p
      gv[p, 2] <- (100 * (p - .5)) / pc
      gv[p, 3] <- which(abs(garrett_table[, 2] - gv[p, 2]) ==
                          min(abs(garrett_table[, 2] - gv[p, 2])))
      if (num_rank == 2) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 3) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else{
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 4) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 5) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 6) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 7) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 8) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 9) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 10) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 11) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 11] / 11)
          garrett_i[i, p] <-  garrett[i, p] * 11
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 3) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 10) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1

        }
      } else if (num_rank == 12) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 12] / 12)
          garrett_i[i, p] <-  garrett[i, p] * 12
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 11] / 11)
          garrett_i[i, p] <-  garrett[i, p] * 11
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 4) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 10) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 11) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 13) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 13] / 13)
          garrett_i[i, p] <-  garrett[i, p] * 13
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 12] / 12)
          garrett_i[i, p] <-  garrett[i, p] * 12
        } else if (p == 3) {
          garrett[i, p] <- sum(X[X == 11] / 11)
          garrett_i[i, p] <-  garrett[i, p] * 11
        } else if  (p == 4) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 5) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 10) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 11) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 12) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      } else if (num_rank == 14) {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 14] / 14)
          garrett_i[i, p] <-  garrett[i, p] * 14
        } else if  (p == 2) {
          garrett[i, p] <- sum(X[X == 13] / 13)
          garrett_i[i, p] <-  garrett[i, p] * 13
        } else if (p == 3) {
          garrett[i, p] <- sum(X[X == 12] / 12)
          garrett_i[i, p] <-  garrett[i, p] * 12
        } else if (p == 4) {
          garrett[i, p] <- sum(X[X == 11] / 11)
          garrett_i[i, p] <-  garrett[i, p] * 11
        } else if  (p == 5) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 6) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 7) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 10) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 11) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 12) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 13) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      }else {
        if (p == 1) {
          garrett[i, p] <- sum(X[X == 15] / 15)
          garrett_i[i, p] <-  garrett[i, p] * 15
        } else if (p == 2) {
          garrett[i, p] <- sum(X[X == 14] / 14)
          garrett_i[i, p] <-  garrett[i, p] * 14
        } else if  (p == 3) {
          garrett[i, p] <- sum(X[X == 13] / 13)
          garrett_i[i, p] <-  garrett[i, p] * 13
        } else if (p == 4) {
          garrett[i, p] <- sum(X[X == 12] / 12)
          garrett_i[i, p] <-  garrett[i, p] * 12
        } else if (p == 5) {
          garrett[i, p] <- sum(X[X == 11] / 11)
          garrett_i[i, p] <-  garrett[i, p] * 11
        } else if  (p == 6) {
          garrett[i, p] <- sum(X[X == 10] / 10)
          garrett_i[i, p] <-  garrett[i, p] * 10
        } else if (p == 7) {
          garrett[i, p] <- sum(X[X == 9] / 9)
          garrett_i[i, p] <-  garrett[i, p] * 9
        } else if  (p == 8) {
          garrett[i, p] <- sum(X[X == 8] / 8)
          garrett_i[i, p] <-  garrett[i, p] * 8
        } else if  (p == 9) {
          garrett[i, p] <- sum(X[X == 7] / 7)
          garrett_i[i, p] <-  garrett[i, p] * 7
        } else if  (p == 10) {
          garrett[i, p] <- sum(X[X == 6] / 6)
          garrett_i[i, p] <-  garrett[i, p] * 6
        } else if  (p == 11) {
          garrett[i, p] <- sum(X[X == 5] / 5)
          garrett_i[i, p] <-  garrett[i, p] * 5
        } else if  (p == 12) {
          garrett[i, p] <- sum(X[X == 4] / 4)
          garrett_i[i, p] <-  garrett[i, p] * 4
        } else if  (p == 13) {
          garrett[i, p] <- sum(X[X == 3] / 3)
          garrett_i[i, p] <-  garrett[i, p] * 3
        } else if  (p == 14) {
          garrett[i, p] <- sum(X[X == 2] / 2)
          garrett_i[i, p] <-  garrett[i, p] * 2
        } else {
          garrett[i, p] <- sum(X[X == 1] / 1)
          garrett_i[i, p] <-  garrett[i, p] * 1
        }
      }
    }
  }

  names(gv) <- c("Number", "Garrett Point", "Garrett index")
  gv_p <- gv %>%
    dplyr::filter(gv$`Garrett index` %in% garrett_table$Number)
  garrett_p <- dplyr::left_join(garrett_table, gv,
                                by = c("Number" = "Garrett index")) %>%
    kk(.) %>%
    dplyr::select(., c(4, 5, 1, 3))
  names(garrett_p) <- c("Number", "Garrett point", "Garrett index",
                        "Garrett value")
  `Garrett value` <- t(data.frame(garrett_p[, 4]))

  `Total Garrett Score` <- 0
  for (p in 1: num_rank) {
    `Total Garrett Score` <- `Total Garrett Score` +
      garrett[, p] * `Garrett value`[p]
  }

  garrett$V16 <- rowSums(garrett)
  garrett_d <- dplyr::bind_cols(ggn, garrett, z_m, `Total Garrett Score`)

  row.names(z_m) <- ggn
  z_m <- rownames_to_column(z_m, var = "Description")
  names(z_m) <- c("Description", "Mean")
  z_m$`Total Item score` <- rowSums(garrett_i)
  z_m$`Relative importance index` <- rowSums(garrett_i / (num_rank * pr))
  #z_m$Remark <- ifelse(z_m$Mean > cuty, "Above",
  #                     ifelse(z_m$Mean == cuty, "Equal", "Below"))
  #z_m <- z_m[order(-z_m$Mean), ] %>%
  #  mutate(Rank = dplyr::row_number()) %>%
  #  rownames_to_column(., var = "S/No")

  names(garrett_d) <- c("Description", ranking, "Total", "Mean",
                        "Total Garrett Score")
  garrett_d$`Mean Garrett score` <- garrett_d$`Total Garrett Score` /
    garrett_d$Total
  garrett_d$`Total Item score` <- rowSums(garrett_i)
  garrett_d$`Relative importance index` <- rowSums(garrett_i / (num_rank * pr))
  garrett_d <- garrett_d[order(-garrett_d$`Mean Garrett score`), ] %>%
    mutate(Rank = dplyr::row_number()) %>%
    rownames_to_column(., var = "S/No")
  results <- list(`Garrett value` = garrett_p,
                  `Garrett ranked data` = garrett_d, RII = garrett_i)
  return(results)
}

kk <- function(x, v = NULL) {
  if (!is.data.frame(x))
    x <- as.data.frame(x)
  if (is.null(v))
    v <- names(x)
  r <- x[stats::complete.cases(x[v]), ]
  return(r)
}
