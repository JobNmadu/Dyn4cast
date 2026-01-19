#' Custom plot of correlation matrix
#'
#' @description
#' This is a custom plot for correlation matrix in which the coefficients are
#' displayed along with graphics showing the magnitude of each coefficient.
#'
#' @param r Correlation matrix of the data for the plot
#'
#' @return The function returns a custom plot of the correlation matrix
#' \item{\code{corplot}}{The custom plot of the correlation matrix}
#'
#' @export corplot
#'
#' @importFrom corrplot corrplot.mixed
#'
corplot <- function(r) {
  plot <- function() {
    corrplot::corrplot.mixed(r, bg = "forestgreen", lower.col = "black",
                             tl.pos = "lt", tl.col = "darkgreen")
  }
  list(plot = plot)
}
