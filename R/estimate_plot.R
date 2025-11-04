#' Plot of Order of Significance of Estimated Regression Coefficients
#'
#' @description
#' This function provides graphic displays of the estimated coefficients in the
#'  order of their significance in the models. This would assists in accessing
#'   models to decide which can be used for further analysis, prediction and
#'   policy consideration.
#'
#' @param model25 Estimated model for which the estimated coefficients would be
#'  plotted
#' @param limit Number of variables to be included in the coefficients plots
#'
#' @return The function returns a plot of the order of importance of the
#' estimated coefficients
#' \item{\code{estimate_plot}}{The plot of the order of importance of estimated
#'  coefficients}
#'
#' @export estimate_plot
#'
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 theme_minimal
#' @importFrom ggplot2 geom_col
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 guides
#' @importFrom ggplot2 geom_text
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 .data
#' @importFrom stats aggregate
#' @importFrom stats coef
#' @importFrom utils install.packages
#' @importFrom utils installed.packages
#' @importFrom utils  menu
#'
estimate_plot <- function(model25, limit) {
  modelv <- dyn4_misc(model25)
  variables <- row.names(modelv)
  modelv <- cbind(variables, modelv)
  kk0(var = modelv$variables, imp = modelv$Overall, limit = limit,
     colours = modelv$variables)
}

kk0 <- function(var, imp, limit, colours = NA) {
  if (is.null(imp)) {
    return(NULL)
  }
  if (length(var) != length(imp)) {
    message("Estimates and relevance vectors should be the same length.")
    stop(message(paste("There are", length(var),
                       "estimates and", length(imp), "relevance values!")))
  }
  if (is.na(colours[1])) {
    colours <- "gold"
  }
  out <- data.frame(var = var, imp = 100 * imp, Type = colours)
  if (length(var) < limit)
    limit <- length(var)
  output <- out[1:limit, ]
  p <- ggplot2::ggplot(output, aes(x = stats::reorder(.data$var, .data$imp),
                                   y = .data$imp,
                                   label = xnumt(.data$imp, 1))) +
    ggplot2::geom_col(aes(fill = .data$Type),
                      width = 0.08, colour = "transparent") +
    ggplot2::geom_point(ggplot2::aes(colour = .data$Type),
                        size = 6.2) + ggplot2::coord_flip() +
    ggplot2::geom_text(hjust = 0.5,
                       size = 2.1, inherit.aes = TRUE, colour = "white") +
    ggplot2::labs(title = paste0("Order of significance from ", limit,
                                 " to ", 1),
                  x = NULL, y = NULL) +
    ggplot2::scale_y_continuous(position = "right", expand = c(0, 0),
                                limits = c(0, 1.03 * max(output$imp))) +
    ggplot2::guides(fill = "none", colour = "none") +
    ggplot2::theme_minimal()
  if (length(unique(output$Type)) == 1) {
    p <- p +
      ggplot2::geom_col(fill = colours, width = 0.2, colour = "transparent") +
      ggplot2::geom_point(colour = colours, size = 6) +
      ggplot2::guides(fill = "none", colour = "none") +
      ggplot2::geom_text(hjust = 0.5, size = 2, inherit.aes = TRUE,
                         colour = "white")
  }
  return(p)
}

xnumt <- function (x, decimals = 2, signif = NULL, type = 2,
                   pre = "", pos = "", sign = FALSE, abbr = FALSE, ...)
{
  if (sign)
    signs <- ifelse(x > 0, "+", "")
  if (is.null(decimals))
    decimals <- getOption("digits")
  x <- base::round(x, digits = decimals)
  if (!is.null(signif))
    x <- base::signif(x, signif)
  if (abbr) {
    x <- num_abbr(x, n = decimals + 1)
  }
  else {
    if (is.null(decimals))
      decimals <- 0L
    if (type == 1) {
      x <- format(as.numeric(x), big.mark = ".", decimal.mark = ",",
                  ...)
    }
    else {
      x <- format(as.numeric(x), big.mark = ",", decimal.mark = ".",
                  ...)
    }
    x <- trimws(x)
  }
  if (pre == "$")
    x <- gsub("\\$-", "-$", x)
  if (sign)
    x <- paste0(signs, x)
  ret <- paste0(pre, x, pos)
  return(ret)
}

num_abbr <- function (x, n = 3)
{
  if (!is.numeric(x))
    stop("Input vector x needs to be numeric.")
  if (!is.numeric(n))
    stop("n needs to be numeric.")
  if (length(n) > 1)
    stop("Please make sure that n takes on a single value.")
  if (!n %in% 1:6)
    stop("Please make sure that n takes on an interger value between 1 to 6.")
  negative_positions <- ifelse(x < 0, "-", "")
  x <- abs(x)
  div <- findInterval(x, c(0, 1000, 1e+06, 1e+09, 1e+12, 1e+15,
                           1e+18))
  x <- round(x, -nchar(round(x, 0)) + n)/10^(3 * (div - 1))
  div <- ifelse(nchar(as.integer(x)) > 3, div + 1, div)
  x <- ifelse(nchar(as.integer(x)) > 3, x/1000, x)
  x <- round(x, 3)
  x <- paste0(x, c("", "K", "M", "B", "T", "Qa", "Qi")[div])
  output <- paste0(negative_positions, x)
  output[grepl("NA", output)] <- NA
  return(output)
}

"dyn4_misc" <- function(object, ...){
  UseMethod("varImp")
}

varImpDependencies <- function(libName){
  code <- getModelInfo(libName, regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along.with = code$library))
    do.call("requireNamespaceQuietStop", list(package = code$library[i]))
  return(code)
}

varImp.lm <- function(object, ...){
  code <- varImpDependencies("lm")
  code$varImp(object, ...)
}

checkInstall <- function(pkg){
  good <- rep(TRUE, length(pkg))
  for(i in seq(along.with = pkg)){
    tested <- try(find.package(pkg[i]), silent = TRUE)
    if (inherits(tested, "try-error")) good[i] <- FALSE
  }
  if(any(!good)){
    pkList <- paste(pkg[!good], collapse = ", ")
    msg <- paste(sum(!good),
                 ifelse(sum(!good) > 1, " packages are", " package is"),
                 " needed and",
                 ifelse(sum(!good) > 1, " are", " is"),
                 " not installed. (",
                 pkList,
                 "). Would you like to try to install",
                 ifelse(sum(!good) > 1, " them", " it"),
                 " now?",
                 sep = "")

    if(interactive()) {
      cat(msg)
      bioc <- c("affy", "logicFS", "gpls", "vbmp")
      installChoice <- menu(c("yes", "no"))
      if(installChoice == 1){
        hasBioc <- any(pkg[!good] %in% bioc)
        if(!hasBioc) {
          install.packages(pkg[!good])
        } else {
          inst <- pkg[!good]
          instC <- inst[!(inst %in% bioc)]
          instB <- inst[inst %in% bioc]
          if(length(instC) > 0) install.packages(instC)
          biocLite <- NULL
          source("http://bioconductor.org/biocLite.R")
          biocLite(instB)
        }
      } else  {
        stop("Required packages are missing: ", pkList, call. = FALSE)
      }
    } else {
      stop("Required packages are missing: ", pkList, call. = FALSE)
    }
  }
}

getModelInfo <- function(model, regex = TRUE, ...) {
  load(system.file("models", "models.RData", package = "caret"))
  if(!is.null(model)){
    keepers <- if(regex) grepl(model, names(models), ...)
    else which(model == names(models))[1]
    models <- models[keepers]
  }
  if(length(models) == 0) stop("That model is not in caret's built-in library")
  models
}
