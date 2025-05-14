#' Plot of Order of Significance of Estimated Regression Coefficients
#'
#' @description
#' This function provides graphic displays of the order of significance
#' estimated coefficients of models. This would assists in accessing models so
#'  as to decide which can be used for further analysis, prediction and policy
#'  consideration.
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

xnumt <- function(x, decimals = 2, signif = NULL, type = 2,
                  pre = "", pos = "", sign = FALSE, abbr = FALSE, ...) {
  if (sign)
    signs <- ifelse(x > 0, "+", "")
  if (is.null(decimals))
    decimals <- getOption("digits")
  x <- base::round(x, digits = decimals)
  if (!is.null(signif))
    x <- base::signif(x, signif)
  if (abbr) {
    x <- num_abbr(x, n = decimals + 1)
  } else {
    if (is.null(decimals))
      decimals <- 0L
    if (type == 1) {
      x <- format(as.numeric(x), big.mark = ".", decimal.mark = ",",
                  ...)
    } else {
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

num_abbr <- function(x, n = 3) {
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
  x <- round(x, -nchar(round(x, 0)) + n) / 10^(3 * (div - 1))
  div <- ifelse(nchar(as.integer(x)) > 3, div + 1, div)
  x <- ifelse(nchar(as.integer(x)) > 3, x / 1000, x)
  x <- round(x, 3)
  x <- paste0(x, c("", "K", "M", "B", "T", "Qa", "Qi")[div])
  output <- paste0(negative_positions, x)
  output[grepl("NA", output)] <- NA
  return(output)
}

"dyn4_misc" <- function(object, ...){
  UseMethod("varImp")
}

GarsonWeights <- function(object)
{
  beta <- coef(object)
  abeta <- abs(beta)
  nms <- names(beta)
  i2h <- array(NA, dim = object$n[2:1])
  h2o <- array(NA, dim = object$n[2:3])

  for (hidden in 1:object$n[2]) {
    for (input in 1:object$n[1]) {
      label <- paste("i", input, "->h", hidden,"$", sep = "")
      i2h[hidden, input] <- abeta[grep(label, nms, fixed = FALSE)]
    }
  }
  for(hidden in 1:object$n[2]){
    for(output in 1:object$n[3]){
      label <- paste("h", hidden, "->o",
                     ifelse(object$n[3] == 1, "", output),
                     sep = "")
      h2o[hidden,output] <- abeta[grep(label, nms, fixed = TRUE)]
    }
  }

  if(FALSE)
  {
    i2h <- matrix(c(-1.67624,  3.29022,  1.32466,
                    -0.51874, -0.22921, -0.25526,
                    -4.01764,  2.12486, -0.08168,
                    -1.75691, -1.44702,  0.58286),
                  ncol = 3, byrow = TRUE)
    h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221),
                  ncol = 1)
  }
  imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])
  for(output in 1:object$n[3])
  {
    Pij <- i2h * NA
    for(hidden in 1:object$n[2]) Pij[hidden,] <- i2h[hidden,] * h2o[hidden,
                                                                    output]
    Qij <- Pij * NA
    for(hidden in 1:object$n[2]) Qij[hidden,] <- Pij[hidden,] /
      sum(Pij[hidden,])
    Sj <- apply(Qij, 2, sum)

    imp[,output] <- Sj/sum(Sj)*100
    rm(Pij, Qij, Sj)
  }

  colnames(imp) <- if(!is.null(colnames(object$residuals)))
    colnames(object$residuals) else paste("Y", 1:object$n[3], sep = "")
  rownames(imp) <- if(!is.null(object$coefnames)) object$coefnames
  else  paste("X", 1:object$n[1], sep = "")
  imp
}

GarsonWeights_FCNN4R <- function (object, xnames = NULL, ynames = NULL) {
  beta <- abs(object$net@m_w_values[which(object$net@m_w_flags != 0L)])
  dims <- object$net@m_layers

  index <- (dims[1]+1)*dims[2]
  i2h <- t(matrix(beta[1:index], ncol = dims[2]))
  i2h <- i2h[, -1,drop = FALSE]

  h2o <- matrix(beta[(index+1):length(beta)], ncol = dims[3])
  h2o <- h2o[-1,,drop = FALSE]

  imp <- matrix(NA, nrow = dims[1], ncol = dims[3])
  for (output in 1:dims[3]) {
    Pij <- i2h * NA
    for (hidden in 1:dims[2]) Pij[hidden, ] <- i2h[hidden,] * h2o[hidden,
                                                                  output]
    Qij <- Pij * NA
    for (hidden in 1:dims[2]) Qij[hidden, ] <- Pij[hidden,]/sum(Pij[hidden, ])
    Sj <- apply(Qij, 2, sum)
    imp[, output] <- Sj/sum(Sj) * 100
    rm(Pij, Qij, Sj)
  }
  rownames(imp) <- if(is.null(xnames))
    paste("X", 1:dims[1], sep = "") else
      xnames
  colnames(imp) <- if(is.null(ynames))
    paste("Y", 1:dims[3], sep = "") else
      ynames
  imp
}

varImpDependencies <- function(libName){
  code <- getModelInfo(libName, regex = FALSE)[[1]]
  checkInstall(code$library)
  for(i in seq(along.with = code$library))
    do.call("requireNamespaceQuietStop", list(package = code$library[i]))
  return(code)
}

varImp.bagEarth <- function(object, ...){
  code <- varImpDependencies("bagEarth")
  code$varImp(object, ...)
}

varImp.bagFDA <- function(object, ...){
  code <- varImpDependencies("bagFDA")
  code$varImp(object, ...)
}

varImp.C5.0 <- function(object, ...){
  code <- varImpDependencies("C5.0")
  code$varImp(object, ...)
}

varImp.cubist <- function(object, weights = c(0.5, 0.5), ...){
  code <- varImpDependencies("cubist")
  code$varImp(object, weights = weights, ...)
}


varImp.dsa <- function(object, cuts = NULL, ...){
  code <- varImpDependencies("partDSA")
  code$varImp(object, cuts = cuts, ...)
}

varImp.glm <- function(object, ...){
  code <- varImpDependencies("glm")
  code$varImp(object, ...)
}

varImp.glmnet <- function(object, lambda = NULL, ...){
  code <- varImpDependencies("glmnet")
  code$varImp(object, lambda = lambda, ...)
}

varImp.JRip <- function(object, ...){
  code <- varImpDependencies("JRip")
  code$varImp(object, ...)
}

varImp.multinom <- function(object, ...){
  code <- varImpDependencies("multinom")
  code$varImp(object, ...)
}

varImp.nnet <- function(object, ...){
  code <- varImpDependencies("nnet")
  code$varImp(object, ...)
}

varImp.avNNet <- function(object, ...){
  code <- varImpDependencies("nnet")
  imps <- lapply(object$model, code$varImp)
  imps <- do.call("rbind", imps)
  imps <- aggregate(imps, by = list(vars = rownames(imps)), mean)
  rownames(imps) <- as.character(imps$vars)
  imps$vars <- NULL
  imps
}

varImp.PART <- function(object, ...){
  code <- varImpDependencies("PART")
  code$varImp(object, ...)
}

varImp.RRF <- function(object, ...){
  code <- varImpDependencies("RRF")
  code$varImp(object, ...)
}

varImp.rpart <- function(object, surrogates = FALSE, competes = TRUE, ...){
  code <- varImpDependencies("rpart")
  code$varImp(object, surrogates = surrogates, competes = competes, ...)
}

varImp.randomForest <- function(object, ...){
  code <- varImpDependencies("rf")
  code$varImp(object, ...)
}

varImp.gbm <- function(object, numTrees = NULL, ...){
  code <- varImpDependencies("gbm")
  code$varImp(object, numTrees = numTrees, ...)
}

varImp.classbagg <- function(object, ...){
  code <- varImpDependencies("treebag")
  code$varImp(object, ...)
}

varImp.regbagg <- function(object, ...){
  code <- varImpDependencies("treebag")
  code$varImp(object, ...)
}

varImp.pamrtrained <- function(object, threshold, data, ...){
  code <- varImpDependencies("pam")
  code$varImp(object,
              threshold = object$bestTune$threshold,
              data = object$finalModel$xData,
              ...)
}

varImp.lm <- function(object, ...){
  code <- varImpDependencies("lm")
  code$varImp(object, ...)
}

varImp.mvr <- function(object, estimate = NULL, ...){
  code <- varImpDependencies("pls")
  code$varImp(object, estimate = estimate, ...)
}

varImp.earth <- function(object, value = "gcv", ...){
  code <- varImpDependencies("earth")
  code$varImp(object, value = value, ...)
}

varImp.RandomForest <- function(object, ...){
  code <- varImpDependencies("cforest")
  code$varImp(object, ...)
}

varImp.plsda <- function(object, ...){
  code <- varImpDependencies("pls")
  code$varImp(object, ...)
}

varImp.fda <- function(object, value = "gcv", ...){
  code <- varImpDependencies("fda")
  code$varImp(object, value = value, ...)
}

varImp.gam <- function(object, ...){
  code <- varImpDependencies("gam")
  code$varImp(object, ...)
}

varImp.Gam <- function(object, ...){
  code <- varImpDependencies("gamSpline")
  code$varImp(object, ...)
}

modelLookup <- function(model){
  load(system.file("models", "models.RData", package = "caret"))
  if(!is.null(model)){
    if(!(model %in% names(models))) stop(paste("Model is not in the ",
                                               "set of existing models",
                                               sep = ""))

    models <- models[model == names(models)]
  }
  out <- lapply(models,
                function(x) {
                  out <- x$parameters[, c("parameter", "label")]
                  out$forReg <- "Regression" %in% x$type
                  out$forClass <- "Classification" %in% x$type
                  out$probModel <- !is.null(x$prob)
                  out
                })
  for(i in seq(along.with = out)) out[[i]]$model <- names(models)[i]
  out <- do.call("rbind", out)
  rownames(out) <- NULL
  out <- out[, c('model', 'parameter', 'label', 'forReg', 'forClass',
                 'probModel')]
  out[order(out$model),]
}

missing_packages <- function(mods = getModelInfo()) {
  libs <- unique(unlist(lapply(mods, function(x) x$library)))
  here <- rownames(installed.packages())
  libs[!(libs %in% here)]
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
