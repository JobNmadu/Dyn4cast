
#' Sequential Computation of Dynamic Multidimensional Poverty Indices (MDPI)
#'
#'@description
#' This function computes the indices and all associated measures of
#'  multidimensional poverty sequentially in a dynamic way. Sequentially
#'   the function computes _Incidence of poverty (H = q / n)_,
#'  _Adjusted incidence of poverty (H / (q / D))_, _Deprivation Score_ of each
#'   dimension in the computation, _Intensity of poverty (A)_,
#'    _Multidimensional poverty index (MDPI = H * A)_, the _Contribution_ in
#'    % of each of the dimensions to MDPI, and
#'    _Average deprivation among the deprived (A * D)_. Dynamically, it
#'    computes the various indices for between three and nine `dimensions (D)`.
#'     The first five dimensions included in the computations are _Health_,
#'      _Education_, _Living standard_, _Social security_ and,
#'       _Employment and Income_ depending on the choice of the user. Four
#'       additional dimensions can be included in the computations. The
#'       computations are carried out either for the `national sample data` or
#'       can be dis-aggregated based on `grouping factors`, like region, sex,
#'       gender, marital status or any suitable one. The cut-off mark
#'       demarcating `poor (q)` and `non-poor (n-q)` members in the `sample (n)`
#'        is defaulted to `0.4` but can be varied as may be dictated by the
#'        interests or the need for the computation. The computations are in
#'         line with various procedures already outlined in literature starting
#'          with the work of Alkire et. al, (2015) but has been expanded from
#'           three dimensions to nine. Each dimension is given `equal weight` in
#'            the computation but all indicators are weighted in line with
#'            existing guidelines in Alkire & Foster (2011) and Alkire & Santos
#'             (2010). See also Alkire & Santos (2014) and Chan & Wong (2024).
#'
#' @param data `data frame` containing all the variables for the computation.
#' Note that the variables to be used for the computation must be coded `(0,1)`.
#' @param dm list of vectors of _indicators_ making up each _dimension_ to be
#'  computed
#' @param Bar an optional vector of cut-of used to divide the population into
#' those in the poverty category and those that are not. Defaults to 0.4 if not
#'  supplied.
#' @param id_addn an optional vector of additional dimensions to be used for the
#'  computation up to a _maximum of four_.
#' @param Factor an optional grouping factor for the computation which must be a
#'  variable in the *data*. If not supplied, only the national MDPI will be
#'   computed.
#' @param id a vector of the first three dimensions used in the computation
#'  given as **Health**, **Education** and **Living standard**. Can be redefined
#'   but must match the indicators and cannot be `NULL`.
#' @param id_add a vector of the fourth dimension in the computation given
#' as **Social security**. Can be re-defined but never `NULL`.
#' @param id_add1 a vector of the fifth dimension in the computation given
#'  as **Employment and Income**. Can be re-defined but never `NULL`.
#' @param plots plots of the various measures. For this to be possible, the
#' number of options in the `Factor` argument must be less than 41. The default
#'  is `NULL`. To produce the plots, any character string will overwrite the
#'   default.
#' @param Echo Optional indicating whether the progress note is visible
#'  defaults to TRUE.
#'
#' @returns A list with the following components:
#' \item{\code{MDPI_p}}{Publication-ready table of the factor and national
#' MDPI prepared with `summarymodels package`. Will not _return_ if only
#' national computation is carried out.}
#' \item{\code{MDPI}}{`Data frame` of the factor and national MDPI. Will
#'  not _return_ if only national computation is carried out.}
#'  \item{\code{MDPI mean}}{`Data frame` of the mean MDPI. Will not _return_ if
#'   only national computation is carried out.}
#'  \item{\code{MDPI SD}}{`Data frame` of the SD of MDPI. Will not _return_ if
#'   only national computation is carried out.}
#'   \item{\code{national}}{`Data frame` of national MDPI with mean and SD.}
#'  \item{\code{dimensions}}{`Data frame` of the scores for each dimension in
#' the computation.}
#'  \item{\code{Score}}{`Data frame` of the scores for each indicator in the
#'  computation.}
#'
#' @importFrom tidyselect all_of
#'
#' @export mdpi
#'
#' @aliases mdpi1
#' @aliases mdpi2
#'
#' @examples
#' # # Not run, uncomment to run
#' # # data from `MPI` package
#' # data <- mdpi1
#' # dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
#' #            d2 = c("Years.of.education", "School.attendance", "School.lag"),
#' #            d3 = c("Cooking.Fuel", "Access.to.clean.source.of.water",
#' #                   "Access.to.an.improve.sanatation", "Electricity",
#' #                   "Housing.Materials", "Asset.ownership"))
#' # mdpi(data, dm, plots = "t", Factor = "Region")
#' # mdpi(data, dm, plots = "t")
#' #
#' # # data from `mpitbR` package
#' # data <- mdpi2
#' # dm <- list(d1 = c("d_nutr","d_cm"),
#' #            d2 = c("d_satt","d_educ"),
#' #            d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))
#' # mdpi(data, dm, plots = "t", Factor = "region")
#' # mdpi(data, dm, plots = "t")
#'
#' @references
#' Alkire, S. & Foster, J. (2011). Counting and Multidimensional Poverty
#' Measurement. Journal of Public Economics 95(7-8): 476–87.
#' https://doi.org/10.1016/j.jpubeco.2010.11.006.
#'
#' Alkire, S., Foster, J. E., Seth, S., Santos, M. E., Roche, J., & Ballon, P.
#' (2015). Multidimensional poverty measurement and analysis. Oxford University
#'  Press.
#'
#' Alkire, S. & Santos, M. E. (2010).  Acute Multidimensional Poverty: A New
#'  Index for Developing Countries. Oxford Poverty and Human Development
#'  Initiative (OPHI) Working Paper No. 38.
#'
#' Alkire, S. & Santos, M. E. (2014). Measuring Acute Poverty in the Developing
#'  World: Robustness and Scope of the Multidimensional Poverty Index. World
#'  Development 59:251-274. https://doi.org/10.1016/j.worlddev.2014.01.026.
#'
#'  Siu Ming Chan & Hung Wong (2024): Measurement and determinants of
#'  multidimensional poverty: the case of Hong Kong, Journal of Asian Public
#'  Policy, DOI: 10.1080/17516234.2024.2325857
mdpi <- function(data, dm, Bar = 0.4,
                 id_addn = NULL,
                 Factor = NULL,
                 plots = NULL,
                 id = c("Health", "Education", "Living standard"),
                 id_add  = "Social security",
                 id_add1 = "Employment and Income",
                 Echo = TRUE) {

  Bar <-  Bar
  Factor <- Factor
  plots <- plots
  id <- id
  id_add <- id_add
  id_add1 <- id_add1
  id_addn <- id_addn
  ddm <- length(dm)
  k <- 1 / ddm
  Echo  <-  Echo

  if (ddm < 3L) {
    stop("Number of dimensions must be an integer not less than 3")
  } else if (ddm > 9L) {
    stop("Number of dimensions must be an integer not greater than 9")
  } else {
    cata <- "Number of dimensions correct, proceeding..."
    progaress(Echo, cata)
  }
  if (!is.null(id_addn)) {
    id0 <- c(id, id_add, id_add1, id_addn)
    cata <- "Additional dimension is evaluated..."
    progaress(Echo, cata)
  } else if (ddm == 5) {
    id0 <- c(id, id_add, id_add1)
    cat("Additional dimension is null...", "\n")
  } else if (ddm == 4) {
    id0 <- c(id, id_add)
    cata <- "Additional dimension is null..."
    progaress(Echo, cata)
  } else {
    id0 <- id
    cata <- "Additional dimension is null..."
    progaress(Echo, cata)
  }
  Analysis <- c("q", "Non Poor", "n", "Incidence of poverty",
                rep("Adjusted incidence of poverty", ddm + 1),
                rep("Deprivation Score", ddm + 1),
                rep("Intensity of poverty", ddm + 1),
                rep("Multidimensional poverty index", ddm + 1),
                rep("Contribution", ddm + 1),
                rep("Average deprivation among the deprived", ddm + 1))
  Order <- seq(1, length(Analysis), by = 1)
  cata <- "Computation commences..."
  progaress(Echo, cata)
  if (ddm == 3) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3))
    Scores <- 	dplyr::bind_cols(d1, d2, d3)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd))
  } else if (ddm == 4) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd))
  } else if (ddm == 5) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    d5 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d5))
    d5n <- NCOL(d5)
    d5 <- d5 * (k / d5n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4),
                               rowSums(d5))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4, d5)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4), rowMeans(d5))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd),
                            apply(d5, 1, sd))
  } else if (ddm == 6) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    d5 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d5))
    d5n <- NCOL(d5)
    d5 <- d5 * (k / d5n)
    d6 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d6))
    d6n <- NCOL(d6)
    d6 <- d6 * (k / d6n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4),
                               rowSums(d5), rowSums(d6))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4, d5, d6)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4), rowMeans(d5), rowMeans(d6))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd),
                            apply(d5, 1, sd), apply(d6, 1, sd))
  } else if (ddm == 7) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    d5 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d5))
    d5n <- NCOL(d5)
    d5 <- d5 * (k / d5n)
    d6 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d6))
    d6n <- NCOL(d6)
    d6 <- d6 * (k / d6n)
    d7 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d7))
    d7n <- NCOL(d7)
    d7 <- d7 * (k / d7n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4), rowSums(d5), rowSums(d6),
                               rowSums(d7))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4, d5, d6, d7)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4), rowMeans(d5), rowMeans(d6),
                              rowMeans(d7))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd),
                            apply(d5, 1, sd), apply(d6, 1, sd),
                            apply(d7, 1, sd))
  } else if (ddm == 8) {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    d5 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d5))
    d5n <- NCOL(d5)
    d5 <- d5 * (k / d5n)
    d6 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d6))
    d6n <- NCOL(d6)
    d6 <- d6 * (k / d6n)
    d7 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d7))
    d7n <- NCOL(d7)
    d7 <- d7 * (k / d7n)
    d8 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d8))
    d8n <- NCOL(d8)
    d8 <- d8 * (k / d8n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4), rowSums(d5), rowSums(d6),
                               rowSums(d7), rowSums(d8))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4, d5, d6, d7, d8)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4), rowMeans(d5), rowMeans(d6),
                              rowMeans(d7), rowMeans(d8))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd),
                            apply(d5, 1, sd), apply(d6, 1, sd),
                            apply(d7, 1, sd), apply(d8, 1, sd))
  } else {
    d1 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d1))
    d1n <- NCOL(d1)
    d1 <- d1 * (k / d1n)
    d2 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d2))
    d2n <- NCOL(d2)
    d2 <- d2 * (k / d2n)
    d3 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d3))
    d3n <- NCOL(d3)
    d3 <- d3 * (k / d3n)
    d4 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d4))
    d4n <- NCOL(d4)
    d4 <- d4 * (k / d4n)
    d5 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d5))
    d5n <- NCOL(d5)
    d5 <- d5 * (k / d5n)
    d6 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d6))
    d6n <- NCOL(d6)
    d6 <- d6 * (k / d6n)
    d7 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d7))
    d7n <- NCOL(d7)
    d7 <- d7 * (k / d7n)
    d8 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d8))
    d8n <- NCOL(d8)
    d8 <- d8 * (k / d8n)
    d9 <- data %>%
      dplyr::select(tidyselect::all_of(dm$d9))
    d9n <- NCOL(d9)
    d9 <- d9 * (k / d9n)
    score <- 	dplyr::bind_cols(rowSums(d1), rowSums(d2), rowSums(d3),
                               rowSums(d4), rowSums(d5), rowSums(d6),
                               rowSums(d7), rowSums(d8), rowSums(d9))
    Scores <- 	dplyr::bind_cols(d1, d2, d3, d4, d5, d6, d7, d8, d9)
    Mean <- 	dplyr::bind_cols(rowMeans(d1), rowMeans(d2), rowMeans(d3),
                              rowMeans(d4), rowMeans(d5), rowMeans(d6),
                              rowMeans(d7), rowMeans(d8), rowMeans(d9))
    SD <- 	dplyr::bind_cols(apply(d1, 1, sd), apply(d2, 1, sd),
                            apply(d3, 1, sd), apply(d4, 1, sd),
                            apply(d5, 1, sd), apply(d6, 1, sd),
                            apply(d7, 1, sd), apply(d8, 1, sd),
                            apply(d9, 1, sd))
  }
  names(score) <- names(Mean) <- names(SD) <- id0
  id1 <- c("Combined", id0)
  cata <- "The computation is progressing...1"
  progaress(Echo, cata)
  score <- data.frame(dplyr::bind_cols(Combined = rowSums(score), score))
  Mean <- data.frame(dplyr::bind_cols(Combined = rowMeans(score), Mean))
  SD <- data.frame(dplyr::bind_cols(Combined = apply(score, 1, sd), SD))
  id2  <-  "National"
  kay  <- data.frame(id2 = colSums(score))
  kay <- tibble::rownames_to_column(kay, var = "Dimension")
  id2  <-  "Mean"
  kay_mean <- data.frame(id2 = colSums(Mean))
  kay_mean <- tibble::rownames_to_column(kay_mean, var = "Dimension")
  id2  <-  "SD"
  kay_SD <- data.frame(id2 = apply(SD, 2, sd))
  kay_SD <- tibble::rownames_to_column(kay_SD, var = "Dimension")
  dds <- score
  score <- score %>%
    dplyr::mutate(Poverty = dplyr::case_when(Combined <= Bar ~ "Deprived",
                                             Combined > Bar ~ "Not deprived"))
  q <- nrow(score[score$Poverty == "Deprived", ])
  nq <- nrow(score) - q
  n <- q + nq
  cata <- "The computation is progressing...2"
  progaress(Echo, cata)
  id2  <-  "National"
  kay2 <- kkkk(q, nq, n, kay, id1, id2, ddm, Order, Analysis)
  cata <- "The computation is progressing...3"
  progaress(Echo, cata)
  id2  <-  "Mean"
  KaY2m <- kkkk(q, nq, n, kay = kay_mean, id1, id2, ddm, Order, Analysis)
  cata <- "The computation is progressing...4"
  progaress(Echo, cata)
  id2  <-  "SD"
  kaY2s <- kkkk(q, nq, n, kay = kay_SD, id1, id2, ddm, Order, Analysis)
  cata <- "The computation is progressing...5"
  progaress(Echo, cata)
  if (!is.null(Factor)) {
    modEls2m <- mmmm(data, Scores, score = Mean, Factor, ddm, Analysis,
                     kay2 = KaY2m)
    cata <- "The computation is progressing...6"
    progaress(Echo, cata)
    modEls2s <- mmmm(data, Scores, score = SD, Factor, ddm, Analysis,
                     kay2 = kaY2s)
    cata <- "The computation is progressing...7"
    progaress(Echo, cata)
    models2 <- mmmm(data, Scores, score, Factor, ddm, Analysis, kay2)
    if (!is.null(plots) & length(unique(Factor)) > 40) {
      cat("Palette have 40 colors, plots not possible...", "\n")
    } else if (!is.null(plots) & length(unique(Factor)) < 41) {
      kala <- kolo_mix("Renoir", 40, type = "continuous", direction = -1)
      plots <- plot_mdpi(models2, kala, ddm, factor = Factor)
      cata <- "Proceeding after plots produced..."
      progaress(Echo, cata)
    } else {
      cata <- "Proceeding without plots..."
      progaress(Echo, cata)
    }
    cata <- "The computation is progressing...8"
    progaress(Echo, cata)
    model_l <- list(MDPI_p = modelsummary::datasummary_df(models2, fmt = 4),
                    MDPI = models2,
                    national = cbind(kay2[, -1], Mean = KaY2m[, 4],
                                     SD = kaY2s[, 4]),
                    dimensions = dds,
                    Score = Scores,
                    `MDPI mean` = modEls2m,
                    `MDPI SD` = modEls2s,
                    plots = plots)
    cata <- "National and factor MDPI..."
    progaress(Echo, cata)
  } else {
    model_l <- list(national = cbind(kay2[, -1], Mean = KaY2m[, 4],
                                     SD = kaY2s[, 4]),
                    dimensions = dds,
                    Score = Scores,
                    plots = plots)
    cata <- "National MDPI only..."
    progaress(Echo, cata)
  }
  cata <- "The computation completed..."
  progaress(Echo, cata)
  return(model_l)
}
kkkk <- function(q, nq, n, kay, id1, id2, ddm, Order, Analysis) {
  kaye <- data.frame(rbind(q = q,
                           `Non Poor` = nq,
                           n = n,
                           IOP = q / n))
  names(kaye) <- id2
  kaye <- tibble::rownames_to_column(kaye, var = "Dimension")
  iopoo <- (kay[, 2]) / kaye[1, 2]
  iop0 <- data.frame(Dimension = id1, id2 = iopoo)
  adad <- data.frame(Dimension = id1, id2 = iop0[, 2] * ddm)
  MPI0 <- data.frame(Dimension = id1,
                     id2 = kay[, 2] / kaye[3, 2])
  Contribution <- data.frame(Dimension = id1,
                             id2 = MPI0[, 2] / MPI0[1, 2] * 100)
  ahcr <- data.frame(Dimension = id1, id2 = iop0[, 2] / ddm)
  names(kay) <- names(iop0) <- names(adad) <- names(MPI0) <-
    names(Contribution) <- names(ahcr) <- c("Dimension", id2)
  kay1 <- dplyr::bind_rows(kaye, ahcr, kay, iop0, MPI0, Contribution,
                           adad)
  kay2 <- dplyr::bind_cols(Order = Order, Analysis = Analysis, kay1)
  return(kay2)
}
mmmm <- function(data, Scores, score, Factor, ddm, Analysis, kay2) {
  IDn <- setdiff(names(data), names(Scores))
  fff <- data %>%
    dplyr::select(tidyselect::all_of(IDn))
  Factors <- data %>%
    dplyr::select(tidyselect::all_of(Factor))
  score1234 <- dplyr::bind_cols(score, fff, Scores)
  ssd <- score1234 %>%
    split(Factors) %>%
    purrr::map(\(df) NROW(df[df$Poverty, ]))
  ddfn <- as.data.frame(t(do.call(cbind, ssd)))
  dddm <- ddm + 1
  ssdc <- score1234 %>%
    split(Factors) %>%
    purrr::map(\(df) colSums(df[, 1:dddm]))
  ddfd <- as.data.frame(t(do.call(cbind, ssdc)))
  ssdn <- score1234 %>%
    split(Factors) %>%
    purrr::map(\(df) NROW(df[df$Poverty == "Deprived", ]))
  ddfq <- as.data.frame(t(do.call(cbind, ssdn)))
  ddfnq <- ddfn - ddfq
  IOP <- ddfq / ddfn
  model <- cbind(q = ddfq, Non_Poor = ddfnq, n = ddfn, IOP = IOP)
  names(model) <- c("q", "Non Poor", "n", "IOP")
  iop   <- ddfd / model$q
  MPIc  <- ddfd$Combined / model$n
  MPI   <- ddfd / model$n
  cont  <- MPI * MPIc * 100
  adad1 <- iop * ddm
  ahcr1 <- iop / ddm
  adad1 <- data.frame(t(adad1))
  adad1 <- tibble::rownames_to_column(adad1, var = "Dimension")
  ahcr1 <- data.frame(t(ahcr1))
  ahcr1 <- tibble::rownames_to_column(ahcr1, var = "Dimension")
  cont  <- data.frame(t(cont))
  cont  <- tibble::rownames_to_column(cont, var = "Dimension")
  model <- data.frame(t(model))
  model <- tibble::rownames_to_column(model, var = "Dimension")
  iop   <- data.frame(t(iop))
  iop   <- tibble::rownames_to_column(iop, var = "Dimension")
  MPI   <- data.frame(t(MPI))
  MPI   <- tibble::rownames_to_column(MPI, var = "Dimension")
  ddfd  <- data.frame(t(ddfd))
  ddfd  <- tibble::rownames_to_column(ddfd, var = "Dimension")
  models <- dplyr::bind_rows(model, ahcr1, ddfd, iop, MPI, cont, adad1)
  models2 <- dplyr::bind_cols(Analysis = Analysis, models,
                                National = kay2$National)
  return(models2)
}

kolapalette <- list(
  Renoir = list(c("#17154f", "#2f357c", "#6c5d9e", "#9d9cd5", "#b0799a",
                  "#f6b3b0", "#e48171", "#bf3729", "#e69b00", "#f5bb50",
                  "#ada43b", "#355828"), c(2, 5, 9, 12, 3, 8, 7, 10, 4, 1, 6,
                                           11), colorblind = FALSE))

kolo_mix <- function(palette_name, n, type = c("discrete", "continuous"),
                     direction = c(1, -1), override_order = FALSE,
                     return_hex = FALSE) {

  `%notin%` <- Negate(`%in%`)

  palette <- kolapalette[[palette_name]]

  if (is.null(palette)|is.numeric(palette_name)){
    stop("Palette does not exist.")
  }

  if (missing(n)) {
    n <- length(palette[[1]])
  }

  if (missing(direction)) {
    direction <- 1
  }

  if (direction %notin% c(1, -1)){
    stop("Direction not valid")
  }

  if (missing(type)) {
    if(n > length(palette[[1]])){type <- "continuous"}
    else{type <- "discrete"}
  }

  type <- match.arg(type)


  if (type == "discrete" && n > length(palette[[1]])) {
    stop("Number of requested colors greater than what discrete palette offer")
  }

  continuous <-  if(direction == 1) {
    grDevices::colorRampPalette(palette[[1]])(n)
  }else{
    grDevices::colorRampPalette(rev(palette[[1]]))(n)}

  discrete <- if(direction == 1 & override_order == FALSE){
    palette[[1]][which(palette[[2]] %in% c(1:n) == TRUE)]
  }else if(direction == -1 & override_order == FALSE){
    rev(palette[[1]][which(palette[[2]] %in% c(1:n) == TRUE)])
  } else if(direction == 1 & override_order == TRUE){
    palette[[1]][1:n]
  } else{
    rev(palette[[1]])[1:n]
  }

  out <- switch(type,
                continuous = continuous,
                discrete = discrete
  )
  if(return_hex == T) {print(out)}
  structure(out, class = "palette", name = palette_name)
}

progaress <- function(Echo, cata) {
  if (Echo == TRUE) {
    cat(cata, "\n")
  } else{
    cat("", "\n")
  }
}
