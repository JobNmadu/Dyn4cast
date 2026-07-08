#' Plots of Multidimensional Index Measures
#'
#' @param data `Data frame` of Multidimensional Index measures which is an
#' object from `mdi`
#' @param kala color palette with at least 15 colors but must be equal or higher
#'  than the number of options in the factor argument
#' @param factor the optional grouping factor used in the computation measures.
#' If not supplied only the national plots will be produced irrespective of
#' whether the factor was used in the computation.
#' @param dma number of `Dimensions` involved in the computation of
#' Multidimensional Index measures.
#'
#' @returns A list of the following plots:
#' \item{\code{Multidimensional index}}{plot.}
#' \item{\code{Deprivation Score}}{plot.}
#' \item{\code{Adjusted incidence}}{plot.}
#' \item{\code{Intensity}}{plot.}
#' \item{\code{Average deprivation among the deprived}}{plot.}
#' \item{\code{Contribution of each Dimension}}{plot.}
#' \item{\code{combined dimensions}}{plot.}
#' \item{\code{national}}{plot.}
#' \item{\code{combined dimensions of national}}{plot.}
#'
#' @importFrom stats reorder
#'
#' @export
#'
#' @examples
#' # data from `mpitbR` package
#' data <- mdpi2
#' dm <- list(d1 = c("d_nutr","d_cm"),
#'            d2 = c("d_satt","d_educ"),
#'            d3 = c("d_elct","d_sani","d_wtr","d_hsg","d_ckfl","d_asst"))
#' dp <-  mdi(data, dm, plots = "t")
#' library(MetBrewer)
#' kala <- met.brewer("OKeeffe1", 20, type = "continuous")
#' dma <- 3
#' plot_mdi(dp$national, kala, dma)
plot_mdi <- function(data, kala, dma, factor = NULL) {

  if (lifecycle::is_present(plot_mdpi)) {
    lifecycle::deprecate_warn(
      when = "11.11.28",
      what   = "plot_mdpi()",
      with   = "plot_mdi()")
  }

  factor <- factor
  Analysis <-  Dimension <-  `Multidimensional index measure` <-
    State <- NULL
  idk <- data[6:(dma + 6 - 1), 2]
  data <- tidyr::pivot_longer(data, -c(1, 2), names_to = "State",
                              values_to = "Multidimensional index measure")
  D7 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% idk) %>%
    dplyr::filter(State %in% "National") %>%
    dplyr::filter(`Multidimensional index measure` > 0)

  L7 <- ggplot(D7) +
    ggplot2::aes(x = stats::reorder(Dimension,
                                    `Multidimensional index measure`),
                 y = `Multidimensional index measure`, fill = Dimension) +
    ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
    scale_fill_manual(values = kala) +
    labs(y = "Multidimensional index measure", x = "Dimensions") +
    ggplot2::theme_minimal() +
    guides(color = ggplot2::guide_none()) +
    theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_flip() +
    facet_wrap(vars(Analysis), scales = "free")

  D9 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% "Combined") %>%
    dplyr::filter((State %in% "National")) %>%
    dplyr::filter(`Multidimensional index measure` > 0)

  L9 <- ggplot(D9) +
    ggplot2::aes(x = Analysis, y = `Multidimensional index measure`,
                 fill = Analysis) +
    ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
    scale_fill_manual(values = kala) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    facet_wrap(vars(Analysis), scales = "free")
  if (!is.null(factor)) {
    D1 <- data %>%
      dplyr::filter(Analysis %in% "Multidimensional index") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L1 <- ggplot(D1) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Multidimensional index",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D2 <- data %>%
      dplyr::filter(Analysis %in% "Deprivation Score") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L2 <- ggplot(D2) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Deprivation Score",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D3 <- data %>%
      dplyr::filter(Analysis %in% "Adjusted incidence") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L3 <- ggplot(D3) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Adjusted incidence",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D4 <- data %>%
      dplyr::filter(Analysis %in% "Intensity") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L4 <- ggplot(D4) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Intensity",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D5 <- data %>%
      dplyr::filter(Analysis %in% "Average deprivation among the deprived") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L5 <- ggplot(D5) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Average deprivation among the deprived",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D6 <- data %>%
      dplyr::filter(Analysis %in% "Contribution") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L6 <- ggplot(D6) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional index measure`),
                   y = `Multidimensional index measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Contribution of each Dimension",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D8 <-  data %>%
      dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
      dplyr::filter(Dimension %in% "Combined") %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional index measure` > 0)

    L8 <- ggplot(D8) +
      ggplot2::aes(x = reorder(State,
                               `Multidimensional index measure`),
                   y = round(`Multidimensional index measure`, 3),
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      labs(y = "Multidimensional index measure", x = "Factor options") +
      ggplot2::theme_minimal() +
      guides(color = ggplot2::guide_none()) +
      theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Analysis), scales = "free")
    return(list(`Multidimensional index` = L1,
                `Deprivation Score` = L2,
                `Adjusted incidence` = L3,
                `Intensity` = L4,
                `Average deprivation among the deprived` = L5,
                `Contribution of each Dimension` = L6,
                combined_only = L8,
                national_only = L7,
                combined_national_only = L9))
  } else {
    return(list(national_only = L7,
                combined_national_only = L9))
  }
}
