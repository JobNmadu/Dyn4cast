#' Plots of Multidimensional Poverty Measures
#'
#' @param data `Data frame` of Multidimensional Poverty measures which is an
#' object from `mdpi`
#' @param kala color palette with at least 15 colors but must be equal or higher
#'  than the number of options in the factor argument
#' @param factor the optional grouping factor used in the computation measures.
#' If not supplied only the national plots will be produced irrespective of
#' whether the factor was used in the computation.
#' @param dma number of `Dimensions` involved in the computation of
#' Multidimensional Poverty measures.
#'
#' @returns A list of the following plots:
#' \item{\code{Multidimensional poverty index}}{plot.}
#' \item{\code{Deprivation Score}}{plot.}
#' \item{\code{Adjusted incidence of poverty}}{plot.}
#' \item{\code{Intensity of poverty}}{plot.}
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
#' # Not run, uncomment to run
#' # library(MPI)
#' # data("examplePovertydf")
#' # data <- examplePovertydf
#' # dm <- list(d1 = c("Child.Mortality", "Access.to.health.care"),
#' #            d2 = c("Years.of.education", "School.attendance", "School.lag"),
#' #            d3 = c("Cooking.Fuel", "Access.to.clean.source.of.water",
#' #                   "Access.to.an.improve.sanatation", "Electricity",
#' #                   "Housing.Materials", "Asset.ownership"))
#' # dp <- mdpi(data, dm, Factor = "Region")
#' # library(MetBrewer)
#' # kala <- met.brewer("OKeeffe1", 15, type = "continuous")
#' # dma <- 3
#' # plot_mdpi(dp$MDPI, kala, dma, "Region")
plot_mdpi <- function(data, kala, dma, factor = NULL) {
  factor <- factor
  Analysis <-  Dimension <-  `Multidimensional poverty measure` <-
    State <- NULL
  idk <- data[6:(dma + 6 - 1), 2]
  data <- tidyr::pivot_longer(data, -c(1, 2), names_to = "State",
                              values_to = "Multidimensional poverty measure")
  D7 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% idk) %>%
    dplyr::filter(State %in% "National") %>%
    dplyr::filter(`Multidimensional poverty measure` > 0)

  L7 <- ggplot(D7) +
    ggplot2::aes(x = stats::reorder(Dimension,
                                    `Multidimensional poverty measure`),
                 y = `Multidimensional poverty measure`, fill = Dimension) +
    ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
    scale_fill_manual(values = kala) +
    labs(y = "Multidimensional poverty measure", x = "Dimensions") +
    ggplot2::theme_minimal() +
    guides(color = ggplot2::guide_none())+
    theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::coord_flip() +
    facet_wrap(vars(Analysis), scales = "free")

  D9 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% "Combined") %>%
    dplyr::filter((State %in% "National")) %>%
    dplyr::filter(`Multidimensional poverty measure` > 0)

  L9 <- ggplot(D9) +
    ggplot2::aes(x = Analysis, y = `Multidimensional poverty measure`,
                 fill = Analysis) +
    ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
    scale_fill_manual(values = kala) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    facet_wrap(vars(Analysis), scales = "free")
  if(!is.null(factor)){
    D1 <- data %>%
      dplyr::filter(Analysis %in% "Multidimensional poverty index") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L1 <- ggplot(D1) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Multidimensional poverty index",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D2 <- data %>%
      dplyr::filter(Analysis %in% "Deprivation Score") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L2 <- ggplot(D2) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
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
      dplyr::filter(Analysis %in% "Adjusted incidence of poverty") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L3 <- ggplot(D3) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Adjusted incidence of poverty",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

   D4 <- data %>%
      dplyr::filter(Analysis %in% "Intensity of poverty") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L4 <- ggplot(D4) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
                   fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      ggplot2::labs(y = "Intensity of poverty",
                    x = "Factor options") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Dimension), scales = "free")

    D5 <-data %>%
      dplyr::filter(Analysis %in% "Average deprivation among the deprived") %>%
      dplyr::filter(Dimension %in% idk) %>%
      dplyr::filter(!(State %in% "National")) %>%
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L5 <- ggplot(D5) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
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
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L6 <- ggplot(D6) +
      ggplot2::aes(x = stats::reorder(State,
                                      `Multidimensional poverty measure`),
                   y = `Multidimensional poverty measure`,
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
      dplyr::filter(`Multidimensional poverty measure` > 0)

    L8 <- ggplot(D8) +
      ggplot2::aes(x = reorder(State,
                               `Multidimensional poverty measure`),
          y = round(`Multidimensional poverty measure`, 3),
          fill = State) +
      ggplot2::geom_bar(stat = "summary", fun = "mean", position = "dodge2") +
      scale_fill_manual(values = kala) +
      labs(y = "Multidimensional poverty measure", x = "Factor options") +
      ggplot2::theme_minimal() +
      guides(color = ggplot2::guide_none())+
      theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggplot2::coord_flip() +
      facet_wrap(vars(Analysis), scales = "free")
    return(list(`Multidimensional poverty index` = L1,
             `Deprivation Score` = L2,
             `Adjusted incidence of poverty` = L3,
             `Intensity of poverty` = L4,
             `Average deprivation among the deprived` = L5,
             `Contribution of each Dimension` = L6,
             combined_only = L8,
             national_only = L7,
             combined_national_only = L9))
  }else{
    return(list(national_only = L7,
                combined_national_only = L9))
  }
}
