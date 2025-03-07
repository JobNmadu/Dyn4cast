#' Plots of Multidimensional Poverty Measures
#'
#' @param data `Data frame` of Multidimensional Poverty measures from `mdpi`
#' @param kala color palette with at least 15 colors but must be equal or higher
#'  than the number of options in the factor argument
#' @param factor the optional grouping factor used in the computation measures.
#' If not supplied only the national plots will be produced irrespective of
#' whether the factor was used in the computation.
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
#' # plot_mdpi(dp$MDPI, kala, "Region")
plot_mdpi <- function(data, kala, factor = NULL) {
  factor <- factor
  Analysis <-  Dimension <-  `Multidimensional poverty measure` <-
    State <- NULL
  data <- tidyr::pivot_longer(data, -c(1, 2), names_to = "State",
                       values_to = "Multidimensional poverty measure")
  L7 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% c("Education", "Employment.and.Income",
                            "Health", "Living.standard", "Social.security")) %>%
    dplyr::filter(State %in% "National") %>%
    ggplot() +
    ggplot2::aes(x = stats::reorder(Dimension,
                                    `Multidimensional poverty measure`),
        y = `Multidimensional poverty measure`, fill = Dimension) +
    ggplot2::geom_bar(stat = "summary", fun = "mean") +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(x = "Dimensions") +
    ggplot2::coord_flip() +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    facet_wrap(vars(Analysis), scales = "free")

  L9 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% "Combined") %>%
    dplyr::filter((State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Analysis, y = `Multidimensional poverty measure`,
                 fill = Analysis) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = "none")+
    facet_wrap(vars(Analysis), scales = "free")
  if(!is.null(factor)){
   L1 <- data %>%
    dplyr::filter(Analysis %in% "Multidimensional poverty index") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
     ggplot2::geom_col() +
     ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Multidimensional poverty index",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  L2 <- data %>%
    dplyr::filter(Analysis %in% "Deprivation Score") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Deprivation Score",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  L3 <- data %>%
    dplyr::filter(Analysis %in% "Adjusted incidence of poverty") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Adjusted incidence of poverty",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  L4 <- data %>%
    dplyr::filter(Analysis %in% "Intensity of poverty") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Intensity of poverty",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  L5 <- data %>%
    dplyr::filter(Analysis %in% "Average deprivation among the deprived") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Average deprivation among the deprived",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

  L6 <- data %>%
    dplyr::filter(Analysis %in% "Contribution") %>%
    dplyr::filter(Dimension %in% c("Education",
                            "Employment.and.Income", "Health",
                            "Living.standard", "Social.security")) %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = Dimension, y = `Multidimensional poverty measure`,
                 fill = State) +
    ggplot2::geom_col() +
    ggplot2::scale_fill_manual(values = kala) +
    ggplot2::labs(y = "Contribution of each Dimension",
         fill = factor) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")

 L8 <- data %>%
    dplyr::filter(!(Analysis %in% c("q", "Non Poor", "n"))) %>%
    dplyr::filter(Dimension %in% "Combined") %>%
    dplyr::filter(!(State %in% "National")) %>%
    ggplot() +
    ggplot2::aes(x = stats::reorder(Analysis,
                                    `Multidimensional poverty measure`),
        y = `Multidimensional poverty measure`,
        fill = State) +
   ggplot2::labs(x = "Analysis", fill = factor) +
   ggplot2::geom_col(position = "dodge") +
   ggplot2::scale_fill_manual(values = kala) +
   ggplot2::theme_minimal()+
   ggplot2::theme(legend.position = "bottom")+
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
