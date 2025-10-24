
#' Convert Likert Data to Relative Scores and knowledge-based **Adaptive Capacity**
#'
#' @description
#' Adaptive capacity refers to the ability of systems—biological, social, or
#' institutional—to adjust to environmental changes, capitalize on emerging
#' opportunities, and mitigate potential threats in order to preserve essential
#'  functions. In the context of climate change, adaptive capacity denotes the
#'   competence of social-ecological systems to cope with present variability
#'   and prepare for uncertain future conditions.
#'
#' From a machine learning perspective, adaptive capacity is closely linked to
#' the system’s ability to process large-scale, heterogeneous data sources,
#' identify patterns, and support the development of predictive models and
#' adaptive strategies. Key components of adaptive capacity include access to
#' relevant and reliable information, computational infrastructure, financial
#' and human capital, and strong social and institutional networks.
#'
#' Machine learning can enhance adaptive capacity by enabling dynamic learning
#'  from historical and real-time data, improving climate risk assessments, and
#'   optimizing adaptation strategies. Moreover, the iterative nature of model
#'   refinement and feedback integration mirrors the learning processes
#'   inherent in adaptive systems. Thus, adaptive capacity in this context
#'   involves not only the ability to design and implement effective
#'   interventions but also to learn from outcomes and continuously update
#'    strategies in light of new data and evolving conditions.
#'
#' This function is for knowledge-based **Adaptive Capacity**. The indices from
#'  the various knowledge areas like Awareness, availability, affordability,
#'  	accessibility, benefits, adequacy, usage, effectiveness, etc can be
#'  	obtained individually and converted to adaptive capacity. Adaptive
#'  	capacity based on financial and human capital, strong social and
#'  	institutional networks can be obtained from `model_factors()`.
#'
#' @param data Data frame of likert data either in **text** or **scores**.
#' @param Likert Vector of likert-type factors in **descending order** as in the
#'  data frame which must be given if the data frame is in **text**.
#' @param Option Optional vector indicating whether the data frame is in text
#' or scores format. Defaults to **text** if not given.
#' @param Ranks Optional vector of number of levels which is required if the
#' data frame is in scores rather than text. There are only four choices
#'  i.e. 3, 5, 7, 9.
#'
#' @returns A list with the following components:
#' \item{\code{lik_num}}{`dataframe` of likert scores}
#' \item{\code{lik_rate}}{`dataframe` of relative likert scores}
#' \item{\code{lik_sum}}{`dataframe` summary of relative scores based on factors.}
#' \item{\code{lik_col}}{A vector of indices for **Adaptive Capacity**.}
#'
#' @export relative_likert
#'
#' @examples
#' library(readr)
#' garrett_data <- data.frame(garrett_data)
#' relative_likert(garrett_data, Ranks = 3, Option = "sccore")
#' relative_likert(garrett_data, Ranks = 5, Option = "sccore")
#' relative_likert(garrett_data, Ranks = 7, Option = "sccore")
#' relative_likert(garrett_data, Ranks = 9, Option = "sccore")
#'
#' relative_likert(Quicksummary, Ranks = 5, Option = "sccore")
#'
#' library(tidyverse)
#' data_l <- garrett_data %>%
#' pivot_longer(cols = everything()) %>%
#'  mutate(value = case_when(value == 5 ~ "Serious constraint",
#'                           value == 4 ~ "Constraint",
#'                           value == 3 ~ "Not certain it is a constraint",
#'                           value == 2 ~ "Not a constraint",
#'                           value == 1 ~ "Not a serious constraint",
#'                           .default = "None")) %>%
#'  group_by(name) %>%
#'  mutate(row = row_number()) %>%
#'  pivot_wider(names_from = name, values_from = value) %>%
#'  select(-row) %>%
#'  unnest(cols = everything())
#'
#'  ranking <- c("Serious constraint", "Constraint",
#' "Not certain it is a constraint", "Not a constraint",
#' "Not a serious constraint")
#'
#'  relative_likert(data_l, Likert = ranking)
relative_likert <- function(data, Likert = NULL, Ranks = NULL, Option = "text") {

  Likert = Likert
  Ranks = Ranks

  if (is.null(Likert) & is.null(Ranks)) {
    stop("**Likert** and **Ranks** arguments cannot be **NULL** at the same time")
  } else if (is.null(Likert) & Option == "text"){
    stop("**Likert** argument must be provided to progress")
  } else if(!is.null(Ranks) & Option == "text"){
    stop("**Option** argument must be appropriate")
  }else if (!is.null(Likert)) {
    lll <- length(Likert)
  } else {
    lll <- Ranks
  }

  cat("Preliminary check success, proceeding...", "\n")

  if (lll < 3L | lll > 9L | lll %% 2 != 1L) {
    stop("Likert lenght out of range, try again")
  } else {
    cat("Likert lenght check success, proceeding...", "\n")
  }

  Option <- Option
  ss <- 1/lll
  bbb <- rev(seq(from = ss, to = 1, by = ss))
  ab <- rev(1:lll)

  num_item <- ncol(data)

  if(Option != "text") {

    dddd <- data

  } else{
    if(lll == 9) {
      dddd <- data %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::mutate(value = dplyr::case_when(value == Likert[1] ~ ab[9],
                                 value == Likert[2] ~ ab[8],
                                 value == Likert[3] ~ ab[7],
                                 value == Likert[4] ~ ab[6],
                                 value == Likert[5] ~ ab[5],
                                 value == Likert[6] ~ ab[4],
                                 value == Likert[7] ~ ab[3],
                                 value == Likert[8] ~ ab[2],
                                 value == Likert[9] ~ ab[1],
                                 .default = 0))%>%
        dplyr::group_by(name) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::select(-row) %>%
        tidyr::unnest(cols = tidyselect::everything())

    } else if (lll == 7) {
      dddd <- data %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::mutate(value = dplyr::case_when(value == Likert[1] ~ ab[7],
                                 value == Likert[2] ~ ab[6],
                                 value == Likert[3] ~ ab[5],
                                 value == Likert[4] ~ ab[4],
                                 value == Likert[5] ~ ab[3],
                                 value == Likert[6] ~ ab[2],
                                 value == Likert[7] ~ ab[1],
                                 .default = 0))%>%
        dplyr::group_by(name) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::select(-row) %>%
        tidyr::unnest(cols = tidyselect::everything())

    } else if (lll == 5) {
      dddd <- data %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::mutate(value = dplyr::case_when(value == Likert[1] ~ ab[5],
                                 value == Likert[2] ~ ab[4],
                                 value == Likert[3] ~ ab[3],
                                 value == Likert[4] ~ ab[2],
                                 value == Likert[5] ~ ab[1],
                                 .default = 0))%>%
        dplyr::group_by(name) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::select(-row) %>%
        tidyr::unnest(cols = tidyselect::everything())

    } else {
      dddd <- data %>%
        tidyr::pivot_longer(cols = tidyselect::everything()) %>%
        dplyr::mutate(value = dplyr::case_when(value == Likert[1] ~ ab[3],
                                 value == Likert[2] ~ ab[2],
                                 value == Likert[3] ~ ab[1],
                                 .default = 0))%>%
        dplyr::group_by(name) %>%
        dplyr::mutate(row = dplyr::row_number()) %>%
        tidyr::pivot_wider(names_from = name, values_from = value) %>%
        dplyr::select(-row) %>%
        tidyr::unnest(cols = tidyselect::everything())
    }
  }

  dddd[is.na(dddd)] = 0

  if(lll == 9){
    rrrr <- dddd %>%
      tidyr::pivot_longer(cols = tidyselect::everything()) %>%
      dplyr::mutate(value = dplyr::case_when(value == 9 ~ bbb[1],
                               value == 8 ~ bbb[2],
                               value == 7 ~ bbb[3],
                               value == 6 ~ bbb[4],
                               value == 5 ~ bbb[5],
                               value == 4 ~ bbb[6],
                               value == 3 ~ bbb[7],
                               value == 2 ~ bbb[8],
                               value == 1 ~ bbb[9],
                               value == 0 ~ 0.0,
                               .default = 0)) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::select(-row) %>%
      tidyr::unnest(cols = tidyselect::everything())

  } else if (lll == 7) {
    rrrr <- dddd %>%
      tidyr::pivot_longer(cols = tidyselect::everything()) %>%
      dplyr::mutate(value = dplyr::case_when(value == 7 ~ bbb[1],
                               value == 6 ~ bbb[2],
                               value == 5 ~ bbb[3],
                               value == 4 ~ bbb[4],
                               value == 3 ~ bbb[5],
                               value == 2 ~ bbb[6],
                               value == 1 ~ bbb[7],
                               value == 0 ~ 0.0,
                               .default = 0)) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::select(-row) %>%
      tidyr::unnest(cols = tidyselect::everything())

  } else if (lll == 5) {
    rrrr <- dddd %>%
      tidyr::pivot_longer(cols = tidyselect::everything()) %>%
      dplyr::mutate(value = dplyr::case_when(value == 5 ~ bbb[1],
                               value == 4 ~ bbb[2],
                               value == 3 ~ bbb[3],
                               value == 2 ~ bbb[4],
                               value == 1 ~ bbb[5],
                               value == 0 ~ 0.0,
                               .default = 0)) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::select(-row) %>%
      tidyr::unnest(cols = tidyselect::everything())

  } else {
    rrrr <- dddd %>%
      tidyr::pivot_longer(cols = tidyselect::everything()) %>%
      dplyr::mutate(value = dplyr::case_when(value == 3 ~ bbb[1],
                                             value == 2 ~ bbb[2],
                                             value == 1 ~ bbb[3],
                                             value == 0 ~ 0.0,
                                             .default = 0)) %>%
      dplyr::group_by(name) %>%
      dplyr::mutate(row = dplyr::row_number()) %>%
      tidyr::pivot_wider(names_from = name, values_from = value) %>%
      dplyr::select(-row) %>%
      tidyr::unnest(cols = tidyselect::everything())
  }

  ssss <- quicksummary(rrrr, 1)$Summary

  cat("Success, DONE!", "\n")

  return(data <- list(lik_num = dddd, lik_rate = rrrr, lik_sum = ssss,
               lik_col = rowSums(rrrr)/num_item))
}
