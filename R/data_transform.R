#' Standardize `data.frame` for comparable **Machine Learning** prediction and visualization
#'
#' Often economic and other **Machine Learning** data are of different units or sizes making either estimation, interpretation or visualization difficult. The solution to these issues can be handled if the data can be transformed into _unitless_ or data of similar magnitude. This is what `data_transform` is set to do. It is simple and straight forward to use.
#'
#' @param data A `data.frame` with numeric data for transformation. All columns in the data are transformed
#' @param method The type of transformation. There three options. `1` is for `log` transformation, `2` is for `min-max` transformation and `3` is for `mean-SD` transformation.
#' @param MARGIN Option to either transform the data `2 == column-wise` or `1 == row-wise`. Defaults to `column-wise` transformation if no option is indicated.
#'
#' @importFrom stats sd
#'
#' @return This function returns the output of the data transformation process as
#' \item{\code{tata_transformed}}{ A new `data.frame` containing the transformed values}
#'
#' @export data_transform
#'
#' @name data_transform
#'
#' @aliases Transform
#'
#' @examples
#'
#' library(Dyn4cast)
#' # View the data without transformation
#'
#' data0 <- Transform %>%
#' pivot_longer(!X, names_to = "Factors", values_to = "Data")
#'
#' ggplot(data = data0, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#'   geom_line() +
#'   scale_fill_brewer(palette = "Set1") +
#'   scale_color_brewer(palette = "Set1") +
#'   labs(y = "Data", x = "Series", color = "Factors") +
#'   theme_bw(base_size = 12)
#'
#' # Example 1: Transformation by min-max method.
#' # You could also transform the `X column` but is is better not to.
#'
#' data1 <- data_transform(Transform[, -1], 1)
#' data1 <- cbind(Transform[, 1], data1)
#' data1 <- data1 %>%
#'   pivot_longer(!X, names_to = "Factors", values_to = "Data")
#'
#' ggplot(data = data1, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#'   geom_line() +
#'   scale_fill_brewer(palette = "Set1") +
#'   scale_color_brewer(palette = "Set1") +
#'   labs(y = "Data", x = "Series", color = "Factors") +
#'   theme_bw(base_size = 12)
#'
#' # Example 2: `log` transformation
#'
#' data2 <- data_transform(Transform[, -1], 2)
#' data2 <- cbind(Transform[, 1], data2)
#' data2 <- data2 %>%
#'   pivot_longer(!X, names_to = "Factors", values_to = "Data")
#'
#' ggplot(data = data2, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#'   geom_line() +
#'   scale_fill_brewer(palette = "Set1") +
#'   scale_color_brewer(palette = "Set1") +
#'   labs(y = "Data", x = "Series", color = "Factors") +
#'   theme_bw(base_size = 12)
#'
#' # Example 3: `Mean-SD` transformation
#'
#' data3 <- data_transform(Transform[, -1], 3)
#' data3 <- cbind(Transform[, 1], data3)
#' data3 <- data3 %>%
#'   pivot_longer(!X, names_to = "Factors", values_to = "Data")
#'
#' ggplot(data = data3, aes(x = X, y = Data, fill = Factors, color = Factors)) +
#'   geom_line() +
#'   scale_fill_brewer(palette = "Set1") +
#'   scale_color_brewer(palette = "Set1") +
#'   labs(y = "Data", x = "Series", color = "Factors") +
#'   theme_bw(base_size = 12)
#'
data_transform <- function(data, method, MARGIN = 2){

  if(method == 1){

    data_transformed <-  apply(data, MARGIN = MARGIN, FUN = function(x) (x-min(x))/(max(x)-min(x)))

    return (data_transformed)

  }

  else if(method == 2){

    data_transformed <- log(as.data.frame(data))

    return (data_transformed)
  }

  else{

    data_transformed <- apply(data, MARGIN = MARGIN, FUN = function(x) (x-mean(x))/sd(x))

    return (data_transformed)

    }
  }
