#' Model summary for k-means clustering
#'
#' @param model Object of type `kmeans`.
#' @inheritParams model_performance.lm
#'
#' @examples
#' # a 2-dimensional example
#' x <- rbind(
#'   matrix(rnorm(100, sd = 0.3), ncol = 2),
#'   matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2)
#' )
#' colnames(x) <- c("x", "y")
#' model <- kmeans(x, 2)
#' model_performance(model)
#' @export

model_performance.kmeans <- function(model, verbose = TRUE, ...) {
  out <- as.data.frame(model[c("totss", "tot.withinss", "betweenss", "iter")])
  colnames(out) <- c(
    "Sum_Squares_Within", "Sum_Squares_Between", "Sum_Squares_Total",
    "Iterations"
  )

  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  out
}
