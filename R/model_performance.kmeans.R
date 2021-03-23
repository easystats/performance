#' Model summary for k-means clustering
#'
#' @param model Object of type \code{kmeans}.
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
    "Within_Sum_Squares", "Between_Sum_Squares", "Total_Sum_Squares",
    "Iterations"
  )

  row.names(out) <- NULL
  class(out) <- c("performance_model", class(out))

  out
}
