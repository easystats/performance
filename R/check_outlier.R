#' @title Check for influential observations
#' @name check_outlier
#'
#' @description Checks for and locates influential observations (i.e., "outliers") via Cook's Distance.
#'
#' @param x A model object.
#' @param print_outliers Optional argument to list individual outliers. Default set as \code{TRUE}.
#' @param drop_outliers Optional argument to drop outliers and return a data frame minus outliers. Default set as \code{FALSE}.
#' @param ... Currently not used.
#'
#' @return Check (message) on whether outliers were detected or not, as well as an optional
#' data frame minus outliers (i.e., \code{drop_outliers = TRUE}).
#'
#' @details Performs a Cook's distance test to check for influential observations. Those greater than 4/n, are considered
#' outliers. This relatively conservative threshold is useful only for detection, rather than justificaiton for automatic observation
#' deletion. If users opt to drop observations that may be problematic, they may do so by specifying \code{drop_outliers = TRUE}.
#'
#' @references Cook, R. D. (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18.
#'
#' @examples
#' mt1 <- mtcars[, c(1,3)] # select only mpg and disp (continuous)
#' oo <- data.frame(mpg = c(37, 40), disp = c(300, 400)) # create some fake outliers
#' mt2 <- rbind(mt1, oo) # attach outliers to main df
#' model <- lm(disp ~ mpg, data = mt2) # fit model with outliers
#' check_outlier(model, print_outliers = TRUE, drop_outliers = FALSE)
#'
#' @importFrom stats cooks.distance
#' @export
check_outlier <- function(x, ...) {
  UseMethod("check_outlier")
}

#' @rdname check_outlier
#' @export
check_outlier.default <- function(x, print_outliers = TRUE, drop_outliers = FALSE, ...) {
  data <- insight::get_data(x)
  n <- nrow(data)

  cook <- stats::cooks.distance(x)
  outliers <- length(as.numeric(names(cook)[(cook > (4 / n))]))

  if (outliers >= 1) {
    if (print_outliers) {
      o <- paste0(" (cases ", paste0(names(cook)[(cook > (4 / n))], collapse = ", "), ")")
    } else {
      o <- ""
    }
    insight::print_color(sprintf("Warning: Outliers detected%s.\n", o), 'red')
  } else {
    insight::print_color("OK: No outliers detected.\n", 'green')
  }

  if (drop_outliers == TRUE) {
    data_dropped <- data[!(cook > (4 / n)), ]
    return(data_dropped)
  }
}