#' @title Check for influential observations
#' @name check_outliers
#'
#' @description Checks for and locates influential observations (i.e., "outliers") via Cook's Distance.
#'
#' @param x A model object.
#' @param threshold The threshold indicating at which distance an observation is
#'   considered as outlier. For the Cook's Distance method, \code{threshold} defaults
#'   to 4 divided by numbers of observations.
#' @param ... Currently not used.
#'
#' @return Check (message) on whether outliers were detected or not, as well as a
#' data frame (with the original data that was used to fit the model), including
#' information on the distance measure and whether or not an observation is considered
#' as outlier.
#'
#' @details Performs a Cook's distance test to check for influential observations. Those greater than 4/n, are considered
#' outliers. This relatively conservative threshold is useful only for detection, rather than justificaiton for automatic observation
#' deletion. If users opt to drop observations that may be problematic, they may do so by specifying \code{drop_outliers = TRUE}.
#'
#' @references Cook, R. D. (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18.
#'
#' @examples
#' # select only mpg and disp (continuous)
#' mt1 <- mtcars[, c(1,3)]
#' # create some fake outliers and attach outliers to main df
#' mt2 <- rbind(mt1, data.frame(mpg = c(37, 40), disp = c(300, 400)))
#' # fit model with outliers
#' model <- lm(disp ~ mpg, data = mt2)
#'
#' check_outliers(model)
#'
#' @importFrom insight n_obs
#' @importFrom stats cooks.distance
#' @export
check_outliers <- function(x, ...) {
  UseMethod("check_outliers")
}

#' @rdname check_outliers
#' @export
check_outliers.default <- function(x, threshold = 4 / insight::n_obs(x), ...) {
  dat <- insight::get_data(x)

  cook <- tryCatch(
    {
      unname(stats::cooks.distance(x))
    },
    error = function(e) { NULL }
  )

  if (is.null(cook)) {
    insight::print_color(sprintf("Objects of class \"%s\" are not supported yet.\n", class(x)[1]), "red")
    return(NULL)
  }

  dat[[".id"]] <- 1:nrow(dat)
  dat[[".outliers"]] <- cook > threshold
  dat[[".distance"]] <- cook

  class(dat) <- c("check_outliers", "see_check_outliers", "data.frame")
  attr(dat, "threshold") <- threshold
  dat
}
