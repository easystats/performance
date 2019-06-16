#' @title Check for influential observations
#' @name check_outliers
#'
#' @description Checks for and locates influential observations (i.e., "outliers") via several distance methods.
#'
#' @param x A model object.
#' @param threshold The threshold indicating at which distance an observation is
#'   considered as outlier. Possible values are \code{"cook"} (Cook's Distance),
#'   \code{"mahalanobis"} (Mahalanobis Distance) or \code{"ics"} (Invariant
#'   Coordinate Selection, \cite{Archimbaud et al. 2018}). May be abbreviated.
#'   See 'Details'.
#' @param method The method to calculate the distance, at which value a point is
#'   considered as "outlier".
#' @param ... When \code{method = "ics"}, further arguments in \code{...} are
#'   passed down to \code{ICSOutlier::ics.outlier()}.
#'
#' @return Check (message) on whether outliers were detected or not, as well as a
#' data frame (with the original data that was used to fit the model), including
#' information on the distance measure and whether or not an observation is considered
#' as outlier.
#'
#' @details Performs a distance test to check for influential observations. Those
#' greater as a certain threshold, are considered outliers. This relatively conservative
#' threshold is useful only for detection, rather than justificaiton for automatic
#' observation deletion.
#' \subsection{Choosing Default Thresholds}{
#' \describe{
#' \item{\strong{Cook's Distance}}{
#' When \code{method = "cook"}, \code{threshold} defaults to 4 divided by numbers of observations.
#' }
#' \item{\strong{Mahalanobis Distance}}{
#' When \code{method = "mahalanobis"}, the default for \code{threshold} is based on
#' a weird formula (\code{floor(3 * sqrt(sum(cov(predictors)^2)) / nobs(x))}),
#' which is limted to values between 3 and 10, to account for different variation
#' in the data depending on the number of observations. There is no "rule of thumb"
#' for the threshold regarding the Mahalanobis Distance, most studies use a value
#' between 3 and 10. It is most likely better to define own, sensible thresholds.
#' }
#' \item{\strong{Invariant Coordinate Selection}}{
#' If \code{method = "ics"}, the threshold is determined by \code{ICSOutlier::ics.outlier()}.
#' Refer to the help-file of that function to get more details about this procedure.
#' Note that \code{method = "ics"} requires both \pkg{ICS} and \pkg{ICSOutlier}
#' to be installed, and that it takes a bit longer to compute the results.
#' }
#' }
#' }
#'
#' @references \itemize{
#' \item Cook, R. D. (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18.
#' \item Archimbaud, A., Nordhausen, K., & Ruiz-Gazen, A. (2018). ICS for multivariate outlier detection with application to quality control. Computational Statistics & Data Analysis, 128, 184–199. \doi{10.1016/j.csda.2018.06.011}
#' }
#'
#' @examples
#' # select only mpg and disp (continuous)
#' mt1 <- mtcars[, c(1, 3, 4)]
#' # create some fake outliers and attach outliers to main df
#' mt2 <- rbind(mt1, data.frame(mpg = c(37, 40), disp = c(300, 400), hp = c(110, 120)))
#' # fit model with outliers
#' model <- lm(disp ~ mpg + hp, data = mt2)
#'
#' check_outliers(model)
#' plot(check_outliers(model))
#'
#' check_outliers(model, method = "m")
#'
#' \dontrun{
#' # This one takes some seconds to finish...
#' check_outliers(model, method = "ics")}
#'
#' @importFrom insight n_obs get_predictors get_data
#' @importFrom stats cooks.distance mahalanobis cov
#' @export
check_outliers <- function(x, ...) {
  UseMethod("check_outliers")
}

#' @rdname check_outliers
#' @export
check_outliers.default <- function(x, method = c("cook", "mahalanobis", "ics"), threshold = NULL, ...) {
  method <- match.arg(method)
  dat <- insight::get_data(x)
  preds <- insight::get_predictors(x)
  preds <- preds[, sapply(preds, is.numeric), drop = FALSE]

  if (is.null(threshold)) {
    threshold <- switch(
      method,
      "cook" = 4 / insight::n_obs(x),
      "mahalanobis" = floor(3 * sqrt(sum(stats::cov(preds)^2)) / insight::n_obs(x)),
      "ics" = NULL
    )
  }


  dist <- tryCatch(
    {
      if (method == "cook") {
        unname(stats::cooks.distance(x))
      } else if (method == "mahalanobis") {
        threshold <- ifelse(threshold < 3, 3, ifelse(threshold > 10, 10, threshold))
        stats::mahalanobis(preds, center = colMeans(preds), cov = stats::cov(preds))
      } else if (method == "ics") {
        if (!requireNamespace("ICS", quietly = TRUE)) {
          stop("Package `ICS` needed for this function to work. Please install it.", call. = FALSE)
        }
        if (!requireNamespace("ICSOutlier", quietly = TRUE)) {
          stop("Package `ICSOutlier` needed for this function to work. Please install it.", call. = FALSE)
        }
        ncores <- if (!requireNamespace("parallel", quietly = TRUE))
          NULL
        else
          parallel::detectCores()
        ics <- ICS::ics2(preds)
        outliers <- ICSOutlier::ics.outlier(object = ics, ncores = ncores, ...)
        threshold <- outliers@ics.dist.cutoff
        outliers@ics.distances
      } else {
        NULL
      }
    },
    error = function(e) { NULL }
  )


  if (is.null(dist)) {
    if (method == "ics") {
      if (ncol(preds) == 1)
        insight::print_color("At least two numeric predictors are required to detect outliers.\n", "red")
      else
        insight::print_color(sprintf("'check_outliers()' does not support models of class '%s'.\n", class(x)[1]), "red")
    } else {
      insight::print_color(sprintf("'check_outliers()' does not support models of class '%s'.\n", class(x)[1]), "red")
    }
    return(NULL)
  }

  dat[[".id"]] <- 1:nrow(dat)
  dat[[".outliers"]] <- dist > threshold
  dat[[".distance"]] <- dist

  class(dat) <- c("check_outliers", "see_check_outliers", "data.frame")
  attr(dat, "threshold") <- threshold
  attr(dat, "method") <- method
  attr(dat, "text_size") <- 3
  dat
}
