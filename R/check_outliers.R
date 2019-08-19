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
#' \subsection{Methods}{
#' \describe{
#' \item{\strong{Cook's Distance}}{
#'  Among outlier detection methods, Cook's distance and leverage are less common
#'  than the basic Mahalanobis distance, but still used. Cook's distance estimates
#'  the variations in regression coefficients after removing each observation,
#'  one by one (Cook, 1977). Since Cook's distance is in the metric of an F distribution with p and n-p degrees of freedom, the median point of the quantile distribution can be used as a cut-off (Bollen, 1985). A common approximation or heuristic is to use 4 divided by the numbers of observations, which usually correponds to a lower threshold (i.e., more outliers are detected).
#' }
#' \item{\strong{Mahalanobis Distance}}{
#' Mahalanobis distance (Mahalanobis, 1930) is often used for multivariate outliers
#' detection as this distance takes into account the shape of the observations.
#' The default \code{threshold} is often arbitrarily set to some deviation (in
#' terms of SD or MAD) from the mean (or median) of the Mahalanobis distance.
#' However, as the Mahalanobis distance can be approximated by a Chi squared
#' distribution (Rousseeuw & Van Zomeren, 1990), we can use the the alpha quantile
#' of the chi-square distribution with k degrees of freedom (k being the number of
#' columns). By default, the alpha threshold is set to 0.025 (corresponding to the
#' 2.5\% most extreme observations; Cabana, 2019). This criterion is a natural extension of the
#' median plus or minus a coefficient times the MAD method (Leys et al., 2013).
#' }
#' \item{\strong{Minimum Covariance Determinant (MCD)}}{
#' Leys et al. (2018) argue that Mahalanobis Distance s not a robust way to
#' determine outliers, as it uses the means and covariances of all the data
#' – including the outliers – to determine individual difference scores. Minimum
#' Covariance Determinant calculates the mean and covariance matrix based on the
#' most central subset of the data (for instance, 50\%), before computing the
#' Mahalanobis Distance. This is deemed to be a more robust method of identifying
#' and removing outliers than regular Mahalanobis distance.
#' }
#' \item{\strong{Invariant Coordinate Selection (ICS)}}{
#'  The outlier are detected using ICS, which by default uses an alpha threshold
#'  of 0.025 (corresponding to the 2.5\% most extreme observations) as a cut-off value for outliers classification. Refer to the help-file
#'  of \code{ICSOutlier::ics.outlier()} to get more details about this procedure.
#'  Note that \code{method = "ics"} requires both \pkg{ICS} and \pkg{ICSOutlier}
#'  to be installed, and that it takes some time to compute the results.
#' }
#' \item{\strong{OPTICS}}{
#'  The Ordering Points To Identify the Clustering Structure (OPTICS) algorithm (Ankerst et al., 1999) is using similar concepts to DBSCAN (an unsupervised clustering technique that can be used for outliers detection). The threshold argument is passsed as \code{minPts}, which corresponds to the minimum size of a cluster. By default, this size is set at 2 times the number of columns (Sander et al., 1998). Compared to the others techniques, that will always detect several outliers (as these are usually defined as a percentage of extreme values), this algorithm functions in a different manner and won't always detect outliers. Note that \code{method = "optics"} requires the \pkg{dbscan} package to be installed, and that it takes some time to compute the results.
#' }
#' \item{\strong{Isolation Forest}}{
#'  The outliers are detected using the anomaly score of an isolation forest (a class of random forest). The default threshold
#'  of 0.025 will classify as outliers the observations located at \code{qnorm(1-0.025) * MAD) (a robust equivalent of SD) of the median (roughly corresponding to the 2.5\% most extreme observations).
#' }
#' }
#' }
#'
#' @references \itemize{
#' \item Cook, R. D. (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18.
#' \item Bollen, K. A., & Jackman, R. W. (1985). Regression diagnostics: An expository treatment of outliers and influential cases. Sociological Methods & Research, 13(4), 510-542.
#' \item Archimbaud, A., Nordhausen, K., \& Ruiz-Gazen, A. (2018). ICS for multivariate outlier detection with application to quality control. Computational Statistics & Data Analysis, 128, 184–199. \doi{10.1016/j.csda.2018.06.011}
#' \item Leys, C., Klein, O., Dominicy, Y., \& Ley, C. (2018). Detecting multivariate outliers: Use a robust variant of Mahalanobis distance. Journal of Experimental Social Psychology, 74, 150-156.
#' \item Rousseeuw, P. J., \& Van Zomeren, B. C. (1990). Unmasking multivariate outliers and leverage points. Journal of the American Statistical association, 85(411), 633-639.
#' \item Cabana, E., Lillo, R. E., \& Laniado, H. (2019). Multivariate outlier detection based on a robust Mahalanobis distance with shrinkage estimators. arXiv preprint arXiv:1904.02596.
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
#' # For dataframes
#' check_outliers(mtcars)
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














# "cook" threshold = stats::qf(0.5, ncol(x), nrow(x) -  ncol(x)),
#' @rdname check_outliers
#' @export
check_outliers.data.frame <- function(x, method = c("mahalanobis", "mcd", "ics", "optics", "iforest"), threshold = NULL, ...) {

  # Remove non-numerics
  x <- x[, sapply(x, is.numeric), drop = FALSE]

  # Check args
  method <- match.arg(method, several.ok = TRUE)


  # Default thresholds
  if (is.null(threshold)) {
    thresholds <- list(
      "mahalanobis" = stats::qchisq(p = 1 - 0.025, df = ncol(x)),
      "mcd" = stats::qchisq(p = 1 - 0.025, df = ncol(x)),
      "ics" = 0.025,
      "optics" = 2 * ncol(x),
      "iforest" = 0.025)
  } else if(is.list(threshold)){
    thresholds <- threshold
    for(i in c(method)){
      if(is.null(thresholds[[i]])){
        thresholds[[i]] <- threshold
      }
    }
  } else{
    thresholds <- list()
    for(i in c(method)){
      thresholds[[i]] <- threshold
    }
  }



  out <- list()
  # Mahalanobis
  if("mahalanobis" %in% c(method)){
    out <- c(out, .check_outliers_mahalanobis(x, threshold = thresholds$mahalanobis))
  }

  # MCD
  if("mcd" %in% c(method)){
    out <- c(out, .check_outliers_mcd(x, threshold = thresholds$mcd, percentage_central = .50))
  }

  # ICS
  if ("ics" %in% c(method)) {
    out <- c(out, .check_outliers_ics(x, threshold = thresholds$ics))
  }
  # OPTICS
  if ("optics" %in% c(method)) {
    out <- c(out, .check_outliers_optics(x, threshold = thresholds$optics))
  }
  # Isolation Forest
  if ("iforest" %in% c(method)) {
    out <- c(out, .check_outliers_iforest(x, threshold = thresholds$iforest))
  }

  # Combine outlier data
  df <- data.frame(Obs = 1:nrow(x))
  for(i in names(out[sapply(out, is.data.frame)])){
    df <- cbind(df, out[[i]])
  }
  df$Obs <- NULL


  # Composite outlier score
  df$Outlier <- rowMeans(df[grepl("Outlier_", names(df))])

  # Out
  outlier <- df$Outlier

  # Attributes
  # class(df) <- c("check_outliers", "see_check_outliers", class(df))
  attr(outlier, "data") <- df
  attr(outlier, "threshold") <- thresholds
  attr(outlier, "method") <- method
  attr(outlier, "text_size") <- 3
  outlier
}

















#' @keywords internal
.check_outliers_ics <- function(x, threshold = 0.025, ...){
  out <- data.frame(Obs = 1:nrow(x))

  # Install packages
  if (!requireNamespace("ICS", quietly = TRUE)) {
    stop("Package `ICS` needed for this function to work. Please install it.", call. = FALSE)
  }
  if (!requireNamespace("ICSOutlier", quietly = TRUE)) {
    stop("Package `ICSOutlier` needed for this function to work. Please install it.", call. = FALSE)
  }

  # Get n cores
  n_cores <- if (!requireNamespace("parallel", quietly = TRUE)){
    NULL
  } else{
    parallel::detectCores() - 1
  }

  # Run algorithm
  # Try
  outliers <- tryCatch({
    ics <- ICS::ics2(x)
    ICSOutlier::ics.outlier(object = ics, ncores = n_cores, level.dist = threshold, ...)
  },
  error = function(e) {
    NULL
  })

  if(is.null(outliers)){
    if (ncol(x) == 1){
      insight::print_color("At least two numeric predictors are required to detect outliers.\n", "red")
    }
    else{
      insight::print_color(sprintf("'check_outliers()' does not support models of class '%s'.\n", class(x)[1]), "red")
    }
  }

  # Get results
  cutoff <- outliers@ics.dist.cutoff
  out$Distance_ICS <- outliers@ics.distances
  out$Outlier_ICS <- as.numeric(out$Distance_ICS > cutoff)
  out$Obs <- NULL

  # Out
  list("data_ICS" = out,
       "threshold_ICS" = threshold)
}














#' @keywords internal
.check_outliers_mahalanobis <- function(x, threshold = NULL){
  out <- data.frame(Obs = 1:nrow(x))

  # Compute
  out$Distance_Mahalanobis <- stats::mahalanobis(x, center = colMeans(x), cov = stats::cov(x))

  # Filter
  out$Outlier_Mahalanobis <- as.numeric(out$Distance_Mahalanobis > threshold)

  out$Obs <- NULL
  list("data_mahalanobis" = out,
       "threshold_mahalanobis" = threshold)
}













#' @keywords internal
.check_outliers_mcd <- function(x, threshold = NULL, percentage_central = .50){
  out <- data.frame(Obs = 1:nrow(x))



  # Install packages
  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("Package `MASS` needed for this function to work. Please install it.", call. = FALSE)
  }

  # Compute
  mcd <- MASS::cov.mcd(x, quantile.used = percentage_central * nrow(x))
  out$Distance_MCD <- stats::mahalanobis(x, center = mcd$center, cov = mcd$cov)

  # Filter
  out$Outlier_MCD <- as.numeric(out$Distance_MCD > threshold)

  out$Obs <- NULL
  list("data_mcd" = out,
       "threshold_mcd" = threshold)
}





#' @keywords internal
.check_outliers_optics <- function(x, threshold = NULL){
  out <- data.frame(Obs = 1:nrow(x))

  # Install packages
  if (!requireNamespace("dbscan", quietly = TRUE)) {
    stop("Package `dbscan` needed for this function to work. Please install it.", call. = FALSE)
  }

  # Compute
  rez <- dbscan::optics(x, minPts = threshold)
  rez <- dbscan::extractXi(rez, xi = 0.05)

  out$Distance_OPTICS <- rez$coredist
  # Filter
  out$Outlier_OPTICS <- as.numeric(rez$cluster == 0)

  out$Obs <- NULL
  list("data_optics" = out,
       "threshold_optics" = threshold)
}


#' @keywords internal
.check_outliers_iforest <- function(x, threshold = 0.025){
  out <- data.frame(Obs = 1:nrow(x))

  # Install packages
  if (!requireNamespace("solitude", quietly = TRUE)) {
    stop("Package `solitude` needed for this function to work. Please install it.", call. = FALSE)
  }

  # Compute
  iforest <- solitude::isolationForest(x)
  out$Distance_iforest <- predict(iforest, x, type = "anomaly_score")

  # Threshold
  cutoff <- median(out$Distance_iforest) + qnorm(1 - threshold) * mad(out$Distance_iforest)
  # Filter
  out$Outlier_iforest <- as.numeric(out$Distance_iforest >= cutoff)

  out$Obs <- NULL
  list("data_iforest" = out,
       "threshold_iforest" = threshold)
}