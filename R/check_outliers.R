#' @title Outliers detection (check for influential observations)
#' @name check_outliers
#'
#' @description Checks for and locates influential observations (i.e., "outliers") via several distance and/or clustering methods. If several methods are selected, the returned "Outlier" vector will be a composite outlier score, made of the average of the binary (0 or 1) results of each method. It represents the probability of each observation of being classified as an outlier by at least one method. The decision rule used by default is to classify as outliers observations which composite outlier score is superior or equal to 0.5 (i.e., that were classified as outliers by at least half of the methods). See the \strong{Details} section below for a description of the methods.
#'
#' @param x A model or a data.frame object.
#' @param method The outlier detection method(s). Can be "all" or some of c("cook", "pareto", "zscore", "iqr", "mahalanobis", "robust", "mcd", "ics", "optics", "lof").
#' @param threshold A list containing the threshold values for each method (e.g. \code{list('mahalanobis' = 7, 'cook' = 1)}), above which an observation is
#'   considered as outlier. If \code{NULL}, default values will be used (see 'Details').
#'
#' @param ... When \code{method = "ics"}, further arguments in \code{...} are
#'   passed down to \code{ICSOutlier::ics.outlier()}.
#'
#' @return A logical vector of the detected outliers with a nice printing
#'   method: a check (message) on whether outliers were detected or not. The
#'   information on the distance measure and whether or not an observation is
#'   considered as outlier can be recovered with the \code{as.data.frame}
#'   function.
#'
#' @note There is also a \href{https://easystats.github.io/see/articles/performance.html}{\code{plot()}-method} implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}. \strong{Please note} that the range of the distance-values along the y-axis is re-scaled to range from 0 to 1.
#'
#' @details Outliers can be defined as particularly influential observations. Most methods rely on the computation of some distance metric, and the observations greater than a certain threshold are considered outliers. Importantly, outliers detection methods are meant to provide information to the researcher, rather than being an automatized procedure which mindless application is a substitute for thinking.
#'
#' An \strong{example sentence} for reporting the usage of the composite method could be:
#'
#' \emph{"Based on a composite outlier score (see the 'check_outliers' function in the 'performance' R package; Lüdecke et al., 2019) obtained via the joint application of multiple outliers detection algorithms (Z-scores, Iglewicz, 1993; Interquartile range (IQR); Mahalanobis distance, Cabana, 2019; Robust Mahalanobis distance, Gnanadesikan & Kettenring, 1972; Minimum Covariance Determinant, Leys et al., 2018; Invariant Coordinate Selection, Archimbaud et al., 2018; OPTICS, Ankerst et al., 1999; Isolation Forest, Liu et al. 2008; and Local Outlier Factor, Breunig et al., 2000), we excluded n participants that were classified as outliers by at least half of the methods used."}
#'
#' \subsection{Model-specific methods}{
#' \itemize{
#' \item \strong{Cook's Distance}:
#'  Among outlier detection methods, Cook's distance and leverage are less common
#'  than the basic Mahalanobis distance, but still used. Cook's distance estimates
#'  the variations in regression coefficients after removing each observation,
#'  one by one (Cook, 1977). Since Cook's distance is in the metric of an F
#'  distribution with p and n-p degrees of freedom, the median point of the quantile
#'  distribution can be used as a cut-off (Bollen, 1985). A common approximation
#'  or heuristic is to use 4 divided by the numbers of observations, which usually
#'  corresponds to a lower threshold (i.e., more outliers are detected). This only works for Frequentist models. For Bayesian models, see \code{pareto}.
#'
#' \item \strong{Pareto}:
#' The reliability and approximate convergence of Bayesian models can be assessed using the estimates for the shape parameter k of the generalized Pareto distribution. If the estimated tail shape parameter k exceeds 0.5, the user should be warned, although in practice the authors of the \code{loo} package observed good performance for values of k up to 0.7 (the default threshold used by \code{performance}).
#' }}
#'
#'
#' \subsection{Univariate methods}{
#' \itemize{
#' \item \strong{Z-scores}:
#'  The Z-score, or standard score, is a way of describing a data point as deviance from a central value, in terms of standard deviations from the mean or, as it is here the case by default (Iglewicz, 1993), in terms of Median Absolute Deviation (MAD) from the median (which are robust measures of dispersion and centrality). The default threshold to classify outliers is 1.959 (\code{threshold = list("zscore" = = 1.959)}), corresponding to the 2.5\% (\code{qnorm(0.975)}) most extreme observations (assuming the data is normally distributed). Importantly, the Z-score method is univariate: it is computed column by column. If a data.frame is passed, then the maximum distance is kept for each observations. Thus, all observations that are extreme for at least one variable will be detected. However, this method is not suited for high dimensional data (with many columns), returning too liberal results (detecting many outliers).
#'
#' \item \strong{IQR}:
#'  Using the IQR (interquartile range) is a robust method developed by John Tukey, which often appears in box-and-whisker plots (e.g., in \code{geom_boxplot}). The interquartile range is the range between the first and the third quartiles. Tukey considered as outliers any data point that fell outside of either 1.5 times (the default threshold) the IQR below the first or above the third quartile. Similar to the Z-score method, this is a univariate method for outliers detection, returning outliers detected for at least one column, and might thus not be suited to high dimensional data.
#' }}
#'
#' \subsection{Multivariate methods}{
#' \itemize{
#' \item \strong{Mahalanobis Distance}:
#' Mahalanobis distance (Mahalanobis, 1930) is often used for multivariate outliers
#' detection as this distance takes into account the shape of the observations.
#' The default \code{threshold} is often arbitrarily set to some deviation (in
#' terms of SD or MAD) from the mean (or median) of the Mahalanobis distance.
#' However, as the Mahalanobis distance can be approximated by a Chi squared
#' distribution (Rousseeuw & Van Zomeren, 1990), we can use the alpha quantile
#' of the chi-square distribution with k degrees of freedom (k being the number of
#' columns). By default, the alpha threshold is set to 0.025 (corresponding to the
#' 2.5\% most extreme observations; Cabana, 2019). This criterion is a natural extension of the
#' median plus or minus a coefficient times the MAD method (Leys et al., 2013).
#'
#' \item \strong{Robust Mahalanobis Distance}:
#' A robust version of Mahalanobis distance using an Orthogonalized
#' Gnanadesikan-Kettenring pairwise estimator (Gnanadesikan \& Kettenring, 1972).
#' Requires the \pkg{bigutilsr} package. See the \code{bigutilsr::dist_ogk()} function.
#'
#' \item \strong{Minimum Covariance Determinant (MCD)}:
#' Another robust version of Mahalanobis. Leys et al. (2018) argue that Mahalanobis Distance is not a robust way to
#' determine outliers, as it uses the means and covariances of all the data
#' – including the outliers – to determine individual difference scores. Minimum
#' Covariance Determinant calculates the mean and covariance matrix based on the
#' most central subset of the data (by default, 66\%), before computing the
#' Mahalanobis Distance. This is deemed to be a more robust method of identifying
#' and removing outliers than regular Mahalanobis distance.
#'
#' \item \strong{Invariant Coordinate Selection (ICS)}:
#'  The outlier are detected using ICS, which by default uses an alpha threshold
#'  of 0.025 (corresponding to the 2.5\% most extreme observations) as a cut-off value for outliers classification. Refer to the help-file
#'  of \code{ICSOutlier::ics.outlier()} to get more details about this procedure.
#'  Note that \code{method = "ics"} requires both \pkg{ICS} and \pkg{ICSOutlier}
#'  to be installed, and that it takes some time to compute the results.
#'
#' \item \strong{OPTICS}:
#'  The Ordering Points To Identify the Clustering Structure (OPTICS) algorithm (Ankerst et al., 1999) is using similar concepts to DBSCAN (an unsupervised clustering technique that can be used for outliers detection). The threshold argument is passed as \code{minPts}, which corresponds to the minimum size of a cluster. By default, this size is set at 2 times the number of columns (Sander et al., 1998). Compared to the others techniques, that will always detect several outliers (as these are usually defined as a percentage of extreme values), this algorithm functions in a different manner and won't always detect outliers. Note that \code{method = "optics"} requires the \pkg{dbscan} package to be installed, and that it takes some time to compute the results.
#'
#' \item \strong{Isolation Forest}:
#'  The outliers are detected using the anomaly score of an isolation forest (a class of random forest). The default threshold
#'  of 0.025 will classify as outliers the observations located at \code{qnorm(1-0.025) * MAD)} (a robust equivalent of SD) of the median (roughly corresponding to the 2.5\% most extreme observations). Requires the \pkg{solitude} package.
#'
#'  \item \strong{Local Outlier Factor}:
#'  Based on a K nearest neighbours algorithm, LOF compares the local density
#'  of an point to the local densities of its neighbors instead of computing a
#'  distance from the center (Breunig et al., 2000). Points that have a
#'  substantially lower density than their neighbors are considered outliers.
#'  A LOF score of approximately 1 indicates that density around the point is
#'  comparable to its neighbors. Scores significantly larger than 1 indicate outliers.
#'  The default threshold of 0.025 will classify as outliers the observations
#'  located at \code{qnorm(1-0.025) * SD)} of the log-transformed LOF distance.
#'  Requires the \pkg{dbscan} package.
#' }}
#'
#' \subsection{Threshold specification}{
#' Default thresholds are currently specified as follows:
#'
#' \preformatted{
#' list(
#'   zscore = stats::qnorm(p = 1 - 0.025),
#'   iqr = 1.5,
#'   cook = stats::qf(0.5, ncol(x), nrow(x) - ncol(x)),
#'   pareto = 0.7,
#'   mahalanobis = stats::qchisq(p = 1 - 0.025, df = ncol(x)),
#'   robust = stats::qchisq(p = 1 - 0.025, df = ncol(x)),
#'   mcd = stats::qchisq(p = 1 - 0.025, df = ncol(x)),
#'   ics = 0.025,
#'   optics = 2 * ncol(x),
#'   iforest = 0.025,
#'   lof = 0.025
#' )
#' }}
#'
#' @references \itemize{
#' \item Archimbaud, A., Nordhausen, K., \& Ruiz-Gazen, A. (2018). ICS for multivariate outlier detection with application to quality control. Computational Statistics & Data Analysis, 128, 184–199. \doi{10.1016/j.csda.2018.06.011}
#' \item Gnanadesikan, R., \& Kettenring, J. R. (1972). Robust estimates, residuals, and outlier detection with multiresponse data. Biometrics, 81-124.
#' \item Bollen, K. A., & Jackman, R. W. (1985). Regression diagnostics: An expository treatment of outliers and influential cases. Sociological Methods & Research, 13(4), 510-542.
#' \item Cabana, E., Lillo, R. E., \& Laniado, H. (2019). Multivariate outlier detection based on a robust Mahalanobis distance with shrinkage estimators. arXiv preprint arXiv:1904.02596.
#' \item Cook, R. D. (1977). Detection of influential observation in linear regression. Technometrics, 19(1), 15-18.
#' \item Iglewicz, B., & Hoaglin, D. C. (1993). How to detect and handle outliers (Vol. 16). Asq Press.
#' \item Leys, C., Klein, O., Dominicy, Y., \& Ley, C. (2018). Detecting multivariate outliers: Use a robust variant of Mahalanobis distance. Journal of Experimental Social Psychology, 74, 150-156.
#' \item Liu, F. T., Ting, K. M., & Zhou, Z. H. (2008, December). Isolation forest. In 2008 Eighth IEEE International Conference on Data Mining (pp. 413-422). IEEE.
#' \item Rousseeuw, P. J., \& Van Zomeren, B. C. (1990). Unmasking multivariate outliers and leverage points. Journal of the American Statistical association, 85(411), 633-639.
#' }
#'
#' @examples
#' data <- mtcars # Size nrow(data) = 32
#'
#' # For single variables ------------------------------------------------------
#' outliers_list <- check_outliers(data$mpg) # Find outliers
#' outliers_list # Show the row index of the outliers
#' as.numeric(outliers_list) # The object is a binary vector...
#' filtered_data <- data[!outliers_list, ] # And can be used to filter a dataframe
#' nrow(filtered_data) # New size, 28 (4 outliers removed)
#'
#'
#' # For dataframes ------------------------------------------------------
#' check_outliers(data) # It works the same way on dataframes
#'
#' # You can also use multiple methods at once
#' outliers_list <- check_outliers(data, method = c(
#'   "mahalanobis",
#'   "iqr",
#'   "zscore"
#' ))
#' outliers_list
#' # Using `as.data.frame()`, we can access more details!
#' outliers_info <- as.data.frame(outliers_list)
#' head(outliers_info)
#' outliers_info$Outlier # Including the probability of being an outlier
#' # And we can be more stringent in our outliers removal process
#' filtered_data <- data[outliers_info$Outlier < 0.1, ]
#' \dontrun{
#' # You can also run all the methods
#' check_outliers(data, method = "all")
#'
#' # For statistical models ---------------------------------------------
#' # select only mpg and disp (continuous)
#' mt1 <- mtcars[, c(1, 3, 4)]
#' # create some fake outliers and attach outliers to main df
#' mt2 <- rbind(mt1, data.frame(
#'   mpg = c(37, 40), disp = c(300, 400),
#'   hp = c(110, 120)
#' ))
#' # fit model with outliers
#' model <- lm(disp ~ mpg + hp, data = mt2)
#'
#' outliers_list <- check_outliers(model)
#' # plot(outliers_list)
#' insight::get_data(model)[outliers_list, ] # Show outliers data
#'
#' if (require("MASS")) {
#'   check_outliers(model, method = c("mahalabonis", "mcd"))
#' }
#' if (require("ICS")) {
#'   # This one takes some seconds to finish...
#'   check_outliers(model, method = "ics")
#' }
#' }
#' @export
check_outliers <- function(x, ...) {
  UseMethod("check_outliers")
}




#' @rdname check_outliers
#' @export
check_outliers.default <- function(x, method = c("cook", "pareto"), threshold = NULL, ...) {

  # Check args
  if (all(method == "all")) {
    method <- c("zscore", "iqr", "cook", "pareto", "mahalanobis", "robust", "mcd", "ics", "optics", "iforest", "lof")
  }
  method <- match.arg(method, c("zscore", "iqr", "cook", "pareto", "mahalanobis", "robust", "mcd", "ics", "optics", "iforest", "lof"), several.ok = TRUE)

  # Remove non-numerics
  data <- insight::get_modelmatrix(x)



  # Thresholds
  if (is.null(threshold)) {
    thresholds <- .check_outliers_thresholds(data)
  } else if (is.list(threshold)) {
    thresholds <- .check_outliers_thresholds(data)
    thresholds[[names(threshold)]] <- threshold[[names(threshold)]]
  } else {
    stop("The `threshold` argument must be NULL (for default values) or a list containig threshold values for desired methods (e.g., `list('mahalanobis' = 7)`).")
  }



  # Others
  if (!all(method %in% c("cook", "pareto"))) {
    df <- check_outliers(data, method, threshold, ...)
    df <- attributes(df)$data
  } else {
    df <- data.frame(Obs = row.names(data))
  }

  # Cook
  if ("cook" %in% c(method) & insight::model_info(x)$is_bayesian == FALSE & !inherits(x, "bife")) {
    df <- cbind(df, .check_outliers_cook(x, threshold = thresholds$cook)$data_cook)
  }
  # Pareto
  if ("pareto" %in% c(method) & insight::model_info(x)$is_bayesian) {
    df <- cbind(df, .check_outliers_pareto(x, threshold = thresholds$pareto)$data_pareto)
  }



  # Composite outlier score
  df$Outlier <- rowMeans(df[grepl("Outlier_", names(df))])
  df <- df[c(names(df)[names(df) != "Outlier"], "Outlier")]

  # Out
  outlier <- df$Outlier > 0.5

  # Attributes
  class(outlier) <- c("check_outliers", "see_check_outliers", class(outlier))
  attr(outlier, "data") <- df
  attr(outlier, "threshold") <- thresholds
  attr(outlier, "method") <- method
  attr(outlier, "text_size") <- 3
  attr(outlier, "influential_obs") <- .influential_obs(x)
  outlier
}






#' @rdname check_outliers
#' @export
check_outliers.numeric <- function(x, method = "zscore", threshold = NULL, ...) {
  check_outliers(as.data.frame(x), method = method, threshold = threshold, ...)
}



#' @rdname check_outliers
#' @export
check_outliers.data.frame <- function(x, method = "mahalanobis", threshold = NULL, ...) {

  # Remove non-numerics
  x <- x[, sapply(x, is.numeric), drop = FALSE]

  # Check args
  if (all(method == "all")) {
    method <- c("zscore", "iqr", "cook", "pareto", "mahalanobis", "robust", "mcd", "ics", "optics", "iforest", "lof")
  }
  method <- match.arg(method, c("zscore", "iqr", "cook", "pareto", "mahalanobis", "robust", "mcd", "ics", "optics", "lof"), several.ok = TRUE)

  # Thresholds
  if (is.null(threshold)) {
    thresholds <- .check_outliers_thresholds(x)
  } else if (is.list(threshold)) {
    thresholds <- .check_outliers_thresholds(x)
    thresholds[[names(threshold)]] <- threshold[[names(threshold)]]
  } else {
    stop("The `threshold` argument must be NULL (for default values) or a list containig threshold values for desired methods (e.g., `list('mahalanobis' = 7)`).")
  }

  out <- list()
  # Z-score
  if ("zscore" %in% c(method)) {
    out <- c(out, .check_outliers_zscore(x, threshold = thresholds$zscore, robust = TRUE, method = "max"))
  }

  # IQR
  if ("iqr" %in% c(method)) {
    out <- c(out, .check_outliers_iqr(x, threshold = thresholds$iqr, method = "tukey"))
  }

  # Mahalanobis
  if ("mahalanobis" %in% c(method)) {
    out <- c(out, .check_outliers_mahalanobis(x, threshold = thresholds$mahalanobis, ...))
  }

  # Robust Mahalanobis
  if ("robust" %in% c(method)) {
    out <- c(out, .check_outliers_robust(x, threshold = thresholds$robust))
  }

  # MCD
  if ("mcd" %in% c(method)) {
    out <- c(out, .check_outliers_mcd(x, threshold = thresholds$mcd, percentage_central = .66))
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
  # if ("iforest" %in% c(method)) {
  #   out <- c(out, .check_outliers_iforest(x, threshold = thresholds$iforest))
  # }

  # Local Outlier Factor
  if ("lof" %in% c(method)) {
    out <- c(out, .check_outliers_lof(x, threshold = thresholds$lof))
  }

  # Combine outlier data
  df <- data.frame(Obs = row.names(x))
  for (i in names(out[sapply(out, is.data.frame)])) {
    df <- cbind(df, out[[i]])
  }
  df$Obs <- NULL # Remove temp column


  # Composite outlier score
  df$Outlier <- rowMeans(df[grepl("Outlier_", names(df))])

  # Out
  outlier <- df$Outlier > 0.5

  # Attributes
  class(outlier) <- c("check_outliers", "see_check_outliers", class(outlier))
  attr(outlier, "data") <- df
  attr(outlier, "threshold") <- thresholds
  attr(outlier, "method") <- method
  attr(outlier, "text_size") <- 3
  outlier
}



#' @export
as.data.frame.check_outliers <- function(x, ...) {
  attributes(x)$data
}

#' @export
as.numeric.check_outliers <- function(x, ...) {
  attributes(x)$data$Outlier
}




.check_outliers_thresholds <- function(x) {
  suppressWarnings(.check_outliers_thresholds_nowarn(x))
}

.check_outliers_thresholds_nowarn <- function(x) {
  zscore <- stats::qnorm(p = 1 - 0.025)
  iqr <- 1.5
  cook <- stats::qf(0.5, ncol(x), nrow(x) - ncol(x))
  pareto <- 0.7
  mahalanobis <- stats::qchisq(p = 1 - 0.025, df = ncol(x))
  robust <- stats::qchisq(p = 1 - 0.025, df = ncol(x))
  mcd <- stats::qchisq(p = 1 - 0.025, df = ncol(x))
  ics <- 0.025
  optics <- 2 * ncol(x)
  iforest <- 0.025
  lof <- 0.025

  list(
    "zscore" = zscore,
    "iqr" = iqr,
    "cook" = cook,
    "pareto" = pareto,
    "mahalanobis" = mahalanobis,
    "robust" = robust,
    "mcd" = mcd,
    "ics" = ics,
    "optics" = optics,
    "iforest" = iforest,
    "lof" = lof
  )
}



.check_outliers_zscore <- function(x, threshold = stats::qnorm(p = 1 - 0.025), robust = TRUE, method = "max") {
  # Standardize
  if (robust == FALSE) {
    d <- abs(as.data.frame(sapply(x, function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE))))
  } else {
    d <- abs(as.data.frame(sapply(x, function(x) (x - stats::median(x, na.rm = TRUE)) / stats::mad(x, na.rm = TRUE))))
  }

  out <- data.frame(Obs = 1:nrow(as.data.frame(d)))

  out$Distance_Zscore <- sapply(as.data.frame(t(d)), method, na.omit = TRUE, na.rm = TRUE)

  # Filter
  out$Outlier_Zscore <- as.numeric(out$Distance_Zscore > threshold)

  out$Obs <- NULL
  list(
    "data_zscore" = out,
    "threshold_zscore" = threshold
  )
}



.check_outliers_iqr <- function(x, threshold = 1.5, method = "tukey") {
  d <- data.frame(Obs = 1:nrow(as.data.frame(x)))
  for (col in 1:ncol(as.data.frame(x))) {
    v <- x[, col]

    if (method == "tukey") {
      iqr <- stats::quantile(v, 0.75, na.rm = TRUE) - stats::quantile(v, 0.25, na.rm = TRUE)
    } else {
      iqr <- stats::IQR(v, na.rm = TRUE)
    }

    lower <- stats::quantile(v, 0.25, na.rm = TRUE) - (iqr * threshold)
    upper <- stats::quantile(v, 0.75, na.rm = TRUE) + (iqr * threshold)

    d[names(as.data.frame(x))[col]] <- ifelse(v > upper, 1,
      ifelse(v < lower, 1, 0)
    )
  }
  d$Obs <- NULL

  out <- data.frame(Obs = 1:nrow(as.data.frame(d)))
  out$Distance_IQR <- sapply(as.data.frame(t(d)), function(x) {
    ifelse(all(is.na(x)), NA, mean(x))
  })
  out$Outlier_IQR <- sapply(as.data.frame(t(d)), function(x) {
    ifelse(all(is.na(x)), NA, max(x))
  })

  out$Obs <- NULL
  list(
    "data_iqr" = out,
    "threshold_iqr" = threshold
  )
}




.check_outliers_cook <- function(x, threshold = NULL) {
  # Compute
  d <- unname(stats::cooks.distance(x))
  out <- data.frame(Obs = 1:length(d))
  out$Distance_Cook <- d

  # Filter
  out$Outlier_Cook <- as.numeric(out$Distance_Cook > threshold)

  out$Obs <- NULL
  list(
    "data_cook" = out,
    "threshold_cook" = threshold
  )
}




.check_outliers_pareto <- function(x, threshold = 0.7) {
  insight::check_if_installed("loo")

  # Compute
  d <- suppressWarnings(loo::pareto_k_values(loo::loo(x)))

  out <- data.frame(Obs = 1:length(d))
  out$Distance_Pareto <- d

  # Filter
  out$Outlier_Pareto <- as.numeric(out$Distance_Pareto > threshold)

  out$Obs <- NULL
  list(
    "data_pareto" = out,
    "threshold_pareto" = threshold
  )
}




.check_outliers_mahalanobis <- function(x, threshold = NULL, ...) {
  out <- data.frame(Obs = 1:nrow(x))

  # Compute
  out$Distance_Mahalanobis <- stats::mahalanobis(x, center = colMeans(x), cov = stats::cov(x), ...)

  # Filter
  out$Outlier_Mahalanobis <- as.numeric(out$Distance_Mahalanobis > threshold)

  out$Obs <- NULL
  list(
    "data_mahalanobis" = out,
    "threshold_mahalanobis" = threshold
  )
}


# Bigutils not yet fully available on CRAN
.check_outliers_robust <- function(x, threshold = NULL) {
  out <- data.frame(Obs = 1:nrow(x))

  insight::check_if_installed("bigutilsr")

  # Compute
  U <- svd(scale(x))$u
  out$Distance_Robust <- bigutilsr::dist_ogk(U)

  # Filter
  out$Outlier_Robust <- as.numeric(out$Distance_Robust > threshold)

  out$Obs <- NULL
  list(
    "data_robust" = out,
    "threshold_robust" = threshold
  )
}



.check_outliers_mcd <- function(x, threshold = NULL, percentage_central = .50) {
  out <- data.frame(Obs = 1:nrow(x))
  insight::check_if_installed("MASS")

  # Compute
  mcd <- MASS::cov.mcd(x, quantile.used = percentage_central * nrow(x))
  out$Distance_MCD <- stats::mahalanobis(x, center = mcd$center, cov = mcd$cov)

  # Filter
  out$Outlier_MCD <- as.numeric(out$Distance_MCD > threshold)

  out$Obs <- NULL
  list(
    "data_mcd" = out,
    "threshold_mcd" = threshold
  )
}




.check_outliers_ics <- function(x, threshold = 0.025, ...) {
  out <- data.frame(Obs = 1:nrow(x))

  insight::check_if_installed("ICS")
  insight::check_if_installed("ICSOutlier")

  # Get n cores
  n_cores <- if (!requireNamespace("parallel", quietly = TRUE)) {
    NULL
  } else {
    parallel::detectCores() - 1
  }

  # Run algorithm
  # Try
  outliers <- tryCatch(
    {
      ics <- ICS::ics2(x)
      ICSOutlier::ics.outlier(object = ics, ncores = n_cores, level.dist = threshold, ...)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(outliers)) {
    if (ncol(x) == 1) {
      insight::print_color("At least two numeric predictors are required to detect outliers.\n", "red")
    } else {
      insight::print_color(sprintf("'check_outliers()' does not support models of class '%s'.\n", class(x)[1]), "red")
    }
  }

  # Get results
  cutoff <- outliers@ics.dist.cutoff
  out$Distance_ICS <- outliers@ics.distances
  out$Outlier_ICS <- as.numeric(out$Distance_ICS > cutoff)
  out$Obs <- NULL

  # Out
  list(
    "data_ICS" = out,
    "threshold_ICS" = threshold
  )
}




.check_outliers_optics <- function(x, threshold = NULL) {
  out <- data.frame(Obs = 1:nrow(x))
  insight::check_if_installed("dbscan")

  # Compute
  rez <- dbscan::optics(x, minPts = threshold)
  rez <- dbscan::extractXi(rez, xi = 0.05) # TODO: find automatic way of setting xi

  out$Distance_OPTICS <- rez$coredist
  # Filter
  if (is.null(rez$cluster)) {
    out$Outlier_OPTICS <- 0
  } else {
    out$Outlier_OPTICS <- as.numeric(rez$cluster == 0)
  }


  out$Obs <- NULL
  list(
    "data_optics" = out,
    "threshold_optics" = threshold
  )
}


# .check_outliers_iforest <- function(x, threshold = 0.025) {
#   out <- data.frame(Obs = 1:nrow(x))
#
#   # Install packages
#  insight::check_if_installed("solitude")
#
#   # Compute
#   if (utils::packageVersion("solitude") < "0.2.0") {
#     iforest <- solitude::isolationForest(x)
#     out$Distance_iforest <- stats::predict(iforest, x, type = "anomaly_score")
#   } else if (utils::packageVersion("solitude") == "0.2.0") {
#     stop("Must update package `solitude` (above version 0.2.0). Please run `install.packages('solitude')`.", call. = FALSE)
#   } else {
#     iforest <- solitude::isolationForest$new(sample_size = nrow(x))
#     suppressMessages(iforest$fit(x))
#     out$Distance_iforest <- iforest$scores$anomaly_score
#   }
#
#
#   # Threshold
#   cutoff <- stats::median(out$Distance_iforest) + stats::qnorm(1 - threshold) * stats::mad(out$Distance_iforest)
#   # Filter
#   out$Outlier_iforest <- as.numeric(out$Distance_iforest >= cutoff)
#
#   out$Obs <- NULL
#   list(
#     "data_iforest" = out,
#     "threshold_iforest" = threshold
#   )
# }



.check_outliers_lof <- function(x, threshold = NULL) {
  out <- data.frame(Obs = 1:nrow(x))
  insight::check_if_installed("dbscan")

  # Compute
  out$Distance_LOF <- log(dbscan::lof(x, k = ncol(x) - 1))

  # Threshold
  # TODO: use tukey_mc from bigutilsr package
  cutoff <- stats::qnorm(1 - threshold) * stats::sd(out$Distance_LOF)

  # Filter
  out$Outlier_LOF <- as.numeric(out$Distance_LOF > cutoff)

  out$Obs <- NULL
  list(
    "data_lof" = out,
    "threshold_lof" = threshold
  )
}


# influential observations data --------

.influential_obs <- function(x, threshold = NULL) {
  tryCatch(
    {
      .diag_influential_obs(x, threshold = threshold)
    },
    error = function(e) {
      NULL
    }
  )
}


# Non-supported model classes ---------------------------------------

#' @export
check_outliers.glmmTMB <- function(x, ...) {
  NULL
}

#' @export
check_outliers.lme <- function(x, ...) {
  NULL
}

#' @export
check_outliers.lmrob <- function(x, ...) {
  NULL
}

#' @export
check_outliers.glmrob <- function(x, ...) {
  NULL
}
