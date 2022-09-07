#' @title Outliers detection (check for influential observations)
#' @name check_outliers
#'
#' @description Checks for and locates influential observations (i.e.,
#'   "outliers") via several distance and/or clustering methods. If several
#'   methods are selected, the returned "Outlier" vector will be a composite
#'   outlier score, made of the average of the binary (0 or 1) results of each
#'   method. It represents the probability of each observation of being
#'   classified as an outlier by at least one method. The decision rule used by
#'   default is to classify as outliers observations which composite outlier
#'   score is superior or equal to 0.5 (i.e., that were classified as outliers
#'   by at least half of the methods). See the **Details** section below
#'   for a description of the methods.
#'
#' @param x A model or a data.frame object.
#' @param method The outlier detection method(s). Can be `"all"`` or some of
#'   `"cook"`, `"pareto"`, `"zscore"`, `"zscore_robust"`, `"iqr"`, `"ci"`, `"eti"`,
#'   `"hdi"`, `"bci"`, `"mahalanobis"`, `"mahalanobis_robust"`, `"mcd"`, `"ics"`,
#'   `"optics"` or `"lof"`.
#' @param threshold A list containing the threshold values for each method (e.g.
#'   `list('mahalanobis' = 7, 'cook' = 1)`), above which an observation is
#'   considered as outlier. If `NULL`, default values will be used (see
#'   'Details'). If a numeric value is given, it will be used as the threshold
#'   for any of the method run.
#' @param ID Optional, to report an ID column along with the row number.
#' @param ... When `method = "ics"`, further arguments in `...` are passed
#' down to `ICSOutlier::ics.outlier()`. When `method = "mahalanobis"`,
#' they are  passed down to `stats::mahalanobis()`.
#'
#' @return A logical vector of the detected outliers with a nice printing
#'   method: a check (message) on whether outliers were detected or not. The
#'   information on the distance measure and whether or not an observation is
#'   considered as outlier can be recovered with the `as.data.frame`
#'   function.
#'
#' @note There is also a
#'   [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#'   implemented in the
#'   \href{https://easystats.github.io/see/}{\pkg{see}-package}. **Please
#'   note** that the range of the distance-values along the y-axis is re-scaled
#'   to range from 0 to 1.
#'
#' @details Outliers can be defined as particularly influential observations.
#'   Most methods rely on the computation of some distance metric, and the
#'   observations greater than a certain threshold are considered outliers.
#'   Importantly, outliers detection methods are meant to provide information to
#'   consider for the researcher, rather than to be an automatized procedure
#'   which mindless application is a substitute for thinking.
#'
#' An **example sentence** for reporting the usage of the composite method
#' could be:
#'
#' *"Based on a composite outlier score (see the 'check_outliers' function
#' in the 'performance' R package; Lüdecke et al., 2021) obtained via the joint
#' application of multiple outliers detection algorithms (Z-scores, Iglewicz,
#' 1993; Interquartile range (IQR); Mahalanobis distance, Cabana, 2019; Robust
#' Mahalanobis distance, Gnanadesikan and Kettenring, 1972; Minimum Covariance
#' Determinant, Leys et al., 2018; Invariant Coordinate Selection, Archimbaud et
#' al., 2018; OPTICS, Ankerst et al., 1999; Isolation Forest, Liu et al. 2008;
#' and Local Outlier Factor, Breunig et al., 2000), we excluded n participants
#' that were classified as outliers by at least half of the methods used."*
#'
#' @section Model-specific methods:
#'
#'  - **Cook's Distance**:
#'  Among outlier detection methods, Cook's distance and leverage are less
#'  common than the basic Mahalanobis distance, but still used. Cook's distance
#'  estimates the variations in regression coefficients after removing each
#'  observation, one by one (Cook, 1977). Since Cook's distance is in the metric
#'  of an F distribution with p and n-p degrees of freedom, the median point of
#'  the quantile distribution can be used as a cut-off (Bollen, 1985). A common
#'  approximation or heuristic is to use 4 divided by the numbers of
#'  observations, which usually corresponds to a lower threshold (i.e., more
#'  outliers are detected). This only works for Frequentist models. For Bayesian
#'  models, see `pareto`.
#'
#' - **Pareto**:
#' The reliability and approximate convergence of Bayesian models can be
#' assessed using the estimates for the shape parameter k of the generalized
#' Pareto distribution. If the estimated tail shape parameter k exceeds 0.5, the
#' user should be warned, although in practice the authors of the `loo`
#' package observed good performance for values of k up to 0.7 (the default
#' threshold used by `performance`).
#'
#' @section Univariate methods:
#'
#'  - **Z-scores** `("zscore", "zscore_robust")`:
#'  The Z-score, or standard score, is a way of describing a data point as
#'  deviance from a central value, in terms of standard deviations from the mean
#'  (`"zscore"`) or, as it is here the case (`"zscore_robust"`) by
#'  default (Iglewicz, 1993), in terms of Median Absolute Deviation (MAD) from
#'  the median (which are robust measures of dispersion and centrality). The
#'  default threshold to classify outliers is 1.959 (`threshold =
#'  list("zscore" = 1.959)`), corresponding to the 2.5\% (`qnorm(0.975)`)
#'  most extreme observations (assuming the data is normally distributed).
#'  Importantly, the Z-score method is univariate: it is computed column by
#'  column. If a dataframe is passed, the Z-score is calculated for each
#'  variable separately, and the maximum (absolute) Z-score is kept for each
#'  observations. Thus, all observations that are extreme on at least one
#'  variable might be detected as outliers. Thus, this method is not suited for
#'  high dimensional data (with many columns), returning too liberal results
#'  (detecting many outliers).
#'
#'  - **IQR** `("iqr")`:
#'  Using the IQR (interquartile range) is a robust method developed by John
#'  Tukey, which often appears in box-and-whisker plots (e.g., in
#'  `geom_boxplot`). The interquartile range is the range between the first
#'  and the third quartiles. Tukey considered as outliers any data point that
#'  fell outside of either 1.5 times (the default threshold is 1.7) the IQR below
#'  the first or above the third quartile. Similar to the Z-score method, this is
#'  a univariate method for outliers detection, returning outliers detected for
#'  at least one column, and might thus not be suited to high dimensional data.
#'
#'  - **CI** `("ci", "eti", "hdi", "bci")`:
#'  Another univariate method is to compute, for each variable, some sort of
#'  "confidence" interval and consider as outliers values lying beyond the edges
#'  of that interval. By default, `"ci"` computes the Equal-Tailed Interval
#'  (`"eti"`), but other types of intervals are available, such as Highest
#'  Density Interval (`"hdi"`) or the Bias Corrected and Accelerated
#'  Interval (`"bci"`). The default threshold is `0.95`, considering
#'  as outliers all observations that are outside the 95% CI on any of the
#'  variable. See [bayestestR::ci()] for more details
#'  about the intervals.
#'
#' @section Multivariate methods:
#'
#' - **Mahalanobis Distance**:
#' Mahalanobis distance (Mahalanobis, 1930) is often used for multivariate
#' outliers detection as this distance takes into account the shape of the
#' observations. The default `threshold` is often arbitrarily set to some
#' deviation (in terms of SD or MAD) from the mean (or median) of the
#' Mahalanobis distance. However, as the Mahalanobis distance can be
#' approximated by a Chi squared distribution (Rousseeuw and Van Zomeren, 1990),
#' we can use the alpha quantile of the chi-square distribution with k degrees
#' of freedom (k being the number of columns). By default, the alpha threshold
#' is set to 0.025 (corresponding to the 2.5\% most extreme observations;
#' Cabana, 2019). This criterion is a natural extension of the median plus or
#' minus a coefficient times the MAD method (Leys et al., 2013).
#'
#' - **Robust Mahalanobis Distance**:
#' A robust version of Mahalanobis distance using an Orthogonalized
#' Gnanadesikan-Kettenring pairwise estimator (Gnanadesikan and Kettenring,
#' 1972). Requires the \pkg{bigutilsr} package. See the
#' `bigutilsr::dist_ogk()` function.
#'
#' - **Minimum Covariance Determinant (MCD)**:
#' Another robust version of Mahalanobis. Leys et al. (2018) argue that
#' Mahalanobis Distance is not a robust way to determine outliers, as it uses
#' the means and covariances of all the data - including the outliers - to
#' determine individual difference scores. Minimum Covariance Determinant
#' calculates the mean and covariance matrix based on the most central subset of
#' the data (by default, 66\%), before computing the Mahalanobis Distance. This
#' is deemed to be a more robust method of identifying and removing outliers
#' than regular Mahalanobis distance.
#'
#'  - **Invariant Coordinate Selection (ICS)**:
#'  The outlier are detected using ICS, which by default uses an alpha threshold
#'  of 0.025 (corresponding to the 2.5\% most extreme observations) as a cut-off
#'  value for outliers classification. Refer to the help-file of
#'  `ICSOutlier::ics.outlier()` to get more details about this procedure.
#'  Note that `method = "ics"` requires both \pkg{ICS} and \pkg{ICSOutlier}
#'  to be installed, and that it takes some time to compute the results.
#'
#'  - **OPTICS**:
#'  The Ordering Points To Identify the Clustering Structure (OPTICS) algorithm
#'  (Ankerst et al., 1999) is using similar concepts to DBSCAN (an unsupervised
#'  clustering technique that can be used for outliers detection). The threshold
#'  argument is passed as `minPts`, which corresponds to the minimum size
#'  of a cluster. By default, this size is set at 2 times the number of columns
#'  (Sander et al., 1998). Compared to the other techniques, that will always
#'  detect several outliers (as these are usually defined as a percentage of
#'  extreme values), this algorithm functions in a different manner and won't
#'  always detect outliers. Note that `method = "optics"` requires the
#'  \pkg{dbscan} package to be installed, and that it takes some time to compute
#'  the results.
#'
#' @section Threshold specification:
#'
#' Default thresholds are currently specified as follows:
#'
#' ```
#' list(
#'   zscore = stats::qnorm(p = 1 - 0.001),
#'   zscore_robust = stats::qnorm(p = 1 - 0.001),
#'   iqr = 1.7,
#'   ci = 0.999,
#'   eti = 0.999,
#'   hdi = 0.999,
#'   bci = 0.999,
#'   cook = stats::qf(0.5, ncol(x), nrow(x) - ncol(x)),
#'   pareto = 0.7,
#'   mahalanobis = stats::qchisq(p = 1 - 0.001, df = ncol(x)),
#'   mahalanobis_robust = stats::qchisq(p = 1 - 0.001, df = ncol(x)),
#'   mcd = stats::qchisq(p = 1 - 0.001, df = ncol(x)),
#'   ics = 0.001,
#'   optics = 2 * ncol(x),
#'   lof = 0.001
#' )
#' ```
#'
#' @references
#' - Archimbaud, A., Nordhausen, K., and Ruiz-Gazen, A. (2018). ICS for
#' multivariate outlier detection with application to quality control.
#' Computational Statistics and Data Analysis, 128, 184-199.
#' \doi{10.1016/j.csda.2018.06.011}
#'
#' - Gnanadesikan, R., and Kettenring, J. R. (1972). Robust estimates, residuals,
#' and outlier detection with multiresponse data. Biometrics, 81-124.
#'
#' - Bollen, K. A., and Jackman, R. W. (1985). Regression diagnostics: An
#' expository treatment of outliers and influential cases. Sociological Methods
#' and Research, 13(4), 510-542.
#'
#' - Cabana, E., Lillo, R. E., and Laniado, H. (2019). Multivariate outlier
#' detection based on a robust Mahalanobis distance with shrinkage estimators.
#' arXiv preprint arXiv:1904.02596.
#'
#' - Cook, R. D. (1977). Detection of influential observation in linear
#' regression. Technometrics, 19(1), 15-18.
#'
#' - Iglewicz, B., and Hoaglin, D. C. (1993). How to detect and handle outliers
#' (Vol. 16). Asq Press.
#'
#' - Leys, C., Klein, O., Dominicy, Y., and Ley, C. (2018). Detecting
#' multivariate outliers: Use a robust variant of Mahalanobis distance. Journal
#' of Experimental Social Psychology, 74, 150-156.
#'
#' - Liu, F. T., Ting, K. M., and Zhou, Z. H. (2008, December). Isolation forest.
#' In 2008 Eighth IEEE International Conference on Data Mining (pp. 413-422).
#' IEEE.
#'
#' - Lüdecke, D., Ben-Shachar, M. S., Patil, I., Waggoner, P., and Makowski, D.
#' (2021). performance: An R package for assessment, comparison and testing of
#' statistical models. Journal of Open Source Software, 6(60), 3139.
#' \doi{10.21105/joss.03139}
#'
#' - Rousseeuw, P. J., and Van Zomeren, B. C. (1990). Unmasking multivariate
#' outliers and leverage points. Journal of the American Statistical
#' association, 85(411), 633-639.
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
#' # Find all observations beyond +/- 2 SD
#' check_outliers(data$mpg, method = "zscore", threshold = 2)
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
#'
#' # Using `as.data.frame()`, we can access more details!
#' outliers_info <- as.data.frame(outliers_list)
#' head(outliers_info)
#' outliers_info$Outlier # Including the probability of being an outlier
#'
#' # And we can be more stringent in our outliers removal process
#' filtered_data <- data[outliers_info$Outlier < 0.1, ]
#'
#' # We can run the function stratified by groups using `{dplyr}` package:
#' if (require("poorman")) {
#'   iris %>%
#'     group_by(Species) %>%
#'     check_outliers()
#' }
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
#'
#' if (require("see")) {
#'   plot(outliers_list)
#' }
#'
#' insight::get_data(model)[outliers_list, ] # Show outliers data
#'
#' if (require("MASS")) {
#'   check_outliers(model, method = c("mahalanobis", "mcd"))
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



# default ---------------------

#' @rdname check_outliers
#' @export
check_outliers.default <- function(x,
                                   method = c("cook", "pareto"),
                                   threshold = NULL,
                                   ID = NULL,
                                   ...) {
  # Check args
  if (all(method == "all")) {
    method <- c(
      "zscore_robust",
      "iqr",
      "ci",
      "cook",
      "pareto",
      "mahalanobis",
      "mahalanobis_robust",
      "mcd",
      "ics",
      "optics",
      "lof"
    )
  }

  method <- match.arg(
    method,
    c(
      "zscore",
      "zscore_robust",
      "iqr",
      "ci",
      "hdi",
      "eti",
      "bci",
      "cook",
      "pareto",
      "mahalanobis",
      "mahalanobis_robust",
      "mcd",
      "ics",
      "optics",
      "lof"
    ),
    several.ok = TRUE
  )

  # Remove non-numerics
  data <- as.data.frame(insight::get_modelmatrix(x))

  # Thresholds
  if (is.null(threshold)) {
    thresholds <- .check_outliers_thresholds(data)
  } else if (is.list(threshold)) {
    thresholds <- .check_outliers_thresholds(data)
    thresholds[[names(threshold)]] <- threshold[[names(threshold)]]
  } else {
    stop(insight::format_message(
      "The `threshold` argument must be NULL (for default values) or a list containing threshold values for desired methods (e.g., `list('mahalanobis' = 7)`)."
    ), call. = FALSE)
  }

  if (!missing(ID)) {
    warning(insight::format_message(
      "ID argument not supported for model objects of class `", class(x)[1], "`."
    ), call. = FALSE)
  }

  # Others
  if (!all(method %in% c("cook", "pareto"))) {
    out <- check_outliers(data, method, threshold)
    outlier_var <- attributes(out)$outlier_var
    outlier_count <- attributes(out)$outlier_count
    df <- attributes(out)$data
    df <- df[!names(df) %in% "Outlier"]
  } else {
    df <- data.frame(Row = seq_len(nrow(as.data.frame(data))))
    outlier_count <- list()
    outlier_var <- list()
  }

  # Cook
  if ("cook" %in% method &&
    insight::model_info(x)$is_bayesian == FALSE &&
    !inherits(x, "bife")) {
    data_cook <- .check_outliers_cook(
      x,
      threshold = thresholds$cook
    )$data_cook

    df <- datawizard::data_merge(list(df, data_cook),
      join = "full",
      by = c("Row")
    )

    count.table <- datawizard::data_filter(
      data_cook, "Outlier_Cook > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "Cook",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_Cook <- "(Multivariate)"
    }

    outlier_count$cook <- count.table

    if (!all(method %in% c("cook", "pareto"))) {
      outlier_count$all <- datawizard::data_merge(
        list(outlier_count$all, count.table),
        join = "full",
        by = c("Row")
      )
    } else {
      outlier_count$all <- count.table
    }
  } else {
    method <- method[!(method %in% "cook")]
  }

  # Pareto
  if ("pareto" %in% method && insight::model_info(x)$is_bayesian) {
    data_pareto <- .check_outliers_pareto(
      x,
      threshold = thresholds$pareto
    )$data_pareto

    df <- datawizard::data_merge(list(df, data_pareto),
      join = "full",
      by = c("Row")
    )

    count.table <- datawizard::data_filter(
      data_pareto, "Outlier_Pareto > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "Pareto",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_Pareto <- "(Multivariate)"
    }

    outlier_count$pareto <- count.table

    if (!all(method %in% c("cook", "pareto"))) {
      outlier_count$all <- datawizard::data_merge(
        list(outlier_count$all, count.table),
        join = "full",
        by = c("Row")
      )
    } else {
      outlier_count$all <- count.table
    }
  } else {
    method <- method[!(method %in% "pareto")]
  }

  outlier_count$all <- datawizard::convert_na_to(outlier_count$all,
    replace_num = 0,
    replace_char = "0",
    replace_fac = 0
  )

  num.df <- outlier_count$all[!names(outlier_count$all) %in% c("Row", ID)]
  if (isTRUE(nrow(num.df) > 0)) {
    num.df <- datawizard::change_code(
      num.df,
      recode = list(`2` = "(Multivariate)")
    )
    num.df <- as.data.frame(lapply(num.df, as.numeric))

    outlier_count$all$max <- apply(num.df, 1, max)
    outlier_count$all <- datawizard::data_filter(
      outlier_count$all,
      max >= 2
    )
    outlier_count$all <- datawizard::data_remove(
      outlier_count$all,
      "max"
    )
  }
  row.names(outlier_count$all) <- NULL

  thresholds <- thresholds[names(thresholds) %in% method]

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
  attr(outlier, "variables") <- "(Whole model)"
  attr(outlier, "raw_data") <- data
  attr(outlier, "outlier_var") <- outlier_var
  attr(outlier, "outlier_count") <- outlier_count

  outlier
}



# Methods -----------------------------------------------------------------

#' @export
as.data.frame.check_outliers <- function(x, ...) {
  attributes(x)$data
}

#' @export
as.numeric.check_outliers <- function(x, ...) {
  attributes(x)$data$Outlier
}

#' @export
print.check_outliers <- function(x, ...) {
  outliers <- which(x)

  method <- attr(x, "method")

  thresholds <- lapply(attr(x, "threshold"), round, 2)

  method.thresholds <- data.frame(
    method = method,
    thresholds = unlist(thresholds)
  )
  method.thresholds <- paste0(method.thresholds$method, " (",
    method.thresholds$thresholds, ")",
    collapse = ", "
  )

  method.univariate <- c(
    "zscore", "zscore_robust", "iqr", "ci",
    "eti", "hdi", "bci"
  )

  vars <- paste(attr(x, "variables"), collapse = ", ")
  vars.outliers <- attr(x, "outlier_var")

  var.plural <- ifelse(length(attr(x, "variables")) > 1,
    "variables", "variable"
  )
  method.plural <- ifelse(length(thresholds) > 1,
    "methods and thresholds",
    "method and threshold"
  )
  long_dash <- paste0("\n", paste0(rep("-", 77), collapse = ""), "\n")
  if (length(outliers) > 1) {
    outlier.plural <- "outliers"
    case.plural <- "cases"
  } else {
    outlier.plural <- "outlier"
    case.plural <- "case"
  }

  if (length(outliers) >= 1) {
    outlier.count <- attr(x, "outlier_count")
    o <- paste0(outliers, collapse = ", ")
    insight::print_color(insight::format_message(
      sprintf(
        "%i %s detected: %s %s.", length(outliers),
        outlier.plural, case.plural, o
      ),
      sprintf(
        "- Based on the following %s: %s.",
        method.plural, method.thresholds
      ),
      sprintf("- For %s: %s.\n", var.plural, vars),
      indent = ""
    ), color = "yellow")

    if (length(method) > 1) {
      insight::print_color(
        c(
          "\nNote: Outliers were classified as such by",
          "at least half of the selected methods. \n"
        ), "yellow"
      )
    }

    if (isTRUE(nrow(outlier.count$all) > 0) || isTRUE(attributes(x)$grouped)) {
      if (length(method) > 1 || all(method %in% method.univariate)) {
        cat(long_dash,
          "The following observations were considered outliers ",
          "for two or more variables \n",
          "by at least one of the selected methods: \n\n",
          sep = ""
        )
        ifelse(isTRUE(attributes(x)$grouped),
          print(lapply(outlier.count, function(x) x$all)),
          print(outlier.count$all)
        )
      }
    }

    if (length(method) == 1 && all(method %in% method.univariate)) {
      cat(long_dash, "Outliers per variable (", method,
        "): \n\n",
        sep = ""
      )
      ifelse(isTRUE(attributes(x)$grouped),
        print(vars.outliers),
        print(vars.outliers[[1]])
      )
    }
  } else {
    insight::print_color(
      sprintf("OK: No outliers detected.
- Based on the following %s: %s.
- For %s: %s\n\n", method.plural, method.thresholds, var.plural, vars),
      "green"
    )
  }
  invisible(x)
}

#' @export
plot.check_outliers <- function(x, ...) {
  insight::check_if_installed("see", "to plot outliers")
  NextMethod()
}



# other classes -------------------------

#' @rdname check_outliers
#' @export
check_outliers.numeric <- function(x,
                                   method = "zscore_robust",
                                   threshold = NULL,
                                   ...) {
  x <- as.data.frame(x)
  names(x) <- datawizard::text_remove(sys.call()[2], "()")
  check_outliers(x,
    method = method,
    threshold = threshold,
    ...
  )
}



#' @rdname check_outliers
#' @export
check_outliers.data.frame <- function(x,
                                      method = "mahalanobis",
                                      threshold = NULL,
                                      ID = NULL,
                                      ...) {
  # Preserve ID column if desired
  ID.names <- switch(!is.null(ID),
    x[ID]
  )

  # Remove non-numerics
  data <- x
  x <- x[, sapply(x, is.numeric), drop = FALSE]

  # Check args
  if (all(method == "all")) {
    method <- c(
      "zscore_robust", "iqr", "ci", "cook", "pareto", "mahalanobis",
      "mahalanobis_robust", "mcd", "ics", "optics", "lof"
    )
  }
  method <- match.arg(method, c(
    "zscore", "zscore_robust", "iqr", "ci", "hdi",
    "eti", "bci", "cook", "pareto", "mahalanobis",
    "mahalanobis_robust", "mcd", "ics", "optics",
    "lof"
  ), several.ok = TRUE)

  # Thresholds
  if (is.null(threshold)) {
    thresholds <- .check_outliers_thresholds(x)
  } else if (is.list(threshold)) {
    thresholds <- .check_outliers_thresholds(x)
    thresholds[[names(threshold)]] <- threshold[[names(threshold)]]
  } else if (is.numeric(threshold)) {
    thresholds <- .check_outliers_thresholds(x)
    thresholds <- lapply(thresholds, function(x) threshold)
  } else {
    stop(insight::format_message(
      "The `threshold` argument must be NULL (for default values) or a list containing threshold values for desired methods (e.g., `list('mahalanobis' = 7)`)."
    ), call. = FALSE)
  }

  thresholds <- thresholds[names(thresholds) %in% method]

  # Clean up per-variable list of outliers
  process_outlier_list <- function(outlier.list, Outlier_method) {
    outlier.list <- lapply(outlier.list, "[[", 1)
    outlier.list <- lapply(outlier.list, function(x) {
      x[x[[Outlier_method]] >= 0.5, ]
    })
    outlier.list <- outlier.list[lapply(outlier.list, nrow) > 0]
    outlier.list <- lapply(outlier.list, datawizard::data_remove,
      Outlier_method,
      as_data_frame = TRUE
    )
    outlier.list
  }

  # Count table of repeated outliers (for several variables)
  count_outlier_table <- function(outlier.list) {
    count.table <- do.call(rbind, outlier.list)
    name.method <- grep("Distance_", names(count.table), value = TRUE)
    name.method <- paste0("n_", gsub("Distance_", "", name.method))
    if (isTRUE(nrow(count.table) > 0)) {
      count.values <- rle(sort(count.table$Row))
      count.table <- data.frame(Row = count.values$values)
      if (!is.null(ID)) {
        count.table[ID] <- ID.names[count.table$Row, ]
      }
      count.table <- cbind(count.table, val = count.values$lengths)
      names(count.table)[names(count.table) == "val"] <- name.method
      count.table <- count.table[order(-count.table[[name.method]]), ]
    }
    count.table
  }

  # Preparation
  out <- list()
  outlier_var <- list()
  outlier_count <- list()

  # Z-score
  if ("zscore" %in% method) {
    if (thresholds$zscore < 1) {
      stop(insight::format_message(
        "The `threshold` argument must be one or greater for method `zscore`."
      ), call. = FALSE)
    }

    out <- c(out, .check_outliers_zscore(
      x,
      threshold = thresholds$zscore,
      robust = FALSE,
      method = "max",
      ID.names = ID.names
    ))

    # Outliers per variable
    zscore.var <- lapply(x, function(x) {
      .check_outliers_zscore(
        x,
        threshold = thresholds$zscore,
        robust = FALSE,
        method = "max",
        ID.names = ID.names
      )
    })

    outlier_var$zscore <- process_outlier_list(zscore.var, "Outlier_Zscore")
    outlier_count$zscore <- count_outlier_table(outlier_var$zscore)
  }

  if ("zscore_robust" %in% method) {
    out <- c(out, .check_outliers_zscore(
      x,
      threshold = thresholds$zscore_robust,
      robust = TRUE,
      method = "max",
      ID.names = ID.names
    ))

    # Outliers per variable
    zscore_robust.var <- lapply(x, function(x) {
      .check_outliers_zscore(
        x,
        threshold = thresholds$zscore_robust,
        robust = TRUE,
        method = "max",
        ID.names = ID.names
      )
    })

    outlier_var$zscore_robust <- process_outlier_list(
      zscore_robust.var, "Outlier_Zscore_robust"
    )
    outlier_count$zscore_robust <- count_outlier_table(
      outlier_var$zscore_robust
    )
  }

  # IQR
  if ("iqr" %in% method) {
    out <- c(out, .check_outliers_iqr(
      x,
      threshold = thresholds$iqr,
      method = "tukey",
      ID.names = ID.names
    ))

    # Outliers per variable
    iqr.var <- lapply(x, function(x) {
      y <- as.data.frame(x)
      .check_outliers_iqr(
        y,
        threshold = thresholds$iqr,
        method = "tukey",
        ID.names = ID.names
      )
    })

    outlier_var$iqr <- process_outlier_list(iqr.var, "Outlier_IQR")
    outlier_count$iqr <- count_outlier_table(outlier_var$iqr)
  }

  # CI
  if (any(c("ci", "hdi", "eti", "bci") %in% method)) {
    for (i in method[method %in% c("ci", "hdi", "eti", "bci")]) {
      out <- c(out, .check_outliers_ci(
        x,
        threshold = thresholds[i],
        method = i,
        ID.names = ID.names
      ))

      # Outliers per variable
      loop.var <- lapply(x, function(x) {
        y <- as.data.frame(x)
        .check_outliers_ci(
          y,
          threshold = thresholds[i],
          method = i,
          ID.names = ID.names
        )
      })

      outlier_var[[i]] <- process_outlier_list(
        loop.var, paste0("Outlier_", i)
      )
      outlier_count[[i]] <- count_outlier_table(outlier_var[[i]])
    }
  }

  # Mahalanobis
  if ("mahalanobis" %in% method) {
    out <- c(out, .check_outliers_mahalanobis(
      x,
      threshold = thresholds$mahalanobis,
      ID.names = ID.names,
      ...
    ))

    count.table <- datawizard::data_filter(
      out$data_mahalanobis, "Outlier_Mahalanobis > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "Mahalanobis",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_Mahalanobis <- "(Multivariate)"
    }

    outlier_count$mahalanobis <- count.table
  }

  # Robust Mahalanobis
  if ("mahalanobis_robust" %in% method) {
    out <- c(out, .check_outliers_mahalanobis_robust(
      x,
      threshold = thresholds$mahalanobis_robust,
      ID.names = ID.names
    ))

    count.table <- datawizard::data_filter(
      out$data_mahalanobis_robust, "Outlier_Mahalanobis_robust > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "Mahalanobis",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_Mahalanobis_robust <- "(Multivariate)"
    }

    outlier_count$mahalanobis_robust <- count.table
  }

  # MCD
  if ("mcd" %in% method) {
    out <- c(out, .check_outliers_mcd(
      x,
      threshold = thresholds$mcd,
      percentage_central = .66,
      ID.names = ID.names
    ))

    count.table <- datawizard::data_filter(
      out$data_mcd, "Outlier_MCD > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "MCD",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_MCD <- "(Multivariate)"
    }

    outlier_count$mcd <- count.table
  }

  # ICS
  if ("ics" %in% method) {
    out <- c(out, .check_outliers_ics(
      x,
      threshold = thresholds$ics,
      ID.names = ID.names
    ))

    count.table <- datawizard::data_filter(
      out$data_ics, "Outlier_ICS > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "ICS",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_ICS <- "(Multivariate)"
    }

    outlier_count$ics <- count.table
  }

  # OPTICS
  if ("optics" %in% method) {
    out <- c(out, .check_outliers_optics(
      x,
      threshold = thresholds$optics,
      ID.names = ID.names
    ))

    count.table <- datawizard::data_filter(
      out$data_optics, "Outlier_OPTICS > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "OPTICS",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_OPTICS <- "(Multivariate)"
    }

    outlier_count$optics <- count.table
  }

  # Isolation Forest
  # if ("iforest" %in% method) {
  #   out <- c(out, .check_outliers_iforest(x, threshold = thresholds$iforest))
  # }

  # Local Outlier Factor
  if ("lof" %in% method) {
    out <- c(out, .check_outliers_lof(
      x,
      threshold = thresholds$lof,
      ID.names = ID.names
    ))

    count.table <- datawizard::data_filter(
      out$data_lof, "Outlier_LOF > 0.5"
    )

    count.table <- datawizard::data_remove(
      count.table, "LOF",
      regex = TRUE, as_data_frame = TRUE
    )

    if (nrow(count.table) >= 1) {
      count.table$n_LOF <- "(Multivariate)"
    }

    outlier_count$lof <- count.table
  }

  # Combine outlier data
  df <- out[sapply(out, is.data.frame)]
  if (length(df) > 1 && !is.null(ID)) {
    df <- datawizard::data_merge(df, by = c("Row", ID))
  } else if (length(df) > 1) {
    df <- datawizard::data_merge(df, by = "Row")
  } else {
    df <- df[[1]]
  }

  # Composite outlier score
  df$Outlier <- rowMeans(df[grepl("Outlier_", names(df))])

  # Out
  outlier <- df$Outlier > 0.5

  # Combine outlier frequency table
  if (length(outlier_count) > 1 && !is.null(ID)) {
    outlier_count$all <- datawizard::data_merge(outlier_count,
      join = "full",
      by = c("Row", ID)
    )
  } else if (length(outlier_count) > 1) {
    outlier_count$all <- datawizard::data_merge(outlier_count,
      join = "full",
      by = c("Row")
    )
  } else if (length(outlier_count) == 1) {
    outlier_count$all <- outlier_count[[1]]
  } else {
    outlier_count$all <- data.frame()
  }
  outlier_count$all <- datawizard::convert_na_to(outlier_count$all,
    replace_num = 0,
    replace_char = "0",
    replace_fac = 0
  )

  outlier_count <- lapply(outlier_count, function(x) {
    num.df <- x[!names(x) %in% c("Row", ID)]
    if (isTRUE(nrow(num.df) >= 1)) {
      num.df <- datawizard::change_code(
        num.df,
        recode = list(`2` = "(Multivariate)")
      )
      num.df <- as.data.frame(lapply(num.df, as.numeric))
      x$max <- apply(num.df, 1, max)
      x <- datawizard::data_filter(x, max >= 2)
      x <- datawizard::data_remove(x, "max")
    }
  })

  row.names(outlier_count$all) <- NULL

  # Attributes
  class(outlier) <- c("check_outliers", "see_check_outliers", class(outlier))
  attr(outlier, "data") <- df
  attr(outlier, "threshold") <- thresholds
  attr(outlier, "method") <- method
  attr(outlier, "text_size") <- 3
  attr(outlier, "variables") <- names(x)
  attr(outlier, "raw_data") <- data
  attr(outlier, "outlier_var") <- outlier_var
  attr(outlier, "outlier_count") <- outlier_count
  outlier
}



#' @export
check_outliers.grouped_df <- function(x,
                                      method = "mahalanobis",
                                      threshold = NULL,
                                      ID = NULL,
                                      ...) {
  info <- attributes(x)

  # poorman < 0.8.0?
  if ("indices" %in% names(info)) {
    grps <- lapply(attr(x, "indices", exact = TRUE), function(x) x + 1)
  } else {
    grps <- attr(x, "groups", exact = TRUE)[[".rows"]]
  }

  # Initialize elements
  data <- data.frame()
  out <- c()
  thresholds <- list()
  outlier_var <- list()
  outlier_count <- list()

  # Loop through groups
  for (i in seq_along(grps)) {
    rows <- grps[[i]]
    subset <- check_outliers(as.data.frame(x[rows, ]),
      method = method,
      threshold = threshold,
      ID = ID,
      ...
    )
    data <- rbind(data, as.data.frame(subset))
    out <- c(out, subset)
    thresholds[[paste0("group_", i)]] <- attributes(subset)$threshold
    outlier_var[[i]] <- lapply(
      attributes(subset)$outlier_var, function(x) {
        lapply(x, function(y) {
          y$Row <- rows[which(seq(length(rows)) %in% y$Row)]
          y
        })
      }
    )
    outlier_count[[i]] <- lapply(
      attributes(subset)$outlier_count, function(y) {
        y$Row <- rows[which(seq(length(rows)) %in% y$Row)]
        y
      }
    )
  }

  outlier_var <- stats::setNames(outlier_var, info$groups[[1]])
  outlier_count <- stats::setNames(outlier_count, info$groups[[1]])

  groups <- lapply(seq_along(info$groups$.rows), function(x) {
    info$groups$.rows[[x]] <- rep(
      info$groups[[1]][x],
      length(info$groups$.rows[[x]])
    )
    info$groups$.rows[[x]] <- as.data.frame(info$groups$.rows[[x]])
  })

  data[names(info$groups)[1]] <- do.call(rbind, groups)
  data <- datawizard::data_relocate(data,
    select = names(info$groups)[1],
    after = "Row"
  )
  data$Row <- seq(nrow(data))

  class(out) <- c("check_outliers", "see_check_outliers", class(out))
  attr(out, "data") <- data
  attr(out, "method") <- method
  attr(out, "threshold") <- thresholds[[1]]
  attr(out, "text_size") <- 3
  attr(out, "variables") <- names(x[, sapply(x, is.numeric), drop = FALSE])
  attr(out, "raw_data") <- x
  attr(out, "outlier_var") <- outlier_var
  attr(out, "outlier_count") <- outlier_count
  attr(out, "grouped") <- TRUE
  out
}



#' @export
check_outliers.BFBayesFactor <- function(x,
                                         ID = NULL,
                                         ...) {
  if (!insight::is_model(x)) {
    stop("Collinearity only applicable to regression models.", call. = FALSE)
  }

  if (!missing(ID)) {
    warning(
      paste0("ID argument not supported for objects of class `", class(x)[1], "`."),
      call. = FALSE
    )
  }

  d <- insight::get_predictors(x)
  d[[insight::find_response(x)]] <- insight::get_response(x)

  check_outliers(d, ID = ID, ...)
}



#' @export
check_outliers.gls <- function(x,
                               method = "pareto",
                               threshold = NULL,
                               ID = NULL,
                               ...) {
  if (!missing(ID)) {
    warning(
      paste0("ID argument not supported for objects of class `", class(x)[1], "`."),
      call. = FALSE
    )
  }

  valid_methods <- c("zscore_robust", "iqr", "ci", "pareto", "optics")

  if (all(method == "all")) {
    method <- valid_methods
  }

  if (!method %in% valid_methods) {
    method <- "pareto"
  }

  check_outliers.default(x, method = method, threshold = threshold, ...)
}

#' @export
check_outliers.lme <- check_outliers.gls

#' @export
check_outliers.fixest <- check_outliers.gls

#' @export
check_outliers.geeglm <- check_outliers.gls




# Thresholds --------------------------------------------------------------

.check_outliers_thresholds <- function(x) {
  suppressWarnings(.check_outliers_thresholds_nowarn(x))
}

.check_outliers_thresholds_nowarn <- function(x) {
  zscore = stats::qnorm(p = 1 - 0.001)
  zscore_robust = stats::qnorm(p = 1 - 0.001)
  iqr = 1.7
  ci = 0.999
  eti = 0.999
  hdi = 0.999
  bci = 0.999
  cook = stats::qf(0.5, ncol(x), nrow(x) - ncol(x))
  pareto = 0.7
  mahalanobis = stats::qchisq(p = 1 - 0.001, df = ncol(x))
  mahalanobis_robust = stats::qchisq(p = 1 - 0.001, df = ncol(x))
  mcd = stats::qchisq(p = 1 - 0.001, df = ncol(x))
  ics = 0.001
  optics = 2 * ncol(x)
  lof = 0.001

  list(
    "zscore" = zscore,
    "zscore_robust" = zscore_robust,
    "iqr" = iqr,
    "ci" = ci,
    "hdi" = hdi,
    "eti" = eti,
    "bci" = bci,
    "cook" = cook,
    "pareto" = pareto,
    "mahalanobis" = mahalanobis,
    "mahalanobis_robust" = mahalanobis_robust,
    "mcd" = mcd,
    "ics" = ics,
    "optics" = optics,
    "lof" = lof
  )
}



# utilities --------------------

.check_outliers_zscore <- function(x,
                                   threshold = stats::qnorm(p = 1 - 0.001),
                                   robust = TRUE,
                                   method = "max",
                                   ID.names = NULL) {
  x <- as.data.frame(x)

  # Standardize
  if (robust == FALSE) {
    d <- abs(as.data.frame(sapply(x, function(x) (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE))))
  } else {
    d <- abs(as.data.frame(sapply(x, function(x) (x - stats::median(x, na.rm = TRUE)) / stats::mad(x, na.rm = TRUE))))
  }

  out <- data.frame(Row = seq_len(nrow(as.data.frame(d))))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  out$Distance_Zscore <- sapply(as.data.frame(t(d)), method,
    na.omit = TRUE, na.rm = TRUE
  )

  # Filter
  out$Outlier_Zscore <- as.numeric(out$Distance_Zscore > threshold)

  output <- list(
    "data_zscore" = out,
    "threshold_zscore" = threshold
  )

  if (isTRUE(robust)) {
    names(output) <- paste0(names(output), "_robust")
    output$data_zscore_robust <- datawizard::data_addsuffix(
      output$data_zscore_robust, "_robust",
      select = "Zscore$", regex = TRUE
    )
  }

  output
}



.check_outliers_iqr <- function(x,
                                threshold = 1.7,
                                method = "tukey",
                                ID.names = NULL) {

  d <- data.frame(Row = seq_len(nrow(as.data.frame(x))))
  Distance_IQR <- d

  for (col in seq_len(ncol(as.data.frame(x)))) {
    v <- x[, col]

    if (method == "tukey") {
      iqr <- stats::quantile(v, 0.75, na.rm = TRUE) - stats::quantile(v, 0.25, na.rm = TRUE)
    } else {
      iqr <- stats::IQR(v, na.rm = TRUE)
    }

    lower <- stats::quantile(v, 0.25, na.rm = TRUE) - (iqr * threshold)
    upper <- stats::quantile(v, 0.75, na.rm = TRUE) + (iqr * threshold)

    m.int <- stats::median(c(lower, upper), na.rm = TRUE)
    d2 <- abs(v - m.int)
    Distance_IQR[names(as.data.frame(x))[col]] <- d2/(iqr * threshold)

    d[names(as.data.frame(x))[col]] <- ifelse(v > upper, 1,
      ifelse(v < lower, 1, 0)
    )

  }

  out <- data.frame(Row = d$Row)
  d$Row <- NULL
  Distance_IQR$Row <- NULL

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  # out$Distance_IQR <- Distance_IQR

  out$Distance_IQR <- sapply(as.data.frame(t(Distance_IQR)), function(x) {
    ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
  })

  out$Outlier_IQR <- sapply(as.data.frame(t(d)), function(x) {
    ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
  })

  list(
    "data_iqr" = out,
    "threshold_iqr" = threshold
  )
}



.check_outliers_ci <- function(x,
                               threshold = 0.999,
                               method = "ci",
                               ID.names = NULL) {

  # Run through columns
  d <- data.frame(Row = seq_len(nrow(x)))
  Distance_CI <- d

  for (col in names(x)) {

    v <- x[, col]
    ci <- bayestestR::ci(v, ci = threshold, method = method)
    d[col] <- ifelse(x[[col]] > ci$CI_high |
                       x[[col]] < ci$CI_low, 1, 0)

    m.int <- stats::median(c(ci$CI_low, ci$CI_high), na.rm = TRUE)
    d2 <- abs(v - m.int)
    ci.range <- (ci$CI_high - ci$CI_low)/2

    Distance_CI[col] <- d2/ci.range

  }

  out.0 <- data.frame(Row = d$Row)
  d$Row <- NULL
  Distance_CI$Row <- NULL

  if (!is.null(ID.names)) {
    out.0 <- cbind(out.0, ID.names)
  }

  # Take the max
  out <- as.data.frame(apply(Distance_CI, 1, max, na.rm = TRUE))



    data.frame(x = as.numeric(sapply(as.data.frame(t(Distance_CI)),
    max,
    na.omit = TRUE,
    na.rm = TRUE
  )))
  names(out) <- paste0("Distance_", method)

  # Filter
  out[paste0("Outlier_", method)] <- sapply(
    as.data.frame(t(d)), function(x) {
      ifelse(all(is.na(x)), NA, max(x, na.rm = TRUE))
  })

  out <- cbind(out.0, out)

  output <- list(
    "data_" = out,
    "threshold_" = threshold
  )
  names(output) <- paste0(names(output), method)
  output
}



.check_outliers_cook <- function(x,
                                 threshold = NULL,
                                 ID.names = NULL) {
  # Compute
  d <- unname(stats::cooks.distance(x))
  out <- data.frame(Row = seq_along(d))
  out$Distance_Cook <- d

  # Filter
  out$Outlier_Cook <- as.numeric(out$Distance_Cook > threshold)

  list(
    "data_cook" = out,
    "threshold_cook" = threshold
  )
}



.check_outliers_pareto <- function(x, threshold = 0.7) {
  insight::check_if_installed("loo")

  # Compute
  d <- suppressWarnings(loo::pareto_k_values(loo::loo(x)))

  out <- data.frame(Row = seq_along(d))
  out$Distance_Pareto <- d

  # Filter
  out$Outlier_Pareto <- as.numeric(out$Distance_Pareto > threshold)

  list(
    "data_pareto" = out,
    "threshold_pareto" = threshold
  )
}



.check_outliers_mahalanobis <- function(x,
                                        threshold = stats::qchisq(
                                          p = 1 - 0.001, df = ncol(x)),
                                        ID.names = NULL,
                                        ...) {
  if (any(is.na(x)) || any(with(x, x == Inf))) {
    stop("Missing or infinite values are not allowed.", call. = FALSE)
  }

  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  # Compute
  out$Distance_Mahalanobis <- stats::mahalanobis(x, center = colMeans(x), cov = stats::cov(x), ...)

  # Filter
  out$Outlier_Mahalanobis <- as.numeric(out$Distance_Mahalanobis > threshold)

  list(
    "data_mahalanobis" = out,
    "threshold_mahalanobis" = threshold
  )
}



# Bigutils not yet fully available on CRAN
.check_outliers_mahalanobis_robust <- function(x,
                                               threshold = stats::qchisq(
                                                 p = 1 - 0.001, df = ncol(x)),
                                               ID.names = NULL) {
  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  insight::check_if_installed("bigutilsr")

  # Compute
  U <- svd(scale(x))$u
  out$Distance_Mahalanobis_robust <- bigutilsr::dist_ogk(U)

  # Filter
  out$Outlier_Mahalanobis_robust <- as.numeric(
    out$Distance_Mahalanobis_robust > threshold
  )

  list(
    "data_mahalanobis_robust" = out,
    "threshold_mahalanobis_robust" = threshold
  )
}



.check_outliers_mcd <- function(x,
                                threshold = stats::qchisq(
                                  p = 1 - 0.001, df = ncol(x)),
                                percentage_central = .50,
                                ID.names = NULL) {
  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  insight::check_if_installed("MASS")

  # Compute
  mcd <- MASS::cov.mcd(x, quantile.used = percentage_central * nrow(x))
  out$Distance_MCD <- stats::mahalanobis(x, center = mcd$center, cov = mcd$cov)

  # Filter
  out$Outlier_MCD <- as.numeric(out$Distance_MCD > threshold)

  list(
    "data_mcd" = out,
    "threshold_mcd" = threshold
  )
}



.check_outliers_ics <- function(x,
                                threshold = 0.001,
                                ID.names = NULL,
                                ...) {
  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

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
      ICSOutlier::ics.outlier(
        object = ics,
        ncores = n_cores,
        level.dist = threshold,
        ...
      )
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(outliers)) {
    if (ncol(x) == 1) {
      insight::print_color("At least two numeric predictors are required to detect outliers.\n", "red")
    } else {
      insight::print_color(sprintf("`check_outliers()` does not support models of class `%s`.\n", class(x)[1]), "red")
    }
  }

  # Get results
  cutoff <- outliers@ics.dist.cutoff
  out$Distance_ICS <- outliers@ics.distances
  out$Outlier_ICS <- as.numeric(out$Distance_ICS > cutoff)

  # Out
  list(
    "data_ics" = out,
    "threshold_ics" = threshold
  )
}



.check_outliers_optics <- function(x,
                                   threshold = NULL,
                                   ID.names = NULL) {
  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

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

  list(
    "data_optics" = out,
    "threshold_optics" = threshold
  )
}


# .check_outliers_iforest <- function(x, threshold = 0.025) {
#   out <- data.frame(Row = seq_len(nrow(x)))
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
#   out$Row <- NULL
#   list(
#     "data_iforest" = out,
#     "threshold_iforest" = threshold
#   )
# }



.check_outliers_lof <- function(x,
                                threshold = 0.001,
                                ID.names = NULL) {
  out <- data.frame(Row = seq_len(nrow(x)))

  if (!is.null(ID.names)) {
    out <- cbind(out, ID.names)
  }

  insight::check_if_installed("dbscan")

  # Compute
  out$Distance_LOF <- log(dbscan::lof(x, minPts = ncol(x)))

  # Threshold
  cutoff <- stats::qnorm(1 - threshold) * stats::sd(out$Distance_LOF)

  # Filter
  out$Outlier_LOF <- as.numeric(out$Distance_LOF > cutoff)

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
  message(paste0("`check_outliers()` does not yet support models of class `", class(x)[1], "`."))
  NULL
}

#' @export
check_outliers.lmrob <- check_outliers.glmmTMB

#' @export
check_outliers.glmrob <- check_outliers.glmmTMB
