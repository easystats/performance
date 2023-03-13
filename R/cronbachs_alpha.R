#' @title Cronbach's Alpha for Items or Scales
#' @name cronbachs_alpha
#'
#' @description Compute various measures of internal consistencies
#'    for tests or item-scales of questionnaires.
#'
#' @param x A matrix or a data frame.
#' @param verbose Toggle warnings.
#' @param ... Currently not used.
#'
#' @return The Cronbach's Alpha value for `x`.
#'
#' @details The Cronbach's Alpha value for `x`. A value closer to 1
#'    indicates greater internal consistency, where usually following
#'    rule of thumb is applied to interpret the results:
#'    \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.5 is unacceptable,
#'    0.5 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.6 is poor,
#'    0.6 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.7 is questionable,
#'    0.7 < \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}{alpha}} < 0.8 is acceptable,
#'    and everything > 0.8 is good or excellent.
#'
#' @references Bland, J. M., and Altman, D. G. Statistics notes: Cronbach's
#'   alpha. BMJ 1997;314:572. 10.1136/bmj.314.7080.572
#'
#' @examples
#' data(mtcars)
#' x <- mtcars[, c("cyl", "gear", "carb", "hp")]
#' cronbachs_alpha(x)
#' @export
cronbachs_alpha <- function(x, ...) {
  UseMethod("cronbachs_alpha")
}



#' @export
cronbachs_alpha.data.frame <- function(x, verbose = TRUE, ...) {
  # remove missings
  .data <- stats::na.omit(x)

  # we need at least two columns for Cronach's Alpha
  if (is.null(ncol(.data)) || ncol(.data) < 2) {
    if (verbose) {
      insight::format_warning("Too few columns in `x` to compute Cronbach's Alpha.")
    }
    return(NULL)
  }

  # Compute Cronbach's Alpha
  dim(.data)[2] / (dim(.data)[2] - 1) * (1 - sum(apply(.data, 2, stats::var)) / stats::var(rowSums(.data)))
}



#' @export
cronbachs_alpha.matrix <- function(x, verbose = TRUE, ...) {
  cronbachs_alpha(as.data.frame(x), verbose = verbose, ...)
}



#' @export
cronbachs_alpha.parameters_pca <- function(x, verbose = TRUE, ...) {
  # fetch data used for the PCA
  pca_data <- attributes(x)$dataset

  ## TODO: remove once parameters 0.18.3 or higher on CRAN
  # backward compatibility to parameters 0.18.2
  if (is.null(pca_data)) {
    pca_data <- attributes(x)$data_set
  }

  # if NULL, can we get from environment?
  if (is.null(pca_data)) {
    pca_data <- attr(x, "data")
    if (is.null(pca_data)) {
      if (verbose) {
        insight::format_warning("Could not find data frame that was used for the PCA.")
      }
      return(NULL)
    }
    pca_data <- get(pca_data, envir = parent.frame())
  }

  # get assignment of columns to extracted components, based on the max loading
  factor_assignment <- attributes(x)$closest_component

  # sort and get unique IDs so we only get data from relevant columns
  unique_factors <- sort(unique(factor_assignment))

  # apply cronbach's alpha for each component,
  # only for variables with max loading
  cronb <- sapply(unique_factors, function(i) {
    cronbachs_alpha(
      pca_data[, as.vector(x$Variable[factor_assignment == i]), drop = FALSE],
      verbose = verbose,
      ...
    )
  })

  names(cronb) <- paste0("PC", unique_factors)
  unlist(cronb)
}
