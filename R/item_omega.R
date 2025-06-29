#' @title McDonald's Omega for Items or Scales
#' @name item_omega
#'
#' @description This function computes McDonald's omega reliability coefficients
#' alongside Cronbach's alpha for a set of items or a scale. It acts as a
#' wrapper for the [`psych::omega()`] function. The aim is to make McDonald's
#' omega readily available and present it with the widely-known Cronbach's
#' alpha, allowing for a more complete understanding of scale reliability. The
#' output includes various forms of omega (e.g., total, hierarchical) depending
#' on the factor structure specified.
#'
#' @param x A matrix or a data frame.
#' @param n Number of factors to extract.
#' @param rotation Rotation to be applied. Defaults to `"oblimin"`. Further
#' options are `"simplimax"`, `"Promax"`, `"cluster"` and `"target"`. See
#' `?psych::omega` for details.
#' @param factor_method The factoring method to be used. Passed to the `fm`
#' argument in `psych::omega()`. Defaults to `"minres"` (minimum residual).
#' Other options include `"ml"` (maximum likelihood), `"pa"` (principal axis),
#' etc.
#' @param n_obs Number of observations in the original data set if `x` is a
#' correlation matrix. Required to compute correct fit indices.
#' @param poly_cor Logical, if `TRUE`, polychoric correlations will be computed
#' (by passing `poly = TRUE` to `psych::omega()`). Defaults to `FALSE`.
#' @param verbose Logical, if `TRUE` (default), messages are printed.
#' @param ... Additional arguments passed to [`psych::omega()`].
#'
#' @return A data frames containing the reliability coefficients. Use `summary()`
#' or `parameters::model_parameters()` on the returned object to extract more
#' information.
#'
#' @details
#' `item_omega()` is a simple wrapper around `psych::omega()`, which returns
#' the reliability coefficients. The original object returned by `psych::omega()`
#' is saved as `$model` attribute. Further information are accessible via the
#' `summary()` and `parameters::model_parameters()` methods. Use `as.numeric()`
#' to return the reliability coefficients as (named) numeric vector. Detailed
#' information can be found in the docs of `?psych::omega`.
#'
#' @references
#' - Bland, J. M., & Altman, D. G. (1997). Statistics notes: Cronbach's alpha.
#'   BMJ, 314(7080), 572. \doi{10.1136/bmj.314.7080.572}
#' - Revelle, W., & Zinbarg, R. E. (2009). Coefficients alpha, beta, omega, and
#'   the glb: Comments on Sijtsma. Psychometrika, 74(1), 145–154.
#'   \doi{10.1007/s11336-008-9102-z}
#' - Zinbarg, R.E., Revelle, W., Yovel, I., & Li. W. (2005). Cronbach's Alpha,
#'   Revelle's Beta, McDonald's Omega: Their relations with each and two
#'   alternative conceptualizations of reliability. Psychometrika. 70, 123-133
#'
#' @examplesIf insight::check_if_installed("parameters", quietly = TRUE)
#' data(mtcars)
#' x <- mtcars[1:7]
#' result <- item_omega(x, n = 2)
#'
#' result
#'
#' as.numeric(result)
#'
#' summary(result)
#'
#' parameters::model_parameters(result)
#' @export
item_omega <- function(x, ...) {
  UseMethod("item_omega")
}


#' @rdname item_omega
#' @export
item_omega.data.frame <- function(x,
                                  n = "auto",
                                  rotation = "oblimin",
                                  factor_method = "minres",
                                  poly_cor = FALSE,
                                  verbose = TRUE,
                                  ...) {
  insight::check_if_installed(c("psych", "parameters"))

  # remove missings
  .data <- stats::na.omit(x)

  # we need at least two columns for Cronbach's Alpha
  if (is.null(ncol(.data)) || ncol(.data) < 2) {
    if (verbose) {
      insight::format_alert(
        "Too few columns in `x` to compute McDonald's Omega."
      )
    }
    return(NULL)
  }

  # determine number of factors
  n <- .get_n_factors(.data, n = n, rotation = rotation)

  # calculate omega
  model <- psych::omega(
    .data,
    nfactors = n,
    rotation = rotation,
    fm = factor_method,
    poly = poly_cor,
    key = NULL,
    plot = FALSE,
    ...
  )

  out <- data.frame(
    Statistic = c("Alpha", "G.6", "Omega (hierarchical)", "Omega (asymptotic H)", "Omega (total)"),
    Coefficient = c(model$alpha, model$G6, model$omega_h, model$omega.lim, model$omega.tot),
    stringsAsFactors = FALSE
  )

  attr(out, "model") <- model
  attr(out, "rotation") <- rotation
  attr(out, "factor_method") <- factor_method
  attr(out, "poly_cor") <- poly_cor
  attr(out, "n") <- n

  class(out) <- c("item_omega", "data.frame")
  out
}


#' @rdname item_omega
#' @export
item_omega.matrix <- function(x,
                              n = "auto",
                              rotation = "oblimin",
                              factor_method = "minres",
                              n_obs = NULL,
                              poly_cor = FALSE,
                              verbose = TRUE,
                              ...) {
  # validate n_obs
  if (!is.null(n_obs) && (!is.numeric(n_obs) || n_obs <= 0 || n_obs %% 1 != 0)) {
    insight::format_error(
      "`n_obs` must be either NULL or a positive integer. Please provide a valid value."
    )
  }

  # check if we have a square matrix. in this case, we assume that
  # the user wants to do a factor analysis on the correlation matrix
  if ((nrow(x) == ncol(x)) && is.null(n_obs)) {
    insight::format_error(
      "You provided a square matrix, which is assumed to be a correlation matrix. Please specify the number of observations with `n_obs`. If your matrix is not a correlation matrix, please provide a data frame instead."
    )
  }

  # the default n.obs argument in `psych::fa()` is `NA`, so we change
  # our default `NULL` to `NA` to avoid errors
  if (is.null(n_obs)) {
    n_obs <- NA
  }

  item_omega.data.frame(
    x,
    n = n,
    rotation = rotation,
    factor_method = factor_method,
    n.obs = n_obs,
    poly_cor = poly_cor,
    verbose = verbose,
    ...
  )
}


# methods ------------------------------------------------


#' @export
summary.item_omega <- function(object, ...) {
  insight::check_if_installed("parameters")
  summary(parameters::model_parameters(object, ...))
}


#' @export
print.item_omega <- function(x, ...) {
  cat(insight::export_table(x, caption = c("# Reliability Coefficients", "blue")))
}


#' @export
print_md.item_omega <- function(x, ...) {
  insight::export_table(x, caption = "Reliability Coefficients", format = "markdown", ...)
}


#' @export
print_html.item_omega <- function(x, ...) {
  insight::export_table(x, caption = "Reliability Coefficients", format = "html", ...)
}


#' @export
display.item_omega <- function(object, format = "markdown", ...) {
  if (format == "markdown") {
    print_md(object, ...)
  } else {
    print_html(object, ...)
  }
}


#' @export
as.double.item_omega <- function(x, ...) {
  stats::setNames(as.vector(x$Coefficient), x$Statistic)
}


# helper ------------------------------------------------


.get_n_factors <- function(x, n = NULL, rotation, ...) {
  insight::check_if_installed("parameters")
  if (is.null(n) || n == "auto") {
    n <- as.numeric(parameters::n_factors(x, type = "FA", rotation = rotation, ...))
  } else if (n == "all") {
    n <- ncol(x) - 1
  } else if (n >= ncol(x)) {
    n <- ncol(x)
  } else if (n < 1) {
    n <- 1
  }
  n
}
