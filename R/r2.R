#' @title Compute r-squared
#' @name r2
#'
#' @description to do.
#'
#' @param x A fitted model.
#' @param type Character vector, indicating the type of r-squared that is
#'    requested (like \code{"CoxSnell"} or \code{"adjusted"}).
#' @param ... Currently not used.
#'
#' @return The requested r-squared value.
#'
#' @details For linear models, the r-squared and adjusted r-squared value is returned,
#'          as provided by the \code{summary}-function.
#'          \cr \cr
#'          For generalized linear models, Cox & Snell's and Nagelkerke's
#'          pseudo r-squared values are returned.
#'
#' @examples
#' data(iris)
#'
#' m <- lm(Sepal.Width ~ Species, data = iris)
#' r2(m)
#'
#' @importFrom stats logLik update
#' @importFrom insight n_obs
#' @export
r2 <- function(x, ...) {
  UseMethod("r2")
}


#' @export
r2.default <- function(x, ...) {
  NULL
}


#' @rdname r2
#' @export
r2.glm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)

  ## TODO how to handle poisson?

  n <- insight::n_obs(x)
  CoxSnell <- (1 - exp((x$dev - x$null) / n))

  R2 <- switch(
    type,
    CoxSnell = CoxSnell,
    Nakelkerke = CoxSnell / (1 - exp(-x$null / n))
  )

  ## TODO should we return a named vector?

  names(R2) <- type
  R2
}


#' @export
r2.mlogit <- function(x, ...) {
  R2 <- as.vector(summary(x)$mfR2)
  names(R2) <- "McFadden"
  R2
}


#' @rdname r2
#' @export
r2.lm <- function(x, type = c("adjusted", "unadjusted"), ...) {
  type <- match.arg(type)
  R2 <- switch(
    type,
    adjusted = summary(x)$adj.r.squared,
    unadjusted = summary(x)$r.squared
  )

  names(R2) <- type
  R2
}


#' @export
r2.polr <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @rdname r2
#' @export
r2.clm2 <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, location = ~ 1, scale = ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.clm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.vglm <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  if (!(is.null(x@call$summ) && !identical(x@call$summ, 0)))
    stop("Can't get log-likelihood when `summ` is not zero.", call. = FALSE)

  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1))
  r2glm(x, L.base, type)
}


#' @export
r2.multinom <- function(x, type = c("Nagelkerke", "CoxSnell"), ...) {
  type <- match.arg(type)
  L.base <- stats::logLik(stats::update(x, ~ 1, trace = FALSE))
  r2glm(x, L.base, type)
}


#' @export
r2.plm <- function(x, type = c("adjusted", "unadjusted"), ...) {
  type <- match.arg(type)
  R2 <- switch(
    type,
    adjusted = summary(x)$r.squared[2],
    unadjusted = summary(x)$r.squared[1]
  )

  names(R2) <- type
  R2
}


r2glm <- function(x, L.base, type) {
  L.full <- stats::logLik(x)
  D.full <- -2 * L.full

  D.base <- -2 * L.base
  G2 <- -2 * (L.base - L.full)

  if (inherits(x, c("vglm", "clm2")))
    n <- insight::n_obs(x)
  else
    n <- attr(L.full, "nobs")

  R2 <- switch(
    type,
    CoxSnell = 1 - exp(-G2 / n),
    Nakelkerke = (1 - exp((D.full - D.base) / n)) / (1 - exp(-D.base / n))
  )

  ## TODO should we return a named vector?

  names(R2) <- type
  R2
}
