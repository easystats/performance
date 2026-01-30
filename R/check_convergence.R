#' @title Convergence test for mixed effects models
#' @name check_convergence
#'
#' @description `check_convergence()` provides an alternative convergence
#'   test for `merMod`-objects.
#'
#' @param model A `merMod` or `glmmTMB`-object.
#' @param tolerance Indicates up to which value the convergence result is
#'   accepted. The smaller `tolerance` is, the stricter the test will be.
#' @param x Deprecated, please use `model` instead.
#' @param ... Currently not used.
#'
#' @return `TRUE` if convergence is fine and `FALSE` if convergence
#'   is suspicious. Additionally, the convergence value is returned as attribute.
#'
#' @inheritSection insight::is_converged Convergence and log-likelihood
#'
#' @inheritSection insight::is_converged Inspect model convergence
#'
#' @inheritSection insight::is_converged Resolving convergence issues
#'
#' @inheritSection insight::is_converged Convergence versus Singularity
#'
#' @references
#' Bates, D., Mächler, M., Bolker, B., and Walker, S. (2015). Fitting Linear
#' Mixed-Effects Models Using lme4. Journal of Statistical Software, 67(1),
#' 1-48. \doi{10.18637/jss.v067.i01}
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examplesIf require("lme4") && require("glmmTMB")
#' data(cbpp, package = "lme4")
#' set.seed(1)
#' cbpp$x <- rnorm(nrow(cbpp))
#' cbpp$x2 <- runif(nrow(cbpp))
#'
#' model <- lme4::glmer(
#'   cbind(incidence, size - incidence) ~ period + x + x2 + (1 + x | herd),
#'   data = cbpp,
#'   family = binomial()
#' )
#'
#' check_convergence(model)
#'
#' \donttest{
#' model <- suppressWarnings(glmmTMB::glmmTMB(
#'   Sepal.Length ~ poly(Petal.Width, 4) * poly(Petal.Length, 4) +
#'     (1 + poly(Petal.Width, 4) | Species),
#'   data = iris
#' ))
#' check_convergence(model)
#' }
#' @export
check_convergence <- function(model = NULL, tolerance = 0.001, x = NULL, ...) {
  ## TODO remove deprecation warning later
  if (!is.null(x) && is.null(model)) {
    insight::format_warning(
      "Argument `x` is deprecated; please use `model` instead."
    )
    model <- x
  }
  .is_model_valid(model)
  out <- .safe(insight::is_converged(model, tolerance = tolerance, ...))
  if (is.null(out)) {
    insight::format_alert("Could not compute convergence information.")
    out <- NA
  }

  out
}
