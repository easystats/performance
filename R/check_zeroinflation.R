#' @title Check for zero-inflation in count models
#' @name check_zeroinflation
#'
#' @description `check_zeroinflation()` checks whether count models are
#' over- or underfitting zeros in the outcome.
#'
#' @param x Fitted model of class `merMod`, `glmmTMB`, `glm`, or `glm.nb`
#' (package **MASS**).
#' @param tolerance The tolerance for the ratio of observed and predicted
#' zeros to considered as over- or underfitting zeros. A ratio
#' between 1 +/- `tolerance` is considered as OK, while a ratio
#' beyond or below this threshold would indicate over- or underfitting.
#' @param alternative A character string specifying the alternative hypothesis.
#' Can be one of `"two.sided"`, `"less"`, or `"greater"`.
#' @param ... Arguments passed down to [`simulate_residuals()`]. This only applies
#' for models with zero-inflation component, or for models of class `glmmTMB`
#' from `nbinom1` or `nbinom2` family.
#'
#' @return A list with information about the amount of predicted and observed
#'  zeros in the outcome, as well as the ratio between these two values.
#'
#' @details If the amount of observed zeros is larger than the amount of
#' predicted zeros, the model is underfitting zeros, which indicates a
#' zero-inflation in the data. In such cases, it is recommended to use
#' negative binomial or zero-inflated models.
#'
#' In case of negative binomial models, models with zero-inflation component,
#' hurdle models, or Poisson mixed models, the results from `check_zeroinflation()`
#' are based on [`simulate_residuals()`], i.e.
#' `check_zeroinflation(simulate_residuals(model))` is internally called if necessary.
#'
#' @section Tests based on simulated residuals:
#' For certain models, resp. model from certain families, tests are based on
#' simulated residuals (see [`simulate_residuals()`]). These are usually more
#' accurate for testing such models than the traditionally used Pearson residuals.
#' However, when simulating from more complex models, such as mixed models or
#' models with zero-inflation, there are several important considerations.
#' Arguments specified in `...` are passed to [`simulate_residuals()`], which
#' relies on [`DHARMa::simulateResiduals()`] (and therefore, arguments in `...`
#' are passed further down to _DHARMa_). The defaults in DHARMa are set on the
#' most conservative option that works for all models. However, in many cases,
#' the help advises to use different settings in particular situations or for
#' particular models. It is recommended to read the 'Details' in
#' `?DHARMa::simulateResiduals` closely to understand the implications of the
#' simulation process and which arguments should be modified to get the most
#' accurate results.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examplesIf require("glmmTMB") && require("DHARMa")
#' data(Salamanders, package = "glmmTMB")
#' m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#' check_zeroinflation(m)
#'
#' # for models with zero-inflation component, it's better to carry out
#' # the check for zero-inflation using simulated residuals
#' m <- glmmTMB::glmmTMB(
#'   count ~ spp + mined,
#'   ziformula = ~ mined + spp,
#'   family = poisson,
#'   data = Salamanders
#' )
#' res <- simulate_residuals(m)
#' check_zeroinflation(res)
#' @export
check_zeroinflation <- function(x, ...) {
  UseMethod("check_zeroinflation")
}


#' @rdname check_zeroinflation
#' @export
check_zeroinflation.default <- function(x, tolerance = 0.05, ...) {
  # check if we have poisson
  model_info <- insight::model_info(x)
  if (!model_info$is_count) {
    insight::format_error("Model must be from Poisson-family.")
  }

  # get actual zero of response
  obs.zero <- sum(insight::get_response(x, verbose = FALSE) == 0L)

  if (obs.zero == 0) {
    insight::print_color("Model has no observed zeros in the response variable.\n", "red")
    return(NULL)
  }

  # model classes not supported in DHARMa
  not_supported <- c("fixest", "glmx")

  # for models with zero-inflation component, negative binomial families,
  # or Poisson mixed models, we use simulate_residuals()
  # Note: now including Poisson mixed models (see #595, #643)
  use_simulated <- model_info$is_zero_inflated ||
    model_info$is_negbin ||
    model_info$family == "genpois" ||
    (model_info$is_mixed && model_info$is_poisson)

  if (!inherits(x, not_supported) && use_simulated) {
    if (missing(tolerance)) {
      tolerance <- 0.1
    }
    return(check_zeroinflation(simulate_residuals(x, ...), tolerance = tolerance, ...))
  }

  # get predictions of outcome
  mu <- stats::fitted(x)
  # get predicted zero-counts
  pred.zero <- round(sum(stats::dpois(x = 0, lambda = mu)))

  # proportion
  structure(
    class = "check_zi",
    list(
      predicted.zeros = pred.zero,
      observed.zeros = obs.zero,
      ratio = pred.zero / obs.zero,
      tolerance = tolerance
    )
  )
}


#' @rdname check_zeroinflation
#' @export
check_zeroinflation.performance_simres <- function(
  x,
  tolerance = 0.1,
  alternative = "two.sided",
  ...
) {
  alternative <- insight::validate_argument(
    alternative,
    c("two.sided", "less", "greater")
  )

  # compute test results
  result <- .simres_statistics(
    x,
    statistic_fun = function(i) sum(i == 0),
    alternative = alternative
  )

  structure(
    class = "check_zi",
    list(
      predicted.zeros = round(mean(result$simulated)),
      observed.zeros = result$observed,
      ratio = mean(result$simulated) / result$observed,
      tolerance = tolerance,
      p.value = result$p
    )
  )
}

#' @export
check_zeroinflation.DHARMa <- check_zeroinflation.performance_simres


# methods ------------------

#' @export
print.check_zi <- function(x, ...) {
  insight::print_color("# Check for zero-inflation\n\n", "blue")
  cat(sprintf("   Observed zeros: %i\n", x$observed.zeros))
  cat(sprintf("  Predicted zeros: %i\n", x$predicted.zeros))
  cat(sprintf("            Ratio: %.2f\n\n", x$ratio))

  lower <- 1 - x$tolerance
  upper <- 1 + x$tolerance

  if (is.null(x$p.value)) {
    p_string <- ""
  } else {
    p_string <- paste0(" (", insight::format_p(x$p.value), ")")
  }

  if (x$ratio < lower) {
    message("Model is underfitting zeros (probable zero-inflation)", p_string, ".")
  } else if (x$ratio > upper) {
    message("Model is overfitting zeros", p_string, ".")
  } else {
    insight::format_alert(paste0(
      "Model seems ok, ratio of observed and predicted zeros is within the tolerance range",
      p_string,
      "."
    ))
  }

  invisible(x)
}
