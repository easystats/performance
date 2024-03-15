#' @title Check for zero-inflation in count models
#' @name check_zeroinflation
#'
#' @description `check_zeroinflation()` checks whether count models are
#' over- or underfitting zeros in the outcome.
#'
#' @param x Fitted model of class `merMod`, `glmmTMB`, `glm`, or `glm.nb`
#' (package **MASS**).
#' @param tolerance The tolerance for the ratio of observed and predicted
#'  zeros to considered as over- or underfitting zeros. A ratio
#'  between 1 +/- `tolerance` is considered as OK, while a ratio
#'  beyond or below this threshold would indicate over- or underfitting.
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
#' or hurdle models, the results from `check_zeroinflation()` are likely to be
#' unreliable. In such cases, it is recommended to use `simulate_residuals()`
#' first, followed by `check_zeroinflation()` to check for zero-inflation,
#' e.g.: `check_zeroinflation(simulate_residuals(model))`.
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

  # for warning message
  model_name <- insight::safe_deparse(substitute(x))

  # get actual zero of response
  obs.zero <- sum(insight::get_response(x, verbose = FALSE) == 0L)

  if (obs.zero == 0) {
    insight::print_color("Model has no observed zeros in the response variable.\n", "red")
    return(NULL)
  }

  # for zero-inflated models, tell user to use simulated_residuals()
  if (model_info$is_zero_inflated) {
    insight::format_warning(paste0(
      "\nModel has zero-inflation component, returned results are likely to be unreliable. ",
      "Please use `check_zeroinflation(simulate_residuals(",
      model_name,
      "))` to check for zero-inflation."
    ))
  }

  # get predictions of outcome
  mu <- stats::fitted(x)

  # get overdispersion parameters
  if (model_info$is_negbin) {
    if (methods::is(x, "glmmTMB")) {
      theta <- stats::sigma(x)
    } else if (methods::is(x, "glmerMod")) {
      theta <- environment(x@resp$family$aic)[[".Theta"]]
    } else {
      theta <- x$theta
    }
  } else {
    theta <- NULL
  }

  # get predicted zero-counts
  if (!is.null(theta)) {
    pred.zero <- round(sum(stats::dnbinom(x = 0, size = theta, mu = mu)))
  } else {
    pred.zero <- round(sum(stats::dpois(x = 0, lambda = mu)))
  }

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
check_zeroinflation.performance_simres <- function(x,
                                                   tolerance = 0.05,
                                                   alternative = c("two.sided", "less", "greater"),
                                                   ...) {
  # match arguments
  alternative <- match.arg(alternative)
  # count observed and simulated zeros
  observed <- sum(x$observedResponse == 0)
  simulated <- apply(x$simulatedResponse, 2, function(i) sum(i == 0))
  # p is simply ratio of simulated zeros to observed zeros
  p <- switch(
    alternative,
    greater = mean(simulated >= observed),
    less = mean(simulated <= observed),
    min(min(mean(simulated <= observed), mean(simulated >= observed)) * 2, 1)
  )

  structure(
    class = "check_zi",
    list(
      predicted.zeros = round(mean(simulated)),
      observed.zeros = observed,
      ratio = mean(simulated) / observed,
      tolerance = tolerance,
      p.value = p
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

  if (!is.null(x$p.value)) {
    p_string <- paste0(" (", insight::format_p(x$p.value), ")")
  } else {
    p_string <- ""
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
