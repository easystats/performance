#' @title Posterior predictive checks
#' @name check_predictions
#'
#' @description Posterior predictive checks mean \dQuote{simulating replicated data
#'   under the fitted model and then comparing these to the observed data}
#'   \cite{(Gelman and Hill, 2007, p. 158)}. Posterior predictive checks
#'   can be used to \dQuote{look for systematic discrepancies between real and
#'   simulated data} \cite{(Gelman et al. 2014, p. 169)}.
#'
#'   \pkg{performance} provides posterior predictive check methods for a variety
#'   of frequentist models (e.g., `lm`, `merMod`, `glmmTMB`, ...). For Bayesian
#'   models, the model is passed to \code{\link[bayesplot:pp_check]{bayesplot::pp_check()}}.
#'
#' @param object A statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param check_range Logical, if `TRUE`, includes a plot with the minimum
#'   value of the original response against the minimum values of the replicated
#'   responses, and the same for the maximum value. This plot helps judging whether
#'   the variation in the original data is captured by the model or not
#'   (\cite{Gelman et al. 2020, pp.163}). The minimum and maximum values of `y` should
#'   be inside the range of the related minimum and maximum values of `yrep`.
#' @param re_formula Formula containing group-level effects (random effects) to
#'   be considered in the simulated data. If `NULL` (default), condition
#'   on all random effects. If `NA` or `~0`, condition on no random
#'   effects. See `simulate()` in **lme4**.
#' @param ... Passed down to `simulate()`.
#'
#' @return A data frame of simulated responses and the original response vector.
#'
#' @details An example how posterior predictive checks can also be used for model
#'   comparison is Figure 6 from \cite{Gabry et al. 2019, Figure 6}.
#'   \cr
#'   \if{html}{\cr \figure{pp_check.png}{options: width="90\%" alt="Posterior Predictive Check"} \cr}
#'   The model shown in the right panel (b) can simulate new data that are more
#'   similar to the observed outcome than the model in the left panel (a). Thus,
#'   model (b) is likely to be preferred over model (a).
#'
#' @note  Every model object that has a `simulate()`-method should work with
#'   `check_predictions()`. On R 3.6.0 and higher, if \pkg{bayesplot}
#'   (or a package that imports \pkg{bayesplot} such as \pkg{rstanarm} or \pkg{brms})
#'   is loaded, `pp_check()` is also available as an alias for `check_predictions()`.
#'
#' @references \itemize{
#'   \item Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., & Gelman, A. (2019). Visualization in Bayesian workflow. Journal of the Royal Statistical Society: Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
#'   \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'   \item Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., & Rubin, D. B. (2014). Bayesian data analysis. (Third edition). CRC Press.
#'   \item Gelman, A., Hill, J., & Vehtari, A. (2020). Regression and Other Stories. Cambridge University Press.
#' }
#'
#' @examples
#' library(performance)
#' model <- lm(mpg ~ disp, data = mtcars)
#' if (require("see") && getRversion() >= "3.5.0") {
#'   check_predictions(model)
#' }
#' @export
check_predictions <- function(object,
                              iterations = 50,
                              check_range = FALSE,
                              re_formula = NULL,
                              ...) {
  UseMethod("check_predictions")
}

#' @export
check_predictions.default <- function(object,
                                      iterations = 50,
                                      check_range = FALSE,
                                      re_formula = NULL,
                                      ...) {
  if (isTRUE(insight::model_info(object, verbose = FALSE)$is_bayesian) &&
      isFALSE(inherits(object, "BFBayesFactor"))) {
    insight::check_if_installed(
      "bayesplot",
      "to create posterior prediction plots for Stan models"
    )
    bayesplot::pp_check(object)
  } else {
    pp_check.lm(
      object,
      iterations = iterations,
      check_range = check_range,
      re_formula = re_formula,
      ...
    )
  }
}

#' @export
check_predictions.BFBayesFactor <- function(object,
                                            iterations = 50,
                                            check_range = FALSE,
                                            re_formula = NULL,
                                            ...) {
  everything_we_need <- .get_bfbf_predictions(object, iterations = iterations)

  y <- everything_we_need[["y"]]
  sig <- everything_we_need[["sigma"]]
  if (isTRUE(is.na(re_formula))) {
    yy <- everything_we_need[["y_pred_marginal"]]
  } else {
    if (!is.null(re_formula)) warning("re_formula can only be NULL or NA", call. = FALSE)
    yy <- everything_we_need[["y_pred"]]
  }

  yrep <- apply(yy, 2, function(mu) stats::rnorm(length(mu), mu, sig))
  yrep <- t(yrep)

  out <- as.data.frame(yrep)
  colnames(out) <- paste0("sim_", seq(ncol(out)))
  out$y <- y
  attr(out, "check_range") <- check_range
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}

pp_check.BFBayesFactor <- check_predictions.BFBayesFactor


# pp-check functions -------------------------------------

pp_check.lm <- function(object,
                        iterations = 50,
                        check_range = FALSE,
                        re_formula = NULL,
                        ...) {

  # if we have a matrix-response, continue here...
  if (grepl("^cbind\\((.*)\\)", insight::find_response(object, combine = TRUE))) {
    return(pp_check.glm(object, iterations, check_range, re_formula, ...))
  }

  # else, proceed as usual
  out <- tryCatch(
    {
      stats::simulate(object, nsim = iterations, re.form = re_formula, ...)
    },
    error = function(e) {
      NULL
    }
  )

  # glmmTMB returns column matrix for bernoulli
  if (inherits(object, "glmmTMB") && insight::model_info(object)$is_binomial) {
    out <- as.data.frame(lapply(out, function(i) {
      if (is.matrix(i)) {
        i[, 1]
      } else {
        i
      }
    }))
  }

  if (is.null(out)) {
    stop(insight::format_message(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'?", class(object)[1])), call. = FALSE)
  }

  # get response data, and response term, to check for transformations
  response <- insight::get_response(object)
  resp_string <- insight::find_terms(object)$response
  pattern <- "^(scale|exp|expm1|log|log1p|log10|log2|sqrt)"

  # check for transformed response, and backtransform simulations
  if (!is.null(resp_string) && grepl(paste0(pattern, "\\("), resp_string)) {
    out <- .backtransform_sims(out, resp_string)
  }

  out$y <- response

  attr(out, "check_range") <- check_range
  attr(out, "response_name") <- resp_string
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}


pp_check.glm <- function(object,
                         iterations = 50,
                         check_range = FALSE,
                         re_formula = NULL,
                         ...) {

  # if we have no matrix-response, continue here...
  if (!grepl("^cbind\\((.*)\\)", insight::find_response(object, combine = TRUE))) {
    return(pp_check.lm(object, iterations, check_range, re_formula, ...))
  }

  # else, process matrix response. for matrix response models, we compute
  # the ratio of successes and failures, because the plot cannot handle
  # matrix columns with separate success/failures in simulations.

  out <- tryCatch(
    {
      matrix_sim <- stats::simulate(object, nsim = iterations, re.form = re_formula, ...)
      as.data.frame(sapply(matrix_sim, function(i) i[, 1] / i[, 2], simplify = TRUE))
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out)) {
    stop(insight::format_message(sprintf("Could not simulate responses. Maybe there is no 'simulate()' for objects of class '%s'?", class(object)[1])), call. = FALSE)
  }

  # get response data, and response term
  response <- eval(.str2lang(insight::find_response(object)),
                   envir = insight::get_response(object))
  resp_string <- insight::find_terms(object)$response

  out$y <- response[, 1] / response[, 2]

  attr(out, "check_range") <- check_range
  attr(out, "response_name") <- resp_string
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}


# styler: off
pp_check.glmmTMB   <-
  pp_check.glm.nb  <-
  pp_check.lme     <-
  pp_check.merMod  <-
  pp_check.MixMod  <-
  pp_check.mle2    <-
  pp_check.negbin  <-
  pp_check.polr    <-
  pp_check.rma     <-
  pp_check.vlm     <-
  pp_check.wbm     <-
  pp_check.lm
# styler: on



#' @rawNamespace if (getRversion() >= "3.6.0") {
#'   S3method(bayesplot::pp_check, lm)
#'   S3method(bayesplot::pp_check, glm)
#'   S3method(bayesplot::pp_check, glmmTMB)
#'   S3method(bayesplot::pp_check, glm.nb)
#'   S3method(bayesplot::pp_check, merMod)
#'   S3method(bayesplot::pp_check, MixMod)
#'   S3method(bayesplot::pp_check, mle2)
#'   S3method(bayesplot::pp_check, negbin)
#'   S3method(bayesplot::pp_check, polr)
#'   S3method(bayesplot::pp_check, rma)
#'   S3method(bayesplot::pp_check, vlm)
#'   S3method(bayesplot::pp_check, wbm)
#'   S3method(bayesplot::pp_check, BFBayesFactor)
#' }



# aliases --------------------------

#' @rdname check_predictions
#' @export
posterior_predictive_check <- check_predictions

#' @rdname check_predictions
#' @export
check_posterior_predictions <- check_predictions




# methods -----------------------


#' @export
print.performance_pp_check <- function(x, verbose = TRUE, ...) {
  original <- x$y
  replicated <- x[which(names(x) != "y")]

  if (isTRUE(verbose)) {
    if (is.numeric(original)) {
      if (min(replicated) > min(original)) {
        insight::print_color(
          insight::format_message("Warning: Minimum value of original data is not included in the replicated data.", "Model may not capture the variation of the data."),
          "red"
        )
      }

      if (max(replicated) < max(original)) {
        insight::print_color(
          insight::format_message("Warning: Maximum value of original data is not included in the replicated data.", "Model may not capture the variation of the data."),
          "red"
        )
      }
    } else {
      missing_levs <- setdiff(original, unlist(replicated))
      if (length(missing_levs)) {
        insight::print_color(
          insight::format_message(
            paste0("Warning: Level",
                   ifelse(length(missing_levs) == 1, " ", "s "),
                   paste0("'", missing_levs, "'", collapse = ", "),
                   " from original data is not included in the replicated data."), "Model may not capture the variation of the data."),
          "red"
        )
      }
    }
  }

  if (requireNamespace("see", quietly = TRUE)) {
    NextMethod()
  }
  invisible(x)
}


#' @export
plot.performance_pp_check <- function(x, ...) {
  insight::check_if_installed("see", "to plot posterior predictive checks")

  NextMethod()
}



.backtransform_sims <- function(sims, resp_string) {
  if (grepl("log(log(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) exp(exp(i)))
  } else if (grepl("log(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) exp(i))
  } else if (grepl("log1p(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) expm1(i))
  } else if (grepl("log10(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) 10^i)
  } else if (grepl("log2(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) 2^i)
  } else if (grepl("sqrt(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) i^2)
  } else if (grepl("exp(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) log(i))
  } else if (grepl("expm1(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) log1p(i))
  }

  sims
}
