#' @title Posterior predictive checks
#' @name check_predictions
#'
#' @description Posterior predictive checks mean "simulating replicated data
#'   under the fitted model and then comparing these to the observed data"
#'   (_Gelman and Hill, 2007, p. 158_). Posterior predictive checks
#'   can be used to "look for systematic discrepancies between real and
#'   simulated data" (_Gelman et al. 2014, p. 169_).
#'
#'   **performance** provides posterior predictive check methods for a variety
#'   of frequentist models (e.g., `lm`, `merMod`, `glmmTMB`, ...). For Bayesian
#'   models, the model is passed to [`bayesplot::pp_check()`].
#'
#'   If `check_predictions()` doesn't work as expected, try setting
#'   `verbose = TRUE` to get hints about possible problems.
#'
#' @param object A statistical model.
#' @param iterations The number of draws to simulate/bootstrap.
#' @param check_range Logical, if `TRUE`, includes a plot with the minimum
#'   value of the original response against the minimum values of the replicated
#'   responses, and the same for the maximum value. This plot helps judging whether
#'   the variation in the original data is captured by the model or not
#'   (_Gelman et al. 2020, pp.163_). The minimum and maximum values of `y` should
#'   be inside the range of the related minimum and maximum values of `yrep`.
#' @param re_formula Formula containing group-level effects (random effects) to
#'   be considered in the simulated data. If `NULL` (default), condition
#'   on all random effects. If `NA` or `~0`, condition on no random
#'   effects. See `simulate()` in **lme4**.
#' @param bandwidth A character string indicating the smoothing bandwidth to
#'   be used. Unlike `stats::density()`, which used `"nrd0"` as default, the
#'   default used here is `"nrd"` (which seems to give more plausible results
#'   for non-Gaussian models). When problems with plotting occur, try to change
#'   to a different value.
#' @param type Plot type for the posterior predictive checks plot. Can be `"density"`,
#' `"discrete_dots"`, `"discrete_interval"` or `"discrete_both"` (the `discrete_*`
#' options are appropriate for models with discrete - binary, integer or ordinal
#' etc. - outcomes).
#' @param verbose Toggle warnings.
#' @param ... Passed down to `simulate()`.
#'
#' @return A data frame of simulated responses and the original response vector.
#'
#' @seealso [`simulate_residuals()`] and [`check_residuals()`]. See also
#' [`see::print.see_performance_pp_check()`] for options to customize the plot.
#'
#' @details An example how posterior predictive checks can also be used for model
#'   comparison is Figure 6 from _Gabry et al. 2019, Figure 6_.
#'
#'   \if{html}{\cr \figure{pp_check.png}{options: width="90\%" alt="Posterior Predictive Check"} \cr}
#'   The model shown in the right panel (b) can simulate new data that are more
#'   similar to the observed outcome than the model in the left panel (a). Thus,
#'   model (b) is likely to be preferred over model (a).
#'
#' @note Every model object that has a `simulate()`-method should work with
#' `check_predictions()`. On R 3.6.0 and higher, if **bayesplot** (or a
#' package that imports **bayesplot** such as **rstanarm** or **brms**)
#' is loaded, `pp_check()` is also available as an alias for `check_predictions()`.
#'
#' If `check_predictions()` doesn't work as expected, try setting `verbose = TRUE`
#' to get hints about possible problems.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @references
#' - Gabry, J., Simpson, D., Vehtari, A., Betancourt, M., and Gelman, A. (2019).
#'   Visualization in Bayesian workflow. Journal of the Royal Statistical Society:
#'   Series A (Statistics in Society), 182(2), 389–402. https://doi.org/10.1111/rssa.12378
#'
#' - Gelman, A., and Hill, J. (2007). Data analysis using regression and
#'   multilevel/hierarchical models. Cambridge; New York: Cambridge University Press.
#'
#' - Gelman, A., Carlin, J. B., Stern, H. S., Dunson, D. B., Vehtari, A., and
#'   Rubin, D. B. (2014). Bayesian data analysis. (Third edition). CRC Press.
#'
#' - Gelman, A., Hill, J., and Vehtari, A. (2020). Regression and Other Stories.
#'   Cambridge University Press.
#'
#' @examplesIf insight::check_if_installed("see", minimum_version = "0.9.1", quietly = TRUE)
#' # linear model
#' model <- lm(mpg ~ disp, data = mtcars)
#' check_predictions(model)
#'
#' # discrete/integer outcome
#' set.seed(99)
#' d <- iris
#' d$skewed <- rpois(150, 1)
#' model <- glm(
#'   skewed ~ Species + Petal.Length + Petal.Width,
#'   family = poisson(),
#'   data = d
#' )
#' check_predictions(model, type = "discrete_both")
#'
#' @export
check_predictions <- function(object, ...) {
  UseMethod("check_predictions")
}

#' @rdname check_predictions
#' @export
check_predictions.default <- function(object,
                                      iterations = 50,
                                      check_range = FALSE,
                                      re_formula = NULL,
                                      bandwidth = "nrd",
                                      type = "density",
                                      verbose = TRUE,
                                      ...) {
  .is_model_valid(object)
  # check_predictions() can't handle exotic formula notation
  if (verbose) {
    insight::formula_ok(
      object,
      action = "error",
      prefix_msg = "Posterior predictive checks failed due to an incompatible model formula." # nolint
    )
  }

  # retrieve model information
  minfo <- insight::model_info(object, verbose = FALSE)

  # try to find sensible default for "type" argument
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical || minfo$is_multinomial) # nolint
  if (missing(type) && suggest_dots) {
    type <- "discrete_interval"
  }

  # args
  type <- insight::validate_argument(
    type,
    c("density", "discrete_dots", "discrete_interval", "discrete_both")
  )

  pp_check.lm(
    object,
    iterations = iterations,
    check_range = check_range,
    re_formula = re_formula,
    bandwidth = bandwidth,
    type = type,
    verbose = verbose,
    model_info = minfo,
    ...
  )
}


#' @export
check_predictions.stanreg <- function(object,
                                      iterations = 50,
                                      check_range = FALSE,
                                      re_formula = NULL,
                                      bandwidth = "nrd",
                                      type = "density",
                                      verbose = TRUE,
                                      ...) {
  # retrieve model information
  minfo <- insight::model_info(object, verbose = FALSE)

  # try to find sensible default for "type" argument
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical || minfo$is_multinomial) # nolint
  if (missing(type) && suggest_dots) {
    type <- "discrete_interval"
  }

  # args
  type <- insight::validate_argument(
    type,
    c("density", "discrete_dots", "discrete_interval", "discrete_both")
  )

  # convert to type-argument for pp_check
  pp_type <- switch(type,
    density = "dens",
    "bars"
  )

  insight::check_if_installed(
    "bayesplot",
    "to create posterior prediction plots for Stan models"
  )

  # for plotting
  resp_string <- insight::find_terms(object)$response

  if (inherits(object, "brmsfit")) {
    out <- as.data.frame(bayesplot::pp_check(object, type = pp_type, ndraws = iterations, ...)$data)
  } else {
    out <- as.data.frame(bayesplot::pp_check(object, type = pp_type, nreps = iterations, ...)$data)
  }

  # bring data into shape, like we have for other models with `check_predictions()`
  if (pp_type == "dens") {
    d_filter <- out[!out$is_y, ]
    d_filter <- datawizard::data_to_wide(
      d_filter,
      id_cols = "y_id",
      values_from = "value",
      names_from = "rep_id"
    )
    d_filter$y_id <- NULL
    colnames(d_filter) <- paste0("sim_", colnames(d_filter))
    d_filter$y <- out$value[out$is_y]
    out <- d_filter
  } else {
    colnames(out) <- c("x", "y", "CI_low", "Mean", "CI_high")
    # to long, for plotting
    out <- datawizard::data_to_long(
      out,
      select = c("y", "Mean"),
      names_to = "Group",
      values_to = "Count"
    )
  }

  attr(out, "is_stan") <- TRUE
  attr(out, "check_range") <- check_range
  attr(out, "response_name") <- resp_string
  attr(out, "bandwidth") <- bandwidth
  attr(out, "model_info") <- minfo
  attr(out, "type") <- type
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}

#' @export
check_predictions.brmsfit <- check_predictions.stanreg


#' @export
check_predictions.BFBayesFactor <- function(object,
                                            iterations = 50,
                                            check_range = FALSE,
                                            re_formula = NULL,
                                            bandwidth = "nrd",
                                            verbose = TRUE,
                                            ...) {
  everything_we_need <- .get_bfbf_predictions(object, iterations = iterations)

  y <- everything_we_need[["y"]]
  sig <- everything_we_need[["sigma"]]
  if (isTRUE(is.na(re_formula))) {
    yy <- everything_we_need[["y_pred_marginal"]]
  } else {
    if (!is.null(re_formula)) {
      insight::format_warning("`re_formula` can only be `NULL` or `NA`.")
    }
    yy <- everything_we_need[["y_pred"]]
  }

  yrep <- apply(yy, 2, function(mu) stats::rnorm(length(mu), mu, sig))
  yrep <- t(yrep)

  out <- as.data.frame(yrep)
  colnames(out) <- paste0("sim_", seq_len(ncol(out)))
  out$y <- y
  attr(out, "bandwidth") <- bandwidth
  attr(out, "check_range") <- check_range
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}

pp_check.BFBayesFactor <- check_predictions.BFBayesFactor


#' @export
check_predictions.lme <- function(object, ...) {
  insight::format_error("`check_predictions()` does currently not work for models of class `lme`.")
}


# pp-check functions -------------------------------------

pp_check.lm <- function(object,
                        iterations = 50,
                        check_range = FALSE,
                        re_formula = NULL,
                        bandwidth = "nrd",
                        type = "density",
                        verbose = TRUE,
                        model_info = NULL,
                        ...) {
  # we need the formula and the response values to check for matrix responses
  # or proportions in binomial models
  model_response <- insight::find_response(object, combine = TRUE)
  response_values <- insight::get_response(object)

  # if we have a matrix-response, continue here...
  if (grepl("^cbind\\((.*)\\)", model_response) || is.matrix(response_values)) {
    return(pp_check.glm(object, iterations, check_range, re_formula, bandwidth, type, verbose, model_info, ...))
  }

  # else, proceed as usual
  out <- .safe(stats::simulate(object, nsim = iterations, re.form = re_formula, ...))

  # validation check, for mixed models, where re.form = NULL (default) might fail
  out <- .check_re_formula(out, object, iterations, re_formula, verbose, ...)

  # save information about model
  if (is.null(model_info)) {
    minfo <- insight::model_info(object)
  } else {
    minfo <- model_info
  }

  # glmmTMB returns column matrix for bernoulli
  if (inherits(object, "glmmTMB") && minfo$is_binomial && !is.null(out)) {
    # check if we have a response defined as proportions. in that case, we
    # have to handle the results from `simulate()` differently. We have a
    # proportion response if the formula contains a `/` in the response,
    # or if the response is a non-integer numeric vector between 0 and 1.
    proportion_response <- grepl("/", model_response, fixed = TRUE) ||
      (!is.null(response_values) && !all(.is_integer(response_values)))

    out <- as.data.frame(lapply(out, function(i) {
      if (is.matrix(i)) {
        # do we have a response defined as proportions?
        if (proportion_response) {
          # if so, calculate the proportions
          i <- i[, 1] / rowSums(i, na.rm = TRUE)
        } else {
          # if not, we just take the first column
          i <- i[, 1]
        }
      }
      # and return as a vector
      as.vector(i)
    }))
  }

  if (is.null(out)) {
    insight::format_error(sprintf(
      "Could not simulate responses. Maybe there is no `simulate()` for objects of class `%s`?",
      class(object)[1]
    ))
  }

  # get response data, and response term, to check for transformations
  response <- insight::get_response(object)
  resp_string <- insight::find_terms(object)$response
  pattern <- "^(scale|exp|expm1|log|log1p|log10|log2|sqrt)"

  # check for transformed response, and backtransform simulations
  if (!is.null(resp_string) && length(resp_string) == 1 && grepl(paste0(pattern, "\\("), resp_string)) {
    out <- .backtransform_sims(out, resp_string)
  }

  # sanity check - do we have a ratio or similar?
  if (is.data.frame(response)) {
    # get response data, evaluate formula
    response <- eval(str2lang(insight::find_response(object)),
      envir = insight::get_response(object)
    )
  }

  out$y <- response

  attr(out, "check_range") <- check_range
  attr(out, "response_name") <- resp_string
  attr(out, "bandwidth") <- bandwidth
  attr(out, "model_info") <- minfo
  attr(out, "type") <- type
  class(out) <- c("performance_pp_check", "see_performance_pp_check", class(out))
  out
}


pp_check.glm <- function(
  object,
  iterations = 50,
  check_range = FALSE,
  re_formula = NULL,
  bandwidth = "nrd",
  type = "density",
  verbose = TRUE,
  model_info = NULL,
  ...
) {
  # we need the formula and the response values to check for matrix responses
  # or proportions in binomial models
  model_response <- insight::find_response(object, combine = TRUE)
  response_values <- insight::get_response(object)

  # if we have no matrix-response, continue here...
  if (!grepl("^cbind\\((.*)\\)", model_response) && !is.matrix(response_values)) {
    return(pp_check.lm(
      object,
      iterations,
      check_range,
      re_formula,
      bandwidth,
      type,
      verbose,
      model_info,
      ...
    ))
  }

  # else, process matrix response. for matrix response models, we compute
  # the ratio of successes and failures, because the plot cannot handle
  # matrix columns with separate success/failures in simulations.

  out <- tryCatch(
    {
      matrix_sim <- stats::simulate(
        object,
        nsim = iterations,
        re.form = re_formula,
        ...
      )
      as.data.frame(sapply(
        matrix_sim,
        function(i) i[, 1] / rowSums(i, na.rm = TRUE),
        simplify = TRUE
      ))
    },
    error = function(e) {
      NULL
    }
  )

  # validation check, for mixed models, where re.form = NULL (default) might fail
  out <- .check_re_formula(out, object, iterations, re_formula, verbose, ...)

  if (is.null(out)) {
    insight::format_error(sprintf(
      "Could not simulate responses. Maybe there is no `simulate()` for objects of class `%s`?",
      class(object)[1]
    ))
  }

  # get response data, and response term
  response <- .safe(
    eval(
      str2lang(insight::find_response(object)),
      envir = insight::get_response(object)
    ),
    insight::get_response(object)
  )
  resp_string <- insight::find_terms(object)$response

  out$y <- response[, 1] / rowSums(response, na.rm = TRUE)

  # safe information about model
  if (is.null(model_info)) {
    minfo <- insight::model_info(object)
  } else {
    minfo <- model_info
  }

  attr(out, "check_range") <- check_range
  attr(out, "response_name") <- resp_string
  attr(out, "bandwidth") <- bandwidth
  attr(out, "model_info") <- minfo
  attr(out, "type") <- type
  class(out) <- c(
    "performance_pp_check",
    "see_performance_pp_check",
    class(out)
  )
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


#' @rawNamespace
#' S3method(bayesplot::pp_check, lm)
#' S3method(bayesplot::pp_check, glm)
#' S3method(bayesplot::pp_check, glmmTMB)
#' S3method(bayesplot::pp_check, glm.nb)
#' S3method(bayesplot::pp_check, merMod)
#' S3method(bayesplot::pp_check, MixMod)
#' S3method(bayesplot::pp_check, mle2)
#' S3method(bayesplot::pp_check, negbin)
#' S3method(bayesplot::pp_check, polr)
#' S3method(bayesplot::pp_check, rma)
#' S3method(bayesplot::pp_check, vlm)
#' S3method(bayesplot::pp_check, wbm)
#' S3method(bayesplot::pp_check, BFBayesFactor)


# methods -----------------------


#' @export
print.performance_pp_check <- function(x, verbose = TRUE, ...) {
  original <- x$y
  replicated <- x[which(names(x) != "y")]

  if (isTRUE(verbose)) {
    if (is.numeric(original)) {
      if (min(replicated) > min(original)) {
        insight::print_color(
          insight::format_message(
            "Warning: Minimum value of original data is not included in the replicated data.",
            "Model may not capture the variation of the data."
          ),
          "red"
        )
      }

      if (max(replicated) < max(original)) {
        insight::print_color(
          insight::format_message(
            "Warning: Maximum value of original data is not included in the replicated data.",
            "Model may not capture the variation of the data."
          ),
          "red"
        )
      }
    } else {
      missing_levs <- setdiff(original, unlist(replicated))
      if (length(missing_levs)) {
        insight::print_color(
          insight::format_message(
            paste0(
              "Warning: Level",
              ifelse(length(missing_levs) == 1, " ", "s "),
              paste0("'", missing_levs, "'", collapse = ", "),
              " from original data is not included in the replicated data."
            ), "Model may not capture the variation of the data."
          ),
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


# helper --------------------

.backtransform_sims <- function(sims, resp_string) {
  if (grepl("log(log(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) exp(exp(i)))
  } else if (grepl("log(", resp_string, fixed = TRUE)) {
    # exceptions: log(x+1) or log(1+x)
    # 1. try: log(x + number)
    plus_minus <- .safe(eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\2", resp_string))))
    # 2. try: log(number + x)
    if (is.null(plus_minus)) {
      plus_minus <- .safe(eval(parse(text = gsub("log\\(([^,\\+)]*)(.*)\\)", "\\1", resp_string))))
    }
    if (is.null(plus_minus) || !is.numeric(plus_minus)) {
      sims[] <- lapply(sims, exp)
    } else {
      sims[] <- lapply(sims, function(i) exp(i) - plus_minus)
    }
  } else if (grepl("log1p(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, expm1)
  } else if (grepl("log10(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) 10^i)
  } else if (grepl("log2(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) 2^i)
  } else if (grepl("sqrt(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, function(i) i^2)
  } else if (grepl("exp(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, log)
  } else if (grepl("expm1(", resp_string, fixed = TRUE)) {
    sims[] <- lapply(sims, log1p)
  }

  sims
}


.check_re_formula <- function(out, object, iterations, re_formula, verbose, ...) {
  # validation check, for mixed models, where re.form = NULL (default) might fail
  if (is.null(out) && insight::is_mixed_model(object) && !isTRUE(is.na(re_formula))) {
    if (verbose) {
      insight::format_alert(
        paste0(
          "Failed to compute posterior predictive checks with `re_formula=",
          deparse(re_formula),
          "`."
        ),
        "Trying again with `re_formula=NA` now."
      )
    }
    out <- .safe(stats::simulate(object, nsim = iterations, re.form = NA, ...))
  }
  out
}
