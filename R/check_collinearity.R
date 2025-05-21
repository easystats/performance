#' @title Check for multicollinearity of model terms
#' @name check_collinearity
#'
#' @description
#'
#'  `check_collinearity()` checks regression models for
#'  multicollinearity by calculating the variance inflation factor (VIF).
#'  `multicollinearity()` is an alias for `check_collinearity()`.
#'  `check_concurvity()` is a wrapper around `mgcv::concurvity()`, and can be
#'  considered as a collinearity check for smooth terms in GAMs. Confidence
#'  intervals for VIF and tolerance are based on Marcoulides et al.
#'  (2019, Appendix B).
#'
#' @param x A model object (that should at least respond to `vcov()`,
#'  and if possible, also to `model.matrix()` - however, it also should
#'  work without `model.matrix()`).
#' @param component For models with zero-inflation component, multicollinearity
#'  can be checked for the conditional model (count component,
#'  `component = "conditional"` or `component = "count"`),
#'  zero-inflation component (`component = "zero_inflated"` or
#'  `component = "zi"`) or both components (`component = "all"`).
#'  Following model-classes are currently supported: `hurdle`,
#'  `zeroinfl`, `zerocount`, `MixMod` and `glmmTMB`.
#' @param ci Confidence Interval (CI) level for VIF and tolerance values.
#' @param verbose Toggle off warnings or messages.
#' @param ... Currently not used.
#'
#' @return A data frame with information about name of the model term, the
#'   variance inflation factor and associated confidence intervals, the factor
#'   by which the standard error is increased due to possible correlation
#'   with other terms, and tolerance values (including confidence intervals),
#'   where `tolerance = 1/vif`.
#'
#' @seealso [`see::plot.see_check_collinearity()`] for options to customize the plot.
#'
#' @section Multicollinearity:
#' Multicollinearity should not be confused with a raw strong correlation
#' between predictors. What matters is the association between one or more
#' predictor variables, *conditional on the other variables in the
#' model*. In a nutshell, multicollinearity means that once you know the
#' effect of one predictor, the value of knowing the other predictor is rather
#' low. Thus, one of the predictors doesn't help much in terms of better
#' understanding the model or predicting the outcome. As a consequence, if
#' multicollinearity is a problem, the model seems to suggest that the
#' predictors in question don't seems to be reliably associated with the
#' outcome (low estimates, high standard errors), although these predictors
#' actually are strongly associated with the outcome, i.e. indeed might have
#' strong effect (_McElreath 2020, chapter 6.1_).
#'
#' Multicollinearity might arise when a third, unobserved variable has a causal
#' effect on each of the two predictors that are associated with the outcome.
#' In such cases, the actual relationship that matters would be the association
#' between the unobserved variable and the outcome.
#'
#' Remember: "Pairwise correlations are not the problem. It is the conditional
#' associations - not correlations - that matter." (_McElreath 2020, p. 169_)
#'
#' @section Interpretation of the Variance Inflation Factor:
#' The variance inflation factor is a measure to analyze the magnitude of
#' multicollinearity of model terms. A VIF less than 5 indicates a low
#' correlation of that predictor with other predictors. A value between 5 and
#' 10 indicates a moderate correlation, while VIF values larger than 10 are a
#' sign for high, not tolerable correlation of model predictors (_James et al.
#' 2013_). The *Increased SE* column in the output indicates how much larger
#' the standard error is due to the association with other predictors
#' conditional on the remaining variables in the model. Note that these
#' thresholds, although commonly used, are also criticized for being too high.
#' _Zuur et al. (2010)_ suggest using lower values, e.g. a VIF of 3 or larger
#' may already no longer be considered as "low".
#'
#' @section Multicollinearity and Interaction Terms:
#' If interaction terms are included in a model, high VIF values are expected.
#' This portion of multicollinearity among the component terms of an
#' interaction is also called "inessential ill-conditioning", which leads to
#' inflated VIF values that are typically seen for models with interaction
#' terms _(Francoeur 2013)_. Centering interaction terms can resolve this
#' issue _(Kim and Jung 2024)_.
#'
#' @section Multicollinearity and Polynomial Terms:
#' Polynomial transformations are considered a single term and thus VIFs are
#' not calculated between them.
#'
#' @section Concurvity for Smooth Terms in Generalized Additive Models:
#' `check_concurvity()` is a wrapper around `mgcv::concurvity()`, and can be
#' considered as a collinearity check for smooth terms in GAMs."Concurvity
#' occurs when some smooth term in a model could be approximated by one or more
#' of the other smooth terms in the model." (see `?mgcv::concurvity`).
#' `check_concurvity()` returns a column named _VIF_, which is the "worst"
#' measure. While `mgcv::concurvity()` range between 0 and 1, the _VIF_ value
#' is `1 / (1 - worst)`, to make interpretation comparable to classical VIF
#' values, i.e. `1` indicates no problems, while higher values indicate
#' increasing lack of identifiability. The _VIF proportion_ column equals the
#' "estimate" column from `mgcv::concurvity()`, ranging from 0 (no problem) to
#' 1 (total lack of identifiability).
#'
#' @references
#'
#' - Francoeur, R. B. (2013). Could Sequential Residual Centering Resolve
#'   Low Sensitivity in Moderated Regression? Simulations and Cancer Symptom
#'   Clusters. Open Journal of Statistics, 03(06), 24-44.
#'
#' - James, G., Witten, D., Hastie, T., and Tibshirani, R. (eds.). (2013). An
#'   introduction to statistical learning: with applications in R. New York:
#'   Springer.
#'
#' - Kim, Y., & Jung, G. (2024). Understanding linear interaction analysis with
#'   causal graphs. British Journal of Mathematical and Statistical Psychology,
#'   00, 1–14.
#'
#' - Marcoulides, K. M., and Raykov, T. (2019). Evaluation of Variance
#'   Inflation Factors in Regression Models Using Latent Variable Modeling
#'   Methods. Educational and Psychological Measurement, 79(5), 874–882.
#'
#' - McElreath, R. (2020). Statistical rethinking: A Bayesian course with
#'   examples in R and Stan. 2nd edition. Chapman and Hall/CRC.
#'
#' - Vanhove, J. (2019). Collinearity isn't a disease that needs curing.
#'   [webpage](https://janhove.github.io/posts/2019-09-11-collinearity/)
#'
#' - Zuur AF, Ieno EN, Elphick CS. A protocol for data exploration to avoid
#'   common statistical problems: Data exploration. Methods in Ecology and
#'   Evolution (2010) 1:3–14.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @note The code to compute the confidence intervals for the VIF and tolerance
#' values was adapted from the Appendix B from the Marcoulides et al. paper.
#' Thus, credits go to these authors the original algorithm. There is also
#' a [`plot()`-method](https://easystats.github.io/see/articles/performance.html)
#' implemented in the \href{https://easystats.github.io/see/}{\pkg{see}-package}.
#'
#' @examples
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_collinearity(m)
#'
#' @examplesIf insight::check_if_installed("see", minimum_version = "0.9.1", quietly = TRUE)
#' # plot results
#' x <- check_collinearity(m)
#' plot(x)
#' @export
check_collinearity <- function(x, ...) {
  UseMethod("check_collinearity")
}


#' @rdname check_collinearity
#' @export
multicollinearity <- check_collinearity


# default ------------------------------

#' @rdname check_collinearity
#' @export
check_collinearity.default <- function(x, ci = 0.95, verbose = TRUE, ...) {
  .is_model_valid(x)
  .check_collinearity(x, component = "conditional", ci = ci, verbose = verbose)
}


# methods -------------------------------------------

#' @export
print.check_collinearity <- function(x, ...) {
  insight::print_color("# Check for Multicollinearity\n", "blue")

  if ("Component" %in% colnames(x)) {
    comp <- split(x, x$Component)
    for (i in seq_along(comp)) {
      cat(paste0("\n* ", comp[[i]]$Component[1], " component:\n"))
      .print_collinearity(datawizard::data_remove(comp[[i]], "Component"))
    }
  } else {
    .print_collinearity(x)
  }

  invisible(x)
}


#' @export
plot.check_collinearity <- function(x, ...) {
  insight::check_if_installed("see", "to plot collinearity-check")
  NextMethod()
}


.print_collinearity <- function(x) {
  vifs <- x$VIF
  low_vif <- which(vifs < 5)
  mid_vif <- which(vifs >= 5 & vifs < 10)
  high_vif <- which(vifs >= 10)

  all_vifs <- insight::compact_list(list(low_vif, mid_vif, high_vif))

  # if we have no CIs, remove those columns
  x <- datawizard::remove_empty_columns(x)

  # format table for each "ViF" group - this ensures that CIs are properly formatted
  x <- insight::format_table(x)
  x <- datawizard::data_rename(
    x,
    select = "SE_factor",
    replacement = "Increased SE",
    verbose = FALSE
  )

  if (length(low_vif)) {
    cat("\n")
    insight::print_color("Low Correlation\n\n", "green")
    print.data.frame(x[low_vif, ], row.names = FALSE)
  }

  if (length(mid_vif)) {
    cat("\n")
    insight::print_color("Moderate Correlation\n\n", "yellow")
    print.data.frame(x[mid_vif, ], row.names = FALSE)
  }

  if (length(high_vif)) {
    cat("\n")
    insight::print_color("High Correlation\n\n", "red")
    print.data.frame(x[high_vif, ], row.names = FALSE)
  }
}


# other classes ----------------------------------

#' @export
check_collinearity.afex_aov <- function(x, verbose = TRUE, ...) {
  if (length(attr(x, "within")) == 0L) {
    return(check_collinearity(x$lm, verbose = verbose, ...))
  }

  f <- insight::find_formula(x)[[1]]
  f <- Reduce(paste, deparse(f))
  f <- sub("\\+\\s*Error\\(.*\\)$", "", f)
  f <- stats::as.formula(f)

  d <- insight::get_data(x, verbose = FALSE)
  is_num <- vapply(d, is.numeric, logical(1))
  d[is_num] <- sapply(d[is_num], datawizard::center, simplify = TRUE)
  is_fac <- !is_num
  contrs <- lapply(is_fac, function(...) stats::contr.sum)[is_fac]

  if (verbose) {
    insight::format_alert(
      "All predictors have been centered (factors with `contr.sum()`, numerics with `scale()`)."
    )
  }

  check_collinearity(suppressWarnings(stats::lm(
    formula = f,
    data = d,
    contrasts = contrs
  )))
}

#' @export
check_collinearity.BFBayesFactor <- function(x, verbose = TRUE, ...) {
  if (!insight::is_model(x)) {
    insight::format_error("Collinearity only applicable to regression models.")
  }

  f <- insight::find_formula(x)[[1]]
  d <- insight::get_data(x, verbose = FALSE)
  check_collinearity(stats::lm(f, d))
}

# mfx models -------------------------------

#' @export
check_collinearity.logitor <- function(x, ci = 0.95, verbose = TRUE, ...) {
  .check_collinearity(x$fit, component = "conditional", ci = ci, verbose = verbose)
}

#' @export
check_collinearity.logitmfx <- check_collinearity.logitor

#' @export
check_collinearity.probitmfx <- check_collinearity.logitor

#' @export
check_collinearity.poissonirr <- check_collinearity.logitor

#' @export
check_collinearity.poissonmfx <- check_collinearity.logitor

#' @export
check_collinearity.negbinirr <- check_collinearity.logitor

#' @export
check_collinearity.negbinmfx <- check_collinearity.logitor

#' @export
check_collinearity.betaor <- check_collinearity.logitor

#' @export
check_collinearity.betamfx <- check_collinearity.logitor


# zi-models -------------------------------------

#' @rdname check_collinearity
#' @export
check_collinearity.glmmTMB <- function(x,
                                       component = "all",
                                       ci = 0.95,
                                       verbose = TRUE,
                                       ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "count", "zi", "zero_inflated")
  )
  .check_collinearity_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
check_collinearity.MixMod <- function(x,
                                      component = "all",
                                      ci = 0.95,
                                      verbose = TRUE,
                                      ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "count", "zi", "zero_inflated")
  )
  .check_collinearity_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
check_collinearity.hurdle <- function(x,
                                      component = "all",
                                      ci = 0.95,
                                      verbose = verbose,
                                      ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "count", "zi", "zero_inflated")
  )
  .check_collinearity_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
check_collinearity.zeroinfl <- function(x,
                                        component = "all",
                                        ci = 0.95,
                                        verbose = verbose,
                                        ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "count", "zi", "zero_inflated")
  )
  .check_collinearity_zi_model(x, component, ci = ci, verbose = verbose)
}


#' @export
check_collinearity.zerocount <- function(x,
                                         component = "all",
                                         ci = 0.95,
                                         verbose = verbose,
                                         ...) {
  component <- insight::validate_argument(
    component,
    c("all", "conditional", "count", "zi", "zero_inflated")
  )
  .check_collinearity_zi_model(x, component, ci = ci, verbose = verbose)
}


# utilities ---------------------------------

.check_collinearity_zi_model <- function(x, component, ci = 0.95, verbose = TRUE) {
  if (component == "count") component <- "conditional"
  if (component == "zi") component <- "zero_inflated"

  mi <- insight::model_info(x, verbose = FALSE)
  if (!mi$is_zero_inflated) component <- "conditional"

  if (component == "all") {
    cond <- .check_collinearity(x, "conditional", ci = ci, verbose = verbose)
    zi <- .check_collinearity(x, "zero_inflated", ci = ci, verbose = FALSE)
    if (is.null(cond) && is.null(zi)) {
      return(NULL)
    }
    if (is.null(cond)) {
      zi$Component <- "zero inflated"
      return(zi)
    }
    if (is.null(zi)) {
      cond$Component <- "conditional"
      return(cond)
    }

    # retrieve data for plotting
    dat_cond <- attr(cond, "data")
    dat_zi <- attr(zi, "data")
    ci_cond <- attr(cond, "CI")
    ci_zi <- attr(zi, "CI")

    # add component
    cond$Component <- "conditional"
    zi$Component <- "zero inflated"
    dat_cond$Component <- "conditional"
    dat_zi$Component <- "zero inflated"
    ci_cond$Component <- "conditional"
    ci_zi$Component <- "zero inflated"

    # create final data
    dat <- rbind(cond, zi)
    attr(dat, "data") <- rbind(dat_cond, dat_zi)
    attr(dat, "CI") <- rbind(ci_cond, ci_zi)
    dat
  } else {
    .check_collinearity(x, component, ci = ci, verbose = verbose)
  }
}


.check_collinearity <- function(x, component, ci = 0.95, verbose = TRUE) {
  v <- .safe(insight::get_varcov(x, component = component, verbose = FALSE))

  # sanity check
  if (is.null(v)) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        paste(
          sprintf("Could not extract the variance-covariance matrix for the %s component of the model.", component),
          "Please try to run `vcov(model)`, which may help identifying the problem."
        )
      )
    }
    return(NULL)
  }

  term_assign <- .term_assignments(x, component, verbose = verbose)

  # any assignment found?
  if (is.null(term_assign) || all(is.na(term_assign))) {
    if (verbose) {
      insight::format_alert(
        sprintf("Could not extract model terms for the %s component of the model.", component)
      )
    }
    return(NULL)
  }


  # we have rank-deficiency here. remove NA columns from assignment
  if (isTRUE(attributes(v)$rank_deficient) && !is.null(attributes(v)$na_columns_index)) {
    term_assign <- term_assign[-attributes(v)$na_columns_index]
    if (isTRUE(verbose)) {
      insight::format_alert(
        "Model matrix is rank deficient. VIFs may not be sensible."
      )
    }
  }

  # check for missing intercept
  if (insight::has_intercept(x)) {
    v <- v[-1, -1]
    term_assign <- term_assign[-1]
  } else if (isTRUE(verbose)) {
    insight::format_alert("Model has no intercept. VIFs may not be sensible.")
  }

  f <- insight::find_formula(x, verbose = FALSE)

  # hurdle or zeroinfl model can have no zero-inflation formula, in which case
  # we have the same formula as for conditional formula part
  if (inherits(x, c("hurdle", "zeroinfl", "zerocount")) &&
    component == "zero_inflated" &&
    is.null(f[["zero_inflated"]])) {
    f$zero_inflated <- f$conditional
  }

  if (inherits(x, "mixor")) {
    model_terms <- labels(x$terms)
  } else {
    model_terms <- labels(stats::terms(f[[component]]))
  }

  if ("instruments" %in% names(f)) {
    model_terms <- unique(c(model_terms, labels(stats::terms(f[["instruments"]]))))
  }

  n.terms <- length(model_terms)

  if (n.terms < 2) {
    if (isTRUE(verbose)) {
      insight::format_alert(
        sprintf("Not enough model terms in the %s part of the model to check for multicollinearity.", component)
      )
    }
    return(NULL)
  }

  R <- stats::cov2cor(v)
  detR <- det(R)

  result <- vector("numeric")
  na_terms <- vector("numeric")

  # sanity check - models with offset(?) may contain too many term assignments
  if (length(term_assign) > ncol(v)) {
    term_assign <- term_assign[seq_len(ncol(v))]
  }

  for (term in 1:n.terms) {
    subs <- which(term_assign == term)
    if (length(subs)) {
      result <- c(
        result,
        det(as.matrix(R[subs, subs])) * det(as.matrix(R[-subs, -subs])) / detR
      )
    } else {
      na_terms <- c(na_terms, term)
    }
  }

  # any terms to remove, due to rank deficiency?
  if (length(na_terms)) {
    model_terms <- model_terms[-na_terms]
  }

  # check for interactions, VIF might be inflated...
  if (!is.null(insight::find_interactions(x)) && any(result > 10) && isTRUE(verbose)) {
    insight::format_alert(
      "Model has interaction terms. VIFs might be inflated.",
      "Try to center the variables used for the interaction, or check multicollinearity among predictors of a model without interaction terms." # nolint
    )
  }

  # check if CIs are requested
  conf_ints <- .vif_ci(x, result, ci)

  out <- insight::text_remove_backticks(
    data.frame(
      Term = model_terms,
      VIF = result,
      VIF_CI_low = 1 / (1 - conf_ints[1]),
      VIF_CI_high = 1 / (1 - conf_ints[2]),
      SE_factor = sqrt(result),
      Tolerance = 1 / result,
      Tolerance_CI_low = 1 - conf_ints[2],
      Tolerance_CI_high = 1 - conf_ints[1],
      stringsAsFactors = FALSE
    ),
    column = "Term"
  )
  attr(out, "ci") <- ci

  attr(out, "data") <- insight::text_remove_backticks(
    data.frame(
      Term = model_terms,
      VIF = result,
      SE_factor = sqrt(result),
      stringsAsFactors = FALSE
    ),
    column = "Term"
  )

  attr(out, "CI") <- data.frame(
    VIF_CI_low = 1 / (1 - conf_ints[1]),
    VIF_CI_high = 1 / (1 - conf_ints[2]),
    Tolerance_CI_low = 1 - conf_ints[2],
    Tolerance_CI_high = 1 - conf_ints[1],
    stringsAsFactors = FALSE
  )

  class(out) <- c("check_collinearity", "see_check_collinearity", "data.frame")
  out
}


.vif_ci <- function(x, result, ci) {
  # CIs, see Appendix B 10.1177/0013164418817803
  r <- 1 - (1 / result)
  n <- insight::n_obs(x)
  p <- insight::n_parameters(x)

  if (!is.null(ci) && !is.na(ci) && is.numeric(ci)) {
    ci_lvl <- (1 + ci) / 2

    logis_r <- stats::qlogis(r) # see Raykov & Marcoulides (2011, ch. 7) for details.
    se <- sqrt((1 - r^2)^2 * (n - p - 1)^2 / ((n^2 - 1) * (n + 3)))
    se_log <- se / (r * (1 - r))
    ci_log_lo <- logis_r - stats::qnorm(ci_lvl) * se_log
    ci_log_up <- logis_r + stats::qnorm(ci_lvl) * se_log
    ci_lo <- stats::plogis(ci_log_lo)
    ci_up <- stats::plogis(ci_log_up)
  } else {
    ci_lo <- ci_up <- NA
  }
  c(ci_lo, ci_up)
}


.term_assignments <- function(x, component, verbose = TRUE) {
  tryCatch(
    {
      if (inherits(x, c("hurdle", "zeroinfl", "zerocount"))) {
        term_assign <- switch(component,
          conditional = attr(insight::get_modelmatrix(x, model = "count"), "assign"),
          zero_inflated = attr(insight::get_modelmatrix(x, model = "zero"), "assign")
        )
      } else if (inherits(x, "glmmTMB")) {
        term_assign <- switch(component,
          conditional = attr(insight::get_modelmatrix(x), "assign"),
          zero_inflated = .zi_term_assignment(x, component, verbose = verbose)
        )
      } else if (inherits(x, "MixMod")) {
        term_assign <- switch(component,
          conditional = attr(insight::get_modelmatrix(x, type = "fixed"), "assign"),
          zero_inflated = attr(insight::get_modelmatrix(x, type = "zi_fixed"), "assign")
        )
      } else {
        term_assign <- attr(insight::get_modelmatrix(x), "assign")
      }

      if (is.null(term_assign)) {
        term_assign <- .find_term_assignment(x, component, verbose = verbose)
      }

      term_assign
    },
    error = function(e) {
      .find_term_assignment(x, component, verbose = verbose)
    }
  )
}


.find_term_assignment <- function(x, component, verbose = TRUE) {
  pred <- insight::find_predictors(x)[[component]]

  if (is.null(pred)) {
    return(NULL)
  }

  dat <- insight::get_data(x, verbose = FALSE)[, pred, drop = FALSE]

  parms <- unlist(lapply(seq_along(pred), function(i) {
    p <- pred[i]
    if (is.factor(dat[[p]])) {
      ps <- paste0(p, levels(dat[[p]]))
      names(ps)[seq_along(ps)] <- i
      ps
    } else {
      names(p) <- i
      p
    }
  }))

  if (insight::is_gam_model(x)) {
    model_params <- as.vector(unlist(insight::find_parameters(x)[c(component, "smooth_terms")]))
  } else {
    model_params <- insight::find_parameters(x)[[component]]
  }

  as.numeric(names(parms)[match(
    insight::clean_names(model_params),
    parms
  )])
}


.zi_term_assignment <- function(x, component = "zero_inflated", verbose = TRUE) {
  tryCatch(
    {
      rhs <- insight::find_formula(x)[[component]]
      d <- insight::get_data(x, verbose = FALSE)
      attr(insight::get_modelmatrix(rhs, data = d), "assign")
    },
    error = function(e) {
      NULL
    }
  )
}
