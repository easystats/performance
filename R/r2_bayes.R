#' @title Bayesian R2
#' @name r2_bayes
#'
#' @description Compute R2 for Bayesian models. For mixed models (including a
#'   random part), it additionally computes the R2 related to the fixed effects
#'   only (marginal R2). While `r2_bayes()` returns a single R2 value,
#'   `r2_posterior()` returns a posterior sample of Bayesian R2 values.
#'
#' @param model A Bayesian regression model (from **brms**,
#'   **rstanarm**, **BayesFactor**, etc).
#' @param robust Logical, if `TRUE`, the median instead of mean is used to
#'   calculate the central tendency of the variances.
#' @param ci Value or vector of probability of the CI (between 0 and 1) to be
#'   estimated.
#' @param ... Arguments passed to `r2_posterior()`.
#' @inheritParams model_performance.lm
#'
#' @return A list with the Bayesian R2 value. For mixed models, a list with the
#'   Bayesian R2 value and the marginal Bayesian R2 value. The standard errors
#'   and credible intervals for the R2 values are saved as attributes.
#'
#' @details `r2_bayes()` returns an "unadjusted" R2 value. See
#'   [r2_loo()] to calculate a LOO-adjusted R2, which comes
#'   conceptually closer to an adjusted R2 measure.
#'
#'   For mixed models, the conditional and marginal R2 are returned. The marginal
#'   R2 considers only the variance of the fixed effects, while the conditional
#'   R2 takes both the fixed and random effects into account.
#'
#'   `r2_posterior()` is the actual workhorse for `r2_bayes()` and
#'   returns a posterior sample of Bayesian R2 values.
#'
#' @examplesIf require("rstanarm") && require("rstantools") && require("BayesFactor") && require("brms")
#' library(performance)
#' \donttest{
#' model <- suppressWarnings(rstanarm::stan_glm(
#'   mpg ~ wt + cyl,
#'   data = mtcars,
#'   chains = 1,
#'   iter = 500,
#'   refresh = 0,
#'   show_messages = FALSE
#' ))
#' r2_bayes(model)
#'
#' model <- suppressWarnings(rstanarm::stan_lmer(
#'   Petal.Length ~ Petal.Width + (1 | Species),
#'   data = iris,
#'   chains = 1,
#'   iter = 500,
#'   refresh = 0
#' ))
#' r2_bayes(model)
#' }
#'
#' BFM <- BayesFactor::generalTestBF(mpg ~ qsec + gear, data = mtcars, progress = FALSE)
#' FM <- BayesFactor::lmBF(mpg ~ qsec + gear, data = mtcars)
#'
#' r2_bayes(FM)
#' r2_bayes(BFM[3])
#' r2_bayes(BFM, average = TRUE) # across all models
#'
#' # with random effects:
#' mtcars$gear <- factor(mtcars$gear)
#' model <- BayesFactor::lmBF(
#'   mpg ~ hp + cyl + gear + gear:wt,
#'   mtcars,
#'   progress = FALSE,
#'   whichRandom = c("gear", "gear:wt")
#' )
#'
#' r2_bayes(model)
#'
#' \donttest{
#' model <- suppressWarnings(brms::brm(
#'   mpg ~ wt + cyl,
#'   data = mtcars,
#'   silent = 2,
#'   refresh = 0
#' ))
#' r2_bayes(model)
#'
#' model <- suppressWarnings(brms::brm(
#'   Petal.Length ~ Petal.Width + (1 | Species),
#'   data = iris,
#'   silent = 2,
#'   refresh = 0
#' ))
#' r2_bayes(model)
#' }
#' @references
#' Gelman, A., Goodrich, B., Gabry, J., and Vehtari, A. (2018).
#' R-squared for Bayesian regression models. The American Statistician, 1–6.
#' \doi{10.1080/00031305.2018.1549100}
#' @export
r2_bayes <- function(model, robust = TRUE, ci = 0.95, verbose = TRUE, ...) {
  r2_bayesian <- r2_posterior(model, verbose = verbose, ...)

  if (is.null(r2_bayesian)) {
    return(NULL)
  }

  if (insight::is_multivariate(model)) {
    structure(
      class = "r2_bayes_mv",
      rapply(r2_bayesian, function(i) {
        if (robust) {
          stats::median(i)
        } else {
          mean(i)
        }
      }),
      SE = rapply(r2_bayesian, function(i) {
        if (robust) {
          stats::mad(i)
        } else {
          stats::sd(i)
        }
      }),
      # "Estimates" = rapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      CI = rapply(r2_bayesian, bayestestR::hdi, ci = ci),
      ci_method = "HDI",
      robust = robust
    )
  } else {
    structure(
      class = "r2_bayes",
      lapply(r2_bayesian, function(i) {
        if (robust) {
          stats::median(i)
        } else {
          mean(i)
        }
      }),
      SE = lapply(r2_bayesian, function(i) {
        if (robust) {
          stats::mad(i)
        } else {
          stats::sd(i)
        }
      }),
      # Estimates = lapply(r2_bayesian, bayestestR::point_estimate, centrality = "all", dispersion = TRUE),
      CI = lapply(r2_bayesian, bayestestR::hdi, ci = ci),
      ci_method = "HDI",
      robust = robust
    )
  }
}

#' @export
#' @rdname r2_bayes
r2_posterior <- function(model, ...) {
  UseMethod("r2_posterior")
}

#' @export
#' @rdname r2_bayes
r2_posterior.brmsfit <- function(model, verbose = TRUE, ...) {
  insight::check_if_installed("rstantools")

  algorithm <- insight::find_algorithm(model)
  if (algorithm$algorithm != "sampling") {
    insight::format_warning(
      "`r2()` only available for models fit using the `sampling` algorithm."
    )
    return(NA)
  }

  tryCatch(
    {
      mi <- insight::model_info(model)

      if (insight::is_multivariate(model)) {
        res <- insight::find_response(model)
        if (mi[[1]]$is_mixed) {
          br2_mv <- list(
            R2_Bayes = rstantools::bayes_R2(
              model,
              re.form = NULL,
              re_formula = NULL,
              summary = FALSE
            ),
            R2_Bayes_marginal = rstantools::bayes_R2(
              model,
              re.form = NA,
              re_formula = NA,
              summary = FALSE
            )
          )
          br2 <- lapply(seq_along(res), function(x) {
            list(
              R2_Bayes = unname(as.vector(br2_mv$R2_Bayes[, x])),
              R2_Bayes_marginal = unname(as.vector(br2_mv$R2_Bayes_marginal[, x]))
            )
          })
          names(br2) <- res
        } else {
          br2_mv <- list(R2_Bayes = rstantools::bayes_R2(model, summary = FALSE))
          br2 <- lapply(seq_along(res), function(x) {
            list(R2_Bayes = unname(as.vector(br2_mv$R2_Bayes[, x])))
          })
          names(br2) <- res
        }
      } else {
        if (mi$is_mixed) {
          br2 <- list(
            R2_Bayes = as.vector(rstantools::bayes_R2(
              model,
              re.form = NULL,
              re_formula = NULL,
              summary = FALSE
            )),
            R2_Bayes_marginal = as.vector(rstantools::bayes_R2(
              model,
              re.form = NA,
              re_formula = NA,
              summary = FALSE
            ))
          )
          names(br2$R2_Bayes) <- rep("Conditional R2", length(br2$R2_Bayes))
          names(br2$R2_Bayes_marginal) <- rep("Marginal R2", length(br2$R2_Bayes))
        } else {
          br2 <- list(R2_Bayes = as.vector(rstantools::bayes_R2(model, summary = FALSE)))
          names(br2$R2_Bayes) <- rep("R2", length(br2$R2_Bayes))
        }
      }

      br2
    },
    error = function(e) {
      if (inherits(e, c("simpleError", "error"))) {
        insight::print_color(e$message, "red")
        cat("\n")
      }
      NULL
    }
  )
}


#' @export
#' @rdname r2_bayes
r2_posterior.stanreg <- r2_posterior.brmsfit

#' @export
r2_posterior.stanmvreg <- function(model, verbose = TRUE, ...) {
  if (isTRUE(verbose)) {
    insight::format_error("Models of class `stanmvreg` not yet supported.")
  }
  NULL
}


#' @param average Compute model-averaged index? See [bayestestR::weighted_posteriors()].
#' @inheritParams bayestestR::weighted_posteriors
#' @inheritParams r2_bayes
#' @export
#' @rdname r2_bayes
r2_posterior.BFBayesFactor <- function(model,
                                       average = FALSE,
                                       prior_odds = NULL,
                                       verbose = TRUE,
                                       ...) {
  mi <- insight::model_info(model, verbose = FALSE)
  if (!mi$is_linear || mi$is_correlation || mi$is_ttest || mi$is_binomial || mi$is_meta) {
    if (verbose) {
      insight::format_warning("Can produce R2 only for linear models.")
    }
    return(NULL)
  }

  if (average) {
    return(.r2_posterior_model_average(model, prior_odds = prior_odds, verbose = verbose))
  }

  insight::check_if_installed("rstantools")
  insight::check_if_installed("BayesFactor")

  everything_we_need <- .get_bfbf_predictions(model, verbose = verbose)

  # Compute R2!
  y <- everything_we_need[["y"]]
  yy <- everything_we_need[["y_pred"]]
  r2_bayesian <- data.frame(R2_Bayes = rstantools::bayes_R2(yy, y = y))

  if ("y_pred_marginal" %in% names(everything_we_need)) {
    yy <- everything_we_need[["y_pred_marginal"]]
    r2_bayesian$R2_Bayes_marginal <- rstantools::bayes_R2(yy, y = y)
  }

  r2_bayesian
}



#' @keywords internal
.r2_posterior_model_average <- function(model, prior_odds = NULL, verbose = TRUE) {
  insight::check_if_installed("BayesFactor")

  BFMods <- bayestestR::bayesfactor_models(model, verbose = FALSE)

  if (!is.null(BFMods$log_BF)) {
    BFMods$BF <- exp(BFMods$log_BF)
  }

  has_random <- !is.null(insight::find_predictors(model, effects = "random", flatten = TRUE))

  if (any(is.na(BFMods$BF) | is.infinite(BFMods$BF))) {
    if (verbose) {
      insight::format_warning(
        "Can't compute model-averaged index. One or more Bayes factors are NA or infinite."
      )
    }
    return(NULL)
  }

  # extract parameters
  intercept_only <- which(BFMods$Model == "1")
  params <- vector(mode = "list", length = nrow(BFMods))
  for (m in seq_along(params)) {
    if (length(intercept_only) && m == intercept_only) {
      params[[m]] <- data.frame(R2_Bayes = rep(0, 4000))
    } else if (m == 1) {
      # If the model is the "den" model
      params[[m]] <- suppressMessages(r2_posterior(1 / model[1]))
    } else {
      params[[m]] <- suppressMessages(r2_posterior(model[m - 1]))
    }

    # when there is no random effect, marginal = conditional
    if (has_random && is.null(params[[m]]$R2_Bayes_marginal)) {
      params[[m]]$R2_Bayes_marginal <- params[[m]]$R2_Bayes
    }
  }


  # Compute posterior model probabilities
  if (!is.null(prior_odds)) {
    prior_odds <- c(1, prior_odds)
  } else {
    prior_odds <- rep(1, nrow(BFMods))
  }
  posterior_odds <- prior_odds * BFMods$BF
  posterior_odds <- posterior_odds[-1] / posterior_odds[1]

  do.call(
    bayestestR::weighted_posteriors,
    c(params, list(missing = 0, prior_odds = posterior_odds))
  )
}



#' @export
as.data.frame.r2_bayes <- function(x, ...) {
  out <- data.frame(
    R2 = x$R2_Bayes,
    SD = attributes(x)$SE$R2_Bayes,
    CI = attributes(x)$CI$R2_Bayes$CI,
    CI_low = attributes(x)$CI$R2_Bayes$CI_low,
    CI_high = attributes(x)$CI$R2_Bayes$CI_high,
    CI_method = attributes(x)$ci_method,
    stringsAsFactors = FALSE
  )

  if (!is.null(x$R2_Bayes_marginal)) {
    out_marginal <- data.frame(
      R2 = x$R2_Bayes_marginal,
      SD = attributes(x)$SE$R2_Bayes_marginal,
      CI = attributes(x)$CI$R2_Bayes_marginal$CI,
      CI_low = attributes(x)$CI$R2_Bayes_marginal$CI_low,
      CI_high = attributes(x)$CI$R2_Bayes_marginal$CI_high,
      CI_method = attributes(x)$ci_method,
      stringsAsFactors = FALSE
    )

    out$Component <- "conditional"
    out_marginal$Component <- "marginal"
    out <- rbind(out, out_marginal)
  }

  out$Effectsize <- "Bayesian R-squared"
  out
}



# Utils -------------------------------------------------------------------

.get_bfbf_predictions <- function(model, iterations = 4000, verbose = TRUE) {
  insight::check_if_installed("BayesFactor")

  # Estimates
  params <- insight::get_parameters(
    model,
    unreduce = FALSE,
    iterations = iterations,
    verbose = verbose
  )

  # remove sig and g cols
  params_theta <- params[, !grepl(pattern = "^sig2$|^g_|^g$", colnames(params))]
  params_sigma <- sqrt(params[, grepl(pattern = "^sig2$", colnames(params))])

  # Model Matrix
  mm <- insight::get_modelmatrix(model[1])
  colnames(mm)[1] <- "mu"

  # match?
  if ((length(colnames(params_theta)) != length(colnames(mm))) ||
    !all(colnames(params_theta) == colnames(mm))) {
    if (utils::packageVersion("BayesFactor") < package_version("0.9.12.4.3")) {
      insight::format_error("R2 for BayesFactor models with random effects requires BayesFactor v0.9.12.4.3 or higher.")
    }
    insight::format_error("Woops, you seem to have stumbled on some weird edge case. Please file an issue at {.url https://github.com/easystats/performance/issues}") # nolint
  }

  out <- list(
    y = insight::get_response(model, verbose = FALSE),
    y_pred = (as.matrix(params_theta) %*% t(mm))
  )

  rand <- insight::find_predictors(model[1], effects = "random", flatten = TRUE, verbose = FALSE)
  if (!is.null(rand)) {
    idx <- sapply(paste0("\\b", rand, "\\b"), grepl, x = colnames(params_theta))
    idx <- apply(idx, 1, any)
    params_theta[idx] <- 0

    out[["y_pred_marginal"]] <- (as.matrix(params_theta) %*% t(mm))
  }

  out[["sigma"]] <- params_sigma
  out
}

#' @export
residuals.BFBayesFactor <- function(object, ...) {
  everything_we_need <- .get_bfbf_predictions(object, verbose = FALSE)

  everything_we_need[["y"]] - colMeans(everything_we_need[["y_pred"]])
}

#' @export
fitted.BFBayesFactor <- function(object, ...) {
  colMeans(.get_bfbf_predictions(object, verbose = FALSE)[["y_pred"]])
}
