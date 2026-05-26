# prepare data for VIF plot ----------------------------------

.model_diagnostic_vif <- function(model, verbose = TRUE) {
  out <- check_collinearity(model, verbose = verbose)
  dat <- insight::compact_list(out)
  if (is.null(dat)) {
    return(NULL)
  }
  dat$group <- "low"
  dat$group[dat$VIF >= 5 & dat$VIF < 10] <- "moderate"
  dat$group[dat$VIF >= 10] <- "high"

  dat <- datawizard::data_select(
    dat,
    select = c(x = "Term", y = "VIF", facet = "Component", group = "group"),
    verbose = FALSE
  )

  if (insight::n_unique(dat$facet) <= 1) {
    dat$facet <- NULL
  }

  attr(dat, "CI") <- attributes(out)$CI
  dat
}


# prepare data for QQ plot ----------------------------------

.model_diagnostic_qq <- function(model, model_info = NULL, verbose = TRUE) {
  if (
    inherits(model, c("lme", "lmerMod", "merMod", "afex_aov", "BFBayesFactor", "gam"))
  ) {
    res_ <- stats::residuals(model)
  } else if (inherits(model, "geeglm")) {
    res_ <- stats::residuals(model, type = "pearson")
  } else if (inherits(model, "glmmTMB")) {
    res_ <- stats::residuals(model, type = "deviance")
  } else if (inherits(model, "glm")) {
    res_ <- .safe(abs(stats::rstandard(model, type = "deviance")))
  } else {
    res_ <- .safe(stats::rstudent(model))
    if (is.null(res_)) {
      res_ <- .safe(stats::residuals(model))
    }
  }

  if (is.null(res_) || all(is.na(res_))) {
    if (verbose) {
      if (is.null(model_info$family)) {
        fam <- "model"
      } else {
        fam <- paste0("`", model_info$family, "`")
      }
      insight::format_alert(
        paste(
          sprintf(
            "QQ plot could not be created. Cannot extract residuals from objects of class `%s`.",
            class(model)[1]
          ),
          sprintf(
            "Maybe the model class or the %s family does not support the computation of (deviance) residuals?",
            fam
          )
        )
      )
    }
    return(NULL)
  }

  if (
    inherits(model, c("glm", "glmerMod")) ||
      (inherits(model, "glmmTMB") && isFALSE(model_info$is_linear))
  ) {
    fitted_ <- stats::qnorm((stats::ppoints(length(res_)) + 1) / 2)
  } else {
    fitted_ <- stats::fitted(model)
  }

  # validation check, sometimes either residuals or fitted can contain NA, see #488
  if (anyNA(res_) || anyNA(fitted_)) {
    # drop NA and make sure both fitted and residuals match
    non_na <- !is.na(fitted_) & !is.na(res_)
    fitted_ <- fitted_[non_na]
    res_ <- res_[non_na]
  }

  res_ <- sort(res_, na.last = NA)
  fitted_ <- sort(fitted_, na.last = NA)

  data.frame(x = fitted_, y = res_)
}


# prepare data for random effects QQ plot ----------------------------------

.model_diagnostic_ranef_qq <- function(
  model,
  level = 0.95,
  model_info = NULL,
  verbose = TRUE
) {
  # check if we have mixed model
  if (is.null(model_info) || !model_info$is_mixed) {
    return(NULL)
  }

  insight::check_if_installed("lme4")

  tryCatch(
    if (inherits(model, "glmmTMB")) {
      var_attr <- "condVar"
      re <- .collapse_cond(lme4::ranef(model, condVar = TRUE))
    } else {
      var_attr <- "postVar"
      re <- lme4::ranef(model, condVar = TRUE)
    },
    error = function(e) {
      NULL
    }
  )

  se <- tryCatch(
    suppressWarnings(lapply(re, function(.x) {
      pv <- attr(.x, var_attr, exact = TRUE)
      cols <- seq_len(dim(pv)[1])
      unlist(lapply(cols, function(.y) sqrt(pv[.y, .y, ])))
    })),
    error = function(e) {
      NULL
    }
  )

  if (is.null(se)) {
    if (verbose) {
      insight::format_alert(
        "Could not compute standard errors from random effects for diagnostic plot."
      )
    }
    return(NULL)
  }

  Map(
    function(.re, .se) {
      ord <- unlist(lapply(.re, order)) +
        rep((0:(ncol(.re) - 1)) * nrow(.re), each = nrow(.re))

      df.y <- unlist(.re)[ord]
      df.ci <- stats::qnorm((1 + level) / 2) * .se[ord]

      data.frame(
        x = rep(stats::qnorm(stats::ppoints(nrow(.re))), ncol(.re)),
        y = df.y,
        conf.low = df.y - df.ci,
        conf.high = df.y + df.ci,
        facet = gl(ncol(.re), nrow(.re), labels = names(.re)),
        stringsAsFactors = FALSE,
        row.names = NULL
      )
    },
    re,
    se
  )
}


# prepare data for normality of residuals plot ----------------------------------

.model_diagnostic_normality <- function(model, verbose = TRUE) {
  r <- try(as.numeric(stats::residuals(model)), silent = TRUE)

  if (inherits(r, "try-error")) {
    insight::format_alert(sprintf(
      "Non-normality of residuals could not be computed. Cannot extract residuals from objects of class '%s'.",
      class(model)[1]
    ))
    return(NULL)
  }

  dat <- as.data.frame(bayestestR::estimate_density(r))
  dat$curve <- stats::dnorm(
    seq(min(dat$x), max(dat$x), length.out = nrow(dat)),
    mean(r),
    stats::sd(r)
  )
  dat
}


# prepare data for influential obs plot ----------------------------------

.model_diagnostic_outlier <- function(model, threshold = NULL) {
  s <- summary(model)

  if (inherits(model, "lm", which = TRUE) == 1) {
    cook_levels <- round(stats::qf(0.5, s$fstatistic[2], s$fstatistic[3]), 2)
  } else if (is.null(threshold)) {
    cook_levels <- c(0.5, 1)
  } else {
    cook_levels <- threshold
  }

  n_params <- tryCatch(model$rank, error = function(e) insight::n_parameters(model))

  infl <- stats::influence(model, do.coef = FALSE)
  model_resid <- as.numeric(insight::get_residuals(model))

  std_resid <- tryCatch(stats::rstandard(model, infl), error = function(e) model_resid)

  plot_data <- data.frame(
    Hat = infl$hat,
    Cooks_Distance = stats::cooks.distance(model, infl),
    Fitted = insight::get_predicted(model, ci = NULL),
    Residuals = model_resid,
    Std_Residuals = std_resid,
    stringsAsFactors = FALSE
  )
  plot_data$Index <- seq_len(nrow(plot_data))
  plot_data$Influential <- "OK"
  plot_data$Influential[
    abs(plot_data$Cooks_Distance) >= max(cook_levels)
  ] <- "Influential"

  attr(plot_data, "cook_levels") <- cook_levels
  attr(plot_data, "n_params") <- n_params
  plot_data
}


# prepare data for non-constant variance plot ----------------------------------

.model_diagnostic_ncv <- function(model, verbose = TRUE) {
  ncv <- tryCatch(
    data.frame(
      x = as.numeric(stats::fitted(model)),
      y = as.numeric(stats::residuals(model))
    ),
    error = function(e) {
      NULL
    }
  )

  if (is.null(ncv)) {
    if (verbose) {
      insight::format_alert(sprintf(
        "Non-constant error variance could not be computed. Cannot extract residuals from objects of class '%s'.",
        class(model)[1]
      ))
    }
    return(NULL)
  }

  ncv
}


# prepare data for homogeneity of variance plot ----------------------------------

.model_diagnostic_homogeneity <- function(model, verbose = TRUE) {
  faminfo <- insight::model_info(model)
  r <- tryCatch(
    if (inherits(model, "merMod")) {
      stats::residuals(model, scaled = TRUE)
    } else if (inherits(model, "gam")) {
      stats::residuals(model, type = "scaled.pearson")
    } else if (inherits(model, c("glmmTMB", "MixMod"))) {
      residual_sigma <- if (faminfo$is_mixed) {
        sqrt(insight::get_variance_residual(model))
      } else {
        .sigma_glmmTMB_nonmixed(model, faminfo)
      }
      stats::residuals(model) / residual_sigma
    } else if (inherits(model, "glm")) {
      ## TODO: check if we can / should use deviance residuals (as for QQ plots) here as well?
      stats::rstandard(model, type = "pearson")
    } else {
      stats::rstandard(model)
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(r)) {
    if (verbose) {
      insight::format_alert(sprintf(
        "Homogeneity of variance could not be computed. Cannot extract residual variance from objects of class '%s'.",
        class(model)[1]
      ))
    }
    return(NULL)
  }

  data.frame(
    x = stats::fitted(model),
    y = sqrt(abs(r))
  )
}


.model_diagnostic_overdispersion <- function(model, residual_type = NULL, ...) {
  faminfo <- insight::model_info(model)
  d <- NULL

  # validate argument
  if (!is.null(residual_type)) {
    insight::validate_argument(residual_type, c("normal", "simulated"))
  }

  # For mixed models and glmmTMB models, use simulated residuals from DHARMa
  # for more accurate overdispersion visualization. Pearson residuals can be
  # misleading for these model types.

  if (is.null(residual_type)) {
    # fmt: skip
    use_simres <- isTRUE(faminfo$is_mixed) || inherits(model, "glmmTMB") || faminfo$is_negbin
  } else {
    use_simres <- switch(residual_type, normal = FALSE, simulated = TRUE)
  }

  # catch models/families not supported by DHARMa - we need to add more
  # exceptions here as they appear, but for now, `check_model()` also
  # automatically falls back to normal Q-Q plot for all models not supported
  # by DHARMa
  # fmt: skip
  if (faminfo$family %in% c("quasipoisson", "quasibinomial") || !requireNamespace("DHARMa", quietly = TRUE)) {
    use_simres <- FALSE
  }

  if (use_simres) {
    d <- .safe({
      simres <- simulate_residuals(model, ...)
      predicted <- simres$fittedPredictedResponse
      # Use response-scale residuals (observed - predicted) so that Res2 and V
      # are on the same scale as the family variance formula.
      raw_res <- simres$observedResponse - predicted
      data.frame(
        Predicted = predicted,
        Residuals = raw_res,
        Res2 = raw_res^2
      )
    })
    if (!is.null(d)) {
      d$V <- .expected_variance(model, faminfo, d)
      # Pearson-like standardized residuals: divide by sqrt of expected variance.
      # Use 1e-6 floor to avoid division by near-zero values when predicted
      # means are very small, while keeping the value practically interpretable.
      d$StdRes <- d$Residuals / sqrt(pmax(d$V, 1e-6))
      return(d)
    }
  }

  # data for poisson models
  if (faminfo$is_poisson && !faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- insight::get_residuals(model, type = "pearson")
  }

  # data for negative binomial models
  if (faminfo$is_negbin && !faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- insight::get_residuals(model, type = "pearson")
  }

  # data for zero-inflated poisson models
  if (faminfo$is_poisson && faminfo$is_zero_inflated) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    if (inherits(model, "glmmTMB")) {
      ptype <- "zprob"
    } else {
      ptype <- "zero"
    }
    d$Prob <- stats::predict(model, type = ptype)
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- insight::get_residuals(model, type = "pearson")
  }

  # data for zero-inflated negative binomial models
  if (faminfo$is_negbin && faminfo$is_zero_inflated && !faminfo$is_dispersion) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- insight::get_residuals(model, type = "pearson")
  }

  # data for zero-inflated negative binomial models with dispersion
  if (faminfo$is_negbin && faminfo$is_zero_inflated && faminfo$is_dispersion) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_response(model) - as.vector(d$Predicted)
    d$Res2 <- d$Residuals^2
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- insight::get_residuals(model, type = "pearson")
  }

  # fallback for remaining models
  if (is.null(d)) {
    d <- data.frame(Predicted = stats::predict(model, type = "response"))
    d$Residuals <- insight::get_residuals(model)
    d$Res2 <- d$Residuals^2
    d$V <- .expected_variance(model, faminfo, d)
    d$StdRes <- d$Residuals / sqrt(pmax(d$V, 1e-6))
  }

  d
}


# helpers ----------------------------------

.expected_variance <- function(model, faminfo, d) {
  expected_var <- NULL

  # Helper: get per-observation dispersion (if dispersion model) or scalar sigma
  .get_disp <- function() {
    if (isTRUE(faminfo$is_dispersion)) {
      stats::predict(model, type = "disp")
    } else {
      insight::get_sigma(model)
    }
  }

  # Helper: get zero-inflation probability (glmmTMB uses "zprob", others "zero")
  .get_zprob <- function() {
    ptype <- if (inherits(model, "glmmTMB")) "zprob" else "zero"
    stats::predict(model, type = ptype)
  }

  # handle models without zero-inflation component
  if (!faminfo$is_zero_inflated) {
    if (faminfo$is_poisson) {
      # Poisson: V = mu
      expected_var <- d$Predicted
    } else if (isTRUE(faminfo$family == "nbinom1")) {
      # nbinom1: V = mu(1 + phi)  [phi=0 is Poisson limit; larger phi = more OD]
      expected_var <- d$Predicted * (1 + .get_disp())
    } else if (isTRUE(faminfo$family == "nbinom2")) {
      # nbinom2: V = mu(1 + mu/phi)  [phi->inf is Poisson limit]
      expected_var <- d$Predicted * (1 + d$Predicted / .get_disp())
    } else if (faminfo$is_negbin) {
      # General negbin (e.g., MASS::glm.nb): parameterized as nbinom2
      expected_var <- d$Predicted * (1 + d$Predicted / insight::get_sigma(model))
    }
  } else if (faminfo$is_poisson) {
    # Zero-inflated Poisson: V = mu(1-p)(1 + mu*p)
    d$Prob <- .get_zprob()
    expected_var <- d$Predicted * (1 - d$Prob) * (1 + d$Predicted * d$Prob)
  } else if (isTRUE(faminfo$family == "nbinom1")) {
    # Zero-inflated nbinom1
    disp <- .get_disp()
    d$Prob <- .get_zprob()
    expected_var <- d$Predicted *
      (1 + disp) *
      (1 - d$Prob) *
      (1 + d$Predicted * (1 + disp) * d$Prob)
  } else if (isTRUE(faminfo$family == "nbinom2")) {
    # Zero-inflated nbinom2
    disp <- .get_disp()
    d$Prob <- .get_zprob()
    expected_var <- d$Predicted *
      (1 + d$Predicted / disp) *
      (1 - d$Prob) *
      (1 + d$Predicted * (1 + d$Predicted / disp) * d$Prob)
  } else if (faminfo$is_negbin) {
    # General zero-inflated negbin
    disp <- insight::get_sigma(model)
    d$Prob <- .get_zprob()
    expected_var <- d$Predicted *
      (1 + d$Predicted / disp) *
      (1 - d$Prob) *
      (1 + d$Predicted * (1 + d$Predicted / disp) * d$Prob)
  }

  # fallback for all other families
  if (is.null(expected_var)) {
    # find required arguments
    formal_args <- .safe(formals(stats::family(model)$variance))
    # error when not supported
    if (!is.null(formal_args)) {
      fun_args <- list()
      if ("mu" %in% names(formal_args)) {
        fun_args$mu <- d$Predicted
      }
      if ("theta" %in% names(formal_args)) {
        fun_args$theta <- .get_disp()
      }
      if ("phi" %in% names(formal_args)) {
        fun_args$phi <- .get_disp()
      }
      expected_var <- .safe(do.call(stats::family(model)$variance, fun_args))
    }
  }

  if (is.null(expected_var)) {
    insight::format_error(paste0(
      "Models of class `",
      class(model)[1],
      "` or model family `",
      faminfo$family,
      "` are not yet supported by `check_overdispersion()`. ",
      "Please file an issue at {.url https://github.com/easystats/performance/issues/}."
    ))
  }

  expected_var
}


.sigma_glmmTMB_nonmixed <- function(model, faminfo) {
  if (!is.na(match(faminfo$family, c("binomial", "poisson", "truncated_poisson")))) {
    return(1)
  }
  betad <- model$fit$par["betadisp"]
  switch(
    faminfo$family,
    gaussian = exp(0.5 * betad),
    Gamma = exp(-0.5 * betad),
    exp(betad)
  )
}
