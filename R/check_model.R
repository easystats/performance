#' @title Visual check of model assumptions
#' @name check_model
#'
#' @description
#' Visual check of various model assumptions (normality of residuals, normality
#' of random effects, linear relationship, homogeneity of variance,
#' multicollinearity).
#'
#' If `check_model()` doesn't work as expected, try setting `verbose = TRUE` to
#' get hints about possible problems.
#'
#' @param x A model object.
#' @param size_dot,size_line Size of line and dot-geoms.
#' @param base_size,size_title,size_axis_title Base font size for axis and plot titles.
#' @param panel Logical, if `TRUE`, plots are arranged as panels; else,
#' single plots for each diagnostic are returned.
#' @param check Character vector, indicating which checks for should be performed
#' and plotted. May be one or more of `"all"`, `"vif"`, `"qq"`, `"normality"`,
#' `"linearity"`, `"ncv"`, `"homogeneity"`, `"outliers"`, `"reqq"`, `"pp_check"`,
#' `"binned_residuals"` or `"overdispersion"`. Note that not all check apply
#' to all type of models (see 'Details'). `"reqq"` is a QQ-plot for random
#' effects and only available for mixed models. `"ncv"` is an alias for
#' `"linearity"`, and checks for non-constant variance, i.e. for
#' heteroscedasticity, as well as the linear relationship. By default, all
#' possible checks are performed and plotted.
#' @param alpha,alpha_dot The alpha level of the confidence bands and dot-geoms.
#' Scalar from 0 to 1.
#' @param colors Character vector with color codes (hex-format). Must be of
#' length 3. First color is usually used for reference lines, second color
#' for dots, and third color for outliers or extreme values.
#' @param theme String, indicating the name of the plot-theme. Must be in the
#' format `"package::theme_name"` (e.g. `"ggplot2::theme_minimal"`).
#' @param detrend Logical. Should Q-Q/P-P plots be detrended? Defaults to
#' `TRUE` for linear models or when `residual_type = "normal"`. Defaults to
#' `FALSE` for QQ plots based on simulated residuals (i.e. when
#' `residual_type = "simulated"`).
#' @param residual_type Character, indicating the type of residuals to be used.
#' For non-Gaussian models, the default is `"simulated"`, which uses simulated
#' residuals. These are based on [`simulate_residuals()`] and thus uses the
#' **DHARMa** package to return randomized quantile residuals. For Gaussian
#' models, the default is `"normal"`, which uses the default residuals from
#' the model. Setting `residual_type = "normal"` for non-Gaussian models will
#' use a half-normal Q-Q plot of the absolute value of the standardized deviance
#' residuals.
#' @param show_dots Logical, if `TRUE`, will show data points in the plot. Set
#' to `FALSE` for models with many observations, if generating the plot is too
#' time-consuming. By default, `show_dots = NULL`. In this case `check_model()`
#' tries to guess whether performance will be poor due to a very large model
#' and thus automatically shows or hides dots.
#' @param verbose If `FALSE` (default), suppress most warning messages.
#' @param ... Arguments passed down to the individual check functions, especially
#' to `check_predictions()` and `binned_residuals()`.
#' @inheritParams check_predictions
#'
#' @return The data frame that is used for plotting.
#'
#' @note This function just prepares the data for plotting. To create the plots,
#' **see** needs to be installed. Furthermore, this function suppresses
#' all possible warnings. In case you observe suspicious plots, please refer
#' to the dedicated functions (like `check_collinearity()`,
#' `check_normality()` etc.) to get informative messages and warnings.
#'
#' @details For Bayesian models from packages **rstanarm** or **brms**,
#' models will be "converted" to their frequentist counterpart, using
#' [`bayestestR::bayesian_as_frequentist`](https://easystats.github.io/bayestestR/reference/convert_bayesian_as_frequentist.html).
#' A more advanced model-check for Bayesian models will be implemented at a
#' later stage.
#'
#' See also the related [vignette](https://easystats.github.io/performance/articles/check_model.html).
#'
#' @section Posterior Predictive Checks:
#' Posterior predictive checks can be used to look for systematic discrepancies
#' between real and simulated data. It helps to see whether the type of model
#' (distributional family) fits well to the data. See [`check_predictions()`]
#' for further details.
#'
#' @section Linearity Assumption:
#' The plot **Linearity** checks the assumption of linear relationship.
#' However, the spread of dots also indicate possible heteroscedasticity (i.e.
#' non-constant variance, hence, the alias `"ncv"` for this plot), thus it shows
#' if residuals have non-linear patterns. This plot helps to see whether
#' predictors may have a non-linear relationship with the outcome, in which case
#' the reference line may roughly indicate that relationship. A straight and
#' horizontal line indicates that the model specification seems to be ok. But
#' for instance, if the line would be U-shaped, some of the predictors probably
#' should better be modeled as quadratic term. See [`check_heteroscedasticity()`]
#' for further details.
#'
#' **Some caution is needed** when interpreting these plots. Although these
#' plots are helpful to check model assumptions, they do not necessarily indicate
#' so-called "lack of fit", e.g. missed non-linear relationships or interactions.
#' Thus, it is always recommended to also look at
#' [effect plots, including partial residuals](https://strengejacke.github.io/ggeffects/articles/introduction_partial_residuals.html).
#'
#' @section Homogeneity of Variance:
#' This plot checks the assumption of equal variance (homoscedasticity). The
#' desired pattern would be that dots spread equally above and below a straight,
#' horizontal line and show no apparent deviation.
#'
#' @section Influential Observations:
#' This plot is used to identify influential observations. If any points in this
#' plot fall outside of Cook’s distance (the dashed lines) then it is considered
#' an influential observation. See [`check_outliers()`] for further details.
#'
#' @section Multicollinearity:
#' This plot checks for potential collinearity among predictors. In a nutshell,
#' multicollinearity means that once you know the effect of one predictor, the
#' value of knowing the other predictor is rather low. Multicollinearity might
#' arise when a third, unobserved variable has a causal effect on each of the
#' two predictors that are associated with the outcome. In such cases, the actual
#' relationship that matters would be the association between the unobserved
#' variable and the outcome. See [`check_collinearity()`] for further details.
#'
#' @section Normality of Residuals:
#' This plot is used to determine if the residuals of the regression model are
#' normally distributed. Usually, dots should fall along the line. If there is
#' some deviation (mostly at the tails), this indicates that the model doesn't
#' predict the outcome well for that range that shows larger deviations from
#' the line. For generalized linear models and when `residual_type = "normal"`,
#' a half-normal Q-Q plot of the absolute value of the standardized deviance
#' residuals is shown, however, the interpretation of the plot remains the same.
#' See [`check_normality()`] for further details. Usually, for generalized linear
#' (mixed) models, a test for uniformity of residuals based on simulated residuals
#' is conducted (see next section).
#'
#' @section Uniformity of Residuals:
#' Fore non-Gaussian models, when `residual_type = "simulated"` (the default
#' for generalized linear (mixed) models), residuals are not expected to be
#' normally distributed. In this case, the created Q-Q plot checks the uniformity
#' of residuals. The interpretation of the plot is the same as for the normal
#' Q-Q plot. See [`simulate_residuals()`] and [`check_residuals()`] for further
#' details.
#'
#' @section Overdispersion:
#' For count models, an *overdispersion plot* is shown. Overdispersion occurs
#' when the observed variance is higher than the variance of a theoretical model.
#' For Poisson models, variance increases with the mean and, therefore, variance
#' usually (roughly) equals the mean value. If the variance is much higher,
#' the data are "overdispersed". See [`check_overdispersion()`] for further
#' details.
#'
#' @section Binned Residuals:
#' For models from binomial families, a *binned residuals plot* is shown.
#' Binned residual plots are achieved by cutting the the data into bins and then
#' plotting the average residual versus the average fitted value for each bin.
#' If the model were true, one would expect about 95% of the residuals to fall
#' inside the error bounds. See [`binned_residuals()`] for further details.
#'
#' @section Residuals for (Generalized) Linear Models:
#' Plots that check the homogeneity of variance use standardized Pearson's
#' residuals for generalized linear models, and standardized residuals for
#' linear models. The plots for the normality of residuals (with overlayed
#' normal curve) and for the linearity assumption use the default residuals
#' for `lm` and `glm` (which are deviance residuals for `glm`). The Q-Q plots
#' use simulated residuals (see [`simulate_residuals()`]) for non-Gaussian
#' models and standardized residuals for linear models.
#'
#' @section Troubleshooting:
#' For models with many observations, or for more complex models in general,
#' generating the plot might become very slow. One reason might be that the
#' underlying graphic engine becomes slow for plotting many data points. In
#' such cases, setting the argument `show_dots = FALSE` might help. Furthermore,
#' look at the `check` argument and see if some of the model checks could be
#' skipped, which also increases performance.
#'
#' If `check_model()` doesn't work as expected, try setting `verbose = TRUE` to
#' get hints about possible problems.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examplesIf require("lme4")
#' \donttest{
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_model(m)
#'
#' data(sleepstudy, package = "lme4")
#' m <- lme4::lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#' check_model(m, panel = FALSE)
#' }
#' @export
check_model <- function(x, ...) {
  UseMethod("check_model")
}



# default ----------------------------

#' @rdname check_model
#' @export
check_model.default <- function(x,
                                panel = TRUE,
                                check = "all",
                                detrend = TRUE,
                                bandwidth = "nrd",
                                type = "density",
                                residual_type = NULL,
                                show_dots = NULL,
                                size_dot = 2,
                                size_line = 0.8,
                                size_title = 12,
                                size_axis_title = base_size,
                                base_size = 10,
                                alpha = 0.2,
                                alpha_dot = 0.8,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                theme = "see::theme_lucid",
                                verbose = FALSE,
                                ...) {
  # check model formula
  if (verbose) {
    insight::formula_ok(x)
  }

  minfo <- insight::model_info(x, verbose = FALSE)

  # set default for residual_type
  if (is.null(residual_type)) {
    residual_type <- ifelse(minfo$is_linear && !minfo$is_gam, "normal", "simulated")
  }

  # catch models/families not supported by DHARMa - we need to add more
  # exceptions here as they appear, but for now, `check_model()` also
  # automatically falls back to normal Q-Q plot for all models not supported
  # by DHARMa
  if (minfo$family %in% c("quasipoisson", "quasibinomial")) {
    residual_type <- "normal"
  }

  # set default for detrend
  if (missing(detrend)) {
    detrend <- residual_type == "normal"
  }

  assumptions_data <- tryCatch(
    if (minfo$is_bayesian) {
      suppressWarnings(.check_assumptions_stan(x, ...))
    } else if (minfo$is_linear) {
      suppressWarnings(.check_assumptions_linear(x, minfo, check, residual_type, verbose, ...))
    } else {
      suppressWarnings(.check_assumptions_glm(x, minfo, check, residual_type, verbose, ...))
    },
    error = function(e) {
      e
    }
  )

  if (inherits(assumptions_data, c("error", "simpleError"))) {
    pattern <- "(\n|\\s{2,})"
    replacement <- " "
    cleaned_string <- gsub(pattern, replacement, assumptions_data$message)
    insight::format_error(
      paste("`check_model()` returned following error:", cleaned_string),
      paste0("\nIf the error message does not help identifying your problem, another reason why `check_model()` failed might be that models of class `", class(x)[1], "` are not yet supported.") # nolint
    )
  }

  # did Q-Q plot work with simulated residuals?
  if (is.null(assumptions_data$QQ) && residual_type == "simulated") {
    insight::format_alert(paste0(
      "Cannot simulate residuals for models of class `",
      class(x)[1],
      "`. Please try `check_model(..., residual_type = \"normal\")` instead."
    ))
  }

  # try to find sensible default for "type" argument
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical || minfo$is_multinomial) # nolint
  if (missing(type) && suggest_dots) {
    type <- "discrete_interval"
  }

  # set default for show_dots, based on "model size"
  if (is.null(show_dots)) {
    n <- .safe(insight::n_obs(x))
    show_dots <- is.null(n) || n <= 1e5
  }

  attr(assumptions_data, "panel") <- panel
  attr(assumptions_data, "dot_size") <- size_dot
  attr(assumptions_data, "line_size") <- size_line
  attr(assumptions_data, "base_size") <- base_size
  attr(assumptions_data, "axis_title_size") <- size_axis_title
  attr(assumptions_data, "title_size") <- size_title
  attr(assumptions_data, "check") <- check
  attr(assumptions_data, "alpha") <- alpha
  attr(assumptions_data, "dot_alpha") <- alpha_dot
  attr(assumptions_data, "show_dots") <- isTRUE(show_dots)
  attr(assumptions_data, "detrend") <- detrend
  attr(assumptions_data, "colors") <- colors
  attr(assumptions_data, "theme") <- theme
  attr(assumptions_data, "model_info") <- minfo
  attr(assumptions_data, "overdisp_type") <- list(...)$plot_type
  attr(assumptions_data, "bandwidth") <- bandwidth
  attr(assumptions_data, "type") <- type
  attr(assumptions_data, "model_class") <- class(x)[1]
  assumptions_data
}


# methods ----------------------------------

#' @export
print.check_model <- function(x, ...) {
  insight::check_if_installed("see", "for model diagnostic plots")
  NextMethod()
}

#' @export
plot.check_model <- function(x, ...) {
  insight::check_if_installed("see", "for model diagnostic plots")
  NextMethod()
}



# other classes ---------------------------

## TODO for now, convert to freq, see https://github.com/easystats/performance/issues/354
## need to fix this later

#' @export
check_model.stanreg <- function(x,
                                panel = TRUE,
                                check = "all",
                                detrend = TRUE,
                                bandwidth = "nrd",
                                type = "density",
                                residual_type = NULL,
                                show_dots = NULL,
                                size_dot = 2,
                                size_line = 0.8,
                                size_title = 12,
                                size_axis_title = base_size,
                                base_size = 10,
                                alpha = 0.2,
                                alpha_dot = 0.8,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                theme = "see::theme_lucid",
                                verbose = FALSE,
                                ...) {
  check_model(bayestestR::bayesian_as_frequentist(x),
    size_dot = size_dot,
    size_line = size_line,
    panel = panel,
    check = check,
    alpha = alpha,
    alpha_dot = alpha_dot,
    colors = colors,
    theme = theme,
    base_size = base_size,
    size_axis_title = size_axis_title,
    detrend = detrend,
    show_dots = show_dots,
    bandwidth = bandwidth,
    type = type,
    residual_type = residual_type,
    verbose = verbose,
    ...
  )
}


#' @export
check_model.brmsfit <- check_model.stanreg


#' @export
check_model.model_fit <- function(x,
                                  panel = TRUE,
                                  check = "all",
                                  detrend = TRUE,
                                  bandwidth = "nrd",
                                  type = "density",
                                  residual_type = NULL,
                                  show_dots = NULL,
                                  size_dot = 2,
                                  size_line = 0.8,
                                  size_title = 12,
                                  size_axis_title = base_size,
                                  base_size = 10,
                                  alpha = 0.2,
                                  alpha_dot = 0.8,
                                  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                  theme = "see::theme_lucid",
                                  verbose = FALSE,
                                  ...) {
  check_model(
    x$fit,
    size_dot = size_dot,
    size_line = size_line,
    panel = panel,
    check = check,
    alpha = alpha,
    size_axis_title = size_axis_title,
    alpha_dot = alpha_dot,
    colors = colors,
    theme = theme,
    base_size = base_size,
    detrend = detrend,
    show_dots = show_dots,
    bandwidth = bandwidth,
    type = type,
    residual_type = residual_type,
    verbose = verbose,
    ...
  )
}


#' @export
check_model.performance_simres <- function(x,
                                           panel = TRUE,
                                           check = "all",
                                           detrend = TRUE,
                                           bandwidth = "nrd",
                                           type = "density",
                                           residual_type = NULL,
                                           show_dots = NULL,
                                           size_dot = 2,
                                           size_line = 0.8,
                                           size_title = 12,
                                           size_axis_title = base_size,
                                           base_size = 10,
                                           alpha = 0.2,
                                           alpha_dot = 0.8,
                                           colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                           theme = "see::theme_lucid",
                                           verbose = FALSE,
                                           ...) {
  check_model(
    x$fittedModel,
    size_dot = size_dot,
    size_line = size_line,
    panel = panel,
    check = check,
    alpha = alpha,
    alpha_dot = alpha_dot,
    size_axis_title = size_axis_title,
    colors = colors,
    theme = theme,
    base_size = base_size,
    detrend = detrend,
    show_dots = show_dots,
    bandwidth = bandwidth,
    type = type,
    residual_type = "simulated",
    verbose = verbose,
    ...
  )
}

#' @export
check_model.DHARMa <- check_model.performance_simres



# compile plots for checks of linear models  ------------------------

.check_assumptions_linear <- function(model, model_info, check = "all", residual_type = "normal", verbose = TRUE, ...) {
  dat <- list()

  # multicollinearity --------------
  if (any(c("all", "vif") %in% check)) {
    dat$VIF <- .model_diagnostic_vif(model, verbose = verbose)
  }

  # Q-Q plot (normality/uniformity of residuals) --------------
  if (any(c("all", "qq") %in% check)) {
    dat$QQ <- switch(residual_type,
      simulated = .safe(simulate_residuals(model, ...)),
      .model_diagnostic_qq(model, model_info = model_info, verbose = verbose)
    )
  }

  # Random Effects Q-Q plot (normality of BLUPs) --------------
  if (any(c("all", "reqq") %in% check)) {
    dat$REQQ <- .model_diagnostic_ranef_qq(model, level = 0.95, model_info = model_info, verbose = verbose)
  }

  # normal-curve plot (normality of residuals) --------------
  if (any(c("all", "normality") %in% check)) {
    dat$NORM <- .model_diagnostic_normality(model, verbose = verbose)
  }

  # non-constant variance (heteroskedasticity, liniearity) --------------
  if (any(c("all", "ncv", "linearity") %in% check)) {
    dat$NCV <- .model_diagnostic_ncv(model, verbose = verbose)
  }

  # homogeneity of variance --------------
  if (any(c("all", "homogeneity") %in% check)) {
    dat$HOMOGENEITY <- .model_diagnostic_homogeneity(model, verbose = verbose)
  }

  # outliers --------------
  if (any(c("all", "outliers") %in% check)) {
    dat$OUTLIERS <- .safe(check_outliers(model, method = "cook"))
    if (is.null(dat$OUTLIERS)) {
      threshold <- NULL
    } else {
      threshold <- attributes(dat$OUTLIERS)$threshold$cook
    }
    dat$INFLUENTIAL <- .safe(.model_diagnostic_outlier(model, threshold = threshold))
  }

  # posterior predictive checks --------------
  if (any(c("all", "pp_check") %in% check)) {
    dat$PP_CHECK <- .safe(check_predictions(model, verbose = verbose, ...))
  }

  dat <- insight::compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}



# compile plots for checks of generalized linear models  ------------------------

.check_assumptions_glm <- function(model, model_info, check = "all", residual_type = "simulated", verbose = TRUE, ...) {
  dat <- list()

  # multicollinearity --------------
  if (any(c("all", "vif") %in% check)) {
    dat$VIF <- .model_diagnostic_vif(model, verbose = verbose)
  }

  # Q-Q plot (normality/uniformity of residuals) --------------
  if (any(c("all", "qq") %in% check)) {
    dat$QQ <- switch(residual_type,
      simulated = .safe(simulate_residuals(model, ...)),
      .model_diagnostic_qq(model, model_info = model_info, verbose = verbose)
    )
  }

  # homogeneity of variance --------------
  if (any(c("all", "homogeneity") %in% check)) {
    dat$HOMOGENEITY <- .model_diagnostic_homogeneity(model, verbose = verbose)
  }

  # Random Effects Q-Q plot (normality of BLUPs) --------------
  if (any(c("all", "reqq") %in% check)) {
    dat$REQQ <- .model_diagnostic_ranef_qq(model, level = 0.95, model_info = model_info, verbose = verbose)
  }

  # outliers --------------
  if (any(c("all", "outliers") %in% check)) {
    dat$OUTLIERS <- .safe(check_outliers(model, method = "cook"))
    if (is.null(dat$OUTLIERS)) {
      threshold <- NULL
    } else {
      threshold <- attributes(dat$OUTLIERS)$threshold$cook
    }
    dat$INFLUENTIAL <- .safe(.model_diagnostic_outlier(model, threshold = threshold))
  }

  # posterior predictive checks --------------
  if (any(c("all", "pp_check") %in% check)) {
    dat$PP_CHECK <- .safe(check_predictions(model, verbose = verbose, ...))
  }

  # binned residuals for bernoulli/binomial --------------
  if (isTRUE(model_info$is_binomial) && any(c("all", "binned_residuals") %in% check)) {
    dat$BINNED_RESID <- .safe(binned_residuals(model, verbose = verbose, ...))
  }

  # misspecified dispersion and zero-inflation --------------
  if (isTRUE(model_info$is_count) && any(c("all", "overdispersion") %in% check)) {
    dat$OVERDISPERSION <- .model_diagnostic_overdispersion(model)
  }

  dat <- insight::compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}



# compile plots for checks of Bayesian models  ------------------------

.check_assumptions_stan <- function(model, ...) {
  if (inherits(model, "brmsfit")) {
    # check if brms can be loaded

    if (!requireNamespace("brms", quietly = TRUE)) {
      insight::format_error("Package `brms` needs to be loaded first!")
    }

    # check if prior sample are available

    d2 <- brms::prior_samples(model)

    if (is.null(d2)) {
      insight::format_error(
        "No prior-samples found. Please use option `sample_prior = TRUE` when fitting the model."
      )
    }

    d1 <- brms::posterior_samples(model)

    # get samples from posterior and prior

    d1 <- d1[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d1), perl = TRUE)]
    d2 <- d2[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d2), perl = TRUE)]
  } else if (inherits(model, c("stanreg", "stanfit"))) {
    # check if rstanarm can be loaded
    if (!requireNamespace("rstanarm", quietly = TRUE)) {
      insight::format_error("Package `rstanarm` needs to be loaded first!")
    }


    # get samples from posterior and prior

    prior <- suppressWarnings(
      stats::update(
        model,
        prior_PD = TRUE,
        refresh = -1,
        iter = 2000,
        chains = 2
      )
    )

    d1 <- as.data.frame(model)
    d2 <- as.data.frame(prior)


    # remove intercept from output for ridgeline plot.
    # this would increase the range of the scale too much

    if (insight::object_has_names(d1, "(Intercept)")) {
      d1 <- datawizard::data_remove(d1, "(Intercept)")
    }

    if (insight::object_has_names(d2, "(Intercept)")) {
      d2 <- datawizard::data_remove(d2, "(Intercept)")
    }

    if (insight::object_has_names(d1, "sigma")) {
      d1 <- datawizard::data_remove(d1, "sigma")
    }

    if (insight::object_has_names(d2, "sigma")) {
      d2 <- datawizard::data_remove(d2, "sigma")
    }

    d1 <- d1[, grepl(pattern = "^(?!(b\\[\\(Intercept\\)|Sigma\\[))(.*)", colnames(d1), perl = TRUE)]
    d2 <- d2[, grepl(pattern = "^(?!(b\\[\\(Intercept\\)|Sigma\\[))(.*)", colnames(d2), perl = TRUE)]
  }


  # grouping variable

  d1$group <- "Posterior"
  d2$group <- "Prior"

  gather.cols <- colnames(d1)[1:(ncol(d1) - 1)]

  dat <- stats::reshape(
    rbind(d1, d2),
    idvar = "id",
    times = gather.cols,
    timevar = "y",
    v.names = "x",
    varying = gather.cols,
    direction = "long"
  )

  class(dat) <- c("check_model", "see_check_model", "data.frame")
  dat
}
