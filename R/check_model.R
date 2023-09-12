#' @title Visual check of model assumptions
#' @name check_model
#'
#' @description
#'
#' Visual check of various model assumptions (normality of residuals, normality
#' of random effects, linear relationship, homogeneity of variance,
#' multicollinearity).
#'
#' @param x A model object.
#' @param dot_size,line_size Size of line and dot-geoms.
#' @param panel Logical, if `TRUE`, plots are arranged as panels; else,
#' single plots for each diagnostic are returned.
#' @param check Character vector, indicating which checks for should be performed
#'   and plotted. May be one or more of `"all"`, `"vif"`, `"qq"`, `"normality"`,
#'   `"linearity"`, `"ncv"`, `"homogeneity"`, `"outliers"`, `"reqq"`, `"pp_check"`,
#'   `"binned_residuals"` or `"overdispersion"`, Not that not all check apply
#'   to all type of models (see 'Details'). `"reqq"` is a QQ-plot for random
#'   effects and only available for mixed models. `"ncv"` is an alias for
#'   `"linearity"`, and checks for non-constant variance, i.e. for
#'   heteroscedasticity, as well as the linear relationship. By default, all
#'   possible checks are performed and plotted.
#' @param alpha,dot_alpha The alpha level of the confidence bands and dot-geoms.
#'   Scalar from 0 to 1.
#' @param colors Character vector with color codes (hex-format). Must be of
#'   length 3. First color is usually used for reference lines, second color
#'   for dots, and third color for outliers or extreme values.
#' @param theme String, indicating the name of the plot-theme. Must be in the
#'   format `"package::theme_name"` (e.g. `"ggplot2::theme_minimal"`).
#' @param detrend Logical. Should Q-Q/P-P plots be detrended? Defaults to
#'   `TRUE`.
#' @param show_dots Logical, if `TRUE`, will show data points in the plot. Set
#'   to `FALSE` for models with many observations, if generating the plot is too
#'   time-consuming. By default, `show_dots = NULL`. In this case `check_model()`
#'   tries to guess whether performance will be poor due to a very large model
#'   and thus automatically shows or hides dots.
#' @param verbose If `FALSE` (default), suppress most warning messages.
#' @param ... Currently not used.
#' @inheritParams check_predictions
#'
#' @return The data frame that is used for plotting.
#'
#' @note This function just prepares the data for plotting. To create the plots,
#'   **see** needs to be installed. Furthermore, this function suppresses
#'   all possible warnings. In case you observe suspicious plots, please refer
#'   to the dedicated functions (like `check_collinearity()`,
#'   `check_normality()` etc.) to get informative messages and warnings.
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
#' the line. For generalized linear models, a half-normal Q-Q plot of the
#' absolute value of the standardized deviance residuals is shown, however, the
#' interpretation of the plot remains the same. See [`check_normality()`] for
#' further details.
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
#' Plots that check the normality of residuals (QQ-plot) or the homogeneity of
#' variance use standardized Pearson's residuals for generalized linear models,
#' and standardized residuals for linear models. The plots for the normality of
#' residuals (with overlayed normal curve) and for the linearity assumption use
#' the default residuals for `lm` and `glm` (which are deviance
#' residuals for `glm`).
#'
#' @section Troubleshooting:
#' For models with many observations, or for more complex models in general,
#' generating the plot might become very slow. One reason might be that the
#' underlying graphic engine becomes slow for plotting many data points. In
#' such cases, setting the argument `show_dots = FALSE` might help. Furthermore,
#' look at the `check` argument and see if some of the model checks could be
#' skipped, which also increases performance.
#'
#' @family functions to check model assumptions and and assess model quality
#'
#' @examples
#' \dontrun{
#' m <- lm(mpg ~ wt + cyl + gear + disp, data = mtcars)
#' check_model(m)
#'
#' if (require("lme4")) {
#'   m <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
#'   check_model(m, panel = FALSE)
#' }
#'
#' if (require("rstanarm")) {
#'   m <- stan_glm(mpg ~ wt + gear, data = mtcars, chains = 2, iter = 200)
#'   check_model(m)
#' }
#' }
#' @export
check_model <- function(x, ...) {
  UseMethod("check_model")
}



# default ----------------------------

#' @rdname check_model
#' @export
check_model.default <- function(x,
                                dot_size = 2,
                                line_size = 0.8,
                                panel = TRUE,
                                check = "all",
                                alpha = 0.2,
                                dot_alpha = 0.8,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                theme = "see::theme_lucid",
                                detrend = TRUE,
                                show_dots = NULL,
                                bandwidth = "nrd",
                                type = "density",
                                verbose = FALSE,
                                ...) {
  # check model formula
  if (verbose) {
    insight::formula_ok(x)
  }

  minfo <- insight::model_info(x, verbose = FALSE)

  ca <- tryCatch(
    {
      if (minfo$is_bayesian) {
        suppressWarnings(.check_assumptions_stan(x))
      } else if (minfo$is_linear) {
        suppressWarnings(.check_assumptions_linear(x, minfo, verbose))
      } else {
        suppressWarnings(.check_assumptions_glm(x, minfo, verbose))
      }
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(ca)) {
    insight::format_error(paste0("`check_model()` not implemented for models of class `", class(x)[1], "` yet."))
  }

  # try to find sensible default for "type" argument
  suggest_dots <- (minfo$is_bernoulli || minfo$is_count || minfo$is_ordinal || minfo$is_categorical || minfo$is_multinomial)
  if (missing(type) && suggest_dots) {
    type <- "discrete_interval"
  }

  # set default for show_dots, based on "model size"
  if (is.null(show_dots)) {
    n <- .safe(insight::n_obs(x))
    show_dots <- is.null(n) || n <= 1e5
  }

  attr(ca, "panel") <- panel
  attr(ca, "dot_size") <- dot_size
  attr(ca, "line_size") <- line_size
  attr(ca, "check") <- check
  attr(ca, "alpha") <- alpha
  attr(ca, "dot_alpha") <- dot_alpha
  attr(ca, "show_dots") <- isTRUE(show_dots)
  attr(ca, "detrend") <- detrend
  attr(ca, "colors") <- colors
  attr(ca, "theme") <- theme
  attr(ca, "model_info") <- minfo
  attr(ca, "overdisp_type") <- list(...)$plot_type
  attr(ca, "bandwidth") <- bandwidth
  attr(ca, "type") <- type
  ca
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
                                dot_size = 2,
                                line_size = 0.8,
                                panel = TRUE,
                                check = "all",
                                alpha = 0.2,
                                dot_alpha = 0.8,
                                colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                theme = "see::theme_lucid",
                                detrend = FALSE,
                                show_dots = NULL,
                                bandwidth = "nrd",
                                type = "density",
                                verbose = TRUE,
                                ...) {
  check_model(bayestestR::bayesian_as_frequentist(x),
    dot_size = dot_size,
    line_size = line_size,
    panel = panel,
    check = check,
    alpha = alpha,
    dot_alpha = dot_alpha,
    colors = colors,
    theme = theme,
    detrend = detrend,
    show_dots = show_dots,
    bandwidth = bandwidth,
    type = type,
    verbose = verbose,
    ...
  )
}


#' @export
check_model.brmsfit <- check_model.stanreg


#' @export
check_model.model_fit <- function(x,
                                  dot_size = 2,
                                  line_size = 0.8,
                                  panel = TRUE,
                                  check = "all",
                                  alpha = 0.2,
                                  dot_alpha = 0.8,
                                  colors = c("#3aaf85", "#1b6ca8", "#cd201f"),
                                  theme = "see::theme_lucid",
                                  detrend = FALSE,
                                  show_dots = NULL,
                                  bandwidth = "nrd",
                                  type = "density",
                                  verbose = TRUE,
                                  ...) {
  check_model(
    x$fit,
    dot_size = dot_size,
    line_size = line_size,
    panel = panel,
    check = check,
    alpha = alpha,
    dot_alpha = dot_alpha,
    colors = colors,
    theme = theme,
    detrend = detrend,
    show_dots = show_dots,
    bandwidth = bandwidth,
    type = type,
    verbose = verbose,
    ...
  )
}



# compile plots for checks of linear models  ------------------------

.check_assumptions_linear <- function(model, model_info, verbose = TRUE) {
  dat <- list()

  dat$VIF <- .diag_vif(model, verbose = verbose)
  dat$QQ <- .diag_qq(model, verbose = verbose)
  dat$REQQ <- .diag_reqq(model, level = 0.95, model_info = model_info, verbose = verbose)
  dat$NORM <- .diag_norm(model, verbose = verbose)
  dat$NCV <- .diag_ncv(model, verbose = verbose)
  dat$HOMOGENEITY <- .diag_homogeneity(model, verbose = verbose)
  dat$OUTLIERS <- check_outliers(model, method = "cook")
  if (!is.null(dat$OUTLIERS)) {
    threshold <- attributes(dat$OUTLIERS)$threshold$cook
  } else {
    threshold <- NULL
  }
  dat$INFLUENTIAL <- .influential_obs(model, threshold = threshold)
  dat$PP_CHECK <- .safe(check_predictions(model))

  dat <- insight::compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}



# compile plots for checks of generalized linear models  ------------------------

.check_assumptions_glm <- function(model, model_info, verbose = TRUE) {
  dat <- list()

  dat$VIF <- .diag_vif(model, verbose = verbose)
  dat$QQ <- .diag_qq(model, verbose = verbose)
  dat$HOMOGENEITY <- .diag_homogeneity(model, verbose = verbose)
  dat$REQQ <- .diag_reqq(model, level = 0.95, model_info = model_info, verbose = verbose)
  dat$OUTLIERS <- check_outliers(model, method = "cook")
  if (!is.null(dat$OUTLIERS)) {
    threshold <- attributes(dat$OUTLIERS)$threshold$cook
  } else {
    threshold <- NULL
  }
  dat$INFLUENTIAL <- .influential_obs(model, threshold = threshold)
  dat$PP_CHECK <- .safe(check_predictions(model))
  if (isTRUE(model_info$is_binomial)) {
    dat$BINNED_RESID <- binned_residuals(model)
  }
  if (isTRUE(model_info$is_count)) {
    dat$OVERDISPERSION <- .diag_overdispersion(model)
  }

  dat <- insight::compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}



# compile plots for checks of Bayesian models  ------------------------

.check_assumptions_stan <- function(model) {
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
