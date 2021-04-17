#' @title Visual check of model assumptions
#' @name check_model
#'
#' @description Visual check of model various assumptions (normality of residuals,
#' normality of random effects, linear relationship, homogeneity of variance,
#' multicollinearity).
#'
#' @param x A model object.
#' @param dot_size,line_size Size of line and dot-geoms.
#' @param panel Logical, if \code{TRUE}, plots are arranged as panels; else,
#' single plots for each diagnostic are returned.
#' @param check Character vector, indicating which checks for should be performed
#'   and plotted. May be one or more of
#'   \code{"all", "vif", "qq", "normality", "linearity", "ncv", "homogeneity", "outliers", "reqq"}.
#'   \code{"reqq"} is a QQ-plot for random effects and only available for mixed models.
#'   \code{"ncv"} is an alias for \code{"linearity"}, and checks for non-constant
#'   variance, i.e. for heteroscedasticity, as well as the linear relationship.
#'   By default, all possible checks are performed and plotted.
#' @param alpha,dot_alpha The alpha level of the confidence bands and dot-geoms.
#'   Scalar from 0 to 1.
#' @param colors Character vector with color codes (hex-format). Must be of
#'   length 3. First color is usually used for reference lines, second color
#'   for dots, and third color for outliers or extreme values.
#' @param theme String, indicating the name of the plot-theme. Must be in the
#'   format \code{"package::theme_name"} (e.g. \code{"ggplot2::theme_minimal"}).
#' @param detrend Should QQ/PP plots be detrended?
#' @param ... Currently not used.
#'
#' @return The data frame that is used for plotting.
#'
#' @note This function just prepares the data for plotting. To create the plots,
#' \CRANpkg{see} needs to be installed. Furthermore, this function suppresses
#' all possible warnings. In case you observe suspicious plots, please refer to
#' the dedicated functions (like \code{check_collinearity()}, \code{check_normality()}
#' etc.) to get informative messages and warnings.
#'
#' @section Linearity Assumption:
#' The plot \strong{Linearity} checks the assumption of linear
#' relationship. However, the spread of dots also indicate possible
#' heteroscedasticity (i.e. non-constant variance); hence, the alias \code{"ncv"}
#' for this plot. \strong{Some caution is needed} when interpreting these plots.
#' Although these plots are helpful to check model assumptions, they do not
#' necessarily indicate so-called "lack of fit", e.g. missed non-linear
#' relationships or interactions. Thus, it is always recommended to also look
#' at \href{https://strengejacke.github.io/ggeffects/articles/introduction_partial_residuals.html}{effect plots, including partial residuals}.
#'
#' @section Residuals for (Generalized) Linear Models:
#' Plots that check the normality of residuals (QQ-plot) or the homogeneity of
#' variance use standardized Pearson's residuals for generalized linear models,
#' and standardized residuals for linear models. The plots for the normality of
#' residuals (with overlayed normal curve) and for the linearity assumption use
#' the default residuals for \code{lm} and \code{glm} (which are deviance
#' residuals for \code{glm}).
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


#' @rdname check_model
#' @export
check_model.default <- function(x, dot_size = 2, line_size = .8, panel = TRUE, check = "all", alpha = .2, dot_alpha = .8, colors = c("#3aaf85", "#1b6ca8", "#cd201f"), theme = "see::theme_lucid", detrend = FALSE, ...) {
  minfo <- insight::model_info(x)

  if (minfo$is_bayesian) {
    ca <- suppressWarnings(.check_assumptions_stan(x))
  } else if (minfo$is_linear) {
    ca <- suppressWarnings(.check_assumptions_linear(x, minfo))
  } else {
    ca <- suppressWarnings(.check_assumptions_glm(x, minfo))
  }
  # else {
  #   stop(paste0("`check_assumptions()` not implemented for models of class '", class(x)[1], "' yet."), call. = FALSE)
  # }

  attr(ca, "panel") <- panel
  attr(ca, "dot_size") <- dot_size
  attr(ca, "line_size") <- line_size
  attr(ca, "check") <- check
  attr(ca, "alpha") <- alpha
  attr(ca, "dot_alpha") <- dot_alpha
  attr(ca, "detrend") <- detrend
  attr(ca, "colors") <- colors
  attr(ca, "theme") <- theme
  ca
}


#' @export
check_model.model_fit <- function(x, dot_size = 2, line_size = .8, panel = TRUE, check = "all", alpha = .2, detrend = FALSE, ...) {
  check_model(x$fit, dot_size = dot_size, line_size = line_size, panel = panel, check = check, alpha = alpha, detrend = detrend, ...)
}





.check_assumptions_linear <- function(model, model_info) {
  dat <- list()

  dat$VIF <- .diag_vif(model)
  dat$QQ <- .diag_qq(model)
  dat$REQQ <- .diag_reqq(model, level = .95, model_info = model_info)
  dat$NORM <- .diag_norm(model)
  dat$NCV <- .diag_ncv(model)
  dat$HOMOGENEITY <- .diag_homogeneity(model)
  dat$OUTLIERS <- check_outliers(model, method = "cook")
  if (!is.null(dat$OUTLIERS)) {
    threshold <- attributes(dat$OUTLIERS)$threshold$cook
  } else {
    threshold <- NULL
  }
  dat$INFLUENTIAL <- .diag_influential_obs(model, threshold = threshold)

  dat <- .compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}




.check_assumptions_glm <- function(model, model_info) {
  dat <- list()

  dat$VIF <- .diag_vif(model)
  dat$QQ <- .diag_qq(model)
  dat$HOMOGENEITY <- .diag_homogeneity(model)
  dat$REQQ <- .diag_reqq(model, level = .95, model_info = model_info)
  dat$OUTLIERS <- check_outliers(model, method = "cook")
  if (!is.null(dat$OUTLIERS)) {
    threshold <- attributes(dat$OUTLIERS)$threshold$cook
  } else {
    threshold <- NULL
  }
  dat$INFLUENTIAL <- .diag_influential_obs(model, threshold = threshold)

  dat <- .compact_list(dat)
  class(dat) <- c("check_model", "see_check_model")
  dat
}




.check_assumptions_stan <- function(model) {
  if (inherits(model, "brmsfit")) {

    # check if brms can be loaded

    if (!requireNamespace("brms", quietly = TRUE)) {
      stop("Package `brms` needs to be loaded first!", call. = FALSE)
    }

    # check if prior sample are available

    d2 <- brms::prior_samples(model)

    if (is.null(d2)) {
      stop("No prior-samples found. Please use option `sample_prior = TRUE` when fitting the model.", call. = FALSE)
    }

    d1 <- brms::posterior_samples(model)

    # get samples from posterior and prior

    d1 <- d1[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d1), perl = TRUE)]
    d2 <- d2[, grepl(pattern = "(b_|bs_|bsp_|bcs_)(?!(Intercept|zi_Intercept))(.*)", colnames(d2), perl = TRUE)]
  } else if (inherits(model, c("stanreg", "stanfit"))) {

    # check if rstanarm can be loaded
    if (!requireNamespace("rstanarm", quietly = TRUE)) {
      stop("Package `rstanarm` needs to be loaded first!", call. = FALSE)
    }


    # get samples from posterior and prior

    prior <- suppressWarnings(
      stats::update(model, prior_PD = TRUE, refresh = -1, iter = 2000, chains = 2)
    )

    d1 <- as.data.frame(model)
    d2 <- as.data.frame(prior)


    # remove intercept from output for ridgeline plot.
    # this would increase the range of the scale too much

    if (.obj_has_name(d1, "(Intercept)")) {
      d1 <- .remove_column(d1, "(Intercept)")
    }

    if (.obj_has_name(d2, "(Intercept)")) {
      d2 <- .remove_column(d2, "(Intercept)")
    }

    if (.obj_has_name(d1, "sigma")) {
      d1 <- .remove_column(d1, "sigma")
    }

    if (.obj_has_name(d2, "sigma")) {
      d2 <- .remove_column(d2, "sigma")
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
