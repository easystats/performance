#' @title Binned residuals for binomial logistic regression
#' @name binned_residuals
#'
#' @description Check model quality of binomial logistic regression models.
#'
#' @param model A `glm`-object with *binomial*-family.
#' @param term Name of independent variable from `x`. If not `NULL`,
#'   average residuals for the categories of `term` are plotted; else,
#'   average residuals for the estimated probabilities of the response are
#'   plotted.
#' @param n_bins Numeric, the number of bins to divide the data. If
#'   `n_bins = NULL`, the square root of the number of observations is
#'   taken.
#' @param ci Numeric, the confidence level for the error bounds.
#' @param ci_type Character, the type of error bounds to calculate. Can be
#'   `"gaussian"` (default), `"exact"` or `"boot"`.
#' @param residuals Character, the type of residuals to calculate. Can be
#'   `"response"` (default), `"pearson"` or `"deviance"`.
#' @param iterations Integer, the number of iterations to use for the
#'   bootstrap method. Only used if `ci_type = "boot"`.
#' @param show_dots Logical, if `TRUE`, will show data points in the plot. Set
#'   to `FALSE` for models with many observations, if generating the plot is too
#'   time-consuming. By default, `show_dots = NULL`. In this case `binned_residuals()`
#'   tries to guess whether performance will be poor due to a very large model
#'   and thus automatically shows or hides dots.
#' @param ... Currently not used.
#'
#' @return A data frame representing the data that is mapped in the accompanying
#'   plot. In case all residuals are inside the error bounds, points are black.
#'   If some of the residuals are outside the error bounds (indicated by the
#'   grey-shaded area), blue points indicate residuals that are OK, while red
#'   points indicate model under- or over-fitting for the relevant range of
#'   estimated probabilities.
#'
#' @details Binned residual plots are achieved by "dividing the data into
#'   categories (bins) based on their fitted values, and then plotting
#'   the average residual versus the average fitted value for each bin."
#'   _(Gelman, Hill 2007: 97)_. If the model were true, one would
#'   expect about 95% of the residuals to fall inside the error bounds.
#'
#'   If `term` is not `NULL`, one can compare the residuals in
#'   relation to a specific model predictor. This may be helpful to check if a
#'   term would fit better when transformed, e.g. a rising and falling pattern
#'   of residuals along the x-axis is a signal to consider taking the logarithm
#'   of the predictor (cf. Gelman and Hill 2007, pp. 97-98).
#'
#' @note `binned_residuals()` returns a data frame, however, the `print()`
#'   method only returns a short summary of the result. The data frame itself
#'   is used for plotting. The `plot()` method, in turn, creates a ggplot-object.
#'
#' @references
#' Gelman, A., and Hill, J. (2007). Data analysis using regression and
#' multilevel/hierarchical models. Cambridge; New York: Cambridge University
#' Press.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' result <- binned_residuals(model)
#' result
#'
#' # look at the data frame
#' as.data.frame(result)
#'
#' \donttest{
#' # plot
#' if (require("see")) {
#'   plot(result, show_dots = TRUE)
#' }
#' }
#'
#' @export
binned_residuals <- function(model,
                             term = NULL,
                             n_bins = NULL,
                             show_dots = NULL,
                             ci = 0.95,
                             ci_type = c("gaussian", "exact", "boot"),
                             residuals = c("response", "pearson", "deviance"),
                             iterations = 1000,
                             ...) {
  # match arguments
  ci_type <- match.arg(ci_type)
  residuals <- match.arg(residuals)

  fitted_values <- stats::fitted(model)
  mf <- insight::get_data(model, verbose = FALSE)

  if (is.null(term)) {
    pred <- fitted_values
  } else {
    pred <- mf[[term]]
  }

  # set default for show_dots, based on "model size"
  if (is.null(show_dots)) {
    n <- .safe(insight::n_obs(model))
    show_dots <- is.null(n) || n <= 1e5
  }

  y0 <- .recode_to_zero(insight::get_response(model, verbose = FALSE))

  # calculate residuals
  y <- switch(residuals,
    response = y0 - fitted_values,
    pearson = .safe((y0 - fitted_values) / sqrt(fitted_values * (1 - fitted_values))),
    deviance = .safe(stats::residuals(model, type = "deviance"))
  )

  if (is.null(n_bins)) n_bins <- round(sqrt(length(pred)))

  breaks.index <- floor(length(pred) * (1:(n_bins - 1)) / n_bins)
  breaks <- unique(c(-Inf, sort(pred)[breaks.index], Inf))

  model.binned <- as.numeric(cut(pred, breaks))

  d <- suppressWarnings(lapply(1:n_bins, function(.x) {
    items <- (seq_along(pred))[model.binned == .x]
    model.range <- range(pred[items], na.rm = TRUE)
    xbar <- mean(pred[items], na.rm = TRUE)
    ybar <- mean(y[items], na.rm = TRUE)
    n <- length(items)
    sdev <- stats::sd(y[items], na.rm = TRUE)

    r <- switch(ci_type,
      gaussian = stats::qnorm(c((1 - ci) / 2, (1 + ci) / 2), mean = ybar, sd = sdev / sqrt(n)),
      exact = stats:::binom.test(sum(y0[items]), n)$conf.int,
      boot = .boot_binned_ci(y[items], ci, iterations)
    )
    names(r) <- c("CI_low", "CI_high")

    d0 <- data.frame(
      xbar = xbar,
      ybar = ybar,
      n = n,
      x.lo = model.range[1],
      x.hi = model.range[2],
      se = stats::qnorm((1 + ci) / 2) * sdev / sqrt(n),
      ci_range = sdev / sqrt(n)
    )
    cbind(d0, rbind(r))
  }))

  d <- do.call(rbind, d)
  d <- d[stats::complete.cases(d), ]

  gr <- abs(d$ybar) > abs(d$se)
  d$group <- "yes"
  d$group[gr] <- "no"

  resid_ok <- sum(d$group == "yes") / length(d$group)

  class(d) <- c("binned_residuals", "see_binned_residuals", class(d))
  attr(d, "resid_ok") <- resid_ok
  attr(d, "resp_var") <- insight::find_response(model)
  attr(d, "term") <- term
  attr(d, "show_dots") <- show_dots

  d
}


# utilities ---------------------------

.boot_binned_ci <- function(x, ci = 0.95, iterations = 1000) {
  x <- x[!is.na(x)]
  n <- length(x)
  out <- vector("numeric", iterations)
  for (i in seq_len(iterations)) {
    out[i] <- sum(x[sample.int(n, n, replace = TRUE)])
  }
  out <- out / n

  quant <- stats::quantile(out, c((1 - ci) / 2, (1 + ci) / 2))
  c(CI_low = quant[1L], CI_high = quant[2L])
}


# methods -----------------------------

#' @export
print.binned_residuals <- function(x, ...) {
  resid_ok <- attributes(x)$resid_ok

  if (!is.null(resid_ok)) {
    if (resid_ok < 0.8) {
      insight::print_color(
        sprintf(
          "Warning: Probably bad model fit. Only about %g%% of the residuals are inside the error bounds.\n",
          round(100 * resid_ok)
        ),
        "red"
      )
    } else if (resid_ok < 0.95) {
      insight::print_color(
        sprintf(
          "Warning: About %g%% of the residuals are inside the error bounds (~95%% or higher would be good).\n",
          round(100 * resid_ok)
        ),
        "yellow"
      )
    } else {
      insight::print_color(
        sprintf(
          "Ok: About %g%% of the residuals are inside the error bounds.\n",
          round(100 * resid_ok)
        ),
        "green"
      )
    }
  }
}


#' @export
plot.binned_residuals <- function(x, ...) {
  insight::check_if_installed("see", "to plot binned residuals")
  NextMethod()
}
