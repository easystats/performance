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
#' @param ... Currently not used.
#'
#' @return A data frame representing the data that is mapped in the accompanying
#'   plot. In case all residuals are inside the error bounds, points are black.
#'   If some of the residuals are outside the error bounds (indicated by the
#'   grey-shaded area), blue points indicate residuals that are OK, while red
#'   points indicate model under- or over-fitting for the relevant range of
#'   estimated probabilities.
#'
#' @details Binned residual plots are achieved by \dQuote{dividing the data into
#'   categories (bins) based on their fitted values, and then plotting
#'   the average residual versus the average fitted value for each bin.}
#'   \cite{(Gelman, Hill 2007: 97)}. If the model were true, one would
#'   expect about 95% of the residuals to fall inside the error bounds.
#'   \cr \cr
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
#' # plot
#' if (require("see") && getRversion() >= "3.6.0") {
#'   plot(result)
#' }
#' @export
binned_residuals <- function(model, term = NULL, n_bins = NULL, ...) {
  fv <- stats::fitted(model)
  mf <- insight::get_data(model)

  if (is.null(term)) {
    pred <- fv
  } else {
    pred <- mf[[term]]
  }

  y <- .recode_to_zero(insight::get_response(model, verbose = FALSE)) - fv

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

    data.frame(
      xbar = xbar,
      ybar = ybar,
      n = n,
      x.lo = model.range[1],
      x.hi = model.range[2],
      se = stats::qnorm(.975) * sdev / sqrt(n),
      ci_range = sdev / sqrt(n)
    )
  }))

  d <- do.call(rbind, d)
  d <- d[stats::complete.cases(d), ]

  # CIs
  d$CI_low <- d$ybar - stats::qnorm(.975) * d$ci_range
  d$CI_high <- d$ybar + stats::qnorm(.975) * d$ci_range

  gr <- abs(d$ybar) > abs(d$se)
  d$group <- "yes"
  d$group[gr] <- "no"

  resid_ok <- sum(d$group == "yes") / length(d$group)

  class(d) <- c("binned_residuals", "see_binned_residuals", class(d))
  attr(d, "resid_ok") <- resid_ok
  attr(d, "resp_var") <- insight::find_response(model)
  attr(d, "term") <- term

  d
}



# methods -----------------------------

#' @export
print.binned_residuals <- function(x, ...) {
  resid_ok <- attributes(x)$resid_ok

  if (!is.null(resid_ok)) {
    if (resid_ok < .8) {
      insight::print_color(sprintf("Warning: Probably bad model fit. Only about %g%% of the residuals are inside the error bounds.\n", round(100 * resid_ok)), "red")
    } else if (resid_ok < .95) {
      insight::print_color(sprintf("Warning: About %g%% of the residuals are inside the error bounds (~95%% or higher would be good).\n", round(100 * resid_ok)), "yellow")
    } else {
      insight::print_color(sprintf("Ok: About %g%% of the residuals are inside the error bounds.\n", round(100 * resid_ok)), "green")
    }
  }
}


#' @export
plot.binned_residuals <- function(x, ...) {
  insight::check_if_installed("see", "to plot binned residuals")
  NextMethod()
}
