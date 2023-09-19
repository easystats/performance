#' @title Hosmer-Lemeshow goodness-of-fit test
#' @name performance_hosmer
#'
#' @description Check model quality of logistic regression models.
#'
#' @param model A `glm`-object with binomial-family.
#' @param n_bins Numeric, the number of bins to divide the data.
#'
#' @return An object of class `hoslem_test` with following values:
#'   `chisq`, the Hosmer-Lemeshow chi-squared statistic; `df`, degrees
#'   of freedom and `p.value` the p-value for the goodness-of-fit test.
#'
#' @details A well-fitting model shows *no* significant difference between
#'   the model and the observed data, i.e. the reported p-value should be
#'   greater than 0.05.
#'
#' @references
#' Hosmer, D. W., and Lemeshow, S. (2000). Applied Logistic Regression. Hoboken,
#' NJ, USA: John Wiley and Sons, Inc. \doi{10.1002/0471722146}
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' performance_hosmer(model)
#' @export
performance_hosmer <- function(model, n_bins = 10) {
  if (inherits(model, "merMod")) {
    insight::check_if_installed("lme4")
  }

  # check for valid object class
  if (!inherits(model, c("glmerMod", "glm"))) {
    insight::format_error("`model` must be an object of class `glm` or `glmerMod`.")
  }

  # mixed models (lme4)
  if (inherits(model, "glmerMod")) {
    y <- lme4::getME(model, "y")
    yhat <- stats::fitted(model)
  } else {
    y <- model$y
    yhat <- stats::fitted(model)
  }

  cutyhat <- cut(
    yhat,
    breaks = stats::quantile(yhat, probs = seq(0, 1, 1 / n_bins)),
    include.lowest = TRUE
  )

  obs <- stats::xtabs(cbind(1 - y, y) ~ cutyhat)
  expect <- stats::xtabs(cbind(1 - yhat, yhat) ~ cutyhat)
  chisq <- sum((obs - expect)^2 / expect)
  p.value <- 1 - stats::pchisq(chisq, n_bins - 2)

  hoslem <- list(
    chisq = chisq,
    df = n_bins - 2,
    p.value = p.value
  )

  class(hoslem) <- c("performance_hosmer", "list")
  hoslem
}



# methods ---------------------------------

#' @export
print.performance_hosmer <- function(x, ...) {
  insight::print_color("# Hosmer-Lemeshow Goodness-of-Fit Test\n\n", "blue")

  v1 <- sprintf("%.3f", x$chisq)
  v2 <- sprintf("%i    ", x$df)
  v3 <- sprintf("%.3f", x$p.value)

  space <- max(nchar(c(v1, v2, v3)))

  cat(sprintf("  Chi-squared: %*s\n", space, v1))
  cat(sprintf("           df: %*s\n", space, v2))
  cat(sprintf("      p-value: %*s\n\n", space, v3))

  if (x$p.value >= 0.05) {
    message("Summary: model seems to fit well.")
  } else {
    message("Summary: model does not fit well.")
  }

  invisible(x)
}
