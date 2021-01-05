#' @title Hosmer-Lemeshow goodness-of-fit test
#' @name performance_hosmer
#'
#' @description Check model quality of logistic regression models.
#'
#' @param model A \code{glm}-object with binomial-family.
#' @param n_bins Numeric, the number of bins to divide the data.
#'
#' @return An object of class \code{hoslem_test} with following values: \code{chisq},
#'   the Hosmer-Lemeshow chi-squared statistic; \code{df}, degrees of freedom
#'   and \code{p.value} the p-value for the goodness-of-fit test.
#'
#' @details A well-fitting model shows \emph{no} significant difference between
#'   the model and the observed data, i.e. the reported p-value should be
#'   greater than 0.05.
#'
#' @references Hosmer, D. W., & Lemeshow, S. (2000). Applied Logistic Regression. Hoboken, NJ, USA: John Wiley & Sons, Inc. \doi{10.1002/0471722146}
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' performance_hosmer(model)
#' @importFrom stats fitted quantile xtabs pchisq
#' @export
performance_hosmer <- function(model, n_bins = 10) {
  if (inherits(model, "merMod") && !requireNamespace("lme4", quietly = TRUE)) {
    stop("Package `lme4` needed for this function to work. Please install it.", call. = FALSE)
  }

  # check for valid object class
  if (!inherits(model, c("glmerMod", "glm"))) {
    stop("`model` must be an object of class 'glm' or 'glmerMod'.", call. = FALSE)
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
