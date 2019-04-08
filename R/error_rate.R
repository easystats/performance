#' @title Error Rate for Logistic Regression Models
#' @name error_rate
#'
#' @description Compute the error rate for logistic regression models, which is
#'    a crude measure for the model fit.
#'
#' @param model A \code{glm}-object with binomial-family.
#'
#' @references Gelman A, Hill J (2007) Data Analysis Using Regression and Multilevel/Hierarchical Models. Cambridge, New York: Cambridge University Press
#'
#' @details The error rate is a crude measure for model fit for logistic regression
#'    models. It is defined as the proportion of cases for which the
#'    deterministic prediction is wrong, i.e. the proportion where the the
#'    predicted probability is above 0.5, although y = 0 (and vice versa).
#'    In general, the error rate should be below 0.5 (i.e. 50\%), the
#'    closer to zero, the better. Furthermore, the error rate of the full
#'    model should be considerably below the null model's error rate
#'    (cf. Gelman and Hill 2007, pp. 99).
#'
#' @return A list with four values: the error rate of the full and the null model,
#'     as well as the chi-squared and p-value from the Likelihood-Ratio-Test
#'     between the full and null model.
#'
#' @examples
#' data(mtcars)
#' m <- glm(am ~ mpg + hp + cyl, data = mtcars, family= binomial)
#' error_rate(m)
#'
#' @importFrom insight get_response find_response get_data
#' @importFrom stats binomial predict.glm pchisq logLik weights as.formula glm
#' @export
error_rate <- function(model) {
  m0 <- suppressWarnings(stats::glm(
    formula = stats::as.formula(sprintf("%s ~ 1", insight::find_response(model))),
    family = stats::binomial(link = "logit"),
    data = insight::get_data(model),
    weights = stats::weights(model)
  ))

  y1 <- insight::get_response(model)
  y0 <- insight::get_response(m0)

  p1 <- stats::predict.glm(model, type = "response")
  error1 <- mean((p1 > .5 & y1 == 0) | (p1 <= .5 & y1 == 1))

  p0 <- stats::predict.glm(m0, type = "response")
  error0 <- mean((p0 > .5 & y0 == 0) | (p0 <= .5 & y0 == 1))

  lrt.p <- 1 - stats::pchisq(
    q = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual,
    lower.tail = TRUE
  )

  lrt.chisq <- 2 * abs(stats::logLik(model) - stats::logLik(m0))

  list(
    error.model = error1,
    error.null = error0,
    lrt.chisq = as.vector(lrt.chisq),
    lrt.p = lrt.p
  )
}
