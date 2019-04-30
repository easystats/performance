#' @title Percentage of Correct Predictions
#' @name performance_pcp
#'
#' @description Percentage of correct predictions (PCP) for models
#'   with binary outcome.
#'
#' @param model Model with binary outcome.
#' @param ci The level of the confidence interval.
#' @param method Name of the method to calculate the PCP (see 'Details'). May be abbreviated.
#'
#' @return A list with several elements: the percentage of correct predictions
#'   of the full and the null model, their confidence intervals, as well as the
#'   chi-squared and p-value from the Likelihood-Ratio-Test between the full and
#'   null model.
#'
#' @details \code{method = "Gelman-Hill"} computes the PCP based on the
#'   proposal from \cite{Gelman and Hill 2017, 99}, which is defined as the
#'   proportion of cases for which the deterministic prediction is wrong,
#'   i.e. the proportion where the predicted probability is above 0.5,
#'   although y=0 (and vice versa) (see also \cite{Herron 1999, 90}).
#'   \cr \cr
#'   \code{method = "Herron"} computes a modified version of the PCP
#'   (\cite{Herron 1999, 90-92}), which is the sum of predicted probabilities,
#'   where y=1, plus the sum of 1 - predicted probabilities, where y=0, divided
#'   by the number of observations. This approach is said to be more accurate.
#'   \cr \cr
#'   The PCP ranges from 0 to 1, where values closer to 1 mean that the model
#'   predicts the outcome better than models with an PCP closer to 0. In general,
#'   the PCP should be above 0.5 (i.e. 50\%), the closer to one, the better.
#'   Furthermore, the PCP of the full model should be considerably above
#'   the null model's PCP.
#'   \cr \cr
#'   The likelihood-ratio test indicates whether the model has a significantly
#'   better fit than the null-model (in such cases, p < 0.05).
#'
#'
#' @references \itemize {
#'   \item Herron, M. (1999). Postestimation Uncertainty in Limited Dependent Variable Models. Political Analysis, 8, 83–98.
#'   \item Gelman, A., & Hill, J. (2007). Data analysis using regression and multilevel/hierarchical models. Cambridge; New York: Cambridge University Press, 99
#' }
#'
#' @examples
#' data(mtcars)
#' m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
#' performance_pcp(m)
#' performance_pcp(m, method = "Gelman-Hill")
#'
#' @importFrom stats predict qnorm binomial predict.glm pchisq logLik weights as.formula glm
#' @importFrom insight get_response n_obs model_info find_response get_data
#' @export
performance_pcp <- function(model, ci = 0.95, method = c("Herron", "Gelman-Hill")) {
  method <- match.arg(method)
  mi <- insight::model_info(model)

  if (!mi$is_binomial) {
    stop("`performance_pcp()` only works for models with binary outcome.")
  }

  m0 <- suppressWarnings(stats::glm(
    formula = stats::as.formula(sprintf("%s ~ 1", insight::find_response(model))),
    family = stats::binomial(link = "logit"),
    data = insight::get_data(model),
    weights = stats::weights(model)
  ))

  if (method == "Herron")
    .pcp_herron(model, m0, ci)
  else
    .pcp_gelman_hill(model, m0, ci)
}


.pcp_herron <- function(model, m0, ci) {
  y_full <- insight::get_response(model)
  y_null <- insight::get_response(m0)

  n_full <- insight::n_obs(model)
  n_null <- insight::n_obs(m0)

  pr_full <- stats::predict(model, type = "response")
  pr_null <- stats::predict(m0, type = "response")

  pcp_full <- (sum(1 - pr_full[y_full == 0]) + sum(pr_full[y_full == 1])) / n_full
  pcp_null <- (sum(1 - pr_null[y_null == 0]) + sum(pr_null[y_null == 1])) / n_null

  lrt.p <- 1 - stats::pchisq(
    q = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual,
    lower.tail = TRUE
  )

  lrt.chisq <- 2 * abs(stats::logLik(model) - stats::logLik(m0))

  structure(
    class = "performance_pcp",
    list(
      pcp_model = pcp_full,
      model_ci_low = pcp_full - stats::qnorm((1 + ci) / 2) * sqrt(pcp_full * (1 - pcp_full) / n_full),
      model_ci_high = pcp_full + stats::qnorm((1 + ci) / 2) * sqrt(pcp_full * (1 - pcp_full) / n_full),
      pcp_m0 = pcp_null,
      null_ci_low = pcp_null - stats::qnorm((1 + ci) / 2) * sqrt(pcp_null * (1 - pcp_null) / n_null),
      null_ci_high = pcp_null + stats::qnorm((1 + ci) / 2) * sqrt(pcp_null * (1 - pcp_null) / n_null),
      lrt_chisq = as.vector(lrt.chisq),
      lrt_p = lrt.p
    )
  )
}


.pcp_gelman_hill <- function(model, m0, ci) {
  y_full <- insight::get_response(model)
  y_null <- insight::get_response(m0)

  n_full <- insight::n_obs(model)
  n_null <- insight::n_obs(m0)

  pr_full <- stats::predict(model, type = "response")
  pr_null <- stats::predict(m0, type = "response")

  pcp_full <- 1 - mean((pr_full > .5 & y_full == 0) | (pr_full <= .5 & y_full == 1))
  pcp_null <- 1 - mean((pr_null > .5 & y_null == 0) | (pr_null <= .5 & y_null == 1))

  lrt.p <- 1 - stats::pchisq(
    q = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual,
    lower.tail = TRUE
  )

  lrt.chisq <- 2 * abs(stats::logLik(model) - stats::logLik(m0))

  structure(
    class = "performance_pcp",
    list(
      pcp_model = pcp_full,
      model_ci_low = pcp_full - stats::qnorm((1 + ci) / 2) * sqrt(pcp_full * (1 - pcp_full) / n_full),
      model_ci_high = pcp_full + stats::qnorm((1 + ci) / 2) * sqrt(pcp_full * (1 - pcp_full) / n_full),
      pcp_m0 = pcp_null,
      null_ci_low = pcp_null - stats::qnorm((1 + ci) / 2) * sqrt(pcp_null * (1 - pcp_null) / n_null),
      null_ci_high = pcp_null + stats::qnorm((1 + ci) / 2) * sqrt(pcp_null * (1 - pcp_null) / n_null),
      lrt_chisq = as.vector(lrt.chisq),
      lrt_p = lrt.p
    )
  )
}
