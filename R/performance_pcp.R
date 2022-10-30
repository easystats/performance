#' @title Percentage of Correct Predictions
#' @name performance_pcp
#'
#' @description Percentage of correct predictions (PCP) for models
#'   with binary outcome.
#'
#' @param model Model with binary outcome.
#' @param ci The level of the confidence interval.
#' @param method Name of the method to calculate the PCP (see 'Details').
#'   Default is `"Herron"`. May be abbreviated.
#' @inheritParams model_performance.lm
#'
#' @return A list with several elements: the percentage of correct predictions
#'   of the full and the null model, their confidence intervals, as well as the
#'   chi-squared and p-value from the Likelihood-Ratio-Test between the full and
#'   null model.
#'
#' @details `method = "Gelman-Hill"` (or `"gelman_hill"`) computes the
#'   PCP based on the proposal from _Gelman and Hill 2017, 99_, which is
#'   defined as the proportion of cases for which the deterministic prediction
#'   is wrong, i.e. the proportion where the predicted probability is above 0.5,
#'   although y=0 (and vice versa) (see also _Herron 1999, 90_).
#'
#'   `method = "Herron"` (or `"herron"`) computes a modified version
#'   of the PCP (_Herron 1999, 90-92_), which is the sum of predicted
#'   probabilities, where y=1, plus the sum of 1 - predicted probabilities,
#'   where y=0, divided by the number of observations. This approach is said to
#'   be more accurate.
#'
#'   The PCP ranges from 0 to 1, where values closer to 1 mean that the model
#'   predicts the outcome better than models with an PCP closer to 0. In general,
#'   the PCP should be above 0.5 (i.e. 50\%), the closer to one, the better.
#'   Furthermore, the PCP of the full model should be considerably above
#'   the null model's PCP.
#'
#'   The likelihood-ratio test indicates whether the model has a significantly
#'   better fit than the null-model (in such cases, p < 0.05).
#'
#'
#' @references
#' - Herron, M. (1999). Postestimation Uncertainty in Limited Dependent
#'   Variable Models. Political Analysis, 8, 83–98.
#' - Gelman, A., and Hill, J. (2007). Data analysis using regression and
#'   multilevel/hierarchical models. Cambridge; New York: Cambridge University
#'   Press, 99.
#'
#' @examples
#' data(mtcars)
#' m <- glm(formula = vs ~ hp + wt, family = binomial, data = mtcars)
#' performance_pcp(m)
#' performance_pcp(m, method = "Gelman-Hill")
#' @export
performance_pcp <- function(model,
                            ci = 0.95,
                            method = "Herron",
                            verbose = TRUE) {
  # fix special cases
  if (inherits(model, c("model_fit", "logitor", "logitmfx", "probitmfx"))) {
    model <- model$fit
  }

  method <- match.arg(method, choices = c("Herron", "Gelman-Hill", "herron", "gelman_hill"))
  mi <- insight::model_info(model, verbose = verbose)

  if (!mi$is_binomial) {
    insight::format_error("`performance_pcp()` only works for models with binary outcome.")
  }

  resp <- insight::get_response(model, verbose = verbose)

  if (!is.null(ncol(resp)) && ncol(resp) > 1) {
    if (verbose) insight::print_color("`performance_pcp()` only works for models with binary response values.\n", "red")
    return(NULL)
  }

  m0 <- suppressWarnings(stats::glm(
    formula = stats::as.formula(sprintf("%s ~ 1", insight::find_response(model))),
    family = stats::binomial(link = "logit"),
    data = insight::get_data(model, verbose = verbose),
    weights = stats::weights(model)
  ))

  .performance_pcp(model, m0, ci, method = method, verbose = verbose)
}



# methods ----------------------------------

#' @export
print.performance_pcp <- function(x, digits = 2, ...) {
  insight::print_color("# Percentage of Correct Predictions from Logistic Regression Model\n\n", "blue")
  cat(sprintf("  Full model: %.2f%% [%.2f%% - %.2f%%]\n", 100 * x$pcp_model, 100 * x$model_ci_low, 100 * x$model_ci_high))
  cat(sprintf("  Null model: %.2f%% [%.2f%% - %.2f%%]\n", 100 * x$pcp_m0, 100 * x$null_ci_low, 100 * x$null_ci_high))

  insight::print_color("\n# Likelihood-Ratio-Test\n\n", "blue")

  v1 <- sprintf("%.3f", x$lrt_chisq)
  v2 <- sprintf("%.3f", x$lrt_df_error)
  v3 <- sprintf("%.3f", x$lrt_p)

  space <- max(nchar(c(v1, v2)))

  cat(sprintf("  Chi-squared: %*s\n", space, v1))
  cat(sprintf("  df: %*s\n", space, v2))
  cat(sprintf("  p-value: %*s\n\n", space, v3))

  invisible(x)
}


#' @export
as.data.frame.performance_pcp <- function(x, row.names = NULL, ...) {
  data.frame(
    "Model" = c("full", "null"),
    "Estimate" = c(x$pcp_model, x$pcp_m0),
    "CI_low" = c(x$model_ci_low, x$null_ci_low),
    "CI_high" = c(x$model_ci_high, x$null_ci_high),
    "Chisq" = c(NA, x$lrt_chisq),
    "df_error" = c(NA, x$lrt_df_error),
    "p" = c(NA, x$lrt_p),
    stringsAsFactors = FALSE,
    row.names = row.names,
    ...
  )
}



# utilities --------------------------------------

.performance_pcp <- function(model, m0, ci, method, verbose = TRUE) {
  y_full <- .recode_to_zero(insight::get_response(model, verbose = verbose))
  y_null <- .recode_to_zero(insight::get_response(m0, verbose = verbose))

  n_full <- suppressWarnings(insight::n_obs(model))
  n_null <- suppressWarnings(insight::n_obs(m0))

  pr_full <- stats::predict(model, type = "response")
  pr_null <- stats::predict(m0, type = "response")

  if (tolower(method) == "herron") {
    pcp_full <- (sum(1 - pr_full[y_full == 0]) + sum(pr_full[y_full == 1])) / n_full
    pcp_null <- (sum(1 - pr_null[y_null == 0]) + sum(pr_null[y_null == 1])) / n_null
  } else {
    pcp_full <- 1 - mean((pr_full > .5 & y_full == 0) | (pr_full <= .5 & y_full == 1))
    pcp_null <- 1 - mean((pr_null > .5 & y_null == 0) | (pr_null <= .5 & y_null == 1))
  }

  lrt.p <- 1 - stats::pchisq(
    q = model$null.deviance - model$deviance,
    df = model$df.null - model$df.residual,
    lower.tail = TRUE
  )

  lrt.chisq <- 2 * abs(insight::get_loglikelihood(model, verbose = verbose) - insight::get_loglikelihood(m0, verbose = verbose))

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
      lrt_df_error = model$df.null - model$df.residual,
      lrt_p = lrt.p
    )
  )
}
