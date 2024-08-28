#' @title Multivariate R2
#' @name r2_mlm
#'
#' @description
#' Calculates two multivariate R2 values for multivariate linear regression.
#'
#' @param model Multivariate linear regression model.
#' @param ... Currently not used.
#'
#' @details
#' The two indexes returned summarize model fit for the set of predictors
#' given the system of responses. As compared to the default
#' [r2][performance::r2] index for multivariate linear models, the indexes
#' returned by this function provide a single fit value collapsed across
#' all responses.
#'
#' The two returned indexes were proposed by *Van den Burg and Lewis (1988)*
#' as an extension of the metrics proposed by *Cramer and Nicewander (1979)*.
#' Of the numerous indexes proposed across these two papers, only two metrics,
#' the \eqn{R_{xy}} and \eqn{P_{xy}}, are recommended for use
#' by *Azen and Budescu (2006)*.
#'
#' For a multivariate linear regression with \eqn{p} predictors and
#' \eqn{q} responses where \eqn{p > q}, the \eqn{R_{xy}} index is
#' computed as:
#'
#' \deqn{R_{xy} = 1 - \prod_{i=1}^p (1 - \rho_i^2)}
#'
#' Where \eqn{\rho} is a canonical variate from a
#' [canonical correlation][cancor] between the predictors and responses.
#' This metric is symmetric and its value does not change when the roles of
#' the variables as predictors or responses are swapped.
#'
#' The \eqn{P_{xy}} is computed as:
#'
#' \deqn{P_{xy} = \frac{q - trace(\bf{S}_{\bf{YY}}^{-1}\bf{S}_{\bf{YY|X}})}{q}}
#'
#' Where \eqn{\bf{S}_{\bf{YY}}} is the matrix of response covariances and
#' \eqn{\bf{S}_{\bf{YY|X}}} is the matrix of residual covariances given
#' the predictors. This metric is asymmetric and can change
#' depending on which variables are considered predictors versus responses.
#'
#' @return A named vector with the R2 values.
#'
#' @examples
#' model <- lm(cbind(qsec, drat) ~ wt + mpg + cyl, data = mtcars)
#' r2_mlm(model)
#'
#' model_swap <- lm(cbind(wt, mpg, cyl) ~ qsec + drat, data = mtcars)
#' r2_mlm(model_swap)
#'
#' @references
#' - Azen, R., & Budescu, D. V. (2006). Comparing predictors in
#'   multivariate regression models: An extension of dominance analysis.
#'   Journal of Educational and Behavioral Statistics, 31(2), 157-180.
#'-  Cramer, E. M., & Nicewander, W. A. (1979). Some symmetric,
#'   invariant measures of multivariate association. Psychometrika, 44, 43-54.
#' - Van den Burg, W., & Lewis, C. (1988). Some properties of two
#'   measures of multivariate association. Psychometrika, 53, 109-122.
#'
#' @author Joseph Luchman
#'
#' @export
r2_mlm <- function(model, ...) {
  UseMethod("r2_mlm")
}

# methods ---------------------------

#' @export
r2_mlm.mlm <- function(model, verbose = TRUE, ...) {
  rho2_vec <- 1 - stats::cancor(
    insight::get_predictors(model),
    insight::get_response(model)
  )$cor^2
  R_xy  <- 1 - Reduce(`*`, rho2_vec, 1)

  resid_cov <- stats::cov(residuals(model))
  resp_cov <- stats::cov(insight::get_response(model))
  qq <- ncol(insight::get_response(model))
  V_xy <- qq - sum(diag(solve(resp_cov) %*% resid_cov))
  P_xy <- V_xy / qq

  c("Symmetric Rxy" = R_xy, "Asymmetric Pxy" = P_xy)
}
