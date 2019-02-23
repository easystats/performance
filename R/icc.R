#' The Intraclass Correlation Coefficient (ICC) for mixed models
#'
#' DESCRIPTION TO BE IMPROVED.
#'
#' @param model A statistical model.
#'
#' @examples
#' \dontrun{
#' library(lme4)
#' model <- lme4::lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#' icc(model)
#' }
#' 
#' @export
icc <- function(model) {
  vars <- .compute_variances(model, name = "icc")


  # Calculate ICC values

  icc_adjusted <- vars$var.ranef / (vars$var.ranef + vars$var.resid)
  icc_conditional <- vars$var.ranef / (vars$var.fixef + vars$var.ranef + vars$var.resid)


  out <- data.frame(
    "ICC_adjusted" = icc_adjusted,
    "ICC_conditional" = icc_conditional
  )


  out
}
