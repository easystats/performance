#' LOO-adjusted R2.
#'
#' Compute LOO-adjusted R2.
#'
#' @param model A Bayesian regression model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' performance_R2_LOO_adjusted(model)
#' }
#'
#'
#' @importFrom stats var
#' @importFrom utils install.packages
#' @export
performance_R2_LOO_adjusted <- function(model) {
  looR2(model)
}