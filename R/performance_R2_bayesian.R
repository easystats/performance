#' R2 for Bayesian regressions.
#'
#' Compute R2 for Bayesian regressions.
#'
#' @param model A Bayesian regression model.
#'
#' @examples
#' \dontrun{
#' library(rstanarm)
#'
#' model <- rstanarm::stan_glm(mpg ~ wt + cyl, data = mtcars)
#' performance_R2_bayesian(model)
#' }
#'
#'
#' @importFrom stats var
#' @importFrom utils install.packages
#' @export
performance_R2_bayesian <- function(model) {

  if(!requireNamespace("rstanarm")){
    warning("This function needs `rstanarm` to be installed... installing now.")
    install.packages("rstanarm")
    requireNamespace("rstanarm")
  }

  R2_bayesian <- rstanarm::bayes_R2(model)
  return(R2_bayesian)
}