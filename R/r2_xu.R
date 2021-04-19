#' @title Xu' R2 (Omega-squared)
#' @name r2_xu
#'
#' @description Calculates Xu' Omega-squared value, a simple R2 equivalent for linear mixed models.
#'
#' @param model A linear (mixed) model.
#'
#' @return The R2 value.
#'
#' @details \code{r2_xu()} is a crude measure for the explained variance from
#'   linear (mixed) effects models, which is originally denoted as
#'   \ifelse{html}{\out{&Omega;<sup>2</sup>}}{\eqn{\Omega^2}}.
#'
#' @references Xu, R. (2003). Measuring explained variation in linear mixed effects models. Statistics in Medicine, 22(22), 3527–3541. \doi{10.1002/sim.1572}
#'
#' @examples
#' model <- lm(Sepal.Length ~ Petal.Length + Species, data = iris)
#' r2_xu(model)
#' @export
r2_xu <- function(model) {
  mi <- insight::model_info(model)
  if (!mi$is_linear) {
    stop("Xu's R2 is only applicable for linear models.")
  }

  .r2_xu <- 1 - stats::var(stats::residuals(model)) / stats::var(insight::get_response(model))
  names(.r2_xu) <- "Xu's R2"
  .r2_xu
}
