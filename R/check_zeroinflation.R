#' @title Check for zero-inflation in count models
#' @name check_zeroinflation
#'
#' @description \code{check_zeroinflation()} checks whether count models are over- or
#'    underfitting zeros in the outcome.
#'
#' @param x Fitted model of class \code{merMod}, \code{glmmTMB}, \code{glm},
#'    or \code{glm.nb} (package \pkg{MASS}).
#' @param tolerance The tolerance for the ratio of observed and predicted
#'    zeros to considered as over- or underfitting zeros. A ratio
#'    between 1 +/- \code{tolerance} is considered as OK, while a ratio
#'    beyond or below this treshold would indicate over- or underfitting.
#'
#' @return A list with information about the amount of predicted and observed
#'    zeros in the outcome, as well as the ratio between these two values.
#'
#' @details If the amount of observed zeros is larger than the amount of
#'   predicted zeros, the model is underfitting zeros, which indicates a
#'   zero-inflation in the data. In such cases, it is recommended to use
#'   negative binomial or zero-inflated models.
#'
#' @examples
#' library(glmmTMB)
#' data(Salamanders)
#' m <- glm(count ~ spp + mined, family = poisson, data = Salamanders)
#' check_zeroinflation(m)
#'
#' @importFrom insight get_response
#' @importFrom stats fitted dpois family
#' @export
check_zeroinflation <- function(x, tolerance = .05) {
  # check if we have poisson
  if (!stats::family(x)$family %in% c("poisson", "quasipoisson"))
    stop("Model must be from Poisson-family.", call. = F)

  # get actual zero of response
  obs.zero <- sum(insight::get_response(x) == 0)

  if (obs.zero == 0) {
    insight::print_color("Model has no observed zeros in the response variable.\n", "red")
    return(NULL)
  }

  # get predictions of outcome
  mu <- stats::fitted(x)

  # get predicted zero-counts
  pred.zero <- round(sum(stats::dpois(x = 0, lambda = mu)))

  # proportion
  structure(
    class = "check_zi",
    list(
      predicted.zeros = pred.zero,
      observed.zeros = obs.zero,
      ratio = pred.zero / obs.zero,
      tolerance = tolerance
    )
  )
}
