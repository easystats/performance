#' @title McKelvey & Zavoinas R2
#' @name r2_mckelvey
#'
#' @description Calculates McKelvey & Zavoinas pseudo R2.
#'
#' @param model Generalized linear model.
#'
#' @return The R2 value.
#'
#' @references
#' \itemize{
#'   \item McKelvey, R., Zavoina, W. (1975), "A Statistical Model for the Analysis of Ordinal Level Dependent Variables", Journal of Mathematical Sociology 4, S. 103–120.
#' }
#'
#' @details McKelvey & Zavoinas R2 is based on the explained variance,
#'   where the variance of the predicted response is divided by the sum
#'   of the variance of the predicted response and residual variance.
#'   For binomial models, the residual variance is either \code{pi^2/3}
#'   for logit-link and 1 for probit-link. For poisson-models, the
#'   residual variance is based on log-normal approximation, similar to
#'   the \emph{distribution-specific variance} as described in
#'   \code{?insight::get_variance}.
#'
#'
#' @examples
#' ## Dobson (1990) Page 93: Randomized Controlled Trial:
#' counts <- c(18, 17, 15, 20, 10, 20, 25, 13, 12) #
#' outcome <- gl(3, 1, 9)
#' treatment <- gl(3, 3)
#' model <- glm(counts ~ outcome + treatment, family = poisson())
#'
#' r2_mckelvey(model)
#' @importFrom insight n_obs model_info
#' @importFrom stats predict family coef update
#' @export
r2_mckelvey <- function(model) {
  UseMethod("r2_mckelvey")
}


#' @export
r2_mckelvey.default <- function(model) {
  .r2_mckelvey(model)
}


.r2_mckelvey <- function(model) {
  faminfo <- insight::model_info(model)
  n <- insight::n_obs(model)

  if (faminfo$is_binomial | faminfo$is_ordinal | faminfo$is_multinomial) {
    dist.variance <- switch(
      faminfo$link_function,
      probit = 1,
      logit = pi^2 / 3,
      clogloglink = pi^2 / 6,
      NA
    )
  } else if (faminfo$is_count) {
    dist.variance <- switch(
      faminfo$link_function,
      log = .get_poisson_variance(model),
      sqrt = 0.25,
      0
    )
  }

  y.hat <- stats::predict(model, type = "link")

  # fix for VGAM
  yhat_columns <- ncol(y.hat)
  if (!is.null(yhat_columns) && yhat_columns > 1) y.hat <- as.vector(y.hat[, 1])

  dist.residual <- sum((y.hat - mean(y.hat))^2)

  mckelvey <- dist.residual / (n * dist.variance + dist.residual)
  names(mckelvey) <- "McKelvey's R2"
  mckelvey
}


.null_model <- function(model) {
  stats::update(model, ~1)
}


.get_poisson_variance <- function(model) {
  mu <- exp(stats::coef(.null_model(model)))
  if (is.na(mu)) {
    return(0)
  }
  cvsquared <- stats::family(model)$variance(mu) / mu^2
  log1p(cvsquared)
}
