#' @title Nagelkerke's R2
#' @name r2_nagelkerke
#'
#' @description DESCRIPTION TO BE IMPROVED.
#'
#' @param model Binomial Model.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_nagelkerke(model)
#'
#' @references Nagelkerke, N. J. (1991). A note on a general definition of the coefficient of determination. Biometrika, 78(3), 691-692.
#'
#' @export
r2_nagelkerke <- function(model) {
  UseMethod("r2_nagelkerke")
}


.r2_nagelkerke <- function(model, l_base) {
  L.full <- stats::logLik(model)
  D.full <- -2 * L.full

  D.base <- -2 * l_base
  # Is it still necessary?
  if (inherits(model, c("vglm", "clm2"))) {
    n <- insight::n_obs(model)
  } else {
    n <- attr(L.full, "nobs")
  }

  r2_nagelkerke <- (1 - exp((D.full - D.base) / n)) / (1 - exp(-D.base / n))

  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}



#' @export
r2_nagelkerke.glm <- function(model) {
  r2_nagelkerke <- r2_coxnell(model) / (1 - exp(-model$null / insight::n_obs(model)))
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}


#' @export
r2_nagelkerke.multinom <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1, trace = FALSE))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.vglm <- function(model) {
  if (!(is.null(model@call$summ) && !identical(model@call$summ, 0))) {
    stop("Can't get log-likelihood when `summ` is not zero.", call. = FALSE)
  }

  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.clm <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.clm2 <- function(model) {
  l_base <- stats::logLik(stats::update(model, location = ~1, scale = ~1))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.polr <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_nagelkerke(model, l_base)
}
