#' @title Cox & Snell's R2
#' @name r2_coxsnell
#'
#' @description Calculates the pseudo-R2 value based on the proposal from \cite{Cox & Snell (1989)}.
#'
#' @param model Model with binary outcome.
#'
#' @details This index was proposed by \cite{Cox & Snell (1989, pp. 208-9)} and,
#'   apparently independently, by \cite{Magee (1990)}; but had been suggested earlier
#'   for binary response models by \cite{Maddala (1983)}. However, this index achieves
#'   a maximum of less than 1 for discrete models (i.e. models whose likelihood
#'   is a product of probabilities) which have a maximum of 1, instead of
#'   densities, which can become infinite \cite{(Nagelkerke, 1991)}.
#'
#' @return A named vector with the R2 value.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_coxsnell(model)
#'
#' @references
#' \itemize{
#'   \item Cox, D. R., Snell, E. J. (1989). Analysis of binary data (Vol. 32). Monographs on Statistics and Applied Probability.
#'   \item Magee, L. (1990). R 2 measures based on Wald and likelihood ratio joint significance tests. The American Statistician, 44(3), 250-253.
#'   \item Maddala, G. S. (1986). Limited-dependent and qualitative variables in econometrics (No. 3). Cambridge university press.
#'   \item Nagelkerke, N. J. (1991). A note on a general definition of the coefficient of determination. Biometrika, 78(3), 691-692.
#' }
#'
#' @importFrom insight n_obs
#' @importFrom stats logLik update
#' @export
r2_coxsnell <- function(model) {
  UseMethod("r2_coxsnell")
}


.r2_coxsnell <- function(model, l_base) {
  l_full <- stats::logLik(model)
  G2 <- -2 * (l_base - l_full)

  # Is it still necessary?
  if (inherits(model, c("vglm", "clm2"))) {
    n <- insight::n_obs(model)
  } else {
    n <- attr(l_full, "nobs")
    if (is.null(n)) n <- insight::n_obs(model)
  }

  r2_coxsnell <- as.vector(1 - exp(-G2 / n))

  names(r2_coxsnell) <- "Cox & Snell's R2"
  r2_coxsnell
}


#' @export
r2_coxsnell.glm <- function(model) {
  r2_coxsnell <- (1 - exp((model$deviance - model$null.deviance) / insight::n_obs(model)))
  names(r2_coxsnell) <- "Cox & Snell's R2"
  r2_coxsnell
}


#' @export
r2_coxsnell.BBreg <- function(model) {
  r2_coxsnell <- (1 - exp((model$deviance - model$null.deviance) / insight::n_obs(model)))
  names(r2_coxsnell) <- "Cox & Snell's R2"
  r2_coxsnell
}


#' @export
r2_coxsnell.multinom <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1, trace = FALSE))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.vglm <- function(model) {
  if (!(is.null(model@call$summ) && !identical(model@call$summ, 0))) {
    stop("Can't get log-likelihood when `summ` is not zero.", call. = FALSE)
  }

  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.coxph <- function(model) {
  l_base <- model$loglik[1]
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.survreg <- function(model) {
  l_base <- model$loglik[1]
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.clm <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.crch <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.censReg <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.truncreg <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.clm2 <- function(model) {
  l_base <- stats::logLik(stats::update(model, location = ~1, scale = ~1))
  .r2_coxsnell(model, l_base)
}


#' @export
r2_coxsnell.polr <- function(model) {
  l_base <- stats::logLik(stats::update(model, ~1))
  .r2_coxsnell(model, l_base)
}
