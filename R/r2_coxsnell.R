#' @title Cox & Snell's R2
#' @name r2_coxsnell
#'
#' @description
#' Calculates the pseudo-R2 value based on the proposal from \cite{Cox & Snell
#' (1989)}.
#'
#' @param model Model with binary outcome.
#' @param ... Currently not used.
#'
#' @details
#' This index was proposed by \cite{Cox & Snell (1989, pp. 208-9)} and,
#' apparently independently, by \cite{Magee (1990)}; but had been suggested
#' earlier for binary response models by \cite{Maddala (1983)}. However, this
#' index achieves a maximum of less than 1 for discrete models (i.e. models
#' whose likelihood is a product of probabilities) which have a maximum of 1,
#' instead of densities, which can become infinite \cite{(Nagelkerke, 1991)}.
#'
#' @return A named vector with the R2 value.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_coxsnell(model)
#' @references
#' \itemize{
#'   \item Cox, D. R., Snell, E. J. (1989). Analysis of binary data (Vol. 32).
#'   Monographs on Statistics and Applied Probability.
#'
#'   \item Magee, L. (1990). R 2 measures based on Wald and likelihood ratio
#'   joint significance tests. The American Statistician, 44(3), 250-253.
#'
#'   \item Maddala, G. S. (1986). Limited-dependent and qualitative variables in
#'   econometrics (No. 3). Cambridge university press.
#'
#'   \item Nagelkerke, N. J. (1991). A note on a general definition of the
#'   coefficient of determination. Biometrika, 78(3), 691-692.
#' }
#' @export
r2_coxsnell <- function(model, ...) {
  UseMethod("r2_coxsnell")
}


# helper ---------------------------


.r2_coxsnell <- function(model, l_base) {
  l_full <- insight::get_loglikelihood(model)
  G2 <- -2 * (l_base - l_full)

  # Is it still necessary?
  if (inherits(model, c("vglm", "vgam", "clm2"))) {
    n <- suppressWarnings(insight::n_obs(model))
  } else {
    n <- attr(l_full, "nobs")
    if (is.null(n)) n <- suppressWarnings(insight::n_obs(model, disaggregate = TRUE))
  }

  r2_coxsnell <- as.vector(1 - exp(-G2 / n))

  names(r2_coxsnell) <- "Cox & Snell's R2"
  r2_coxsnell
}



# r2-coxsnell based on model information ---------------------------


#' @export
r2_coxsnell.glm <- function(model, verbose = TRUE, ...) {
  info <- insight::model_info(model)
  if (info$is_binomial && !info$is_bernoulli && class(model)[1] == "glm") {
    if (verbose) {
      warning(insight::format_message("Can't calculate accurate R2 for binomial models that are not Bernoulli models."), call. = FALSE)
    }
    return(NULL)
  } else {
    # if no deviance, return NA
    if (is.null(model$deviance)) {
      return(NULL)
    }
    r2_coxsnell <- (1 - exp((model$deviance - model$null.deviance) / insight::n_obs(model, disaggregate = TRUE)))
    names(r2_coxsnell) <- "Cox & Snell's R2"
    r2_coxsnell
  }
}

#' @export
r2_coxsnell.BBreg <- r2_coxsnell.glm

#' @export
r2_coxsnell.mclogit <- r2_coxsnell.glm

#' @export
r2_coxsnell.bife <- function(model, ...) {
  r2_coxsnell <- (1 - exp((model$deviance - model$null_deviance) / insight::n_obs(model)))
  names(r2_coxsnell) <- "Cox & Snell's R2"
  r2_coxsnell
}



# mfx models ---------------------


#' @export
r2_coxsnell.logitmfx <- function(model, ...) {
  r2_coxsnell(model$fit, ...)
}

#' @export
r2_coxsnell.logitor <- r2_coxsnell.logitmfx

#' @export
r2_coxsnell.poissonirr <- r2_coxsnell.logitmfx

#' @export
r2_coxsnell.poissonmfx <- r2_coxsnell.logitmfx

#' @export
r2_coxsnell.probit <- r2_coxsnell.logitmfx

#' @export
r2_coxsnell.negbinirr <- r2_coxsnell.logitmfx

#' @export
r2_coxsnell.negbinmfx <- r2_coxsnell.logitmfx




# r2-coxsnell based on loglik stored in model object ---------------------------


#' @export
r2_coxsnell.coxph <- function(model, ...) {
  l_base <- model$loglik[1]
  .r2_coxsnell(model, l_base)
}

#' @export
r2_coxsnell.survreg <- r2_coxsnell.coxph

#' @export
r2_coxsnell.svycoxph <- function(model, ...) {
  l_base <- model$ll[1]
  .r2_coxsnell(model, l_base)
}



# r2-coxsnell based on loglik of null-model (update) ---------------------------


#' @export
r2_coxsnell.multinom <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, ~1, trace = FALSE))
  .r2_coxsnell(model, l_base)
}

#' @export
r2_coxsnell.clm2 <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, location = ~1, scale = ~1))
  .r2_coxsnell(model, l_base)
}

#' @export
r2_coxsnell.bayesx <- function(model, ...) {
  junk <- utils::capture.output(l_base <- insight::get_loglikelihood(stats::update(model, ~1)))
  .r2_coxsnell(model, l_base)
}

#' @export
r2_coxsnell.clm <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, ~1))
  # if no loglik, return NA
  if (length(as.numeric(l_base)) == 0) {
    return(NULL)
  }
  .r2_coxsnell(model, l_base)
}

#' @export
r2_coxsnell.crch <- r2_coxsnell.clm

#' @export
r2_coxsnell.cpglm <- r2_coxsnell.clm

#' @export
r2_coxsnell.censReg <- r2_coxsnell.clm

#' @export
r2_coxsnell.truncreg <- r2_coxsnell.clm

#' @export
r2_coxsnell.polr <- r2_coxsnell.clm

#' @export
r2_coxsnell.glmx <- r2_coxsnell.clm

#' @export
r2_coxsnell.DirichletRegModel <- r2_coxsnell.clm
