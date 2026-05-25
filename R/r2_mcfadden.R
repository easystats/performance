#' @title McFadden's R2
#' @name r2_mcfadden
#'
#' @description Calculates McFadden's pseudo R2.
#'
#' @param model Generalized linear or multinomial logit (`mlogit`) model.
#' @param ... Currently not used.
#'
#' @return For most models, a list with McFadden's R2 and adjusted McFadden's
#'   R2 value. For some models, only McFadden's R2 is available.
#'
#' @references
#' - McFadden, D. (1987). Regression-based specification tests for the
#'   multinomial logit model. Journal of econometrics, 34(1-2), 63-82.
#' - McFadden, D. (1973). Conditional logit analysis of qualitative choice
#'   behavior.
#'
#' @examples
#' if (require("mlogit")) {
#'   data("Fishing", package = "mlogit")
#'   Fish <- mlogit.data(Fishing, varying = c(2:9), shape = "wide", choice = "mode")
#'
#'   model <- mlogit(mode ~ price + catch, data = Fish)
#'   r2_mcfadden(model)
#' }
#' @export
r2_mcfadden <- function(model, ...) {
  UseMethod("r2_mcfadden")
}


# helper -----------------------

.r2_mcfadden <- function(model, l_null) {
  l_full <- insight::get_loglikelihood(model)
  k <- length(insight::find_parameters(model))
  mcfadden <- 1 - (as.vector(l_full) / as.vector(l_null))
  mcfadden_adjusted <- 1 - ((as.vector(l_full) - k) / as.vector(l_null))

  out <- list(
    R2 = c(`McFadden's R2` = mcfadden),
    R2_adjusted = c(`adjusted McFadden's R2` = mcfadden_adjusted)
  )

  attr(out, "model_type") <- "Generalized Linear"
  structure(class = "r2_generic", out)
}


# r2 via loglik and update --------------------------

#' @export
r2_mcfadden.glm <- function(model, verbose = TRUE, ...) {
  info <- list(...)$model_info
  if (is.null(info)) {
    info <- suppressWarnings(insight::model_info(model, verbose = FALSE))
  }
  matrix_response <- grepl("cbind", insight::find_response(model), fixed = TRUE)

  if (
    info$is_binomial &&
      !info$is_betabinomial &&
      !info$is_bernoulli &&
      class(model)[1] %in% c("glm", "glmmTMB")
  ) {
    if (verbose) {
      insight::format_warning(
        "Can't calculate accurate R2 for binomial models that are not Bernoulli models."
      )
    }
    return(NULL)
  }
  # currently, beta-binomial models without proportion response are not supported
  if (info$is_betabinomial && matrix_response) {
    if (verbose) {
      insight::format_warning(
        "Can't calculate accurate R2 for beta-binomial models with matrix-response formulation."
      )
    }
    return(NULL)
  }

  l_null <- insight::get_loglikelihood(insight::null_model(model))
  .r2_mcfadden(model, l_null)
}

#' @export
r2_mcfadden.clm <- r2_mcfadden.glm

#' @export
r2_mcfadden.serp <- r2_mcfadden.glm

#' @export
r2_mcfadden.cpglm <- r2_mcfadden.glm

#' @export
r2_mcfadden.glmx <- r2_mcfadden.glm

#' @export
r2_mcfadden.polr <- r2_mcfadden.glm

#' @export
r2_mcfadden.bracl <- r2_mcfadden.glm

#' @export
r2_mcfadden.brmultinom <- r2_mcfadden.glm

#' @export
r2_mcfadden.censReg <- r2_mcfadden.glm

#' @export
r2_mcfadden.glmmTMB <- r2_mcfadden.glm

#' @export
r2_mcfadden.truncreg <- r2_mcfadden.glm

#' @export
r2_mcfadden.mclogit <- function(model, ...) {
  insight::check_if_installed("mclogit", reason = "to calculate R2")
  s <- mclogit::getSummary.mclogit(model)
  r2_mcfadden <- s$sumstat["McFadden"]
  names(r2_mcfadden) <- "McFadden's R2"
  r2_mcfadden
}

#' @export
r2_mcfadden.mblogit <- function(model, ...) {
  insight::check_if_installed("mclogit", reason = "to calculate R2")
  s <- mclogit::getSummary.mblogit(model)
  r2_mcfadden <- s$sumstat["McFadden"]
  names(r2_mcfadden) <- "McFadden's R2"
  r2_mcfadden
}


# mfx models ---------------------

#' @export
r2_mcfadden.logitmfx <- function(model, ...) {
  r2_mcfadden(model$fit, ...)
}

#' @export
r2_mcfadden.logitor <- r2_mcfadden.logitmfx

#' @export
r2_mcfadden.poissonirr <- r2_mcfadden.logitmfx

#' @export
r2_mcfadden.poissonmfx <- r2_mcfadden.logitmfx

#' @export
r2_mcfadden.negbinirr <- r2_mcfadden.logitmfx

#' @export
r2_mcfadden.probitmfx <- r2_mcfadden.logitmfx

#' @export
r2_mcfadden.negbinmfx <- r2_mcfadden.logitmfx


# special models -------------------------------------------

#' @export
r2_mcfadden.vglm <- function(model, ...) {
  if (!(is.null(model@call$summ) && !identical(model@call$summ, 0))) {
    insight::format_error("Can't get log-likelihood when `summ` is not zero.")
  }

  l_null <- insight::get_loglikelihood(insight::null_model(model))
  .r2_mcfadden(model, l_null)
}


#' @export
r2_mcfadden.clm2 <- function(model, ...) {
  l_null <- insight::get_loglikelihood(insight::null_model(model))
  .r2_mcfadden(model, l_null)
}


#' @export
r2_mcfadden.multinom <- r2_mcfadden.clm2


#' @export
r2_mcfadden.mlogit <- function(model, ...) {
  R2 <- as.vector(summary(model)$mfR2)
  names(R2) <- "McFadden's R2"
  R2
}
