#' @title Nagelkerke's R2
#' @name r2_nagelkerke
#'
#' @description Calculate Nagelkerke's pseudo-R2.
#'
#' @param model A generalized linear model, including cumulative links resp.
#'   multinomial models.
#' @param ... Currently not used.
#'
#' @return A named vector with the R2 value.
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2_nagelkerke(model)
#' @references
#' Nagelkerke, N. J. (1991). A note on a general definition of the coefficient
#' of determination. Biometrika, 78(3), 691-692.
#'
#' @export
r2_nagelkerke <- function(model, ...) {
  UseMethod("r2_nagelkerke")
}


# helper ---------------------------


.r2_nagelkerke <- function(model, l_base) {
  L.full <- insight::get_loglikelihood(model)
  D.full <- -2 * L.full

  D.base <- -2 * l_base
  # Is it still necessary?
  if (inherits(model, c("vglm", "vgam", "clm2"))) {
    n <- insight::n_obs(model)
  } else {
    n <- attr(L.full, "nobs")
    if (is.null(n)) n <- insight::n_obs(model, disaggregate = TRUE)
  }

  r2_nagelkerke <- as.vector((1 - exp((D.full - D.base) / n)) / (1 - exp(-D.base / n)))

  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}


# Nagelkerke's R2 based on Cox&Snell's R2 ----------------

#' @export
r2_nagelkerke.glm <- function(model, verbose = TRUE, ...) {
  info <- list(...)$model_info
  if (is.null(info)) {
    info <- suppressWarnings(insight::model_info(model, verbose = FALSE))
  }

  if (info$is_binomial && !info$is_bernoulli && class(model)[1] == "glm") {
    if (verbose) {
      insight::format_warning("Can't calculate accurate R2 for binomial models that are not Bernoulli models.")
    }
    return(NULL)
  }

  r2cox <- r2_coxsnell(model)

  if (is.na(r2cox) || is.null(r2cox)) {
    return(NULL)
  }

  r2_nagelkerke <- r2cox / (1 - exp(-model$null.deviance / insight::n_obs(model, disaggregate = TRUE)))
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}

#' @export
r2_nagelkerke.BBreg <- r2_nagelkerke.glm


#' @export
r2_nagelkerke.glmmTMB <- function(model, verbose = TRUE, ...) {
  info <- list(...)$model_info
  if (is.null(info)) {
    info <- suppressWarnings(insight::model_info(model, verbose = FALSE))
  }

  if (info$is_binomial && !info$is_bernoulli) {
    if (verbose) {
      insight::format_warning("Can't calculate accurate R2 for binomial models that are not Bernoulli models.")
    }
    return(NULL)
  }

  dev <- stats::deviance(model)

  # if no deviance, return NA
  if (is.null(dev)) {
    return(NULL)
  }

  null_mod <- suppressWarnings(insight::null_model(model))
  null_dev <- stats::deviance(null_mod)
  r2cox <- (1 - exp((dev - null_dev) / insight::n_obs(model, disaggregate = TRUE)))

  if (is.na(r2cox) || is.null(r2cox)) {
    return(NULL)
  }

  r2_nagelkerke <- r2cox / (1 - exp(-null_dev / insight::n_obs(model, disaggregate = TRUE)))
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}


#' @export
r2_nagelkerke.nestedLogit <- function(model, ...) {
  n <- insight::n_obs(model, disaggregate = TRUE)
  stats::setNames(
    lapply(names(model$models), function(i) {
      m <- model$models[[i]]
      # if no deviance, return NA
      if (is.null(m$deviance)) {
        return(NA)
      }
      r2_nagelkerke <- (1 - exp((m$deviance - m$null.deviance) / n[[i]])) / (1 - exp(-m$null.deviance / n[[i]]))
      names(r2_nagelkerke) <- "Nagelkerke's R2"
      r2_nagelkerke
    }),
    names(model$models)
  )
}


#' @export
r2_nagelkerke.bife <- function(model, ...) {
  r2_nagelkerke <- r2_coxsnell(model) / (1 - exp(-model$null_deviance / insight::n_obs(model)))
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}


# mfx models ---------------------


#' @export
r2_nagelkerke.logitmfx <- function(model, ...) {
  r2_nagelkerke(model$fit, ...)
}

#' @export
r2_nagelkerke.logitor <- r2_nagelkerke.logitmfx

#' @export
r2_nagelkerke.poissonirr <- r2_nagelkerke.logitmfx

#' @export
r2_nagelkerke.poissonmfx <- r2_nagelkerke.logitmfx

#' @export
r2_nagelkerke.probitmfx <- r2_nagelkerke.logitmfx

#' @export
r2_nagelkerke.negbinirr <- r2_nagelkerke.logitmfx

#' @export
r2_nagelkerke.negbinmfx <- r2_nagelkerke.logitmfx


# Nagelkerke's R2 based on LogLik ----------------


#' @export
r2_nagelkerke.multinom <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, ~1, trace = FALSE))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.clm2 <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, location = ~1, scale = ~1))
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.clm <- function(model, ...) {
  l_base <- insight::get_loglikelihood(stats::update(model, ~1))
  # if no loglik, return NA
  if (length(as.numeric(l_base)) == 0) {
    return(NULL)
  }
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.polr <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.serp <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.cpglm <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.bracl <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.glmx <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.brmultinom <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.censReg <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.truncreg <- r2_nagelkerke.clm

#' @export
r2_nagelkerke.DirichletRegModel <- r2_coxsnell.clm


# Nagelkerke's R2 based on LogLik stored in model object ----------------


#' @export
r2_nagelkerke.coxph <- function(model, ...) {
  l_base <- model$loglik[1]
  .r2_nagelkerke(model, l_base)
}

#' @export
r2_nagelkerke.survreg <- r2_nagelkerke.coxph

#' @export
r2_nagelkerke.crch <- r2_nagelkerke.coxph

#' @export
r2_nagelkerke.svycoxph <- function(model, ...) {
  l_base <- model$ll[1]
  .r2_nagelkerke(model, l_base)
}


# other ---------------------

#' @export
r2_nagelkerke.mclogit <- function(model, ...) {
  insight::check_if_installed("mclogit", reason = "to calculate R2")
  s <- mclogit::getSummary.mclogit(model)
  r2_nagelkerke <- s$sumstat["Nagelkerke"]
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}

#' @export
r2_nagelkerke.mblogit <- function(model, ...) {
  insight::check_if_installed("mclogit", reason = "to calculate R2")
  s <- mclogit::getSummary.mblogit(model)
  r2_nagelkerke <- s$sumstat["Nagelkerke"]
  names(r2_nagelkerke) <- "Nagelkerke's R2"
  r2_nagelkerke
}
