#' @title Compute the model's R2
#' @name r2
#'
#' @description Calculate the R2, also known as the coefficient of
#'   determination, value for different model objects. Depending on the model,
#'   R2, pseudo-R2, or marginal / adjusted R2 values are returned.
#'
#' @param model A statistical model.
#' @param ci Confidence Interval (CI) level. Default is \code{NULL}. Confidence
#'   intervals for R2 can be calculated based on different methods, see
#'   \code{ci_method}.
#' @param ci_method Method for constructing the R2 confidence interval.
#'   Options are \code{"analytical"} for sampling-theory-based frequentist
#'   intervals and \code{"bootstrap"} for bootstrap intervals. Analytical intervals
#'   are not available for all models. For Bayesian models, [r2_bayes()] is used.
#' @param verbose Logical. Should details about R2 and CI methods be given (`TRUE`) or not (`FALSE`)?
#' @param ... Arguments passed down to the related r2-methods.
#' @inheritParams r2_nakagawa
#'
#' @return Returns a list containing values related to the most appropriate R2
#'   for the given model (or `NULL` if no R2 could be extracted). See the
#'   list below:
#' \itemize{
#'   \item Logistic models: [Tjur's R2][r2_tjur]
#'   \item General linear models: [Nagelkerke's R2][r2_nagelkerke]
#'   \item Multinomial Logit: [McFadden's R2][r2_mcfadden]
#'   \item Models with zero-inflation: [R2 for zero-inflated models][r2_zeroinflated]
#'   \item Mixed models: [Nakagawa's R2][r2_nakagawa]
#'   \item Bayesian models: [R2 bayes][r2_bayes]
#' }
#'
#' @note If there is no `r2()`-method defined for the given model class,
#'   `r2()` tries to return a "generic r2 value, calculated as following:
#'   `1-sum((y-y_hat)^2)/sum((y-y_bar)^2))`
#'
#' @seealso [r2_bayes()], [r2_coxsnell()], [r2_kullback()],
#'   [r2_loo()], [r2_mcfadden()], [r2_nagelkerke()],
#'   [r2_nakagawa()], [r2_tjur()], [r2_xu()] and
#'   [r2_zeroinflated()].
#'
#' @examples
#' model <- glm(vs ~ wt + mpg, data = mtcars, family = "binomial")
#' r2(model)
#'
#' if (require("lme4")) {
#'   model <- lmer(Sepal.Length ~ Petal.Length + (1 | Species), data = iris)
#'   r2(model)
#' }
#' @export
r2 <- function(model, ...) {
  UseMethod("r2")
}




# Default models -----------------------------------------------


#' @rdname r2
#' @export
r2.default <- function(model, ci = NULL, ci_method = "analytical", verbose = TRUE, ...) {
  ci_method <- match.arg(ci_method, choices = c("analytical", "bootstrap"))

  # check input
  ci <- .check_r2_ci_args(ci, ci_method, "bootstrap", verbose)

  out <- tryCatch(
    {
      if (insight::model_info(model)$is_binomial) {
        resp <- .recode_to_zero(insight::get_response(model, verbose = FALSE))
      } else {
        resp <- .factor_to_numeric(insight::get_response(model, verbose = FALSE))
      }
      mean_resp <- mean(resp, na.rm = TRUE)
      pred <- insight::get_predicted(model, verbose = FALSE)
      list(R2 = 1 - sum((resp - pred)^2) / sum((resp - mean_resp)^2))
    },
    error = function(e) {
      NULL
    }
  )

  if (is.null(out) && isTRUE(verbose)) {
    insight::print_color(sprintf("'r2()' does not support models of class '%s'.\n", class(model)[1]), "red")
  }

  if (!is.null(out)) {
    names(out$R2) <- "R2"
    class(out) <- c("r2_generic", class(out))
  }

  out
}


#' @export
r2.lm <- function(model, ...) {
  .r2_lm(summary(model))
}


.r2_lm <- function(model_summary) {
  out <- list(
    R2 = model_summary$r.squared,
    R2_adjusted = model_summary$adj.r.squared
  )

  names(out$R2) <- "R2"
  names(out$R2_adjusted) <- "adjusted R2"

  f.stat <- model_summary$fstatistic[1]
  DoF <- model_summary$fstatistic[2]
  DoF_residual <- model_summary$fstatistic[3]

  if (!is.null(f.stat)) {
    attr(out, "p") <- stats::pf(f.stat, DoF, DoF_residual, lower.tail = FALSE)
    attr(out, "F") <- f.stat
    attr(out, "df") <- DoF
    attr(out, "df_residual") <- DoF_residual
  }

  attr(out, "model_type") <- "Linear"
  structure(class = "r2_generic", out)
}



#' @export
r2.summary.lm <- function(model, ...) {
  .r2_lm(model)
}



#' @export
r2.systemfit <- function(model, ...) {
  out <- lapply(summary(model)$eq, function(model_summary) {
    s <- list(
      R2 = model_summary$r.squared,
      R2_adjusted = model_summary$adj.r.squared
    )

    names(s$R2) <- "R2"
    names(s$R2_adjusted) <- "adjusted R2"

    s
  })

  names(out) <- names(insight::find_formula(model))
  out
}



#' @export
r2.ols <- function(model, ...) {
  out <- list(R2 = model$stats["R2"])
  names(out$R2) <- "R2"

  attr(out, "model_type") <- "Linear"
  structure(class = "r2_generic", out)
}

#' @export
r2.lrm <- r2.ols

#' @export
r2.cph <- r2.ols



#' @export
r2.mhurdle <- function(model, ...) {
  resp <- insight::get_response(model, verbose = FALSE)
  mean_resp <- mean(resp, na.rm = TRUE)
  ftd <- model$fitted.values[, "pos", drop = TRUE] * (1 - model$fitted.values[, "zero", drop = TRUE])
  n <- length(resp)
  K <- insight::n_parameters(model)
  Ko <- length(model$naive$coefficients)

  out <- list(
    R2 = 1 - sum((resp - ftd)^2) / sum((resp - mean_resp)^2),
    R2_adjusted = 1 - (n - Ko) / (n - K) * sum((resp - ftd)^2) / sum((resp - mean_resp)^2)
  )

  names(out$R2) <- "R2"
  names(out$R2_adjusted) <- "adjusted R2"

  attr(out, "model_type") <- "Limited Dependent Variable"
  structure(class = "r2_generic", out)
}



#' @export
r2.aov <- function(model, ...) {
  model_summary <- stats::summary.lm(model)

  out <- list(
    R2 = model_summary$r.squared,
    R2_adjusted = model_summary$adj.r.squared
  )

  names(out$R2) <- "R2"
  names(out$R2_adjusted) <- "adjusted R2"

  attr(out, "model_type") <- "Anova"
  structure(class = "r2_generic", out)
}



#' @export
r2.mlm <- function(model, ...) {
  model_summary <- summary(model)

  out <- lapply(names(model_summary), function(i) {
    tmp <- list(
      R2 = model_summary[[i]]$r.squared,
      R2_adjusted = model_summary[[i]]$adj.r.squared,
      Response = sub("Response ", "", i)
    )
    names(tmp$R2) <- "R2"
    names(tmp$R2_adjusted) <- "adjusted R2"
    names(tmp$Response) <- "Response"
    tmp
  })

  names(out) <- names(model_summary)

  attr(out, "model_type") <- "Multivariate Linear"
  structure(class = "r2_mlm", out)
}



#' @export
r2.glm <- function(model, verbose = TRUE, ...) {
  info <- insight::model_info(model)

  if (info$family %in% c("gaussian", "inverse.gaussian")) {
    out <- r2.default(model, ...)
  } else if (info$is_logit && info$is_bernoulli) {
    out <- list("R2_Tjur" = r2_tjur(model))
    attr(out, "model_type") <- "Logistic"
    names(out$R2_Tjur) <- "Tjur's R2"
    class(out) <- c("r2_pseudo", class(out))
  } else if (info$is_binomial && !info$is_bernoulli && class(model)[1] == "glm") {
    if (verbose) {
      warning(insight::format_message("Can't calculate accurate R2 for binomial models that are not Bernoulli models."), call. = FALSE)
    }
    out <- NULL
  } else {
    out <- list("R2_Nagelkerke" = r2_nagelkerke(model))
    names(out$R2_Nagelkerke) <- "Nagelkerke's R2"
    attr(out, "model_type") <- "Generalized Linear"
    class(out) <- c("r2_pseudo", class(out))
  }
  out
}

#' @export
r2.glmx <- r2.glm




# mfx models ---------------------


#' @export
r2.logitmfx <- function(model, ...) {
  r2(model$fit, ...)
}

#' @export
r2.logitor <- r2.logitmfx

#' @export
r2.poissonirr <- r2.logitmfx

#' @export
r2.poissonmfx <- r2.logitmfx

#' @export
r2.probitmfx <- r2.logitmfx

#' @export
r2.negbinirr <- r2.logitmfx

#' @export
r2.negbinmfx <- r2.logitmfx

#' @export
r2.betamfx <- r2.logitmfx

#' @export
r2.betaor <- r2.logitmfx

#' @export
r2.model_fit <- r2.logitmfx




# Cox & Snell R2 ---------------------


#' @export
r2.BBreg <- function(model, ...) {
  out <- list("R2_CoxSnell" = r2_coxsnell(model))
  names(out$R2_CoxSnell) <- "Cox & Snell's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}

#' @export
r2.crch <- r2.BBreg

#' @export
r2.bayesx <- r2.BBreg




# Nagelkerke R2 ----------------------


#' @export
r2.censReg <- function(model, ...) {
  out <- list("R2_Nagelkerke" = r2_nagelkerke(model))
  names(out$R2_Nagelkerke) <- "Nagelkerke's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}

#' @export
r2.cpglm <- r2.censReg

#' @export
r2.clm <- r2.censReg

#' @export
r2.clm2 <- r2.censReg

#' @export
r2.coxph <- r2.censReg

#' @export
r2.multinom <- r2.censReg

#' @export
r2.mclogit <- r2.censReg

#' @export
r2.polr <- r2.censReg

#' @export
r2.survreg <- r2.censReg

#' @export
r2.truncreg <- r2.censReg

#' @export
r2.bracl <- r2.censReg

#' @export
r2.brmultinom <- r2.censReg

#' @export
r2.bife <- r2.censReg









# Zeroinflated R2 ------------------


#' @export
r2.hurdle <- function(model, ...) {
  r2_zeroinflated(model)
}

#' @export
r2.zerotrunc <- r2.hurdle

#' @export
r2.zeroinfl <- r2.hurdle


# Nakagawa R2 ----------------------


#' @rdname r2
#' @export
r2.merMod <- function(model, tolerance = 1e-5, ...) {
  r2_nakagawa(model, tolerance = tolerance, ...)
}

#' @export
r2.glmmTMB <- r2.merMod

#' @export
r2.cpglmm <- r2.merMod

#' @export
r2.glmmadmb <- r2.merMod

#' @export
r2.lme <- r2.merMod

#' @export
r2.clmm <- r2.merMod

#' @export
r2.mixed <- r2.merMod

#' @export
r2.MixMod <- r2.merMod

#' @export
r2.rlmerMod <- r2.merMod


#' @export
r2.wbm <- function(model, tolerance = 1e-5, ...) {
  out <- r2_nakagawa(model, tolerance = tolerance)

  if (is.null(out) || is.na(out)) {
    s <- summary(model)

    r2_marginal <- s$mod_info_list$pR2_fe
    r2_conditional <- s$mod_info_list$pR2_total

    names(r2_conditional) <- "Conditional R2"
    names(r2_marginal) <- "Marginal R2"

    out <- list(
      "R2_conditional" = r2_conditional,
      "R2_marginal" = r2_marginal
    )

    attr(out, "model_type") <- "Fixed Effects"
    class(out) <- "r2_nakagawa"
  }

  out
}



#' @export
r2.sem <- function(model, ...) {
  r2_conditional <- model$r2c
  r2_marginal <- model$r2m

  names(r2_conditional) <- "Conditional R2"
  names(r2_marginal) <- "Marginal R2"

  structure(
    class = "r2_nakagawa",
    list(
      "R2_conditional" = r2_conditional,
      "R2_marginal" = r2_marginal
    )
  )
}



# Bayes R2 ------------------------


#' @export
r2.brmsfit <- function(model, ...) {
  r2_bayes(model, ...)
}

#' @export
r2.stanreg <- r2.brmsfit


#' @export
r2.BFBayesFactor <- r2.brmsfit


# Other methods ------------------------------


#' @export
r2.gam <- function(model, ...) {

  # gamlss inherits from gam, and summary.gamlss prints results automatically
  printout <- utils::capture.output(s <- summary(model))

  if (!is.null(s$r.sq)) {
    list(
      R2 = c(`Adjusted R2` = s$r.sq)
    )
  } else {
    NextMethod()
  }
}

#' @export
r2.scam <- r2.gam


#' @export
r2.betareg <- function(model, ...) {
  out <- list(R2 = c(`Pseudo R2` = model$pseudo.r.squared))
  attr(out, "model_type") <- "Beta"
  class(out) <- c("r2_generic", class(out))
  out
}


#' @export
r2.rma <- function(model, ...) {
  s <- summary(model)

  if (is.null(s$R2)) {
    return(NULL)
  }

  out <- list(R2 = s$R2 / 100)
  attr(out, "model_type") <- "Meta-Analysis"
  structure(class = "r2_generic", out)
}



#' @export
r2.feis <- function(model, ...) {
  out <- list(
    R2 = c(`R2` = model$r2),
    R2_adjusted = c(`adjusted R2` = model$adj.r2)
  )

  attr(out, "model_type") <- "Fixed Effects Individual Slope"
  structure(class = "r2_generic", out)
}



#' @export
r2.fixest <- function(model, ...) {
  insight::check_if_installed("fixest")

  r2 <- fixest::r2(model)

  out_normal <- .compact_list(list(
    R2 = r2["r2"],
    R2_adjusted = r2["ar2"],
    R2_within = r2["wr2"],
    R2_within_adjusted = r2["war2"]
  ), remove_na = TRUE)

  out_pseudo <- .compact_list(list(
    R2 = r2["pr2"],
    R2_adjusted = r2["apr2"],
    R2_within = r2["wpr2"],
    R2_within_adjusted = r2["wapr2"]
  ), remove_na = TRUE)

  if (length(out_normal)) {
    out <- out_normal
  } else {
    out <- out_pseudo
  }

  attr(out, "model_type") <- "Fixed Effects"
  structure(class = "r2_generic", out)
}



#' @export
r2.felm <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r2),
    R2_adjusted = c(`adjusted R2` = model_summary$r2adj)
  )

  attr(out, "model_type") <- "Fixed Effects"
  structure(class = "r2_generic", out)
}




#' @export
r2.iv_robust <- function(model, ...) {
  out <- list(
    R2 = c(`R2` = model$r.squared),
    R2_adjusted = c(`adjusted R2` = model$adj.r.squared)
  )

  attr(out, "model_type") <- "Two-Stage Least Squares Instrumental-Variable"
  structure(class = "r2_generic", out)
}



#' @export
r2.ivreg <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r.squared),
    R2_adjusted = c(`adjusted R2` = model_summary$adj.r.squared)
  )

  attr(out, "model_type") <- "Instrumental-Variable"
  structure(class = "r2_generic", out)
}



#' @export
r2.bigglm <- function(model, ...) {
  out <- list("R2_CoxSnell" = summary(model)$rsq)
  names(out$R2_CoxSnell) <- "Cox & Snell's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}



#' @export
r2.biglm <- function(model, ...) {
  df.int <- ifelse(insight::has_intercept(model), 1, 0)
  n <- suppressWarnings(insight::n_obs(model))

  rsq <- summary(model)$rsq
  adj.rsq <- 1 - (1 - rsq) * ((n - df.int) / model$df.resid)

  out <- list(
    R2 = rsq,
    R2_adjusted = adj.rsq
  )

  names(out$R2) <- "R2"
  names(out$R2_adjusted) <- "adjusted R2"

  attr(out, "model_type") <- "Linear"
  structure(class = "r2_generic", out)
}



#' @export
r2.lmrob <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    R2 = c(`R2` = model_summary$r.squared),
    R2_adjusted = c(`adjusted R2` = model_summary$adj.r.squared)
  )

  attr(out, "model_type") <- "Robust Linear"
  structure(class = "r2_generic", out)
}

#' @export
r2.complmrob <- r2.lmrob



#' @export
r2.mlogit <- function(model, ...) {
  out <- list("R2_McFadden" = r2_mcfadden(model))
  names(out$R2_McFadden) <- "McFadden's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}



#' @export
r2.mmclogit <- function(model, ...) {
  list(R2 = NA)
}


#' @export
r2.mclogit <- function(model, ...) {
  list(R2 = NA)
}


#' @export
r2.Arima <- function(model, ...) {
  if (!requireNamespace("forecast", quietly = TRUE)) {
    list(R2 = NA)
  } else {
    list(R2 = stats::cor(stats::fitted(model), insight::get_data(model))^2)
  }
}



#' @export
r2.plm <- function(model, ...) {
  model_summary <- summary(model)
  out <- list(
    "R2" = c(`R2` = model_summary$r.squared[1]),
    "R2_adjusted" = c(`adjusted R2` = model_summary$r.squared[2])
  )

  attr(out, "model_type") <- "Panel Data"
  structure(class = "r2_generic", out)
}



#' @export
r2.selection <- function(model, ...) {
  model_summary <- summary(model)
  if (is.null(model_summary$rSquared)) {
    return(NULL)
  }
  out <- list(
    "R2" = c(`R2` = model_summary$rSquared$R2),
    "R2_adjusted" = c(`adjusted R2` = model_summary$rSquared$R2adj)
  )

  attr(out, "model_type") <- "Tobit 2"
  structure(class = "r2_generic", out)
}



#' @export
r2.svyglm <- function(model, ...) {
  rsq <- (model$null.deviance - model$deviance) / model$null.deviance
  rsq.adjust <- 1 - ((1 - rsq) * (model$df.null / model$df.residual))

  out <- list(
    R2 = c(`R2` = rsq),
    R2_adjusted = c(`adjusted R2` = rsq.adjust)
  )

  attr(out, "model_type") <- "Survey"
  structure(class = "r2_generic", out)
}



#' @export
r2.vglm <- function(model, ...) {
  out <- list("R2_McKelvey" = r2_mckelvey(model))
  names(out$R2_McKelvey) <- "McKelvey's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}

#' @export
r2.vgam <- r2.vglm



#' @export
r2.DirichletRegModel <- function(model, ...) {
  out <- list("R2_Nagelkerke" = r2_nagelkerke(model))
  names(out$R2_Nagelkerke) <- "Nagelkerke's R2"
  class(out) <- c("r2_pseudo", class(out))
  out
}





# helper -------------------

.check_r2_ci_args <- function(ci = NULL, ci_method = "bootstrap", valid_ci_method = NULL, verbose = TRUE) {
  if (!is.null(ci) && !is.na(ci)) {
    if (!is.null(valid_ci_method) && !ci_method %in% valid_ci_method) {
      if (verbose) {
        warning(paste0("Method '", ci_method, "' to compute confidence intervals for R2 not supported."), call. = FALSE)
      }
      return(NULL)
    }
  }
  ci
}
